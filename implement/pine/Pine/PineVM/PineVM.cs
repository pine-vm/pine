using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;

public interface IPineVM
{
    Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment);
}

public record struct EvalCacheEntryKey(
    PineValue ExprValue,
    PineValue EnvValue);

public class PineVM : IPineVM
{
    public long EvaluateExpressionCount { private set; get; }

    public long FunctionApplicationMaxEnvSize { private set; get; }

    private readonly ParseExprDelegate parseExpressionDelegate;

    private IDictionary<EvalCacheEntryKey, PineValue>? EvalCache { init; get; }

    private readonly Action<EvaluationReport>? reportFunctionApplication;

    public record EvaluationReport(
        PineValue ExpressionValue,
        Expression Expression,
        PineValue Environment,
        long ParseAndEvalCount,
        TimeSpan ElapsedTime,
        PineValue ReturnValue);

    public static PineVM Construct(
        IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>? parseExpressionOverrides = null,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null)
    {
        var parseExpressionOverridesDict =
            parseExpressionOverrides
            ?.ToFrozenDictionary(
                keySelector: encodedExprAndDelegate => encodedExprAndDelegate.Key,
                elementSelector: encodedExprAndDelegate => new Expression.DelegatingExpression(encodedExprAndDelegate.Value));

        return new PineVM(
            overrideParseExpression:
            parseExpressionOverridesDict switch
            {
                null =>
                originalHandler => originalHandler,

                not null =>
                _ => value => ExpressionEncoding.ParseExpressionFromValue(value, parseExpressionOverridesDict)
            },
            evalCache: evalCache);
    }

    public PineVM(
        OverrideParseExprDelegate? overrideParseExpression = null,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        Action<EvaluationReport>? reportFunctionApplication = null)
    {
        parseExpressionDelegate =
            overrideParseExpression
            ?.Invoke(ExpressionEncoding.ParseExpressionFromValueDefault) ??
            ExpressionEncoding.ParseExpressionFromValueDefault;

        EvalCache = evalCache;

        this.reportFunctionApplication = reportFunctionApplication;
    }

    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment) =>
        EvaluateExpressionDefault(
            expression,
            environment,
            config: new EvaluationConfig(ParseAndEvalCountLimit: null));

    public record StackFrameInstructions(
        IReadOnlyList<StackInstruction> Instructions)
    {
        public virtual bool Equals(StackFrameInstructions? other)
        {
            if (other is not { } notNull)
                return false;

            return
                ReferenceEquals(this, notNull) ||
                Instructions.Count == notNull.Instructions.Count &&
                Instructions.SequenceEqual(notNull.Instructions);
        }

        public override int GetHashCode()
        {
            var hashCode = new HashCode();

            foreach (var item in Instructions)
            {
                hashCode.Add(item.GetHashCode());
            }

            return hashCode.ToHashCode();
        }
    }

    record StackFrame(
        PineValue? ExpressionValue,
        Expression Expression,
        StackFrameInstructions Instructions,
        PineValue EnvironmentValue,
        Memory<PineValue> InstructionsResultValues,
        long BeginParseAndEvalCount,
        long? BeginTimestamp)
    {
        public int InstructionPointer { get; set; } = 0;

        public int LastEvalResultIndex { get; set; } = -1;

        public void PushInstructionResult(PineValue value)
        {
            InstructionsResultValues.Span[InstructionPointer] = value;
            LastEvalResultIndex = InstructionPointer;
            ++InstructionPointer;
        }

        public PineValue LastEvalResult()
        {
            if (LastEvalResultIndex < 0)
                throw new InvalidOperationException("Reference to last eval result before first eval");

            return InstructionsResultValues.Span[LastEvalResultIndex];
        }
    }

    readonly Dictionary<Expression, StackFrameInstructions> stackFrameDict = [];

    StackFrame StackFrameFromExpression(
        PineValue? expressionValue,
        Expression expression,
        PineValue environment,
        long BeginParseAndEvalCount)
    {
        var instructions = InstructionsFromExpression(expression);

        var beginTimestamp =
            expressionValue is null
            ?
            (long?)null
            :
            System.Diagnostics.Stopwatch.GetTimestamp();

        return new StackFrame(
            expressionValue,
            expression,
            instructions,
            EnvironmentValue: environment,
            new PineValue[instructions.Instructions.Count],
            BeginParseAndEvalCount: BeginParseAndEvalCount,
            BeginTimestamp: beginTimestamp);
    }

    public StackFrameInstructions InstructionsFromExpression(Expression rootExpression)
    {
        if (stackFrameDict.TryGetValue(rootExpression, out var cachedInstructions))
        {
            return cachedInstructions;
        }

        var instructions = InstructionsFromExpressionLessCache(rootExpression);

        stackFrameDict[rootExpression] = instructions;

        return instructions;
    }

    public static StackFrameInstructions InstructionsFromExpressionLessCache(Expression rootExpression) =>
        new([.. InstructionsFromExpressionTransitive(rootExpression).Append(StackInstruction.Return)]);

    public static IReadOnlyList<StackInstruction> InstructionsFromExpressionTransitive(
        Expression rootExpression)
    {
        var node = NodeFromExpressionTransitive(
            rootExpression,
            conditionalsToSkip: []);

        return InstructionsFromNode(
            node,
            reusableExpressionResultOffset: _ => null,
            shouldSeparate: _ => false);
    }

    public static ImperativeNode NodeFromExpressionTransitive(
        Expression rootExpression,
        ImmutableHashSet<Expression.ConditionalExpression> conditionalsToSkip)
    {
        var conditionalToSplit =
            ListComponentsOrderedForCompilation(rootExpression, skipDescendants: null)
            .OfType<Expression.ConditionalExpression>()
            .Where(c => !conditionalsToSkip.Contains(c))
            .FirstOrDefault();

        if (conditionalToSplit is not null)
        {
            var conditionNode =
                NodeFromExpressionTransitive(
                    conditionalToSplit.condition,
                    conditionalsToSkip: []);

            var falseNode =
                NodeFromExpressionTransitive(
                    conditionalToSplit.ifFalse,
                    conditionalsToSkip: []);

            var trueNode =
                NodeFromExpressionTransitive(
                    conditionalToSplit.ifTrue,
                    conditionalsToSkip: []);

            var continuationNode =
                NodeFromExpressionTransitive(
                    rootExpression,
                    conditionalsToSkip: conditionalsToSkip.Add(conditionalToSplit));

            return
                new ImperativeNode.ConditionalNode(
                    Origin: conditionalToSplit,
                    Condition: conditionNode,
                    FalseBranch: falseNode,
                    TrueBranch: trueNode,
                    Continuation: continuationNode);
        }

        return new ImperativeNode.LeafNode(rootExpression);
    }

    public static IReadOnlyList<StackInstruction> InstructionsFromNode(
        ImperativeNode imperativeNode,
        Func<Expression, int?> reusableExpressionResultOffset,
        Func<Expression, bool> shouldSeparate)
    {
        if (imperativeNode is ImperativeNode.LeafNode leaf)
        {
            var instructionExprsFiltered =
                ExpressionsToSeparateSkippingConditional(
                    leaf.Expression,
                    expressionAlreadyCovered: expr => reusableExpressionResultOffset(expr).HasValue,
                    shouldSeparate: shouldSeparate)
                .Append(leaf.Expression)
                .Distinct()
                .ToImmutableArray();

            var localInstructionIndexFromExpr = new Dictionary<Expression, int>();

            int? reusableEvalResult(Expression expr)
            {
                {
                    if (reusableExpressionResultOffset.Invoke(expr) is { } reusableIndex)
                    {
                        return reusableIndex;
                    }
                }

                {
                    if (localInstructionIndexFromExpr.TryGetValue(expr, out var earlierIndex))
                    {
                        return earlierIndex;
                    }
                }

                return null;
            }

            var instructionsOptimized =
                instructionExprsFiltered
                .Select((expression, instructionIndex) =>
                {
                    if (reusableEvalResult(expression) is { } reusableIndex)
                    {
                        return new Expression.StackReferenceExpression(reusableIndex - instructionIndex);
                    }

                    localInstructionIndexFromExpr.Add(expression, instructionIndex);

                    return expression;
                })
                .Select((instruction, instructionIndex) =>
                OptimizeExpressionTransitive(instruction, instructionIndex, reusableEvalResult))
                .Select(StackInstruction.Eval)
                .ToImmutableArray();

            return instructionsOptimized;
        }

        if (imperativeNode is ImperativeNode.ConditionalNode conditional)
        {
            var unconditionalNodesUnderCondition =
                ImperativeNode.EnumerateSelfAndDescendants(
                    conditional.Condition,
                    skipBranches: true)
                .ToImmutableArray();

            var unconditionalExprUnderCondition = new HashSet<Expression>();

            foreach (var nodeUnderCondition in unconditionalNodesUnderCondition)
            {
                if (nodeUnderCondition is not ImperativeNode.LeafNode leafNode)
                    continue;

                foreach (var expression in
                    Expression.EnumerateSelfAndDescendants(
                        leafNode.Expression,
                        skipDescendants:
                        expr => reusableExpressionResultOffset(expr).HasValue))
                {
                    unconditionalExprUnderCondition.Add(expression);
                }
            }

            var otherNodes =
                new[] { conditional.FalseBranch, conditional.TrueBranch, conditional.Continuation }
                .SelectMany(otherRoot =>
                ImperativeNode.EnumerateSelfAndDescendants(
                    otherRoot,
                    skipBranches: false))
                .ToImmutableArray();

            var candidatesForCSE = new HashSet<Expression>();

            foreach (var otherNode in otherNodes)
            {
                if (otherNode is not ImperativeNode.LeafNode otherLeafNode)
                    continue;

                foreach (var otherExpr in Expression.EnumerateSelfAndDescendants(
                    otherLeafNode.Expression,
                    skipDescendants: candidatesForCSE.Contains))
                {
                    if (!ExpressionLargeEnoughForCSE(otherExpr))
                        continue;

                    if (reusableExpressionResultOffset(otherExpr).HasValue)
                        continue;

                    if (unconditionalExprUnderCondition.Contains(otherExpr))
                    {
                        candidatesForCSE.Add(otherExpr);
                    }
                }
            }

            var conditionInstructions =
                InstructionsFromNode(
                    conditional.Condition,
                    reusableExpressionResultOffset,
                    shouldSeparate: candidatesForCSE.Contains);

            var reusableFromCondition = new Dictionary<Expression, int>();

            for (int i = 0; i < conditionInstructions.Count; i++)
            {
                var instruction = conditionInstructions[i];

                if (instruction is not StackInstruction.EvalInstruction evalInstruction)
                {
                    // Only include the range of instructions that will be executed unconditionally.
                    break;
                }

                reusableFromCondition[evalInstruction.Expression] = i;
            }

            IReadOnlyList<StackInstruction> ifInvalidInstructions =
                [StackInstruction.Eval(new Expression.LiteralExpression(PineValue.EmptyList))];

            var instructionsBeforeBranchFalseCount =
                conditionInstructions.Count + 1 + ifInvalidInstructions.Count + 1;

            int? reusableResultOffsetForBranchFalse(Expression expression)
            {
                if (reusableExpressionResultOffset(expression) is { } earlierOffset)
                {
                    return earlierOffset - instructionsBeforeBranchFalseCount;
                }

                if (reusableFromCondition.TryGetValue(expression, out var offsetFromCondition))
                {
                    return offsetFromCondition - conditionInstructions.Count;
                }

                return null;
            }

            IReadOnlyList<StackInstruction> ifFalseInstructions =
                [.. InstructionsFromNode(
                    conditional.FalseBranch,
                    reusableResultOffsetForBranchFalse,
                    shouldSeparate: _ => false)];

            IReadOnlyList<StackInstruction> ifTrueInstructions =
                [.. InstructionsFromNode(
                    conditional.TrueBranch,
                    reusableExpressionResultOffset:
                    expr => reusableResultOffsetForBranchFalse(expr) - (ifFalseInstructions.Count + 1),
                    shouldSeparate: _ => false)];

            IReadOnlyList<StackInstruction> ifInvalidInstructionsAndJump =
                [.. ifInvalidInstructions,
                StackInstruction.Jump(ifFalseInstructions.Count + 1 + ifTrueInstructions.Count + 1)];

            IReadOnlyList<StackInstruction> ifFalseInstructionsAndJump =
                [.. ifFalseInstructions,
                StackInstruction.Jump(ifTrueInstructions.Count + 1)];

            var branchInstruction =
                new StackInstruction.ConditionalJumpInstruction(
                    IfFalseOffset: ifInvalidInstructionsAndJump.Count,
                    IfTrueOffset: ifInvalidInstructionsAndJump.Count + ifFalseInstructionsAndJump.Count);

            IReadOnlyList<StackInstruction> instructionsBeforeContinuation =
                [..conditionInstructions,
                branchInstruction,
                ..ifInvalidInstructionsAndJump,
                ..ifFalseInstructionsAndJump,
                ..ifTrueInstructions,
                new StackInstruction.CopyLastAssignedInstruction()];

            int? reusableResultOffsetForContinuation(Expression expression)
            {
                if (expression == conditional.Origin)
                {
                    return -1;
                }

                return
                    reusableResultOffsetForBranchFalse(expression) -
                    (instructionsBeforeContinuation.Count - instructionsBeforeBranchFalseCount);
            }

            IReadOnlyList<StackInstruction> continuationInstructions =
                [.. InstructionsFromNode(
                    conditional.Continuation,
                    reusableResultOffsetForContinuation,
                    shouldSeparate: _ => false)];

            IReadOnlyList<StackInstruction> mergedInstructions =
                [..instructionsBeforeContinuation,
                ..continuationInstructions];

            return mergedInstructions;
        }

        throw new NotImplementedException(
            "Unexpected node type: " + imperativeNode.GetType().FullName);
    }

    static Expression OptimizeExpressionTransitive(
        Expression expression,
        int instructionIndex,
        Func<Expression, int?> reusableEvalResultOffset)
    {
        return
            CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                findReplacement:
                expr => OptimizeExpressionStep(
                    expr,
                    instructionIndex,
                    reusableEvalResultOffset),
                expression).expr;
    }

    static Expression? OptimizeExpressionStep(
        Expression expression,
        int instructionIndex,
        Func<Expression, int?> reusableEvalResultOffset)
    {
        if (expression is Expression.EnvironmentExpression)
            return null;

        if (reusableEvalResultOffset(expression) is { } reusableOffset)
        {
            var offset = reusableOffset - instructionIndex;

            if (offset != 0)
            {
                if (offset >= 0)
                {
                    throw new Exception(
                        "Found non-negative offset for stack ref expr: " + offset +
                        " (selfIndex is " + instructionIndex + ")");
                }

                return new Expression.StackReferenceExpression(offset: offset);
            }
        }

        {
            /*
             * Skip over all expressions that we do not descend into when enumerating the components.
             * (EnumerateComponentsOrderedForCompilation)
             * */

            if (expression is Expression.ConditionalExpression)
            {
                // Return non-null value to stop descend.
                return expression;
            }
        }

        if (TryFuseStep(expression) is { } fused)
        {
            return
                OptimizeExpressionTransitive(
                    fused,
                    instructionIndex,
                    reusableEvalResultOffset: reusableEvalResultOffset);
        }

        return null;
    }

    static IReadOnlyList<Expression> ExpressionsToSeparateSkippingConditional(
        Expression rootExpression,
        Func<Expression, bool> expressionAlreadyCovered,
        Func<Expression, bool> shouldSeparate)
    {
        var allExpressions =
            ListComponentsOrderedForCompilation(
                rootExpression,
                skipDescendants: null)
            .ToImmutableArray();

        var allExpressionsCount = new Dictionary<Expression, int>();

        foreach (var expression in allExpressions)
        {
            if (allExpressionsCount.TryGetValue(expression, out var count))
            {
                allExpressionsCount[expression] = count + 1;
            }
            else
            {
                allExpressionsCount[expression] = 1;
            }
        }

        var allExpressionsExceptUnderDuplicate =
            ListComponentsOrderedForCompilation(
                rootExpression,
                skipDescendants: node => 1 < allExpressionsCount[node])
            .ToImmutableArray();

        var allExpressionsExceptUnderDuplicateCount = new Dictionary<Expression, int>();

        foreach (var expression in allExpressionsExceptUnderDuplicate)
        {
            if (allExpressionsExceptUnderDuplicateCount.TryGetValue(expression, out var count))
            {
                allExpressionsExceptUnderDuplicateCount[expression] = count + 1;
            }
            else
            {
                allExpressionsExceptUnderDuplicateCount[expression] = 1;
            }
        }

        var separatedInstructions =
            allExpressions
            .SelectMany(expression =>
            {
                if (expressionAlreadyCovered(expression))
                    return [];

                if (shouldSeparate(expression))
                    return [expression];

                if (expression is Expression.ParseAndEvalExpression parseAndEval)
                {
                    return (IReadOnlyList<Expression>)[expression];
                }

                if (ExpressionLargeEnoughForCSE(expression) &&
                allExpressionsExceptUnderDuplicateCount.TryGetValue(expression, out var exprInstCount) && 1 < exprInstCount)
                {
                    return [expression];
                }

                return [];
            })
            .ToImmutableArray();

        return separatedInstructions;
    }

    public static IEnumerable<Expression> ListComponentsOrderedForCompilation(
        Expression rootExpression,
        Func<Expression, bool>? skipDescendants)
    {
        var stack = new Stack<Expression>();
        var deepestDescendants = new Stack<Expression>();

        stack.Push(rootExpression);

        while (stack.TryPop(out var expression))
        {
            if (skipDescendants?.Invoke(expression) ?? false)
            {
                deepestDescendants.Push(expression);
                continue;
            }

            if (expression is Expression.ListExpression list)
            {
                for (var i = 0; i < list.List.Count; ++i)
                {
                    stack.Push(list.List[i]);
                }
            }

            if (expression is Expression.ParseAndEvalExpression parseAndEval)
            {
                stack.Push(parseAndEval.expression);
                stack.Push(parseAndEval.environment);
            }

            if (expression is Expression.KernelApplicationExpression kernelApp)
            {
                stack.Push(kernelApp.argument);
            }

            if (expression is Expression.ConditionalExpression conditional)
            {
                stack.Push(conditional.condition);

                /*
                 *
                 * For now, we create a new stack frame for each conditional expression.
                 * Therefore do not descend into the branches of the conditional expression.

                stack.Push(conditional.ifTrue);
                stack.Push(conditional.ifFalse);
                */
            }

            if (expression is Expression.StringTagExpression stringTag)
            {
                stack.Push(stringTag.tagged);
            }

            deepestDescendants.Push(expression);
        }

        while (deepestDescendants.Count > 0)
        {
            yield return deepestDescendants.Pop();
        }
    }

    static bool ExpressionLargeEnoughForCSE(Expression expression)
    {
        if (expression is Expression.KernelApplicationExpression)
            return true;

        if (expression is Expression.ListExpression list)
        {
            for (int i = 0; i < list.List.Count; ++i)
            {
                if (ExpressionLargeEnoughForCSE(list.List[i]))
                    return true;
            }

            return false;
        }

        return false;
    }

    public static Expression? TryFuseStep(Expression expression)
    {
        if (TryMapToKernelApplications_Skip_ListHead_Path_Expression(expression) is { } fused)
        {
            return fused;
        }

        if (TryMapToKernelApplication_Equal_Two(expression) is { } fusedEqualTwo)
        {
            return fusedEqualTwo;
        }

        return null;
    }

    public static Expression.KernelApplications_Skip_ListHead_Path_Expression?
        TryMapToKernelApplications_Skip_ListHead_Path_Expression(Expression expression)
    {
        if (expression is not Expression.KernelApplicationExpression kernelApp)
            return null;

        if (kernelApp.functionName is not nameof(KernelFunction.list_head))
            return null;

        if (kernelApp.argument is not Expression.KernelApplicationExpression innerKernelApp)
        {
            return continueWithSkipCount(skipCount: 0, kernelApp.argument);
        }

        if (innerKernelApp.functionName is not nameof(KernelFunction.skip))
            return null;

        if (innerKernelApp.argument is not Expression.ListExpression skipListExpr)
            return null;

        if (skipListExpr.List.Count is not 2)
            return null;

        var skipCountValueExpr = skipListExpr.List[0];

        /*
        if (!Expression.IsIndependent(skipCountValueExpr))
            return null;
        */

        if (Expression.EnumerateSelfAndDescendants(skipCountValueExpr)
            .Any(desc =>
            desc is Expression.EnvironmentExpression ||
            desc is Expression.ParseAndEvalExpression ||
            desc is Expression.StackReferenceExpression))
        {
            return null;
        }

        if (CompilePineToDotNet.ReducePineExpression.TryEvaluateExpressionIndependent(skipCountValueExpr)
            is not Result<string, PineValue>.Ok skipCountEvalOk)
            return null;

        if (KernelFunction.SignedIntegerFromValueRelaxed(skipCountEvalOk.Value) is not { } skipCount)
            return null;

        var currentArg = skipListExpr.List[1];

        return continueWithSkipCount((int)skipCount, currentArg);

        static Expression.KernelApplications_Skip_ListHead_Path_Expression continueWithSkipCount(
            int skipCount,
            Expression currentArg)
        {
            if (TryMapToKernelApplications_Skip_ListHead_Path_Expression(currentArg) is { } pathContinued)
            {
                return
                    pathContinued
                    with
                    {
                        SkipCounts = (int[])[.. pathContinued.SkipCounts.Span, skipCount]
                    };
            }

            return new Expression.KernelApplications_Skip_ListHead_Path_Expression(
                SkipCounts: (int[])[skipCount],
                Argument: currentArg);
        }
    }

    public static Expression.KernelApplication_Equal_Two? TryMapToKernelApplication_Equal_Two(Expression expression)
    {
        if (expression is not Expression.KernelApplicationExpression kernelApp)
            return null;

        if (kernelApp.functionName is not nameof(KernelFunction.equal))
            return null;

        if (kernelApp.argument is not Expression.ListExpression listExpr)
            return null;

        if (listExpr.List.Count is not 2)
            return null;

        return new Expression.KernelApplication_Equal_Two(
            left: listExpr.List[0],
            right: listExpr.List[1]);
    }

    public record EvaluationConfig(
        int? ParseAndEvalCountLimit);

    public Result<string, PineValue> EvaluateExpressionDefault(
        Expression rootExpression,
        PineValue rootEnvironment,
        EvaluationConfig config)
    {
        int parseAndEvalCount = 0;

        var stack = new Stack<StackFrame>();

        stack.Push(
            StackFrameFromExpression(
                expressionValue: null,
                rootExpression,
                rootEnvironment,
                BeginParseAndEvalCount: parseAndEvalCount));

        while (true)
        {
            var currentFrame = stack.Peek();

            if (currentFrame.Instructions.Instructions.Count <= currentFrame.InstructionPointer)
            {
                return
                    "Instruction pointer out of bounds. Missing explicit return instruction.";
            }

            ReadOnlyMemory<PineValue> stackPrevValues =
                currentFrame.InstructionsResultValues[..currentFrame.InstructionPointer];

            var currentInstruction = currentFrame.Instructions.Instructions[currentFrame.InstructionPointer];

            if (currentInstruction is StackInstruction.ReturnInstruction)
            {
                var lastAssignedIndex = currentFrame.LastEvalResultIndex;

                if (lastAssignedIndex < 0)
                {
                    return "Return instruction before assignment";
                }

                var frameReturnValue =
                    currentFrame.InstructionsResultValues.Span[lastAssignedIndex];

                if (currentFrame.ExpressionValue is { } currentFrameExprValue && EvalCache is { } evalCache &&
                    currentFrame.BeginTimestamp is { } beginTimestamp)
                {
                    var frameElapsedTime = System.Diagnostics.Stopwatch.GetElapsedTime(beginTimestamp);

                    if (frameElapsedTime.TotalMilliseconds > 3)
                    {
                        evalCache.TryAdd(
                            new EvalCacheEntryKey(currentFrameExprValue, currentFrame.EnvironmentValue),
                            frameReturnValue);
                    }

                    reportFunctionApplication?.Invoke(
                        new EvaluationReport(
                            ExpressionValue: currentFrameExprValue,
                            currentFrame.Expression,
                            currentFrame.EnvironmentValue,
                            ParseAndEvalCount: parseAndEvalCount - currentFrame.BeginParseAndEvalCount,
                            frameElapsedTime,
                            frameReturnValue));
                }

                stack.Pop();

                if (stack.Count is 0)
                {
                    return frameReturnValue;
                }

                var previousFrame = stack.Peek();

                previousFrame.PushInstructionResult(frameReturnValue);

                continue;
            }

            if (currentInstruction is StackInstruction.CopyLastAssignedInstruction)
            {
                var lastAssignedIndex = currentFrame.LastEvalResultIndex;

                if (lastAssignedIndex < 0)
                {
                    return "CopyLastAssignedInstruction before assignment";
                }

                var lastAssignedValue = currentFrame.InstructionsResultValues.Span[lastAssignedIndex];

                currentFrame.PushInstructionResult(lastAssignedValue);

                continue;
            }

            if (currentInstruction is StackInstruction.EvalInstruction evalInstr)
            {
                if (evalInstr.Expression is Expression.ParseAndEvalExpression parseAndEval)
                {
                    {
                        ++parseAndEvalCount;

                        if (config.ParseAndEvalCountLimit is { } limit && parseAndEvalCount > limit)
                        {
                            return "Parse and eval count limit exceeded: " + limit;
                        }
                    }

                    var expressionValue =
                        EvaluateExpressionDefaultLessStack(
                            parseAndEval.expression,
                            currentFrame.EnvironmentValue,
                            stackPrevValues: stackPrevValues);

                    var environmentValue =
                        EvaluateExpressionDefaultLessStack(
                            parseAndEval.environment,
                            currentFrame.EnvironmentValue,
                            stackPrevValues: stackPrevValues);

                    if (EvalCache is { } evalCache)
                    {
                        var evalCacheKey = new EvalCacheEntryKey(expressionValue, environmentValue);

                        if (evalCache.TryGetValue(evalCacheKey, out var cachedValue) && cachedValue is not null)
                        {
                            currentFrame.PushInstructionResult(cachedValue);

                            continue;
                        }
                    }

                    var parseResult = parseExpressionDelegate(expressionValue);

                    if (parseResult is Result<string, Expression>.Err parseErr)
                    {
                        return
                            "Failed to parse expression from value: " + parseErr.Value +
                            " - expressionValue is " + DescribeValueForErrorMessage(expressionValue) +
                            " - environmentValue is " + DescribeValueForErrorMessage(expressionValue);
                    }

                    if (parseResult is not Result<string, Expression>.Ok parseOk)
                    {
                        throw new NotImplementedException("Unexpected result type: " + parseResult.GetType().FullName);
                    }

                    stack.Push(
                        StackFrameFromExpression(
                            expressionValue: expressionValue,
                            parseOk.Value,
                            environmentValue,
                            BeginParseAndEvalCount: parseAndEvalCount));

                    continue;
                }

                if (evalInstr.Expression is Expression.ConditionalExpression conditionalExpr)
                {
                    var conditionValue =
                        EvaluateExpressionDefaultLessStack(
                            conditionalExpr.condition,
                            currentFrame.EnvironmentValue,
                            stackPrevValues: stackPrevValues);

                    var expressionToContinueWith =
                        conditionValue == PineVMValues.TrueValue
                        ?
                        conditionalExpr.ifTrue
                        :
                        conditionValue == PineVMValues.FalseValue
                        ?
                        conditionalExpr.ifFalse
                        :
                        null;

                    if (expressionToContinueWith is not null)
                    {
                        if (ExpressionShouldGetNewStackFrame(expressionToContinueWith))
                        {
                            stack.Push(
                                StackFrameFromExpression(
                                    expressionValue: null,
                                    expressionToContinueWith,
                                    currentFrame.EnvironmentValue,
                                    BeginParseAndEvalCount: parseAndEvalCount));

                            continue;
                        }

                        var evalBranchResult =
                            EvaluateExpressionDefaultLessStack(
                                expressionToContinueWith,
                                currentFrame.EnvironmentValue,
                                stackPrevValues: stackPrevValues);

                        currentFrame.PushInstructionResult(evalBranchResult); ;

                        continue;
                    }
                    else
                    {
                        currentFrame.PushInstructionResult(PineValue.EmptyList);

                        continue;
                    }
                }

                var evalResult =
                    EvaluateExpressionDefaultLessStack(
                        evalInstr.Expression,
                        currentFrame.EnvironmentValue,
                        stackPrevValues: stackPrevValues);

                currentFrame.PushInstructionResult(evalResult);

                continue;
            }

            if (currentInstruction is StackInstruction.JumpInstruction jumpInstruction)
            {
                currentFrame.InstructionPointer += jumpInstruction.Offset;
                continue;
            }

            if (currentInstruction is StackInstruction.ConditionalJumpInstruction conditionalStatement)
            {
                var conditionValue = currentFrame.LastEvalResult();

                currentFrame.InstructionPointer++;

                if (conditionValue == PineVMValues.FalseValue)
                {
                    currentFrame.InstructionPointer += conditionalStatement.IfFalseOffset;
                    continue;
                }

                if (conditionValue == PineVMValues.TrueValue)
                {
                    currentFrame.InstructionPointer += conditionalStatement.IfTrueOffset;
                    continue;
                }

                continue;
            }

            return "Unexpected instruction type: " + currentInstruction.GetType().FullName;
        }
    }

    private static bool ExpressionShouldGetNewStackFrame(Expression expression)
    {
        if (expression is Expression.LiteralExpression)
            return false;

        if (expression is Expression.EnvironmentExpression)
            return false;

        if (expression is Expression.ParseAndEvalExpression)
            return true;

        if (expression is Expression.KernelApplicationExpression kernelApp)
        {
            return ExpressionShouldGetNewStackFrame(kernelApp.argument);
        }

        if (expression is Expression.ConditionalExpression conditional)
        {
            return
                ExpressionShouldGetNewStackFrame(conditional.condition) ||
                ExpressionShouldGetNewStackFrame(conditional.ifTrue) ||
                ExpressionShouldGetNewStackFrame(conditional.ifFalse);
        }

        if (expression is Expression.ListExpression list)
        {
            for (var i = 0; i < list.List.Count; i++)
            {
                if (ExpressionShouldGetNewStackFrame(list.List[i]))
                {
                    return true;
                }
            }

            return false;
        }

        if (expression is Expression.StringTagExpression stringTag)
            return ExpressionShouldGetNewStackFrame(stringTag.tagged);

        if (expression is Expression.DelegatingExpression)
            return false;

        if (expression is Expression.StackReferenceExpression)
            return false;

        if (expression is Expression.KernelApplications_Skip_ListHead_Path_Expression)
            return false;

        if (expression is Expression.KernelApplication_Equal_Two)
            return false;

        throw new NotImplementedException(
            "Unexpected shape of expression: " + expression.GetType().FullName);
    }

    private PineValue EvaluateExpressionDefaultLessStack(
        Expression expression,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        if (expression is Expression.LiteralExpression literalExpression)
            return literalExpression.Value;

        if (expression is Expression.ListExpression listExpression)
        {
            return EvaluateListExpression(listExpression, environment, stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.ParseAndEvalExpression applicationExpression)
        {
            return
                EvaluateParseAndEvalExpression(
                    applicationExpression,
                    environment,
                    stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.KernelApplicationExpression kernelApplicationExpression)
        {
            return
                EvaluateKernelApplicationExpression(
                    environment,
                    kernelApplicationExpression,
                    stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.ConditionalExpression conditionalExpression)
        {
            return EvaluateConditionalExpression(
                environment,
                conditionalExpression,
                stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.EnvironmentExpression)
        {
            return environment;
        }

        if (expression is Expression.StringTagExpression stringTagExpression)
        {
            return EvaluateExpressionDefaultLessStack(
                stringTagExpression.tagged,
                environment,
                stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.DelegatingExpression delegatingExpr)
        {
            return
                delegatingExpr.Delegate.Invoke(
                    (expression, environment) =>
                    EvaluateExpressionDefault(
                        expression,
                        environment,
                        config: new EvaluationConfig(ParseAndEvalCountLimit: null)),
                    environment)
                .Extract(err => throw new GenericEvalException("Error from delegate: " + err));
        }

        if (expression is Expression.StackReferenceExpression stackRef)
        {
            var index = stackPrevValues.Length + stackRef.offset;

            if (index < 0 || index >= stackPrevValues.Length)
            {
                throw new InvalidExpressionException(
                    "Invalid stack reference: offset " + stackRef.offset +
                    " from " + stackPrevValues.Length +
                    " results in index " + index);
            }

            var content = stackPrevValues.Span[index];

            if (content is null)
            {
                throw new InvalidExpressionException(
                    "Null value in stack reference: offset " + stackRef.offset +
                    " from " + stackPrevValues.Length +
                    " results in index " + index);
            }

            return content;
        }

        if (expression is Expression.KernelApplications_Skip_ListHead_Path_Expression kernelApplicationsSkipListHead)
        {
            return
                EvaluateKernelApplications_Skip_ListHead_Expression(
                    environment: environment,
                    kernelApplicationsSkipListHead,
                    stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.KernelApplication_Equal_Two kernelApplicationEqualTwo)
        {
            var leftValue = EvaluateExpressionDefaultLessStack(
                kernelApplicationEqualTwo.left,
                environment,
                stackPrevValues: stackPrevValues);

            var rightValue = EvaluateExpressionDefaultLessStack(
                kernelApplicationEqualTwo.right,
                environment,
                stackPrevValues: stackPrevValues);

            return KernelFunction.equal(leftValue, rightValue);
        }

        throw new NotImplementedException(
            "Unexpected shape of expression: " + expression.GetType().FullName);
    }

    public PineValue EvaluateListExpression(
        Expression.ListExpression listExpression,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var listItems = new List<PineValue>(listExpression.List.Count);

        for (var i = 0; i < listExpression.List.Count; i++)
        {
            var item = listExpression.List[i];

            var itemResult = EvaluateExpressionDefaultLessStack(
                item,
                environment,
                stackPrevValues: stackPrevValues);
            listItems.Add(itemResult);
        }

        return PineValue.List(listItems);
    }

    public PineValue EvaluateParseAndEvalExpression(
        Expression.ParseAndEvalExpression parseAndEval,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var environmentValue =
            EvaluateExpressionDefaultLessStack(
                parseAndEval.environment,
                environment,
                stackPrevValues: stackPrevValues);

        var expressionValue =
            EvaluateExpressionDefaultLessStack(
                parseAndEval.expression,
                environment,
                stackPrevValues: stackPrevValues);

        var parseResult = parseExpressionDelegate(expressionValue);

        if (parseResult is Result<string, Expression>.Err parseErr)
        {
            var message =
                "Failed to parse expression from value: " + parseErr.Value +
                " - expressionValue is " + DescribeValueForErrorMessage(expressionValue) +
                " - environmentValue is " + DescribeValueForErrorMessage(expressionValue);

            throw new ParseExpressionException(message);
        }

        if (parseResult is not Result<string, Expression>.Ok parseOk)
        {
            throw new NotImplementedException("Unexpected result type: " + parseResult.GetType().FullName);
        }

        if (environmentValue is PineValue.ListValue list)
        {
            FunctionApplicationMaxEnvSize =
            FunctionApplicationMaxEnvSize < list.Elements.Count ? list.Elements.Count : FunctionApplicationMaxEnvSize;
        }

        return
            EvaluateExpression(environment: environmentValue, expression: parseOk.Value)
            .Extract(err => throw new Exception("Failed continuing parse and eval: " + err));
    }

    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        PineValueAsString.StringFromValue(pineValue)
        .Unpack(fromErr: _ => "not a string", fromOk: asString => "string \'" + asString + "\'");

    public PineValue EvaluateKernelApplicationExpression(
        PineValue environment,
        Expression.KernelApplicationExpression application,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        if (application.functionName is nameof(KernelFunction.list_head) &&
            application.argument is Expression.KernelApplicationExpression innerKernelApplication)
        {
            if (innerKernelApplication.functionName == nameof(KernelFunction.skip) &&
                innerKernelApplication.argument is Expression.ListExpression skipListExpr &&
                skipListExpr.List.Count is 2)
            {
                var skipValue =
                    EvaluateExpressionDefaultLessStack(
                        skipListExpr.List[0],
                        environment,
                        stackPrevValues);

                if (KernelFunction.SignedIntegerFromValueRelaxed(skipValue) is { } skipCount)
                {
                    if (EvaluateExpressionDefaultLessStack(
                        skipListExpr.List[1],
                        environment,
                        stackPrevValues) is PineValue.ListValue list)
                    {
                        if (list.Elements.Count < 1 || list.Elements.Count <= skipCount)
                        {
                            return PineValue.EmptyList;
                        }

                        return list.Elements[skipCount < 0 ? 0 : (int)skipCount];
                    }
                    else
                    {
                        return PineValue.EmptyList;
                    }
                }
            }
        }

        return EvaluateKernelApplicationExpressionGeneric(environment, application, stackPrevValues);
    }

    public PineValue EvaluateKernelApplicationExpressionGeneric(
        PineValue environment,
        Expression.KernelApplicationExpression application,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var argumentValue = EvaluateExpressionDefaultLessStack(application.argument, environment, stackPrevValues);

        return application.function(argumentValue);
    }

    public PineValue EvaluateKernelApplications_Skip_ListHead_Expression(
        PineValue environment,
        Expression.KernelApplications_Skip_ListHead_Path_Expression application,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var argumentValue =
            EvaluateExpressionDefaultLessStack(
                application.Argument,
                environment,
                stackPrevValues: stackPrevValues);

        return ValueFromPathInValueOrEmptyList(argumentValue, application.SkipCounts.Span);
    }

    public static PineValue ValueFromPathInValueOrEmptyList(
        PineValue environment,
        ReadOnlySpan<int> path)
    {
        if (path.Length is 0)
            return environment;

        if (environment is not PineValue.ListValue listValue)
            return PineValue.EmptyList;

        var skipCount = path[0];

        if (path[0] >= listValue.Elements.Count)
            return PineValue.EmptyList;

        return
            ValueFromPathInValueOrEmptyList(
                listValue.Elements[skipCount < 0 ? 0 : skipCount],
                path[1..]);
    }

    public PineValue EvaluateConditionalExpression(
        PineValue environment,
        Expression.ConditionalExpression conditional,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var conditionValue =
            EvaluateExpressionDefaultLessStack(
                conditional.condition,
                environment,
                stackPrevValues: stackPrevValues);

        return
            conditionValue == PineVMValues.TrueValue
            ?
            EvaluateExpressionDefaultLessStack(
                conditional.ifTrue,
                environment,
                stackPrevValues: stackPrevValues)
            :
            conditionValue == PineVMValues.FalseValue
            ?
            EvaluateExpressionDefaultLessStack(
                conditional.ifFalse,
                environment,
                stackPrevValues: stackPrevValues)
            :
            PineValue.EmptyList;
    }
}
