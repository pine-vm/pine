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
        new(InstructionsFromExpressionTransitive(rootExpression));

    public static IReadOnlyList<StackInstruction> InstructionsFromExpressionTransitive(Expression rootExpression)
    {
        var conditionalToSplit =
            Expression.EnumerateSelfAndDescendants(rootExpression)
            .OfType<Expression.ConditionalExpression>()
            .FirstOrDefault(conditional =>
            ExpressionShouldGetNewStackFrame(conditional.ifFalse) ||
            ExpressionShouldGetNewStackFrame(conditional.ifTrue));

        if (conditionalToSplit is not null)
        {
            var ifFalseRootExpr =
                CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                    findReplacement:
                    expr => expr == conditionalToSplit ? conditionalToSplit.ifFalse : null,
                    rootExpression).expr;

            var ifTrueRootExpr =
                CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                    findReplacement:
                    expr => expr == conditionalToSplit ? conditionalToSplit.ifTrue : null,
                    rootExpression).expr;

            var ifInvalidRootExpr =
                CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                    findReplacement:
                    expr => expr == conditionalToSplit ? new Expression.LiteralExpression(PineValue.EmptyList) : null,
                    rootExpression).expr;

            var ifInvalidInstructionsFlat = InstructionsFromExpressionTransitive(ifInvalidRootExpr);
            var ifFalseInstructionsFlat = InstructionsFromExpressionTransitive(ifFalseRootExpr);
            var ifTrueInstructionsFlat = InstructionsFromExpressionTransitive(ifTrueRootExpr);

            var branchInstruction =
                new StackInstruction.ConditionalJumpInstruction(
                    Condition: conditionalToSplit.condition,
                    IfFalseOffset: ifInvalidInstructionsFlat.Count,
                    IfTrueOffset: ifInvalidInstructionsFlat.Count + ifFalseInstructionsFlat.Count);

            var mergedInstructions =
                (IReadOnlyList<StackInstruction>)
                [branchInstruction,
                ..ifInvalidInstructionsFlat,
                ..ifFalseInstructionsFlat,
                ..ifTrueInstructionsFlat];

            return mergedInstructions;
        }

        var instructionsBeforeFilter =
            InstructionsFromExpressionSkippingConditional(rootExpression)
            .Concat([StackInstruction.Eval(rootExpression), StackInstruction.Return])
            .ToImmutableArray();

        var instructionsFiltered =
            SkipConsecutiveDuplicateInstructions(instructionsBeforeFilter)
            .ToImmutableArray();

        var localInstructionIndexFromExpr = new Dictionary<Expression, int>();

        StackInstruction OptimizeInstruction(StackInstruction instruction, int selfIndex)
        {
            return
                 StackInstruction.TransformExpressionWithOptionalReplacement(
                    findReplacement:
                    descendant =>
                    {
                        if (localInstructionIndexFromExpr.TryGetValue(descendant, out var instructionIndex))
                        {
                            var offset = instructionIndex - selfIndex;

                            if (offset != 0)
                            {
                                if (offset >= 0)
                                {
                                    throw new Exception(
                                        "Found non-negative offset for stack ref expr: " + offset +
                                        " (selfIndex is " + selfIndex + ")");
                                }

                                return new Expression.StackReferenceExpression(offset: offset);
                            }
                        }

                        {
                            /*
                             * Skip over all expressions that we do not descend into when enumerating the components.
                             * (EnumerateComponentsOrderedForCompilation)
                             * */

                            if (descendant is Expression.ConditionalExpression)
                            {
                                // Return non-null value to stop descend.
                                return descendant;
                            }
                        }

                        if (TryFuseTransitive(descendant) is { } fused)
                        {
                            return fused;
                        }

                        return null;
                    },
                    instruction: instruction);
        }

        var instructionsOptimized =
            instructionsFiltered
            .Select((instruction, instructionIndex) =>
            {
                if (instruction is StackInstruction.EvalInstruction evalInst)
                {
                    if (localInstructionIndexFromExpr.TryGetValue(evalInst.Expression, out var earlierIndex))
                    {
                        return StackInstruction.Eval(
                            new Expression.StackReferenceExpression(earlierIndex - instructionIndex));
                    }

                    localInstructionIndexFromExpr.Add(evalInst.Expression, instructionIndex);
                }

                return instruction;
            })
            .Select(OptimizeInstruction)
            .ToImmutableArray();

        return instructionsOptimized;
    }

    static IEnumerable<StackInstruction> SkipConsecutiveDuplicateInstructions(IEnumerable<StackInstruction> sequence)
    {
        StackInstruction? prevInstruction = null;

        foreach (var item in sequence)
        {
            if (item is StackInstruction.EvalInstruction && item == prevInstruction)
            {
                continue;
            }

            prevInstruction = item;

            yield return item;
        }
    }

    static IReadOnlyList<StackInstruction> InstructionsFromExpressionSkippingConditional(Expression rootExpression)
    {
        var allExpressions =
            EnumerateComponentsOrderedForCompilation(
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
            EnumerateComponentsOrderedForCompilation(
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

        static IReadOnlyList<StackInstruction>? instructionsFromExpression(Expression expression)
        {
            if (expression is Expression.ParseAndEvalExpression parseAndEval)
            {
                return [StackInstruction.Eval(expression)];
            }

            return null;
        }

        var separatedInstructions =
            allExpressions
            .SelectMany(expression =>
            {
                if (instructionsFromExpression(expression) is { } instructions)
                {
                    return instructions;
                }

                if (ExpressionLargeEnoughForCSE(expression) &&
                allExpressionsExceptUnderDuplicateCount.TryGetValue(expression, out var exprInstCount) && 1 < exprInstCount)
                {
                    return
                    (IReadOnlyList<StackInstruction>)
                    [StackInstruction.Eval(expression)];
                }

                return [];
            })
            .ToImmutableArray();

        return separatedInstructions;
    }

    public static IEnumerable<Expression> EnumerateComponentsOrderedForCompilation(
        Expression expression,
        Func<Expression, bool>? skipDescendants)
    {
        if (skipDescendants?.Invoke(expression) ?? false)
        {
            yield return expression;
            yield break;
        }

        if (expression is Expression.ListExpression list)
        {
            foreach (var item in list.List)
            {
                foreach (var descendant in EnumerateComponentsOrderedForCompilation(item, skipDescendants))
                {
                    yield return descendant;
                }
            }
        }

        if (expression is Expression.ParseAndEvalExpression parseAndEval)
        {
            foreach (var descendant in EnumerateComponentsOrderedForCompilation(parseAndEval.expression, skipDescendants))
            {
                yield return descendant;
            }

            foreach (var descendant in EnumerateComponentsOrderedForCompilation(parseAndEval.environment, skipDescendants))
            {
                yield return descendant;
            }
        }

        if (expression is Expression.KernelApplicationExpression kernelApp)
        {
            foreach (var descendant in EnumerateComponentsOrderedForCompilation(kernelApp.argument, skipDescendants))
            {
                yield return descendant;
            }
        }

        if (expression is Expression.ConditionalExpression conditional)
        {
            foreach (var descendant in EnumerateComponentsOrderedForCompilation(conditional.condition, skipDescendants))
            {
                yield return descendant;
            }

            /*
             *
             * For now, we create a new stack frame for each conditional expression.
             * Therefore do not descend into the branches of the conditional expression.

            foreach (var descendant in EnumerateComponentsOrderedForCompilation(conditional.ifTrue, skipDescendants))
            {
                yield return descendant;
            }

            foreach (var descendant in EnumerateComponentsOrderedForCompilation(conditional.ifFalse, skipDescendants))
            {
                yield return descendant;
            }
            */
        }

        if (expression is Expression.StringTagExpression stringTag)
        {
            foreach (var descendant in EnumerateComponentsOrderedForCompilation(stringTag.tagged, skipDescendants))
            {
                yield return descendant;
            }
        }

        yield return expression;
    }

    static bool ExpressionLargeEnoughForCSE(Expression expression)
    {
        if (expression is Expression.KernelApplicationExpression)
            return true;

        if (expression is Expression.ListExpression list)
        {
            foreach (var item in list.List)
            {
                if (ExpressionLargeEnoughForCSE(item))
                {
                    return true;
                }
            }

            return false;
        }

        return false;
    }

    public static Expression? TryFuseTransitive(Expression expression)
    {
        if (TryMapToKernelApplications_Skip_ListHead_Expression(expression) is not { } fused)
        {
            return null;
        }

        var fusedArgument =
            CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                findReplacement: TryFuseTransitive,
                expression: fused.argument).expr;

        return
            fused with { argument = fusedArgument };
    }

    public static Expression.KernelApplications_Skip_ListHead_Expression?
        TryMapToKernelApplications_Skip_ListHead_Expression(Expression expression)
    {
        if (expression is not Expression.KernelApplicationExpression kernelApp)
            return null;

        if (kernelApp.functionName is not nameof(KernelFunction.list_head))
            return null;

        if (kernelApp.argument is not Expression.KernelApplicationExpression innerKernelApp)
            return null;

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

        return new Expression.KernelApplications_Skip_ListHead_Expression(
            skipCount: (int)skipCount,
            argument: skipListExpr.List[1]);
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
                if (currentFrame.InstructionPointer is 0)
                {
                    return "Return instruction at beginning of frame";
                }

                var frameReturnValue =
                    currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer - 1];

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

                previousFrame.InstructionsResultValues.Span[previousFrame.InstructionPointer] = frameReturnValue;

                previousFrame.InstructionPointer++;
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

                    var evalExprValueResult =
                        EvaluateExpressionDefaultLessStack(
                            parseAndEval.expression,
                            currentFrame.EnvironmentValue,
                            stackPrevValues: stackPrevValues);

                    if (evalExprValueResult is Result<string, PineValue>.Err parseFunctionError)
                    {
                        return "Failed to evaluate expr value for parse and eval: " + parseFunctionError;
                    }

                    if (evalExprValueResult is not Result<string, PineValue>.Ok evalExprValueOk)
                    {
                        throw new NotImplementedException(
                            "Unexpected result type: " + evalExprValueResult.GetType().FullName);
                    }

                    var evalEnvResult =
                        EvaluateExpressionDefaultLessStack(
                            parseAndEval.environment,
                            currentFrame.EnvironmentValue,
                            stackPrevValues: stackPrevValues);

                    if (evalEnvResult is Result<string, PineValue>.Err evalEnvError)
                    {
                        return "Failed to evaluate environment: " + evalEnvError;
                    }

                    if (evalEnvResult is not Result<string, PineValue>.Ok evalEnvOk)
                    {
                        throw new NotImplementedException(
                            "Unexpected result type: " + evalEnvResult.GetType().FullName);
                    }

                    if (EvalCache is { } evalCache)
                    {
                        var evalCacheKey = new EvalCacheEntryKey(evalExprValueOk.Value, evalEnvOk.Value);

                        if (evalCache.TryGetValue(evalCacheKey, out var cachedValue) && cachedValue is not null)
                        {
                            currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer] = cachedValue;
                            currentFrame.InstructionPointer++;
                            continue;
                        }
                    }

                    var parseResult = parseExpressionDelegate(evalExprValueOk.Value);

                    if (parseResult is Result<string, Expression>.Err parseErr)
                    {
                        return
                            "Failed to parse expression from value: " + parseErr.Value +
                            " - expressionValue is " + DescribeValueForErrorMessage(evalExprValueOk.Value) +
                            " - environmentValue is " + DescribeValueForErrorMessage(evalExprValueOk.Value);
                    }

                    if (parseResult is not Result<string, Expression>.Ok parseOk)
                    {
                        throw new NotImplementedException("Unexpected result type: " + parseResult.GetType().FullName);
                    }

                    stack.Push(
                        StackFrameFromExpression(
                            expressionValue: evalExprValueOk.Value,
                            parseOk.Value,
                            evalEnvOk.Value,
                            BeginParseAndEvalCount: parseAndEvalCount));

                    continue;
                }

                if (evalInstr.Expression is Expression.ConditionalExpression conditionalExpr)
                {
                    var evalConditionResult =
                        EvaluateExpressionDefaultLessStack(
                            conditionalExpr.condition,
                            currentFrame.EnvironmentValue,
                            stackPrevValues: stackPrevValues);

                    if (evalConditionResult is Result<string, PineValue>.Err evalConditionError)
                    {
                        return "Failed to evaluate condition: " + evalConditionError;
                    }

                    if (evalConditionResult is not Result<string, PineValue>.Ok evalConditionOk)
                    {
                        throw new NotImplementedException(
                            "Unexpected result type: " + evalConditionResult.GetType().FullName);
                    }

                    var expressionToContinueWith =
                        evalConditionOk.Value == PineVMValues.TrueValue
                        ?
                        conditionalExpr.ifTrue
                        :
                        evalConditionOk.Value == PineVMValues.FalseValue
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

                        if (evalBranchResult is Result<string, PineValue>.Err evalBranchError)
                        {
                            return "Failed to evaluate conditional branch: " + evalBranchError;
                        }

                        if (evalBranchResult is not Result<string, PineValue>.Ok evalBranchOk)
                        {
                            throw new NotImplementedException(
                                "Unexpected result type: " + evalBranchResult.GetType().FullName);
                        }

                        currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer] = evalBranchOk.Value;
                        currentFrame.InstructionPointer++;
                        continue;
                    }
                    else
                    {
                        currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer] = PineValue.EmptyList;
                        currentFrame.InstructionPointer++;
                        continue;
                    }
                }

                var evalResult =
                    EvaluateExpressionDefaultLessStack(
                        evalInstr.Expression,
                        currentFrame.EnvironmentValue,
                        stackPrevValues: stackPrevValues);

                if (evalResult is Result<string, PineValue>.Err evalError)
                {
                    return "Failed to evaluate expression: " + evalError;
                }

                if (evalResult is not Result<string, PineValue>.Ok evalOk)
                {
                    throw new NotImplementedException("Unexpected result type: " + evalResult.GetType().FullName);
                }

                currentFrame.InstructionsResultValues.Span[currentFrame.InstructionPointer] = evalOk.Value;
                currentFrame.InstructionPointer++;
                continue;
            }

            if (currentInstruction is StackInstruction.ConditionalJumpInstruction conditionalStatement)
            {
                var evalConditionResult =
                    EvaluateExpressionDefaultLessStack(
                        conditionalStatement.Condition,
                        currentFrame.EnvironmentValue,
                        stackPrevValues: stackPrevValues);

                if (evalConditionResult is Result<string, PineValue>.Err evalConditionError)
                {
                    return "Failed to evaluate condition: " + evalConditionError;
                }

                if (evalConditionResult is not Result<string, PineValue>.Ok evalConditionOk)
                {
                    throw new NotImplementedException(
                        "Unexpected result type: " + evalConditionResult.GetType().FullName);
                }

                currentFrame.InstructionPointer++;

                if (evalConditionOk.Value == PineVMValues.FalseValue)
                {
                    currentFrame.InstructionPointer += conditionalStatement.IfFalseOffset;
                    continue;
                }

                if (evalConditionOk.Value == PineVMValues.TrueValue)
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
            foreach (var item in list.List)
            {
                if (ExpressionShouldGetNewStackFrame(item))
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

        if (expression is Expression.KernelApplications_Skip_ListHead_Expression)
            return false;

        throw new NotImplementedException(
            "Unexpected shape of expression: " + expression.GetType().FullName);
    }

    private Result<string, PineValue> EvaluateExpressionDefaultLessStack(
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
                    stackPrevValues: stackPrevValues)
                switch
                {
                    Result<string, PineValue>.Err err =>
                    "Failed to evaluate parse and evaluate: " + err,

                    var other =>
                    other
                };
        }

        if (expression is Expression.KernelApplicationExpression kernelApplicationExpression)
        {
            return
                EvaluateKernelApplicationExpression(
                    environment,
                    kernelApplicationExpression,
                    stackPrevValues: stackPrevValues) switch
                {
                    Result<string, PineValue>.Err err =>
                    "Failed to evaluate kernel function application: " + err,

                    var other =>
                    other
                };
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
            return delegatingExpr.Delegate.Invoke(
                (expr, envValue) =>
                EvaluateExpressionDefault(
                    expression,
                    envValue,
                    config: new EvaluationConfig(ParseAndEvalCountLimit: null)),
                environment);
        }

        if (expression is Expression.StackReferenceExpression stackRef)
        {
            var index = stackPrevValues.Length + stackRef.offset;

            if (index < 0 || index >= stackPrevValues.Length)
            {
                return
                    "Invalid stack reference: offset " + stackRef.offset +
                    " from " + stackPrevValues.Length +
                    " results in index " + index;
            }

            return stackPrevValues.Span[index];
        }

        if (expression is Expression.KernelApplications_Skip_ListHead_Expression kernelApplicationsSkipListHead)
        {
            return
                EvaluateKernelApplications_Skip_ListHead_Expression(
                    environment: environment,
                    kernelApplicationsSkipListHead,
                    stackPrevValues: stackPrevValues);
        }

        throw new NotImplementedException(
            "Unexpected shape of expression: " + expression.GetType().FullName);
    }

    public Result<string, PineValue> EvaluateListExpression(
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

            if (itemResult is Result<string, PineValue>.Err itemErr)
                return "Failed to evaluate list element [" + i + "]: " + itemErr.Value;

            if (itemResult is Result<string, PineValue>.Ok itemOk)
            {
                listItems.Add(itemOk.Value);
                continue;
            }

            throw new NotImplementedException("Unexpected result type: " + itemResult.GetType().FullName);
        }

        return PineValue.List(listItems);
    }

    public Result<string, PineValue> EvaluateParseAndEvalExpression(
        Expression.ParseAndEvalExpression parseAndEval,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        Result<string, PineValue> continueWithEnvValueAndFunction(
            PineValue environmentValue,
            Expression functionExpression)
        {
            if (environmentValue is PineValue.ListValue list)
            {
                FunctionApplicationMaxEnvSize =
                FunctionApplicationMaxEnvSize < list.Elements.Count ? list.Elements.Count : FunctionApplicationMaxEnvSize;
            }

            return EvaluateExpression(environment: environmentValue, expression: functionExpression);
        }

        return
            EvaluateExpressionDefaultLessStack(
                parseAndEval.environment,
                environment,
                stackPrevValues: stackPrevValues) switch
            {
                Result<string, PineValue>.Err envErr =>
                "Failed to evaluate argument: " + envErr.Value,

                Result<string, PineValue>.Ok environmentValue =>
                EvaluateExpressionDefaultLessStack(
                    parseAndEval.expression,
                    environment,
                    stackPrevValues: stackPrevValues) switch
                {
                    Result<string, PineValue>.Err exprErr =>
                    "Failed to evaluate expression: " + exprErr.Value,

                    Result<string, PineValue>.Ok expressionValue =>
                    parseExpressionDelegate(expressionValue.Value) switch
                    {
                        Result<string, Expression>.Err parseErr =>
                        "Failed to parse expression from value: " + parseErr.Value +
                        " - expressionValue is " + DescribeValueForErrorMessage(expressionValue.Value) +
                        " - environmentValue is " + DescribeValueForErrorMessage(expressionValue.Value),

                        Result<string, Expression>.Ok functionExpression =>
                        continueWithEnvValueAndFunction(environmentValue.Value, functionExpression.Value),

                        var otherResult =>
                        throw new NotImplementedException("Unexpected result type for parse: " + otherResult.GetType().FullName)
                    },

                    var otherResult =>
                    throw new NotImplementedException("Unexpected result type for expr: " + otherResult.GetType().FullName)
                },

                var otherResult =>
                throw new NotImplementedException("Unexpected result type for env: " + otherResult.GetType().FullName)
            };
    }

    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        PineValueAsString.StringFromValue(pineValue)
        .Unpack(fromErr: _ => "not a string", fromOk: asString => "string \'" + asString + "\'");

    public Result<string, PineValue> EvaluateKernelApplicationExpression(
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
                if (EvaluateExpressionDefaultLessStack(
                    skipListExpr.List[0],
                    environment,
                    stackPrevValues) is Result<string, PineValue>.Ok skipCountEvalOk)
                {
                    if (KernelFunction.SignedIntegerFromValueRelaxed(skipCountEvalOk.Value) is { } skipCount)
                    {
                        if (EvaluateExpressionDefaultLessStack(
                            skipListExpr.List[1],
                            environment,
                            stackPrevValues) is Result<string, PineValue>.Ok sourceListEvalOk)
                        {
                            if (sourceListEvalOk.Value is PineValue.ListValue list)
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
                        else
                        {
                            return PineValue.EmptyList;
                        }
                    }
                }
            }
        }

        return EvaluateKernelApplicationExpressionGeneric(environment, application, stackPrevValues);
    }

    public Result<string, PineValue> EvaluateKernelApplicationExpressionGeneric(
        PineValue environment,
        Expression.KernelApplicationExpression application,
        ReadOnlyMemory<PineValue> stackPrevValues) =>
        EvaluateExpressionDefaultLessStack(application.argument, environment, stackPrevValues) switch
        {
            Result<string, PineValue>.Ok argument =>
            application.function(argument.Value),

            Result<string, PineValue>.Err error =>
            "Failed to evaluate argument: " + error,

            var otherResult =>
            throw new NotImplementedException(
                "Unexpected result type: " + otherResult.GetType().FullName)
        };

    public Result<string, PineValue> EvaluateKernelApplications_Skip_ListHead_Expression(
        PineValue environment,
        Expression.KernelApplications_Skip_ListHead_Expression application,
        ReadOnlyMemory<PineValue> stackPrevValues) =>
        EvaluateExpressionDefaultLessStack(
            application.argument,
            environment,
            stackPrevValues: stackPrevValues) switch
        {
            Result<string, PineValue>.Ok argument =>
            argument.Value switch
            {
                PineValue.ListValue list =>
                list.Elements.Count < 1 || list.Elements.Count <= application.skipCount
                ?
                PineValue.EmptyList
                :
                list.Elements[application.skipCount < 0 ? 0 : application.skipCount],

                _ =>
                PineValue.EmptyList,
            },

            Result<string, PineValue>.Err error =>
            "Failed to evaluate argument: " + error,

            var otherResult =>
            throw new NotImplementedException(
                "Unexpected result type: " + otherResult.GetType().FullName)
        };

    public Result<string, PineValue> EvaluateConditionalExpression(
        PineValue environment,
        Expression.ConditionalExpression conditional,
        ReadOnlyMemory<PineValue> stackPrevValues) =>
        EvaluateExpressionDefaultLessStack(
            conditional.condition,
            environment,
            stackPrevValues: stackPrevValues)
        switch
        {
            Result<string, PineValue>.Ok conditionValue =>
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
            PineValue.EmptyList,

            Result<string, PineValue>.Err error =>
            "Failed to evaluate condition: " + error,

            var otherResult =>
            throw new NotImplementedException(
                "Unexpected result type: " + otherResult.GetType().FullName)
        };
}
