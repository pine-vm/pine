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

    private readonly IReadOnlyDictionary<Expression, IReadOnlyList<EnvConstraintId>>? compilationEnvClasses;

    private readonly bool disableReductionInCompilation;

    private readonly PineVMCache parseCache = new();

    public record EvaluationReport(
        PineValue ExpressionValue,
        Expression Expression,
        PineValue Environment,
        long InstructionCount,
        long ParseAndEvalCount,
        PineValue ReturnValue);

    public static PineVM Construct(
        IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>? parseExpressionOverrides,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache)
    {
        var parseExpressionOverridesDict =
            parseExpressionOverrides
            ?.ToFrozenDictionary(
                keySelector: encodedExprAndDelegate => encodedExprAndDelegate.Key,
                elementSelector: encodedExprAndDelegate => new Expression.DelegatingExpression(encodedExprAndDelegate.Value));

        var parseCache = new Dictionary<PineValue, Result<string, Expression>>();

        return new PineVM(
            overrideParseExpression:
            parseExpressionOverridesDict switch
            {
                null =>
                originalHandler => originalHandler,

                not null =>
                _ => value =>
                {
                    if (parseExpressionOverridesDict.TryGetValue(value, out var delegatingExpr))
                        return delegatingExpr;

                    if (parseCache.TryGetValue(value, out var fromCache))
                    {
                        return fromCache;
                    }

                    var parseResult = ExpressionEncoding.ParseExpressionFromValue(value, parseExpressionOverridesDict);

                    parseCache[value] = parseResult;

                    return parseResult;
                }
            },
            evalCache: evalCache);
    }

    public PineVM(
        OverrideParseExprDelegate? overrideParseExpression = null,
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        Action<EvaluationReport>? reportFunctionApplication = null,
        IReadOnlyDictionary<Expression, IReadOnlyList<EnvConstraintId>>? compilationEnvClasses = null,
        bool disableReductionInCompilation = false)
    {
        parseExpressionDelegate =
            overrideParseExpression
            ?.Invoke(ExpressionEncoding.ParseExpressionFromValueDefault) ??
            ExpressionEncoding.ParseExpressionFromValueDefault;

        EvalCache = evalCache;

        this.reportFunctionApplication = reportFunctionApplication;

        this.compilationEnvClasses = compilationEnvClasses;

        this.disableReductionInCompilation = disableReductionInCompilation;
    }

    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment) =>
        EvaluateExpressionOnCustomStack(
            expression,
            environment,
            config: new EvaluationConfig(ParseAndEvalCountLimit: null))
        .Map(report => report.ReturnValue);

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
        long BeginInstructionCount,
        long BeginParseAndEvalCount)
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

    public record ExpressionCompilation(
        StackFrameInstructions Generic,
        IReadOnlyList<(IReadOnlyList<EnvConstraintItem> constraint, StackFrameInstructions instructions)> Specialized)
    {
        public StackFrameInstructions SelectInstructionsForEnvironment(PineValue environment)
        {
            for (var i = 0; i < Specialized.Count; i++)
            {
                var specialization = Specialized[i];

                bool foundMismatch = false;

                for (var specializationIndex = 0; specializationIndex < specialization.constraint.Count; specializationIndex++)
                {
                    var constraintItem = specialization.constraint[specializationIndex];

                    if (CodeAnalysis.ValueFromPathInValue(environment, constraintItem.Path.Span) is not { } pathValue)
                    {
                        foundMismatch = true;
                        break;
                    }

                    if (!pathValue.Equals(constraintItem.Value))
                    {
                        foundMismatch = true;
                        break;
                    }
                }

                if (!foundMismatch)
                {
                    return specialization.instructions;
                }
            }

            return Generic;
        }
    }

    public record struct EnvConstraintItem(
        ReadOnlyMemory<int> Path,
        PineValue Value);

    readonly Dictionary<Expression, ExpressionCompilation> expressionCompilationDict = [];

    StackFrame StackFrameFromExpression(
        PineValue? expressionValue,
        Expression expression,
        PineValue environment,
        long beginInstructionCount,
        long beginParseAndEvalCount)
    {
        var compilation = GetExpressionCompilation(expression);

        var instructions = compilation.SelectInstructionsForEnvironment(environment);

        return new StackFrame(
            expressionValue,
            expression,
            instructions,
            EnvironmentValue: environment,
            new PineValue[instructions.Instructions.Count],
            BeginInstructionCount: beginInstructionCount,
            BeginParseAndEvalCount: beginParseAndEvalCount);
    }

    public ExpressionCompilation GetExpressionCompilation(
        Expression rootExpression)
    {
        if (expressionCompilationDict.TryGetValue(rootExpression, out var cachedCompilation))
        {
            return cachedCompilation;
        }

        var compilation = ExpressionCompilationLessCache(rootExpression);

        expressionCompilationDict[rootExpression] = compilation;

        return compilation;
    }

    public ExpressionCompilation ExpressionCompilationLessCache(Expression rootExpression)
    {
        IReadOnlyList<EnvConstraintId>? specializations = null;

        compilationEnvClasses?.TryGetValue(rootExpression, out specializations);

        return
            CompileExpression(
                rootExpression,
                specializations ?? [],
                parseCache: parseCache,
                disableReduction: disableReductionInCompilation);
    }

    public static ExpressionCompilation CompileExpression(
        Expression rootExpression,
        IReadOnlyList<EnvConstraintId> specializations,
        PineVMCache parseCache,
        bool disableReduction)
    {
        var generic =
            new StackFrameInstructions(
                InstructionsFromExpressionTransitive(
                    rootExpression,
                    envConstraintId: null,
                    parseCache: parseCache,
                    disableReduction: disableReduction));

        var specialized =
            specializations
            // Order to prefer more specific constraints when selecting at runtime.
            .OrderDescending(EnvironmentClassSpecificityComparer.Instance)
            .Select(
                specialization =>
                ((IReadOnlyList<EnvConstraintItem>)
                [..specialization.ParsedEnvItems
                .Select(envItem => new EnvConstraintItem(envItem.Key.ToArray(), envItem.Value))],
                new StackFrameInstructions(
                    InstructionsFromExpressionTransitive(
                        rootExpression,
                        envConstraintId: specialization,
                        parseCache: parseCache,
                        disableReduction: disableReduction))))
            .ToImmutableArray();

        return new ExpressionCompilation(
            Generic: generic,
            Specialized: specialized);
    }

    /*
     * 
     * In the older impl to compile to C#, we had CompilationUnitEnvExprEntry to inform which
     * declarations will be available in the environment to reference.
     * Here, we dont need that info for the first iteration:
     * Instead of a global shared representation, we can inline were referenced.
     * For recursive functions, we can stop inlining which will lead to a lookup in the dictionary for each iteration.
     * However, in some cases (like the adaptive partial application emitted by the Elm compiler),
     * inlining the same expression multiple times can be better.
     * For that specific case, expanding the environment constraint (collected by CA) to enable erasing conditionals might
     * improve overall efficiency a lot.
     * Optimizing for more runtime efficiency follows in later iterations.
     * 
     * (In addition to maybe being easier to implement and read, the inlining will also improve runtime efficiency in many cases.)
     * 
     * */

    public static IReadOnlyList<StackInstruction> InstructionsFromExpressionTransitive(
        Expression rootExpression,
        EnvConstraintId? envConstraintId,
        PineVMCache parseCache,
        bool disableReduction)
    {
        var expressionWithEnvConstraint =
            envConstraintId is null ?
            rootExpression
            :
            SubstituteSubexpressionsForEnvironmentConstraint(
                rootExpression,
                envConstraintId: envConstraintId);

        /*
         * Substituting subexpressions for the given environment constraint once at the root should be enough.
         * */

        var reducedExpression =
            disableReduction
            ?
            expressionWithEnvConstraint
            :
            ReduceExpressionAndInlineRecursive(
                rootExpression: rootExpression,
                currentExpression: expressionWithEnvConstraint,
                maxDepth: 7,
                maxSubexpressionCount: 4_000,
                parseCache: parseCache,
                envConstraintId: envConstraintId,
                disableRecurseAfterInline: false);

        IReadOnlyList<StackInstruction> allInstructions =
            [.. InstructionsFromExpressionTransitive(reducedExpression).Append(StackInstruction.Return)];

        return allInstructions;
    }

    public static Expression ReduceExpressionAndInlineRecursive(
        Expression rootExpression,
        Expression currentExpression,
        EnvConstraintId? envConstraintId,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMCache parseCache,
        bool disableRecurseAfterInline)
    {
        var expressionSubstituted =
            envConstraintId is null
            ?
            currentExpression
            :
            SubstituteSubexpressionsForEnvironmentConstraint(currentExpression, envConstraintId);

        var expressionReduced =
            CompilePineToDotNet.ReducePineExpression.SearchForExpressionReductionRecursive(
                maxDepth: 10,
                expressionSubstituted,
                envConstraintId: envConstraintId);

        if (maxDepth <= 0)
        {
            return expressionReduced;
        }

        if (envConstraintId is null)
        {
            /*
             * Adapt to observation 2024-07-14:
             * Stopping recursion here if envConstraintId is null resulted in significantly faster
             * completion times in a test compiling all modules of the Elm compiler.
             * */
            return expressionReduced;
        }

        var subexprCount = 0;

        foreach (var subexpr in Expression.EnumerateSelfAndDescendants(expressionReduced))
        {
            ++subexprCount;

            /*
             * Install a limit after observing cases with more than a hundred million subexpressions.
             * */

            if (maxSubexpressionCount < subexprCount)
                return expressionReduced;
        }

        ParseExprDelegate parseExpression =
            parseCache.BuildParseExprDelegate(ExpressionEncoding.ParseExpressionFromValueDefault);

        Expression? TryInlineParseAndEval(
            Expression.ParseAndEvalExpression parseAndEvalExpr,
            bool noRecursion)
        {
            Expression? ContinueReduceForKnownExprValue(PineValue exprValue)
            {
                if (parseExpression(exprValue) is not Result<string, Expression>.Ok parseOk)
                {
                    return null;
                }

                if (noRecursion)
                {
                    if (rootExpression.Equals(parseOk.Value))
                    {
                        return null;
                    }
                }

                /*
                 * For inlining, translate instances of EnvironmentExpression to the parent environment.
                 * */

                var inlinedExpr =
                    CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                        findReplacement:
                        descendant =>
                        {
                            if (descendant is Expression.EnvironmentExpression)
                            {
                                return parseAndEvalExpr.environment;
                            }

                            return null;
                        },
                        parseOk.Value).expr;

                if (disableRecurseAfterInline)
                {
                    return inlinedExpr;
                }

                var inlinedExprSubstituted =
                    envConstraintId is null
                    ?
                    inlinedExpr
                    :
                    SubstituteSubexpressionsForEnvironmentConstraint(inlinedExpr, envConstraintId);

                var inlinedExprReduced =
                    CompilePineToDotNet.ReducePineExpression.SearchForExpressionReductionRecursive(
                        maxDepth: 10,
                        inlinedExprSubstituted,
                        envConstraintId: envConstraintId);

                {
                    var conditionsCount = 0;
                    var invocationsCount = 0;
                    var inlinedSubExprCount = 0;

                    foreach (var subexpr in Expression.EnumerateSelfAndDescendants(inlinedExprReduced))
                    {
                        ++inlinedSubExprCount;

                        if (300 < inlinedSubExprCount)
                        {
                            return null;
                        }

                        if (subexpr is Expression.ConditionalExpression)
                        {
                            ++conditionsCount;

                            if (3 < conditionsCount)
                            {
                                return null;
                            }

                            continue;
                        }

                        if (subexpr is Expression.ParseAndEvalExpression)
                        {
                            ++invocationsCount;

                            if (4 < invocationsCount)
                            {
                                return null;
                            }

                            continue;
                        }
                    }
                }

                var inlinedFinal =
                    ReduceExpressionAndInlineRecursive(
                        rootExpression: rootExpression,
                        // currentExpression: inlinedExpr,
                        currentExpression: inlinedExprReduced,
                        envConstraintId: envConstraintId,
                        maxDepth: maxDepth - 1,
                        maxSubexpressionCount: maxSubexpressionCount,
                        parseCache: parseCache,
                        disableRecurseAfterInline: disableRecurseAfterInline);

                {
                    var conditionsCount = 0;
                    var invocationsCount = 0;
                    var inlinedSubExprCount = 0;

                    foreach (var subexpr in Expression.EnumerateSelfAndDescendants(inlinedFinal))
                    {
                        ++inlinedSubExprCount;

                        if (300 < inlinedSubExprCount)
                        {
                            return null;
                        }

                        if (subexpr is Expression.ConditionalExpression)
                        {
                            ++conditionsCount;

                            if (3 < conditionsCount)
                            {
                                return null;
                            }

                            continue;
                        }

                        if (subexpr is Expression.ParseAndEvalExpression)
                        {
                            ++invocationsCount;

                            if (4 < invocationsCount)
                            {
                                return null;
                            }

                            continue;
                        }
                    }
                }

                return inlinedFinal;
            }

            if (Expression.IsIndependent(parseAndEvalExpr.expression))
            {
                if (CompilePineToDotNet.ReducePineExpression.TryEvaluateExpressionIndependent(parseAndEvalExpr.expression) is
                    Result<string, PineValue>.Ok evalExprOk)
                {
                    return ContinueReduceForKnownExprValue(evalExprOk.Value);
                }
            }

            return null;
        }

        Expression InlineParseAndEvalRecursive(
            Expression expression,
            bool underConditional)
        {
            return
                CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                findReplacement: expr =>
                {
                    /*
                     * Do not inline invocations that are still conditional after substituting for the environment constraint.
                     * Inlining these cases can lead to suboptimal overall performance for various reasons.
                     * One reason is that inlining in a generic wrapper causes us to miss an opportunity to select
                     * a more specialized implementation because this selection only happens on invocation.
                     * */

                    /*
                     * 2024-07-20 Adaptation, for cases like specializations of `List.map`:
                     * When optimizing `List.map` (or its recursive helper function) (or `List.foldx` for example),
                     * better also inline the application of the generic partial application used with the function parameter.
                     * That application is conditional (list empty?), but we want to inline that to eliminate the generic wrapper for
                     * the function application and inline the parameter function directly.
                     * Thus, the new rule also enables inlining under conditional expressions unless it is recursive.
                     * */

                    if (expr is Expression.ConditionalExpression conditional)
                    {
                        var conditionInlined =
                        InlineParseAndEvalRecursive(
                            conditional.condition,
                            underConditional: underConditional);

                        var falseBranchInlined =
                        InlineParseAndEvalRecursive(
                            conditional.ifFalse,
                            underConditional: true);

                        var trueBranchInlined =
                        InlineParseAndEvalRecursive(
                            conditional.ifTrue,
                            underConditional: true);

                        return new Expression.ConditionalExpression(
                            condition: conditionInlined,
                            ifFalse: falseBranchInlined,
                            ifTrue: trueBranchInlined);
                    }

                    if (expr is Expression.ParseAndEvalExpression parseAndEval)
                    {
                        if (TryInlineParseAndEval(parseAndEval, noRecursion: underConditional) is { } inlined)
                        {
                            return inlined;
                        }
                    }

                    return null;
                },
                expression).expr;
        }

        var expressionInlined =
            InlineParseAndEvalRecursive(
                expressionReduced,
                underConditional: false);

        var expressionInlinedReduced =
            CompilePineToDotNet.ReducePineExpression.SearchForExpressionReductionRecursive(
                maxDepth: 10,
                expressionInlined);

        return expressionInlinedReduced;
    }

    public static Expression SubstituteSubexpressionsForEnvironmentConstraint(
        Expression originalExpression,
        EnvConstraintId envConstraintId)
    {
        return
            CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                findReplacement:
                descendant =>
                {
                    if (CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(descendant) is { } indexPath)
                    {
                        if (indexPath is ExprMappedToParentEnv.LiteralInParentEnv asLiteral)
                        {
                            return new Expression.LiteralExpression(asLiteral.Value);
                        }

                        if (indexPath is ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
                        {
                            if (envConstraintId?.TryGetValue(pathInParentEnv.Path) is { } value)
                            {
                                return new Expression.LiteralExpression(value);
                            }
                        }
                    }

                    return null;
                },
                originalExpression).expr;
    }

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
                        var offset = reusableIndex - instructionIndex;

                        return new Expression.StackReferenceExpression(offset);
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

            var instructionsBeforeBranchFalseCount =
                conditionInstructions.Count + 1;

            int? reusableResultOffsetForBranchFalse(Expression expression)
            {
                if (reusableExpressionResultOffset(expression) is { } earlierOffset)
                {
                    return earlierOffset - instructionsBeforeBranchFalseCount;
                }

                if (reusableFromCondition.TryGetValue(expression, out var offsetFromCondition))
                {
                    return offsetFromCondition - instructionsBeforeBranchFalseCount;
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

            IReadOnlyList<StackInstruction> ifFalseInstructionsAndJump =
                [.. ifFalseInstructions,
                StackInstruction.Jump(ifTrueInstructions.Count + 1)];

            var branchInstruction =
                new StackInstruction.ConditionalJumpInstruction(
                    InvalidBranchOffset: ifFalseInstructionsAndJump.Count + ifTrueInstructions.Count,
                    TrueBranchOffset: ifFalseInstructionsAndJump.Count);

            IReadOnlyList<StackInstruction> instructionsBeforeContinuation =
                [..conditionInstructions,
                branchInstruction,
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
                skipDescendants: expressionAlreadyCovered)
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
                skipDescendants: node => 1 < allExpressionsCount[node] || expressionAlreadyCovered(node))
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

    public Result<string, EvaluationReport> EvaluateExpressionOnCustomStack(
        Expression rootExpression,
        PineValue rootEnvironment,
        EvaluationConfig config)
    {
        long instructionCount = 0;

        int parseAndEvalCount = 0;

        var stack = new Stack<StackFrame>();

        stack.Push(
            StackFrameFromExpression(
                expressionValue: null,
                rootExpression,
                rootEnvironment,
                beginInstructionCount: instructionCount,
                beginParseAndEvalCount: parseAndEvalCount));

        while (true)
        {
            var currentFrame = stack.Peek();

            ++instructionCount;

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

                if (currentFrame.ExpressionValue is { } currentFrameExprValue)
                {
                    var frameInstructionCount = instructionCount - currentFrame.BeginInstructionCount;

                    if (frameInstructionCount > 3_000 && EvalCache is { } evalCache)
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
                            InstructionCount: frameInstructionCount,
                            ParseAndEvalCount: parseAndEvalCount - currentFrame.BeginParseAndEvalCount,
                            frameReturnValue));
                }

                stack.Pop();

                if (stack.Count is 0)
                {
                    var rootExprValue =
                        ExpressionEncoding.EncodeExpressionAsValue(rootExpression)
                        .Extract(err => throw new Exception("Failed to encode root expression: " + err));

                    return new EvaluationReport(
                        ExpressionValue: rootExprValue,
                        Expression: rootExpression,
                        Environment: rootEnvironment,
                        InstructionCount: instructionCount,
                        ParseAndEvalCount: parseAndEvalCount,
                        ReturnValue: frameReturnValue);
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

                    var newFrame =
                        StackFrameFromExpression(
                            expressionValue: expressionValue,
                            parseOk.Value,
                            environmentValue,
                            beginInstructionCount: instructionCount,
                            beginParseAndEvalCount: parseAndEvalCount);

                    stack.Push(newFrame);

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
                                    beginInstructionCount: instructionCount,
                                    beginParseAndEvalCount: parseAndEvalCount));

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
                    continue;
                }

                if (conditionValue == PineVMValues.TrueValue)
                {
                    currentFrame.InstructionPointer += conditionalStatement.TrueBranchOffset;
                    continue;
                }

                currentFrame.PushInstructionResult(PineValue.EmptyList);
                currentFrame.InstructionPointer += conditionalStatement.InvalidBranchOffset;

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

    public PineValue EvaluateExpressionDefaultLessStack(
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
                    EvaluateExpressionDefaultLessStack(
                        expression,
                        environment,
                        stackPrevValues: ReadOnlyMemory<PineValue>.Empty),
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

        if (EvalCache is { } evalCache)
        {
            var cacheKey = new EvalCacheEntryKey(ExprValue: expressionValue, EnvValue: environmentValue);

            if (evalCache.TryGetValue(cacheKey, out var fromCache))
            {
                return fromCache;
            }
        }

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
            EvaluateExpressionDefaultLessStack(
                environment: environmentValue,
                expression: parseOk.Value,
                stackPrevValues: ReadOnlyMemory<PineValue>.Empty);
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
