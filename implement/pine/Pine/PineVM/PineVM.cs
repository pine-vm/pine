using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Pine.Core;
using Pine.Core.PineVM;
using Pine.Pine.PineVM;

namespace Pine.PineVM;


public record struct EvalCacheEntryKey(
    PineValue ExprValue,
    PineValue EnvValue);

public class PineVM : IPineVM
{
    public long EvaluateExpressionCount { private set; get; }

    public long FunctionApplicationMaxEnvSize { private set; get; }

    private IDictionary<EvalCacheEntryKey, PineValue>? EvalCache { init; get; }

    private readonly EvaluationConfig? evaluationConfigDefault;

    private readonly Action<EvaluationReport>? reportFunctionApplication;

    private readonly IReadOnlyDictionary<Expression, IReadOnlyList<EnvConstraintId>>? compilationEnvClasses;

    private readonly bool disableReductionInCompilation;

    private readonly bool disablePrecompiled;

    private readonly bool enableTailRecursionOptimization;

    public readonly PineVMParseCache parseCache = new();

    private readonly IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>? overrideInvocations;

    public record EvaluationReport(
        PineValue ExpressionValue,
        Expression Expression,
        PineValue Environment,
        long InstructionCount,
        long InvocationCount,
        long LoopIterationCount,
        PineValue ReturnValue,
        IReadOnlyList<Expression> StackTrace);

    public PineVM(
        IDictionary<EvalCacheEntryKey, PineValue>? evalCache = null,
        EvaluationConfig? evaluationConfigDefault = null,
        Action<EvaluationReport>? reportFunctionApplication = null,
        IReadOnlyDictionary<Expression, IReadOnlyList<EnvConstraintId>>? compilationEnvClasses = null,
        bool disableReductionInCompilation = false,
        bool disablePrecompiled = false,
        bool enableTailRecursionOptimization = false,
        IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>? overrideInvocations = null,
        IReadOnlyDictionary<PineValue, IReadOnlyList<string>>? expressionsDisplayNames = null)
    {
        EvalCache = evalCache;

        this.evaluationConfigDefault = evaluationConfigDefault;

        this.reportFunctionApplication = reportFunctionApplication;

        this.compilationEnvClasses = compilationEnvClasses;

        this.disableReductionInCompilation = disableReductionInCompilation;
        this.disablePrecompiled = disablePrecompiled;
        this.enableTailRecursionOptimization = enableTailRecursionOptimization;

        this.overrideInvocations = overrideInvocations;
    }

    public Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment) =>
        EvaluateExpressionOnCustomStack(
            expression,
            environment,
            config:
            evaluationConfigDefault ?? new EvaluationConfig(ParseAndEvalCountLimit: null))
        .Map(report => report.ReturnValue);

    public record StackFrameInstructions(
        IReadOnlyList<StackInstruction> Instructions,
        EnvConstraintId? TrackEnvConstraint = null)
    {
        public int MaxLocalIndex { init; get; } =
            Instructions
            .Select(i => i.Kind is StackInstructionKind.Local_Set ? i.LocalIndex ?? 0 : 0)
            .DefaultIfEmpty(-1)
            .Max();

        public int MaxStackUsage { init; get; } =
            ComputeMaxStackUsage(Instructions);

        public static int ComputeMaxStackUsage(
            IReadOnlyList<StackInstruction> Instructions)
        {
            int currentDepth = 0;
            int maxDepth = 0;

            for (int i = 0; i < Instructions.Count; i++)
            {
                var instr = Instructions[i];

                var instructionDetails =
                    StackInstruction.GetDetails(instr);

                currentDepth =
                    currentDepth - instructionDetails.PopCount + instructionDetails.PushCount;

                if (currentDepth < 0)
                {
                    var instructionSequenceString =
                        string.Join(
                            "\n",
                            Instructions
                            .Select((instr, index) => "[" + index + "] " + instr));

                    throw new InvalidOperationException(
                        "Instruction sequence pops more items than available:\n" + instructionSequenceString);
                }

                maxDepth =
                    maxDepth < currentDepth
                    ?
                    currentDepth
                    :
                    maxDepth;
            }

            return maxDepth;
        }

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
        Memory<PineValue> StackValues,
        Memory<PineValue> LocalsValues,
        long BeginInstructionCount,
        long BeginParseAndEvalCount,
        long BeginStackFrameCount,
        ApplyStepwise? Specialization)
    {
        public int InstructionPointer { get; set; } = 0;

        public int StackPointer { get; set; } = 0;

        public long LoopIterationCount { get; set; } = 0;

        public long InstructionCount { get; set; } = 0;

        public void ReturnFromChildFrame(PineValue frameReturnValue)
        {
            if (Specialization is not null)
            {
                Specialization.ReturningFromChildFrame(frameReturnValue);
                return;
            }

            PushInstructionResult(frameReturnValue);
        }

        public void PushInstructionResult(PineValue value)
        {
            if (value is null)
            {
                throw new InvalidOperationException(
                    "PushInstructionResult called with null value");
            }

            StackValues.Span[StackPointer] = value;
            StackPointer++;
            ++InstructionPointer;
        }

        public void LocalSet(int localIndex, PineValue value)
        {
            if (value is null)
            {
                throw new InvalidOperationException(
                    "LocalSet called with null value");
            }

            LocalsValues.Span[localIndex] = value;
        }

        public PineValue LocalGet(int localIndex)
        {
            var value = LocalsValues.Span[localIndex];

            if (value is null)
            {
                throw new InvalidOperationException(
                    "LocalGet called with null value");
            }

            return value;
        }

        public PineValue PopTopmostFromStack()
        {
            if (StackPointer <= 0)
                throw new InvalidOperationException("ConsumeSingleFromStack called with empty stack");

            --StackPointer;
            return StackValues.Span[StackPointer];
        }

        public PineValue PeekTopmostFromStack()
        {
            if (StackPointer <= 0)
                throw new InvalidOperationException("PeekTopmostFromStack called with empty stack");

            return StackValues.Span[StackPointer - 1];
        }
    }

    /*
     * TODO: Expand the stack frame instruction format so that we can model these specializations
     * as precompiled stack frames.
     * That means the stack frame (instruction) model needs to be able to loop (mutate counter in place) and to supply inputs.
     * */
    public record ApplyStepwise
    {
        public StepResult CurrentStep { private set; get; }

        public ApplyStepwise(StepResult.Continue start)
        {
            CurrentStep = start;
        }

        public void ReturningFromChildFrame(PineValue frameReturnValue)
        {
            if (CurrentStep is StepResult.Continue cont)
            {
                CurrentStep = cont.Callback(frameReturnValue);
            }
            else
            {
                throw new Exception("Returning on frame already completed earlier.");
            }
        }

        public abstract record StepResult
        {
            public sealed record Continue(
                Expression Expression,
                PineValue EnvironmentValue,
                Func<PineValue, StepResult> Callback)
                : StepResult;

            public sealed record Complete(PineValue PineValue)
                : StepResult;
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

    readonly static CompilePineToDotNet.CompilerMutableCache mutableCache = new();

    StackFrame StackFrameFromExpression(
        PineValue? expressionValue,
        Expression expression,
        PineValue environment,
        long beginInstructionCount,
        long beginParseAndEvalCount,
        long beginStackFrameCount)
    {
        var compilation = GetExpressionCompilation(expression);

        var instructions = compilation.SelectInstructionsForEnvironment(environment);

        return new StackFrame(
            expressionValue,
            expression,
            instructions,
            EnvironmentValue: environment,
            StackValues: new PineValue[instructions.MaxStackUsage],
            LocalsValues: new PineValue[instructions.MaxLocalIndex + 1],
            BeginInstructionCount: beginInstructionCount,
            BeginParseAndEvalCount: beginParseAndEvalCount,
            BeginStackFrameCount: beginStackFrameCount,
            Specialization: null);
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

        bool skipInlining(Expression expr, EnvConstraintId? envConstraintId)
        {
            if (Precompiled.HasPrecompiledForExpression(expr))
            {
                return true;
            }

            if (envConstraintId is null && (compilationEnvClasses?.ContainsKey(expr) ?? false))
            {
                return true;
            }

            return false;
        }

        return
        CompileExpression(
            rootExpression,
            specializations ?? [],
            parseCache: parseCache,
            disableReduction: disableReductionInCompilation,
            skipInlining: skipInlining,
            enableTailRecursionOptimization: enableTailRecursionOptimization);
    }

    public static ExpressionCompilation CompileExpression(
        Expression rootExpression,
        IReadOnlyList<EnvConstraintId> specializations,
        PineVMParseCache parseCache,
        bool disableReduction,
        bool enableTailRecursionOptimization,
        Func<Expression, EnvConstraintId?, bool> skipInlining)
    {
        var generic =
            new StackFrameInstructions(
                InstructionsFromExpressionTransitive(
                    rootExpression,
                    envConstraintId: null,
                    parseCache: parseCache,
                    disableReduction: disableReduction,
                    skipInlining: skipInlining,
                    enableTailRecursionOptimization: enableTailRecursionOptimization),
                TrackEnvConstraint: null);

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
                                    disableReduction: disableReduction,
                        enableTailRecursionOptimization: enableTailRecursionOptimization,
                                    skipInlining: skipInlining),
                    TrackEnvConstraint: specialization)))
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
        PineVMParseCache parseCache,
        bool disableReduction,
        bool enableTailRecursionOptimization,
        Func<Expression, EnvConstraintId?, bool> skipInlining)
    {
        var inlinedStaticInvocations =
            disableReduction || enableTailRecursionOptimization
            ?
            rootExpression
            :
            InlineStaticInvocationsAndReduceRecursive(
                rootExpression,
                inlinedParents: [],
                maxDepth: 6,
                maxSubexpressionCount: 4_000,
                parseCache,
                disableRecurseAfterInline: false,
                skipInlining: e => skipInlining(e, null));

        var expressionWithEnvConstraint =
            envConstraintId is null || enableTailRecursionOptimization
            ?
            inlinedStaticInvocations
            :
            SubstituteSubexpressionsForEnvironmentConstraint(
                inlinedStaticInvocations,
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
                currentExpression: expressionWithEnvConstraint,
                inlinedParents: [],
                maxDepth: 7,
                maxSubexpressionCount: 4_000,
                parseCache: parseCache,
                envConstraintId: envConstraintId,
                rootExprForms: [rootExpression],
                disableRecurseAfterInline: false,
                skipInlining: skipInlining);

        IReadOnlyList<StackInstruction> allInstructions =
            [.. InstructionsFromExpression(
                rootExpression: reducedExpression,
                rootExprAlternativeForms: [rootExpression],
                envClass: enableTailRecursionOptimization ? envConstraintId : null,
                parseCache),
            StackInstruction.Return
            ];

        return allInstructions;
    }


    public static Expression InlineStaticInvocationsAndReduceRecursive(
        Expression currentExpression,
        ImmutableStack<Expression> inlinedParents,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, bool> skipInlining)
    {
        var expressionReduced =
            CompilePineToDotNet.ReducePineExpression.ReduceExpressionBottomUp(
                currentExpression);

        if (maxDepth <= 0)
        {
            return expressionReduced;
        }

        if (maxSubexpressionCount < expressionReduced.SubexpressionCount)
        {
            return expressionReduced;
        }

        Expression? TryInlineParseAndEval(
            Expression.ParseAndEval parseAndEvalExpr)
        {
            if (parseAndEvalExpr.Encoded.ReferencesEnvironment)
            {
                return null;
            }

            if (CompilePineToDotNet.ReducePineExpression.TryEvaluateExpressionIndependent(
                parseAndEvalExpr.Encoded).IsOkOrNull() is not { } exprValue)
            {
                return null;
            }

            if (parseCache.ParseExpression(exprValue).IsOkOrNull() is not { } parseOk)
            {
                return null;
            }

            if (skipInlining(parseOk))
            {
                return null;
            }

            if (inlinedParents.Contains(parseOk))
            {
                return null;
            }

            /*
             * For inlining, translate instances of EnvironmentExpression to the parent environment.
             * */

            var inlinedExpr =
                CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                    findReplacement:
                    descendant =>
                    {
                        if (descendant is Expression.Environment)
                        {
                            return parseAndEvalExpr.Environment;
                        }

                        return null;
                    },
                    parseOk).expr;

            if (disableRecurseAfterInline)
            {
                return inlinedExpr;
            }

            var inlinedExprReduced =
                CompilePineToDotNet.ReducePineExpression.ReduceExpressionBottomUp(
                    inlinedExpr);

            var inlinedFinal =
                InlineStaticInvocationsAndReduceRecursive(
                    currentExpression: inlinedExprReduced,
                    inlinedParents: inlinedParents.Push(parseOk),
                    maxDepth: maxDepth - 1,
                    maxSubexpressionCount: maxSubexpressionCount,
                    parseCache: parseCache,
                    skipInlining: skipInlining,
                    disableRecurseAfterInline: disableRecurseAfterInline);

            return inlinedFinal;
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

        if (expr is Expression.Conditional conditional)
        {
            var conditionInlined =
            InlineParseAndEvalRecursive(
                conditional.Condition,
                underConditional: underConditional);

            var falseBranchInlined =
            InlineParseAndEvalRecursive(
                conditional.FalseBranch,
                underConditional: true);

            var trueBranchInlined =
            InlineParseAndEvalRecursive(
                conditional.TrueBranch,
                underConditional: true);

            return Expression.ConditionalInstance(
                condition: conditionInlined,
                falseBranch: falseBranchInlined,
                trueBranch: trueBranchInlined);
        }

        if (expr is Expression.ParseAndEval parseAndEval)
        {
            if (TryInlineParseAndEval(parseAndEval) is { } inlined)
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
            CompilePineToDotNet.ReducePineExpression.ReduceExpressionBottomUp(expressionInlined);

        return expressionInlinedReduced;
    }

    public static Expression ReduceExpressionAndInlineRecursive(
        Expression rootExpression,
        ImmutableHashSet<Expression> rootExprAlternativeForms,
        EnvConstraintId? envConstraintId,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, EnvConstraintId?, bool> skipInlining) =>
        ReduceExpressionAndInlineRecursive(
            currentExpression: rootExpression,
            inlinedParents: [],
            envConstraintId: envConstraintId,
            rootExprForms: rootExprAlternativeForms.Add(rootExpression),
            maxDepth: maxDepth,
            maxSubexpressionCount: maxSubexpressionCount,
            parseCache: parseCache,
            disableRecurseAfterInline: disableRecurseAfterInline,
            skipInlining: skipInlining);

    public static Expression ReduceExpressionAndInlineRecursive(
        Expression currentExpression,
        ImmutableStack<Expression> inlinedParents,
        EnvConstraintId? envConstraintId,
        ImmutableHashSet<Expression> rootExprForms,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, EnvConstraintId?, bool> skipInlining)
    {
        var expressionSubstituted =
            envConstraintId is null
            ?
            currentExpression
            :
            SubstituteSubexpressionsForEnvironmentConstraint(
                currentExpression,
                envConstraintId);

        var expressionReduced =
            CompilePineToDotNet.ReducePineExpression.ReduceExpressionBottomUp(expressionSubstituted);

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

            /*
             * 2024-11-30: Enable inlining also for cases without environment classes.
             * 
            return expressionReduced;
            */
        }


        /*
         * Install a limit after observing cases with more than a hundred million subexpressions.
         * */

        if (maxSubexpressionCount < expressionReduced.SubexpressionCount)
        {
            return expressionReduced;
        }

        Expression? TryInlineParseAndEval(
            Expression.ParseAndEval parseAndEvalExpr,
            bool noRecursion)
        {
            Expression? ContinueReduceForKnownExprValue(PineValue exprValue)
            {
                if (parseCache.ParseExpression(exprValue).IsOkOrNull() is not { } parseOk)
                {
                    return null;
                }

                if (skipInlining(parseOk, envConstraintId))
                {
                    return null;
                }

                if (noRecursion)
                {
                    if (inlinedParents.Contains(parseOk))
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
                            if (descendant is Expression.Environment)
                            {
                                return parseAndEvalExpr.Environment;
                            }

                            return null;
                        },
                        parseOk).expr;

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
                    CompilePineToDotNet.ReducePineExpression.ReduceExpressionBottomUp(inlinedExprSubstituted);

                {
                    if (500 < inlinedExprReduced.SubexpressionCount)
                    {
                        return null;
                    }

                    var conditionsCount = 0;
                    var invocationsCount = 0;

                    foreach (var subexpr in Expression.EnumerateSelfAndDescendants(inlinedExprReduced))
                    {
                        if (subexpr is Expression.Conditional)
                        {
                            ++conditionsCount;

                            if (5 < conditionsCount)
                            {
                                return null;
                            }

                            continue;
                        }

                        if (subexpr is Expression.ParseAndEval)
                        {
                            ++invocationsCount;

                            if (5 < invocationsCount)
                            {
                                return null;
                            }

                            continue;
                        }
                    }
                }

                var inlinedFinal =
                    ReduceExpressionAndInlineRecursive(
                        // currentExpression: inlinedExpr,
                        currentExpression: inlinedExprReduced,
                        inlinedParents: inlinedParents.Push(parseOk),
                        rootExprForms: rootExprForms,
                        envConstraintId: envConstraintId,
                        maxDepth: maxDepth - 1,
                        maxSubexpressionCount: maxSubexpressionCount,
                        parseCache: parseCache,
                        skipInlining: skipInlining,
                        disableRecurseAfterInline: disableRecurseAfterInline);

                {
                    if (500 < inlinedFinal.SubexpressionCount)
                    {
                        return null;
                    }

                    var conditionsCount = 0;
                    var invocationsCount = 0;

                    foreach (var subexpr in Expression.EnumerateSelfAndDescendants(inlinedFinal))
                    {
                        if (subexpr is Expression.Conditional)
                        {
                            ++conditionsCount;

                            if (5 < conditionsCount)
                            {
                                return null;
                            }

                            continue;
                        }

                        if (subexpr is Expression.ParseAndEval)
                        {
                            ++invocationsCount;

                            if (5 < invocationsCount)
                            {
                                return null;
                            }

                            continue;
                        }
                    }
                }

                return inlinedFinal;
            }

            if (!parseAndEvalExpr.Encoded.ReferencesEnvironment)
            {
                if (CompilePineToDotNet.ReducePineExpression.TryEvaluateExpressionIndependent(
                    parseAndEvalExpr.Encoded).IsOkOrNull() is
                    { } evalExprOk)
                {
                    return ContinueReduceForKnownExprValue(evalExprOk);
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

                    if (expr is Expression.Conditional conditional)
                    {
                        var conditionInlined =
                        InlineParseAndEvalRecursive(
                            conditional.Condition,
                            underConditional: underConditional);

                        var falseBranchInlined =
                        InlineParseAndEvalRecursive(
                            conditional.FalseBranch,
                            underConditional: true);

                        var trueBranchInlined =
                        InlineParseAndEvalRecursive(
                            conditional.TrueBranch,
                            underConditional: true);

                        return Expression.ConditionalInstance(
                            condition: conditionInlined,
                            falseBranch: falseBranchInlined,
                            trueBranch: trueBranchInlined);
                    }

                    if (expr is Expression.ParseAndEval parseAndEval)
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
            CompilePineToDotNet.ReducePineExpression.ReduceExpressionBottomUp(expressionInlined);

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
                            return Expression.LiteralInstance(asLiteral.Value);
                        }

                        if (indexPath is ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
                        {
                            if (envConstraintId?.TryGetValue(pathInParentEnv.Path) is { } value)
                            {
                                return Expression.LiteralInstance(value);
                            }
                        }
                    }

                    return null;
                },
                originalExpression).expr;
    }

    public static IReadOnlyList<StackInstruction> InstructionsFromExpression(
        Expression rootExpression,
        ImmutableHashSet<Expression> rootExprAlternativeForms,
        EnvConstraintId? envClass,
        PineVMParseCache parseCache)
    {
        return
            PineIRCompiler.CompileExpression(
                rootExpression,
                rootExprAlternativeForms: rootExprAlternativeForms,
                envClass,
                parseCache)
            .Instructions;
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

            if (expression is Expression.List list)
            {
                for (var i = 0; i < list.items.Count; ++i)
                {
                    stack.Push(list.items[i]);
                }
            }

            if (expression is Expression.ParseAndEval parseAndEval)
            {
                stack.Push(parseAndEval.Encoded);
                stack.Push(parseAndEval.Environment);
            }

            if (expression is Expression.KernelApplication kernelApp)
            {
                stack.Push(kernelApp.Input);
            }

            if (expression is Expression.Conditional conditional)
            {
                stack.Push(conditional.Condition);

                /*
                 *
                 * For now, we create a new stack frame for each conditional expression.
                 * Therefore do not descend into the branches of the conditional expression.

                stack.Push(conditional.falseBranch);
                stack.Push(conditional.trueBranch);
                */
            }

            if (expression is Expression.StringTag stringTag)
            {
                stack.Push(stringTag.Tagged);
            }

            deepestDescendants.Push(expression);
        }

        while (deepestDescendants.Count > 0)
        {
            yield return deepestDescendants.Pop();
        }
    }

    public record EvaluationConfig(
        int? ParseAndEvalCountLimit);

    public Result<string, EvaluationReport> EvaluateExpressionOnCustomStack(
        Expression rootExpression,
        PineValue rootEnvironment,
        EvaluationConfig config)
    {
        long instructionCount = 0;
        long loopIterationCount = 0;
        long parseAndEvalCount = 0;
        long stackFrameCount = 0;
        long lastCacheEntryInstructionCount = 0;
        long lastCacheEntryParseAndEvalCount = 0;

        var stack = new Stack<StackFrame>();

        string? invokePrecompiledOrBuildStackFrame(
            PineValue? expressionValue,
            Expression expression,
            PineValue environmentValue)
        {
            var currentFrame = stack.Peek();

            if (!disablePrecompiled &&
                Precompiled.SelectPrecompiled(expression, environmentValue, parseCache) is { } precompiledDelegate)
            {
                var precompiledResult = precompiledDelegate();

                switch (precompiledResult)
                {
                    case Precompiled.PrecompiledResult.FinalValue finalValue:

                        stackFrameCount += finalValue.StackFrameCount;

                        currentFrame.PushInstructionResult(finalValue.Value);

                        reportFunctionApplication?.Invoke(
                            new EvaluationReport(
                                ExpressionValue: expressionValue,
                                expression,
                                environmentValue,
                                InstructionCount: 0,
                                LoopIterationCount: 0,
                                InvocationCount: finalValue.StackFrameCount,
                                ReturnValue: finalValue.Value,
                                StackTrace: CompileStackTrace(10)));

                        return null;

                    case Precompiled.PrecompiledResult.ContinueParseAndEval continueParseAndEval:
                        {
                            if (InvocationCachedResultOrOverride(
                                expressionValue: continueParseAndEval.ExpressionValue,
                                environmentValue: continueParseAndEval.EnvironmentValue) is { } fromCacheOrDelegate)
                            {
                                currentFrame.PushInstructionResult(fromCacheOrDelegate);

                                return null;
                            }

                            var contParseResult = parseCache.ParseExpression(continueParseAndEval.ExpressionValue);

                            if (contParseResult.IsErrOrNull() is { } contParseErr)
                            {
                                return
                                    "Failed to parse expression from value: " + contParseErr +
                                    " - expressionValue is " + DescribeValueForErrorMessage(expressionValue) +
                                    " - environmentValue is " + DescribeValueForErrorMessage(environmentValue);
                            }

                            if (contParseResult.IsOkOrNull() is not { } contParseOk)
                            {
                                throw new NotImplementedException(
                                    "Unexpected result type: " + contParseResult.GetType().FullName);
                            }

                            return
                                invokePrecompiledOrBuildStackFrame(
                                    expressionValue: continueParseAndEval.ExpressionValue,
                                    expression: contParseOk,
                                    environmentValue: continueParseAndEval.EnvironmentValue);
                        }

                    case Precompiled.PrecompiledResult.StepwiseSpecialization specialization:
                        {
                            var newFrame =
                                new StackFrame(
                                    ExpressionValue: expressionValue,
                                    Expression: expression,
                                    Instructions: null,
                                    EnvironmentValue: environmentValue,
                                    StackValues: null,
                                    LocalsValues: null,
                                    BeginInstructionCount: instructionCount,
                                    BeginParseAndEvalCount: parseAndEvalCount,
                                    BeginStackFrameCount: stackFrameCount,
                                    Specialization: specialization.Stepwise);

                            pushStackFrame(newFrame);

                            return null;
                        }

                    default:
                        throw new Exception(
                            "Unexpected return type from precompiled: " + precompiledResult.GetType().FullName);
                }
            }
            else
            {
                buildAndPushStackFrame
                (
                    expressionValue: expressionValue,
                    expression: expression,
                    environmentValue: environmentValue
                );

                return null;
            }
        }

        void buildAndPushStackFrame(
            PineValue? expressionValue,
            Expression expression,
            PineValue environmentValue)
        {
            var newFrame =
                StackFrameFromExpression(
                    expressionValue: expressionValue,
                    expression: expression,
                    environment: environmentValue,
                    beginInstructionCount: instructionCount,
                    beginParseAndEvalCount: parseAndEvalCount,
                    beginStackFrameCount: stackFrameCount);

            pushStackFrame(newFrame);
        }

        void pushStackFrame(StackFrame newFrame)
        {
            stack.Push(newFrame);

            ++stackFrameCount;
        }

        EvaluationReport? returnFromStackFrame(PineValue frameReturnValue)
        {
            var currentFrame = stack.Peek();

            if (currentFrame.ExpressionValue is { } currentFrameExprValue)
            {
                var frameTotalInstructionCount =
                    instructionCount - currentFrame.BeginInstructionCount;

                var frameParseAndEvalCount = parseAndEvalCount - currentFrame.BeginParseAndEvalCount;
                var frameStackFrameCount = stackFrameCount - currentFrame.BeginStackFrameCount;

                if (frameTotalInstructionCount + frameStackFrameCount * 100 > 700 && EvalCache is { } evalCache)
                {
                    var parseAndEvalCountSinceLastCacheEntry =
                        parseAndEvalCount - lastCacheEntryParseAndEvalCount;

                    var instructionCountSinceLastCacheEntry =
                        instructionCount - lastCacheEntryInstructionCount;

                    if (instructionCountSinceLastCacheEntry + parseAndEvalCountSinceLastCacheEntry * 100 > 700)
                    {
                        if (evalCache.TryAdd(
                            new EvalCacheEntryKey(currentFrameExprValue, currentFrame.EnvironmentValue),
                            frameReturnValue))
                        {
                            lastCacheEntryInstructionCount = instructionCount;
                            lastCacheEntryParseAndEvalCount = parseAndEvalCount;
                        }
                    }
                }

                reportFunctionApplication?.Invoke(
                    new EvaluationReport(
                        ExpressionValue: currentFrameExprValue,
                        currentFrame.Expression,
                        currentFrame.EnvironmentValue,
                        InstructionCount: frameTotalInstructionCount,
                        LoopIterationCount: currentFrame.LoopIterationCount,
                        InvocationCount: frameParseAndEvalCount,
                        ReturnValue: frameReturnValue,
                        StackTrace: CompileStackTrace(10)));
            }

            stack.Pop();

            if (stack.Count is 0)
            {
                var rootExprValue =
                    ExpressionEncoding.EncodeExpressionAsValue(rootExpression);

                return new EvaluationReport(
                    ExpressionValue: rootExprValue,
                    Expression: rootExpression,
                    Environment: rootEnvironment,
                    InstructionCount: instructionCount,
                    LoopIterationCount: loopIterationCount,
                    InvocationCount: parseAndEvalCount,
                    ReturnValue: frameReturnValue,
                    StackTrace: []);
            }

            var previousFrame = stack.Peek();

            previousFrame.ReturnFromChildFrame(frameReturnValue);

            return null;
        }

        IReadOnlyList<Expression> CompileStackTrace(int frameCountMax)
        {
            var frameCount = Math.Min(frameCountMax, stack.Count - 1);

            var stackTrace = new Expression[frameCount];

            for (int i = 0; i < frameCount; i++)
            {
                stackTrace[i] = stack.ElementAt(i + 1).Expression;
            }

            return stackTrace;
        }

        buildAndPushStackFrame(
            expressionValue: null,
            rootExpression,
            rootEnvironment);

        static ExecutionErrorReport buildErrorReport(StackFrame stackFrame)
        {
            return
                new(
                    FrameExpression: stackFrame.Expression,
                    EnvironmentValue: stackFrame.EnvironmentValue,
                    Instructions: stackFrame.Instructions,
                    FrameInstructionPointer: stackFrame.InstructionPointer);
        }

        while (true)
        {
            var currentFrame = stack.Peek();

            ++instructionCount;

            ++currentFrame.InstructionCount;

            try
            {
                if (currentFrame.Specialization is { } specializedFrame)
                {
                    var stepResult = specializedFrame.CurrentStep;

                    if (stepResult is ApplyStepwise.StepResult.Complete complete)
                    {
                        var returnOverall =
                            returnFromStackFrame(complete.PineValue);

                        if (returnOverall is not null)
                        {
                            return returnOverall;
                        }

                        continue;
                    }

                    if (stepResult is ApplyStepwise.StepResult.Continue cont)
                    {
                        if (invokePrecompiledOrBuildStackFrame(
                            expressionValue: null,
                            expression: cont.Expression,
                            environmentValue: cont.EnvironmentValue) is { } error)
                        {
                            return error;
                        }

                        continue;
                    }

                    throw new NotImplementedException(
                        "Unexpected step result type: " + stepResult.GetType().FullName);
                }


                if (currentFrame.Instructions.Instructions.Count <= currentFrame.InstructionPointer)
                {
                    return
                        "Instruction pointer out of bounds. Missing explicit return instruction.";
                }

                var currentInstruction =
                    currentFrame.Instructions.Instructions[currentFrame.InstructionPointer]
                    ??
                    throw new InvalidOperationException("currentInstruction is null");

                var instructionKind = currentInstruction.Kind;

                switch (instructionKind)
                {
                    case StackInstructionKind.Push_Literal:
                        {
                            currentFrame.PushInstructionResult(
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value"));

                            continue;
                        }

                    case StackInstructionKind.Push_Environment:
                        {
                            currentFrame.PushInstructionResult(currentFrame.EnvironmentValue);

                            continue;
                        }

                    case StackInstructionKind.Equal_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var areEqual = left == right;

                            currentFrame.PushInstructionResult(
                                areEqual ? PineVMValues.TrueValue : PineVMValues.FalseValue);

                            continue;
                        }

                    case StackInstructionKind.Equal_Binary_Const:
                        {
                            var right = currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            var areEqual = left == right;

                            currentFrame.PushInstructionResult(
                                areEqual ? PineVMValues.TrueValue : PineVMValues.FalseValue);

                            continue;
                        }

                    case StackInstructionKind.Not_Equal_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var areEqual = left == right;

                            currentFrame.PushInstructionResult(
                                areEqual ? PineVMValues.FalseValue : PineVMValues.TrueValue);

                            continue;
                        }

                    case StackInstructionKind.Not_Equal_Binary_Const:
                        {
                            var right = currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            var areEqual = left == right;

                            currentFrame.PushInstructionResult(
                                areEqual ? PineVMValues.FalseValue : PineVMValues.TrueValue);

                            continue;
                        }

                    case StackInstructionKind.Length:
                        {
                            var listValue = currentFrame.PopTopmostFromStack();

                            var lengthValue = KernelFunction.length(listValue);

                            currentFrame.PushInstructionResult(lengthValue);

                            continue;
                        }

                    case StackInstructionKind.Skip_Head_Const:
                        {
                            var index =
                                currentInstruction.SkipCount
                                ??
                                throw new Exception("Invalid operation form: Missing index value");

                            var indexClamped =
                                index < 0 ? 0 : index;

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var fromIndexValue =
                                prevValue switch
                                {
                                    PineValue.ListValue listValue =>
                                    listValue.Elements.Length <= indexClamped
                                    ?
                                    PineValue.EmptyList
                                    :
                                    listValue.Elements.Span[indexClamped],

                                    _ =>
                                    PineValue.EmptyList
                                };

                            currentFrame.PushInstructionResult(fromIndexValue);

                            continue;
                        }

                    case StackInstructionKind.Skip_Head_Binary:
                        {
                            var indexValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(indexValue) is { } skipCount)
                            {
                                var skipCountInt = (int)skipCount;

                                var skipCountClamped =
                                    skipCountInt < 0 ? 0 : skipCountInt;

                                resultValue =
                                    prevValue switch
                                    {
                                        PineValue.ListValue listValue =>
                                        listValue.Elements.Length <= skipCountClamped
                                        ?
                                        PineValue.EmptyList
                                        :
                                        listValue.Elements.Span[skipCountClamped],

                                        _ =>
                                        PineValue.EmptyList
                                    };
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Head_Generic:
                        {
                            var prevValue = currentFrame.PopTopmostFromStack();

                            var headValue = KernelFunction.head(prevValue);

                            currentFrame.PushInstructionResult(headValue);

                            continue;
                        }

                    case StackInstructionKind.Skip_Binary:
                        {
                            var skipCountValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(skipCountValue) is { } skipCount)
                            {
                                resultValue = KernelFunction.skip(skipCount, prevValue);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Take_Binary:
                        {
                            var takeCountValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(takeCountValue) is { } takeCount)
                            {
                                resultValue = KernelFunction.take(takeCount, prevValue);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Build_List:
                        {
                            var itemsCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var items = new PineValue[itemsCount];

                            for (int i = 0; i < itemsCount; ++i)
                            {
                                items[itemsCount - i - 1] = currentFrame.PopTopmostFromStack();
                            }

                            currentFrame.PushInstructionResult(PineValue.List(items));

                            continue;
                        }

                    case StackInstructionKind.Concat_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            currentFrame.PushInstructionResult(KernelFunction.concat(left, right));

                            continue;
                        }

                    case StackInstructionKind.Prepend_List_Item_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValue.EmptyList;

                            if (right is PineValue.ListValue rightList)
                            {
                                var elements = new PineValue[rightList.Elements.Length + 1];

                                elements[0] = left;

                                rightList.Elements.Span.CopyTo(elements.AsSpan(1));

                                resultValue = PineValue.List(elements);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Concat_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack();

                            var concatenated = KernelFunction.concat(listValue);

                            currentFrame.PushInstructionResult(concatenated);

                            continue;
                        }

                    case StackInstructionKind.Slice_Skip_Var_Take_Var:
                        {
                            var takeCountValue = currentFrame.PopTopmostFromStack();
                            var skipCountValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(takeCountValue) is { } takeCount &&
                                KernelFunction.SignedIntegerFromValueRelaxed(skipCountValue) is { } skipCount)
                            {
                                resultValue =
                                    FusedSkipAndTake(
                                        prevValue,
                                        skipCount: (int)skipCount,
                                        takeCount: (int)takeCount);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Slice_Skip_Var_Take_Const:
                        {
                            var takeCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var skipCountValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(skipCountValue) is { } skipCount)
                            {
                                resultValue =
                                    FusedSkipAndTake(
                                        prevValue,
                                        skipCount: (int)skipCount,
                                        takeCount: takeCount);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Reverse:
                        {
                            var listValue = currentFrame.PopTopmostFromStack();

                            var reversed = KernelFunction.reverse(listValue);

                            currentFrame.PushInstructionResult(reversed);

                            continue;
                        }

                    case StackInstructionKind.Local_Set:
                        {
                            var fromStack = currentFrame.PeekTopmostFromStack();

                            currentFrame.LocalSet(
                                currentInstruction.LocalIndex
                                ??
                                throw new Exception("Invalid operation form: Missing local index"),
                                fromStack);

                            currentFrame.InstructionPointer++;

                            continue;
                        }

                    case StackInstructionKind.Local_Get:
                        {
                            var value =
                                currentFrame.LocalGet(
                                    currentInstruction.LocalIndex
                                    ??
                                    throw new Exception("Invalid operation form: Missing local index"));

                            currentFrame.PushInstructionResult(value);

                            continue;
                        }

                    case StackInstructionKind.Int_Add_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = KernelFunction.int_add(left, right);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Add_Const:
                        {
                            var rightInt =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var leftValue = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(leftValue) is { } leftInt)
                            {
                                resultValue = PineValueAsInteger.ValueFromSignedInteger(leftInt + rightInt);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Sub_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(left) is { } leftInt &&
                                KernelFunction.SignedIntegerFromValueRelaxed(right) is { } rightInt)
                            {
                                resultValue = PineValueAsInteger.ValueFromSignedInteger(leftInt - rightInt);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Mul_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(left) is { } leftInt &&
                                KernelFunction.SignedIntegerFromValueRelaxed(right) is { } rightInt)
                            {
                                resultValue = PineValueAsInteger.ValueFromSignedInteger(leftInt * rightInt);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Mul_Const:
                        {
                            var right = currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(left) is { } leftInt)
                            {
                                resultValue = PineValueAsInteger.ValueFromSignedInteger(leftInt * right);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Less_Than_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(left) is { } leftInt &&
                                KernelFunction.SignedIntegerFromValueRelaxed(right) is { } rightInt)
                            {
                                resultValue =
                                    leftInt < rightInt ?
                                    PineVMValues.TrueValue :
                                    PineVMValues.FalseValue;
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Less_Than_Or_Equal_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(left) is { } leftInt &&
                                KernelFunction.SignedIntegerFromValueRelaxed(right) is { } rightInt)
                            {
                                resultValue =
                                    leftInt <= rightInt ?
                                    PineVMValues.TrueValue :
                                    PineVMValues.FalseValue;
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Negate:
                        {
                            var value = currentFrame.PopTopmostFromStack();

                            var resultValue = KernelFunction.negate(value);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Return:
                        {
                            var frameReturnValue =
                                currentFrame.PopTopmostFromStack();

                            var returnOverall =
                                returnFromStackFrame(frameReturnValue);

                            if (returnOverall is not null)
                            {
                                return returnOverall;
                            }

                            continue;
                        }

                    case StackInstructionKind.Skip_Generic:
                        {
                            var genericValue = currentFrame.PopTopmostFromStack();

                            var resultValue = KernelFunction.skip(genericValue);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Take_Generic:
                        {
                            var genericValue = currentFrame.PopTopmostFromStack();

                            var resultValue = KernelFunction.take(genericValue);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Is_Sorted_Asc_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack();

                            var isSorted = KernelFunction.int_is_sorted_asc(listValue);

                            currentFrame.PushInstructionResult(isSorted);

                            continue;
                        }

                    case StackInstructionKind.Parse_And_Eval_Binary:
                        {
                            {
                                ++parseAndEvalCount;

                                if (config.ParseAndEvalCountLimit is { } limit && parseAndEvalCount > limit)
                                {
                                    var stackTraceHashes =
                                        CompileStackTrace(100)
                                        .Select(expr => mutableCache.ComputeHash(ExpressionEncoding.EncodeExpressionAsValue(expr)))
                                        .ToArray();

                                    return
                                        "Parse and eval count limit exceeded: " +
                                        CommandLineInterface.FormatIntegerForDisplay(limit) +
                                        "\nLast stack frames expressions:\n" +
                                        string.Join("\n", stackTraceHashes.Select(hash => Convert.ToHexStringLower(hash.Span)[..8]));
                                }
                            }

                            var expressionValue = currentFrame.PopTopmostFromStack();

                            var environmentValue = currentFrame.PopTopmostFromStack();

                            {
                                if (InvocationCachedResultOrOverride(
                                    expressionValue: expressionValue,
                                    environmentValue: environmentValue) is { } fromCacheOrDelegate)
                                {
                                    currentFrame.PushInstructionResult(fromCacheOrDelegate);

                                    continue;
                                }
                            }

                            var parseResult = parseCache.ParseExpression(expressionValue);

                            if (parseResult.IsErrOrNull() is { } parseErr)
                            {
                                return
                                    "Failed to parse expression from value: " + parseErr +
                                    " - expressionValue is " + DescribeValueForErrorMessage(expressionValue) +
                                    " - environmentValue is " + DescribeValueForErrorMessage(environmentValue);
                            }

                            if (parseResult.IsOkOrNull() is not { } parseOk)
                            {
                                throw new NotImplementedException(
                                    "Unexpected result type: " + parseResult.GetType().FullName);
                            }

                            {
                                if (invokePrecompiledOrBuildStackFrame(
                                    expressionValue: expressionValue,
                                    parseOk,
                                    environmentValue) is { } error)
                                {
                                    return error;
                                }

                                continue;
                            }
                        }

                    case StackInstructionKind.Jump_Const:
                        {
                            var jumpOffset =
                                currentInstruction.JumpOffset
                                ??
                                throw new Exception("Invalid operation form: Missing jump offset");

                            currentFrame.InstructionPointer += jumpOffset;

                            if (jumpOffset < 0)
                            {
                                loopIterationCount++;
                                currentFrame.LoopIterationCount++;
                            }

                            continue;
                        }

                    case StackInstructionKind.Jump_If_True_Const:
                        {
                            var conditionValue = currentFrame.PopTopmostFromStack();

                            if (conditionValue == PineVMValues.TrueValue)
                            {
                                var jumpOffset =
                                    currentInstruction.JumpOffset
                                    ??
                                    throw new Exception("Invalid operation form: Missing jump offset");

                                currentFrame.InstructionPointer += 1 + jumpOffset;

                                if (jumpOffset < 0)
                                {
                                    loopIterationCount++;
                                    currentFrame.LoopIterationCount++;
                                }

                                continue;
                            }

                            currentFrame.InstructionPointer++;

                            continue;
                        }

                    case StackInstructionKind.Bit_And_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = KernelFunction.bit_and(PineValue.List([left, right]));

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Or_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = KernelFunction.bit_or(PineValue.List([left, right]));

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Xor_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = KernelFunction.bit_xor(PineValue.List([left, right]));

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Not:
                        {
                            var value = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = KernelFunction.bit_not(value);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Left_Binary:
                        {
                            var shiftValue = currentFrame.PopTopmostFromStack();
                            var value = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (value is PineValue.BlobValue blobValue)
                            {
                                if (KernelFunction.SignedIntegerFromValueRelaxed(shiftValue) is { } shiftCount)
                                {
                                    resultValue = KernelFunction.bit_shift_left(shiftCount, blobValue);
                                }
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Right_Binary:
                        {
                            var shiftValue = currentFrame.PopTopmostFromStack();
                            var value = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (value is PineValue.BlobValue blobValue)
                            {
                                if (KernelFunction.SignedIntegerFromValueRelaxed(shiftValue) is { } shiftCount)
                                {
                                    resultValue = KernelFunction.bit_shift_right(shiftCount, blobValue);
                                }
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Add_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack();

                            var sumValue = KernelFunction.int_add(listValue);

                            currentFrame.PushInstructionResult(sumValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Mul_Generic:
                        {
                            var listValue = currentFrame.PopTopmostFromStack();

                            var productValue = KernelFunction.int_mul(listValue);

                            currentFrame.PushInstructionResult(productValue);

                            continue;
                        }

                    case StackInstructionKind.Pop:
                        {
                            currentFrame.PopTopmostFromStack();

                            currentFrame.InstructionPointer++;

                            continue;
                        }

                    default:
                        throw new NotImplementedException(
                            "Unexpected instruction kind: " + instructionKind);
                }
            }
            catch (Exception e)
            {
                var errorReport = buildErrorReport(currentFrame);

                throw new InvalidIntermediateCodeException(
                    e.Message,
                    innerException: e,
                    errorReport);
            }
        }
    }

    public static IEnumerable<string> DisplayText(ExecutionErrorReport errorReport)
    {
        var expressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(errorReport.FrameExpression);

        var exprHash =
            mutableCache.ComputeHash(expressionValue);

        var exprHashBase16 = Convert.ToHexStringLower(exprHash.Span);

        var envHash =
            mutableCache.ComputeHash(errorReport.EnvironmentValue);

        var envHashBase16 = Convert.ToHexStringLower(envHash.Span);

        yield return
            "Instruction " + errorReport.FrameInstructionPointer +
            " in expression: " + exprHashBase16[..8] + " for environment " +
            envHashBase16[..8];

        var specializationText =
            errorReport.Instructions.TrackEnvConstraint is { } trackEnvConstraint
            ? "specialized with " + trackEnvConstraint.HashBase16[0..8]
            : "not specialized";

        yield return
            specializationText + " has " +
            errorReport.Instructions.Instructions.Count + " instructions";
    }

    public PineValue EvaluateExpressionDefaultLessStack(
        Expression expression,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        if (expression is Expression.Literal literalExpression)
            return literalExpression.Value;

        if (expression is Expression.List listExpression)
        {
            return EvaluateListExpression(listExpression, environment, stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.ParseAndEval applicationExpression)
        {
            return
                EvaluateParseAndEvalExpression(
                    applicationExpression,
                    environment,
                    stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.KernelApplication kernelApplicationExpression)
        {
            return
                EvaluateKernelApplicationExpression(
                    environment,
                    kernelApplicationExpression,
                    stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.Conditional conditionalExpression)
        {
            return EvaluateConditionalExpression(
                environment,
                conditionalExpression,
                stackPrevValues: stackPrevValues);
        }

        if (expression is Expression.Environment)
        {
            return environment;
        }

        if (expression is Expression.StringTag stringTagExpression)
        {
            return EvaluateExpressionDefaultLessStack(
                stringTagExpression.Tagged,
                environment,
                stackPrevValues: stackPrevValues);
        }

        throw new NotImplementedException(
            "Unexpected shape of expression: " + expression.GetType().FullName);
    }

    public PineValue EvaluateListExpression(
        Expression.List listExpression,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var listItems = new PineValue[listExpression.items.Count];

        for (var i = 0; i < listExpression.items.Count; i++)
        {
            var item = listExpression.items[i];

            var itemResult =
                EvaluateExpressionDefaultLessStack(
                    item,
                    environment,
                    stackPrevValues: stackPrevValues);

            listItems[i] = itemResult;
        }

        return PineValue.List(listItems);
    }

    public PineValue EvaluateParseAndEvalExpression(
        Expression.ParseAndEval parseAndEval,
        PineValue environment,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var environmentValue =
            EvaluateExpressionDefaultLessStack(
                parseAndEval.Environment,
                environment,
                stackPrevValues: stackPrevValues);

        var expressionValue =
            EvaluateExpressionDefaultLessStack(
                parseAndEval.Encoded,
                environment,
                stackPrevValues: stackPrevValues);


        if (InvocationCachedResultOrOverride(
            expressionValue: expressionValue,
            environmentValue: environmentValue) is { } fromCacheOrDelegate)
        {
            return fromCacheOrDelegate;
        }

        var parseResult = parseCache.ParseExpression(expressionValue);

        if (parseResult is Result<string, Expression>.Err parseErr)
        {
            var message =
                "Failed to parse expression from value: " + parseErr.Value +
                " - expressionValue is " + DescribeValueForErrorMessage(expressionValue) +
                " - environmentValue is " + DescribeValueForErrorMessage(environmentValue);

            throw new ParseExpressionException(message);
        }

        if (parseResult is not Result<string, Expression>.Ok parseOk)
        {
            throw new NotImplementedException("Unexpected result type: " + parseResult.GetType().FullName);
        }

        if (environmentValue is PineValue.ListValue list)
        {
            FunctionApplicationMaxEnvSize =
            FunctionApplicationMaxEnvSize < list.Elements.Length ? list.Elements.Length : FunctionApplicationMaxEnvSize;
        }

        return
            EvaluateExpressionDefaultLessStack(
                environment: environmentValue,
                expression: parseOk.Value,
                stackPrevValues: ReadOnlyMemory<PineValue>.Empty);
    }

    private PineValue? InvocationCachedResultOrOverride(
        PineValue expressionValue,
        PineValue environmentValue)
    {
        if (EvalCache is { } evalCache)
        {
            var cacheKey = new EvalCacheEntryKey(ExprValue: expressionValue, EnvValue: environmentValue);

            if (evalCache.TryGetValue(cacheKey, out var fromCache))
            {
                return fromCache;
            }
        }

        if (overrideInvocations?.TryGetValue(expressionValue, out var overrideValue) ?? false)
        {
            var result =
            overrideValue(
                (expr, envVal) => EvaluateExpressionDefaultLessStack(
                    expr,
                    envVal,
                    stackPrevValues: ReadOnlyMemory<PineValue>.Empty),
                environmentValue);

            return
                result
                .Extract(err => throw new Exception(err));
        }

        return null;
    }

    public static string DescribeValueForErrorMessage(PineValue pineValue) =>
        PineValueAsString.StringFromValue(pineValue)
        .Unpack(fromErr: _ => "not a string", fromOk: asString => "string \'" + asString + "\'");

    public PineValue EvaluateKernelApplicationExpression(
        PineValue environment,
        Expression.KernelApplication application,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        if (application.Function is nameof(KernelFunction.head) &&
            application.Input is Expression.KernelApplication innerKernelApplication)
        {
            if (innerKernelApplication.Function is nameof(KernelFunction.skip) &&
                innerKernelApplication.Input is Expression.List skipListExpr &&
                skipListExpr.items.Count is 2)
            {
                var skipValue =
                    EvaluateExpressionDefaultLessStack(
                        skipListExpr.items[0],
                        environment,
                        stackPrevValues);

                if (KernelFunction.SignedIntegerFromValueRelaxed(skipValue) is { } skipCount)
                {
                    if (EvaluateExpressionDefaultLessStack(
                        skipListExpr.items[1],
                        environment,
                        stackPrevValues) is PineValue.ListValue list)
                    {
                        if (list.Elements.Length < 1 || list.Elements.Length <= skipCount)
                        {
                            return PineValue.EmptyList;
                        }

                        return list.Elements.Span[skipCount < 0 ? 0 : (int)skipCount];
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
        Expression.KernelApplication application,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var inputValue = EvaluateExpressionDefaultLessStack(application.Input, environment, stackPrevValues);

        return
            EvaluateKernelApplicationGeneric(
                function: application.Function,
                inputValue: inputValue);
    }

    public static PineValue EvaluateKernelApplicationGeneric(
        string function,
        PineValue inputValue)
    {
        return function switch
        {
            nameof(KernelFunction.equal) =>
            KernelFunction.equal(inputValue),

            nameof(KernelFunction.length) =>
            KernelFunction.length(inputValue),

            nameof(KernelFunction.head) =>
            KernelFunction.head(inputValue),

            nameof(KernelFunction.skip) =>
            KernelFunction.skip(inputValue),

            nameof(KernelFunction.take) =>
            KernelFunction.take(inputValue),

            nameof(KernelFunction.concat) =>
            KernelFunction.concat(inputValue),

            nameof(KernelFunction.reverse) =>
            KernelFunction.reverse(inputValue),

            nameof(KernelFunction.negate) =>
            KernelFunction.negate(inputValue),

            nameof(KernelFunction.int_add) =>
            KernelFunction.int_add(inputValue),

            nameof(KernelFunction.int_mul) =>
            KernelFunction.int_mul(inputValue),

            nameof(KernelFunction.int_is_sorted_asc) =>
            KernelFunction.int_is_sorted_asc(inputValue),

            nameof(KernelFunction.bit_and) =>
            KernelFunction.bit_and(inputValue),

            nameof(KernelFunction.bit_or) =>
            KernelFunction.bit_or(inputValue),

            nameof(KernelFunction.bit_xor) =>
            KernelFunction.bit_xor(inputValue),

            nameof(KernelFunction.bit_not) =>
            KernelFunction.bit_not(inputValue),

            nameof(KernelFunction.bit_shift_left) =>
            KernelFunction.bit_shift_left(inputValue),

            nameof(KernelFunction.bit_shift_right) =>
            KernelFunction.bit_shift_right(inputValue),

            _ =>
            throw new ParseExpressionException(
                "Did not find kernel function '" + function + "'")
        };
    }

    public static PineValue FusedSkipAndTake(PineValue argument, int skipCount, int takeCount)
    {
        skipCount =
            skipCount < 0 ? 0 : skipCount;

        if (argument is PineValue.ListValue argumentList)
        {
            var takeLimit =
                argumentList.Elements.Length - skipCount;

            takeCount =
                takeLimit < takeCount
                ?
                takeLimit
                :
                takeCount;

            if (takeCount < 1)
                return PineValue.EmptyList;

            if (skipCount is 0 && takeCount == argumentList.Elements.Length)
                return argument;

            var slicedItems = new PineValue[takeCount];

            for (var i = 0; i < takeCount; ++i)
            {
                slicedItems[i] = argumentList.Elements.Span[skipCount + i];
            }

            return PineValue.List(slicedItems);
        }

        if (argument is PineValue.BlobValue argumentBlob)
        {
            var takeLimit =
                argumentBlob.Bytes.Length - skipCount;

            takeCount =
                takeLimit < takeCount
                ?
                takeLimit
                :
                takeCount;

            if (takeCount < 1)
                return PineValue.EmptyBlob;

            if (skipCount is 0 && takeCount == argumentBlob.Bytes.Length)
                return argument;

            var slicedBytes =
                argumentBlob.Bytes.Slice(start: skipCount, length: takeCount);

            return PineValue.Blob(slicedBytes);
        }

        throw new NotImplementedException(
            "Unexpected argument type: " + argument.GetType());
    }

    public static PineValue ValueFromPathInValueOrEmptyList(
        PineValue environment,
        ReadOnlySpan<int> path)
    {
        var currentNode = environment;

        for (var i = 0; i < path.Length; i++)
        {
            if (currentNode is not PineValue.ListValue listValue)
                return PineValue.EmptyList;

            var skipCount = path[i];

            if (skipCount >= listValue.Elements.Length)
                return PineValue.EmptyList;

            currentNode = listValue.Elements.Span[skipCount < 0 ? 0 : skipCount];
        }

        return currentNode;
    }

    public PineValue EvaluateConditionalExpression(
        PineValue environment,
        Expression.Conditional conditional,
        ReadOnlyMemory<PineValue> stackPrevValues)
    {
        var conditionValue =
            EvaluateExpressionDefaultLessStack(
                conditional.Condition,
                environment,
                stackPrevValues: stackPrevValues);

        if (conditionValue == PineVMValues.TrueValue)
        {
            return EvaluateExpressionDefaultLessStack(
                conditional.TrueBranch,
                environment,
                stackPrevValues: stackPrevValues);
        }

        return EvaluateExpressionDefaultLessStack(
            conditional.FalseBranch,
            environment,
            stackPrevValues: stackPrevValues);
    }
}
