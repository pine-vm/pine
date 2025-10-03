using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using Pine.Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using KernelFunctionSpecialized = Pine.Core.Internal.KernelFunctionSpecialized;

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

    private readonly IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses;

    private readonly bool disableReductionInCompilation;

    private readonly bool disablePrecompiled;

    private readonly bool enableTailRecursionOptimization;

    public readonly PineVMParseCache parseCache = new();

    private readonly IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>? overrideInvocations;

    private readonly IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? _precompiledLeaves;

    private readonly Action<PineValue, PineValue>? _reportEnterPrecompiledLeaf;

    private readonly Action<PineValue, PineValue, PineValue?>? _reportExitPrecompiledLeaf;

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
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? compilationEnvClasses = null,
        bool disableReductionInCompilation = false,
        bool disablePrecompiled = false,
        bool enableTailRecursionOptimization = false,
        IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>? precompiledLeaves = null,
        Action<PineValue, PineValue>? reportEnterPrecompiledLeaf = null,
        Action<PineValue, PineValue, PineValue?>? reportExitPrecompiledLeaf = null,
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

        _precompiledLeaves =
            precompiledLeaves
            ??
            Core.Bundle.BundledPineToDotnet.LoadBundledTask.Result?.BuildDictionary();

        _reportEnterPrecompiledLeaf = reportEnterPrecompiledLeaf;
        _reportExitPrecompiledLeaf = reportExitPrecompiledLeaf;

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
        PineValueClass? TrackEnvConstraint = null)
    {
        public int MaxLocalIndex { init; get; } =
            Instructions
            .Select(i => i.Kind is StackInstructionKind.Local_Set ? i.LocalIndex ?? 0 : 0)
            .DefaultIfEmpty(-1)
            .Max();

        public int MaxStackUsage { init; get; } =
            ComputeMaxStackUsage(Instructions);

        public static int ComputeMaxStackUsage(
            IReadOnlyList<StackInstruction> instructions)
        {
            IEnumerable<int> GetSuccessors(
                int instructionIndex)
            {
                var inst = instructions[instructionIndex];

                switch (inst.Kind)
                {
                    case StackInstructionKind.Return:
                        yield break;

                    case StackInstructionKind.Jump_Const:

                        yield return
                            instructionIndex +
                            (inst.JumpOffset ?? throw new InvalidOperationException(
                                $"Jump without offset at {instructionIndex}."));

                        break;

                    case StackInstructionKind.Jump_If_True_Const:

                        // fall-through
                        yield return instructionIndex + 1;

                        yield return
                            instructionIndex + 1 +
                            (inst.JumpOffset ?? throw new InvalidOperationException(
                                $"Jump without offset at {instructionIndex}."));

                        break;

                    default:
                        // ordinary instruction
                        yield return instructionIndex + 1;
                        break;
                }
            }

            var instructionsDetails =
                instructions.Select(StackInstruction.GetDetails).ToArray();

            var stackDepthIn = new int?[instructions.Count];     // stack height *before* executing i

            var worklist = new Stack<int>();

            stackDepthIn[0] = 0;
            worklist.Push(0);

            var maxDepth = 0;

            while (worklist.Count > 0)
            {
                var instructionIndex = worklist.Pop();

                var instructionDetails =
                    instructionsDetails[instructionIndex];

                var inDepth =
                    stackDepthIn[instructionIndex]!.Value;

                var delta =
                    -instructionDetails.PopCount + instructionDetails.PushCount;

                var outDepth = inDepth + delta;

                if (outDepth < 0)
                    throw new InvalidOperationException($"Stack under-flow at instruction {instructionIndex}.");

                maxDepth = Math.Max(maxDepth, outDepth);

                foreach (var successorIndex in GetSuccessors(instructionIndex))
                {
                    if (successorIndex >= instructions.Count)
                        continue;

                    if (stackDepthIn[successorIndex] is null)
                    {
                        stackDepthIn[successorIndex] = outDepth;
                        worklist.Push(successorIndex);
                    }
                    else if (stackDepthIn[successorIndex] != outDepth)
                    {
                        // Byte-code would be ill-formed; surface a clear error.
                        throw new InvalidOperationException(
                            $"Inconsistent stack depth at {successorIndex} " +
                            $"({stackDepthIn[successorIndex]} vs {outDepth}).");
                    }
                }
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

    record struct StackFrameProfilingBaseline(
        long BeginInstructionCount,
        long BeginParseAndEvalCount,
        long BeginStackFrameCount);

    record StackFrame(
        PineValue? ExpressionValue,
        Expression Expression,
        StackFrameInstructions Instructions,
        PineValue EnvironmentValue,
        Memory<PineValue> StackValues,
        Memory<PineValue> LocalsValues,
        StackFrameProfilingBaseline ProfilingBaseline,
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

                var foundMismatch = false;

                for (var specializationIndex = 0; specializationIndex < specialization.constraint.Count; specializationIndex++)
                {
                    var constraintItem = specialization.constraint[specializationIndex];

                    if (Core.CodeAnalysis.CodeAnalysis.ValueFromPathInValue(environment, constraintItem.Path.Span) is not { } pathValue)
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

    readonly Dictionary<Expression, ExpressionCompilation> _expressionCompilationDict = [];

    readonly static Core.Addressing.ConcurrentPineValueHashCache s_mutableCacheValueHash = new();

    StackFrame StackFrameFromExpression(
        PineValue? expressionValue,
        Expression expression,
        PineValue environment,
        StackFrameProfilingBaseline profilingBaseline)
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
            ProfilingBaseline: profilingBaseline,
            Specialization: null);
    }

    public ExpressionCompilation GetExpressionCompilation(
        Expression rootExpression)
    {
        if (_expressionCompilationDict.TryGetValue(rootExpression, out var cachedCompilation))
        {
            return cachedCompilation;
        }

        var compilation = ExpressionCompilationLessCache(rootExpression);

        _expressionCompilationDict[rootExpression] = compilation;

        return compilation;
    }

    public ExpressionCompilation ExpressionCompilationLessCache(Expression rootExpression)
    {
        IReadOnlyList<PineValueClass>? specializations = null;

        compilationEnvClasses?.TryGetValue(rootExpression, out specializations);

        bool skipInlining(Expression expr, PineValueClass? envConstraintId)
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
        IReadOnlyList<PineValueClass> specializations,
        PineVMParseCache parseCache,
        bool disableReduction,
        bool enableTailRecursionOptimization,
        Func<Expression, PineValueClass?, bool> skipInlining)
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
            .OrderDescending(PineValueClassSpecificityComparer.Instance)
            .Select(
                specialization =>
                            ((IReadOnlyList<EnvConstraintItem>)
                            [..specialization.ParsedItems
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
        PineValueClass? envConstraintId,
        PineVMParseCache parseCache,
        bool disableReduction,
        bool enableTailRecursionOptimization,
        Func<Expression, PineValueClass?, bool> skipInlining)
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

        var allInstructionsBeforeReturn =
            InstructionsFromExpression(
                rootExpression: reducedExpression,
                rootExprAlternativeForms: [rootExpression],
                envClass: enableTailRecursionOptimization ? envConstraintId : null,
                parseCache)
            .ToArray();

        for (var instructionIndex = allInstructionsBeforeReturn.Length - 1; instructionIndex >= 0; instructionIndex--)
        {
            var instruction = allInstructionsBeforeReturn[instructionIndex];

            if (instruction.Kind is StackInstructionKind.Jump_Const)
            {
                var jumpOffset =
                    instruction.JumpOffset
                    ??
                    throw new InvalidOperationException(
                        "Jump instruction without offset: " + instruction);

                var destInstructionIndex =
                    instructionIndex + jumpOffset;

                if (destInstructionIndex >= allInstructionsBeforeReturn.Length)
                {
                    allInstructionsBeforeReturn[instructionIndex] =
                        StackInstruction.Return;

                    continue;
                }

                var destInstruction =
                    allInstructionsBeforeReturn[destInstructionIndex];

                if (destInstruction.Kind is StackInstructionKind.Return)
                {
                    // Remove jump to return instruction.
                    allInstructionsBeforeReturn[instructionIndex] =
                        StackInstruction.Return;
                }
            }
        }

        IReadOnlyList<StackInstruction> allInstructions =
            [.. allInstructionsBeforeReturn,
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
            ReducePineExpression.ReduceExpressionBottomUp(
                currentExpression,
                parseCache);

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

            if (ReducePineExpression.TryEvaluateExpressionIndependent(
                parseAndEvalExpr.Encoded, parseCache).IsOkOrNull() is not { } exprValue)
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
                ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
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
                ReducePineExpression.ReduceExpressionBottomUp(
                    inlinedExpr,
                    parseCache);

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
            ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
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
            ReducePineExpression.ReduceExpressionBottomUp(expressionInlined, parseCache);

        return expressionInlinedReduced;
    }

    public static Expression ReduceExpressionAndInlineRecursive(
        Expression rootExpression,
        ImmutableHashSet<Expression> rootExprAlternativeForms,
        PineValueClass? envConstraintId,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, PineValueClass?, bool> skipInlining) =>
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
        PineValueClass? envConstraintId,
        ImmutableHashSet<Expression> rootExprForms,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, PineValueClass?, bool> skipInlining)
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
            ReducePineExpression.ReduceExpressionBottomUp(expressionSubstituted, parseCache);

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
                    ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
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
                    ReducePineExpression.ReduceExpressionBottomUp(inlinedExprSubstituted, parseCache);

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
                if (ReducePineExpression.TryEvaluateExpressionIndependent(
                    parseAndEvalExpr.Encoded, parseCache).IsOkOrNull() is
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
                ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
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
            ReducePineExpression.ReduceExpressionBottomUp(expressionInlined, parseCache);

        return expressionInlinedReduced;
    }

    public static Expression SubstituteSubexpressionsForEnvironmentConstraint(
        Expression originalExpression,
        PineValueClass envConstraintId)
    {
        return
            ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                findReplacement:
                descendant =>
                {
                    if (Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(descendant) is { } indexPath)
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
        PineValueClass? envClass,
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
                for (var i = 0; i < list.Items.Count; ++i)
                {
                    stack.Push(list.Items[i]);
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
        long stackFrameReplaceCount = 0;
        long lastCacheEntryInstructionCount = 0;
        long lastCacheEntryParseAndEvalCount = 0;

        var stack = new Stack<StackFrame>();

        string? InvokePrecompiledOrBuildStackFrame(
            PineValue? expressionValue,
            Expression expression,
            PineValue environmentValue,
            bool replaceCurrentFrame)
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
                                InvokePrecompiledOrBuildStackFrame(
                                    expressionValue: continueParseAndEval.ExpressionValue,
                                    expression: contParseOk,
                                    environmentValue: continueParseAndEval.EnvironmentValue,
                                    replaceCurrentFrame: replaceCurrentFrame);
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
                                    ProfilingBaseline:
                                    new StackFrameProfilingBaseline(
                                        BeginInstructionCount: instructionCount,
                                        BeginParseAndEvalCount: parseAndEvalCount,
                                        BeginStackFrameCount: stackFrameCount),
                                    Specialization: specialization.Stepwise);

                            pushStackFrame(
                                newFrame,
                                replaceCurrentFrame: false);

                            return null;
                        }

                    default:
                        throw new Exception(
                            "Unexpected return type from precompiled: " + precompiledResult.GetType().FullName);
                }
            }
            else
            {
                if (_precompiledLeaves is not null && expressionValue is not null)
                {
                    if (_precompiledLeaves.TryGetValue(expressionValue, out var computeLeafDelegate))
                    {
                        _reportEnterPrecompiledLeaf?.Invoke(expressionValue, environmentValue);

                        var valueComputedInLeaf = computeLeafDelegate(expressionValue);

                        _reportExitPrecompiledLeaf?.Invoke(expressionValue, environmentValue, valueComputedInLeaf);

                        if (valueComputedInLeaf is { } computedValue)
                        {
                            currentFrame.PushInstructionResult(computedValue);

                            return null;
                        }
                    }
                }

                buildAndPushStackFrame
                (
                    expressionValue: expressionValue,
                    expression: expression,
                    environmentValue: environmentValue,
                    replaceCurrentFrame: replaceCurrentFrame
                );

                return null;
            }
        }

        void buildAndPushStackFrame(
            PineValue? expressionValue,
            Expression expression,
            PineValue environmentValue,
            bool replaceCurrentFrame)
        {
            var newFrameProfilingBaseline =
                replaceCurrentFrame
                ?
                stack.Peek().ProfilingBaseline
                :
                new StackFrameProfilingBaseline(
                    BeginInstructionCount: instructionCount,
                    BeginParseAndEvalCount: parseAndEvalCount,
                    BeginStackFrameCount: stackFrameCount);

            var newFrame =
                StackFrameFromExpression(
                    expressionValue: expressionValue,
                    expression: expression,
                    environment: environmentValue,
                    profilingBaseline: newFrameProfilingBaseline);

            pushStackFrame(newFrame, replaceCurrentFrame: replaceCurrentFrame);
        }

        void pushStackFrame(
            StackFrame newFrame,
            bool replaceCurrentFrame)
        {
            if (replaceCurrentFrame)
            {
                stack.Pop();

                ++stackFrameReplaceCount;
            }

            stack.Push(newFrame);

            ++stackFrameCount;
        }

        EvaluationReport? returnFromStackFrame(PineValue frameReturnValue)
        {
            var currentFrame = stack.Peek();

            if (currentFrame.ExpressionValue is { } currentFrameExprValue)
            {
                var frameTotalInstructionCount =
                    instructionCount - currentFrame.ProfilingBaseline.BeginInstructionCount;

                var frameParseAndEvalCount = parseAndEvalCount - currentFrame.ProfilingBaseline.BeginParseAndEvalCount;
                var frameStackFrameCount = stackFrameCount - currentFrame.ProfilingBaseline.BeginStackFrameCount;

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

            for (var i = 0; i < frameCount; i++)
            {
                stackTrace[i] = stack.ElementAt(i + 1).Expression;
            }

            return stackTrace;
        }

        buildAndPushStackFrame(
            expressionValue: null,
            rootExpression,
            rootEnvironment,
            replaceCurrentFrame: false);

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
                        if (InvokePrecompiledOrBuildStackFrame(
                            expressionValue: null,
                            expression: cont.Expression,
                            environmentValue: cont.EnvironmentValue,
                            replaceCurrentFrame: false) is { } error)
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
                                areEqual ? PineKernelValues.TrueValue : PineKernelValues.FalseValue);

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
                                areEqual ? PineKernelValues.TrueValue : PineKernelValues.FalseValue);

                            continue;
                        }

                    case StackInstructionKind.Not_Equal_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var areEqual = left == right;

                            currentFrame.PushInstructionResult(
                                areEqual ? PineKernelValues.FalseValue : PineKernelValues.TrueValue);

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
                                areEqual ? PineKernelValues.FalseValue : PineKernelValues.TrueValue);

                            continue;
                        }

                    case StackInstructionKind.Length:
                        {
                            var listValue = currentFrame.PopTopmostFromStack();

                            var lengthValue = KernelFunction.length(listValue);

                            currentFrame.PushInstructionResult(lengthValue);

                            continue;
                        }

                    case StackInstructionKind.Length_Equal_Const:
                        {
                            var topmostValue = currentFrame.PopTopmostFromStack();

                            var lengthValue =
                                topmostValue switch
                                {
                                    PineValue.ListValue listValue =>
                                    listValue.Items.Length,

                                    PineValue.BlobValue blobValue =>
                                    blobValue.Bytes.Length,

                                    _ =>
                                    throw new Exception(
                                        "Unexpected value type for length operation: " + topmostValue.GetType().FullName)
                                };

                            var testedLength =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception(
                                    "Invalid operation form: Missing integer literal value for length comparison");

                            var areEqual = lengthValue == testedLength;

                            currentFrame.PushInstructionResult(
                                areEqual ? PineKernelValues.TrueValue : PineKernelValues.FalseValue);

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
                                    listValue.Items.Length <= indexClamped
                                    ?
                                    PineValue.EmptyList
                                    :
                                    listValue.Items.Span[indexClamped],

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
                                        listValue.Items.Length <= skipCountClamped
                                        ?
                                        PineValue.EmptyList
                                        :
                                        listValue.Items.Span[skipCountClamped],

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

                            var resultValue =
                                KernelFunctionSpecialized.skip(skipCountValue, prevValue);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Skip_Const:
                        {
                            var skipCount =
                                currentInstruction.SkipCount
                                ??
                                throw new Exception("Invalid operation form: Missing skip count");

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue = KernelFunctionSpecialized.skip(skipCount, prevValue);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Take_Binary:
                        {
                            var takeCountValue = currentFrame.PopTopmostFromStack();

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                KernelFunctionSpecialized.take(takeCountValue, prevValue);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Take_Const:
                        {
                            var takeCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var prevValue = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = resultValue =
                                KernelFunctionSpecialized.take(takeCount, prevValue);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Take_Last_Const:
                        {
                            var takeCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing take count");

                            var prevValue = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                KernelFunctionFused.TakeLast(takeCount: takeCount, prevValue);

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

                            for (var i = 0; i < itemsCount; ++i)
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

                            currentFrame.PushInstructionResult(KernelFunctionSpecialized.concat(left, right));

                            continue;
                        }

                    case StackInstructionKind.Prepend_List_Item_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineValue.EmptyList;

                            if (right is PineValue.ListValue rightList)
                            {
                                var elements = new PineValue[rightList.Items.Length + 1];

                                elements[0] = left;

                                rightList.Items.Span.CopyTo(elements.AsSpan(1));

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

                            if (KernelFunction.SignedIntegerFromValueRelaxed(takeCountValue) is { } takeCount)
                            {
                                resultValue =
                                    KernelFunctionFused.SkipAndTake(
                                        takeCount: (int)takeCount,
                                        skipCountValue: skipCountValue,
                                        prevValue);
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

                            var resultValue =
                                KernelFunctionFused.SkipAndTake(
                                    takeCount: takeCount,
                                    skipCountValue: skipCountValue,
                                    prevValue);

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

                            var resultValue = KernelFunctionSpecialized.int_add(left, right);

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
                                resultValue = IntegerEncoding.EncodeSignedInteger(leftInt + rightInt);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Unsigned_Add_Const:
                        {
                            var rightInt =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var leftValue = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.UnsignedIntegerFromValueRelaxed(leftValue) is { } leftInt)
                            {
                                resultValue = IntegerEncoding.EncodeSignedInteger(leftInt + rightInt);
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
                                resultValue = IntegerEncoding.EncodeSignedInteger(leftInt - rightInt);
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
                                resultValue = IntegerEncoding.EncodeSignedInteger(leftInt * rightInt);
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
                                resultValue = IntegerEncoding.EncodeSignedInteger(leftInt * right);
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
                                    PineKernelValues.TrueValue :
                                    PineKernelValues.FalseValue;
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
                                    PineKernelValues.TrueValue :
                                    PineKernelValues.FalseValue;
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Less_Than_Const:
                        {
                            var right = currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(left) is { } leftInt)
                            {
                                resultValue =
                                    leftInt < right ?
                                    PineKernelValues.TrueValue :
                                    PineKernelValues.FalseValue;
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Less_Than_Or_Equal_Const:
                        {
                            var right = currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(left) is { } leftInt)
                            {
                                resultValue =
                                    leftInt <= right ?
                                    PineKernelValues.TrueValue :
                                    PineKernelValues.FalseValue;
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Unsigned_Less_Than_Or_Equal_Const:
                        {
                            var right = currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.UnsignedIntegerFromValueRelaxed(left) is { } leftInt)
                            {
                                resultValue =
                                    leftInt <= right ?
                                    PineKernelValues.TrueValue :
                                    PineKernelValues.FalseValue;
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Greater_Than_Or_Equal_Const:
                        {
                            var right =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(left) is { } leftInt)
                            {
                                resultValue =
                                    leftInt >= right ?
                                    PineKernelValues.TrueValue :
                                    PineKernelValues.FalseValue;
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Int_Unsigned_Greater_Than_Or_Equal_Const:
                        {
                            var right =
                                currentInstruction.IntegerLiteral
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.UnsignedIntegerFromValueRelaxed(left) is { } leftInt)
                            {
                                resultValue =
                                    leftInt >= right ?
                                    PineKernelValues.TrueValue :
                                    PineKernelValues.FalseValue;
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
                                        .Select(expr => s_mutableCacheValueHash.GetHash(ExpressionEncoding.EncodeExpressionAsValue(expr)))
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

                            var followingInstruction =
                                currentFrame.Instructions.Instructions[currentFrame.InstructionPointer + 1];

                            var replaceCurrentFrame =
                                followingInstruction.Kind is StackInstructionKind.Return;

                            {
                                if (InvokePrecompiledOrBuildStackFrame(
                                    expressionValue: expressionValue,
                                    parseOk,
                                    environmentValue,
                                    replaceCurrentFrame: replaceCurrentFrame) is { } error)
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

                            if (conditionValue == PineKernelValues.TrueValue)
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

                            var resultValue =
                                KernelFunctionSpecialized.bit_and(left, right);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_And_Const:
                        {
                            var right = currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                KernelFunctionSpecialized.bit_and(left, right);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Or_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                KernelFunctionSpecialized.bit_or(left, right);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Or_Const:
                        {
                            var right = currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                KernelFunctionSpecialized.bit_or(left, right);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Xor_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                KernelFunctionSpecialized.bit_xor(left, right);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Not:
                        {
                            var value = currentFrame.PopTopmostFromStack();

                            var resultValue = KernelFunction.bit_not(value);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Left_Binary:
                        {
                            var shiftValue = currentFrame.PopTopmostFromStack();
                            var value = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(shiftValue) is { } shiftCount)
                            {
                                resultValue =
                                    KernelFunctionSpecialized.bit_shift_left(shiftCount, value);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Left_Const:
                        {
                            var shiftCount =
                                currentInstruction.ShiftCount
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var value = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                KernelFunctionSpecialized.bit_shift_left(shiftCount, value);

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Right_Binary:
                        {
                            var shiftValue = currentFrame.PopTopmostFromStack();
                            var value = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (KernelFunction.SignedIntegerFromValueRelaxed(shiftValue) is { } shiftCount)
                            {
                                resultValue =
                                    KernelFunctionSpecialized.bit_shift_right(shiftCount, value);
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Bit_Shift_Right_Const:
                        {
                            var shiftCount =
                                currentInstruction.ShiftCount
                                ??
                                throw new Exception("Invalid operation form: Missing literal value");

                            var value = currentFrame.PopTopmostFromStack();

                            var resultValue =
                                KernelFunctionSpecialized.bit_shift_right(shiftCount, value);

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

                    case StackInstructionKind.Logical_And_Binary:
                        {
                            var right = currentFrame.PopTopmostFromStack();
                            var left = currentFrame.PopTopmostFromStack();

                            var resultValue = PineKernelValues.FalseValue;

                            if (left == PineKernelValues.TrueValue && right == PineKernelValues.TrueValue)
                            {
                                resultValue = PineKernelValues.TrueValue;
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Blob_Trim_Leading_Zeros:
                        {
                            var minRemainingCount =
                                currentInstruction.TakeCount
                                ??
                                throw new Exception("Invalid operation form: Missing min remaining count");

                            var blobValue = currentFrame.PopTopmostFromStack();

                            PineValue resultValue = PineValue.EmptyList;

                            if (blobValue is PineValue.BlobValue blob)
                            {
                                var blobBytes = blob.Bytes.Span;

                                if (minRemainingCount <= blobBytes.Length)
                                {
                                    var sliceStartIndex = 0;

                                    while (sliceStartIndex < blobBytes.Length - minRemainingCount)
                                    {
                                        if (blobBytes[sliceStartIndex] is not 0)
                                        {
                                            break;
                                        }

                                        ++sliceStartIndex;
                                    }

                                    if (sliceStartIndex is 0)
                                    {
                                        resultValue = blobValue;
                                    }
                                    else if (sliceStartIndex < blob.Bytes.Length)
                                    {
                                        var trimmedBytes =
                                            blob.Bytes[sliceStartIndex..];

                                        resultValue = PineValue.Blob(trimmedBytes);
                                    }
                                    else
                                    {
                                        resultValue = PineValue.EmptyBlob;
                                    }
                                }
                            }

                            currentFrame.PushInstructionResult(resultValue);

                            continue;
                        }

                    case StackInstructionKind.Starts_With_Const:
                        {
                            var prefixValue =
                                currentInstruction.Literal
                                ??
                                throw new Exception("Invalid operation form: Missing prefix value");

                            var value = currentFrame.PopTopmostFromStack();

                            var resultValue = PineKernelValues.FalseValue;

                            if (prefixValue is PineValue.BlobValue prefixBlob)
                            {
                                if (value is PineValue.BlobValue valueBlob)
                                {
                                    var valueBytes = valueBlob.Bytes.Span;

                                    if (valueBytes.Length >= prefixBlob.Bytes.Length &&
                                        valueBytes[..prefixBlob.Bytes.Length].SequenceEqual(prefixBlob.Bytes.Span))
                                    {
                                        resultValue = PineKernelValues.TrueValue;
                                    }
                                }
                            }

                            if (prefixValue is PineValue.ListValue prefixList)
                            {
                                if (value is PineValue.ListValue valueList)
                                {
                                    if (valueList.Items.Length >= prefixList.Items.Length)
                                    {
                                        var allItemsMatch = true;

                                        for (var i = 0; i < prefixList.Items.Length; i++)
                                        {
                                            if (valueList.Items.Span[i] != prefixList.Items.Span[i])
                                            {
                                                allItemsMatch = false;
                                                break;
                                            }
                                        }

                                        if (allItemsMatch)
                                        {
                                            resultValue = PineKernelValues.TrueValue;
                                        }
                                    }
                                }
                            }

                            currentFrame.PushInstructionResult(resultValue);

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
            s_mutableCacheValueHash.GetHash(expressionValue);

        var exprHashBase16 = Convert.ToHexStringLower(exprHash.Span);

        var envHash =
            s_mutableCacheValueHash.GetHash(errorReport.EnvironmentValue);

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
        var listItems = new PineValue[listExpression.Items.Count];

        for (var i = 0; i < listExpression.Items.Count; i++)
        {
            var item = listExpression.Items[i];

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
            FunctionApplicationMaxEnvSize < list.Items.Length ? list.Items.Length : FunctionApplicationMaxEnvSize;
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
        StringEncoding.StringFromValue(pineValue)
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
                skipListExpr.Items.Count is 2)
            {
                var skipValue =
                    EvaluateExpressionDefaultLessStack(
                        skipListExpr.Items[0],
                        environment,
                        stackPrevValues);

                if (KernelFunction.SignedIntegerFromValueRelaxed(skipValue) is { } skipCount)
                {
                    if (EvaluateExpressionDefaultLessStack(
                        skipListExpr.Items[1],
                        environment,
                        stackPrevValues) is PineValue.ListValue list)
                    {
                        if (list.Items.Length < 1 || list.Items.Length <= skipCount)
                        {
                            return PineValue.EmptyList;
                        }

                        return list.Items.Span[skipCount < 0 ? 0 : (int)skipCount];
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
            KernelFunction.ApplyKernelFunctionGeneric(
                function: application.Function,
                inputValue: inputValue);
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

        if (conditionValue == PineKernelValues.TrueValue)
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
