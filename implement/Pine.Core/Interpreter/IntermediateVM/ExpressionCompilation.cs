using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Represents one environment constraint item used to select a specialized instruction variant at runtime.
/// </summary>
/// <param name="Path">The path inside the environment value to inspect.</param>
/// <param name="Value">The expected value at <paramref name="Path"/>.</param>
public record struct EnvConstraintItem(
    ReadOnlyMemory<int> Path,
    PineValue Value);

/// <summary>
/// Holds the generic and specialized instruction variants compiled for one expression.
/// </summary>
/// <param name="Generic">The generic instructions used when no specialization matches the concrete environment.</param>
/// <param name="Specialized">The specialized variants paired with the environment constraints that select them.</param>
public record ExpressionCompilation(
    StackFrameInstructions Generic,
    IReadOnlyList<(IReadOnlyList<EnvConstraintItem> constraint, StackFrameInstructions instructions)> Specialized)
{
    /// <summary>
    /// Selects the most specific instruction variant whose environment constraints match the supplied environment.
    /// </summary>
    /// <param name="environment">The concrete environment for this evaluation.</param>
    /// <returns>The specialized instructions that match the environment, or <see cref="Generic"/> if none match.</returns>
    public StackFrameInstructions SelectInstructionsForEnvironment(PineValueInProcess environment)
    {
        for (var i = 0; i < Specialized.Count; i++)
        {
            var specialization = Specialized[i];

            var foundMismatch = false;

            for (var specializationIndex = 0; specializationIndex < specialization.constraint.Count; specializationIndex++)
            {
                var constraintItem = specialization.constraint[specializationIndex];

                if (PineValueInProcess.ValueFromPathOrNull(environment, constraintItem.Path.Span) is not { } pathValue)
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

    /// <summary>
    /// Compiles an expression into one generic instruction variant and zero or more environment-specialized variants.
    /// </summary>
    /// <param name="rootExpression">The expression to compile.</param>
    /// <param name="specializations">Environment classes to use for specialized compilation variants.</param>
    /// <param name="parseCache">The parse cache used while compiling nested parse-and-eval expressions.</param>
    /// <param name="disableReduction">Whether reduction should be disabled during compilation.</param>
    /// <param name="enableTailRecursionOptimization">Whether tail-recursion optimization should be enabled.</param>
    /// <param name="skipInlining">Predicate deciding whether a subexpression should skip inlining for a given environment constraint.</param>
    /// <param name="reducedExpressionCache">Optional cache for reduced expressions reused during compilation.</param>
    public static ExpressionCompilation CompileExpression(
        Expression rootExpression,
        IReadOnlyList<PineValueClass> specializations,
        PineVMParseCache parseCache,
        bool disableReduction,
        bool enableTailRecursionOptimization,
        Func<Expression, PineValueClass?, bool> skipInlining,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null,
        int pathMaxLowExclusive = DefaultPathMaxLowExclusive,
        int pathMaxHighInclusive = DefaultPathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation = false)
    {
        var genericParameters =
            StaticFunctionInterface.FromExpression(rootExpression);

        var generic =
            new StackFrameInstructions(
                genericParameters,
                InstructionsFromExpressionTransitive(
                    rootExpression,
                    envConstraintId: null,
                    parametersAsLocals: genericParameters,
                    parseCache: parseCache,
                    disableReduction: disableReduction,
                    skipInlining: skipInlining,
                    enableTailRecursionOptimization: enableTailRecursionOptimization,
                    reducedExpressionCache: reducedExpressionCache,
                    pathMaxLowExclusive: pathMaxLowExclusive,
                    pathMaxHighInclusive: pathMaxHighInclusive,
                    disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation),
                TrackEnvConstraint: null);

        var specialized =
            specializations
            // Order to prefer more specific constraints when selecting at runtime.
            .OrderDescending(PineValueClassSpecificityComparer.Instance)
            .Select(
                specialization =>
                ((IReadOnlyList<EnvConstraintItem>)
                [
                ..specialization.ParsedItems
                .Select(envItem => new EnvConstraintItem(envItem.Key.ToArray(), envItem.Value))
                ],
                new StackFrameInstructions(
                    genericParameters,
                    InstructionsFromExpressionTransitive(
                        rootExpression,
                        envConstraintId: specialization,
                        parametersAsLocals: genericParameters,
                        parseCache: parseCache,
                        disableReduction: disableReduction,
                        enableTailRecursionOptimization: enableTailRecursionOptimization,
                        skipInlining: skipInlining,
                        reducedExpressionCache: reducedExpressionCache,
                        pathMaxLowExclusive: pathMaxLowExclusive,
                        pathMaxHighInclusive: pathMaxHighInclusive,
                        disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation),
                    TrackEnvConstraint: specialization)))
            .ToImmutableArray();

        return
            new ExpressionCompilation(
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

    /// <summary>
    /// Compiles an expression into intermediate VM instructions, applying transitive inlining and reduction decisions as configured.
    /// </summary>
    /// <param name="rootExpression">The expression to compile.</param>
    /// <param name="envConstraintId">Optional environment constraint to specialize for.</param>
    /// <param name="parametersAsLocals">The static parameter layout that should be exposed as locals.</param>
    /// <param name="parseCache">The parse cache used while compiling nested parse-and-eval expressions.</param>
    /// <param name="disableReduction">Whether reduction should be disabled during compilation.</param>
    /// <param name="enableTailRecursionOptimization">Whether tail-recursion optimization should be enabled.</param>
    /// <param name="skipInlining">Predicate deciding whether a subexpression should skip inlining for a given environment constraint.</param>
    /// <param name="reducedExpressionCache">Optional cache for reduced expressions reused during compilation.</param>
    public static IReadOnlyList<StackInstruction> InstructionsFromExpressionTransitive(
        Expression rootExpression,
        PineValueClass? envConstraintId,
        StaticFunctionInterface parametersAsLocals,
        PineVMParseCache parseCache,
        bool disableReduction,
        bool enableTailRecursionOptimization,
        Func<Expression, PineValueClass?, bool> skipInlining,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null,
        int pathMaxLowExclusive = DefaultPathMaxLowExclusive,
        int pathMaxHighInclusive = DefaultPathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation = false)
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
                skipInlining: e => skipInlining(e, null),
                reducedExpressionCache: reducedExpressionCache,
                pathMaxLowExclusive: pathMaxLowExclusive,
                pathMaxHighInclusive: pathMaxHighInclusive,
                disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation);

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
                skipInlining: skipInlining,
                reducedExpressionCache: reducedExpressionCache,
                pathMaxLowExclusive: pathMaxLowExclusive,
                pathMaxHighInclusive: pathMaxHighInclusive,
                disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation);

        var allInstructionsBeforeReturn =
            InstructionsFromExpression(
                rootExpression: reducedExpression,
                rootExprAlternativeForms: [rootExpression],
                envClass: enableTailRecursionOptimization ? envConstraintId : null,
                parametersAsLocals: parametersAsLocals,
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
            [
            .. allInstructionsBeforeReturn,
            StackInstruction.Return
            ];

        return allInstructions;
    }


    /*
     * Path-max gating thresholds.
     *
     * The gate rejects an inlining when the body's per-branch path-max metric
     * (counting only PEs that future inlining would expand) is in the open-low /
     * closed-high window (pathMaxLowExclusive, pathMaxHighInclusive].
     * Setting LOW >= HIGH effectively disables the gate.
     *
     * Thresholds are forwarded as parameters from the entry points
     * (<see cref="CompileExpression"/>, <see cref="InstructionsFromExpressionTransitive"/>,
     * <see cref="InlineStaticInvocationsAndReduceRecursive"/>, and
     * <see cref="ReduceExpressionAndInlineRecursive(Expression, ImmutableHashSet{Expression}, PineValueClass?, int, int, PineVMParseCache, bool, Func{Expression, PineValueClass?, bool}, IDictionary{Expression, Expression}?, int, int)"/>),
     * defaulting to the values committed in the preceding tuning step (see
     * the latest 5 in
     * explore/internal-analysis/2026-04-27-expression-char-literal-consolidation-regression.md).
     *
     * The default lower bound was raised from 5 to 6 after a parameter sweep
     * showed strict improvements on several Expression_* tests (e.g.
     * Expression_list_one_item_max_rounds_2 5099→3700, Expression_char_literal
     * 1822→1576, Expression_int_literal 1921→1675, Expression_string_literal
     * 3083→2835, Expression_empty_list 2551→2225) with no SkipWhile regression.
     * The trade-off is a ~3 % cost on the largest list-shape tests
     * (Expression_flat_list_forty_items, Expression_nested_list_four_by_ten,
     * Expression_application_with_various_argument_kinds).
     */

    /// <summary>
    /// Default lower bound (exclusive) for the path-max gating window.
    /// </summary>
    public const int DefaultPathMaxLowExclusive = 6;

    /// <summary>
    /// Default upper bound (inclusive) for the path-max gating window.
    /// </summary>
    public const int DefaultPathMaxHighInclusive = 30;

    public static Expression InlineStaticInvocationsAndReduceRecursive(
        Expression currentExpression,
        ImmutableStack<Expression> inlinedParents,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, bool> skipInlining,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null,
        int pathMaxLowExclusive = DefaultPathMaxLowExclusive,
        int pathMaxHighInclusive = DefaultPathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation = false)
    {
        var expressionReduced =
            ReducePineExpression.ReduceExpressionBottomUp(
                currentExpression,
                new ReductionConfig(DisableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation, DisableInliningParseAndEval: false),
                parseCache,
                reducedExpressionCache: reducedExpressionCache);

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
                    new ReductionConfig(DisableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation, DisableInliningParseAndEval: false),
                    parseCache,
                    reducedExpressionCache: reducedExpressionCache);

            /*
             * Per-branch (max-of-paths) cost gate: a Conditional contributes the cost of
             * its condition plus the max of its two branches, not the sum. The metric
             * counts only PEs that future inlining would actually expand, so recursive
             * self-calls (which the inliner's recursion check would refuse) and other
             * statically-unresolvable PEs are excluded. This focuses the gate on
             * inlining-driven code growth rather than fixed runtime cost. Static IR size
             * is gated separately by the entry-level maxSubexpressionCount cap above.
             */
            bool IsExpansionCandidateForFirstInliner(Expression.ParseAndEval pe)
            {
                if (pe.Encoded.ReferencesEnvironment)
                {
                    return false;
                }

                if (ReducePineExpression.TryEvaluateExpressionIndependent(
                    pe.Encoded, parseCache).IsOkOrNull() is not { } peValue)
                {
                    return false;
                }

                if (parseCache.ParseExpression(peValue).IsOkOrNull() is not { } peParsed)
                {
                    return false;
                }

                if (skipInlining(peParsed))
                {
                    return false;
                }

                /*
                 * Recursive self-calls and calls to ancestors-being-inlined are NOT
                 * excluded from the path-max metric. While the inliner's own recursion
                 * check refuses to expand them further, leaving them as residual
                 * <see cref="Expression.ParseAndEval"/> nodes still incurs a dynamic
                 * invocation per traversal of the worst path. Rejecting bodies whose
                 * worst path contains too many such residual calls is what keeps tests
                 * like <c>Int_div_1_000_000_by_257</c> within their invocation-count
                 * budget: <c>idivHelper</c> has two self-recursive call sites along its
                 * worst path, and admitting a fully-inlined copy of its body into
                 * <c>idiv</c> would multiply the dynamic invocation count at every
                 * call site reachable from the outer program.
                 */

                return true;
            }

            /*
             * Per-branch (max-of-paths) cost gate, with a "moderate-path-max" rejection
             * window. The metric counts only PEs that future inlining would actually
             * expand (recursive self-calls and other statically-unresolvable PEs are
             * excluded). A Conditional contributes its condition-path cost plus the max
             * of its two branch-path costs, not the sum.
             *
             * Empirical observation (from instrumented runs of the affected tests):
             *   - "Bad" inlinings (cause runaway expansion later) tend to have moderate
             *     path-max (6-9) on bodies that get inlined into many call sites; each
             *     copy carries its PE-along-path forward, ballooning total expansion
             *     cost.
             *   - "Good" inlinings of helpful tail-recursive helpers often have very
             *     high path-max (e.g. 38) coming from a large curried recursive call
             *     chain that won't actually expand further (the recursion is excluded
             *     from the metric, but unconsolidated chains can still inflate the
             *     count). These are typically inlined only once or twice, so the
             *     per-callsite cost is recovered.
             *
             * So the gate uses an upper bound on the rejection window: only path-max
             * values in the moderate band [6 .. 30] are rejected. Values above 30 are
             * admitted on the assumption that they are deep recursive helpers rather
             * than runaway-expansion candidates. Static IR size is gated separately by
             * the entry-level maxSubexpressionCount cap above.
             */
            /*
             * Per-branch (max-of-paths) cost gate, with a "moderate-path-max" rejection
             * window applied only to the substituted-and-reduced body BEFORE the
             * recursive inline-and-reduce step. The metric counts only PEs that future
             * inlining would actually expand (recursive self-calls and other
             * statically-unresolvable PEs are excluded). A Conditional contributes its
             * condition-path cost plus the max of its two branch-path costs, not the
             * sum.
             *
             * Empirical observation (from instrumented runs of the affected tests):
             *   - "Bad" inlinings (cause runaway expansion later) tend to have moderate
             *     path-max (6-9) on small bodies that get inlined into many call sites;
             *     each copy carries its PE-along-path forward, ballooning total
             *     expansion cost.
             *   - "Good" inlinings of helpful tail-recursive helpers often have very
             *     high path-max (e.g. 38) coming from a large curried recursive call
             *     chain that won't actually expand further (the recursion is excluded
             *     from the metric, but unconsolidated chains can still inflate the
             *     count). These are typically inlined only once or twice, so the
             *     per-callsite cost is recovered.
             *
             * The gate uses an upper bound on the rejection window: only path-max
             * values in the moderate band [6 .. 30] are rejected. Values above 30 are
             * admitted on the assumption that they are deep recursive helpers rather
             * than runaway-expansion candidates.
             *
             * Only the pre-recurse check is enforced. The post-recurse check fires for
             * SkipWhile-like recursive helpers whose body's path-max grows transiently
             * during recursive inlining (e.g. to 13) but ultimately settles into a
             * tail-recursive shape that is strictly better than not inlining at all.
             * The pre-recurse gate is sufficient to catch the runaway-expansion shapes
             * we want to reject; further code-size growth at the recursive step is
             * already bounded by the static `SubexpressionCount` cap.
             */
            {
                var pmPre = ComputeParseAndEvalPathMax(inlinedExprReduced, IsExpansionCandidateForFirstInliner);

                if (pathMaxLowExclusive < pmPre && pmPre <= pathMaxHighInclusive)
                {
                    return null;
                }
            }

            var inlinedFinal =
                InlineStaticInvocationsAndReduceRecursive(
                    currentExpression: inlinedExprReduced,
                    inlinedParents: inlinedParents.Push(parseOk),
                    maxDepth: maxDepth - 1,
                    maxSubexpressionCount: maxSubexpressionCount,
                    parseCache: parseCache,
                    skipInlining: skipInlining,
                    disableRecurseAfterInline: disableRecurseAfterInline,
                    reducedExpressionCache: reducedExpressionCache,
                    pathMaxLowExclusive: pathMaxLowExclusive,
                    pathMaxHighInclusive: pathMaxHighInclusive,
                    disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation);

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

                            return
                                Expression.ConditionalInstance(
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
            ReducePineExpression.ReduceExpressionBottomUp(
                expressionInlined,
                new ReductionConfig(DisableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation, DisableInliningParseAndEval: false),
                parseCache,
                reducedExpressionCache: reducedExpressionCache);

        return expressionInlinedReduced;
    }

    /// <summary>
    /// Computes, for an expression, the per-path maximum number of
    /// <see cref="Expression.ParseAndEval"/> nodes that <em>would be expanded by future
    /// inlining</em> along any single dynamic evaluation path. A
    /// <see cref="Expression.Conditional"/> contributes
    /// <c>condition_path + max(true_path, false_path)</c> rather than the
    /// sum of all branches, reflecting that exactly one of <c>TrueBranch</c>/<c>FalseBranch</c>
    /// executes per evaluation. A <see cref="Expression.ParseAndEval"/> contributes 1
    /// only when the supplied <paramref name="isExpansionCandidate"/> predicate returns
    /// true for it; otherwise it contributes 0.
    /// <para>
    /// This is the cost model used by the inliner's per-step cost gate. Including only
    /// expansion-candidate PEs aligns the metric with the question the gate is trying to
    /// answer ("will inlining this body cause further inlining to expand the worst-case
    /// path beyond a safe limit?"), and avoids penalising bodies whose deep PE chains are
    /// fixed runtime cost the inliner cannot influence — most importantly, recursive
    /// self-calls and other environment-dispatched calls. Static IR size is gated
    /// separately by the <c>SubexpressionCount</c> threshold.
    /// </para>
    /// </summary>
    internal static int ComputeParseAndEvalPathMax(
        Expression expression,
        Func<Expression.ParseAndEval, bool> isExpansionCandidate)
    {
        switch (expression)
        {
            case Expression.Conditional conditional:
                {
                    var condP = ComputeParseAndEvalPathMax(conditional.Condition, isExpansionCandidate);
                    var trueP = ComputeParseAndEvalPathMax(conditional.TrueBranch, isExpansionCandidate);
                    var falseP = ComputeParseAndEvalPathMax(conditional.FalseBranch, isExpansionCandidate);

                    return condP + Math.Max(trueP, falseP);
                }

            case Expression.ParseAndEval parseAndEval:
                {
                    var encP = ComputeParseAndEvalPathMax(parseAndEval.Encoded, isExpansionCandidate);
                    var envP = ComputeParseAndEvalPathMax(parseAndEval.Environment, isExpansionCandidate);

                    var selfContribution =
                        isExpansionCandidate(parseAndEval) ? 1 : 0;

                    return selfContribution + encP + envP;
                }

            case Expression.List list:
                {
                    var sumP = 0;

                    for (var i = 0; i < list.Items.Count; i++)
                    {
                        sumP += ComputeParseAndEvalPathMax(list.Items[i], isExpansionCandidate);
                    }

                    return sumP;
                }

            case Expression.KernelApplication kernelApp:
                return ComputeParseAndEvalPathMax(kernelApp.Input, isExpansionCandidate);

            case Expression.StringTag stringTag:
                return ComputeParseAndEvalPathMax(stringTag.Tagged, isExpansionCandidate);

            default:
                return 0;
        }
    }

    public static Expression ReduceExpressionAndInlineRecursive(
        Expression rootExpression,
        ImmutableHashSet<Expression> rootExprAlternativeForms,
        PineValueClass? envConstraintId,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, PineValueClass?, bool> skipInlining,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null,
        int pathMaxLowExclusive = DefaultPathMaxLowExclusive,
        int pathMaxHighInclusive = DefaultPathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation = false) =>
        ReduceExpressionAndInlineRecursive(
            currentExpression: rootExpression,
            inlinedParents: [],
            envConstraintId: envConstraintId,
            rootExprForms: rootExprAlternativeForms.Add(rootExpression),
            maxDepth: maxDepth,
            maxSubexpressionCount: maxSubexpressionCount,
            parseCache: parseCache,
            disableRecurseAfterInline: disableRecurseAfterInline,
            skipInlining: skipInlining,
            reducedExpressionCache: reducedExpressionCache,
            pathMaxLowExclusive: pathMaxLowExclusive,
            pathMaxHighInclusive: pathMaxHighInclusive,
            disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation);

    public static Expression ReduceExpressionAndInlineRecursive(
        Expression currentExpression,
        ImmutableStack<Expression> inlinedParents,
        PineValueClass? envConstraintId,
        ImmutableHashSet<Expression> rootExprForms,
        int maxDepth,
        int maxSubexpressionCount,
        PineVMParseCache parseCache,
        bool disableRecurseAfterInline,
        Func<Expression, PineValueClass?, bool> skipInlining,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null,
        int pathMaxLowExclusive = DefaultPathMaxLowExclusive,
        int pathMaxHighInclusive = DefaultPathMaxHighInclusive,
        bool disableGenericApplicationChainConsolidation = false)
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
            ReducePineExpression.ReduceExpressionBottomUp(
                expressionSubstituted,
                new ReductionConfig(DisableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation, DisableInliningParseAndEval: false),
                parseCache,
                reducedExpressionCache: reducedExpressionCache);

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
                    ReducePineExpression.ReduceExpressionBottomUp(
                        inlinedExprSubstituted,
                        new ReductionConfig(DisableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation, DisableInliningParseAndEval: false),
                        parseCache,
                        reducedExpressionCache: reducedExpressionCache);

                bool IsExpansionCandidateForSecondInliner(Expression.ParseAndEval pe)
                {
                    if (pe.Encoded.ReferencesEnvironment)
                    {
                        return false;
                    }

                    if (ReducePineExpression.TryEvaluateExpressionIndependent(
                        pe.Encoded, parseCache).IsOkOrNull() is not { } peValue)
                    {
                        return false;
                    }

                    if (parseCache.ParseExpression(peValue).IsOkOrNull() is not { } peParsed)
                    {
                        return false;
                    }

                    if (skipInlining(peParsed, envConstraintId))
                    {
                        return false;
                    }

                    /*
                     * Recursive self-calls and calls to ancestors-being-inlined are NOT
                     * excluded from the path-max metric. See the corresponding comment on
                     * <see cref="IsExpansionCandidateForFirstInliner"/> for the rationale
                     * (residual <see cref="Expression.ParseAndEval"/> nodes still cost a
                     * dynamic invocation per traversal, and rejecting bodies with too many
                     * of them along the worst path keeps tests like
                     * <c>Int_div_1_000_000_by_257</c> within their invocation-count budget).
                     */

                    return true;
                }

                {
                    if (500 < inlinedExprReduced.SubexpressionCount)
                    {
                        return null;
                    }

                    var pathInvocations =
                        ComputeParseAndEvalPathMax(
                            inlinedExprReduced,
                            IsExpansionCandidateForSecondInliner);

                    if (pathMaxLowExclusive < pathInvocations && pathInvocations <= pathMaxHighInclusive)
                    {
                        return null;
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
                        disableRecurseAfterInline: disableRecurseAfterInline,
                        reducedExpressionCache: reducedExpressionCache,
                        pathMaxLowExclusive: pathMaxLowExclusive,
                        pathMaxHighInclusive: pathMaxHighInclusive,
                        disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation);

                {
                    if (500 < inlinedFinal.SubexpressionCount)
                    {
                        return null;
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

                            return
                                Expression.ConditionalInstance(
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
            ReducePineExpression.ReduceExpressionBottomUp(
                expressionInlined,
                new ReductionConfig(DisableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation, DisableInliningParseAndEval: false),
                parseCache,
                reducedExpressionCache: reducedExpressionCache);

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
                    if (CodeAnalysis.CodeAnalysis.TryParseAsLiteral(descendant) is { } literal)
                    {
                        return Expression.LiteralInstance(literal);
                    }

                    if (CodeAnalysis.CodeAnalysis.TryParseExprAsPathInEnv(descendant) is { } pathInEnv)
                    {
                        if (envConstraintId.TryGetValue(pathInEnv) is { } value)
                        {
                            return Expression.LiteralInstance(value);
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
        StaticFunctionInterface parametersAsLocals,
        PineVMParseCache parseCache)
    {
        return
            PineIRCompiler.CompileExpression(
                rootExpression,
                rootExprAlternativeForms: rootExprAlternativeForms,
                envClass,
                parametersAsLocals: parametersAsLocals,
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
}
