using Pine.Core.CodeAnalysis;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.InternalExpression;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Expression = Pine.Core.Runtime.InternalExpression.Expression;

namespace Pine.Core.Runtime.IntermediateVM;

internal static class InternalExpressionCompilation
{
    private static bool DuplicatesArgumentEvaluations(
        Expression.Eval call,
        Expression body,
        Expression inlined)
    {
        if (call.Environment.EvalCount is 0 ||
            inlined.EvalCount <= body.EvalCount + call.Environment.EvalCount)
        {
            return false;
        }

        return
            ComputeEvalPathMax(inlined, _ => true) >
            (long)ComputeEvalPathMax(body, _ => true) + ComputeEvalPathMax(call.Environment, _ => true);
    }

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
        bool disableGenericApplicationChainConsolidation = false,
        bool enableDirectInvocation = true,
        Func<PineValue, bool>? skipDirectInvocation = null)
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
                    disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation,
                    enableDirectInvocation: enableDirectInvocation,
                    skipDirectInvocation: skipDirectInvocation),
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
                        disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation,
                        enableDirectInvocation: enableDirectInvocation,
                        skipDirectInvocation: skipDirectInvocation),
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
    /// <param name="pathMaxLowExclusive">Exclusive lower bound for path lengths compiled to specialized instructions.</param>
    /// <param name="pathMaxHighInclusive">Inclusive upper bound for path lengths compiled to specialized instructions.</param>
    /// <param name="disableGenericApplicationChainConsolidation">Whether generic application-chain consolidation should be disabled.</param>
    /// <param name="enableDirectInvocation">Whether direct invocation should be enabled.</param>
    /// <param name="skipDirectInvocation">Optional predicate identifying values that should skip direct invocation.</param>
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
        bool disableGenericApplicationChainConsolidation = false,
        bool enableDirectInvocation = true,
        Func<PineValue, bool>? skipDirectInvocation = null)
    {
        var inlinedStaticInvocations =
            disableReduction
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
            envConstraintId is null
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

        var graph =
            ControlFlowGraphFromExpression(
                rootExpression: reducedExpression,
                rootExprAlternativeForms: [rootExpression],
                envClass: enableTailRecursionOptimization ? envConstraintId : null,
                parametersAsLocals: parametersAsLocals,
                parseCache,
                enableTailRecursionOptimization,
                enableDirectInvocation,
                skipDirectInvocation);

        var optimizedGraph =
            enableTailRecursionOptimization
            ?
            graph
            :
            InlineStaticInvocations(
                graph,
                parseCache,
                parametersAsLocals.ParamsPaths.Count,
                skipInlining,
                envConstraintId,
                enableDirectInvocation,
                skipDirectInvocation,
                activeExpressions: [rootExpression, reducedExpression],
                remainingDepth: 8);

        return
            optimizedGraph
            .OptimizeForExecution(parametersAsLocals.ParamsPaths.Count)
            .LowerToStackInstructions();
    }

    private static PineControlFlowGraph InlineStaticInvocations(
        PineControlFlowGraph graph,
        PineVMParseCache parseCache,
        int parameterCount,
        Func<Expression, PineValueClass?, bool> skipInlining,
        PineValueClass? envConstraint,
        bool enableDirectInvocation,
        Func<PineValue, bool>? skipDirectInvocation,
        ImmutableHashSet<Expression> activeExpressions,
        int remainingDepth)
    {
        const int maxCalleeInstructions = 256;
        const int maxFrameInstructions = 2_048;

        if (remainingDepth is 0)
        {
            return graph;
        }

        for (var blockIndex = 0; blockIndex < graph.Blocks.Length; blockIndex++)
        {
            if (graph.Blocks[blockIndex].Terminator is not PineControlFlowTerminator.Invoke invoke)
            {
                continue;
            }

            if (invoke.Instruction is not
                { Kind: StackInstructionKind.Invoke_StackFrame_Const, OptimizedInvocation: { } direct } ||
                ExpressionTemplatePlan.TryParseTemplate(direct.ExpressionEncoded, parseCache) is not null)
            {
                continue;
            }

            var target = direct.InternalExpression;
            var parameters = direct.InvocationInterface;

            if (activeExpressions.Contains(target) ||
                skipInlining(target, envConstraint) ||
                skipInlining(target, null))
            {
                continue;
            }

            var callBlock = graph.Blocks[blockIndex];

            var knownArguments =
                callBlock.Operations
                .Where(
                    operation => operation.Instruction is
                    { Kind: StackInstructionKind.Push_Literal, Literal: not null })
                .Select(operation => (operation.Results, operation.Instruction.Literal))
                .ToDictionary(item => item.Results[0], item => item.Literal!.Evaluate());

            var knownParameterValues =
                invoke.Inputs
                .Select(
                    (input, index) =>
                    (Path: parameters.ParamsPaths[index],
                    Value: knownArguments.TryGetValue(input, out var value) ? value : null))
                .Where(item => item.Value is not null && item.Value.Equals(direct.ExpressionEncoded))
                .Select(item => new KeyValuePair<IReadOnlyList<int>, PineValue>(item.Path, item.Value!))
                .ToArray();

            var calleeEnvironmentClass =
                knownParameterValues.Length is 0 ? null : PineValueClass.Create(knownParameterValues);

            var calleeGraph =
                ControlFlowGraphFromExpression(
                    target,
                    rootExprAlternativeForms: [],
                    envClass: calleeEnvironmentClass,
                    parametersAsLocals: parameters,
                    parseCache,
                    enableTailRecursionOptimization: true,
                    enableDirectInvocation,
                    skipDirectInvocation);

            var callee =
                InlineStaticInvocations(
                    calleeGraph,
                    parseCache,
                    parameters.ParamsPaths.Count,
                    skipInlining,
                    envConstraint: null,
                    enableDirectInvocation,
                    skipDirectInvocation,
                    activeExpressions.Add(target),
                    remainingDepth - 1)
                .ForwardConstantBooleanBranches();

            var calleeInstructionCount = callee.LowerToStackInstructions().Length;

            if (calleeInstructionCount <= 2 ||
                calleeInstructionCount > maxCalleeInstructions ||
                graph.LowerToStackInstructions().Length + calleeInstructionCount >
                maxFrameInstructions)
            {
                continue;
            }

            graph =
                graph.InlineInvocation(
                    graph.Blocks[blockIndex].Id,
                    callee,
                    parameterCount,
                    parameters.ParamsPaths.Count);

            // Blocks inserted at this site have already been processed transitively.
            blockIndex += callee.Blocks.Length;
        }

        return graph;
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

    /// <summary>
    /// Reduces an expression and recursively inlines static parse-and-eval calls until the configured depth and size limits stop further expansion.
    /// </summary>
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
            ExpressionReduction.ReduceExpressionBottomUp(
                currentExpression,
                ReductionConfig.Default
                with
                {
                    DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                },
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

        Expression? TryInlineEval(
            Expression.Eval evalExpr)
        {
            if (evalExpr.Encoded.ReferencesEnvironment)
            {
                return null;
            }

            if (ExpressionReduction.TryEvalIndependent(
                evalExpr.Encoded,
                parseCache).Value is not { } exprValue)
            {
                return null;
            }

            if (parseCache.ParseInternalExpressionWithoutResultAllocation(exprValue).Expression is not { } parseOk)
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
                EnvironmentSubstitution.SubstituteEnvironmentNode(
                    parseOk,
                    evalExpr.Environment);

            if (disableRecurseAfterInline)
            {
                return inlinedExpr;
            }

            var inlinedExprReduced =
                ExpressionReduction.ReduceExpressionBottomUp(
                    inlinedExpr,
                    ReductionConfig.Default
                    with
                    {
                        DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                    },
                    parseCache,
                    reducedExpressionCache: reducedExpressionCache);

            if (DuplicatesArgumentEvaluations(evalExpr, parseOk, inlinedExprReduced))
                return null;

            /*
             * Per-branch (max-of-paths) cost gate: a Conditional contributes the cost of
             * its condition plus the max of its two branches, not the sum. The metric
             * counts only PEs that future inlining would actually expand, so recursive
             * self-calls (which the inliner's recursion check would refuse) and other
             * statically-unresolvable PEs are excluded. This focuses the gate on
             * inlining-driven code growth rather than fixed runtime cost. Static IR size
             * is gated separately by the entry-level maxSubexpressionCount cap above.
             */
            bool IsExpansionCandidateForFirstInliner(Expression.Eval pe)
            {
                if (pe.Encoded.ReferencesEnvironment)
                {
                    return false;
                }

                if (ExpressionReduction.TryEvalIndependent(
                    pe.Encoded,
                    parseCache).Value is not { } peValue)
                {
                    return false;
                }

                if (parseCache.ParseInternalExpressionWithoutResultAllocation(peValue).Expression is not { } peParsed)
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
                 * <see cref="Expression.Eval"/> nodes still incurs a dynamic
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
                var pmPre = ComputeEvalPathMax(inlinedExprReduced, IsExpansionCandidateForFirstInliner);

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

        var expressionInlined =
            InlineEvalRecursive(
                expressionReduced,
                conditionCount: 0,
                (eval, _) => TryInlineEval(eval));

        var expressionInlinedReduced =
            ExpressionReduction.ReduceExpressionBottomUp(
                expressionInlined,
                ReductionConfig.Default
                with
                {
                    DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                },
                parseCache,
                reducedExpressionCache: reducedExpressionCache);

        return expressionInlinedReduced;
    }

    internal static Expression InlineEvalRecursive(
        Expression expression,
        int conditionCount,
        Func<Expression.Eval, int, Expression?> tryInlineEval)
    {
        if (expression.EvalCount is 0)
            return expression;

        return InlineEvalRecursiveWithCache(expression, conditionCount, tryInlineEval, cache: []);
    }

    private static Expression InlineEvalRecursiveWithCache(
        Expression expression,
        int conditionCount,
        Func<Expression.Eval, int, Expression?> tryInlineEval,
        Dictionary<(Expression expression, int conditionCount), Expression> cache)
    {
        if (expression.EvalCount is 0)
            return expression;

        var cacheKey = (expression, conditionCount);

        if (cache.TryGetValue(cacheKey, out var cached))
            return cached;

        Expression inlined;

        if (expression is Expression.Eval evalExpr &&
            tryInlineEval(evalExpr, conditionCount) is { } inlinedEval)
        {
            inlined = inlinedEval;
        }
        else
        {
            inlined =
                expression switch
                {
                    Expression.List list =>
                    InlineList(list),

                    Expression.Eval eval =>
                    InlineEvalChildren(eval),

                    Expression.Builtin builtin =>
                    InlineBuiltin(builtin),

                    Expression.Conditional conditional =>
                    InlineConditional(conditional),

                    Expression.Label label =>
                    InlineLabel(label),

                    Expression.Literal or Expression.Environment =>
                    expression,

                    Expression.BuiltinBinary or Expression.Length_Equal_Binary or Expression.HeadSkip =>
                    ExpressionFusion.Rebuild(
                        expression,
                        child => InlineEvalRecursiveWithCache(child, conditionCount, tryInlineEval, cache)),

                    _ =>
                    throw new NotImplementedException(
                        "Expression type not implemented: " + expression.GetType().FullName)
                };
        }

        cache[cacheKey] = inlined;

        return inlined;

        Expression InlineList(Expression.List list)
        {
            Expression[]? inlinedItems = null;

            for (var i = 0; i < list.Items.Length; ++i)
            {
                var item = list.Items[i];

                var inlinedItem =
                    InlineEvalRecursiveWithCache(
                        item,
                        conditionCount,
                        tryInlineEval,
                        cache);

                if (inlinedItems is null)
                {
                    if (inlinedItem == item)
                        continue;

                    inlinedItems = new Expression[list.Items.Length];

                    for (var copiedIndex = 0; copiedIndex < i; ++copiedIndex)
                        inlinedItems[copiedIndex] = list.Items[copiedIndex];
                }

                inlinedItems[i] = inlinedItem;
            }

            return
                inlinedItems is null
                ?
                list
                :
                new Expression.List(
                    System.Runtime.InteropServices.ImmutableCollectionsMarshal.AsImmutableArray(inlinedItems));
        }

        Expression InlineEvalChildren(Expression.Eval eval)
        {
            var encodedInlined =
                InlineEvalRecursiveWithCache(
                    eval.Encoded,
                    conditionCount,
                    tryInlineEval,
                    cache);

            var environmentInlined =
                InlineEvalRecursiveWithCache(
                    eval.Environment,
                    conditionCount,
                    tryInlineEval,
                    cache);

            if (encodedInlined == eval.Encoded &&
                environmentInlined == eval.Environment)
            {
                return eval;
            }

            return new Expression.Eval(encodedInlined, environmentInlined);
        }

        Expression InlineBuiltin(Expression.Builtin builtin)
        {
            var inputInlined =
                InlineEvalRecursiveWithCache(
                    builtin.Input,
                    conditionCount,
                    tryInlineEval,
                    cache);

            if (inputInlined == builtin.Input)
                return builtin;

            return Expression.BuiltinInst(builtin.Function, inputInlined);
        }

        Expression InlineConditional(Expression.Conditional conditional)
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

            var conditionInlined =
                InlineEvalRecursiveWithCache(
                    conditional.Condition,
                    conditionCount,
                    tryInlineEval,
                    cache);

            var falseBranchInlined =
                InlineEvalRecursiveWithCache(
                    conditional.FalseBranch,
                    conditionCount: conditionCount + 1,
                    tryInlineEval,
                    cache);

            var trueBranchInlined =
                InlineEvalRecursiveWithCache(
                    conditional.TrueBranch,
                    conditionCount: conditionCount + 1,
                    tryInlineEval,
                    cache);

            if (conditionInlined == conditional.Condition &&
                falseBranchInlined == conditional.FalseBranch &&
                trueBranchInlined == conditional.TrueBranch)
            {
                return conditional;
            }

            return
                Expression.ConditionalInst(
                    condition: conditionInlined,
                    falseBranch: falseBranchInlined,
                    trueBranch: trueBranchInlined);
        }

        Expression InlineLabel(Expression.Label label)
        {
            var taggedInlined =
                InlineEvalRecursiveWithCache(
                    label.Tagged,
                    conditionCount,
                    tryInlineEval,
                    cache);

            if (taggedInlined == label.Tagged)
                return label;

            return new Expression.Label(label.Value, taggedInlined);
        }
    }

    /// <summary>
    /// Computes, for an expression, the per-path maximum number of
    /// <see cref="Expression.Eval"/> nodes that <em>would be expanded by future
    /// inlining</em> along any single dynamic evaluation path. A
    /// <see cref="Expression.Conditional"/> contributes
    /// <c>condition_path + max(true_path, false_path)</c> rather than the
    /// sum of all branches, reflecting that exactly one of <c>TrueBranch</c>/<c>FalseBranch</c>
    /// executes per evaluation. A <see cref="Expression.Eval"/> contributes 1
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
    /// <para>Shared subexpressions are memoized while each executed edge still contributes its cost.
    /// Saturation prevents integer overflow from turning an expensive path into an eligible negative cost.</para>
    /// </summary>
    internal static int ComputeEvalPathMax(
        Expression expression,
        Func<Expression.Eval, bool> isExpansionCandidate)
    {
        var costs = new Dictionary<Expression, int>(ReferenceEqualityComparer.Instance);
        var pending = new Stack<(Expression expression, bool childrenReady)>();
        pending.Push((expression, false));

        while (pending.TryPop(out var next))
        {
            if (costs.ContainsKey(next.expression))
                continue;

            if (!next.childrenReady)
            {
                pending.Push((next.expression, true));

                foreach (var child in Children(next.expression))
                {
                    if (!costs.ContainsKey(child))
                        pending.Push((child, false));
                }

                continue;
            }

            costs.Add(
                next.expression,
                next.expression switch
                {
                    Expression.Conditional conditional =>
                    Add(
                        costs[conditional.Condition],
                        Math.Max(costs[conditional.TrueBranch], costs[conditional.FalseBranch])),

                    Expression.Eval eval =>
                    Add(isExpansionCandidate(eval) ? 1 : 0, Add(costs[eval.Encoded], costs[eval.Environment])),

                    Expression.List list => list.Items.Aggregate(0, (sum, item) => Add(sum, costs[item])),
                    Expression.Builtin builtin => costs[builtin.Input],
                    Expression.Label label => costs[label.Tagged],
                    Expression.Literal => 0,
                    Expression.Environment => 0,
                    Expression.BuiltinBinary equal => Add(costs[equal.Left], costs[equal.Right]),
                    Expression.Length_Equal_Binary length => Add(costs[length.Sequence], costs[length.Other]),
                    Expression.HeadSkip head => costs[head.Input],

                    _ =>
                    throw new NotImplementedException(
                        nameof(ComputeEvalPathMax) + " does not handle expression variant: " +
                        next.expression.GetType().Name),
                });
        }

        return costs[expression];

        static int Add(int left, int right) => (int)Math.Min(int.MaxValue, (long)left + right);

        static IEnumerable<Expression> Children(Expression node) =>
            node switch
            {
                Expression.Conditional conditional =>
                [conditional.Condition, conditional.TrueBranch, conditional.FalseBranch],

                Expression.Eval eval => [eval.Encoded, eval.Environment],
                Expression.List list => list.Items,
                Expression.Builtin builtin => [builtin.Input],
                Expression.Label label => [label.Tagged],
                Expression.Literal => [],
                Expression.Environment => [],
                Expression.BuiltinBinary equal => [equal.Left, equal.Right],
                Expression.Length_Equal_Binary length => [length.Sequence, length.Other],
                Expression.HeadSkip head => [head.Input],

                _ =>
                throw new NotImplementedException(
                    nameof(ComputeEvalPathMax) + " does not handle expression variant: " + node.GetType().Name),
            };
    }

    /// <summary>
    /// Starts environment-aware reduction and recursive inlining for a root expression, seeding the set of alternative root forms used during specialization.
    /// </summary>
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

    /// <summary>
    /// Continues environment-aware reduction and recursive inlining while tracking the current inline stack to avoid runaway recursion.
    /// </summary>
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
            ExpressionReduction.ReduceExpressionBottomUp(
                expressionSubstituted,
                ReductionConfig.Default
                with
                {
                    DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                },
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

        Expression? TryInlineEval(
            Expression.Eval evalExpr,
            bool noRecursion)
        {
            Expression? ContinueReduceForKnownExprValue(PineValue exprValue)
            {
                if (parseCache.ParseInternalExpression(exprValue).IsOkOrNull() is not { } parseOk)
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
                    EnvironmentSubstitution.SubstituteEnvironmentNode(
                        expression: parseOk,
                        environmentReplacement: evalExpr.Environment);

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
                    ExpressionReduction.ReduceExpressionBottomUp(
                        inlinedExprSubstituted,
                        ReductionConfig.Default
                        with
                        {
                            DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation,
                        },
                        parseCache,
                        reducedExpressionCache: reducedExpressionCache);

                if (DuplicatesArgumentEvaluations(evalExpr, parseOk, inlinedExprReduced))
                    return null;

                bool IsExpansionCandidateForSecondInliner(Expression.Eval pe)
                {
                    if (pe.Encoded.ReferencesEnvironment)
                    {
                        return false;
                    }

                    if (ExpressionReduction.TryEvalIndependent(
                        pe.Encoded,
                        parseCache).Value is not { } peValue)
                    {
                        return false;
                    }

                    if (parseCache.ParseInternalExpressionWithoutResultAllocation(peValue).Expression is not { } peParsed)
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
                     * (residual <see cref="Expression.Eval"/> nodes still cost a
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
                        ComputeEvalPathMax(
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

            if (!evalExpr.Encoded.ReferencesEnvironment)
            {
                if (ExpressionReduction.TryEvalIndependent(
                    evalExpr.Encoded,
                    parseCache).Value is { } evalExprOk)
                {
                    return ContinueReduceForKnownExprValue(evalExprOk);
                }
            }

            return null;
        }

        var expressionInlined =
            InlineEvalRecursive(
                expressionReduced,
                conditionCount: 0,
                (eval, conditionCount) =>
                TryInlineEval(eval, noRecursion: 0 < conditionCount));

        var expressionInlinedReduced =
            ExpressionReduction.ReduceExpressionBottomUp(
                expressionInlined,
                ReductionConfig.Default
                with
                {
                    DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                },
                parseCache,
                reducedExpressionCache: reducedExpressionCache);

        return expressionInlinedReduced;
    }

    /// <summary>
    /// Replaces subexpressions that are fixed by the given environment class with literal values before further reduction.
    /// </summary>
    public static Expression SubstituteSubexpressionsForEnvironmentConstraint(
        Expression originalExpression,
        PineValueClass envConstraintId)
    {
        return
            ExpressionReduction.TransformPineExpressionWithOptionalReplacement(
                findReplacement:
                descendant =>
                {
                    if (descendant is Expression.Literal)
                        return null;

                    if (ExpressionAnalysis.TryParseAsLiteral(descendant) is { } literal)
                    {
                        return Expression.LiteralInstance(literal);
                    }

                    if (ExpressionAnalysis.TryParseExprAsPathInEnv(descendant) is { } pathInEnv)
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

    /// <summary>
    /// Compiles an expression into the control-flow graph of the stack frame the intermediate VM executes
    /// for the supplied environment classification.
    /// The graph is built from structured control flow, before any sequential instruction list is created.
    /// </summary>
    public static PineControlFlowGraph ControlFlowGraphFromExpression(
        Expression rootExpression,
        ImmutableHashSet<Expression> rootExprAlternativeForms,
        PineValueClass? envClass,
        StaticFunctionInterface parametersAsLocals,
        PineVMParseCache parseCache,
        bool enableTailRecursionOptimization = false,
        bool enableDirectInvocation = true,
        Func<PineValue, bool>? skipDirectInvocation = null)
    {
        var compiled =
            InternalPineIRCompiler.CompileExpression(
                rootExpression,
                rootExprAlternativeForms: rootExprAlternativeForms,
                envClass,
                parametersAsLocals: parametersAsLocals,
                parseCache,
                enableTailRecursionOptimization,
                enableDirectInvocation,
                skipDirectInvocation);

        return PineControlFlowGraph.FromFragment(compiled.Fragment);
    }

    /// <summary>
    /// Enumerates expression components from deepest to shallowest so dependencies are compiled before the expressions that reference them.
    /// </summary>
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

            Expression.PushChildren(expression, stack, skipConditionalBranches: true);

            deepestDescendants.Push(expression);
        }

        while (deepestDescendants.Count > 0)
        {
            yield return deepestDescendants.Pop();
        }
    }
}
