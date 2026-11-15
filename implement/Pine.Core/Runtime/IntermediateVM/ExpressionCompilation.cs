using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using static Pine.Core.Runtime.InternalExpression.CanonicalExpressionAdapter;

namespace Pine.Core.Runtime.IntermediateVM;

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

    /// <summary>Selects guards directly from a structural terminal-environment recipe.</summary>
    internal StackFrameInstructions SelectInstructionsForTemplate(
        TemplateValueRecipe environment,
        IReadOnlyList<PineValueInProcess> collectedEnvironments)
    {
        foreach (var specialization in Specialized)
        {
            var matches = true;

            foreach (var constraint in specialization.constraint)
            {
                var value = ProjectTemplateGuard(environment, constraint.Path.Span, collectedEnvironments);

                if (value is null || !PineValueInProcess.AreEqual(value, constraint.Value))
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
                return specialization.instructions;
        }

        return Generic;
    }

    private static PineValueInProcess? ProjectTemplateGuard(
        TemplateValueRecipe recipe,
        ReadOnlySpan<int> path,
        IReadOnlyList<PineValueInProcess> environments)
    {
        for (var offset = 0; offset < path.Length; ++offset)
        {
            switch (recipe)
            {
                case TemplateValueRecipe.List list:
                    var index = Math.Max(0, path[offset]);

                    if (index >= list.Items.Count)
                        return null;

                    recipe = list.Items[index];
                    break;

                case TemplateValueRecipe.Constant constant:
                    var value = PineValueExtension.ValueFromPathOrNull(constant.Value, path[offset..]);
                    return value is null ? null : PineValueInProcess.Create(value);

                case TemplateValueRecipe.Slot slot:
                    return PineValueInProcess.ValueInProcessFromPathOrNull(environments[slot.Index], path[offset..]);

                case TemplateValueRecipe.ProjectedSlot slot:
                    return PineValueInProcess.ValueInProcessFromPathOrNull(slot.Instantiate(environments), path[offset..]);

                default:
                    throw new NotImplementedException(
                        "ProjectTemplateGuard does not handle recipe variant: " + recipe.GetType().Name);
            }
        }

        return PineValueInProcess.CreateFromTemplateRecipe(recipe, environments);
    }

    /// <summary>
    /// Selects the most specific instruction variant whose environment constraints match
    /// arguments already projected into a stack-frame input.
    /// </summary>
    public StackFrameInstructions SelectInstructionsForInput(StackFrameInput input)
    {
        for (var specializationIndex = 0; specializationIndex < Specialized.Count; specializationIndex++)
        {
            var specialization = Specialized[specializationIndex];
            var foundMismatch = false;

            for (var constraintIndex = 0; constraintIndex < specialization.constraint.Count; constraintIndex++)
            {
                var constraintItem = specialization.constraint[constraintIndex];
                var parameterIndex = FindParameterIndex(input.Parameters, constraintItem.Path.Span);

                if (parameterIndex is null ||
                    !input.Arguments[parameterIndex.Value].Evaluate().Equals(constraintItem.Value))
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

    private static int? FindParameterIndex(
        StaticFunctionInterface parameters,
        ReadOnlySpan<int> path)
    {
        for (var parameterIndex = 0; parameterIndex < parameters.ParamsPaths.Count; parameterIndex++)
        {
            var parameterPath = parameters.ParamsPaths[parameterIndex];

            if (parameterPath.Count != path.Length)
            {
                continue;
            }

            var pathsEqual = true;

            for (var pathIndex = 0; pathIndex < path.Length; pathIndex++)
            {
                if (parameterPath[pathIndex] != path[pathIndex])
                {
                    pathsEqual = false;
                    break;
                }
            }

            if (pathsEqual)
            {
                return parameterIndex;
            }
        }

        return null;
    }

    /// <summary>Lower bound of the native inliner's path-max gating window.</summary>
    public const int DefaultPathMaxLowExclusive = InternalExpressionCompilation.DefaultPathMaxLowExclusive;

    /// <summary>Upper bound of the native inliner's path-max gating window.</summary>
    public const int DefaultPathMaxHighInclusive = InternalExpressionCompilation.DefaultPathMaxHighInclusive;

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
    /// <param name="pathMaxLowExclusive">Exclusive lower bound for path lengths compiled to specialized instructions.</param>
    /// <param name="pathMaxHighInclusive">Inclusive upper bound for path lengths compiled to specialized instructions.</param>
    /// <param name="disableGenericApplicationChainConsolidation">Whether generic application-chain consolidation should be disabled.</param>
    /// <param name="enableDirectInvocation">Whether direct invocation should be enabled.</param>
    /// <param name="skipDirectInvocation">Optional predicate identifying values that should skip direct invocation.</param>
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
        Func<PineValue, bool>? skipDirectInvocation = null) =>
        InternalExpressionCompilation.CompileExpression(
            ToInternal(rootExpression),
            specializations,
            parseCache,
            disableReduction,
            enableTailRecursionOptimization,
            ToInternal(skipInlining),
            ToInternal(reducedExpressionCache),
            pathMaxLowExclusive,
            pathMaxHighInclusive,
            disableGenericApplicationChainConsolidation,
            enableDirectInvocation,
            skipDirectInvocation);

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
        Func<PineValue, bool>? skipDirectInvocation = null) =>
        InternalExpressionCompilation.InstructionsFromExpressionTransitive(
            ToInternal(rootExpression),
            envConstraintId,
            parametersAsLocals,
            parseCache,
            disableReduction,
            enableTailRecursionOptimization,
            ToInternal(skipInlining),
            ToInternal(reducedExpressionCache),
            pathMaxLowExclusive,
            pathMaxHighInclusive,
            disableGenericApplicationChainConsolidation,
            enableDirectInvocation,
            skipDirectInvocation);

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
        bool disableGenericApplicationChainConsolidation = false) =>
        ToCanonical(
            InternalExpressionCompilation.InlineStaticInvocationsAndReduceRecursive(
                ToInternal(currentExpression),
                ToInternal(inlinedParents),
                maxDepth,
                maxSubexpressionCount,
                parseCache,
                disableRecurseAfterInline,
                ToInternalRequired(skipInlining),
                ToInternal(reducedExpressionCache),
                pathMaxLowExclusive,
                pathMaxHighInclusive,
                disableGenericApplicationChainConsolidation));

    internal static Expression InlineEvalRecursive(
        Expression expression,
        int conditionCount,
        Func<Expression.Eval, int, Expression?> tryInlineEval) =>
        ToCanonical(
            InternalExpressionCompilation.InlineEvalRecursive(
                ToInternal(expression),
                conditionCount,
                ToInternal(tryInlineEval)));

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
        Func<Expression.Eval, bool> isExpansionCandidate) =>
        InternalExpressionCompilation.ComputeEvalPathMax(ToInternal(expression), ToInternal(isExpansionCandidate));

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
        ToCanonical(
            InternalExpressionCompilation.ReduceExpressionAndInlineRecursive(
                ToInternal(rootExpression),
                ToInternal(rootExprAlternativeForms),
                envConstraintId,
                maxDepth,
                maxSubexpressionCount,
                parseCache,
                disableRecurseAfterInline,
                ToInternal(skipInlining),
                ToInternal(reducedExpressionCache),
                pathMaxLowExclusive,
                pathMaxHighInclusive,
                disableGenericApplicationChainConsolidation));

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
        bool disableGenericApplicationChainConsolidation = false) =>
        ToCanonical(
            InternalExpressionCompilation.ReduceExpressionAndInlineRecursive(
                ToInternal(currentExpression),
                ToInternal(inlinedParents),
                envConstraintId,
                ToInternal(rootExprForms),
                maxDepth,
                maxSubexpressionCount,
                parseCache,
                disableRecurseAfterInline,
                ToInternal(skipInlining),
                ToInternal(reducedExpressionCache),
                pathMaxLowExclusive,
                pathMaxHighInclusive,
                disableGenericApplicationChainConsolidation));

    /// <summary>
    /// Replaces subexpressions that are fixed by the given environment class with literal values before further reduction.
    /// </summary>
    public static Expression SubstituteSubexpressionsForEnvironmentConstraint(
        Expression originalExpression,
        PineValueClass envConstraintId) =>
        ToCanonical(
            InternalExpressionCompilation.SubstituteSubexpressionsForEnvironmentConstraint(
                ToInternal(originalExpression),
                envConstraintId));

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
        Func<PineValue, bool>? skipDirectInvocation = null) =>
        InternalExpressionCompilation.ControlFlowGraphFromExpression(
            ToInternal(rootExpression),
            ToInternal(rootExprAlternativeForms),
            envClass,
            parametersAsLocals,
            parseCache,
            enableTailRecursionOptimization,
            enableDirectInvocation,
            skipDirectInvocation);

    /// <summary>
    /// Enumerates expression components from deepest to shallowest so dependencies are compiled before the expressions that reference them.
    /// </summary>
    public static IEnumerable<Expression> ListComponentsOrderedForCompilation(
        Expression rootExpression,
        Func<Expression, bool>? skipDescendants) =>
        ToCanonical(
            InternalExpressionCompilation.ListComponentsOrderedForCompilation(
                ToInternal(rootExpression),
                ToInternal(skipDescendants)));

}
