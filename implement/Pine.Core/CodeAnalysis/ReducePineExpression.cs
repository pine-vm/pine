using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;

using static Pine.Core.Runtime.InternalExpression.CanonicalExpressionAdapter;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Configures optional behaviors of the bottom-up expression reducer
/// (<see cref="ReducePineExpression.ReduceExpressionBottomUp(Expression, ReductionConfig, PineVMParseCache, IDictionary{ValueTuple{Expression, ReductionConfig}, Expression}?)"/>).
/// <para>
/// The configuration also participates in the cache key for the optional
/// <c>reducedExpressionCache</c>, ensuring entries reduced under different
/// configurations do not collide.
/// </para>
/// </summary>
/// <param name="DisableGenericApplicationChainConsolidation">When <c>true</c>,
/// suppresses
/// <see cref="ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(Expression.Eval, PineVMParseCache, ReductionConfig)"/>
/// during reduction. Intended for diagnostic / inspection scenarios where the
/// caller wants to observe the pre-consolidation behavior side-by-side with the
/// optimized one.</param>
/// <param name="InlineEvalEnvironmentSizeLimit">Specifies the maximum size of the environment
/// for which <see cref="ReducePineExpression.TryInlineEvalBottomUp(Expression.Eval, ReductionConfig, PineVMParseCache, IDictionary{ValueTuple{Expression, ReductionConfig}, Expression}?)"/>
/// will attempt to inline <see cref="Expression.Eval"/> nodes. Intended for diagnostic /
/// inspection scenarios where the caller wants to observe the
/// pre-inlining shape of the expression tree.</param>
public record struct ReductionConfig(
    bool DisableGenericApplicationChainConsolidation,
    int InlineEvalEnvironmentSizeLimit)
{
    /// <summary>
    /// The default configuration: all optional behaviors enabled (no suppression flags set).
    /// </summary>
    public static readonly ReductionConfig Default =
        new(
            DisableGenericApplicationChainConsolidation: false,
            InlineEvalEnvironmentSizeLimit: 10_000);
}

/// <summary>
/// Helpers for evaluating and reducing Pine expressions without requiring a runtime environment.
/// Provides utilities for constant folding, small-step reductions, and tree transformations
/// used by analyzers and compilers.
/// </summary>
public class ReducePineExpression
{
    internal readonly struct ValueEvalResult
    {
        private ValueEvalResult(PineValue? value, string? error)
        {
            Value = value;
            Error = error;
        }

        public PineValue? Value { get; }

        public string? Error { get; }

        public bool IsOk => Value is not null;

        public static ValueEvalResult Ok(PineValue value) =>
            new(value, error: null);

        public static ValueEvalResult Err(string error) =>
            new(value: null, error);

        public Result<string, PineValue> ToPublicResult() =>
            Value is { } value
            ?
            value
            :
            Error ??
            throw new InvalidOperationException(
                "Independent evaluation result contains neither a value nor an error");
    }

    /// <summary>
    /// Attempts to evaluate an <see cref="Expression"/> that does not depend on the environment.
    /// Returns a <c>Result</c> whose <c>Ok</c> value is the computed <see cref="PineValue"/>,
    /// or an error message if the expression cannot be evaluated independently.
    /// </summary>
    /// <param name="expression">The expression to evaluate.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>A <see cref="Result{TError, TOk}"/> with an error description or the computed value.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression expression,
        PineVMParseCache parseCache) =>
        Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryEvaluateExpressionIndependent(
            ToInternal(expression),
            parseCache);

    internal static ValueEvalResult TryEvalIndependent(
        Expression expression,
        PineVMParseCache parseCache) =>
        AdaptValueResult(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryEvalIndependent(
                ToInternal(expression),
                parseCache));

    /// <summary>
    /// Attempts to evaluate a list expression by evaluating each item independently.
    /// On success, returns a concrete <see cref="PineValue.ListValue"/>; otherwise the first error encountered.
    /// </summary>
    /// <param name="listExpr">The list expression to evaluate.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>The evaluated list value or an error message.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.List listExpr,
        PineVMParseCache parseCache) =>
        Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryEvaluateExpressionIndependent(
            ToInternal(listExpr),
            parseCache);

    /// <summary>
    /// Attempts to evaluate an Eval expression without a runtime environment.
    /// The encoded expression is parsed using <paramref name="parseCache"/>, the provided environment is inlined,
    /// and the inner expression is then evaluated independently.
    /// </summary>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.Eval evalExpr,
        PineVMParseCache parseCache) =>
        Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryEvaluateExpressionIndependent(
            ToInternal(evalExpr),
            parseCache);

    /// <summary>
    /// Attempts to evaluate a conditional expression independently.
    /// Evaluates the condition and returns the value of either the true or false branch accordingly.
    /// </summary>
    /// <param name="conditionalExpr">The conditional expression.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>The result of the selected branch, or an error message if evaluation fails.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.Conditional conditionalExpr,
        PineVMParseCache parseCache) =>
        Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryEvaluateExpressionIndependent(
            ToInternal(conditionalExpr),
            parseCache);

    /// <summary>
    /// Attempts to evaluate a built-in function application independently.
    /// The input is first evaluated, then dispatched to <see cref="BuiltinFunction.ApplyFunctionGeneric(string, PineValue)"/>.
    /// </summary>
    /// <param name="builtinExpr">The built-in application expression.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>The computed value, or an error message if evaluation fails.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.Builtin builtinExpr,
        PineVMParseCache parseCache) =>
        Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryEvaluateExpressionIndependent(
            ToInternal(builtinExpr),
            parseCache);

    /// <summary>
    /// Searches for a local reduction opportunity in the given <paramref name="expression"/>.
    /// If a reduction is found, returns the reduced expression; otherwise returns <c>null</c>.
    /// Optional environment constraints may enable more reductions.
    /// </summary>
    /// <param name="expression">The expression to inspect.</param>
    /// <param name="envConstraintId">Optional structural constraints for values taken from the environment.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>A reduced expression or <c>null</c> if no reduction applies.</returns>
    public static Expression? SearchForExpressionReduction(
        Expression expression,
        PineValueClass? envConstraintId,
        PineVMParseCache parseCache) =>
        ToCanonicalOrNull(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.SearchForExpressionReduction(
                ToInternal(expression),
                envConstraintId,
                parseCache));

    /// <summary>
    /// Returns <c>true</c> when <paramref name="expression"/> is statically
    /// known to evaluate to a Pine built-in boolean value
    /// (<see cref="PineKernelValues.TrueValue"/> or
    /// <see cref="PineKernelValues.FalseValue"/>).
    /// <para>
    /// This is conservative: it only returns <c>true</c> when the expression
    /// is one of a small set of forms guaranteed to produce a boolean.
    /// It is used to gate optimizations that would otherwise be unsound for
    /// non-boolean values whose byte representation collides with that of
    /// <see cref="PineKernelValues.TrueValue"/> (<c>Blob([4])</c>) or
    /// <see cref="PineKernelValues.FalseValue"/> (<c>Blob([2])</c>).
    /// In particular, the byte sequence <c>[0x02]</c> arises from
    /// <c>Pine_kernel.skip [ 1, 2 ]</c> and
    /// <c>[0x04]</c> from <c>Pine_kernel.skip [ 1, 4 ]</c>, neither of which
    /// is intended to be interpreted as a boolean.
    /// </para>
    /// </summary>
    /// <param name="expression">The expression to inspect.</param>
    /// <returns><c>true</c> if the expression is provably boolean; otherwise <c>false</c>.</returns>
    public static bool IsKnownBooleanExpression(Expression expression) =>
        Pine.Core.Runtime.InternalExpression.ExpressionReduction.IsKnownBooleanExpression(ToInternal(expression));

    /// <summary>
    /// Infers lower and upper bounds for the length of lists produced by the given expression,
    /// using optional environment constraints and local reasoning.
    /// </summary>
    public static (int? lower, int? upper) InferListLengthBounds(
        Expression expression,
        PineValueClass envConstraintId,
        PineVMParseCache parseCache) =>
        Pine.Core.Runtime.InternalExpression.ExpressionReduction.InferListLengthBounds(
            ToInternal(expression),
            envConstraintId,
            parseCache);

    /// <summary>
    /// Infers lower and upper bounds for the length of lists produced by the given expression,
    /// using optional environment constraints and local reasoning.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <param name="envConstraintId">Constraints for values reachable from the environment.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>An enumeration of lower bound candidates for the list length.</returns>
    public static IEnumerable<(int? lower, int? upper)> EnumerateInferListLengthBounds(
        Expression expression,
        PineValueClass envConstraintId,
        PineVMParseCache parseCache) =>
        Pine.Core.Runtime.InternalExpression.ExpressionReduction.EnumerateInferListLengthBounds(
            ToInternal(expression),
            envConstraintId,
            parseCache);

    /// <summary>
    /// Transforms an expression tree by attempting to replace nodes using a provided function.
    /// Returns the transformed expression along with a flag indicating whether the original
    /// environment is still referenced by any node in the transformed tree.
    /// </summary>
    /// <param name="findReplacement">Function that returns a replacement expression for a node, or <c>null</c> to keep it.</param>
    /// <param name="expression">The root expression to transform.</param>
    /// <returns>The transformed expression and whether it references the original environment.</returns>
    public static (Expression expr, bool referencesOriginalEnv) TransformPineExpressionWithOptionalReplacement(
        Func<Expression, Expression?> findReplacement,
        Expression expression) =>
        ToCanonical(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.TransformPineExpressionWithOptionalReplacement(
                ToInternal(findReplacement),
                ToInternal(expression)));

    /// <summary>
    /// Recursively searches for reductions up to a maximum depth.
    /// Applies <see cref="SearchForExpressionReduction"/> repeatedly until no change occurs or the depth limit is reached.
    /// </summary>
    /// <param name="maxDepth">Maximum number of recursive reduction rounds.</param>
    /// <param name="expression">The expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="envConstraintId">Optional environment constraints to enable reductions.</param>
    /// <returns>The possibly reduced expression.</returns>
    public static Expression SearchForExpressionReductionRecursive(
        int maxDepth,
        Expression expression,
        PineVMParseCache parseCache,
        PineValueClass? envConstraintId = null) =>
        ToCanonical(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.SearchForExpressionReductionRecursive(
                maxDepth,
                ToInternal(expression),
                parseCache,
                envConstraintId));

    /// <summary>
    /// Performs a single bottom-up reduction pass with the default <see cref="ReductionConfig"/>:
    ///   1. Recursively reduces children first.
    ///   2. Attempts to reduce the resulting node via <see cref="SearchForExpressionReduction"/>.
    /// </summary>
    /// <param name="expression">The expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache keyed by
    /// <c>(expression, config)</c>. When supplied, lookups and inserts are scoped per
    /// <see cref="ReductionConfig"/> so entries from different configurations cannot collide.</param>
    /// <returns>The reduced expression if any reduction was possible; otherwise the original expression.</returns>
    public static Expression ReduceExpressionBottomUp(
        Expression expression,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null) =>
        ToCanonical(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ReduceExpressionBottomUp(
                ToInternal(expression),
                parseCache,
                ToInternal(reducedExpressionCache)));

    /// <summary>
    /// Performs a single bottom-up reduction pass:
    ///   1. Recursively reduces children first.
    ///   2. Attempts to reduce the resulting node via <see cref="SearchForExpressionReduction"/>.
    /// </summary>
    /// <param name="expression">The expression to reduce.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.
    /// Also forms part of the <paramref name="reducedExpressionCache"/> key.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache keyed by
    /// <c>(expression, config)</c>. When supplied, lookups and inserts are scoped per
    /// <see cref="ReductionConfig"/> so entries from different configurations cannot collide.</param>
    /// <returns>The reduced expression if any reduction was possible; otherwise the original expression.</returns>
    public static Expression ReduceExpressionBottomUp(
        Expression expression,
        ReductionConfig config,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null) =>
        ToCanonical(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ReduceExpressionBottomUp(
                ToInternal(expression),
                config,
                parseCache,
                ToInternal(reducedExpressionCache)));

    /// <summary>
    /// Reduces a list expression bottom-up by first reducing each item, then attempting a local reduction.
    /// </summary>
    /// <param name="listExpr">The list expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.</param>
    /// <returns>The reduced list expression or the original if no change occurs.</returns>
    public static Expression ReduceListExpressionBottomUp(
        Expression.List listExpr,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default) =>
        ToCanonical(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ReduceListExpressionBottomUp(
                ToInternal(listExpr),
                parseCache,
                ToInternal(reducedExpressionCache),
                config));

    /// <summary>
    /// Reduces a built-in application bottom-up by first reducing its input, then attempting a local reduction.
    /// </summary>
    /// <param name="builtinExpr">The built-in application to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceBuiltinBottomUp(
        Expression.Builtin builtinExpr,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default) =>
        ToCanonical(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ReduceBuiltinBottomUp(
                ToInternal(builtinExpr),
                parseCache,
                ToInternal(reducedExpressionCache),
                config));

    /// <summary>
    /// Reduces an Eval expression bottom-up by reducing both the encoded expression and the environment.
    /// </summary>
    /// <param name="evalExpr">The Eval expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors,
    /// including whether
    /// <see cref="TryConsolidateGenericFunctionApplicationChain(Expression.Eval, PineVMParseCache, ReductionConfig)"/>
    /// is applied.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceEvalBottomUp(
        Expression.Eval evalExpr,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default) =>
        ToCanonical(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ReduceEvalBottomUp(
                ToInternal(evalExpr),
                parseCache,
                ToInternal(reducedExpressionCache),
                config));

    /// <summary>
    /// Attempts to inline a <see cref="Expression.Eval"/> whose
    /// <see cref="Expression.Eval.Encoded"/> resolves to a known
    /// constant Pine-encoded expression value, by substituting the parsed inner
    /// expression's <see cref="Expression.Environment"/> nodes with the outer
    /// <see cref="Expression.Eval.Environment"/>.
    /// </summary>
    public static Expression? TryInlineEvalBottomUp(
        Expression.Eval evalExpr,
        ReductionConfig config,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache) =>
        ToCanonicalOrNull(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryInlineEvalBottomUp(
                ToInternal(evalExpr),
                config,
                parseCache,
                ToInternal(reducedExpressionCache)));

    /// <summary>
    /// Attempts to collapse a chain of nested <see cref="Expression.Eval"/> expressions
    /// produced by the generic form of function application
    /// (<see cref="CodeAnalysis.BuildGenericFunctionApplication(Expression, IReadOnlyList{Expression})"/>)
    /// into a single, equivalent expression.
    /// <para>
    /// Triggers only when the recovered argument list contains more than one argument and
    /// the recovered function expression is an <see cref="Expression.Litral"/> whose value
    /// parses successfully as a Pine expression. The transformation symbolically simulates
    /// each application step using the structural encoding emitted by
    /// <c>FunctionValueBuilder</c>: it inlines the literal-encoded function body for the
    /// first argument and then "decodes" the resulting construction expression for each
    /// subsequent argument, producing a single equivalent expression that no longer requires
    /// one <see cref="Expression.Eval"/> per argument.
    /// </para>
    /// </summary>
    /// <param name="evalExpr">The outermost <see cref="Expression.Eval"/> of the chain.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors. When
    /// <see cref="ReductionConfig.DisableGenericApplicationChainConsolidation"/> is <c>true</c>,
    /// the method returns <c>null</c> regardless of the chain shape. Intended exclusively for
    /// diagnostic / inspection scenarios where the caller wants to observe the
    /// pre-consolidation behavior side-by-side with the optimized one.</param>
    /// <returns>The collapsed expression, or <c>null</c> when no consolidation applies.</returns>
    public static Expression? TryConsolidateGenericFunctionApplicationChain(
        Expression.Eval evalExpr,
        PineVMParseCache parseCache,
        ReductionConfig config = default) =>
        ToCanonicalOrNull(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryConsolidateGenericFunctionApplicationChain(
                ToInternal(evalExpr),
                parseCache,
                config));

    /// <summary>
    /// Symbolically computes an expression equivalent to
    /// <c>Eval(<paramref name="construction"/>, <paramref name="envArg"/>)</c>,
    /// assuming <paramref name="construction"/> uses the same structural encoding scheme as
    /// <see cref="ExpressionEncoding.EncodeExpressionAsValue(Expression)"/>.
    /// Returns <c>null</c> when the construction does not match a recognized encoded shape.
    /// </summary>
    /// <param name="construction">An expression that, when evaluated, would yield an
    /// encoded Pine expression value (matching the structural encoding scheme).</param>
    /// <param name="envArg">The environment expression that the encoded inner expression
    /// would receive at runtime.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    public static Expression? TryDecodeApplicationOfConstructedEncoding(
        Expression construction,
        Expression envArg,
        PineVMParseCache parseCache) =>
        ToCanonicalOrNull(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryDecodeApplicationOfConstructedEncoding(
                ToInternal(construction),
                ToInternal(envArg),
                parseCache));

    /// <summary>
    /// Reduces a conditional expression bottom-up by reducing its components and then attempting a local reduction.
    /// </summary>
    /// <param name="conditional">The conditional expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceConditionalBottomUp(
        Expression.Conditional conditional,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default) =>
        ToCanonical(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ReduceConditionalBottomUp(
                ToInternal(conditional),
                parseCache,
                ToInternal(reducedExpressionCache),
                config));

    /// <summary>
    /// Reduces a labelled expression bottom-up by reducing the tagged subexpression.
    /// </summary>
    /// <param name="labelExpr">The labelled expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceLabelBottomUp(
        Expression.Label labelExpr,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default) =>
        ToCanonical(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ReduceLabelBottomUp(
                ToInternal(labelExpr),
                parseCache,
                ToInternal(reducedExpressionCache),
                config));

    /// <summary>
    /// Attempts to push a reverse operation into the structure of an expression without evaluation.
    /// Supports reversing concrete lists and list-literals, cancels double-reverse built-in function applications,
    /// and propagates into both branches of conditionals and into string-tagged subexpressions.
    /// </summary>
    /// <param name="expression">The expression representing a list whose result should be reversed.</param>
    /// <returns>
    /// A structurally transformed expression equivalent to applying <c>reverse</c>,
    /// or <c>null</c> if no structural reduction is applicable.
    /// </returns>
    public static Expression? ApplyBuiltinFunctionReverseToAllBranches(
        Expression expression) =>
        ToCanonicalOrNull(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ApplyBuiltinFunctionReverseToAllBranches(
                ToInternal(expression)));

    /// <summary>
    /// Attempts to push a skip operation into the expression tree without evaluation.
    /// Supports concrete lists, list-literals, nested <c>skip</c> built-in function applications (which are combined),
    /// and propagation into both branches of a conditional and into string-tagged expressions.
    /// </summary>
    /// <param name="count">Number of elements to skip. Negative values are treated as zero.</param>
    /// <param name="expression">The source expression from which to skip elements.</param>
    /// <returns>
    /// A structurally transformed expression that represents the skip, or <c>null</c>
    /// if the transformation cannot be applied safely.
    /// </returns>
    public static Expression? ApplyBuiltinFunctionSkipToAllBranches(
        int count,
        Expression expression) =>
        ToCanonicalOrNull(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ApplyBuiltinFunctionSkipToAllBranches(
                count,
                ToInternal(expression)));

    /// <summary>
    /// Attempts to push a take operation into the expression tree without evaluation.
    /// Supports concrete lists, list-literals, nested <c>take</c> built-in function applications (which are combined),
    /// and propagation into both branches of a conditional and into string-tagged expressions.
    /// </summary>
    /// <param name="count">Number of elements to take. Negative values are treated as zero.</param>
    /// <param name="expression">The source expression from which to take elements.</param>
    /// <returns>
    /// A structurally transformed expression that represents the take, or <c>null</c>
    /// if the transformation cannot be applied safely.
    /// </returns>
    public static Expression? ApplyBuiltinFunctionTakeToAllBranches(
        int count,
        Expression expression) =>
        ToCanonicalOrNull(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ApplyBuiltinFunctionTakeToAllBranches(
                count,
                ToInternal(expression)));

    /// <summary>
    /// Attempts to push a head operation (select first element) into the expression tree without evaluation.
    /// If the input is a concrete list or list-literal, returns the first element or an empty list when out of bounds.
    /// Propagates into both branches of a conditional and into string-tagged expressions when possible.
    /// </summary>
    /// <param name="expression">The source expression representing a list from which to select the head.</param>
    /// <returns>
    /// A structurally transformed expression representing the head selection, or <c>null</c>
    /// if no structural reduction is applicable.
    /// </returns>
    public static Expression? ApplyBuiltinFunctionHeadToAllBranches(Expression expression) =>
        ToCanonicalOrNull(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.ApplyBuiltinFunctionHeadToAllBranches(
                ToInternal(expression)));

    /// <summary>
    /// Attempts to select the list item at a given index from the provided expression without
    /// evaluating with a runtime environment. Supports:
    /// - Expression.List: returns the item or EmptyList if out of bounds.
    /// - Expression.Literal(list): returns a literal of the item or EmptyList if out of bounds.
    /// - Expression.Conditional: pushes the selection into both branches when possible.
    /// - Expression.StringTag: unwraps and attempts on the tagged expression.
    /// Returns null if no structural reduction can be applied.
    /// </summary>
    public static Expression? TryReduceSelectListItem(Expression inputExpression, int index) =>
        ToCanonicalOrNull(
            Pine.Core.Runtime.InternalExpression.ExpressionReduction.TryReduceSelectListItem(
                ToInternal(inputExpression),
                index));

    private static ValueEvalResult AdaptValueResult(
        Pine.Core.Runtime.InternalExpression.ExpressionReduction.ValueEvalResult result) =>
        result.Value is { } value
        ?
        ValueEvalResult.Ok(value)
        :
        ValueEvalResult.Err(result.Error ?? throw new InvalidOperationException("Empty independent evaluation result."));
}
