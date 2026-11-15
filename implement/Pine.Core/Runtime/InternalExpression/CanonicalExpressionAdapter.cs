using Pine.Core.CodeAnalysis;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Canonical = Pine.Core.PineLanguage.Expression;

namespace Pine.Core.Runtime.InternalExpression;

internal static class CanonicalExpressionAdapter
{
    internal static Expression ToInternal(Canonical expression) => ExpressionConversion.FromCanonicalCached(expression);

    internal static Expression.List ToInternal(Canonical.List expression) =>
        (Expression.List)ToInternal((Canonical)expression);

    internal static Expression.Eval ToInternal(Canonical.Eval expression) =>
        (Expression.Eval)ToInternal((Canonical)expression);

    internal static Expression.Conditional ToInternal(Canonical.Conditional expression) =>
        (Expression.Conditional)ToInternal((Canonical)expression);

    internal static Expression.Label ToInternal(Canonical.Label expression) =>
        (Expression.Label)ToInternal((Canonical)expression);

    internal static Canonical ToCanonical(Expression expression) => ExpressionConversion.ToCanonicalCached(expression);

    internal static Canonical? ToCanonicalOrNull(Expression? expression) =>
        expression is null ? null : ToCanonical(expression);

    internal static IEnumerable<Canonical> ToCanonical(IEnumerable<Expression> expressions) =>
        expressions.Select(ToCanonical);

    internal static (Canonical expr, bool referencesOriginalEnv) ToCanonical(
        (Expression expr, bool referencesOriginalEnv) result) =>
        (ToCanonical(result.expr), result.referencesOriginalEnv);

    internal static ImmutableHashSet<Expression> ToInternal(ImmutableHashSet<Canonical> expressions) =>
        [.. expressions.Select(ToInternal)];

    internal static IReadOnlySet<Expression> ToInternal(IReadOnlySet<Canonical> expressions) =>
        expressions.Select(ToInternal).ToImmutableHashSet();

    internal static ImmutableStack<Expression> ToInternal(ImmutableStack<Canonical> expressions) =>
        ImmutableStack.CreateRange(expressions.Reverse().Select(ToInternal));

    internal static ImmutableDictionary<Expression, int> ToInternal(ImmutableDictionary<Canonical, int> expressions) =>
        expressions.ToImmutableDictionary(pair => ToInternal(pair.Key), pair => pair.Value);

    internal static Func<Expression, bool>? ToInternal(Func<Canonical, bool>? predicate) =>
        predicate is null ? null : ToInternalRequired(predicate);

    internal static Func<Expression, bool> ToInternalRequired(Func<Canonical, bool> predicate) =>
        expression => predicate(ToCanonical(expression));

    internal static Func<Expression.Eval, bool> ToInternal(Func<Canonical.Eval, bool> predicate) =>
        expression => predicate((Canonical.Eval)ToCanonical(expression));

    internal static Func<Expression, PineValueClass?, bool> ToInternal(
        Func<Canonical, PineValueClass?, bool> predicate) =>
        (expression, constraint) => predicate(ToCanonical(expression), constraint);

    internal static Func<Expression, Expression?> ToInternal(Func<Canonical, Canonical?> replacement) =>
        expression => replacement(ToCanonical(expression)) is { } result ? ToInternal(result) : null;

    internal static Func<Expression.Eval, int, Expression?> ToInternal(
        Func<Canonical.Eval, int, Canonical?> replacement) =>
        (expression, count) => replacement((Canonical.Eval)ToCanonical(expression), count) is { } result ? ToInternal(result) : null;

    internal static IDictionary<(Expression, ReductionConfig), Expression>? ToInternal(
        IDictionary<(Canonical, ReductionConfig), Canonical>? cache) =>
        cache is RetainedReductionCache retained
        ?
        retained.Entries
        :
        cache is null ? null : new CanonicalReductionCacheAdapter(cache);

    internal static InternalPineIRCompiler.NodeCompilationResult ToInternal(
        PineIRCompiler.NodeCompilationResult result) =>
        new(result.Fragment, ToInternal(result.LocalsSet));

    internal static PineIRCompiler.NodeCompilationResult ToCanonical(
        InternalPineIRCompiler.NodeCompilationResult result) =>
        new(result.Fragment, result.LocalsSet.ToImmutableDictionary(pair => ToCanonical(pair.Key), pair => pair.Value));

    internal static (PineIRCompiler.NodeCompilationResult nodeResult, int localIndex) ToCanonical(
        (InternalPineIRCompiler.NodeCompilationResult nodeResult, int localIndex) result) =>
        (ToCanonical(result.nodeResult), result.localIndex);

    internal static InternalPineIRCompiler.CompilationContext ToInternal(PineIRCompiler.CompilationContext context) =>
        new(
            [.. context.CopyToLocal.Select(ToInternal)],
            context.TailLoop is { } tail
            ?
            new InternalPineIRCompiler.TailLoopTarget(
                ToInternal(tail.RootExpressionForms),
                tail.EnvironmentClass,
                tail.GuardExpressionValues)
            :
            null,
            context.StackFrameParameters,
            context.IsTailPosition,
            context.EnableDirectInvocation,
            context.SkipDirectInvocation);
}
