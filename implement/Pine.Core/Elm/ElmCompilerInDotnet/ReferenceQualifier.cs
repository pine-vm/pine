using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Immutable;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Shared walker that rewrites unqualified
/// <see cref="SyntaxTypes.Expression.FunctionOrValue"/> references into
/// qualified ones under a caller-supplied predicate. Optionally tracks
/// local lexical scope so that names introduced by lambda / let / case
/// patterns shadow the qualification rule.
/// <para>
/// Consolidates three near-identical name-qualifier walkers that
/// previously lived in <c>LambdaLifting.cs</c>,
/// <c>ElmSyntaxOptimization.cs</c>, and <c>WrapperReturnStripping.cs</c>.
/// See <c>explore/internal-analysis/2026-05-18-eliminate-higher-order-parameters-in-focused-tests.md</c>
/// §11.1 for the rationale.
/// </para>
/// </summary>
internal static class ReferenceQualifier
{
    /// <summary>
    /// Walks <paramref name="root"/> and replaces every unqualified
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/> whose
    /// <c>Name</c> satisfies <paramref name="nameSelector"/> with a
    /// qualified reference into <paramref name="targetModule"/>.
    /// <para>
    /// When <paramref name="trackLocalScope"/> is <c>true</c>, names
    /// introduced by lambda / let / case patterns shadow the selector —
    /// a matching name that is locally bound is left untouched. The
    /// optional <paramref name="initialLocalScope"/> seeds the scope at
    /// the root (e.g. with the enclosing function's argument names).
    /// </para>
    /// <para>
    /// Returns the same <see cref="Node{T}"/> reference when no
    /// rewriting was needed, so callers can use
    /// <c>ReferenceEquals</c> to fast-path the unchanged case.
    /// </para>
    /// </summary>
    internal static Node<SyntaxTypes.Expression> Qualify(
        Node<SyntaxTypes.Expression> root,
        ModuleName targetModule,
        Func<string, bool> nameSelector,
        bool trackLocalScope,
        ImmutableHashSet<string>? initialLocalScope = null)
    {
        var scope =
            trackLocalScope
            ?
            (initialLocalScope ?? ImmutableHashSet<string>.Empty.WithComparer(StringComparer.Ordinal))
            :
            null;

        return QualifyNode(root, targetModule, nameSelector, scope);
    }

    private static Node<SyntaxTypes.Expression> QualifyNode(
        Node<SyntaxTypes.Expression> node,
        ModuleName targetModule,
        Func<string, bool> nameSelector,
        ImmutableHashSet<string>? scope)
    {
        var rewritten = QualifyExpression(node.Value, targetModule, nameSelector, scope);

        if (ReferenceEquals(rewritten, node.Value))
            return node;

        return new Node<SyntaxTypes.Expression>(node.Range, rewritten);
    }

    private static SyntaxTypes.Expression QualifyExpression(
        SyntaxTypes.Expression expression,
        ModuleName targetModule,
        Func<string, bool> nameSelector,
        ImmutableHashSet<string>? scope)
    {
        if (expression is SyntaxTypes.Expression.FunctionOrValue fov &&
            fov.ModuleName.Count is 0 &&
            (scope is null || !scope.Contains(fov.Name)) &&
            nameSelector(fov.Name))
        {
            return new SyntaxTypes.Expression.FunctionOrValue(targetModule, fov.Name);
        }

        if (scope is null)
        {
            // No local-scope tracking: a simple child-mapper is enough,
            // since scope-extending variants don't need special handling.
            return
                ElmSyntaxTransformations.MapChildExpressions(
                    expression,
                    child => QualifyNode(child, targetModule, nameSelector, scope: null));
        }

        return
            ElmSyntaxTransformations.MapChildExpressionsWithScope(
                expression,
                scope,
                (child, childScope) =>
                QualifyNode(child, targetModule, nameSelector, childScope));
    }
}
