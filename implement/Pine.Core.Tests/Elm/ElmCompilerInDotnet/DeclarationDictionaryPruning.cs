using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Helpers for pruning a flat Elm declaration dictionary down to the
/// declarations actually needed for a chosen set of root declarations.
///
/// <para>
/// The Elm compiler's lowering pipeline can leave behind intermediate
/// function declarations that are no longer reachable from the program
/// entry points (for example after specialization or inlining produces a
/// new variant and the generic original is no longer referenced). When
/// authoring tests over the compiler output it is convenient to drop such
/// stragglers so snapshots stay focused on what matters.
/// </para>
/// </summary>
public static class DeclarationDictionaryPruning
{
    /// <summary>
    /// Returns a copy of <paramref name="declarations"/> containing only
    /// the function declarations transitively reachable from
    /// <paramref name="roots"/> via <see cref="SyntaxTypes.Expression.FunctionOrValue"/>
    /// references in their bodies, plus every non-function declaration
    /// (custom types, aliases, ports, infix declarations) unchanged.
    ///
    /// <para>
    /// Roots that are not present in <paramref name="declarations"/> are
    /// silently ignored. The traversal handles let-bound functions, lambda
    /// bodies, and case branches via the same child-expression enumeration
    /// used by <see cref="OptimizationOpportunityFinder"/>.
    /// </para>
    /// </summary>
    public static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> PruneToReachable(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        IReadOnlySet<DeclQualifiedName> roots)
    {
        var reachableFunctions = new HashSet<DeclQualifiedName>();

        var queue = new Queue<DeclQualifiedName>();

        foreach (var root in roots)
        {
            if (declarations.TryGetValue(root, out var rootDecl) &&
                rootDecl is SyntaxTypes.Declaration.FunctionDeclaration &&
                reachableFunctions.Add(root))
            {
                queue.Enqueue(root);
            }
        }

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (declarations[current] is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                continue;

            foreach (var referenced in
                EnumerateReferencedDeclarations(
                    funcDecl.Function.Declaration.Value.Expression.Value,
                    declarations))
            {
                if (reachableFunctions.Add(referenced))
                    queue.Enqueue(referenced);
            }
        }

        var resultBuilder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (key, value) in declarations)
        {
            if (value is SyntaxTypes.Declaration.FunctionDeclaration)
            {
                if (reachableFunctions.Contains(key))
                    resultBuilder[key] = value;
            }
            else
            {
                // Non-function declarations (custom types, aliases, ports,
                // infix declarations) are kept as-is: they have no
                // outgoing references in this representation, and other
                // declarations may depend on them in ways that are not
                // expressed via FunctionOrValue references.
                resultBuilder[key] = value;
            }
        }

        return resultBuilder.ToImmutable();
    }

    private static IEnumerable<DeclQualifiedName> EnumerateReferencedDeclarations(
        SyntaxTypes.Expression rootExpression,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations)
    {
        var stack = new Stack<SyntaxTypes.Expression>();
        stack.Push(rootExpression);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            if (current is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
                funcOrValue.ModuleName.Count > 0)
            {
                var candidate =
                    new DeclQualifiedName(
                        Namespaces: funcOrValue.ModuleName,
                        DeclName: funcOrValue.Name);

                if (declarations.ContainsKey(candidate))
                    yield return candidate;
            }

            foreach (var child in EnumerateChildExpressions(current))
                stack.Push(child);
        }
    }

    private static IEnumerable<SyntaxTypes.Expression> EnumerateChildExpressions(
        SyntaxTypes.Expression expression)
    {
        // Every Expression and LetDeclaration variant is listed explicitly so
        // that adding a new variant in the future causes a hard failure here
        // — the C# compiler does not statically check exhaustiveness over an
        // open record hierarchy, so the explicit `default` arm provides that
        // guarantee at runtime instead.
        switch (expression)
        {
            case SyntaxTypes.Expression.Application app:
                foreach (var arg in app.Arguments)
                    yield return arg.Value;

                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                yield return paren.Expression.Value;
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                yield return ifBlock.Condition.Value;
                yield return ifBlock.ThenBlock.Value;
                yield return ifBlock.ElseBlock.Value;
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                yield return caseExpr.CaseBlock.Expression.Value;

                foreach (var caseEntry in caseExpr.CaseBlock.Cases)
                    yield return caseEntry.Expression.Value;

                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
                foreach (var declNode in letExpr.Value.Declarations)
                {
                    switch (declNode.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            yield return letFunc.Function.Declaration.Value.Expression.Value;
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            yield return letDestr.Expression.Value;
                            break;

                        default:
                            throw new System.NotImplementedException(
                                "EnumerateChildExpressions does not handle let declaration variant: " +
                                declNode.Value.GetType().Name);
                    }
                }

                yield return letExpr.Value.Expression.Value;
                break;

            case SyntaxTypes.Expression.LambdaExpression lambda:
                yield return lambda.Lambda.Expression.Value;
                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                    yield return element.Value;

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var element in tupled.Elements)
                    yield return element.Value;

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    yield return field.Value.valueExpr.Value;

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                foreach (var field in recordUpdate.Fields)
                    yield return field.Value.valueExpr.Value;

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                yield return recordAccess.Record.Value;
                break;

            case SyntaxTypes.Expression.Negation negation:
                yield return negation.Expression.Value;
                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                yield return opApp.Left.Value;
                yield return opApp.Right.Value;
                break;

            // Leaf variants with no nested Expression children.
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.Literal:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.Hex:
            case SyntaxTypes.Expression.Floatable:
            case SyntaxTypes.Expression.FunctionOrValue:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                break;

            default:
                throw new System.NotImplementedException(
                    "EnumerateChildExpressions does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }
}
