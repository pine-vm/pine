using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Pure syntax transformations that operate on the <see cref="ElmSyntax.ElmSyntaxAbstract"/>
/// model. This is the abstract-syntax counterpart of the concrete-model
/// <see cref="ElmSyntaxTransformations"/>: as the lowering pipeline that feeds
/// <see cref="OptimizedElmSyntaxDeclarations"/> migrates from the concrete
/// <see cref="ElmSyntax.Stil4mElmSyntax7"/> model to the abstract model, each transformation
/// module reaches for the helpers here instead of round-tripping through the concrete model.
/// <para>
/// The abstract model carries no source ranges, no trivia (comments/documentation) and no
/// redundant parentheses, so the abstract helpers are typically simpler than their concrete
/// analogues: there are no <c>Node&lt;T&gt;</c> wrappers to thread and no
/// <c>ParenthesizedExpression</c> layers to peel.
/// </para>
/// </summary>
internal static class ElmSyntaxAbstractTransformations
{
    /// <summary>
    /// Rebuilds an expression by applying <paramref name="mapChild"/> to all immediate child
    /// expression nodes. This centralizes the expression-variant reconstruction pattern for
    /// tree-mapping operations (substitution, rewriting, flattening). Leaf expressions
    /// (<see cref="SyntaxTypes.Expression.FunctionOrValue"/>, literals, etc.) are returned
    /// unchanged.
    /// </summary>
    public static SyntaxTypes.Expression MapChildExpressions(
        SyntaxTypes.Expression expr,
        Func<SyntaxTypes.Expression, SyntaxTypes.Expression> mapChild)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.Application app:
                return
                    new SyntaxTypes.Expression.Application(
                        mapChild(app.Function),
                        [.. app.Arguments.Select(mapChild)]);

            case SyntaxTypes.Expression.IfBlock ifBlock:
                return
                    new SyntaxTypes.Expression.IfBlock(
                        mapChild(ifBlock.Condition),
                        mapChild(ifBlock.ThenBlock),
                        mapChild(ifBlock.ElseBlock));

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                return
                    new SyntaxTypes.Expression.CaseExpression(
                        mapChild(caseExpr.Expression),
                        [
                        .. caseExpr.Cases.Select(
                            c => new SyntaxTypes.Case(c.Pattern, mapChild(c.Expression)))
                        ]);

            case SyntaxTypes.Expression.LetExpression letExpr:
                return
                    new SyntaxTypes.Expression.LetExpression(
                        [
                        .. letExpr.Declarations.Select(
                            d => d switch
                            {
                                SyntaxTypes.LetDeclaration.LetFunction letFunc =>
                                new SyntaxTypes.LetDeclaration.LetFunction(
                                    letFunc.Function with
                                    {
                                        Declaration =
                                        letFunc.Function.Declaration with
                                        {
                                            Expression = mapChild(letFunc.Function.Declaration.Expression)
                                        }
                                    }),

                                SyntaxTypes.LetDeclaration.LetDestructuring letDestr =>
                                new SyntaxTypes.LetDeclaration.LetDestructuring(
                                    letDestr.Pattern,
                                    mapChild(letDestr.Expression)),

                                _ =>
                                d
                            })
                        ],
                        mapChild(letExpr.Expression));

            case SyntaxTypes.Expression.LambdaExpression lambda:
                return
                    new SyntaxTypes.Expression.LambdaExpression(
                        lambda.Arguments,
                        mapChild(lambda.Expression));

            case SyntaxTypes.Expression.ListExpr listExpr:
                return
                    new SyntaxTypes.Expression.ListExpr(
                        [.. listExpr.Elements.Select(mapChild)]);

            case SyntaxTypes.Expression.TupledExpression tupled:
                return
                    new SyntaxTypes.Expression.TupledExpression(
                        [.. tupled.Elements.Select(mapChild)]);

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                return
                    new SyntaxTypes.Expression.RecordExpr(
                        [
                        .. recordExpr.Fields.Select(
                            f => f with { Value = mapChild(f.Value) })
                        ]);

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                return
                    new SyntaxTypes.Expression.RecordUpdateExpression(
                        recordUpdate.RecordName,
                        [
                        .. recordUpdate.Fields.Select(
                            f => f with { Value = mapChild(f.Value) })
                        ]);

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                return recordAccess with { Record = mapChild(recordAccess.Record) };

            case SyntaxTypes.Expression.Negation negation:
                return new SyntaxTypes.Expression.Negation(mapChild(negation.Expression));

            case SyntaxTypes.Expression.OperatorApplication opApp:
                return
                    new SyntaxTypes.Expression.OperatorApplication(
                        opApp.Operator,
                        opApp.Direction,
                        mapChild(opApp.Left),
                        mapChild(opApp.Right));

            // Leaf expression variants have no child expressions to map; return them
            // unchanged. They are listed explicitly so that the throwing default below never
            // fires for a valid expression value.
            case SyntaxTypes.Expression.UnitExpr:
            case SyntaxTypes.Expression.StringLiteral:
            case SyntaxTypes.Expression.CharLiteral:
            case SyntaxTypes.Expression.Integer:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.FunctionOrValue:
            case SyntaxTypes.Expression.PrefixOperator:
            case SyntaxTypes.Expression.RecordAccessFunction:
            case SyntaxTypes.Expression.GLSLExpression:
                return expr;

            default:
                throw new NotImplementedException(
                    "MapChildExpressions does not handle expression variant: " + expr.GetType().Name);
        }
    }

    /// <summary>
    /// Recursively walks the expression tree and replaces every
    /// <see cref="SyntaxTypes.Expression.Application"/> whose head
    /// (<see cref="SyntaxTypes.Expression.Application.Function"/>) is itself an
    /// <see cref="SyntaxTypes.Expression.Application"/> with the equivalent flat form.
    /// <para>
    /// This is the abstract-model counterpart of the (former) concrete-model
    /// <c>FlattenAllNestedApplicationHeads</c>. In the concrete
    /// model an <c>Application</c> is a single flat argument list whose head is element 0; in
    /// the abstract model the head is a dedicated <see cref="SyntaxTypes.Expression.Application.Function"/>
    /// field, so a "nested head" is an <c>Application</c> whose <c>Function</c> is another
    /// <c>Application</c>. Flattening lets
    /// <see cref="ExpressionCompiler.CompileApplication"/> take the direct-call fast path
    /// instead of allocating a closure for a partial application.
    /// </para>
    /// </summary>
    public static SyntaxTypes.Expression FlattenAllNestedApplicationHeads(
        SyntaxTypes.Expression expr)
    {
        var withChildrenFlattened = MapChildExpressions(expr, FlattenAllNestedApplicationHeads);

        if (withChildrenFlattened is SyntaxTypes.Expression.Application app &&
            app.Function is SyntaxTypes.Expression.Application innerApp)
        {
            // The child pass already flattened innerApp, so innerApp.Function is guaranteed
            // not to be an Application. A single splice therefore yields the flat form.
            var combinedArguments =
                new List<SyntaxTypes.Expression>(innerApp.Arguments.Count + app.Arguments.Count);

            combinedArguments.AddRange(innerApp.Arguments);
            combinedArguments.AddRange(app.Arguments);

            return new SyntaxTypes.Expression.Application(innerApp.Function, combinedArguments);
        }

        return withChildrenFlattened;
    }

    /// <summary>
    /// A constructor (tag) application deconstructed into its qualified constructor
    /// name and argument (field) expressions. Abstract-model counterpart of the
    /// concrete <c>ElmSyntaxTransformations.ConstructorApplication</c>. The constructor
    /// name is a <see cref="DeclQualifiedName"/> (as carried by
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue.QualifiedName"/>).
    /// </summary>
    internal sealed record ConstructorApplication(
        DeclQualifiedName ConstructorName,
        IReadOnlyList<SyntaxTypes.Expression> FieldExpressions);

    /// <summary>
    /// Attempts to view an expression as a constructor (tag) application: either a bare
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/> (zero-arity constructor) or an
    /// <see cref="SyntaxTypes.Expression.Application"/> whose head function is a
    /// <see cref="SyntaxTypes.Expression.FunctionOrValue"/>. Returns <see langword="null"/>
    /// for any other shape. Abstract-model counterpart of the concrete
    /// <c>ElmSyntaxTransformations.TryDeconstructConstructorApplication</c>; the abstract
    /// model has no parentheses to unwrap and already separates the application head from
    /// its arguments.
    /// </summary>
    public static ConstructorApplication? TryDeconstructConstructorApplication(
        SyntaxTypes.Expression expr)
    {
        switch (expr)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrValue:
                return new ConstructorApplication(funcOrValue.QualifiedName, []);

            case SyntaxTypes.Expression.Application app
            when app.Function is SyntaxTypes.Expression.FunctionOrValue constructorRef:
                return new ConstructorApplication(constructorRef.QualifiedName, app.Arguments);

            default:
                return null;
        }
    }

    /// <summary>
    /// Determines whether a constructor reference in a pattern
    /// (<see cref="SyntaxTypes.QualifiedNameRef"/>) is equivalent to a constructor name
    /// carried by a value expression (<see cref="DeclQualifiedName"/>). Two names are
    /// equivalent when the local declaration names match and either the pattern reference is
    /// unqualified or the module paths are identical. Abstract-model counterpart of the
    /// concrete <c>ElmSyntaxTransformations.AreEquivalentConstructorNames</c> overload.
    /// </summary>
    public static bool AreEquivalentConstructorNames(
        SyntaxTypes.QualifiedNameRef left,
        DeclQualifiedName right) =>
        left.Name == right.DeclName &&
        (left.ModuleName.Count is 0 || left.ModuleName.SequenceEqual(right.Namespaces));

    /// <summary>
    /// Resolves a <see cref="SyntaxTypes.Expression.FunctionOrValue"/> reference into a
    /// fully-qualified name. References without an explicit module qualifier (empty
    /// <see cref="DeclQualifiedName.Namespaces"/>) are interpreted as belonging to the
    /// declaring module <paramref name="currentModuleName"/>. Abstract-model counterpart of
    /// the concrete <c>ElmSyntaxTransformations.ResolveReference</c>.
    /// </summary>
    public static DeclQualifiedName ResolveReference(
        SyntaxTypes.Expression.FunctionOrValue reference,
        IReadOnlyList<string> currentModuleName) =>
        reference.QualifiedName.Namespaces.Count is 0
        ?
        DeclQualifiedName.Create(currentModuleName, reference.QualifiedName.DeclName)
        :
        reference.QualifiedName;

    /// <summary>
    /// Resolves a <see cref="SyntaxTypes.QualifiedNameRef"/> (e.g. a constructor name
    /// appearing in a pattern or constructor application) into a fully-qualified name.
    /// References without an explicit module qualifier are interpreted as belonging to the
    /// declaring module <paramref name="currentModuleName"/>. Abstract-model counterpart of
    /// the concrete <c>ElmSyntaxTransformations.ResolveReference</c> overload.
    /// </summary>
    public static DeclQualifiedName ResolveReference(
        SyntaxTypes.QualifiedNameRef qname,
        IReadOnlyList<string> currentModuleName) =>
        qname.ModuleName.Count is 0
        ?
        DeclQualifiedName.Create(currentModuleName, qname.Name)
        :
        DeclQualifiedName.Create(qname.ModuleName, qname.Name);
}
