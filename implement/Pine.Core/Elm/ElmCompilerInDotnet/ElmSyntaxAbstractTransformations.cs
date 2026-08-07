using System;
using System.Collections.Generic;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Syntax transformations that operate on the <see cref="ElmSyntax.ElmSyntaxAbstract"/> model.
/// </summary>
internal static class ElmSyntaxAbstractTransformations
{
    /// <summary>
    /// Rebuilds an expression by applying <paramref name="mapChild"/> to all immediate child
    /// expression nodes. This centralizes the expression-variant reconstruction pattern for
    /// tree-mapping operations (substitution, rewriting, flattening). Leaf expressions
    /// (<see cref="SyntaxTypes.Expression.Identifier"/>, literals, etc.) are returned
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
            case SyntaxTypes.Expression.IntegerLiteral:
            case SyntaxTypes.Expression.FloatLiteral:
            case SyntaxTypes.Expression.Identifier:
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
    /// Flattening lets
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
}
