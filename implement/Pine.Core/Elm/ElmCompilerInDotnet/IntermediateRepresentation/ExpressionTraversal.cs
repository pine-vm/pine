using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

/// <summary>Enumerates IR expressions, including reads introduced by lowering.</summary>
public static class ExpressionTraversal
{
    /// <summary>Enumerates an expression and all its descendants in source order.</summary>
    public static IEnumerable<Expression> Enumerate(Expression expression)
    {
        yield return expression;

        foreach (var child in Children(expression))
        {
            foreach (var descendant in Enumerate(child))
                yield return descendant;
        }
    }

    private static IEnumerable<Expression> Children(Expression expression) =>
        expression switch
        {
            Expression.Negation negation => [negation.Expression],
            Expression.ListExpr list => list.Elements,
            Expression.IfBlock conditional => [conditional.Condition, conditional.ThenBlock, conditional.ElseBlock],
            Expression.Application application => new[] { application.Function }.Concat(application.Arguments),
            Expression.OperatorApplication operation => [operation.Left, operation.Right],
            Expression.TupledExpression tuple => tuple.Elements,
            Expression.ProjectItem projection => [projection.Value],
            Expression.LambdaExpression lambda => [lambda.Expression],

            Expression.CaseExpression cases =>
            new[] { cases.Expression }.Concat(cases.Cases.Select(branch => branch.Expression)),

            Expression.LetExpression let => let.Declarations.Select(LetBody).Append(let.Expression),
            Expression.RecordExpr record => record.Fields.Select(field => field.Value),
            Expression.RecordAccess access => [access.Record],
            Expression.RecordUpdateExpression update => update.Fields.Select(field => field.Value),
            Expression.UnitExpr => [],
            Expression.StringLiteral => [],
            Expression.CharLiteral => [],
            Expression.IntegerLiteral => [],
            Expression.FloatLiteral => [],
            Expression.Identifier => [],
            Expression.PrefixOperator => [],
            Expression.RecordAccessFunction => [],
            Expression.GLSLExpression => [],

            _ =>
            throw new NotImplementedException(
                $"{nameof(Children)} does not handle expression variant: {expression.GetType().Name}")
        };

    private static Expression LetBody(LetDeclaration declaration) =>
        declaration switch
        {
            LetDeclaration.LetFunction function => function.Function.Declaration.Expression,
            LetDeclaration.LetDestructuring binding => binding.Expression,

            _ =>
            throw new NotImplementedException(
                $"{nameof(LetBody)} does not handle let declaration variant: {declaration.GetType().Name}")
        };
}
