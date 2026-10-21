using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

/// <summary>Traversal over resolved emission expressions.</summary>
public static class ExpressionTraversal
{
    /// <summary>Enumerates a node and its descendants.</summary>
    public static IEnumerable<Expression> Enumerate(Expression expression)
    {
        yield return expression;

        foreach (var child in Children(expression))
        {
            foreach (var descendant in Enumerate(child))
                yield return descendant;
        }
    }

    /// <summary>Enumerates immediate children.</summary>
    public static IEnumerable<Expression> Children(Expression expression) =>
        expression switch
        {
            Expression.Negation negation => [negation.Expression],
            Expression.ListExpr list => list.Elements,
            Expression.IfBlock conditional => [conditional.Condition, conditional.ThenBlock, conditional.ElseBlock],
            Expression.Application application => new[] { application.Function }.Concat(application.Arguments),
            Expression.OperatorApplication operation => [operation.Left, operation.Right],
            Expression.ProjectItem projection => [projection.Value],

            Expression.CaseExpression cases =>
            new[] { cases.Expression }.Concat(cases.Cases.Select(branch => branch.Expression)),

            Expression.LetExpression let =>
            let.Declarations.Select(binding => binding.Expression).Append(let.Expression),

            Expression.RecordExpr record => record.Fields.Select(field => field.Value),
            Expression.RecordAccess access => [access.Record],

            Expression.RecordUpdateExpression update =>
            new[] { update.Receiver }.Concat(update.Fields.Select(field => field.Value)),

            Expression.LiteralValue => [],
            Expression.Identifier => [],
            Expression.PrefixOperator => [],
            Expression.RecordAccessFunction => [],
            Expression.GLSLExpression => [],

            _ =>
            throw new NotImplementedException(
                $"{nameof(Children)} does not handle expression variant: {expression.GetType().Name}")
        };
}
