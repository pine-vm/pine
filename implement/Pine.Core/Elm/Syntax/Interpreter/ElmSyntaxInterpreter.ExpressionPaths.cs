using System;
using System.Collections.Generic;
using System.Linq;

using Step = Pine.Core.Elm.Syntax.ElmSyntaxExpressionPath.Step;

namespace Pine.Core.Elm.Syntax.Interpreter;

public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Enumerates range-free paths in prepared syntax. Only valid for preparation without
    /// operator lowering or closed-expression reduction; those transformations change node identities.
    /// Case dispatch grouping preserves branch order and is transparent to these paths.
    /// </summary>
    public static IEnumerable<(PreparedExpression Expression, ElmSyntaxExpressionPath Path)> EnumerateExpressionPaths(
        PreparedExpression expression,
        ElmSyntaxExpressionPath? path = null)
    {
        path ??= ElmSyntaxExpressionPath.Root;
        yield return (expression, path);

        IEnumerable<(PreparedExpression Expression, ElmSyntaxExpressionPath Path)> children;

        switch (expression)
        {
            case PreparedExpression.Negation negation:
                children = [(negation.Expression, path.Append(new Step.Child(0)))];
                break;

            case PreparedExpression.ListExpr list:
                children = Indexed(list.Elements);
                break;

            case PreparedExpression.TupledExpression tuple:
                children = Indexed(tuple.Elements);
                break;

            case PreparedExpression.IfBlock conditional:
                children = Indexed([conditional.Condition, conditional.ThenBlock, conditional.ElseBlock]);
                break;

            case PreparedExpression.Application application:
                children = Indexed([application.Function, .. application.Arguments]);
                break;

            case PreparedExpression.OperatorApplication operation:
                children = Indexed([operation.Left, operation.Right]);
                break;

            case PreparedExpression.LambdaExpression lambda:
                children = [(lambda.Expression, path.Append(new Step.Body()))];
                break;

            case PreparedExpression.CaseExpression cases:
                children =
                    CaseBodies(cases.Dispatch).Select(
                        (body, index) => (body, path.Append(new Step.CaseBranch(index), new Step.Body())))
                    .Prepend((cases.Expression, path.Append(new Step.Child(0))));

                break;

            case PreparedExpression.LetExpression let:
                children =
                    let.Declarations.Select(
                        (declaration, index) => LetBody(declaration, path.Append(new Step.LetDeclaration(index))))
                    .Append((let.Expression, path.Append(new Step.Body())));

                break;

            case PreparedExpression.RecordExpr record:
                children = Fields(record.Fields);
                break;

            case PreparedExpression.RecordUpdateExpression update:
                children = Fields(update.Fields);
                break;

            case PreparedExpression.RecordAccess access:
                children = [(access.Record, path.Append(new Step.Child(0)))];
                break;

            case PreparedExpression.ValueLiteral:
            case PreparedExpression.Identifier:
            case PreparedExpression.PrefixOperator:
            case PreparedExpression.RecordAccessFunction:
            case PreparedExpression.GLSLExpression:
                children = [];
                break;

            default:
                throw new NotImplementedException(
                    nameof(EnumerateExpressionPaths) +
                    " does not handle expression variant: " + expression.GetType().Name);
        }

        foreach (var (child, childPath) in children)
        {
            foreach (var descendant in EnumerateExpressionPaths(child, childPath))
                yield return descendant;
        }

        IEnumerable<(PreparedExpression, ElmSyntaxExpressionPath)> Indexed(
            IReadOnlyList<PreparedExpression> expressions) =>
            expressions.Select((child, index) => (child, path.Append(new Step.Child(index))));

        IEnumerable<(PreparedExpression, ElmSyntaxExpressionPath)> Fields(IReadOnlyList<PreparedRecordSetter> fields)
        {
            var occurrences = new Dictionary<string, int>(StringComparer.Ordinal);

            foreach (var field in fields)
            {
                var occurrence = occurrences.GetValueOrDefault(field.FieldName);
                occurrences[field.FieldName] = occurrence + 1;
                yield return (field.Value, path.Append(new Step.RecordField(field.FieldName, occurrence), new Step.Child(0)));
            }
        }
    }

    private static (PreparedExpression, ElmSyntaxExpressionPath) LetBody(
        PreparedLetDeclaration declaration, ElmSyntaxExpressionPath path) =>
        declaration switch
        {
            PreparedLetDeclaration.LetFunction function =>
            (function.Function.Declaration.Expression, path.Append(new Step.Implementation(), new Step.Body())),

            PreparedLetDeclaration.LetDestructuring destructuring =>
            (destructuring.Expression, path.Append(new Step.Body())),

            _ =>
            throw new NotImplementedException(nameof(LetBody) + " does not handle let variant: " + declaration.GetType().Name)
        };

    private static IEnumerable<PreparedExpression> CaseBodies(PreparedCaseDispatch dispatch)
    {
        foreach (var segment in dispatch.Steps)
        {
            switch (segment)
            {
                case PreparedCaseDispatchSegment.ConstantValuePatterns constants:
                    foreach (var item in constants.Cases)
                        yield return item.Expression;

                    break;

                case PreparedCaseDispatchSegment.PatternCase pattern:
                    yield return pattern.Expression;
                    break;

                case PreparedCaseDispatchSegment.DiscardCase discard:
                    yield return discard.Expression;
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(CaseBodies) + " does not handle dispatch variant: " + segment.GetType().Name);
            }
        }
    }
}
