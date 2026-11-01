using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.Json.Serialization;
using Pine.Core.Json;

namespace Pine.Core.Elm.Syntax;

/// <summary>
/// Range-free addressing within an expression, following ElmSyntax.Path.
/// Parentheses are transparent; record fields use names and occurrence indices rather than source order.
/// Paths must be paired with the CST from which the unlowered AST was derived.
/// </summary>
public sealed record ElmSyntaxExpressionPath(ImmutableArray<ElmSyntaxExpressionPath.Step> Steps)
{
    /// <inheritdoc/>
    public bool Equals(ElmSyntaxExpressionPath? other) =>
        other is not null && Steps.SequenceEqual(other.Steps);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hash = new HashCode();
        foreach (var step in Steps)
            hash.Add(step);
        return hash.ToHashCode();
    }

    /// <summary>One structural edge. This subset addresses expressions, not patterns or type annotations.</summary>
    [JsonConverter(typeof(JsonConverterForChoiceType))]
    public abstract record Step
    {
        /// <summary>Ordered expression child; application child zero is its function.</summary>
        public sealed record Child(int Index) : Step;
        /// <summary>Lambda or let body.</summary>
        public sealed record Body : Step;
        /// <summary>Case branch, followed by Body.</summary>
        public sealed record CaseBranch(int Index) : Step;
        /// <summary>Local declaration, followed by Implementation/Body for functions, or Body for destructuring.</summary>
        public sealed record LetDeclaration(int Index) : Step;
        /// <summary>Local function implementation.</summary>
        public sealed record Implementation : Step;
        /// <summary>Record setter, followed by Child(0) for its value.</summary>
        public sealed record RecordField(string Name, int Occurrence) : Step;
    }

    /// <summary>The root expression.</summary>
    public static ElmSyntaxExpressionPath Root { get; } = new([]);

    /// <summary>Appends structural edges.</summary>
    public ElmSyntaxExpressionPath Append(params Step[] steps) => new(Steps.AddRange(steps));

    /// <summary>
    /// Resolves an AST path against a concrete expression. A mismatched path returns null, never an invented range.
    /// </summary>
    public Concrete.Node<Concrete.Expression>? Resolve(Concrete.Node<Concrete.Expression> root)
    {
        var current = Unwrap(root);

        for (var index = 0; index < Steps.Length; index++)
        {
            var step = Steps[index];
            Concrete.Node<Concrete.Expression>? next;

            switch (step)
            {
                case Step.Child child:
                    next = ChildAt(current.Value, child.Index);
                    break;

                case Step.Body:
                    next = current.Value is Concrete.Expression.LambdaExpression lambda
                        ? lambda.Lambda.Expression
                        : current.Value is Concrete.Expression.LetExpression let ? let.Value.Expression : null;
                    break;

                case Step.CaseBranch branch:
                    if (current.Value is not Concrete.Expression.CaseExpression caseExpression ||
                        branch.Index < 0 || branch.Index >= caseExpression.CaseBlock.Cases.Count ||
                        ++index >= Steps.Length || Steps[index] is not Step.Body)
                        return null;

                    next = caseExpression.CaseBlock.Cases[branch.Index].Expression;
                    break;

                case Step.LetDeclaration declaration:
                    if (current.Value is not Concrete.Expression.LetExpression letExpression ||
                        declaration.Index < 0 || declaration.Index >= letExpression.Value.Declarations.Count)
                        return null;

                    var local = letExpression.Value.Declarations[declaration.Index].Value;
                    switch (local)
                    {
                        case Concrete.Expression.LetDeclaration.LetFunction function:
                            if (++index >= Steps.Length || Steps[index] is not Step.Implementation ||
                                ++index >= Steps.Length || Steps[index] is not Step.Body)
                                return null;
                            next = function.Function.Declaration.Value.Expression;
                            break;
                        case Concrete.Expression.LetDeclaration.LetDestructuring destructuring:
                            if (++index >= Steps.Length || Steps[index] is not Step.Body)
                                return null;
                            next = destructuring.Expression;
                            break;
                        default:
                            throw new NotImplementedException(nameof(Resolve) + " does not handle let variant: " + local.GetType().Name);
                    }
                    break;

                case Step.RecordField field:
                    var fields = RecordFields(current.Value);
                    if (fields is null || field.Occurrence < 0 ||
                        ++index >= Steps.Length || Steps[index] is not Step.Child { Index: 0 })
                        return null;
                    next = fields.Where(item => item.FieldName.Value == field.Name)
                        .Skip(field.Occurrence).FirstOrDefault()?.ValueExpr;
                    break;

                case Step.Implementation:
                    return null;

                default:
                    throw new NotImplementedException(nameof(Resolve) + " does not handle step variant: " + step.GetType().Name);
            }

            if (next is null)
                return null;
            current = Unwrap(next);
        }

        return current;
    }

    private static Concrete.Node<Concrete.Expression> Unwrap(Concrete.Node<Concrete.Expression> node)
    {
        while (node.Value is Concrete.Expression.Parenthesized parenthesized)
            node = parenthesized.Expression;
        return node;
    }

    private static Concrete.Node<Concrete.Expression>? ChildAt(Concrete.Expression expression, int index)
    {
        if (index < 0)
            return null;

        switch (expression)
        {
            case Concrete.Expression.Negation negation:
                return index == 0 ? negation.Expression : null;
            case Concrete.Expression.ListExpr list:
                return index < list.Elements.Count ? list.Elements[index] : null;
            case Concrete.Expression.TupledExpression tuple:
                return index < tuple.Elements.Count ? tuple.Elements[index] : null;
            case Concrete.Expression.IfBlock conditional:
                return index switch { 0 => conditional.Condition, 1 => conditional.ThenBlock, 2 => conditional.ElseBlock, _ => null };
            case Concrete.Expression.Application application:
                return index == 0 ? application.Function : index <= application.Arguments.Count ? application.Arguments[index - 1] : null;
            case Concrete.Expression.OperatorApplication operation:
                return index switch { 0 => operation.Left, 1 => operation.Right, _ => null };
            case Concrete.Expression.RecordAccess access:
                return index == 0 ? access.Record : null;
            case Concrete.Expression.CaseExpression cases:
                return index == 0 ? cases.CaseBlock.Expression : null;
            case Concrete.Expression.Parenthesized parentheses:
                return ChildAt(parentheses.Expression.Value, index);
            case Concrete.Expression.UnitExpr:
            case Concrete.Expression.StringLiteral:
            case Concrete.Expression.MultilineStringLiteral:
            case Concrete.Expression.CharLiteral:
            case Concrete.Expression.IntegerLiteral:
            case Concrete.Expression.FloatLiteral:
            case Concrete.Expression.Identifier:
            case Concrete.Expression.PrefixOperator:
            case Concrete.Expression.LambdaExpression:
            case Concrete.Expression.LetExpression:
            case Concrete.Expression.RecordExpr:
            case Concrete.Expression.RecordAccessFunction:
            case Concrete.Expression.RecordUpdateExpression:
            case Concrete.Expression.GLSLExpression:
                return null;
            default:
                throw new NotImplementedException(nameof(ChildAt) + " does not handle expression variant: " + expression.GetType().Name);
        }
    }

    private static IEnumerable<Concrete.RecordExprField>? RecordFields(Concrete.Expression expression)
    {
        if (expression is Concrete.Expression.RecordExpr record)
            return record.Fields;
        if (expression is Concrete.Expression.RecordUpdateExpression update)
            return update.Fields;
        return null;
    }
}
