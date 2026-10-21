using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Structural traversal and hygienic substitution for compiler-owned expressions.</summary>
public static class ExpressionTransform
{
    /// <summary>Enumerates immediate children without rebuilding expression nodes.</summary>
    public static IEnumerable<Expression> Children(Expression expression) =>
        expression switch
        {
            Expression.List list => list.Items,
            Expression.Conditional conditional => [conditional.Condition, conditional.Then, conditional.Else],
            Expression.Application application => new[] { application.Function }.Concat(application.Arguments),
            Expression.Lambda lambda => [lambda.Body],
            Expression.Let let => let.Bindings.Select(binding => binding.Value).Append(let.Body),
            Expression.Match match => new[] { match.Value }.Concat(match.Branches.Select(branch => branch.Body)),
            Expression.Record record => record.Fields.Select(field => field.Value),
            Expression.RecordAccess access => [access.Value],

            Expression.RecordUpdate update =>
            new[] { update.Receiver }.Concat(update.Fields.Select(field => field.Value)),

            Expression.Project project => [project.Value],
            Expression.Operator operation => [operation.Left, operation.Right],
            Expression.Negate negate => [negate.Value],
            Expression.Literal => [],
            Expression.ReferenceExpression => [],
            Expression.RecordAccessor => [],
            Expression.PrefixOperator => [],
            Expression.Shader => [],

            _ =>
            throw new NotImplementedException(
                $"{nameof(Children)} does not handle expression variant: {expression.GetType().Name}")
        };

    /// <summary>Maps immediate children, without changing identities or lexical binders.</summary>
    public static Expression MapChildren(Expression expression, Func<Expression, Expression> map) =>
        expression switch
        {
            Expression.List list => list with { Items = new(list.Items.Select(map)) },

            Expression.Conditional conditional =>
            conditional with
            { Condition = map(conditional.Condition), Then = map(conditional.Then), Else = map(conditional.Else) },

            Expression.Application application =>
            application with
            { Function = map(application.Function), Arguments = new(application.Arguments.Select(map)) },

            Expression.Lambda lambda => lambda with { Body = map(lambda.Body) },

            Expression.Let let =>
            let with
            { Bindings = new(let.Bindings.Select(binding => binding with { Value = map(binding.Value) })), Body = map(let.Body) },

            Expression.Match match =>
            match with
            { Value = map(match.Value), Branches = new(match.Branches.Select(branch => branch with { Body = map(branch.Body) })) },

            Expression.Record record =>
            record with { Fields = new(record.Fields.Select(field => field with { Value = map(field.Value) })) },

            Expression.RecordAccess access => access with { Value = map(access.Value) },

            Expression.RecordUpdate update =>
            new Expression.RecordUpdate(
                map(update.Receiver),
                new(update.Fields.Select(field => field with { Value = map(field.Value) }))),

            Expression.Project project => new Expression.Project(map(project.Value), project.Index),

            Expression.Operator operation =>
            operation with { Left = map(operation.Left), Right = map(operation.Right) },

            Expression.Negate negation => negation with { Value = map(negation.Value) },
            Expression.Literal => expression,
            Expression.ReferenceExpression => expression,
            Expression.RecordAccessor => expression,
            Expression.PrefixOperator => expression,
            Expression.Shader => expression,

            _ =>
            throw new NotImplementedException(
                $"{nameof(MapChildren)} does not handle expression variant: {expression.GetType().Name}")
        };

    /// <summary>Collects lexical identities bound by a pattern.</summary>
    public static IEnumerable<BindingId> PatternBindings(Pattern pattern) =>
        pattern switch
        {
            Pattern.VarPattern variable => [variable.Binding],
            Pattern.AsPattern alias => PatternBindings(alias.Pattern).Append(alias.Binding),
            Pattern.TuplePattern tuple => tuple.Elements.SelectMany(PatternBindings),
            Pattern.ListPattern list => list.Elements.SelectMany(PatternBindings),
            Pattern.NamedPattern named => named.Arguments.SelectMany(PatternBindings),
            Pattern.UnConsPattern cons => PatternBindings(cons.Head).Concat(PatternBindings(cons.Tail)),
            Pattern.AllPattern => [],
            Pattern.UnitPattern => [],
            Pattern.CharPattern => [],
            Pattern.StringPattern => [],
            Pattern.IntPattern => [],
            Pattern.FloatPattern => [],

            _ =>
            throw new NotImplementedException(
                $"{nameof(PatternBindings)} does not handle pattern variant: {pattern.GetType().Name}")
        };

    /// <summary>Free lexical references, including record-update receivers.</summary>
    public static HashSet<BindingId> FreeBindings(Expression expression)
    {
        var free = new HashSet<BindingId>();

        switch (expression)
        {
            case Expression.ReferenceExpression reference:
                switch (reference.Reference)
                {
                    case Reference.Local local:
                        free.Add(local.Binding);
                        break;

                    case Reference.Function:
                    case Reference.Constructor:
                    case Reference.Native:
                        break;

                    default:
                        throw new NotImplementedException(
                            $"{nameof(FreeBindings)} does not handle reference variant: {reference.Reference.GetType().Name}");
                }

                break;

            case Expression.RecordUpdate update:
                foreach (var child in Children(expression))
                    free.UnionWith(FreeBindings(child));

                break;

            case Expression.Lambda lambda:
                free.UnionWith(FreeBindings(lambda.Body));
                free.ExceptWith(lambda.Parameters.SelectMany(PatternBindings));
                break;

            case Expression.Let let:
                free = FreeBindings(let.Body);

                foreach (var binding in let.Bindings)
                {
                    var bindingFree = FreeBindings(binding.Value);
                    free.UnionWith(bindingFree);
                }

                free.ExceptWith(let.Bindings.SelectMany(binding => PatternBindings(binding.Pattern)));
                break;

            case Expression.Match match:
                free = FreeBindings(match.Value);

                foreach (var branch in match.Branches)
                {
                    var branchFree = FreeBindings(branch.Body);
                    branchFree.ExceptWith(PatternBindings(branch.Pattern));
                    free.UnionWith(branchFree);
                }

                break;

            case Expression.Literal:
            case Expression.List:
            case Expression.Conditional:
            case Expression.Application:
            case Expression.Record:
            case Expression.RecordAccess:
            case Expression.RecordAccessor:
            case Expression.Project:
            case Expression.Operator:
            case Expression.PrefixOperator:
            case Expression.Negate:
            case Expression.Shader:
                foreach (var child in Children(expression))
                    free.UnionWith(FreeBindings(child));

                break;

            default:
                throw new NotImplementedException(
                    $"{nameof(FreeBindings)} does not handle expression variant: {expression.GetType().Name}");
        }

        return free;
    }
}
