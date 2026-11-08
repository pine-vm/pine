using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>Pure identity rebinding, with no source resolution or semantic lowering on cache hits.</summary>
internal sealed class SourceLoweringRebinding(Func<DeclQualifiedName, DeclQualifiedName> map)
{
    private readonly Dictionary<Model.FunctionId, Model.FunctionId> functions = [];

    private readonly Dictionary<Model.BindingId, Model.BindingId> bindings = [];

    public LoweredSourceFunction Apply(LoweredSourceFunction source)
    {
        var evidence =
            ImmutableDictionary.CreateBuilder<Model.Expression, GenericRecordAccessEvidence>(
                ReferenceEqualityComparer.Instance);

        Model.Expression Expression(Model.Expression expression)
        {
            var children = Model.ExpressionTransform.MapChildren(expression, Expression);

            var result =
                children switch
                {
                    Model.Expression.ReferenceExpression reference =>
                    new Model.Expression.ReferenceExpression(
                        reference.Reference switch
                        {
                            Model.Reference.Local local => new Model.Reference.Local(Binding(local.Binding)),
                            Model.Reference.Function function => new Model.Reference.Function(Function(function.Id)),

                            Model.Reference.Constructor constructor =>
                            new Model.Reference.Constructor(map(constructor.Name)),

                            Model.Reference.Native native => new Model.Reference.Native(map(native.Name)),

                            _ =>
                            throw new NotImplementedException(
                                nameof(Apply) + " does not handle reference variant: " +
                                reference.Reference.GetType().Name)
                        }),

                    Model.Expression.Lambda lambda =>
                    new Model.Expression.Lambda(
                        Function(lambda.Id),
                        new(lambda.Parameters.Select(Pattern)),
                        lambda.Body),

                    Model.Expression.Let let =>
                    new Model.Expression.Let(
                        new(let.Bindings.Select(binding => new Model.Binding(Pattern(binding.Pattern), binding.Value))),
                        let.Body),

                    Model.Expression.Match match =>
                    new Model.Expression.Match(
                        match.Value,
                        new(
                            match.Branches.Select(branch => new Model.Branch(Pattern(branch.Pattern), branch.Body)))),

                    Model.Expression.Operator operation => operation with { Name = MapOperator(operation.Name) },
                    Model.Expression.PrefixOperator operation => operation with { Name = MapOperator(operation.Name) },
                    Model.Expression.Literal => children,
                    Model.Expression.List => children,
                    Model.Expression.Conditional => children,
                    Model.Expression.Application => children,
                    Model.Expression.Record => children,
                    Model.Expression.RecordAccess => children,
                    Model.Expression.RecordAccessor => children,
                    Model.Expression.RecordUpdate => children,
                    Model.Expression.Project => children,
                    Model.Expression.Negate => children,
                    Model.Expression.Shader => children,

                    _ =>
                    throw new NotImplementedException(
                        nameof(Apply) + " does not handle expression variant: " + expression.GetType().Name)
                };

            if (source.Evidence.TryGetValue(expression, out var item))
            {
                evidence.Add(
                    result,
                    item with { SubjectType = SourceLoweringProjection.MapType(item.SubjectType, map) });
            }

            return result;
        }

        var function =
            new Model.Function(new(source.Function.Parameters.Select(Pattern)), Expression(source.Function.Body));

        return
            new(
                function,
                source.BindingNames.ToImmutableDictionary(entry => Binding(entry.Key), entry => entry.Value),
                evidence.ToImmutable());
    }

    private string MapOperator(string name) => map(DeclQualifiedName.FromString(name)).FullName;

    private Model.FunctionId Function(Model.FunctionId id)
    {
        if (functions.TryGetValue(id, out var previous))
            return previous;

        var result =
            id switch
            {
                Model.FunctionId.Source source => new Model.FunctionId.Source(map(source.Name)),
                Model.FunctionId.Lifted lifted => new Model.FunctionId.Lifted(Function(lifted.Owner), lifted.Site),

                Model.FunctionId.Specialized specialized =>
                Model.FunctionId.Specialized.Create(
                    Function(specialized.Original),
                    specialized.Constraints.Select(
                        constraint =>
                        constraint switch
                        {
                            Model.ParameterConstraint.KnownFunction known =>
                            new Model.ParameterConstraint.KnownFunction(
                                known.Parameter,
                                Function(known.Function)),

                            Model.ParameterConstraint.ClosedRecord => constraint,

                            _ =>
                            throw new NotImplementedException(
                                nameof(Function) + " does not handle constraint variant: " + constraint.GetType().Name)
                        })),

                _ =>
                throw new NotImplementedException(
                    nameof(Function) + " does not handle function identity variant: " + id.GetType().Name)
            };

        functions.Add(id, result);
        return result;
    }

    private Model.BindingId Binding(Model.BindingId id)
    {
        if (bindings.TryGetValue(id, out var previous))
            return previous;

        Model.BindingId result =
            id switch
            {
                Model.BindingId.Local local => new Model.BindingId.Local(Function(local.Owner), local.Slot),

                Model.BindingId.Temporary temporary =>
                new Model.BindingId.Temporary(Function(temporary.Owner), temporary.Slot),

                _ =>
                throw new NotImplementedException(
                    nameof(Binding) + " does not handle binding identity variant: " + id.GetType().Name)
            };

        bindings.Add(id, result);
        return result;
    }

    private Model.Pattern Pattern(Model.Pattern pattern) =>
        pattern switch
        {
            Model.Pattern.VarPattern variable => new Model.Pattern.VarPattern(Binding(variable.Binding)),

            Model.Pattern.AsPattern alias =>
            new Model.Pattern.AsPattern(Pattern(alias.Pattern), Binding(alias.Binding)),

            Model.Pattern.NamedPattern named =>
            new Model.Pattern.NamedPattern(map(named.Name), new(named.Arguments.Select(Pattern))),

            Model.Pattern.TuplePattern tuple => new Model.Pattern.TuplePattern(new(tuple.Elements.Select(Pattern))),
            Model.Pattern.ListPattern list => new Model.Pattern.ListPattern(new(list.Elements.Select(Pattern))),
            Model.Pattern.UnConsPattern cons => new Model.Pattern.UnConsPattern(Pattern(cons.Head), Pattern(cons.Tail)),
            Model.Pattern.AllPattern => pattern,
            Model.Pattern.UnitPattern => pattern,
            Model.Pattern.CharPattern => pattern,
            Model.Pattern.StringPattern => pattern,
            Model.Pattern.IntPattern => pattern,
            Model.Pattern.FloatPattern => pattern,

            _ =>
            throw new NotImplementedException(
                nameof(Pattern) + " does not handle pattern variant: " + pattern.GetType().Name)
        };
}
