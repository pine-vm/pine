using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Closure conversion using lexical identities and owner-local function sites, never synthetic names.</summary>
public static class LambdaLifting
{
    /// <summary>Lifts lambdas and mutually recursive local functions. Already-lifted input is unchanged.</summary>
    public static Program Lift(Program program)
    {
        var functions = program.Functions.ToBuilder();

        ValueList<BindingId> Order(IEnumerable<BindingId> bindings) =>
            new(bindings.Distinct().OrderBy(binding => binding, IdentityComparer.Instance));

        Pattern CapturePattern(ValueList<BindingId> captures) =>
            captures.Count is 1
            ?
            Variable(captures[0])
            :
            new Pattern.TuplePattern(new ValueList<Pattern>(captures.Select(Variable)));

        Expression Closure(FunctionId id, ValueList<BindingId> captures)
        {
            Expression function = new Expression.ReferenceExpression(new Reference.Function(id));

            if (captures.Count is 0)
                return function;

            var values =
                captures.Select(
                    binding => (Expression)new Expression.ReferenceExpression(new Reference.Local(binding)));

            var argument = captures.Count is 1 ? values.Single() : new Expression.List(new(values));
            return new Expression.Application(function, [argument]);
        }

        Expression Substitute(Expression expression, IReadOnlyDictionary<BindingId, Expression> replacements)
        {
            if (expression is Expression.ReferenceExpression { Reference: Reference.Local local } &&
                replacements.TryGetValue(local.Binding, out var replacement))
            {
                return replacement;
            }

            return ExpressionTransform.MapChildren(expression, child => Substitute(child, replacements));
        }

        void Add(FunctionId id, ValueList<Pattern> parameters, Expression body, ValueList<BindingId> captures)
        {
            var liftedParameters =
                captures.Count is 0
                ?
                parameters
                :
                new ValueList<Pattern>(new[] { CapturePattern(captures) }.Concat(parameters));

            if (!functions.TryAdd(id, new(liftedParameters, body)))
                throw new InvalidOperationException("A local function site was emitted more than once.");
        }

        Expression Rewrite(Expression expression)
        {
            if (expression is Expression.Lambda lambda)
            {
                var body = Rewrite(lambda.Body);
                var free = ExpressionTransform.FreeBindings(body);
                free.ExceptWith(lambda.Parameters.SelectMany(ExpressionTransform.PatternBindings));
                var captures = Order(free);
                Add(lambda.Id, lambda.Parameters, body, captures);
                return Closure(lambda.Id, captures);
            }

            if (expression is Expression.Let let)
            {
                var rewritten =
                    let.Bindings.Select(
                        binding => binding with
                        {
                            Value =
                            binding.Value is Expression.Lambda lambda &&
                            WholeValueBindings(binding.Pattern) is { Count: > 0 }
                            ?
                            lambda with { Body = Rewrite(lambda.Body) }
                            :
                            Rewrite(binding.Value)
                        }).ToArray();

                var localFunctions =
                    rewritten.Where(binding => binding.Value is Expression.Lambda)
                    .SelectMany(
                        binding => WholeValueBindings(binding.Pattern)!.Select(
                            id =>
                            KeyValuePair.Create(id, (Expression.Lambda)binding.Value)))
                    .ToDictionary();

                var captures = new Dictionary<BindingId, HashSet<BindingId>>();
                var dependencies = new Dictionary<BindingId, HashSet<BindingId>>();

                foreach (var (id, localFunction) in localFunctions)
                {
                    var free = ExpressionTransform.FreeBindings(localFunction.Body);
                    free.ExceptWith(localFunction.Parameters.SelectMany(ExpressionTransform.PatternBindings));
                    dependencies[id] = [.. free.Where(localFunctions.ContainsKey)];
                    free.ExceptWith(localFunctions.Keys);
                    captures[id] = free;
                }

                bool changed;

                do
                {
                    changed = false;

                    foreach (var (id, called) in dependencies)
                    {
                        var count = captures[id].Count;

                        foreach (var callee in called)
                            captures[id].UnionWith(captures[callee]);

                        changed |= count != captures[id].Count;
                    }
                }
                while (changed);

                var ordered = captures.ToDictionary(entry => entry.Key, entry => Order(entry.Value));

                var replacements =
                    localFunctions.ToDictionary(
                        entry => entry.Key,
                        entry => Closure(entry.Value.Id, ordered[entry.Key]));

                foreach (var group in localFunctions.GroupBy(entry => entry.Value.Id))
                {
                    var (id, localFunction) = group.First();

                    Add(
                        localFunction.Id,
                        localFunction.Parameters,
                        Substitute(localFunction.Body, replacements),
                        ordered[id]);
                }

                var values =
                    new ValueList<Binding>(
                        rewritten.Where(binding => binding.Value is not Expression.Lambda)
                        .Select(binding => binding with { Value = Substitute(binding.Value, replacements) }));

                var body = Substitute(Rewrite(let.Body), replacements);
                return values.Count is 0 ? body : new Expression.Let(values, body);
            }

            return ExpressionTransform.MapChildren(expression, Rewrite);
        }

        foreach (var (id, function) in program.Functions.OrderBy(entry => entry.Key, IdentityComparer.Instance))
            functions[id] = function with { Body = Rewrite(function.Body) };

        return program with { Functions = functions.ToImmutable() };
    }

    private static Pattern Variable(BindingId binding) =>
        new Pattern.VarPattern(binding);

    private static ValueList<BindingId>? WholeValueBindings(Pattern pattern) =>
        pattern switch
        {
            Pattern.VarPattern variable => [variable.Binding],
            Pattern.AllPattern => [],

            Pattern.AsPattern alias =>
            WholeValueBindings(alias.Pattern) is { } inner ? [.. inner, alias.Binding] : null,

            Pattern.UnitPattern => null,
            Pattern.CharPattern => null,
            Pattern.StringPattern => null,
            Pattern.IntPattern => null,
            Pattern.FloatPattern => null,
            Pattern.TuplePattern => null,
            Pattern.ListPattern => null,
            Pattern.UnConsPattern => null,
            Pattern.NamedPattern => null,

            _ =>
            throw new NotImplementedException(
                $"{nameof(WholeValueBindings)} does not handle pattern variant: {pattern.GetType().Name}")
        };
}
