using AwesomeAssertions;
using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Xunit;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class LambdaLiftingCacheTests
{
    private sealed class TrackingCache : ICache<Model.LambdaLiftingInput, Model.LambdaLiftingResult>
    {
        private readonly MemoryCache<Model.LambdaLiftingInput, Model.LambdaLiftingResult, long> _cache = new(0);

        public List<Model.LambdaLiftingInput> Hits { get; } = [];

        public List<Model.LambdaLiftingInput> Misses { get; } = [];

        public bool TryGetValue(
            Model.LambdaLiftingInput key,
            [MaybeNullWhen(false)] out Model.LambdaLiftingResult value)
        {
            var hit = _cache.TryGetValue(key, out value);
            (hit ? Hits : Misses).Add(key);
            return hit;
        }

        public void Set(Model.LambdaLiftingInput key, Model.LambdaLiftingResult value) =>
            _cache.Set(key, value);
    }

    private static Model.FunctionId Owner(string name) =>
        new Model.FunctionId.Source(DeclQualifiedName.Create(["Test"], name));

    private static Model.Expression Local(Model.BindingId binding) =>
        new Model.Expression.ReferenceExpression(new Model.Reference.Local(binding));

    private static Model.Program Input(string name = "root", int version = 1, string spelling = "capture")
    {
        var owner = Owner(name);
        var capture = new Model.BindingId.Local(owner, 0);
        var argument = new Model.BindingId.Local(owner, 1);
        var innerArgument = new Model.BindingId.Local(owner, 2);

        var function =
            new Model.Function(
                [new Model.Pattern.VarPattern(capture)],
                new Model.Expression.Lambda(
                    new Model.FunctionId.Lifted(owner, 0),
                    [new Model.Pattern.VarPattern(argument)],
                    new Model.Expression.List(
                        [
                            Local(capture),
                            Local(argument),
                            new Model.Expression.Literal(IntegerEncoding.EncodeSignedInteger(version)),
                            new Model.Expression.Lambda(
                                new Model.FunctionId.Lifted(owner, 1),
                                [new Model.Pattern.VarPattern(innerArgument)],
                                new Model.Expression.List([Local(capture), Local(argument), Local(innerArgument)]))
                        ])));

        return
            new(ImmutableDictionary<Model.FunctionId, Model.Function>.Empty.Add(owner, function), [])
            {
                BindingNames = ImmutableDictionary<Model.BindingId, string>.Empty.Add(capture, spelling)
            };
    }

    [Fact]
    public void Lambda_lifting_cache_reuses_equal_inputs_and_all_lifted_functions_without_source_spellings()
    {
        var cache = new TrackingCache();
        var original = Input();
        var cold = Model.LambdaLifting.Lift(original, cache);
        var fresh = Input(spelling: "renamed");
        var warm = Model.LambdaLifting.Lift(fresh, cache);
        var uncached = Model.LambdaLifting.Lift(fresh);

        cache.Misses.Should().ContainSingle();
        cache.Hits.Should().ContainSingle();
        original.Functions[Owner("root")].Should().NotBeSameAs(fresh.Functions[Owner("root")]);
        warm.Functions.Keys.OfType<Model.FunctionId.Lifted>().Should().HaveCount(2);
        warm.Functions.Keys.Should().BeEquivalentTo(cold.Functions.Keys);

        foreach (var (id, function) in cold.Functions)
        {
            warm.Functions[id].Should().BeSameAs(function);
            uncached.Functions[id].Should().Be(function);
        }

        warm.BindingNames.Should().BeSameAs(fresh.BindingNames);
        warm.SourceDeclarations.Should().BeSameAs(fresh.SourceDeclarations);
        Model.ProgramValidation.Validate(warm);
        Model.LambdaLifting.Lift(warm, cache).Functions.Should().BeEquivalentTo(warm.Functions);
    }

    [Fact]
    public void Lambda_lifting_cache_invalidates_only_the_changed_function_body()
    {
        var cache = new TrackingCache();
        var original = Input();
        var independent = Input("independent");
        original = original with { Functions = original.Functions.AddRange(independent.Functions) };
        var cold = Model.LambdaLifting.Lift(original, cache);
        var changed = Input(version: 2);
        changed = changed with { Functions = changed.Functions.AddRange(independent.Functions) };
        var result = Model.LambdaLifting.Lift(changed, cache);

        cache.Misses.Should().HaveCount(3);
        cache.Hits.Select(input => input.Owner).Should().Equal(Owner("independent"));
        result.Functions[Owner("root")].Should().NotBeSameAs(cold.Functions[Owner("root")]);

        var changedHelper = new Model.FunctionId.Lifted(Owner("root"), 0);
        result.Functions[changedHelper].Should().NotBe(cold.Functions[changedHelper]);

        foreach (var id in independent.Functions.Keys.Concat(
            cold.Functions.Keys.OfType<Model.FunctionId.Lifted>().Where(id => id.Owner == Owner("independent"))))
        {
            result.Functions[id].Should().BeSameAs(cold.Functions[id]);
        }

        Model.ProgramValidation.Validate(result);
    }

    [Fact]
    public void Lambda_lifting_cache_reuses_existing_owner_when_an_unrelated_function_is_added()
    {
        var cache = new TrackingCache();
        var original = Input();
        var cold = Model.LambdaLifting.Lift(original, cache);
        var additional = Input("another");
        var expanded = Input();
        expanded = expanded with { Functions = expanded.Functions.AddRange(additional.Functions) };
        var result = Model.LambdaLifting.Lift(expanded, cache);

        cache.Misses.Select(input => input.Owner).Should().Equal(Owner("root"), Owner("another"));
        cache.Hits.Select(input => input.Owner).Should().Equal(Owner("root"));

        foreach (var (id, function) in cold.Functions)
            result.Functions[id].Should().BeSameAs(function);

        result.Functions.Should().HaveCount(6);
        Model.ProgramValidation.Validate(result);
    }

    [Fact]
    public void Lambda_lifting_cache_keys_include_owner_even_for_identical_closed_bodies()
    {
        var cache = new TrackingCache();
        var function = new Model.Function([], new Model.Expression.Literal(PineValue.EmptyList));

        var program =
            new Model.Program(
                ImmutableDictionary<Model.FunctionId, Model.Function>.Empty
                .Add(Owner("first"), function)
                .Add(Owner("second"), function),
                []);

        Model.LambdaLifting.Lift(program, cache);

        cache.Hits.Should().BeEmpty();
        cache.Misses.Select(input => input.Owner).Should().Equal(Owner("first"), Owner("second"));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Lambda_lifting_cache_preserves_duplicate_site_rejection_on_misses_and_hits(bool warm)
    {
        var cache = new TrackingCache();
        var original = Input();

        if (warm)
            Model.LambdaLifting.Lift(original, cache);

        var duplicate = new Model.FunctionId.Lifted(Owner("root"), 0);

        var invalid =
            original with
            {
                Functions =
                original.Functions.Add(
                    duplicate,
                    new Model.Function([], new Model.Expression.Literal(PineValue.EmptyList)))
            };

        Action lift = () => Model.LambdaLifting.Lift(invalid, cache);

        lift.Should().Throw<InvalidOperationException>().WithMessage(
            "A local function site was emitted more than once.");
    }
}
