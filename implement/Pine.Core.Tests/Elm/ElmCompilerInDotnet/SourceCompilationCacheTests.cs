using AwesomeAssertions;
using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Elm.Testing;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class SourceCompilationCacheTests
{
    private sealed class TrackingCache<T>(ICache<SourceTransformationInput, T>? backing = null)
        : ICache<SourceTransformationInput, T>
    {
        private readonly ICache<SourceTransformationInput, T> _cache =
            backing ?? new MemoryCache<SourceTransformationInput, T, long>(0);
        public List<SourceTransformationInput> Hits { get; } = [];
        public List<SourceTransformationInput> Misses { get; } = [];

        public bool TryGetValue(SourceTransformationInput key, [MaybeNullWhen(false)] out T value)
        {
            var hit = _cache.TryGetValue(key, out value);
            (hit ? Hits : Misses).Add(key);
            return hit;
        }

        public void Set(SourceTransformationInput key, T value) => _cache.Set(key, value);

        public void Reset()
        {
            Hits.Clear();
            Misses.Clear();
        }
    }

    [Fact]
    public void Body_only_edits_reuse_unmodified_functions_and_preserve_fresh_results()
    {
        var operators = new TrackingCache<Declaration>();
        var semantic = new TrackingCache<LoweredSourceFunction>();
        var original = Parse("helper n = n + 1\nroot n = helper n\nunrelated n = n + 100");
        var lowered = LowerOperators(original, operators);
        _ = Model.FromCanonicalSyntax.Lower(lowered, semantic);
        operators.Reset();
        semantic.Reset();

        var edited = Parse("helper n = n + 2\nroot n = helper n\nunrelated n = n + 100");
        var editedLowered = LowerOperators(edited, operators);
        var actual = Model.FromCanonicalSyntax.Lower(editedLowered, semantic);
        var expected = Model.FromCanonicalSyntax.Lower(LowerOperators(edited));
        AssertEquivalent(actual, expected);
        operators.Hits.Select(input => input.Name.DeclName).Should().BeEquivalentTo(["root", "unrelated"]);
        semantic.Hits.Select(input => input.Name.DeclName).Should().BeEquivalentTo(["root", "unrelated"]);
        semantic.Misses.Select(input => input.Name.DeclName).Should().Equal("helper");
    }

    [Theory]
    [InlineData("helper n = { x = n }", "helper n = { x = n, y = 0 }")]
    [InlineData("helper : Int -> Int\nhelper n = n", "helper : Float -> Float\nhelper n = n")]
    [InlineData("type alias Shape = { x : Int }", "type alias Shape = { x : Int, y : Int }")]
    [InlineData("type Shape = Shape Int", "type Shape = Shape Int Int")]
    [InlineData("", "newFunction n = n")]
    public void Semantic_environment_changes_invalidate_unchanged_callers(string before, string after)
    {
        const string root = "\nroot record = record.x\n";
        var cache = new TrackingCache<LoweredSourceFunction>();
        _ = Model.FromCanonicalSyntax.Lower(Parse(before + root), cache);
        cache.Reset();
        var edited = Parse(after + root);
        var actual = Model.FromCanonicalSyntax.Lower(edited, cache);
        AssertEquivalent(actual, Model.FromCanonicalSyntax.Lower(edited));
        cache.Misses.Select(input => input.Name.DeclName).Should().Contain("root");
        cache.Hits.Should().BeEmpty();
    }

    [Fact]
    public void Semantic_pruning_keeps_all_type_metadata_and_original_reference_classification()
    {
        var declarations = Parse(
            """
            type alias Person = { age : Int }
            type Choice = Yes | No
            helper n = n
            unused n = n
            root n = helper n
            """);
        var full = Model.FromCanonicalSyntax.Lower(declarations);
        var selected = Model.FromCanonicalSyntax.Lower(declarations, selectedFunctions: new HashSet<DeclQualifiedName>
        {
            Name("root")
        });
        selected.Program.Functions.Should().ContainSingle();
        selected.Program.Functions[new Model.FunctionId.Source(Name("root"))]
            .Should().Be(full.Program.Functions[new Model.FunctionId.Source(Name("root"))]);
        selected.Program.SourceDeclarations.Should().Equal(full.Program.SourceDeclarations);
    }

    [Fact]
    public void Lowering_configuration_and_every_input_component_change_cryptographic_addresses()
    {
        var declarations = Parse("root a b = a + b");
        var cache = new TrackingCache<Declaration>();
        _ = BuiltinOperatorLowering.Apply(declarations, new(true, false), cache);
        var first = cache.Misses.Single();
        cache.Reset();
        _ = BuiltinOperatorLowering.Apply(declarations, new(false, true), cache);
        var second = cache.Misses.Single();
        var clonedSyntax = ElmSyntaxAbstractJson.FromJsonString<Declaration>(
            ElmSyntaxAbstractJson.ToJsonString(first.Syntax));

        first.ComputeHash().Length.Should().Be(32);
        (first with { Syntax = clonedSyntax }).ComputeHash().ToArray().Should().Equal(first.ComputeHash().ToArray());
        var hashes = new[]
        {
            first,
            second,
            first with { Name = Name("other") },
            first with { EnvironmentHash = "different" },
            first with { Syntax = Parse("root a b = a - b")[Name("root")] }
        }.Select(input => Convert.ToHexString(input.ComputeHash().Span)).ToArray();
        hashes.Distinct().Should().HaveCount(hashes.Length);
    }

    [Fact]
    public void Dictionary_order_and_reparsed_equal_syntax_do_not_change_input_hashes()
    {
        const string source = "type Choice a = First a | Last\nhelper n = ( n, Just n )\nroot r = r.x";
        var first = new TrackingCache<LoweredSourceFunction>();
        var second = new TrackingCache<LoweredSourceFunction>();
        _ = Model.FromCanonicalSyntax.Lower(Parse(source), first);
        _ = Model.FromCanonicalSyntax.Lower(Parse(source).Reverse().ToImmutableDictionary(), second);
        first.Misses.Select(input => Convert.ToHexString(input.ComputeHash().Span))
            .Should().Equal(second.Misses.Select(input => Convert.ToHexString(input.ComputeHash().Span)));
    }

    [Fact]
    public void Persistent_layers_restore_both_models_in_fresh_cache_instances()
    {
        var store = new FileStoreFromConcurrentDictionary();
        const string source =
            """
            helper : Int -> Int
            helper n = n + 1
            root record = { x = record.x, call = \n -> helper n }
            """;
        var first = SourceCompilationCaches.CreateInMemory(store);
        var expected = Model.FromCanonicalSyntax.Lower(LowerOperators(Parse(source), first.OperatorLowering),
            first.SemanticFunctions);
        var fresh = SourceCompilationCaches.CreateInMemory(store);
        var operators = new TrackingCache<Declaration>(fresh.OperatorLowering);
        var semantic = new TrackingCache<LoweredSourceFunction>(fresh.SemanticFunctions);
        var actual = Model.FromCanonicalSyntax.Lower(LowerOperators(Parse(source), operators), semantic);
        operators.Misses.Should().BeEmpty();
        semantic.Misses.Should().BeEmpty();
        operators.Hits.Should().HaveCount(2);
        semantic.Hits.Should().HaveCount(2);
        AssertEquivalent(actual, expected);
        actual.GenericAccessEvidence.Should().NotBeEmpty();

        var nodes = new HashSet<Model.Expression>(ReferenceEqualityComparer.Instance);
        foreach (var function in actual.Program.Functions.Values)
            Visit(function.Body);
        actual.GenericAccessEvidence.Keys.Should().OnlyContain(node => nodes.Contains(node));

        Model.Expression Visit(Model.Expression expression)
        {
            nodes.Add(expression);
            _ = Model.ExpressionTransform.MapChildren(expression, Visit);
            return expression;
        }
    }

    [Fact]
    public void Semantic_codec_round_trips_nested_inferred_types_and_reference_bound_evidence()
    {
        var owner = new Model.FunctionId.Source(Name("root"));
        var binding = new Model.BindingId.Local(owner, 0);
        var expression = new Model.Expression.RecordAccess(
            new Model.Expression.ReferenceExpression(new Model.Reference.Local(binding)), "answer");
        TypeInference.InferredType type = new TypeInference.InferredType.OpenRecordType("row",
            [("answer", new TypeInference.InferredType.TupleType([
                new TypeInference.InferredType.ChoiceType(["Module"], "Choice",
                    [new TypeInference.InferredType.TypeVariable("a", TypeVariableConstraint.Comparable)]),
                new TypeInference.InferredType.RecordType([("x", new TypeInference.InferredType.IntType())])
            ]))]);
        var value = new LoweredSourceFunction(
            new(new([new Model.Pattern.VarPattern(binding)]), expression),
            ImmutableDictionary<Model.BindingId, string>.Empty.Add(binding, "record"),
            ImmutableDictionary.Create<Model.Expression, GenericRecordAccessEvidence>(
                ReferenceEqualityComparer.Instance).Add(expression,
                new(type, RecordOperationProvenance.NonIdentifierRecordExpression)));
        var codec = SourceCompilationCaches.SemanticCodec;
        var bytes = codec.Encode(value);
        var restored = codec.Decode(bytes);
        codec.Encode(restored).ToArray().Should().Equal(bytes.ToArray());
        restored.Evidence.Keys.Single().Should().BeSameAs(restored.Function.Body);
        restored.Function.Should().Be(value.Function);
    }

    [Fact]
    public void Corrupt_persistent_transformation_entries_are_explicit()
    {
        var store = new FileStoreFromConcurrentDictionary();
        var caches = SourceCompilationCaches.CreateInMemory(store);
        _ = LowerOperators(Parse("root n = n + 1"), caches.OperatorLowering);
        var path = store.ListFilesInDirectory(ImmutableList<string>.Empty).Single();
        store.SetFileContent(path, new byte[3]);
        var fresh = SourceCompilationCaches.CreateInMemory(store);
        Action read = () => LowerOperators(Parse("root n = n + 1"), fresh.OperatorLowering);
        read.Should().Throw<InvalidDataException>();
    }

    [Fact]
    public async Task Session_trims_only_above_three_working_sets_to_two_including_configuration_variants()
    {
        var caches = SourceCompilationCaches.CreateInMemory();
        await using var session = new ElmTestRunCacheSession(compilationCaches: caches);
        var syntax = Parse("root n = n")[Name("root")];
        var inputs = Enumerable.Range(0, 7)
            .Select(index => new SourceTransformationInput(Name("root"), syntax, "environment", "variant-" + index))
            .ToArray();
        foreach (var input in inputs.Take(6))
            caches.OperatorLowering.Set(input, syntax);

        using (session.BeginCompilationInvocation())
        {
            caches.OperatorLowering.TryGetValue(inputs[0], out _).Should().BeTrue();
            caches.OperatorLowering.TryGetValue(inputs[1], out _).Should().BeTrue();
        }
        await session.WaitForCompilationMaintenanceAsync();
        caches.OperatorLifetime!.Count.Should().Be(6, "the trigger is strictly greater than 3 * used");
        caches.OperatorLowering.Set(inputs[6], syntax);
        using (session.BeginCompilationInvocation())
        {
            caches.OperatorLowering.TryGetValue(inputs[0], out _).Should().BeTrue();
            caches.OperatorLowering.TryGetValue(inputs[1], out _).Should().BeTrue();
        }
        await session.WaitForCompilationMaintenanceAsync();
        caches.OperatorLifetime.Count.Should().Be(4);
        caches.OperatorLowering.TryGetValue(inputs[0], out _).Should().BeTrue();
        caches.OperatorLowering.TryGetValue(inputs[1], out _).Should().BeTrue();
    }

    private sealed record FixedUsage(int EntryCount) : ICacheUsage;

    private sealed class BlockingLifetime : IMarkerCacheLifetime<long>
    {
        public int Count => 100;
        public int WorkingSet = 10;
        public List<int> Budgets { get; } = [];
        public List<Func<long, CacheRetention>> Policies { get; } = [];
        public TaskCompletionSource Release { get; } = new(TaskCreationOptions.RunContinuationsAsynchronously);
        public long Marker;
        public ICacheUsage SetMarker(long marker)
        {
            Marker = marker;
            return new FixedUsage(WorkingSet);
        }

        public Task TrimAsync(int budget, Func<long, CacheRetention> retention,
            CancellationToken cancellationToken = default)
        {
            Budgets.Add(budget);
            Policies.Add(retention);
            return Budgets.Count == 1 ? Release.Task : Task.CompletedTask;
        }
    }

    [Fact]
    public async Task Concurrent_cleanup_coalesces_latest_budget_and_pins_active_invocations()
    {
        var lifetime = new BlockingLifetime();
        var caches = SourceCompilationCaches.CreateInMemory() with
        {
            OperatorLifetime = lifetime,
            SemanticLifetime = null
        };
        await using var session = new ElmTestRunCacheSession(compilationCaches: caches);
        using (session.BeginCompilationInvocation()) { }
        lifetime.Budgets.Should().Equal(20);

        try
        {
            lifetime.WorkingSet = 2;
            using (session.BeginCompilationInvocation())
            {
                lifetime.Marker.Should().Be(2);
                lifetime.Policies.Single()(lifetime.Marker).Should().BeOfType<CacheRetention.Retain>();
            }

            lifetime.Budgets.Should().Equal([20], "cleanup remains single-flight");
            lifetime.Policies.Single()(2).Should().Be(new CacheRetention.Ranked(2));
            var completed = session.WaitForCompilationMaintenanceAsync();
            lifetime.Release.SetResult();
            await completed.WaitAsync(TimeSpan.FromSeconds(10));
            lifetime.Budgets.Should().Equal(20, 4);
        }
        finally
        {
            lifetime.Release.TrySetResult();
        }
    }

    [Fact]
    public void Cancelled_transformation_does_not_publish_a_result_and_can_retry()
    {
        var declarations = Parse("root n = n");
        var cache = new TrackingCache<LoweredSourceFunction>();
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();
        Action run = () => Model.FromCanonicalSyntax.Lower(declarations, cache, cancellationToken: cancellation.Token);
        run.Should().Throw<OperationCanceledException>();
        cache.Misses.Should().BeEmpty();
        _ = Model.FromCanonicalSyntax.Lower(declarations, cache);
        cache.Misses.Should().ContainSingle();
    }

    private static DeclQualifiedName Name(string name) => DeclQualifiedName.Create(["Test"], name);

    private static ImmutableDictionary<DeclQualifiedName, Declaration> Parse(string declarations)
    {
        var parsed = ElmSyntaxParser.ParseModuleText("module Test exposing (..)\n" + declarations + "\n")
            .Extract(error => throw new Exception(error.ToString()));
        var canonical = Canonicalization.CanonicalizeOrThrow([parsed])
            .Extract(error => throw new Exception(error));
        return ElmCompiler.FlattenModulesToDeclarationDictionary(
            [.. canonical.Values.Select(module =>
                ConvertFromConcrete.FromFile(module.Extract(error => throw new Exception(error))))]);
    }

    private static ImmutableDictionary<DeclQualifiedName, Declaration> LowerOperators(
        ImmutableDictionary<DeclQualifiedName, Declaration> declarations,
        ICache<SourceTransformationInput, Declaration>? cache = null) =>
        BuiltinOperatorLowering.Apply(declarations, cache).Extract(error => throw new Exception(error));

    private static void AssertEquivalent(Model.SourceLoweringResult actual, Model.SourceLoweringResult expected)
    {
        var options = Model.CompilerModelJson.CreateOptions();
        JsonSerializer.Serialize(actual.Program, options).Should().Be(JsonSerializer.Serialize(expected.Program, options));
        actual.GenericAccessEvidence.Count.Should().Be(expected.GenericAccessEvidence.Count);
    }
}
