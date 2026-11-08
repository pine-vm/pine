using AwesomeAssertions;
using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Elm.Testing;
using Pine.Core.Files;
using Pine.Core.IO;
using Pine.Core.Tests.Elm.LanguageServer;
using System;
using System.Collections.Concurrent;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using Concrete = Pine.Core.Elm.Syntax.Concrete;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestDiscoveryCacheTests
{
    private sealed class TrackingCache(ICache<PreparedDeclarationInput, PreparedSourceDeclaration> inner)
        : ICache<PreparedDeclarationInput, PreparedSourceDeclaration>
    {
        public ConcurrentQueue<PreparedDeclarationInput> Hits { get; } = new();

        public ConcurrentQueue<PreparedDeclarationInput> Misses { get; } = new();

        public bool TryGetValue(
            PreparedDeclarationInput key,
            [MaybeNullWhen(false)] out PreparedSourceDeclaration value)
        {
            var found = inner.TryGetValue(key, out value);
            (found ? Hits : Misses).Enqueue(key);
            return found;
        }

        public void Set(PreparedDeclarationInput key, PreparedSourceDeclaration value) => inner.Set(key, value);

        public void Reset()
        {
            Hits.Clear();
            Misses.Clear();
        }
    }

    private static Concrete.Declaration Parse(string declaration) =>
        ElmSyntaxParser.ParseModuleText("module Main exposing (..)\n" + declaration)
        .Extract(error => throw new InvalidOperationException(error.ToString())).Declarations.Single().Value;

    private static ElmTestDiscoveryResult Discover(string declarations, ElmTestDiscoveryCaches? caches = null) =>
        ElmTestDiscovery.DiscoverTests(LanguageServerTestingTests.Tree(declarations), caches: caches);

    private static void AssertSameInventory(ElmTestDiscoveryResult actual, ElmTestDiscoveryResult expected) =>
        JsonSerializer.Serialize(actual.Items).Should().Be(JsonSerializer.Serialize(expected.Items));

    [Fact]
    public async Task Body_edits_reuse_callers_but_run_current_productive_code()
    {
        const string source =
            """
            answer _ = 42
            suite = Test.test "answer" (\_ -> Expect.equal 42 (answer 0))
            """;

        var memory = ElmTestDiscoveryCaches.CreateInMemory();
        var tracking = new TrackingCache(memory.PreparedDeclarations);
        var caches = memory with { PreparedDeclarations = tracking };
        using var session = new ElmTestRunCacheSession(discoveryCaches: caches);
        var first = Discover(source, caches);
        var count = caches.CanonicalDeclarations.Count;
        tracking.Reset();

        var changed = Discover(source.Replace("= 42", "= 43", StringComparison.Ordinal), caches);
        tracking.Misses.Select(input => input.Name.DeclName).Should().Equal("answer");
        tracking.Hits.Select(input => input.Name.DeclName).Should().Contain("suite");
        caches.CanonicalDeclarations.Count.Should().Be(count + 1);
        AssertSameInventory(changed, Discover(source.Replace("= 42", "= 43", StringComparison.Ordinal)));
        var entry = changed.PrepareExecution().Single().Value;

        ElmTestRunner.ExecuteDiscoveredTest(changed.Items.Single(), entry, session, default)
            .Kind.Should().Be(CompletedTestKind.Failed);

        var originalEntry = first.PrepareExecution().Single().Value;

        ElmTestRunner.ExecuteDiscoveredTest(first.Items.Single(), originalEntry, session, default)
            .Kind.Should().Be(CompletedTestKind.Passed);

        await caches.WaitForMaintenanceAsync();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Prepared_values_rebind_navigation_to_shifted_current_sources(bool persistent)
    {
        const string source = """suite = Test.test "shifted" (\_ -> Expect.pass)""";
        var store = new FileStoreFromConcurrentDictionary();
        var initial = ElmTestDiscoveryCaches.CreateInMemory(persistent ? store : null);
        var first = Discover(source, initial);
        var memory = persistent ? ElmTestDiscoveryCaches.CreateInMemory(store) : initial;
        var tracking = new TrackingCache(memory.PreparedDeclarations);
        var caches = memory with { PreparedDeclarations = tracking };
        var changed = Discover("\n\n" + source, caches);
        tracking.Misses.Should().BeEmpty("range-free preparation identity survives source movement");
        tracking.Hits.Should().NotBeEmpty();

        changed.Items.Single().SourceLocation.Range.Start.Row.Should().Be(
            first.Items.Single().SourceLocation.Range.Start.Row + 2);

        AssertSameInventory(changed, Discover("\n\n" + source));
        changed.PrepareExecution().Should().ContainSingle();
        await caches.WaitForMaintenanceAsync();
    }

    [Fact]
    public void Identical_functions_in_different_modules_keep_independent_observer_identities()
    {
        const string source = """suite = Test.test "same" (\_ -> Expect.pass)""";

        var tree =
            FileTree.FromSetOfFilesWithStringPath(
                LanguageServerTestingTests.Tree(source).EnumerateFilesTransitive()
                .Select(file => (file.path, file.fileContent))
                .Append(
                    (["tests", "Other.elm"],
                    Encoding.UTF8.GetBytes(
                        LanguageServerTestingTests.Module(source)
                        .Replace("module Tests ", "module Other ", StringComparison.Ordinal)))));

        var caches = ElmTestDiscoveryCaches.CreateInMemory();
        _ = ElmTestDiscovery.DiscoverTests(tree, caches: caches);
        var repeated = ElmTestDiscovery.DiscoverTests(tree, caches: caches);

        repeated.Items.Select(item => item.SourceLocation.FilePath).Should()
            .BeEquivalentTo(["tests/Tests.elm", "tests/Other.elm"]);

        AssertSameInventory(repeated, ElmTestDiscovery.DiscoverTests(tree));
    }

    [Fact]
    public async Task Cached_preparation_keeps_observers_and_settings_local_to_concurrent_discoveries()
    {
        const string source =
            """
            suite = Test.describe "group"
                [ Test.fuzz Fuzz.int "fuzz" (\n -> Expect.equal n n)
                , Test.skip (Test.test "skip" (\_ -> Expect.pass))
                ]
            """;

        var caches = ElmTestDiscoveryCaches.CreateInMemory();
        var first = Discover(source, caches);

        var results =
            await Task.WhenAll(
                Enumerable.Range(0, 4)
                .Select(
                    index => Task.Run(
                        () => ElmTestDiscovery.DiscoverTests(
                            LanguageServerTestingTests.Tree(source),
                            executionSettings: new((uint)(index + 1), (uint)(index + 1)),
                            caches: caches))));

        for (var index = 0; index < results.Length; ++index)
        {
            var fresh =
                ElmTestDiscovery.DiscoverTests(
                    LanguageServerTestingTests.Tree(source),
                    executionSettings: new((uint)(index + 1), (uint)(index + 1)));

            AssertSameInventory(results[index], fresh);
            results[index].PrepareExecution().Single().Value.Fuzz!.RunsRequested.Should().Be((uint)(index + 1));
        }

        var cancelled =
            () => ElmTestDiscovery.DiscoverTests(
                LanguageServerTestingTests.Tree(source),
                cancellationToken: new CancellationToken(true),
                caches: caches);

        cancelled.Should().Throw<OperationCanceledException>();
        AssertSameInventory(Discover(source, caches), first);
        await caches.WaitForMaintenanceAsync();
    }

    [Fact]
    public void Cached_errors_preserve_current_ranges_and_dependency_diagnostics()
    {
        const string source = """suite = Test.test "missing" (\_ -> Expect.equal 1 absent)""";
        var caches = ElmTestDiscoveryCaches.CreateInMemory();
        var initial = () => Discover(source, caches);
        initial.Should().Throw<ElmCompilationException>();
        var shifted = () => Discover("\n\n" + source, caches);
        var fresh = () => Discover("\n\n" + source);

        shifted.Should().Throw<ElmCompilationException>().Which.Message.Should()
            .Be(fresh.Should().Throw<ElmCompilationException>().Which.Message);
    }

    [Fact]
    public async Task Discovery_tiers_trim_versions_above_three_working_sets_toward_two()
    {
        var caches = ElmTestDiscoveryCaches.CreateInMemory();

        var declarations =
            Enumerable.Range(0, 7).Select(
                index =>
                {
                    var name = DeclQualifiedName.Create(["Main"], "f" + index);
                    var declaration = Parse("f" + index + " = " + index);
                    return PreparedDeclarationInput.FromConcrete(name, declaration);
                }).ToArray();

        foreach (var input in declarations.Take(6))
            caches.PreparedDeclarations.Set(input, PreparedSourceDeclaration.Prepare(input.Syntax));

        using (caches.BeginInvocation())
        {
            foreach (var input in declarations.Take(2))
                caches.PreparedDeclarations.TryGetValue(input, out _).Should().BeTrue();
        }

        await caches.WaitForMaintenanceAsync();
        caches.PreparedLifetime!.Count.Should().Be(6, "cleanup triggers strictly above three working sets");
        caches.PreparedDeclarations.Set(declarations[6], PreparedSourceDeclaration.Prepare(declarations[6].Syntax));

        using (caches.BeginInvocation())
        {
            foreach (var input in declarations.Take(2))
                caches.PreparedDeclarations.TryGetValue(input, out _).Should().BeTrue();
        }

        await caches.WaitForMaintenanceAsync();
        caches.PreparedLifetime.Count.Should().Be(4);

        foreach (var input in declarations.Take(2))
            caches.PreparedDeclarations.TryGetValue(input, out _).Should().BeTrue();
    }

    [Theory]
    [InlineData("root = [ 1, 2.5, 3 ]")]
    [InlineData("root x = case x of\n    0 -> { answer = 1 }\n    _ -> { answer = 2 }")]
    [InlineData("root = let\n    f x = ( x, \"value\" )\n    in List.map f [ 1, 2 ]")]
    [InlineData("type Item = One Int | Two")]
    [InlineData("type alias Item = { answer : Int }")]
    [InlineData("port output : String -> Cmd msg")]
    [InlineData("infix left 5 (+) = add")]
    public void Prepared_codec_restores_source_literals_case_dispatch_and_expression_path_identities(string source)
    {
        var original = PreparedSourceDeclaration.Prepare(Parse(source));
        var codec = PreparedSourceDeclaration.Codec;
        var bytes = codec.Encode(original);
        var decoded = codec.Decode(bytes);
        codec.Encode(decoded).ToArray().Should().Equal(bytes.ToArray());

        decoded.ExpressionPaths.Select(item => item.Path).Should()
            .Equal(original.ExpressionPaths.Select(item => item.Path));

        if (decoded.Prepared is ElmSyntaxInterpreter.PreparedDeclaration.FunctionDeclaration function)
        {
            var expressions =
                ElmSyntaxInterpreter.EnumerateExpressionPaths(function.Function.Declaration.Expression)
                .Select(item => item.Expression).ToArray();

            foreach (var item in decoded.ExpressionPaths)
                expressions.Should().Contain(expression => ReferenceEquals(expression, item.Expression));
        }
    }

    [Fact]
    public void Prepared_hash_covers_anonymous_syntax_but_not_source_owners()
    {
        var input =
            PreparedDeclarationInput.FromConcrete(DeclQualifiedName.Create(["Main"], "root"), Parse("root = 1"));

        var same = PreparedDeclarationInput.FromConcrete(input.Name, Parse("\nroot = 1"));
        same.Should().Be(input);
        input.ComputeHash().Length.Should().Be(32);
        same.ComputeHash().ToArray().Should().Equal(input.ComputeHash().ToArray());

        PreparedDeclarationInput.FromConcrete(input.Name, Parse("root = 2"))
            .ComputeHash().ToArray().Should().NotEqual(input.ComputeHash().ToArray());

        PreparedDeclarationInput.FromConcrete(DeclQualifiedName.Create(["Other"], "root"), Parse("root = 1"))
            .ComputeHash().ToArray().Should().Equal(input.ComputeHash().ToArray());
    }
}
