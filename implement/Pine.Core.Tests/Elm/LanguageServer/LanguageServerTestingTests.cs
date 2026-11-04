using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.IO;
using Pine.Core.LanguageServerProtocol;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using ElmLanguageServer = Pine.Core.Elm.LanguageServer.LanguageServer;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class LanguageServerTestingTests
{
    public const string Manifest =
        """
        {
            "type": "application", "source-directories": ["src"], "elm-version": "0.19.1",
            "dependencies": { "direct": { "elm/core": "1.0.5" }, "indirect": {} },
            "test-dependencies": {
                "direct": { "elm-explorations/test": "2.2.1" },
                "indirect": { "elm/random": "1.0.0", "elm/bytes": "1.0.8" }
            }
        }
        """;

    public static string Module(string declarations) =>
        "module Tests exposing (suite)\nimport Test\nimport Expect\nimport Fuzz\n\n" + declarations + "\n";

    public static FileTree Tree(string declarations) =>
        FileTree.FromSetOfFilesWithStringPath(
            new (IReadOnlyList<string>, ReadOnlyMemory<byte>)[]
            {
                (["elm.json"], Encoding.UTF8.GetBytes(Manifest)),
                (["tests", "Tests.elm"], Encoding.UTF8.GetBytes(Module(declarations)))
            });

    private static ElmTestSourceSnapshot Snapshot(FileTree tree) =>
        new(VirtualWorkspace.RootUri, tree, new Dictionary<string, int>());

    [Fact]
    public async Task Testing_reuses_injected_transformation_caches_without_stale_productive_code()
    {
        const string declarations =
            """
            answer : Int -> Int
            answer _ = 42
            suite = Test.test "answer" (\_ -> Expect.equal 42 (answer 0))
            """;

        var current = Tree(declarations);
        var caches = SourceCompilationCaches.CreateInMemory();

        using var service =
            new LanguageServerTestingService(
                (_, _) => Task.FromResult(Snapshot(current)),
                compilationCaches: caches);

        var initial = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        initial.Complete.Should().BeTrue(initial.Error?.Message);
        var first = await service.RunAsync(new(VirtualWorkspace.RootUri, initial.Version, "first"));
        first.Results.Single().State.Should().Be("passed");
        var operatorEntries = caches.OperatorLifetime!.Count;
        var semanticEntries = caches.SemanticLifetime!.Count;
        operatorEntries.Should().BeGreaterThan(0);
        semanticEntries.Should().BeGreaterThan(0);
        var repeated = await service.RunAsync(new(VirtualWorkspace.RootUri, initial.Version, "repeated"));
        repeated.Results.Single().State.Should().Be("passed");
        caches.OperatorLifetime.Count.Should().Be(operatorEntries);
        caches.SemanticLifetime.Count.Should().Be(semanticEntries);

        current = Tree(declarations.Replace("answer _ = 42", "answer _ = 43", StringComparison.Ordinal));
        var updated = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        updated.Complete.Should().BeTrue(updated.Error?.Message);
        var edited = await service.RunAsync(new(VirtualWorkspace.RootUri, updated.Version, "edited"));
        edited.Results.Single().State.Should().Be("failed");
        caches.OperatorLifetime.Count.Should().Be(operatorEntries + 1);
        caches.SemanticLifetime.Count.Should().Be(semanticEntries + 1);
    }

    [Fact]
    public async Task Testing_versions_are_immutable_and_different_versions_run_independently()
    {
        var firstTree = Tree("""suite = Test.test "old" (\_ -> Expect.pass)""");
        var current = firstTree;
        var events = new ConcurrentQueue<PineTestsEvent>();
        var firstStarted = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var releaseFirst = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

        using var service =
            new LanguageServerTestingService(
                (_, _) => Task.FromResult(Snapshot(current)),
                publishEvent:
                async notification =>
                {
                    events.Enqueue(notification);

                    if (notification.RunId == "old-run" && notification.State == "started")
                    {
                        firstStarted.TrySetResult();
                        await releaseFirst.Task;
                    }
                });

        var old = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        old.Complete.Should().BeTrue(old.Error?.Message);
        old.Items!.Single().Label.Should().Be("old");

        var expectedHash =
            Convert.ToHexStringLower(
                PineValueHashTree.ComputeHash(FileTreeEncoding.Encode(FileTree.Sort(firstTree))).Span);

        old.Version.Should().Be("file-tree-" + expectedHash);

        current = Tree("""suite = Test.test "new" (\_ -> Expect.equal 1 2)""");
        var updated = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        updated.Complete.Should().BeTrue(updated.Error?.Message);
        updated.Items!.Single().Label.Should().Be("new");
        updated.Version.Should().NotBe(old.Version);

        var oldRun = service.RunAsync(new(VirtualWorkspace.RootUri, old.Version, "old-run"));
        await firstStarted.Task.WaitAsync(TimeSpan.FromSeconds(60));

        try
        {
            var newRun =
                await service.RunAsync(new(VirtualWorkspace.RootUri, updated.Version, "new-run"))
                .WaitAsync(TimeSpan.FromSeconds(60));

            oldRun.IsCompleted.Should().BeFalse();
            newRun.Version.Should().Be(updated.Version);
            newRun.Results.Single().State.Should().Be("failed");
            newRun.Results.Single().Messages.Single().Expected.Should().Be("1");
            newRun.Results.Single().Messages.Single().Actual.Should().Be("2");
        }
        finally
        {
            releaseFirst.TrySetResult();
        }

        var completedOld = await oldRun;
        completedOld.Version.Should().Be(old.Version);
        completedOld.Results.Single().State.Should().Be("passed");

        service.GetSource(new(VirtualWorkspace.RootUri, old.Version, "tests/Tests.elm")).Text.Should().Contain(
            "\"old\"");

        foreach (var runId in new[] { "old-run", "new-run" })
        {
            var notifications = events.Where(item => item.RunId == runId).ToArray();

            notifications.Select(item => item.Sequence).Should().Equal(
                Enumerable.Range(1, notifications.Length).Select(i => (long)i));

            notifications.Select(item => item.Version).Distinct().Should().ContainSingle();
            notifications.Last().Kind.Should().Be("end");
        }
    }

    [Fact]
    public async Task Testing_selection_expands_groups_and_exclusions_win_without_running_skipped_or_todo_tests()
    {
        var tree =
            Tree(
                """
                suite =
                    Test.describe "suite"
                        [ Test.describe "nested"
                            [ Test.test "pass" (\_ -> Expect.pass)
                            , Test.test "fail" (\_ -> Expect.fail "expected failure")
                            ]
                        , Test.skip (Test.test "skip" (\_ -> Debug.todo "must not run"))
                        , Test.todo "todo"
                        ]
                """);

        using var service = new LanguageServerTestingService((_, _) => Task.FromResult(Snapshot(tree)));
        var discovery = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        discovery.Complete.Should().BeTrue(discovery.Error?.Message);
        var nested = discovery.Items!.Single(item => item.Label == "nested");
        var fail = discovery.Items!.Single(item => item.Label == "fail");

        var selected =
            await service.RunAsync(
                new(
                    VirtualWorkspace.RootUri,
                    discovery.Version,
                    "selected",
                    [
                    nested.Id
                    ],
                    [
                    fail.Id
                    ],
                    DiscoveryId: discovery.DiscoveryId));

        selected.Status.Should().Be("completed", selected.Error?.Message);
        selected.Results.Should().ContainSingle().Which.State.Should().Be("passed");

        var all = await service.RunAsync(new(VirtualWorkspace.RootUri, discovery.Version, "all"));
        all.Results.Select(result => result.State).Should().BeEquivalentTo("passed", "failed", "skipped", "skipped");
        all.Incomplete.Should().BeTrue();

        var none =
            await service.RunAsync(
                new(
                    VirtualWorkspace.RootUri,
                    discovery.Version,
                    "none",
                    Include: [],
                    DiscoveryId: discovery.DiscoveryId));

        none.Results.Should().BeEmpty();

        var invalid =
            await service.RunAsync(
                new(
                    VirtualWorkspace.RootUri,
                    discovery.Version,
                    "invalid",
                    [
                    "unknown"
                    ],
                    DiscoveryId: discovery.DiscoveryId));

        invalid.Status.Should().Be("errored");
        invalid.Error!.Code.Should().Be("unknownTest");
        invalid.Results.Should().BeEmpty();

        var stale =
            await service.RunAsync(
                new(
                    VirtualWorkspace.RootUri,
                    discovery.Version,
                    "stale",
                    [
                    nested.Id
                    ],
                    DiscoveryId: "another-inventory"));

        stale.Error!.Code.Should().Be("staleDiscovery");
        stale.Results.Should().BeEmpty();
    }

    [Fact]
    public async Task Testing_only_inventory_remains_complete_and_marks_excluded_tests()
    {
        var tree =
            Tree("""suite = Test.describe "suite" [ Test.only (Test.test "only" (\_ -> Expect.pass)), Test.test "excluded" (\_ -> Debug.todo "must not run") ]""");

        using var service = new LanguageServerTestingService((_, _) => Task.FromResult(Snapshot(tree)));
        var discovery = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        discovery.Complete.Should().BeTrue(discovery.Error?.Message);
        discovery.HasOnly.Should().BeTrue();
        discovery.Items!.Single(item => item.Label == "excluded").Selection.ToString().Should().Be("ExcludedByOnly");
        var run = await service.RunAsync(new(VirtualWorkspace.RootUri, discovery.Version, "only"));
        run.Results.Select(result => result.State).Should().BeEquivalentTo("passed", "skipped");
        run.Incomplete.Should().BeTrue();
    }

    [Fact]
    public async Task Testing_fuzz_failures_retain_replay_settings_and_shrunk_counterexamples()
    {
        var tree = Tree("""suite = Test.fuzz (Fuzz.intRange 1 10) "property" (\n -> Expect.equal 0 n)""");
        using var service = new LanguageServerTestingService((_, _) => Task.FromResult(Snapshot(tree)));
        var discovery = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        discovery.Complete.Should().BeTrue(discovery.Error?.Message);

        var run =
            await service.RunAsync(new(VirtualWorkspace.RootUri, discovery.Version, "fuzz", Seed: 42, FuzzRuns: 3));

        run.Status.Should().Be("completed", run.Error?.Message);
        var result = run.Results.Single();
        result.State.Should().Be("failed");
        result.Fuzz!.Settings.Seed.Should().Be(42);
        result.Fuzz.RunsRequested.Should().Be(3);
        result.Fuzz.ShrunkInput.Should().NotBeNullOrEmpty();
        result.ReproductionCommand.Should().Contain("--seed 42 --fuzz 3");

        var replay =
            await service.RunAsync(new(VirtualWorkspace.RootUri, discovery.Version, "replay", Seed: 42, FuzzRuns: 3));

        replay.Results.Single().Fuzz!.ShrunkInput.Should().Be(result.Fuzz.ShrunkInput);
        replay.Results.Single().Fuzz!.ShrunkChoices.Should().Equal(result.Fuzz.ShrunkChoices);
    }

    [Fact]
    public async Task Testing_runs_reuse_inventory_identity_and_locations_after_discovery_cancellation()
    {
        var tree = Tree("""suite = Test.test "failure" (\_ -> Expect.fail "expected")""");
        var captures = 0;

        using var service =
            new LanguageServerTestingService(
                (_, _) =>
                {
                    captures++;
                    return Task.FromResult(Snapshot(tree));
                });

        using var cancellation = new CancellationTokenSource();
        var discovered = await service.DiscoverAsync(new(VirtualWorkspace.RootUri), cancellation.Token);
        discovered.Complete.Should().BeTrue(discovered.Error?.Message);
        cancellation.Cancel();
        var item = discovered.Items!.Single();

        for (var index = 0; index < 2; index++)
        {
            var run =
                await service.RunAsync(
                    new(
                        VirtualWorkspace.RootUri,
                        discovered.Version,
                        "reuse-" + index,
                        Include: [item.Id],
                        Seed: (uint)index,
                        FuzzRuns: 3,
                        DiscoveryId: discovered.DiscoveryId));

            run.Status.Should().Be("completed", run.Error?.Message);
            run.DiscoveryId.Should().BeSameAs(discovered.DiscoveryId);
            run.Results.Single().Messages.Single().Location.Should().BeSameAs(item.Location);
        }

        var repeated = await service.DiscoverAsync(new(VirtualWorkspace.RootUri, discovered.Version));
        repeated.DiscoveryId.Should().BeSameAs(discovered.DiscoveryId);
        repeated.Items!.Single().Location.Should().BeSameAs(item.Location);
        captures.Should().Be(1);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Testing_cancelled_preparation_does_not_poison_a_retained_inventory(bool emptySelection)
    {
        var tree = Tree("""suite = Test.fuzz Fuzz.int "property" (\_ -> Expect.pass)""");
        using var cancellation = new CancellationTokenSource();

        using var service =
            new LanguageServerTestingService(
                (_, _) => Task.FromResult(Snapshot(tree)),
                publishEvent:
                notification =>
                {
                    if (notification.RunId == "cancel-prepare" && notification.State == "preparation")
                        cancellation.Cancel();

                    return Task.CompletedTask;
                });

        var discovery = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));

        var cancelled =
            await service.RunAsync(
                new(
                    VirtualWorkspace.RootUri,
                    discovery.Version,
                    "cancel-prepare",
                    Include: emptySelection ? [] : null,
                    Seed: 42,
                    FuzzRuns: 3,
                    DiscoveryId: discovery.DiscoveryId),
                cancellation.Token);

        cancelled.Status.Should().Be("cancelled");

        if (emptySelection)
            cancelled.Results.Should().BeEmpty();

        else
            cancelled.Results.Single().State.Should().Be("skipped");

        var retry =
            await service.RunAsync(
                new(VirtualWorkspace.RootUri, discovery.Version, "retry", Seed: 123, FuzzRuns: 2));

        retry.Status.Should().Be("completed", retry.Error?.Message);
        retry.Results.Single().State.Should().Be("passed");
        retry.Results.Single().Fuzz!.Settings.Seed.Should().Be(123);
        retry.Results.Single().Fuzz!.RunsRequested.Should().Be(2);
    }

    [Fact]
    public async Task Testing_cancellation_terminates_the_run_and_all_queued_tests()
    {
        var tree =
            Tree(
                """
                loop _ = loop ()
                suite = Test.describe "suite" [ Test.test "loop" (\_ -> loop ()), Test.test "next" (\_ -> Expect.pass) ]
                """);

        using var cancellation = new CancellationTokenSource();
        var events = new ConcurrentQueue<PineTestsEvent>();

        using var service =
            new LanguageServerTestingService(
                (_, _) => Task.FromResult(Snapshot(tree)),
                publishEvent:
                notification =>
                {
                    events.Enqueue(notification);

                    if (notification.State == "started")
                        cancellation.CancelAfter(TimeSpan.FromMilliseconds(100));

                    return Task.CompletedTask;
                });

        var discovery = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        discovery.Complete.Should().BeTrue(discovery.Error?.Message);

        var result =
            await service.RunAsync(new(VirtualWorkspace.RootUri, discovery.Version, "cancel"), cancellation.Token)
            .WaitAsync(TimeSpan.FromSeconds(60));

        result.Status.Should().Be("cancelled");
        result.Incomplete.Should().BeTrue();
        result.Results.Should().HaveCount(2).And.OnlyContain(test => test.State == "skipped");
        events.Last().Kind.Should().Be("end");
        events.Last().State.Should().Be("cancelled");
    }

    [Fact]
    public async Task Testing_source_errors_do_not_become_successful_empty_inventories()
    {
        using var service =
            new LanguageServerTestingService(
                (_, _) => Task.FromResult(Snapshot(Tree("suite = Missing.value"))));

        var discovery = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        discovery.Complete.Should().BeFalse();
        discovery.Items.Should().BeNull();
        discovery.Error!.Code.Should().Be("discoveryFailed");
        discovery.Error.Message.Should().Contain("Missing");
    }

    [Fact]
    public async Task Testing_explicit_trees_are_verified_and_unknown_versions_never_fall_back_to_workspace()
    {
        var captures = 0;

        var tree =
            FileTree.FromSetOfFilesWithStringPath(
                [
                    .. Tree("""suite = Test.todo "later" """).EnumerateFilesTransitive()
                    .Select(file => (file.path, file.fileContent)),
                    (["binary.dat"], new byte[] { 0xff })
                ]);

        using var service =
            new LanguageServerTestingService(
                (_, _) =>
                {
                    ++captures;
                    return Task.FromResult(Snapshot(tree));
                });

        var version = "file-tree-" + LanguageServerTestingService.TreeHashOf(tree);

        var files =
            tree.EnumerateFilesTransitive().Select(
                file => new PineTestSourceFile(file.path, Convert.ToBase64String(file.fileContent.Span))).ToArray();

        var result = await service.DiscoverAsync(new(VirtualWorkspace.RootUri, version, Files: files));
        result.Complete.Should().BeTrue(result.Error?.Message);
        captures.Should().Be(0);
        var binaryText = () => service.GetSource(new(result.ProjectUri, result.Version, "binary.dat"));
        binaryText.Should().Throw<PineTestingProtocolException>().Which.Code.Should().Be("invalidSource");

        var unknownVersion = "file-tree-" + new string('0', 64);
        var mismatch = () => service.DiscoverAsync(new(VirtualWorkspace.RootUri, unknownVersion, Files: files));
        (await mismatch.Should().ThrowAsync<PineTestingProtocolException>()).Which.Code.Should().Be("versionMismatch");
        var unknown = () => service.DiscoverAsync(new(VirtualWorkspace.RootUri, unknownVersion));
        (await unknown.Should().ThrowAsync<PineTestingProtocolException>()).Which.Code.Should().Be("versionUnavailable");
        var missingVersion = () => service.RunAsync(new(VirtualWorkspace.RootUri, null!, "missing-version"));

        (await missingVersion.Should().ThrowAsync<PineTestingProtocolException>()).Which.Code.Should().Be(
            "invalidVersion");

        var traversal =
            () => service.DiscoverAsync(new(VirtualWorkspace.RootUri, version, Files: [new(["..", "elm.json"], "")]));

        (await traversal.Should().ThrowAsync<PineTestingProtocolException>()).Which.Code.Should().Be("invalidSource");

        var duplicate =
            () => service.DiscoverAsync(new(VirtualWorkspace.RootUri, version, Files: [files[0], files[0]]));

        (await duplicate.Should().ThrowAsync<PineTestingProtocolException>()).Which.Code.Should().Be("invalidSource");
        captures.Should().Be(0);
    }

    [Fact]
    public async Task Testing_locations_are_zero_based_UTF16_and_navigate_to_original_factory_source()
    {
        const string helper =
            """
            module Helpers exposing (make)
            import Test
            import Expect
            make label = let prefix = "😀" in Test.test (prefix ++ label) (\_ -> Expect.pass)
            """;

        var files =
            new (IReadOnlyList<string>, ReadOnlyMemory<byte>)[]
            {
                (["app", "elm.json"], Encoding.UTF8.GetBytes(Manifest.Replace("\"src\"", "\"../shared\""))),
                (["app", "tests", "Tests.elm"], Encoding.UTF8.GetBytes("module Tests exposing (suite)\nimport Helpers\nsuite = Helpers.make \"case\"\n")),
                (["shared", "Helpers.elm"], Encoding.UTF8.GetBytes(helper))
            };

        var tree = FileTree.FromSetOfFilesWithStringPath(files);
        using var service = new LanguageServerTestingService((_, _) => Task.FromResult(Snapshot(tree)));
        var discovery = await service.DiscoverAsync(new(VirtualWorkspace.RootUri + "app/"));
        discovery.Complete.Should().BeTrue(discovery.Error?.Message);
        var item = discovery.Items!.Single();
        item.SourcePath.Should().Be("shared/Helpers.elm");
        item.Location.Uri.Should().Be(VirtualWorkspace.RootUri + "shared/Helpers.elm");
        item.Location.Range.Start.Line.Should().Be(3);
        var line = helper.Split('\n')[3];
        item.Location.Range.Start.Character.Should().Be((uint)line.IndexOf("Test.test", StringComparison.Ordinal));
        service.GetSource(new(discovery.ProjectUri, discovery.Version, item.SourcePath)).Text.Should().Be(helper);
    }

    [Fact]
    public async Task Testing_workspace_snapshots_include_unsaved_documents_and_refresh_document_versions()
    {
        var original = Module("""suite = Test.test "saved" (\_ -> Expect.pass)""");
        var changed = Module("""suite = Test.test "unsaved" (\_ -> Expect.pass)""");

        var (workspace, _) =
            VirtualWorkspace.Create(
                [(["elm.json"], Manifest), (["tests", "Tests.elm"], original)]);

        using var server = CreateServer(workspace);
        var initialization = server.Initialize(InitializeParameters());
        initialization.Item1.Capabilities.Experimental!.PineTests!.ProtocolVersion.Should().Be(1);
        var saved = await server.TestsDiscoverAsync(new(VirtualWorkspace.RootUri));
        var uri = VirtualWorkspace.DocumentUri("tests", "Tests.elm");
        await server.TextDocument_didOpenAsync(new(uri, "elm", 1, changed));
        var unsaved = await server.TestsDiscoverAsync(new(VirtualWorkspace.RootUri));
        unsaved.Complete.Should().BeTrue(unsaved.Error?.Message);
        unsaved.Items!.Single().Label.Should().Be("unsaved");
        unsaved.Version.Should().NotBe(saved.Version);
        unsaved.DocumentVersions[uri].Should().Be(1);
        await server.TextDocument_didChangeAsync(new(uri, 2), [new(null, null, changed)]);
        var sameBytes = await server.TestsDiscoverAsync(new(VirtualWorkspace.RootUri));
        sameBytes.Version.Should().Be(unsaved.Version);
        sameBytes.DocumentVersions[uri].Should().Be(2);
        var historical = await server.TestsDiscoverAsync(new(VirtualWorkspace.RootUri, saved.Version));
        historical.Items!.Single().Label.Should().Be("saved");
    }

    [Fact]
    public async Task Testing_protocol_requires_explicit_capability_negotiation()
    {
        var (workspace, _) = VirtualWorkspace.Create();
        using var server = CreateServer(workspace);

        var response =
            server.Initialize(
                InitializeParameters() with { Capabilities = new(null, null, new(new(2))) });

        response.Item1.Capabilities.Experimental.Should().BeNull();
        var discover = () => server.TestsDiscoverAsync(new(VirtualWorkspace.RootUri));

        (await discover.Should().ThrowAsync<PineTestingProtocolException>()).Which.Code.Should().Be(
            "unsupportedProtocol");
    }

    [Theory]
    [InlineData("true")]
    [InlineData("[]")]
    [InlineData("\"vendor capability\"")]
    [InlineData("{\"pineTests\":true}")]
    [InlineData("{\"pineTests\":{\"protocolVersion\":\"1\"}}")]
    public void Testing_unrelated_experimental_values_do_not_break_initialization(string experimental)
    {
        var capabilities =
            JsonSerializer.Deserialize<ClientCapabilities>(
                "{\"experimental\":" + experimental + "}",
                new JsonSerializerOptions { PropertyNamingPolicy = JsonNamingPolicy.CamelCase })!;

        var (workspace, _) = VirtualWorkspace.Create();
        using var server = CreateServer(workspace);
        var result = server.Initialize(InitializeParameters() with { Capabilities = capabilities });
        result.Item1.Capabilities.Experimental.Should().BeNull();
    }

    [Fact]
    public async Task Testing_unsaved_sources_preserve_percent_escaped_URI_path_characters()
    {
        const string directory = "app #one";
        const string projectUri = VirtualWorkspace.RootUri + "app%20%23one/";
        var text = Module("""suite = Test.test "saved" (\_ -> Expect.pass)""");
        var unsaved = text.Replace("\"saved\"", "\"unsaved\"");

        var (workspace, _) =
            VirtualWorkspace.Create(
                [([directory, "elm.json"], Manifest), ([directory, "tests", "Tests.elm"], text)]);

        using var server = CreateServer(workspace);
        server.Initialize(InitializeParameters());
        var uri = projectUri + "tests/Tests.elm";
        await server.TextDocument_didOpenAsync(new(uri, "elm", 1, unsaved));
        var result = await server.TestsDiscoverAsync(new(projectUri));
        result.Complete.Should().BeTrue(result.Error?.Message);
        result.Items!.Single().Label.Should().Be("unsaved");
        result.Items!.Single().Location.Uri.Should().Be(uri);
        result.DocumentVersions[uri].Should().Be(1);
    }

    [Theory]
    [InlineData("file:///k%3A/Source/Repos/elm-time/implement/elm-in-elm/")]
    [InlineData("file:///K%3a/Source/Repos/elm-time/implement/elm-in-elm/")]
    public async Task Testing_Windows_workspace_URIs_discover_and_run_root_and_nested_projects(string clientRoot)
    {
        const string diskRoot = "file:///K:/Source/Repos/elm-time/implement/elm-in-elm/";
        var store = new FileStoreFromConcurrentDictionary();
        VirtualWorkspace.SetFile(store, ["elm.json"], Manifest);

        VirtualWorkspace.SetFile(
            store,
            [
            "tests",
            "Tests.elm"
            ],
            Module("""suite = Test.test "root" (\_ -> Expect.pass)"""));

        VirtualWorkspace.SetFile(store, ["pine-elm-syntax", "elm.json"], Manifest);

        VirtualWorkspace.SetFile(
            store,
            [
            "pine-elm-syntax",
            "tests",
            "Tests.elm"
            ],
            Module("""suite = Test.test "nested" (\_ -> Expect.pass)"""));

        var workspace = new WorkspaceFromFileStoreMounts([new FileStoreMount(new Uri(diskRoot), store)]);
        using var server = CreateServer(workspace);

        server.Initialize(
            InitializeParameters() with
            {
                RootUri = clientRoot.TrimEnd('/'),
                WorkspaceFolders = [new(clientRoot.TrimEnd('/'), "elm-in-elm")]
            });

        var root = await server.TestsDiscoverAsync(new(clientRoot));
        root.Complete.Should().BeTrue(root.Error?.Message);
        root.Items!.Single().Label.Should().Be("root");
        root.Items!.Single().FilePath.Should().Be("tests/Tests.elm");
        var nested = await server.TestsDiscoverAsync(new(clientRoot + "pine-elm-syntax/"));
        nested.Complete.Should().BeTrue(nested.Error?.Message);
        nested.Items!.Single().Label.Should().Be("nested");
        nested.Items!.Single().FilePath.Should().Be("pine-elm-syntax/tests/Tests.elm");
        var run = await server.TestsRunAsync(new(clientRoot, root.Version, "windows-root"));
        run.Status.Should().Be("completed", run.Error?.Message);
        run.Results.Should().ContainSingle().Which.State.Should().Be("passed");

        var nestedRun =
            await server.TestsRunAsync(new(clientRoot + "pine-elm-syntax/", nested.Version, "windows-nested"));

        nestedRun.Status.Should().Be("completed", nestedRun.Error?.Message);
        nestedRun.Results.Should().ContainSingle().Which.State.Should().Be("passed");

        var unsavedUri = clientRoot + "tests/Tests.elm";

        await server.TextDocument_didOpenAsync(
            new(
                unsavedUri,
                "elm",
                1,
                Module("""suite = Test.test "unsaved" (\_ -> Expect.pass)""")));

        var unsaved = await server.TestsDiscoverAsync(new(clientRoot));
        unsaved.Complete.Should().BeTrue(unsaved.Error?.Message);
        unsaved.Items!.Single().Label.Should().Be("unsaved");
        unsaved.DocumentVersions.Should().ContainKey(unsavedUri).WhoseValue.Should().Be(1);
        unsaved.Version.Should().NotBe(root.Version);
        server.TestsSource(new(clientRoot, root.Version, "tests/Tests.elm")).Text.Should().Contain("\"root\"");
    }

    [Fact]
    public async Task Testing_Windows_source_versions_accept_client_and_filesystem_URI_spellings()
    {
        const string root = "file:///K:/Source/Repos/elm-time/implement/elm-in-elm/";

        using var service =
            new LanguageServerTestingService(
                (_, _) =>
                Task.FromResult(
                    new ElmTestSourceSnapshot(
                        root,
                        Tree("""suite = Test.todo "root" """),
                        new Dictionary<string, int>())));

        var discovery = await service.DiscoverAsync(new("file:///k%3A/Source/Repos/elm-time/implement/elm-in-elm/"));
        discovery.Complete.Should().BeTrue(discovery.Error?.Message);
        var retained = await service.DiscoverAsync(new(root, discovery.Version));
        retained.DiscoveryId.Should().Be(discovery.DiscoveryId);
        service.GetSource(new(root, discovery.Version, "tests/Tests.elm")).Text.Should().Contain("root");
    }

    [Fact]
    public async Task Testing_evicted_file_tree_versions_fail_explicitly_instead_of_using_current_sources()
    {
        var tree = Tree("""suite = Test.todo "first" """);
        using var service = new LanguageServerTestingService((_, _) => Task.FromResult(Snapshot(tree)));
        var first = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));

        for (var index = 0; index < LanguageServerTestingService.RetainedVersions; ++index)
        {
            tree = Tree("suite = Test.todo \"version " + index + "\"");
            var result = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
            result.Complete.Should().BeTrue(result.Error?.Message);
        }

        var evicted = () => service.RunAsync(new(VirtualWorkspace.RootUri, first.Version, "evicted"));
        (await evicted.Should().ThrowAsync<PineTestingProtocolException>()).Which.Code.Should().Be("versionUnavailable");
    }

    [Fact]
    public async Task Testing_empty_groups_fail_and_VM_errors_are_not_expectation_failures()
    {
        var tree =
            Tree(
                """
                suite =
                    Test.describe "suite"
                        [ Test.describe "empty" []
                        , Test.test "crash" (\_ -> case [] of
                            _ :: _ -> Expect.pass
                          )
                        ]
                """);

        using var service = new LanguageServerTestingService((_, _) => Task.FromResult(Snapshot(tree)));
        var discovery = await service.DiscoverAsync(new(VirtualWorkspace.RootUri));
        discovery.Complete.Should().BeTrue(discovery.Error?.Message);
        var run = await service.RunAsync(new(VirtualWorkspace.RootUri, discovery.Version, "errors"));
        run.Status.Should().Be("completed", run.Error?.Message);
        run.Results.Select(result => result.State).Should().Equal("failed", "errored");
        run.Results[0].Messages.Single().Message.Should().Contain("contains no tests");
        run.Results[1].Messages.Single().Message.Should().Contain("evaluating");
    }

    [Fact]
    public async Task Testing_workspace_capture_includes_declared_parent_sources_outside_the_open_folder()
    {
        const string helper =
            "module Helpers exposing (make)\nimport Test\nimport Expect\nmake = Test.test \"parent\" (\\_ -> Expect.pass)\n";

        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["app", "elm.json"], Manifest.Replace("\"src\"", "\"../shared\"")),
                    (["app", "tests", "Tests.elm"], "module Tests exposing (suite)\nimport Helpers\nsuite = Helpers.make\n"),
                    (["shared", "Helpers.elm"], helper)
                ]);

        using var server = CreateServer(workspace);
        var project = VirtualWorkspace.RootUri + "app/";
        server.Initialize(InitializeParameters() with { RootUri = project, WorkspaceFolders = [new(project, "app")] });
        var discovery = await server.TestsDiscoverAsync(new(project));
        discovery.Complete.Should().BeTrue(discovery.Error?.Message);
        discovery.SourceRootUri.Should().Be(VirtualWorkspace.RootUri);
        discovery.Items!.Single().Label.Should().Be("parent");
        discovery.Items!.Single().Location.Uri.Should().Be(VirtualWorkspace.RootUri + "shared/Helpers.elm");
    }

    [Fact]
    public async Task Testing_many_projects_share_a_source_version_without_evicting_each_other()
    {
        var files =
            Enumerable.Range(0, LanguageServerTestingService.RetainedVersions + 2).SelectMany(
                index =>
                new (IReadOnlyList<string>, ReadOnlyMemory<byte>)[]
                {
                    ([index.ToString(), "elm.json"], Encoding.UTF8.GetBytes(Manifest)),
                    ([index.ToString(), "tests", "Tests.elm"], Encoding.UTF8.GetBytes(Module("suite = Test.todo \"later\"")))
                }).ToArray();

        var tree = FileTree.FromSetOfFilesWithStringPath(files);
        using var service = new LanguageServerTestingService((_, _) => Task.FromResult(Snapshot(tree)));
        var first = await service.DiscoverAsync(new(VirtualWorkspace.RootUri + "0/"));

        for (var index = 1; index < LanguageServerTestingService.RetainedVersions + 2; ++index)
        {
            var discovery = await service.DiscoverAsync(new(VirtualWorkspace.RootUri + index + "/", first.Version));
            discovery.Complete.Should().BeTrue(discovery.Error?.Message);
        }

        var run = await service.RunAsync(new(first.ProjectUri, first.Version, "first-project"));
        run.Status.Should().Be("completed", run.Error?.Message);
        run.Results.Should().ContainSingle().Which.State.Should().Be("skipped");
    }

    public static InitializeParams InitializeParameters() =>
        new(
            ProcessId: null,
            Capabilities: new(null, null, new(new(1))),
            RootPath: null,
            RootUri: VirtualWorkspace.RootUri,
            WorkspaceFolders: [new(VirtualWorkspace.RootUri, "tests")],
            ClientInfo: null);

    public static ElmLanguageServer CreateServer(
        ILanguageServerWorkspace workspace,
        IElmTestCommitSource? commitSource = null) =>
        new(
            StubLanguageServiceSessionFactory.WithSession(new RecordingLanguageServiceSession()),
            workspace,
            EmptyElmPackageSource.Instance,
            new StubDiagnosticsProvider(),
            new StubDocumentFormatter(source => Result<DocumentFormattingError, string>.ok(source)),
            new LanguageServerOptions("test"),
            testCommitSource: commitSource);
}
