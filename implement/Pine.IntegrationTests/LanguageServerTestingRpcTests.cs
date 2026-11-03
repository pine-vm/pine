using AwesomeAssertions;
using Nerdbank.Streams;
using Pine.Core.IO;
using Pine.Core.LanguageServerProtocol;
using Pine.Core.Tests.Elm.LanguageServer;
using Pine.Elm;
using Pine.Elm.LanguageServerAdapters;
using StreamJsonRpc;
using System;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using ElmLanguageServer = Pine.Core.Elm.LanguageServer.LanguageServer;

namespace Pine.IntegrationTests;

public class LanguageServerTestingRpcTests
{
    [Fact]
    public async Task Testing_RPC_discovers_and_runs_real_elm_in_elm_projects()
    {
        var implementDirectory = new DirectoryInfo(AppContext.BaseDirectory);

        while (implementDirectory.Name != "implement")
        {
            implementDirectory =
                implementDirectory.Parent ??
                throw new InvalidOperationException("Could not locate implement directory.");
        }

        var directory = Path.Combine(implementDirectory.FullName, "elm-in-elm");
        var root = new Uri(directory + Path.DirectorySeparatorChar).AbsoluteUri;
        using var server = LanguageServerTestingTests.CreateServer(new FileSystemWorkspace());
        using var connection = new RpcConnection(server);

        await connection.Client.InvokeWithParameterObjectAsync<InitializeResult>(
            "initialize",
            LanguageServerTestingTests.InitializeParameters() with
            {
                RootUri = root.TrimEnd('/'),
                WorkspaceFolders = [new(root.TrimEnd('/'), "elm-in-elm")]
            });

        foreach (var project in new[] { root, root + "pine-elm-syntax/" })
        {
            var discovery =
                await connection.Client.InvokeWithParameterObjectAsync<PineTestsDiscoverResult>(
                    "pine/tests/discover",
                    new PineTestsDiscoverParams(project));

            discovery.Complete.Should().BeTrue(discovery.Error?.Message);
            discovery.Items.Should().NotBeEmpty();

            discovery.Items!.Should().OnlyContain(
                item => item.Location.Range.End.Line >= item.Location.Range.Start.Line);

            var run =
                await connection.Client.InvokeWithParameterObjectAsync<PineTestsRunResult>(
                    "pine/tests/run",
                    new PineTestsRunParams(project, discovery.Version, project == root ? "real-root" : "real-syntax"));

            run.Status.Should().Be("completed", run.Error?.Message);
            run.Results.Should().NotBeEmpty();
            run.Results.Should().OnlyContain(result => result.State == "passed");

            run.Results.Count.Should().Be(
                discovery.Items!.Count(item => item.Kind is not Core.Elm.Testing.ElmTestDiscoveryItemKind.Group));
        }
    }

    [Theory]
    [InlineData("sha1")]
    [InlineData("sha256")]
    public async Task Testing_RPC_discovers_and_runs_exact_commits_without_changing_the_working_tree(
        string objectFormat)
    {
        var directory =
            Path.GetFullPath(Path.Combine("artifacts", "testing-git", Guid.NewGuid().ToString("N") + " space #"));

        Directory.CreateDirectory(Path.Combine(directory, "app", "tests"));

        try
        {
            await Git(directory, "init", "--initial-branch=main", "--object-format=" + objectFormat);

            await File.WriteAllTextAsync(
                Path.Combine(directory, "app", "elm.json"),
                LanguageServerTestingTests.Manifest);

            var sourcePath = Path.Combine(directory, "app", "tests", "Tests.elm");
            var original = LanguageServerTestingTests.Module("""suite = Test.test "old" (\_ -> Expect.pass)""");
            await File.WriteAllTextAsync(sourcePath, original);
            await Commit(directory, "Old version");
            var oldCommit = (await Git(directory, "rev-parse", "HEAD")).Trim();

            await File.WriteAllTextAsync(
                sourcePath,
                LanguageServerTestingTests.Module("""suite = Test.test "new" (\_ -> Expect.equal 1 2)"""));

            await Commit(directory, "New version");
            var newCommit = (await Git(directory, "rev-parse", "HEAD")).Trim();

            await File.WriteAllTextAsync(
                sourcePath,
                LanguageServerTestingTests.Module("""suite = Test.todo "working" """));

            var initialStatus = await Git(directory, "status", "--porcelain");
            var projectUri = new Uri(Path.Combine(directory, "app") + Path.DirectorySeparatorChar).AbsoluteUri;

            using var server =
                LanguageServerTestingTests.CreateServer(new FileSystemWorkspace(), new GitElmTestCommitSource());

            using var connection = new RpcConnection(server);

            var initialization =
                await connection.Client.InvokeWithParameterObjectAsync<InitializeResult>(
                    "initialize",
                    LanguageServerTestingTests.InitializeParameters() with
                    {
                        RootUri = new Uri(directory + Path.DirectorySeparatorChar).AbsoluteUri,
                        WorkspaceFolders =
                        [
                        new(new Uri(directory + Path.DirectorySeparatorChar).AbsoluteUri, "fixture")
                        ]
                    });

            initialization.Capabilities.Experimental!.PineTests!.VersionKinds.Should().Contain("commit");

            var old =
                await connection.Client.InvokeWithParameterObjectAsync<PineTestsDiscoverResult>(
                    "pine/tests/discover",
                    new PineTestsDiscoverParams(projectUri, "commit-" + oldCommit));

            var updated =
                await connection.Client.InvokeWithParameterObjectAsync<PineTestsDiscoverResult>(
                    "pine/tests/discover",
                    new PineTestsDiscoverParams(projectUri, "commit-" + newCommit));

            old.Complete.Should().BeTrue(old.Error?.Message);
            updated.Complete.Should().BeTrue(updated.Error?.Message);
            old.Items!.Single().Label.Should().Be("old");
            updated.Items!.Single().Label.Should().Be("new");
            old.Version.Should().Be("commit-" + oldCommit);
            updated.Version.Should().Be("commit-" + newCommit);
            old.TreeHash.Should().NotBe(updated.TreeHash);

            var wire =
                await connection.Client.InvokeWithParameterObjectAsync<JsonElement>(
                    "pine/tests/discover",
                    new PineTestsDiscoverParams(projectUri, old.Version));

            wire.GetProperty("items")[0].GetProperty("kind").GetString().Should().Be("Unit");
            wire.GetProperty("items")[0].GetProperty("selection").GetString().Should().Be("Included");

            var results =
                await Task.WhenAll(
                    connection.Client.InvokeWithParameterObjectAsync<PineTestsRunResult>(
                        "pine/tests/run",
                        new PineTestsRunParams(projectUri, old.Version, "old")),
                    connection.Client.InvokeWithParameterObjectAsync<PineTestsRunResult>(
                        "pine/tests/run",
                        new PineTestsRunParams(projectUri, updated.Version, "new")));

            results[0].Results.Single().State.Should().Be("passed");
            results[1].Results.Single().State.Should().Be("failed");
            results[1].Results.Single().Messages.Single().Actual.Should().Be("2");
            results[1].ResolutionFingerprint.Should().NotBeNullOrEmpty();

            connection.Receiver.Events.Where(item => item.RunId == "old").Should().OnlyContain(
                item => item.Version == old.Version);

            connection.Receiver.Events.Where(item => item.RunId == "new").Should().OnlyContain(
                item => item.Version == updated.Version);

            var source =
                await connection.Client.InvokeWithParameterObjectAsync<PineTestsSourceResult>(
                    "pine/tests/source",
                    new PineTestsSourceParams(projectUri, old.Version, old.Items![0].SourcePath));

            source.Text.Should().Be(original);
            (await Git(directory, "status", "--porcelain")).Should().Be(initialStatus);

            var current =
                await connection.Client.InvokeWithParameterObjectAsync<PineTestsDiscoverResult>(
                    "pine/tests/discover",
                    new PineTestsDiscoverParams(projectUri));

            current.Version.Should().StartWith("file-tree-");
            current.Items!.Single().Label.Should().Be("working");

            var unavailable =
                () => connection.Client.InvokeWithParameterObjectAsync<PineTestsDiscoverResult>(
                    "pine/tests/discover",
                    new PineTestsDiscoverParams(projectUri, "commit-" + new string('0', 40)));

            (await unavailable.Should().ThrowAsync<RemoteInvocationException>()).Which.ErrorCode.Should().Be(-32001);

            var treeObject = (await Git(directory, "rev-parse", oldCommit + "^{tree}")).Trim();

            var notACommit =
                () => connection.Client.InvokeWithParameterObjectAsync<PineTestsDiscoverResult>(
                    "pine/tests/discover",
                    new PineTestsDiscoverParams(projectUri, "commit-" + treeObject));

            (await notACommit.Should().ThrowAsync<RemoteInvocationException>()).Which.Message.Should().Contain(
                "not a Git commit");

            Directory.Delete(Path.Combine(directory, "app"), recursive: true);

            using var historicalServer =
                LanguageServerTestingTests.CreateServer(new FileSystemWorkspace(), new GitElmTestCommitSource());

            historicalServer.Initialize(LanguageServerTestingTests.InitializeParameters());
            var deletedProject = await historicalServer.TestsDiscoverAsync(new(projectUri, old.Version));
            deletedProject.Complete.Should().BeTrue(deletedProject.Error?.Message);
            deletedProject.Items!.Single().Label.Should().Be("old");

            foreach (var objectPath in Directory.EnumerateFiles(
                Path.Combine(directory, ".git", "objects"),
                "*",
                SearchOption.AllDirectories))
            {
                File.SetAttributes(objectPath, File.GetAttributes(objectPath) | FileAttributes.ReadOnly);
            }
        }
        finally
        {
            Filesystem.DeleteLocalDirectoryRecursive(directory);
        }

        Directory.Exists(directory).Should().BeFalse();
    }

    [Fact]
    public async Task Testing_RPC_cancellation_is_standard_LSP_and_does_not_block_other_requests()
    {
        var tree =
            LanguageServerTestingTests.Tree(
                """
                loop _ = loop ()
                suite = Test.test "loop" (\_ -> loop ())
                """);

        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    .. tree.EnumerateFilesTransitive().Select(
                        file => (file.path.ToArray(), System.Text.Encoding.UTF8.GetString(file.fileContent.Span)))
                ]);

        using var server = LanguageServerTestingTests.CreateServer(workspace);
        using var connection = new RpcConnection(server);

        await connection.Client.InvokeWithParameterObjectAsync<InitializeResult>(
            "initialize",
            LanguageServerTestingTests.InitializeParameters());

        var discovered =
            await connection.Client.InvokeWithParameterObjectAsync<PineTestsDiscoverResult>(
                "pine/tests/discover",
                new PineTestsDiscoverParams(VirtualWorkspace.RootUri));

        discovered.Complete.Should().BeTrue(discovered.Error?.Message);
        using var cancellation = new CancellationTokenSource();

        var run =
            connection.Client.InvokeWithParameterObjectAsync<PineTestsRunResult>(
                "pine/tests/run",
                new PineTestsRunParams(VirtualWorkspace.RootUri, discovered.Version, "loop"),
                cancellationToken: cancellation.Token);

        await connection.Receiver.Started.Task.WaitAsync(TimeSpan.FromSeconds(60));

        var concurrentDiscovery =
            await connection.Client.InvokeWithParameterObjectAsync<PineTestsDiscoverResult>(
                "pine/tests/discover",
                new PineTestsDiscoverParams(VirtualWorkspace.RootUri, discovered.Version))
            .WaitAsync(TimeSpan.FromSeconds(60));

        concurrentDiscovery.Version.Should().Be(discovered.Version);
        run.IsCompleted.Should().BeFalse();
        cancellation.Cancel();
        var cancelled = await run.WaitAsync(TimeSpan.FromSeconds(60));
        cancelled.Status.Should().Be("cancelled");
        await connection.Receiver.Ended.Task.WaitAsync(TimeSpan.FromSeconds(60));
        connection.Receiver.Events.Last().State.Should().Be("cancelled");

        connection.Receiver.Events.Where(item => item.Result is not null).Should().ContainSingle()
            .Which.Result!.State.Should().Be("skipped");
    }

    private static Task<string> Commit(string directory, string message) =>
        Git(
            directory,
            "-c",
            "user.name=Test fixture",
            "-c",
            "user.email=fixture@example.invalid",
            "commit",
            "-am",
            message + "\n\nCo-authored-by: Copilot <223556219+Copilot@users.noreply.github.com>");

    private static async Task<string> Git(string directory, params string[] arguments)
    {
        if (arguments.Contains("commit"))
            await Git(directory, "add", ".");

        var start =
            new ProcessStartInfo("git")
            {
                WorkingDirectory = directory,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };

        foreach (var argument in arguments)
            start.ArgumentList.Add(argument);

        using var process = Process.Start(start) ?? throw new InvalidOperationException("Failed starting Git fixture.");
        var stdout = process.StandardOutput.ReadToEndAsync();
        var stderr = process.StandardError.ReadToEndAsync();
        await process.WaitForExitAsync();
        var output = await stdout;

        if (process.ExitCode != 0)
            throw new InvalidOperationException("Git fixture failed: " + await stderr);

        await stderr;
        return output;
    }

    public sealed class Receiver
    {
        public ConcurrentQueue<PineTestsEvent> Events { get; } = new();

        public TaskCompletionSource Started { get; } = new(TaskCreationOptions.RunContinuationsAsynchronously);

        public TaskCompletionSource Ended { get; } = new(TaskCreationOptions.RunContinuationsAsynchronously);

        [JsonRpcMethod("pine/tests/event", UseSingleObjectParameterDeserialization = true)]
        public void Event(PineTestsEvent notification)
        {
            Events.Enqueue(notification);

            if (notification.State == "started")
                Started.TrySetResult();

            if (notification.Kind == "end")
                Ended.TrySetResult();
        }

        [JsonRpcMethod("pine/tests/changed", UseSingleObjectParameterDeserialization = true)]
        public void Changed(PineTestsChangedParams notification) { }
    }

    private sealed class RpcConnection : IDisposable
    {
        private readonly JsonRpc _server;

        public JsonRpc Client { get; }

        public Receiver Receiver { get; } = new();

        public RpcConnection(ElmLanguageServer server)
        {
            var (clientStream, serverStream) = FullDuplexStream.CreatePair();
            var target = new LanguageServerRpcTarget(server, LogDelegate: null);

            _server =
                new(
                    new HeaderDelimitedMessageHandler(
                        serverStream,
                        serverStream,
                        LanguageServerRpcTarget.JsonRpcMessageFormatterDefault()),
                    target);

            target.JsonRpc = _server;

            Client =
                new(
                    new HeaderDelimitedMessageHandler(
                        clientStream,
                        clientStream,
                        LanguageServerRpcTarget.JsonRpcMessageFormatterDefault()),
                    Receiver);

            _server.StartListening();
            Client.StartListening();
        }

        public void Dispose()
        {
            Client.Dispose();
            _server.Dispose();
        }
    }
}
