using AwesomeAssertions;
using Pine.CLI;
using Pine.Core;
using Pine.Core.CLI;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using Xunit;

namespace Pine.IntegrationTests;

public sealed class GitSourceLoadingTests : IDisposable
{
    private readonly string _directory =
        Path.Combine(Path.GetTempPath(), "pine-git-loading-tests-" + Guid.NewGuid().ToString("N"));

    private const string Manifest =
        """{"type":"application","source-directories":["src"],"elm-version":"0.19.1","dependencies":{"direct":{"elm/core":"1.0.5"},"indirect":{}},"test-dependencies":{"direct":{},"indirect":{}}}""";

    [Fact]
    public void Git_cached_contents_are_used_without_fetching_blobs_for_provenance()
    {
        var tree = Tree(("src/A.elm", "module A exposing (..)\nvalue = 1\n"), ("unrelated.txt", "unrelated"));
        var fixture = new GitRepositoryFixture(tree);
        var cached = FileTreeExtensions.ToFlatDictionaryWithPathComparer(tree.EnumerateFilesTransitive());
        var result = Load(fixture, "blob", "src/A.elm", cached, blobCacheDirectory: _directory);
        result.IsErrOrNull().Should().BeNull();
        var loaded = result.Extract(error => throw new Exception(error));
        loaded.tree.Should().Be(tree.GetNodeAtPath(["src", "A.elm"]));
        loaded.rootCommit.hash.Should().Be(fixture.Commit);
        loaded.firstParentCommitWithSameTree.hash.Should().Be(fixture.Commit);
        fixture.Open(blobCacheDirectory: _directory).ReadPath(["src", "A.elm"]).Should().Be(loaded.tree);
        fixture.Requests.Should().BeEmpty();
    }

    [Fact]
    public void Git_tree_loading_fetches_only_missing_selected_blobs_and_deduplicates_them()
    {
        var tree =
            Tree(
                ("src/A.elm", "cached"),
                ("src/B.elm", "missing"),
                ("src/Again.elm", "missing"),
                ("unrelated.bin", "not requested"));

        var fixture = new GitRepositoryFixture(tree);

        var cached =
            FileTreeExtensions.ToFlatDictionaryWithPathComparer(
                new[] { ((IReadOnlyList<string>)new[] { "src", "A.elm" }, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes("cached")) });

        var result = Load(fixture, "tree", "src", cached);
        result.IsErrOrNull().Should().BeNull();
        result.Extract(error => throw new Exception(error)).tree.Should().Be(tree.GetNodeAtPath(["src"]));
        fixture.Requests.Should().ContainSingle().Which.Should().ContainSingle();
        fixture.DownloadedBytes.Should().Be(Encoding.UTF8.GetByteCount("missing"));
    }

    [Fact]
    public void Git_default_cold_loader_does_not_prefetch_the_entire_commit()
    {
        var fixture = new GitRepositoryFixture(Tree(("selected.txt", "selected"), ("unrelated.bin", "unrelated")));

        var result =
            LoadFromGitHubOrGitLab.LoadFromUrl(
                Url(fixture, "selected.txt"),
                getRepositoryFilesPartialForCommit: null,
                openRepository: (_, _, files) => fixture.Open(files));

        result.IsErrOrNull().Should().BeNull();

        result.Extract(error => throw new Exception(error)).tree.Should().Be(
            FileTree.File(Encoding.UTF8.GetBytes("selected")));

        fixture.RequestedPaths.Should().Equal("selected.txt");
    }

    [Fact]
    public void Git_blob_disk_cache_is_reused_by_a_fresh_repository_session()
    {
        var fixture = new GitRepositoryFixture(Tree(("selected.txt", "selected"), ("unrelated.bin", "unrelated")));
        var first = fixture.Open(blobCacheDirectory: _directory);
        first.ReadPath(["selected.txt"]).Should().Be(FileTree.File(Encoding.UTF8.GetBytes("selected")));
        var second = fixture.Open(blobCacheDirectory: _directory);
        second.ReadPath(["selected.txt"]).Should().Be(FileTree.File(Encoding.UTF8.GetBytes("selected")));
        fixture.RequestedPaths.Should().Equal("selected.txt");
        Directory.GetFiles(_directory).Should().ContainSingle();
    }

    [Fact]
    public void Git_cached_contents_are_still_checked_against_tree_blob_hashes()
    {
        var fixture = new GitRepositoryFixture(Tree(("selected.txt", "correct")));

        var cached =
            FileTreeExtensions.ToFlatDictionaryWithPathComparer(
                new[] { ((IReadOnlyList<string>)new[] { "selected.txt" }, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes("corrupt")) });

        Load(fixture, "blob", "selected.txt", cached).IsErrOrNull().Should().Contain("integrity check failed");
        fixture.Requests.Should().BeEmpty();
    }

    [Fact]
    public void Git_fetched_blob_integrity_is_checked_before_publishing_to_cache()
    {
        var fixture = new GitRepositoryFixture(Tree(("selected.txt", "correct")));

        var reader =
            new GitRemoteRepository(
                fixture.Metadata,
                fixture.Commit,
                hashes =>
                hashes.ToDictionary(hash => hash, _ => (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes("corrupt")),
                blobCacheDirectory: _directory);

        var read = () => reader.ReadPath(["selected.txt"]);
        read.Should().Throw<InvalidDataException>().WithMessage("*integrity check failed*");
        Directory.Exists(_directory).Should().BeFalse();
    }

    [Fact]
    public void Run_remote_discovery_fetches_only_the_governing_project_sources()
    {
        var tree =
            Tree(
                ("project/elm.json", Manifest),
                ("project/src/Main.elm", "module Main exposing (main)\nimport Helper\nmain : String -> Int\nmain text = Helper.size text\n"),
                ("project/src/Helper.elm", "module Helper exposing (size)\nsize text = String.length text\n"),
                ("project/examples/Unrelated.elm", "module Unrelated exposing (value)\nvalue = 42\n"),
                ("project/src/nested/elm.json", Manifest),
                ("project/src/nested/src/Nested.elm", "module Nested exposing (value)\nvalue = 43\n"),
                ("other/Other.elm", "module Other exposing (value)\nvalue = 44\n"));

        tree = tree.SetNodeAtPathSorted(["large.bin"], FileTree.File(new byte[1024 * 1024]));
        var fixture = new GitRepositoryFixture(tree);
        var source = Loader(fixture).Read(Url(fixture, "project/src/Main.elm/main"), null, default);
        source.FilePath.Should().Equal("src", "Main.elm");

        source.Tree.EnumerateFilesTransitive().Select(file => string.Join("/", file.path))
            .Should().BeEquivalentTo("elm.json", "src/Main.elm", "src/Helper.elm");

        fixture.RequestedPaths.Should().BeEquivalentTo(
            "project/elm.json",
            "project/src/Main.elm",
            "project/src/Helper.elm");

        fixture.DownloadedBytes.Should().Be(
            fixture.RequestedPaths.Sum(path => ((FileTree.FileNode)tree.GetNodeAtPath(path.Split('/'))!).Bytes.Length));

        fixture.DownloadedBytes.Should().BeLessThan(1024 * 1024);
        fixture.OpenCount.Should().Be(1);
    }

    [Fact]
    public void Run_remote_standalone_discovery_expands_only_until_imports_are_available()
    {
        var fixture =
            new GitRepositoryFixture(
                Tree(
                    ("sources/nested/Main.elm", "module Main exposing (main)\nimport Helper\nmain text = Helper.size text\n"),
                    ("sources/Helper.elm", "module Helper exposing (size)\nsize text = String.length text\n"),
                    ("unrelated/Unused.elm", "module Unused exposing (value)\nvalue = 42\n"),
                    ("sources/large.bin", "not Elm source")));

        var source = Loader(fixture).Read(Url(fixture, "sources/nested/Main.elm/main"), null, default);
        source.FilePath.Should().Equal("nested", "Main.elm");
        fixture.RequestedPaths.Should().BeEquivalentTo("sources/nested/Main.elm", "sources/Helper.elm");
        fixture.Requests.SelectMany(request => request).Should().OnlyHaveUniqueItems();
    }

    [Fact]
    public void Run_directory_input_is_rejected_without_downloading_its_contents()
    {
        var fixture = new GitRepositoryFixture(Tree(("directory/input.txt", "not needed")));
        var read = () => Loader(fixture).ReadInput(Url(fixture, "directory"), default);
        read.Should().Throw<IOException>().WithMessage("*must select a file*");
        fixture.Requests.Should().BeEmpty();
    }

    [Fact]
    public void Run_remote_discovery_includes_declared_parent_sources_without_other_projects()
    {
        var fixture =
            new GitRepositoryFixture(
                Tree(
                    ("project/elm.json", Manifest.Replace("[\"src\"]", "[\"src\",\"../shared\"]", StringComparison.Ordinal)),
                    ("project/src/Main.elm", "module Main exposing (main)\nimport Helper\nmain : String -> Int\nmain text = Helper.size text\n"),
                    ("shared/Helper.elm", "module Helper exposing (size)\nsize : String -> Int\nsize text = String.length text\n"),
                    ("other/Other.elm", "module Other exposing (value)\nvalue = 42\n")));

        using var output = new StringWriter();
        using var errors = new StringWriter();

        RunCommand.Execute(
            Url(fixture, "project/src/Main.elm/main"),
            input: "hello",
            color: CommandLineColorMode.Never,
            output: output,
            error: errors,
            cacheDirectory: _directory,
            sourceLoader: Loader(fixture),
            timeout: TimeSpan.FromSeconds(20)).Should().Be(0, errors.ToString());

        output.ToString().Should().Be("5" + Environment.NewLine);
        fixture.RequestedPaths.Should().BeEquivalentTo("project/elm.json", "project/src/Main.elm", "shared/Helper.elm");
    }

    [Fact]
    public void Git_cancellation_after_fetch_does_not_publish_blob_contents()
    {
        using var cancelled = new CancellationTokenSource();
        var fixture = new GitRepositoryFixture(Tree(("selected.txt", "correct"))) { BeforeFetch = cancelled.Cancel };
        var reader = fixture.Open(blobCacheDirectory: _directory);
        var read = () => reader.ReadPath(["selected.txt"], cancelled.Token);
        read.Should().Throw<OperationCanceledException>();
        Directory.Exists(_directory).Should().BeFalse();
    }

    private RunSource.Loader Loader(GitRepositoryFixture fixture) =>
        new(
            _directory,
            TextWriter.Null,
            openRemote:
            (_, commit, _) =>
            {
                commit.Should().Be(fixture.Commit);
                return fixture.Open();
            });

    private static string Url(GitRepositoryFixture fixture, string path) =>
        "https://github.com/owner/repo/blob/" + fixture.Commit + "/" + path;

    private static FileTree Tree(params (string Path, string Text)[] files) =>
        FileTree.FromSetOfFilesWithStringPath(
            files.Select(
                file =>
                ((IReadOnlyList<string>)file.Path.Split('/'), (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(file.Text))));

    private static Result<string, LoadFromGitHubOrGitLab.LoadFromUrlSuccess> Load(
        GitRepositoryFixture fixture,
        string kind,
        string path,
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> cached,
        string? blobCacheDirectory = null) =>
        LoadFromGitHubOrGitLab.LoadFromUrl(
            "https://github.com/owner/repo/" + kind + "/" + fixture.Commit + "/" + path,
            _ => Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.ok(cached),
            (_, _, files) => fixture.Open(files, blobCacheDirectory));

    public void Dispose()
    {
        if (Directory.Exists(_directory))
            Directory.Delete(_directory, recursive: true);
    }
}
