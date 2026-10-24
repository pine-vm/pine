using AwesomeAssertions;
using Pine.CLI;
using Pine.CLI.Elm;
using Pine.Core.Files;
using Pine.Core.Functional;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public sealed class GitBranchResolutionCacheTests : IDisposable
{
    private readonly string _directory =
        Path.Combine(Path.GetTempPath(), "pine-git-cache-tests-" + Guid.NewGuid().ToString("N"));

    [Fact]
    public void Git_branch_resolution_logs_fresh_memory_and_disk_mappings_but_not_full_commits()
    {
        const string repository = "https://github.com/owner/repo";
        var commit = new string('a', 40);
        var calls = 0;
        using var output = new StringWriter();

        string Resolve(string _, string __)
        {
            ++calls;
            return commit;
        }

        var cache = new GitBranchResolutionCache(_directory, Resolve);

        cache.Pin(repository + ".git", "main", diagnostics: output).Should().Be(commit);
        cache.Pin(repository, "main", diagnostics: output).Should().Be(commit);

        new GitBranchResolutionCache(_directory, Resolve)
            .PinUrl(repository + "/tree/main/src", diagnostics: output)
            .Should().Be(repository + "/tree/" + commit + "/src");

        cache.Pin(repository, "other", diagnostics: output).Should().Be(commit);

        output.ToString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries).Should().Equal(
            "Resolved Git branch 'main' in " + repository + " to commit " + commit + ".",
            "Resolved Git branch 'main' in " + repository + " to commit " + commit + ".",
            "Resolved Git branch 'main' in " + repository + " to commit " + commit + ".",
            "Resolved Git branch 'other' in " + repository + " to commit " + commit + ".");

        calls.Should().Be(2);

        output.GetStringBuilder().Clear();
        cache.Pin(repository, commit, diagnostics: output).Should().Be(commit);
        output.ToString().Should().BeEmpty();
    }

    [Fact]
    public void Git_branch_cache_refreshes_after_a_day_and_never_falls_back_on_failure()
    {
        var now = new DateTimeOffset(2026, 10, 16, 12, 0, 0, TimeSpan.Zero);
        var calls = 0;

        string Resolve(string _, string __)
        {
            ++calls;

            if (calls > 1)
                throw new IOException("network unavailable");

            return new string('a', 40);
        }

        var cache = new GitBranchResolutionCache(_directory, Resolve, () => now);
        cache.Pin("https://github.com/owner/repo", "main").Should().Be(new string('a', 40));
        now += TimeSpan.FromHours(23);

        new GitBranchResolutionCache(_directory, Resolve, () => now).Pin("https://github.com/owner/repo", "main")
            .Should().Be(new string('a', 40));

        calls.Should().Be(1);
        now += TimeSpan.FromHours(1);

        var refresh =
            () => new GitBranchResolutionCache(_directory, Resolve, () => now).Pin("https://github.com/owner/repo", "main");

        refresh.Should().Throw<IOException>().WithMessage("network unavailable");
    }

    [Fact]
    public void Git_branch_cache_refresh_publishes_complete_entries_and_removes_old_entries()
    {
        var now = DateTimeOffset.UtcNow;
        var current = new string('a', 40);

        new GitBranchResolutionCache(_directory, (_, _) => current, () => now).Pin(
            "https://github.com/owner/repo",
            "main");

        now += TimeSpan.FromDays(2);
        current = new string('b', 40);

        new GitBranchResolutionCache(_directory, (_, _) => current, () => now).Pin(
            "https://github.com/owner/repo",
            "main");

        var entries = Directory.GetFiles(_directory, "*", SearchOption.AllDirectories);
        entries.Should().ContainSingle();
        File.ReadAllText(entries[0]).Should().Be(current);
        Path.GetFileName(entries[0]).Should().Be(now.UtcDateTime.ToString("yyyy-MM-ddTHH-mm-ssZ"));
    }

    [Theory]
    [InlineData("https://github.com/owner/repo/tree/main/src", "https://github.com/owner/repo/tree/")]
    [InlineData("https://github.com/owner/repo/blob/main/Main.elm", "https://github.com/owner/repo/blob/")]
    [InlineData("https://gitlab.com/owner/repo/-/tree/main/src", "https://gitlab.com/owner/repo/-/tree/")]
    [InlineData("http://gitlab.com/owner/repo/-/blob/main/Main.elm", "http://gitlab.com/owner/repo/-/blob/")]
    public void Git_branch_cache_pins_file_and_directory_urls(string source, string prefix)
    {
        var cache =
            new GitBranchResolutionCache(
                _directory,
                (repository, branch) =>
                {
                    repository.Should().EndWith("/owner/repo");
                    branch.Should().Be("main");
                    return new string('b', 40);
                });

        cache.PinUrl(source).Should().StartWith(prefix + new string('b', 40) + "/");
    }

    [Fact]
    public void Git_source_loaders_share_one_pin_per_invocation_and_refresh_on_the_next_invocation()
    {
        const string repository = "https://github.com/owner/repo";
        var now = DateTimeOffset.UtcNow;
        var calls = 0;
        string Resolve(string _, string __) => new(++calls == 1 ? 'a' : 'b', 40);
        using var output = new StringWriter();
        var cache = new GitBranchResolutionCache(_directory, Resolve, () => now, output);

        var files =
            new Dictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>
            {
                [new[] { "elm.json" }] =
                Encoding.UTF8.GetBytes(
                    """{"type":"application","source-directories":["src"],"elm-version":"0.19.1","dependencies":{"direct":{"elm/core":"1.0.5"},"indirect":{}},"test-dependencies":{"direct":{},"indirect":{}}}"""),
                [new[] { "src", "Main.elm" }] =
                Encoding.UTF8.GetBytes(
                    "module Main exposing (main)\n\nmain : String -> Int\nmain text = String.length text\n")
            };

        var console = AnsiConsole.Create(new AnsiConsoleSettings { Out = new AnsiConsoleOutput(output) });

        using (GitBranchResolutionCache.BeginScope(cache))
        {
            // The common loader used by describe, compile, interactive, make, deploy, and server commands.
            var result =
                LoadFromGitHubOrGitLab.LoadFromUrl(
                    repository + "/blob/main/src/Main.elm",
                    request =>
                    {
                        request.commit.Should().Be(new string('a', 40));

                        return
                            Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>.err(
                                "Stop before content transport.");
                    });

            result.IsErrOrNull().Should().Be("Stop before content transport.");
            now += TimeSpan.FromDays(2);

            using var project =
                ElmTestProjectSource.Resolve(
                    repository + "/tree/main",
                    false,
                    console,
                    pinned =>
                    {
                        pinned.Should().Be(repository + "/tree/" + new string('a', 40) + "/");
                        return files;
                    });

            File.Exists(Path.Combine(project.DirectoryPath, "elm.json")).Should().BeTrue();

            var fixture =
                new GitRepositoryFixture(
                    FileTree.FromSetOfFilesWithStringPath(files.Select(file => (file.Key, file.Value))));

            var runSource =
                new RunSource.Loader(
                    _directory,
                    output,
                    openRemote:
                    (url, pinned, _) =>
                    {
                        url.Should().Be(repository + ".git");
                        pinned.Should().Be(new string('a', 40));
                        return fixture.Open();
                    }).Read(repository + "/blob/main/src/Main.elm/main", null, default);

            runSource.Declaration.Should().Be("main");

            LoadFromGitHubOrGitLab.GetCommitFromReference(repository + ".git", "refs/heads/main")
                .IsOkOrNull().Should().Be(new string('a', 40));

            calls.Should().Be(1);

            output.ToString().Split(Environment.NewLine)
                .Count(
                line =>
                line == "Resolved Git branch 'main' in " + repository + " to commit " + new string('a', 40) + ".")
                .Should().Be(4);
        }

        using (GitBranchResolutionCache.BeginScope(new(_directory, Resolve, () => now)))
        {
            LoadFromGitHubOrGitLab.GetCommitFromReference(repository + ".git", "refs/heads/main")
                .IsOkOrNull().Should().Be(new string('b', 40));
        }

        calls.Should().Be(2);
    }

    [Fact]
    public void Git_shared_loader_fails_without_fetching_content_when_expired_verification_fails()
    {
        const string repository = "https://github.com/owner/repo";
        var now = DateTimeOffset.UtcNow;
        new GitBranchResolutionCache(_directory, (_, _) => new string('a', 40), () => now).Pin(repository, "main");
        now += TimeSpan.FromDays(2);

        using var scope =
            GitBranchResolutionCache.BeginScope(
                new(_directory, (_, _) => throw new IOException("network unavailable"), () => now));

        var result =
            LoadFromGitHubOrGitLab.LoadFromUrl(
                repository + "/tree/main/src",
                _ => throw new InvalidOperationException("Content must not be fetched."));

        result.IsErrOrNull().Should().Contain("network unavailable");
    }

    [Fact]
    public void Git_full_commits_bypass_resolution_and_only_full_hashes_are_recognized()
    {
        var commit = new string('A', 40);

        var cache =
            new GitBranchResolutionCache(
                _directory,
                (_, _) => throw new InvalidOperationException("No branch query expected."));

        using var scope = GitBranchResolutionCache.BeginScope(cache);

        cache.PinUrl("https://github.com/owner/repo/tree/" + commit).Should()
            .Be("https://github.com/owner/repo/tree/" + commit.ToLowerInvariant() + "/");

        LoadFromGitHubOrGitLab.GetCommitFromReference("https://github.com/owner/repo.git", commit)
            .IsOk().Should().BeTrue();

        LoadFromGitHubOrGitLab.RefLooksLikeCommit(commit + "1").Should().BeFalse();
        Directory.Exists(_directory).Should().BeFalse();
    }

    [Fact]
    public void Git_branch_scope_restores_the_previous_scope_and_parallel_requests_resolve_once()
    {
        var calls = 0;

        var outer =
            new GitBranchResolutionCache(
                _directory,
                (_, _) =>
                {
                    Interlocked.Increment(ref calls);
                    return new string('a', 40);
                });

        using var scope = GitBranchResolutionCache.BeginScope(outer);

        using (GitBranchResolutionCache.BeginScope(new(_directory)))
            GitBranchResolutionCache.Current.Should().NotBeSameAs(outer);

        GitBranchResolutionCache.Current.Should().BeSameAs(outer);
        Parallel.For(0, 16, _ => GitBranchResolutionCache.Current.Pin("https://github.com/owner/repo", "main"));
        calls.Should().Be(1);
    }

    public void Dispose()
    {
        if (Directory.Exists(_directory))
            Directory.Delete(_directory, recursive: true);
    }
}
