using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Threading;

namespace Pine;

internal sealed class GitBranchResolutionCache(
    string cacheDirectory,
    Func<string, string, string>? resolve = null,
    Func<DateTimeOffset>? clock = null,
    TextWriter? defaultDiagnostics = null)
{
    private static readonly AsyncLocal<GitBranchResolutionCache?> s_current = new();

    private readonly Lock _lock = new();

    private readonly Dictionary<(string Repository, string Branch), string> _resolved = [];

    internal static GitBranchResolutionCache? Active => s_current.Value;

    internal static GitBranchResolutionCache Current =>
        Active ?? new(Filesystem.CacheDirectory);

    internal static IDisposable BeginScope(GitBranchResolutionCache? cache = null)
    {
        var previous = Active;
        s_current.Value = cache ?? Current;
        return new CacheScope(previous);
    }

    private sealed class CacheScope(GitBranchResolutionCache? previous) : IDisposable
    {
        public void Dispose() => s_current.Value = previous;
    }

    internal static bool IsCommit(string value) =>
        value.Length is 40 && value.All(Uri.IsHexDigit);

    public string PinUrl(string source, CancellationToken token = default, TextWriter? diagnostics = null)
    {
        var parsed =
            LoadFromGitHubOrGitLab.ParseUrl(source)
            ?? throw new ArgumentException("Invalid GitHub or GitLab file/directory URL.");

        var location =
            parsed.inRepository
            ?? throw new ArgumentException("Select a file or directory URL containing an explicit branch or full commit ID.");

        var commit = Pin(parsed.repository, Uri.UnescapeDataString(location.@ref), token, diagnostics);

        var pinned =
            LoadFromGitHubOrGitLab.BackToUrl(
                parsed with { inRepository = location with { @ref = commit } });

        return
            source.Contains("/-/" + location.objectType + "/", StringComparison.Ordinal)
            ?
            pinned.Replace(
                "/" + location.objectType + "/",
                "/-/" + location.objectType + "/",
                StringComparison.Ordinal)
            :
            pinned;
    }

    public string Pin(
        string repository,
        string reference,
        CancellationToken token = default,
        TextWriter? diagnostics = null)
    {
        lock (_lock)
        {
            token.ThrowIfCancellationRequested();

            if (IsCommit(reference))
                return reference.ToLowerInvariant();

            repository = repository.TrimEnd('/');

            if (repository.EndsWith(".git", StringComparison.OrdinalIgnoreCase))
                repository = repository[..^4];

            var commit = PinBranch(repository, reference, token);

            (diagnostics ?? defaultDiagnostics ?? Console.Error).WriteLine(
                "Resolved Git branch '" + reference + "' in " + repository + " to commit " + commit + ".");

            return commit;
        }
    }

    private string PinBranch(string repository, string reference, CancellationToken token)
    {
        if (_resolved.TryGetValue((repository, reference), out var pinned))
            return pinned;

        var uri = new Uri(repository);

        var directory =
            Path.Combine(
                cacheDirectory,
                uri.Host,
                uri.AbsolutePath.Trim('/').Replace('/', Path.DirectorySeparatorChar),
                "branch-by-time",
                Convert.ToHexStringLower(SHA256.HashData(Encoding.UTF8.GetBytes(reference))));

        var now = (clock ?? (() => DateTimeOffset.UtcNow))();

        var entries =
            Directory.Exists(directory)
            ?
            Directory.GetFiles(directory)
            .Select(path => (Path: path, Time: ParseTime(Path.GetFileName(path))))
            .Where(entry => entry.Time is not null)
            .OrderByDescending(entry => entry.Time)
            .ToArray()
            :
            [];

        foreach (var entry in entries)
        {
            var age = now - entry.Time!.Value;

            if (age < TimeSpan.Zero || age >= TimeSpan.FromDays(1))
                continue;

            var commit = ReadCommit(entry.Path);

            if (commit is not null && IsCommit(commit))
            {
                _resolved.Add((repository, reference), commit);
                return commit;
            }
        }

        // An expired mapping is never used after failed verification.
        var verified = (resolve ?? ResolveBranch)(repository, reference);
        token.ThrowIfCancellationRequested();

        if (!IsCommit(verified))
            throw new IOException("Remote branch verification did not return a full commit SHA.");

        var verifiedAt = (clock ?? (() => DateTimeOffset.UtcNow))();
        Directory.CreateDirectory(directory);

        AtomicFile.WriteAllBytes(
            Path.Combine(
                directory,
                verifiedAt.UtcDateTime.ToString("yyyy-MM-ddTHH-mm-ssZ", CultureInfo.InvariantCulture)),
            Encoding.ASCII.GetBytes(verified.ToLowerInvariant()),
            token);

        foreach (var entry in entries.Where(entry => verifiedAt - entry.Time > TimeSpan.FromDays(1)))
        {
            File.Delete(entry.Path);
        }

        _resolved.Add((repository, reference), verified.ToLowerInvariant());
        return verified.ToLowerInvariant();
    }

    private static string ResolveBranch(string repository, string branch)
    {
        var uri = new Uri(repository);
        var parts = uri.AbsolutePath.Trim('/').Split('/');

        if (parts.Length is not 2)
            throw new ArgumentException("Expected a GitHub or GitLab owner/repository URL.");

        return
            GitCore.GitSmartHttp.FetchBranchCommitShaAsync(
                uri.GetLeftPart(UriPartial.Authority),
                parts[0],
                parts[1],
                branch).GetAwaiter().GetResult();
    }

    private static string? ReadCommit(string path)
    {
        try
        {
            using var file = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read | FileShare.Delete);
            using var reader = new StreamReader(file, Encoding.ASCII);
            return reader.ReadToEnd().Trim();
        }
        catch (FileNotFoundException)
        {
            // Another invocation may have pruned an entry after enumeration.
            return null;
        }
    }

    private static DateTimeOffset? ParseTime(string name) =>
        DateTimeOffset.TryParseExact(
            name,
            "yyyy-MM-ddTHH-mm-ssZ",
            CultureInfo.InvariantCulture,
            DateTimeStyles.AssumeUniversal | DateTimeStyles.AdjustToUniversal,
            out var time)
        ?
        time
        :
        null;
}
