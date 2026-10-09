using Pine.Core.Elm.Elm019;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text.Json;

namespace Pine.CLI.Elm;

internal sealed class ElmTestProjectSource(string directory, bool ownsDirectory, string? containerDirectory = null) : IDisposable
{
    public string DirectoryPath { get; } = directory;

    internal static bool IsRemote(string source) =>
        source.Contains("://", StringComparison.Ordinal) ||
        source.StartsWith("http:", StringComparison.OrdinalIgnoreCase) ||
        source.StartsWith("https:", StringComparison.OrdinalIgnoreCase);

    internal static string CommandSource(string source) =>
        IsRemote(source) ? source : Path.GetFullPath(source);

    internal static ElmTestProjectSource Resolve(
        string source,
        bool offline,
        IAnsiConsole console,
        Func<string, IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>>? loadRemote = null)
    {
        if (!IsRemote(source))
            return new(source, ownsDirectory: false);

        if (source.Any(char.IsControl) ||
            !Uri.TryCreate(source, UriKind.Absolute, out var uri) ||
            uri.Scheme is not ("http" or "https") ||
            uri.Host is not ("github.com" or "gitlab.com") ||
            !uri.IsDefaultPort ||
            uri.UserInfo.Length > 0 ||
            uri.Query.Length > 0 ||
            uri.Fragment.Length > 0)
        {
            throw new IOException(
                "Remote Elm projects require an HTTP(S) GitHub or GitLab tree URL without credentials, query, or fragment.");
        }

        try
        {
            _ = GitCore.GitSmartHttp.ParseTreeUrl(source);
        }
        catch (Exception exception) when (exception is ArgumentException or InvalidOperationException or NotSupportedException)
        {
            throw new IOException("Invalid GitHub or GitLab project tree URL.");
        }

        if (offline)
            throw new IOException("Cannot load a remote Elm project with --offline. Use a local project directory instead.");

        console.Profile.Out.Writer.WriteLine("Loading remote Elm project: " + source);
        console.Profile.Out.Writer.Flush();

        var stopwatch = Stopwatch.StartNew();
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> files;

        try
        {
            files =
                loadRemote is not null
                ?
                loadRemote(source)
                :
                GitCore.LoadFromUrl.LoadTreeContentsFromUrlAsync(source).GetAwaiter().GetResult();
        }
        catch (Exception exception) when (
            exception.GetType() == typeof(Exception) ||
            exception is HttpRequestException or IOException or UnauthorizedAccessException or
            ArgumentException or InvalidOperationException or NotSupportedException or OperationCanceledException or
            GitCore.IGitContextException)
        {
            // Transport messages can contain credentials or server-controlled text.
            throw new IOException("Cannot load remote Elm project: " + source + ". Check the URL and network access.");
        }

        var paths = new Dictionary<string, (string Original, bool IsFile)>(StringComparer.OrdinalIgnoreCase);
        var hasManifest = false;

        foreach (var file in files)
        {
            if (file.Key.Count is 0 ||
                file.Key.Any(
                    segment =>
                    string.IsNullOrEmpty(segment) ||
                    segment is "." or ".." ||
                    segment.Any(char.IsControl) ||
                    segment.IndexOfAny(['/', '\\', ':', '*', '?', '"', '<', '>', '|']) >= 0 ||
                    segment.EndsWith(' ') || segment.EndsWith('.') ||
                    IsReservedFileName(segment) ||
                    Path.IsPathRooted(segment)))
            {
                throw new IOException("Remote Elm project contains an unsafe file path.");
            }

            for (var length = 1; length <= file.Key.Count; ++length)
            {
                var path = string.Join("/", file.Key.Take(length));
                var isFile = length == file.Key.Count;

                if (paths.TryGetValue(path, out var previous))
                {
                    if (!StringComparer.Ordinal.Equals(path, previous.Original))
                    {
                        throw new IOException(
                            "Remote Elm project contains file paths that collide on case-insensitive filesystems.");
                    }

                    if (isFile || previous.IsFile)
                        throw new IOException("Remote Elm project contains conflicting file paths.");
                }
                else
                {
                    paths.Add(path, (path, isFile));
                }
            }

            if (file.Key.Count is 1 && file.Key[0] is "elm.json")
            {
                hasManifest = true;
                ValidateSourceDirectories(file.Value);
            }
        }

        if (!hasManifest)
        {
            throw new IOException(
                "Remote Elm project does not contain elm.json at the selected URL: " + source +
                ". Select the Elm project directory.");
        }

        var container =
            Path.Combine(Path.GetTempPath(), "pine-elm-test-source-" + Guid.NewGuid().ToString("N"));

        var resolved =
            new ElmTestProjectSource(
                Path.Combine(container, "project"),
                ownsDirectory: true,
                containerDirectory: container);

        try
        {
            Directory.CreateDirectory(resolved.DirectoryPath);

            foreach (var file in files)
            {
                var path = Path.Combine([resolved.DirectoryPath, .. file.Key]);
                Directory.CreateDirectory(Path.GetDirectoryName(path)!);
                File.WriteAllBytes(path, file.Value.Span);
            }

            console.Profile.Out.Writer.WriteLine(
                "Loaded " + files.Count + " project files in " +
                stopwatch.Elapsed.TotalSeconds.ToString("0.00", CultureInfo.InvariantCulture) +
                " seconds. Resolving dependencies and compiling Elm tests.");

            console.Profile.Out.Writer.Flush();

            return resolved;
        }
        catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or ArgumentException)
        {
            resolved.Dispose();

            throw new IOException(
                "Cannot prepare remote Elm project: " + source +
                ". Check local write permissions and project file paths.");
        }
        catch
        {
            resolved.Dispose();
            throw;
        }
    }

    private static bool IsReservedFileName(string segment)
    {
        var name = segment.Split('.')[0].TrimEnd(' ').ToUpperInvariant();

        return
            name is "CON" or "PRN" or "AUX" or "NUL" or "CONIN$" or "CONOUT$" ||
            name.Length is 4 &&
            (name.StartsWith("COM", StringComparison.Ordinal) || name.StartsWith("LPT", StringComparison.Ordinal)) &&
            (name[3] is >= '1' and <= '9' or '¹' or '²' or '³');
    }

    private static void ValidateSourceDirectories(ReadOnlyMemory<byte> manifest)
    {
        try
        {
            using var document = JsonDocument.Parse(manifest);

            if (document.RootElement.ValueKind is not JsonValueKind.Object ||
                !document.RootElement.TryGetProperty("source-directories", out var directories) ||
                directories.ValueKind is not JsonValueKind.Array)
                return;

            foreach (var directory in directories.EnumerateArray())
            {
                if (directory.ValueKind is not JsonValueKind.String)
                    continue;

                var path = directory.GetString()!;

                if (ElmJsonStructure.ParseSourceDirectory(path).ParentLevel > 0 ||
                    path.StartsWith('/') || path.StartsWith('\\') || path.Contains(':'))
                {
                    throw new IOException(
                        "Remote Elm project source-directories must stay within the selected project directory.");
                }
            }
        }
        catch (JsonException)
        {
            // Let the dependency resolver report malformed manifests consistently with local projects.
        }
    }

    public void Dispose()
    {
        if (!ownsDirectory)
            return;

        try
        {
            Directory.Delete(containerDirectory ?? DirectoryPath, recursive: true);
        }
        catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
        {
            // Cleanup must not replace the test result or a compilation error.
        }
    }
}
