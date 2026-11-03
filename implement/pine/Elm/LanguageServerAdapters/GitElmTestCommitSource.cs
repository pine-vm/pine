using Pine.Core.Elm.LanguageServer;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm.LanguageServerAdapters;

/// <summary>Reads exact committed Elm inputs through Git's object database, without checkout, hooks, or network access.</summary>
public sealed class GitElmTestCommitSource : IElmTestCommitSource
{
    private static readonly UTF8Encoding s_utf8 = new(false, true);

    /// <inheritdoc/>
    public async Task<ElmTestSourceSnapshot> LoadCommitAsync(
        string projectUri,
        string commit,
        CancellationToken cancellationToken)
    {
        var uri = new Uri(projectUri);

        if (!uri.IsFile)
            throw new PineTestingProtocolException("commitUnavailable", "Local Git snapshots require a file project URI.");

        var directory = uri.LocalPath;

        while (!Directory.Exists(directory))
        {
            directory =
                Path.GetDirectoryName(Path.TrimEndingDirectorySeparator(directory))
                ?? throw new PineTestingProtocolException(
                    "commitUnavailable",
                    "No existing ancestor directory for the requested project.");
        }

        var rootOutput =
            s_utf8.GetString(await RunGitAsync(directory, ["rev-parse", "--show-toplevel"], cancellationToken));

        var rootPath = rootOutput.EndsWith('\n') ? rootOutput[..^1] : rootOutput;

        if (OperatingSystem.IsWindows() && rootPath.EndsWith('\r'))
            rootPath = rootPath[..^1];

        var type = s_utf8.GetString(await RunGitAsync(directory, ["cat-file", "-t", commit], cancellationToken)).Trim();

        if (type != "commit")
            throw new PineTestingProtocolException("commitUnavailable", "The requested object is not a Git commit: " + commit);

        var listing =
            s_utf8.GetString(
                await RunGitAsync(directory, ["ls-tree", "-r", "-z", "--full-tree", commit], cancellationToken));

        var entries = new List<(string[] Path, string Hash)>();

        foreach (var entry in listing.Split('\0', StringSplitOptions.RemoveEmptyEntries))
        {
            var tab = entry.IndexOf('\t');

            if (tab < 0)
                throw new IOException("Malformed git ls-tree output.");

            var header = entry[..tab].Split(' ');
            var path = entry[(tab + 1)..].Split('/');

            if (path.Any(segment => segment is ".git" or "elm-stuff" or "node_modules") ||
                !(path[^1] == "elm.json" || path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase)))
            {
                continue;
            }

            if (header.Length != 3 || header[1] != "blob" || header[0] is not ("100644" or "100755"))
            {
                throw new PineTestingProtocolException(
                    "invalidSource",
                    "Committed Elm input is not a regular file: " + string.Join('/', path));
            }

            entries.Add((path, header[2]));
        }

        var files = new List<(IReadOnlyList<string>, ReadOnlyMemory<byte>)>();
        using var batch = StartGit(directory, ["cat-file", "--batch"]);
        var stderr = batch.StandardError.ReadToEndAsync();

        try
        {
            long totalBytes = 0;

            foreach (var entry in entries)
            {
                cancellationToken.ThrowIfCancellationRequested();
                await batch.StandardInput.WriteLineAsync(entry.Hash.AsMemory(), cancellationToken);
                await batch.StandardInput.FlushAsync(cancellationToken);
                var header = (await ReadHeaderAsync(batch.StandardOutput.BaseStream, cancellationToken)).Split(' ');

                if (header.Length != 3 || header[0] != entry.Hash || header[1] != "blob" ||
                    !int.TryParse(header[2], out var size) || size < 0)
                {
                    throw new IOException("Could not read committed Git blob: " + entry.Hash);
                }

                totalBytes += size;

                if (totalBytes > LanguageServerTestingService.MaximumSnapshotBytes)
                    throw new PineTestingProtocolException("snapshotTooLarge", "Committed Elm input snapshot exceeds 64 MiB.");

                var bytes = new byte[size];
                await batch.StandardOutput.BaseStream.ReadExactlyAsync(bytes, cancellationToken);
                var terminator = new byte[1];
                await batch.StandardOutput.BaseStream.ReadExactlyAsync(terminator, cancellationToken);

                if (terminator[0] != (byte)'\n')
                    throw new IOException("Malformed git cat-file blob terminator.");

                files.Add((entry.Path, bytes));
            }

            batch.StandardInput.Close();
            await batch.WaitForExitAsync(cancellationToken);
            var errorText = await stderr;

            if (batch.ExitCode != 0)
                throw new PineTestingProtocolException("commitUnavailable", "git cat-file failed: " + errorText.Trim());
        }
        finally
        {
            StopIfRunning(batch);
            await batch.WaitForExitAsync(CancellationToken.None);
            await stderr;
        }

        return
            new(
                new Uri(rootPath.TrimEnd(Path.DirectorySeparatorChar) + Path.DirectorySeparatorChar).AbsoluteUri,
                FileTree.FromSetOfFilesWithStringPath(files),
                new Dictionary<string, int>());
    }

    private static Process StartGit(string directory, IReadOnlyList<string> arguments)
    {
        var start =
            new ProcessStartInfo("git")
            {
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true
            };

        start.ArgumentList.Add("--no-replace-objects");
        start.ArgumentList.Add("-C");
        start.ArgumentList.Add(directory);

        foreach (var argument in arguments)
            start.ArgumentList.Add(argument);

        start.Environment["GIT_TERMINAL_PROMPT"] = "0";
        start.Environment.Remove("GIT_DIR");
        start.Environment.Remove("GIT_WORK_TREE");
        start.Environment.Remove("GIT_INDEX_FILE");

        try
        {
            return Process.Start(start) ?? throw new IOException("Failed starting Git to read a committed test snapshot.");
        }
        catch (Win32Exception exception)
        {
            throw new PineTestingProtocolException("commitUnavailable", "Could not start Git: " + exception.Message);
        }
    }

    private static async Task<byte[]> RunGitAsync(string directory, string[] arguments, CancellationToken token)
    {
        using var process = StartGit(directory, arguments);
        var stderr = process.StandardError.ReadToEndAsync();
        using var output = new MemoryStream();

        try
        {
            process.StandardInput.Close();
            await process.StandardOutput.BaseStream.CopyToAsync(output, token);
            await process.WaitForExitAsync(token);
            var errorText = await stderr;

            if (process.ExitCode != 0)
                throw new PineTestingProtocolException("commitUnavailable", "Git snapshot lookup failed: " + errorText.Trim());

            return output.ToArray();
        }
        finally
        {
            StopIfRunning(process);
            await process.WaitForExitAsync(CancellationToken.None);
            await stderr;
        }
    }

    private static async Task<string> ReadHeaderAsync(Stream stream, CancellationToken token)
    {
        var bytes = new byte[1];
        var header = new StringBuilder();

        while (header.Length < 200)
        {
            await stream.ReadExactlyAsync(bytes, token);

            if (bytes[0] == (byte)'\n')
                return header.ToString();

            header.Append((char)bytes[0]);
        }

        throw new IOException("Oversized git cat-file response header.");
    }

    private static void StopIfRunning(Process process)
    {
        try
        {
            if (!process.HasExited)
                process.Kill(entireProcessTree: true);
        }
        catch (InvalidOperationException) when (process.HasExited)
        {
        }
    }
}
