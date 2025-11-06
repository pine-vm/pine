using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace GitCore;

public static partial class GitObjects
{
    /// <summary>
    /// Represents a signature in a Git commit (author or committer).
    /// </summary>
    public record CommitSignature(
        string Name,
        string Email,
        DateTimeOffset Timestamp);

    /// <summary>
    /// Represents a Git commit with parsed metadata.
    /// </summary>
    public record CommitObject(
        string TreeHash,
        IReadOnlyList<string> ParentHashes,
        CommitSignature Author,
        CommitSignature Committer,
        string Message);

    public record TreeEntry(
        string Mode,
        string Name,
        string HashBase16);

    public record TreeObject(
        IReadOnlyList<TreeEntry> Entries);

    /// <summary>
    /// Parses a Git commit object from its raw byte data.
    /// </summary>
    public static CommitObject ParseCommit(ReadOnlyMemory<byte> data)
    {
        var text = Encoding.UTF8.GetString(data.Span);
        var lines = text.Split('\n');

        string? treeHash = null;
        var parentHashes = new List<string>();
        string? authorLine = null;
        string? committerLine = null;
        var messageLines = new List<string>();
        var inMessage = false;

        foreach (var line in lines)
        {
            if (inMessage)
            {
                messageLines.Add(line);
            }
            else if (string.IsNullOrEmpty(line))
            {
                inMessage = true;
            }
            else if (line.StartsWith("tree "))
            {
                treeHash = line[5..];
            }
            else if (line.StartsWith("parent "))
            {
                parentHashes.Add(line[7..]);
            }
            else if (line.StartsWith("author "))
            {
                authorLine = line[7..];
            }
            else if (line.StartsWith("committer "))
            {
                committerLine = line[10..];
            }
        }

        var message = string.Join("\n", messageLines).TrimEnd('\n');

        if (treeHash is null)
            throw new InvalidOperationException("Commit missing tree");

        if (authorLine is null)
            throw new InvalidOperationException("Commit missing author");

        if (committerLine is null)
            throw new InvalidOperationException("Commit missing committer");

        var author = ParseSignature(authorLine);
        var committer = ParseSignature(committerLine);

        return new CommitObject(
            treeHash,
            parentHashes,
            author,
            committer,
            message);
    }

    /// <summary>
    /// Parses a signature line (author or committer) from a Git commit.
    /// Format: "Name &lt;email&gt; timestamp timezone"
    /// </summary>
    private static CommitSignature ParseSignature(string signatureLine)
    {
        // Pattern: "Name <email> timestamp timezone"
        // Example: "John Doe <john@example.com> 1234567890 +0000"
        var match = CommitSignatureRegex().Match(signatureLine);

        if (!match.Success)
        {
            throw new InvalidOperationException($"Invalid signature format: {signatureLine}");
        }

        var name = match.Groups[1].Value;
        var email = match.Groups[2].Value;
        var timestampStr = match.Groups[3].Value;
        var timezoneStr = match.Groups[4].Value;

        // Parse Unix timestamp
        var timestamp = long.Parse(timestampStr, CultureInfo.InvariantCulture);
        var dateTime = DateTimeOffset.FromUnixTimeSeconds(timestamp);

        // Parse timezone offset
        var timezoneHours =
            int.Parse(timezoneStr.AsSpan(0, 3), CultureInfo.InvariantCulture);

        var timezoneMinutes =
            int.Parse(timezoneStr.AsSpan(3, 2), CultureInfo.InvariantCulture);

        var timezoneOffset =
            new TimeSpan(timezoneHours, timezoneMinutes, 0);

        // Apply timezone offset
        var dateTimeWithOffset = new DateTimeOffset(dateTime.DateTime, timezoneOffset);

        return new CommitSignature(name, email, dateTimeWithOffset);
    }

    public static TreeObject ParseTree(ReadOnlyMemory<byte> data)
    {
        var entries = new List<TreeEntry>();
        var span = data.Span;
        var offset = 0;

        while (offset < span.Length)
        {
            // Read mode (e.g., "100644")
            var modeEnd = offset;

            while (modeEnd < span.Length && span[modeEnd] is not 32)
            {
                modeEnd++;
            }

            var mode = Encoding.UTF8.GetString(span[offset..modeEnd]);
            offset = modeEnd + 1; // Skip space

            // Read name
            var nameEnd = offset;

            while (nameEnd < span.Length && span[nameEnd] is not 0)
            {
                nameEnd++;
            }

            var name = Encoding.UTF8.GetString(span[offset..nameEnd]);
            offset = nameEnd + 1; // Skip null byte

            // Read hash (20 bytes for SHA-1, but we support future hash algorithms)
            var hashBytes = span.Slice(offset, 20);
            var hashBase16 = Convert.ToHexStringLower(hashBytes);
            offset += 20;

            entries.Add(new TreeEntry(mode, name, hashBase16));
        }

        return new TreeObject(entries);
    }

    public static ReadOnlyMemory<byte> GetBlobContent(ReadOnlyMemory<byte> data)
    {
        return data;
    }

    public static IReadOnlyDictionary<string, ReadOnlyMemory<byte>> GetFilesFromTree(
        string treeHashBase16,
        IReadOnlyDictionary<string, PackFile.PackObject> objectsByHashBase16)
    {
        var files = new Dictionary<string, ReadOnlyMemory<byte>>();

        if (!objectsByHashBase16.TryGetValue(treeHashBase16, out var treeObject))
        {
            throw new InvalidOperationException($"Tree {treeHashBase16} not found in pack file");
        }

        if (treeObject.Type is not PackFile.ObjectType.Tree)
        {
            throw new InvalidOperationException($"Object {treeHashBase16} is not a tree");
        }

        var tree = ParseTree(treeObject.Data);

        foreach (var entry in tree.Entries)
        {
            if (entry.Mode.StartsWith("100")) // Regular file
            {
                if (objectsByHashBase16.TryGetValue(entry.HashBase16, out var blobObject))
                {
                    if (blobObject.Type is PackFile.ObjectType.Blob)
                    {
                        files[entry.Name] = GetBlobContent(blobObject.Data);
                    }
                }
            }
            else if (entry.Mode is "40000") // Directory
            {
                // For now, we'll skip subdirectories
                // In a full implementation, we would recursively process them
            }
        }

        return files;
    }

    public static IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> GetAllFilesFromTree(
        string treeHashBase16,
        Func<string, PackFile.PackObject?> getObjectByHashBase16,
        IReadOnlyList<string>? pathPrefix = null)
    {
        var files = new Dictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>(
            comparer: Common.EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        pathPrefix ??= [];

        var treeObject = getObjectByHashBase16(treeHashBase16);
        if (treeObject is null)
        {
            throw new InvalidOperationException($"Tree {treeHashBase16} not found in pack file");
        }

        if (treeObject.Type is not PackFile.ObjectType.Tree)
        {
            throw new InvalidOperationException($"Object {treeHashBase16} is not a tree");
        }

        var tree = ParseTree(treeObject.Data);

        foreach (var entry in tree.Entries)
        {
            var filePath = pathPrefix.Concat([entry.Name]).ToArray();

            if (entry.Mode.StartsWith("100")) // Regular file
            {
                var blobObject = getObjectByHashBase16(entry.HashBase16);
                if (blobObject is not null && blobObject.Type is PackFile.ObjectType.Blob)
                {
                    files[filePath] = GetBlobContent(blobObject.Data);
                }
            }
            else if (entry.Mode is "40000") // Directory
            {
                // Recursively process subdirectories
                var subFiles = GetAllFilesFromTree(entry.HashBase16, getObjectByHashBase16, filePath);
                foreach (var (subPath, content) in subFiles)
                {
                    files[subPath] = content;
                }
            }
        }

        return files;
    }

    /// <summary>
    /// Gets files from a specific subdirectory within a tree.
    /// </summary>
    /// <param name="treeHashBase16">The hash of the root tree</param>
    /// <param name="subdirectoryPath">Path components to the subdirectory (e.g., ["implement", "GitCore"])</param>
    /// <param name="getObjectByHashBase16">Function to retrieve objects by hash</param>
    /// <returns>Dictionary of file paths (relative to subdirectory) to their contents</returns>
    public static IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> GetFilesFromSubdirectory(
        string treeHashBase16,
        IReadOnlyList<string> subdirectoryPath,
        Func<string, PackFile.PackObject?> getObjectByHashBase16)
    {
        // Navigate to the subdirectory by traversing the tree
        var currentTreeHash = treeHashBase16;

        foreach (var pathComponent in subdirectoryPath)
        {
            var treeObject = getObjectByHashBase16(currentTreeHash);
            if (treeObject is null)
            {
                throw new InvalidOperationException($"Tree {currentTreeHash} not found");
            }

            if (treeObject.Type is not PackFile.ObjectType.Tree)
            {
                throw new InvalidOperationException($"Object {currentTreeHash} is not a tree");
            }

            var tree = ParseTree(treeObject.Data);

            var entry =
                tree.Entries.FirstOrDefault(e => e.Name == pathComponent);

            if (entry is null)
            {
                throw new InvalidOperationException($"Path component '{pathComponent}' not found in tree");
            }

            if (entry.Mode is not "40000")
            {
                throw new InvalidOperationException($"Path component '{pathComponent}' is not a directory");
            }

            currentTreeHash = entry.HashBase16;
        }

        // Now get all files from the subdirectory tree
        return GetAllFilesFromTree(currentTreeHash, getObjectByHashBase16, pathPrefix: []);
    }

    public static ReadOnlyMemory<byte> GetFileFromCommit(
        string commitHashBase16,
        string fileName,
        IReadOnlyDictionary<string, PackFile.PackObject> objectsByHashBase16)
    {
        if (!objectsByHashBase16.TryGetValue(commitHashBase16, out var commitObject))
        {
            throw new InvalidOperationException($"Commit {commitHashBase16} not found in pack file");
        }

        if (commitObject.Type is not PackFile.ObjectType.Commit)
        {
            throw new InvalidOperationException($"Object {commitHashBase16} is not a commit");
        }

        var commit = ParseCommit(commitObject.Data);
        var files = GetFilesFromTree(commit.TreeHash, objectsByHashBase16);

        if (!files.TryGetValue(fileName, out var fileContent))
        {
            throw new InvalidOperationException($"File {fileName} not found in tree");
        }

        return fileContent;
    }

    [GeneratedRegex(@"^(.+?)\s+<(.+?)>\s+(\d+)\s+([\+\-]\d{4})$")]
    private static partial Regex CommitSignatureRegex();
}
