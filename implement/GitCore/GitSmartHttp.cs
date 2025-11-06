using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

namespace GitCore;

/// <summary>
/// Implements Git Smart HTTP protocol for fetching objects from remote repositories.
/// </summary>
public static class GitSmartHttp
{
    private static readonly HttpClient s_httpClient = new();

    // Git protocol capabilities we request when fetching pack files
    private const string GitProtocolCapabilities = "multi_ack_detailed side-band-64k ofs-delta";

    /// <summary>
    /// Result of parsing a tree URL.
    /// </summary>
    public record ParseTreeUrlResult(
        string BaseUrl,
        string Owner,
        string Repo,
        string CommitShaOrBranch);

    /// <summary>
    /// Parses a GitHub or GitLab tree URL to extract repository information and commit SHA or branch.
    /// </summary>
    /// <param name="url">URL like https://github.com/owner/repo/tree/commit-sha or https://github.com/owner/repo/tree/main</param>
    /// <returns>Record containing baseUrl, owner, repo, and commitShaOrBranch</returns>
    public static ParseTreeUrlResult ParseTreeUrl(string url)
    {
        var uri = new Uri(url);
        var host = uri.Host;
        var scheme = uri.Scheme;
        var pathParts = uri.AbsolutePath.Trim('/').Split('/');

        if (host is "github.com" && pathParts.Length >= 4 && pathParts[2] is "tree")
        {
            // Format: github.com/owner/repo/tree/commit-sha-or-branch
            return new ParseTreeUrlResult(
                $"{scheme}://{host}",
                pathParts[0],
                pathParts[1],
                pathParts[3]
            );
        }
        else if (host is "gitlab.com" && pathParts.Length >= 5 && pathParts[2] is "-" && pathParts[3] is "tree")
        {
            // Format: gitlab.com/owner/repo/-/tree/commit-sha-or-branch
            return new ParseTreeUrlResult(
                $"{scheme}://{host}",
                pathParts[0],
                pathParts[1],
                pathParts[4]
            );
        }
        else
        {
            throw new ArgumentException($"Unsupported URL format: {url}");
        }
    }

    /// <summary>
    /// Fetches a pack file containing the specified commit and its tree from a remote repository.
    /// </summary>
    /// <param name="baseUrl">Base URL like https://github.com</param>
    /// <param name="owner">Repository owner</param>
    /// <param name="repo">Repository name</param>
    /// <param name="commitSha">Commit SHA to fetch</param>
    /// <param name="httpClient">Optional HttpClient to use for requests. If null, uses a default static client.</param>
    /// <returns>Pack file data</returns>
    public static async Task<ReadOnlyMemory<byte>> FetchPackFileAsync(
        string baseUrl,
        string owner,
        string repo,
        string commitSha,
        HttpClient? httpClient = null)
    {
        var gitUrl = $"{baseUrl}/{owner}/{repo}.git";
        return await FetchPackFileAsync(gitUrl, commitSha, httpClient);
    }

    /// <summary>
    /// Fetches a pack file containing the specified commit and its tree from a remote repository.
    /// </summary>
    /// <param name="gitUrl">Git repository URL like https://github.com/owner/repo.git</param>
    /// <param name="commitSha">Commit SHA to fetch</param>
    /// <param name="httpClient">Optional HttpClient to use for requests. If null, uses a default static client.</param>
    /// <returns>Pack file data</returns>
    public static async Task<ReadOnlyMemory<byte>> FetchPackFileAsync(
        string gitUrl,
        string commitSha,
        HttpClient? httpClient = null)
    {
        return await FetchPackFileAsync(gitUrl, commitSha, subdirectoryPath: null, httpClient);
    }

    /// <summary>
    /// Resolves the target of a symbolic reference from a remote repository using the Git Smart HTTP protocol.
    /// </summary>
    /// <param name="baseUrl">Base URL like https://github.com</param>
    /// <param name="owner">Repository owner</param>
    /// <param name="repo">Repository name</param>
    /// <param name="symbolicRef">Symbolic reference name to resolve</param>
    /// <param name="httpClient">Optional HttpClient to use for requests. If null, uses a default static client.</param>
    /// <returns>The fully qualified reference that the symbolic ref points to (e.g., refs/heads/main)</returns>
    public static async Task<string> FetchSymbolicRefTargetAsync(
        string baseUrl,
        string owner,
        string repo,
        string symbolicRef,
        HttpClient? httpClient = null)
    {
        var gitUrl = $"{baseUrl}/{owner}/{repo}.git";

        return await FetchSymbolicRefTargetAsync(gitUrl, symbolicRef, httpClient);
    }

    /// <summary>
    /// Resolves the target of a symbolic reference from a remote repository using the Git Smart HTTP protocol.
    /// </summary>
    /// <param name="gitUrl">Git repository URL like https://github.com/owner/repo.git</param>
    /// <param name="symbolicRef">Symbolic reference name to resolve</param>
    /// <param name="httpClient">Optional HttpClient to use for requests. If null, uses a default static client.</param>
    /// <returns>The fully qualified reference that the symbolic ref points to (e.g., refs/heads/main)</returns>
    public static async Task<string> FetchSymbolicRefTargetAsync(
        string gitUrl,
        string symbolicRef,
        HttpClient? httpClient = null)
    {
        httpClient ??= s_httpClient;

        if (!gitUrl.EndsWith(".git", StringComparison.Ordinal))
        {
            gitUrl = $"{gitUrl}.git";
        }

        var refsUrl = $"{gitUrl}/info/refs?service=git-upload-pack";

        using var refsRequest = new HttpRequestMessage(HttpMethod.Get, refsUrl);
        using var refsResponse = await httpClient.SendAsync(refsRequest);
        refsResponse.EnsureSuccessStatusCode();

        var responseData = await refsResponse.Content.ReadAsByteArrayAsync();
        var responseText = Encoding.UTF8.GetString(responseData);
        var lines = responseText.Split('\n');

        foreach (var rawLine in lines)
        {
            if (rawLine.Length <= 4)
            {
                continue;
            }

            var line = rawLine.TrimEnd('\r');

            if (line is "0000")
            {
                continue;
            }

            var payload = line[4..];

            var nulIndex = payload.IndexOf('\0');

            if (nulIndex >= 0)
            {
                var capabilitiesSegment = payload[(nulIndex + 1)..];

                if (TryParseSymrefMapping(capabilitiesSegment, symbolicRef, out var targetRef))
                {
                    return targetRef;
                }
            }

            var refEntry = nulIndex >= 0 ? payload[..nulIndex] : payload;
            var parts = refEntry.Split(' ', 2, StringSplitOptions.RemoveEmptyEntries);

            if (parts.Length is 2 && string.Equals(parts[1], symbolicRef, StringComparison.Ordinal))
            {
                // Symbolic ref resolved to direct reference entry (detached scenario).
                return parts[1];
            }
        }

        throw new InvalidOperationException($"Symbolic ref '{symbolicRef}' not found at {gitUrl}");
    }

    /// <summary>
    /// Fetches a pack file containing only objects needed for a specific subdirectory.
    /// </summary>
    /// <param name="gitUrl">Git repository URL like https://github.com/owner/repo.git</param>
    /// <param name="commitSha">Commit SHA to fetch</param>
    /// <param name="subdirectoryPath">Optional subdirectory path to optimize the fetch</param>
    /// <param name="httpClient">Optional HttpClient to use for requests. If null, uses a default static client.</param>
    /// <returns>Pack file data</returns>
    public static async Task<ReadOnlyMemory<byte>> FetchPackFileAsync(
        string gitUrl,
        string commitSha,
        IReadOnlyList<string>? subdirectoryPath,
        HttpClient? httpClient = null)
    {
        httpClient ??= s_httpClient;

        // Ensure the URL ends with .git
        if (!gitUrl.EndsWith(".git"))
        {
            gitUrl = $"{gitUrl}.git";
        }

        // Step 1: Discover refs (optional but following protocol)
        var refsUrl = $"{gitUrl}/info/refs?service=git-upload-pack";

        using var refsRequest = new HttpRequestMessage(HttpMethod.Get, refsUrl);
        using var refsResponse = await httpClient.SendAsync(refsRequest);

        refsResponse.EnsureSuccessStatusCode();

        // Step 2: Request the pack file with the specific commit
        var uploadPackUrl = $"{gitUrl}/git-upload-pack";

        // For subdirectory optimization, use shallow fetch to only get the commit without history
        // Note: To further optimize by fetching only specific subdirectory contents would require:
        // 1. Git Protocol v2 with partial clone and sparse checkout support
        // 2. Multiple round-trips: fetch trees, navigate to subdirectory, then fetch only those blobs
        // The current shallow approach (depth=1) already provides significant optimization
        int? shallowDepth = (subdirectoryPath is not null && subdirectoryPath.Count > 0) ? 1 : null;
        var requestBody = BuildUploadPackRequest(commitSha, shallowDepth);

        using var packRequest = new HttpRequestMessage(HttpMethod.Post, uploadPackUrl)
        {
            Content = new ByteArrayContent(requestBody)
        };

        packRequest.Content.Headers.ContentType =
            new System.Net.Http.Headers.MediaTypeHeaderValue("application/x-git-upload-pack-request");

        using var packResponse = await httpClient.SendAsync(packRequest);

        packResponse.EnsureSuccessStatusCode();

        var responseData = await packResponse.Content.ReadAsByteArrayAsync();

        // Parse the response to extract the pack file
        return ExtractPackFileFromResponse(responseData);
    }

    /// <summary>
    /// Fetches the commit SHA for a given branch from the remote repository.
    /// </summary>
    /// <param name="baseUrl">Base URL like https://github.com</param>
    /// <param name="owner">Repository owner</param>
    /// <param name="repo">Repository name</param>
    /// <param name="branch">Branch name</param>
    /// <param name="httpClient">Optional HttpClient to use for requests. If null, uses a default static client.</param>
    /// <returns>Commit SHA for the branch</returns>
    public static async Task<string> FetchBranchCommitShaAsync(
        string baseUrl,
        string owner,
        string repo,
        string branch,
        HttpClient? httpClient = null)
    {
        httpClient ??= s_httpClient;

        var gitUrl = $"{baseUrl}/{owner}/{repo}.git";
        var refsUrl = $"{gitUrl}/info/refs?service=git-upload-pack";

        using var refsRequest = new HttpRequestMessage(HttpMethod.Get, refsUrl);
        using var refsResponse = await httpClient.SendAsync(refsRequest);
        refsResponse.EnsureSuccessStatusCode();

        var responseData = await refsResponse.Content.ReadAsByteArrayAsync();
        var responseText = Encoding.UTF8.GetString(responseData);

        // Parse pkt-line format to find the ref
        var refName = $"refs/heads/{branch}";
        var lines = responseText.Split('\n');

        foreach (var line in lines)
        {
            if (line.Length > 44 && line.Contains(refName))
            {
                // Extract SHA from the line
                // Format: <4-char-length><40-char-sha><space><ref-name>...
                // Skip the first 4 chars (length prefix)
                var sha = line.Substring(4, 40);
                var rest = line[44..].Trim();

                if (rest.StartsWith(refName))
                {
                    return sha;
                }
            }
        }

        throw new InvalidOperationException($"Branch {branch} not found in repository {owner}/{repo}");
    }

    /// <summary>
    /// Fetches a blobless pack file (commit and trees only, no blobs) for optimized subdirectory loading.
    /// </summary>
    /// <param name="gitUrl">Git repository URL like https://github.com/owner/repo.git</param>
    /// <param name="commitSha">Commit SHA to fetch</param>
    /// <param name="depth">Clone depth to control how many commits to fetch. Null means unlimited depth (full history).</param>
    /// <param name="httpClient">Optional HttpClient to use for requests. If null, uses a default static client.</param>
    /// <returns>Pack file data containing commit and trees but no blobs</returns>
    public static async Task<ReadOnlyMemory<byte>> FetchBloblessPackFileAsync(
        string gitUrl,
        string commitSha,
        int? depth = null,
        HttpClient? httpClient = null)
    {
        var requestBody =
            BuildUploadPackRequest(commitSha, shallowDepth: depth, filter: "blob:none");

        return await FetchPackFileWithRequestBodyAsync(gitUrl, requestBody, httpClient);
    }

    /// <summary>
    /// Fetches specific Git objects by their SHAs.
    /// </summary>
    /// <param name="gitUrl">Git repository URL like https://github.com/owner/repo.git</param>
    /// <param name="objectShas">List of object SHAs to fetch</param>
    /// <param name="httpClient">Optional HttpClient to use for requests. If null, uses a default static client.</param>
    /// <returns>Pack file data containing the requested objects</returns>
    public static async Task<ReadOnlyMemory<byte>> FetchSpecificObjectsAsync(
        string gitUrl,
        IReadOnlyList<string> objectShas,
        HttpClient? httpClient = null)
    {
        var requestBody =
            BuildUploadPackRequestForSpecificObjects(objectShas);

        return await FetchPackFileWithRequestBodyAsync(gitUrl, requestBody, httpClient);
    }

    /// <summary>
    /// Common helper for fetching pack files with a prepared request body.
    /// </summary>
    private static async Task<ReadOnlyMemory<byte>> FetchPackFileWithRequestBodyAsync(
        string gitUrl,
        byte[] requestBody,
        HttpClient? httpClient)
    {
        httpClient ??= s_httpClient;

        // Ensure the URL ends with .git
        if (!gitUrl.EndsWith(".git"))
        {
            gitUrl = $"{gitUrl}.git";
        }

        // Step 1: Discover refs
        var refsUrl = $"{gitUrl}/info/refs?service=git-upload-pack";
        using var refsRequest = new HttpRequestMessage(HttpMethod.Get, refsUrl);
        using var refsResponse = await httpClient.SendAsync(refsRequest);
        refsResponse.EnsureSuccessStatusCode();

        // Step 2: Request pack file
        var uploadPackUrl = $"{gitUrl}/git-upload-pack";

        using var packRequest = new HttpRequestMessage(HttpMethod.Post, uploadPackUrl)
        {
            Content = new ByteArrayContent(requestBody)
        };

        packRequest.Content.Headers.ContentType =
            new System.Net.Http.Headers.MediaTypeHeaderValue("application/x-git-upload-pack-request");

        using var packResponse = await httpClient.SendAsync(packRequest);
        packResponse.EnsureSuccessStatusCode();

        var responseData = await packResponse.Content.ReadAsByteArrayAsync();
        return ExtractPackFileFromResponse(responseData);
    }

    private static bool TryParseSymrefMapping(
        string capabilitiesSegment,
        string symbolicRef,
        out string targetRef)
    {
        targetRef = string.Empty;

        if (string.IsNullOrEmpty(capabilitiesSegment))
        {
            return false;
        }

        var capabilityEntries =
            capabilitiesSegment.Split(' ', StringSplitOptions.RemoveEmptyEntries);

        foreach (var entry in capabilityEntries)
        {
            if (!entry.StartsWith("symref=", StringComparison.Ordinal))
            {
                continue;
            }

            var mapping = entry["symref=".Length..];
            var separatorIndex = mapping.IndexOf(':');

            if (separatorIndex <= 0)
            {
                continue;
            }

            var source = mapping[..separatorIndex];

            if (!string.Equals(source, symbolicRef, StringComparison.Ordinal))
            {
                continue;
            }

            targetRef = mapping[(separatorIndex + 1)..];
            return true;
        }

        return false;
    }

    private static byte[] BuildUploadPackRequest(string commitSha, int? shallowDepth = null, string? filter = null)
    {
        using var ms = new MemoryStream();

        // Want line: want <sha> <capabilities>
        var capabilities = GitProtocolCapabilities;

        if (shallowDepth.HasValue)
        {
            capabilities = $"{capabilities} shallow";
        }

        if (filter is not null)
        {
            capabilities = $"{capabilities} filter";
        }

        var wantLine = $"want {commitSha} {capabilities}\n";
        WritePktLine(ms, wantLine);

        // For shallow clones, request specific depth (only this commit, not its history)
        if (shallowDepth.HasValue)
        {
            var shallowLine = $"deepen {shallowDepth.Value}\n";
            WritePktLine(ms, shallowLine);
        }

        // For filtered fetches, specify the filter
        if (filter is not null)
        {
            var filterLine = $"filter {filter}\n";
            WritePktLine(ms, filterLine);
        }

        // Flush packet
        WritePktLine(ms, null);

        // Done line
        WritePktLine(ms, "done\n");

        return ms.ToArray();
    }

    private static void WritePktLine(Stream stream, string? line)
    {
        if (line is null)
        {
            // Flush packet: "0000"
            stream.Write("0000"u8);
        }
        else
        {
            var lineBytes = Encoding.UTF8.GetBytes(line);
            var length = lineBytes.Length + 4; // +4 for the length prefix itself
            var lengthHex = length.ToString("x4");
            stream.Write(Encoding.UTF8.GetBytes(lengthHex));
            stream.Write(lineBytes);
        }
    }

    private static byte[] BuildUploadPackRequestForSpecificObjects(IReadOnlyList<string> objectShas)
    {
        using var ms = new MemoryStream();

        // Request each object with want lines
        for (var i = 0; i < objectShas.Count; i++)
        {
            var capabilities = i is 0 ? $" {GitProtocolCapabilities}" : "";
            var wantLine = $"want {objectShas[i]}{capabilities}\n";
            WritePktLine(ms, wantLine);
        }

        // Flush packet
        WritePktLine(ms, null);

        // Done line
        WritePktLine(ms, "done\n");

        return ms.ToArray();
    }

    private static ReadOnlyMemory<byte> ExtractPackFileFromResponse(byte[] responseData)
    {
        // The response is in pkt-line format with side-band
        // Side-band byte: 0x01 = pack data, 0x02 = progress, 0x03 = error

        using var output = new MemoryStream();
        var offset = 0;

        while (offset < responseData.Length)
        {
            // Read pkt-line length (4 hex chars)
            if (offset + 4 > responseData.Length)
                break;

            var lengthHex = Encoding.UTF8.GetString(responseData, offset, 4);
            offset += 4;

            if (lengthHex is "0000")
            {
                // Flush packet, continue
                continue;
            }

            var length = Convert.ToInt32(lengthHex, 16);
            var dataLength = length - 4; // Subtract the 4-byte length prefix

            if (dataLength <= 0 || offset + dataLength > responseData.Length)
                break;

            // First byte is the side-band indicator
            var sideBand = responseData[offset];
            offset++;
            dataLength--;

            if (sideBand is 0x01)
            {
                // Pack data
                output.Write(responseData, offset, dataLength);
            }
            // Ignore progress (0x02) and error (0x03) messages for now

            offset += dataLength;
        }

        return output.ToArray();
    }
}
