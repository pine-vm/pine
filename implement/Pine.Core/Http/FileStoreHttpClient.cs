using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text.Json;
using System.Threading.Tasks;

namespace Pine.Core.Http;

/// <summary>
/// HTTP client that implements IFileStore by making REST API calls to a FileStore HTTP server.
/// </summary>
public class FileStoreHttpClient(HttpClient httpClient) : IFileStore
{
    private readonly HttpClient _httpClient =
        httpClient ?? throw new ArgumentNullException(nameof(httpClient));

    /// <inheritdoc />
    public async Task<ReadOnlyMemory<byte>?> GetFileContentAsync(IImmutableList<string> path)
    {
        var url = "/files/" + string.Join("/", path);

        try
        {
            var response = await _httpClient.GetAsync(url);

            if (response.StatusCode == HttpStatusCode.NotFound)
            {
                return null;
            }

            response.EnsureSuccessStatusCode();

            var content = await response.Content.ReadAsByteArrayAsync();
            return new ReadOnlyMemory<byte>(content);
        }
        catch (HttpRequestException)
        {
            return null;
        }
    }

    /// <summary>
    /// Synchronous wrapper for GetFileContentAsync.
    /// </summary>
    public ReadOnlyMemory<byte>? GetFileContent(IImmutableList<string> path)
    {
        return GetFileContentAsync(path).GetAwaiter().GetResult();
    }

    /// <inheritdoc />
    public async Task<IEnumerable<IImmutableList<string>>> ListFilesInDirectoryAsync(IImmutableList<string> directoryPath)
    {
        var url = "/dirs/" + string.Join("/", directoryPath);

        try
        {
            var response = await _httpClient.GetAsync(url);

            if (response.StatusCode is HttpStatusCode.NotFound)
            {
                return [];
            }

            response.EnsureSuccessStatusCode();

            var responseContent = await response.Content.ReadAsStringAsync();
            var result = JsonSerializer.Deserialize<DirectoryListingResponse>(responseContent, s_jsonOptions);

            if (result?.Entries == null)
            {
                return [];
            }

            return result.Entries
                .Where(entry => entry.Type is "file")
                .Select(entry => (IImmutableList<string>)[.. entry.Name.Split('/', StringSplitOptions.RemoveEmptyEntries)]);
        }
        catch (HttpRequestException)
        {
            return [];
        }
    }

    /// <summary>
    /// Synchronous wrapper for ListFilesInDirectoryAsync.
    /// </summary>
    public IEnumerable<IImmutableList<string>> ListFilesInDirectory(IImmutableList<string> directoryPath)
    {
        return ListFilesInDirectoryAsync(directoryPath).GetAwaiter().GetResult();
    }

    /// <inheritdoc />
    public async Task SetFileContentAsync(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        var url = "/files/" + string.Join("/", path);

        using var content = new ByteArrayContent(fileContent.ToArray());
        content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/octet-stream");

        var response = await _httpClient.PutAsync(url, content);
        response.EnsureSuccessStatusCode();
    }

    /// <summary>
    /// Synchronous wrapper for SetFileContentAsync.
    /// </summary>
    public void SetFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        SetFileContentAsync(path, fileContent).GetAwaiter().GetResult();
    }

    /// <inheritdoc />
    public async Task AppendFileContentAsync(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        var url = "/files/" + string.Join("/", path);

        using var content = new ByteArrayContent(fileContent.ToArray());
        content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/octet-stream");

        var request = new HttpRequestMessage(HttpMethod.Post, url)
        {
            Content = content
        };
        request.Headers.Add("X-Operation", "append");

        var response = await _httpClient.SendAsync(request);
        response.EnsureSuccessStatusCode();
    }

    /// <summary>
    /// Synchronous wrapper for AppendFileContentAsync.
    /// </summary>
    public void AppendFileContent(IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        AppendFileContentAsync(path, fileContent).GetAwaiter().GetResult();
    }

    /// <inheritdoc />
    public async Task DeleteFileAsync(IImmutableList<string> path)
    {
        var url = "/files/" + string.Join("/", path);

        try
        {
            var response = await _httpClient.DeleteAsync(url);

            // Consider both 204 (deleted) and 404 (not found) as success
            if (response.StatusCode != HttpStatusCode.NoContent && response.StatusCode != HttpStatusCode.NotFound)
            {
                response.EnsureSuccessStatusCode();
            }
        }
        catch (HttpRequestException)
        {
            // Ignore errors for delete operations to maintain idempotent behavior
        }
    }

    /// <summary>
    /// Synchronous wrapper for DeleteFileAsync.
    /// </summary>
    public void DeleteFile(IImmutableList<string> path)
    {
        DeleteFileAsync(path).GetAwaiter().GetResult();
    }

    private record DirectoryListingResponse(DirectoryEntry[] Entries);
    private record DirectoryEntry(string Name, string Type);

    private static readonly JsonSerializerOptions s_jsonOptions = BuildJsonSerializerOptions();

    /// <summary>
    /// Builds and returns the <see cref="JsonSerializerOptions"/> used for serializing and deserializing HTTP responses.
    /// </summary>
    public static JsonSerializerOptions BuildJsonSerializerOptions() =>
        new(JsonSerializerDefaults.Web)
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            PropertyNameCaseInsensitive = true,
            WriteIndented = false,
        };
}

/// <summary>
/// Extension methods to provide async versions of IFileStore methods.
/// </summary>
public static class FileStoreHttpClientExtensions
{
    /// <summary>
    /// Async version of GetFileContent.
    /// </summary>
    public static async Task<ReadOnlyMemory<byte>?> GetFileContentAsync(this IFileStore fileStore, IImmutableList<string> path)
    {
        if (fileStore is FileStoreHttpClient httpClient)
        {
            return await httpClient.GetFileContentAsync(path);
        }

        // Fallback to synchronous version for other implementations
        return await Task.Run(() => fileStore.GetFileContent(path));
    }

    /// <summary>
    /// Async version of ListFilesInDirectory.
    /// </summary>
    public static async Task<IEnumerable<IImmutableList<string>>> ListFilesInDirectoryAsync(this IFileStore fileStore, IImmutableList<string> directoryPath)
    {
        if (fileStore is FileStoreHttpClient httpClient)
        {
            return await httpClient.ListFilesInDirectoryAsync(directoryPath);
        }

        // Fallback to synchronous version for other implementations
        return await Task.Run(() => fileStore.ListFilesInDirectory(directoryPath));
    }

    /// <summary>
    /// Async version of SetFileContent.
    /// </summary>
    public static async Task SetFileContentAsync(this IFileStore fileStore, IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        if (fileStore is FileStoreHttpClient httpClient)
        {
            await httpClient.SetFileContentAsync(path, fileContent);
        }
        else
        {
            // Fallback to synchronous version for other implementations
            await Task.Run(() => fileStore.SetFileContent(path, fileContent));
        }
    }

    /// <summary>
    /// Async version of AppendFileContent.
    /// </summary>
    public static async Task AppendFileContentAsync(this IFileStore fileStore, IImmutableList<string> path, ReadOnlyMemory<byte> fileContent)
    {
        if (fileStore is FileStoreHttpClient httpClient)
        {
            await httpClient.AppendFileContentAsync(path, fileContent);
        }
        else
        {
            // Fallback to synchronous version for other implementations
            await Task.Run(() => fileStore.AppendFileContent(path, fileContent));
        }
    }

    /// <summary>
    /// Async version of DeleteFile.
    /// </summary>
    public static async Task DeleteFileAsync(this IFileStore fileStore, IImmutableList<string> path)
    {
        if (fileStore is FileStoreHttpClient httpClient)
        {
            await httpClient.DeleteFileAsync(path);
        }
        else
        {
            // Fallback to synchronous version for other implementations
            await Task.Run(() => fileStore.DeleteFile(path));
        }
    }
}
