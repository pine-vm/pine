using Microsoft.AspNetCore.Http;
using Pine.Core.IO;
using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text.Json;
using System.Threading.Tasks;

namespace Pine.Core.Http;

/// <summary>
/// HTTP server middleware that exposes an IFileStore via REST API.
/// </summary>
public class FileStoreHttpServer(IFileStore fileStore)
{
    private readonly IFileStore _fileStore =
        fileStore ?? throw new ArgumentNullException(nameof(fileStore));

    /// <summary>
    /// Tries to handle the HTTP request for file or directory operations.
    /// Returns true if the request was handled by this server, otherwise false.
    /// </summary>
    /// <param name="context">The HTTP context for the current request.</param>
    /// <returns>
    /// A task that represents the asynchronous operation. The task result contains true if the request was handled; otherwise, false.
    /// </returns>
    public async Task<bool> TryHandleRequestAsync(HttpContext context)
    {
        var path = context.Request.Path.Value ?? "";

        try
        {
            if (path.StartsWith("/files/"))
            {
                var remainingPath = path[7..];
                await HandleFileRequestAsync(context, remainingPath, context.Request.Method);
                return true;
            }
            else if (path.StartsWith("/dirs/"))
            {
                var remainingPath = path[6..];
                await HandleDirectoryRequestAsync(context, remainingPath, context.Request.Method);
                return true;
            }

            return false; // Not a FileStore request
        }
        catch (Exception ex)
        {
            context.Response.StatusCode = 500;
            await context.Response.WriteAsync($"Internal Server Error: {ex.Message}");
            return true; // We handled it (with an error)
        }
    }

    private async Task HandleFileRequestAsync(HttpContext context, string pathString, string method)
    {
        var pathSegments = ParsePathSegments(pathString);

        switch (method.ToUpperInvariant())
        {
            case "GET":
                await HandleGetFileAsync(context, pathSegments);
                break;
            case "HEAD":
                await HandleHeadFileAsync(context, pathSegments);
                break;
            case "PUT":
                await HandlePutFileAsync(context, pathSegments);
                break;
            case "POST":
                await HandlePostFileAsync(context, pathSegments);
                break;
            case "DELETE":
                await HandleDeleteFileAsync(context, pathSegments);
                break;
            default:
                context.Response.StatusCode = 405; // Method Not Allowed
                break;
        }
    }

    private async Task HandleDirectoryRequestAsync(HttpContext context, string pathString, string method)
    {
        if (!method.Equals("GET", StringComparison.InvariantCultureIgnoreCase))
        {
            context.Response.StatusCode = 405; // Method Not Allowed
            return;
        }

        var pathSegments = ParsePathSegments(pathString);
        await HandleGetDirectoryAsync(context, pathSegments);
    }

    private async Task HandleGetFileAsync(HttpContext context, IImmutableList<string> pathSegments)
    {
        var fileContent = _fileStore.GetFileContent(pathSegments);
        if (fileContent == null)
        {
            context.Response.StatusCode = 404;
            return;
        }

        var contentBytes = fileContent.Value;
        var etag = ComputeETag(contentBytes);

        context.Response.StatusCode = 200;
        context.Response.ContentType = "application/octet-stream";
        context.Response.ContentLength = contentBytes.Length;
        context.Response.Headers["ETag"] = $"\"{etag}\"";

        // Handle Range requests
        if (context.Request.Headers.ContainsKey("Range"))
        {
            // For simplicity, we'll implement basic range support later if needed
            // For now, return the full content
        }

        // Check for JSON format request
        var acceptHeader = context.Request.Headers["Accept"].FirstOrDefault();
        if (acceptHeader?.Contains("application/json") == true)
        {
            var base64Content = Convert.ToBase64String(contentBytes.Span);

            var jsonResponse = JsonSerializer.Serialize(new { content = base64Content });

            context.Response.ContentType = "application/json";
            await context.Response.WriteAsync(jsonResponse);
        }
        else
        {
            await context.Response.Body.WriteAsync(contentBytes);
        }
    }

    private async Task HandleHeadFileAsync(HttpContext context, IImmutableList<string> pathSegments)
    {
        var fileContent = _fileStore.GetFileContent(pathSegments);
        if (fileContent == null)
        {
            context.Response.StatusCode = 404;
            return;
        }

        var contentBytes = fileContent.Value;
        var etag = ComputeETag(contentBytes);

        context.Response.StatusCode = 200;
        context.Response.ContentType = "application/octet-stream";
        context.Response.ContentLength = contentBytes.Length;
        context.Response.Headers["ETag"] = $"\"{etag}\"";
        context.Response.Headers["Last-Modified"] = DateTimeOffset.UtcNow.ToString("R");
    }

    private async Task HandlePutFileAsync(HttpContext context, IImmutableList<string> pathSegments)
    {
        var existingContent = _fileStore.GetFileContent(pathSegments);
        bool fileExists = existingContent != null;

        // Handle If-None-Match: * (create only)
        var ifNoneMatch = context.Request.Headers["If-None-Match"].FirstOrDefault();
        if (ifNoneMatch == "*" && fileExists)
        {
            context.Response.StatusCode = 412; // Precondition Failed
            return;
        }

        // Handle If-Match (replace only if ETag matches)
        var ifMatch = context.Request.Headers["If-Match"].FirstOrDefault();
        if (ifMatch != null && fileExists)
        {
            var currentETag = ComputeETag(existingContent!.Value);
            var requestedETag = ifMatch.Trim('"');
            if (currentETag != requestedETag)
            {
                context.Response.StatusCode = 412; // Precondition Failed
                return;
            }
        }

        // Read the request body
        using var memoryStream = new MemoryStream();
        await context.Request.Body.CopyToAsync(memoryStream);
        var newContent = memoryStream.ToArray();

        // Set the file content
        _fileStore.SetFileContent(pathSegments, newContent);

        // Compute new ETag
        var newETag = ComputeETag(newContent);

        context.Response.StatusCode = fileExists ? 204 : 201; // No Content or Created
        context.Response.Headers["ETag"] = $"\"{newETag}\"";
    }

    private async Task HandlePostFileAsync(HttpContext context, IImmutableList<string> pathSegments)
    {
        var operation = context.Request.Headers["X-Operation"].FirstOrDefault();
        if (operation != "append")
        {
            context.Response.StatusCode = 400; // Bad Request
            await context.Response.WriteAsync("X-Operation: append header required");
            return;
        }

        var existingContent = _fileStore.GetFileContent(pathSegments);

        // Handle If-Match for concurrency control
        var ifMatch = context.Request.Headers["If-Match"].FirstOrDefault();
        if (ifMatch != null && existingContent != null)
        {
            var currentETag = ComputeETag(existingContent.Value);
            var requestedETag = ifMatch.Trim('"');
            if (currentETag != requestedETag)
            {
                context.Response.StatusCode = 412; // Precondition Failed
                return;
            }
        }

        // Handle X-Expected-Length
        var expectedLengthHeader = context.Request.Headers["X-Expected-Length"].FirstOrDefault();
        if (expectedLengthHeader != null && int.TryParse(expectedLengthHeader, out var expectedLength))
        {
            var currentLength = existingContent?.Length ?? 0;
            if (currentLength != expectedLength)
            {
                context.Response.StatusCode = 412; // Precondition Failed
                return;
            }
        }

        // AppendFileContent should always create the file if it doesn't exist (per IFileStore interface)
        // No need to check for existence or require query parameters

        // Read the request body
        using var memoryStream = new MemoryStream();
        await context.Request.Body.CopyToAsync(memoryStream);
        var appendContent = memoryStream.ToArray();

        // Append to the file
        _fileStore.AppendFileContent(pathSegments, appendContent);

        // Get the updated content to compute new ETag and length
        var updatedContent = _fileStore.GetFileContent(pathSegments);
        if (updatedContent != null)
        {
            var newETag = ComputeETag(updatedContent.Value);
            var newLength = updatedContent.Value.Length;

            context.Response.StatusCode = 200;
            context.Response.Headers["ETag"] = $"\"{newETag}\"";
            context.Response.Headers["X-File-Length"] = newLength.ToString();

            var response = JsonSerializer.Serialize(new { length = newLength, etag = newETag });
            context.Response.ContentType = "application/json";
            await context.Response.WriteAsync(response);
        }
    }

    private async Task HandleDeleteFileAsync(HttpContext context, IImmutableList<string> pathSegments)
    {
        var existingContent = _fileStore.GetFileContent(pathSegments);
        if (existingContent == null)
        {
            context.Response.StatusCode = 404;
            return;
        }

        _fileStore.DeleteFile(pathSegments);
        context.Response.StatusCode = 204; // No Content
    }

    private async Task HandleGetDirectoryAsync(HttpContext context, IImmutableList<string> pathSegments)
    {
        try
        {
            // List all files under the directory (all descendants). Emit only file entries.
            var files = _fileStore.ListFilesInDirectory(pathSegments);

            var entries = files
                .Select(relativePath => new DirectoryEntry(string.Join('/', relativePath), "file"))
                .ToArray();

            var response = new DirectoryListingResponse(entries);
            var json = JsonSerializer.Serialize(response, s_jsonSerializerOptions);

            context.Response.StatusCode = 200;
            context.Response.ContentType = "application/json";
            await context.Response.WriteAsync(json);
        }
        catch
        {
            context.Response.StatusCode = 404;
        }
    }

    private static IImmutableList<string> ParsePathSegments(string pathString)
    {
        if (string.IsNullOrEmpty(pathString))
            return ImmutableList<string>.Empty;

        return pathString.Split('/', StringSplitOptions.RemoveEmptyEntries).ToImmutableList();
    }

    private static string ComputeETag(ReadOnlyMemory<byte> content)
    {
        using var sha256 = SHA256.Create();
        var hash = sha256.ComputeHash(content.ToArray());
        return Convert.ToHexStringLower(hash)[..16]; // Use first 16 characters for brevity
    }

    /// <summary>
    /// Represents the response for a directory listing operation in the file store HTTP server.
    /// </summary>
    public record DirectoryListingResponse
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="DirectoryListingResponse"/> record with the specified entries.
        /// </summary>
        /// <param name="entries">The array of directory entries included in the response.</param>
        public DirectoryListingResponse(DirectoryEntry[] entries) => Entries = entries;

        /// <summary>
        /// Gets the array of directory entries included in the response.
        /// </summary>
        public DirectoryEntry[] Entries { get; init; }
    }

    /// <summary>
    /// Represents an entry in a directory listing. Only files/blobs are emitted.
    /// </summary>
    public record DirectoryEntry
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="DirectoryEntry"/> record with the specified parameters.
        /// </summary>
        /// <param name="name">The relative path (from the requested directory) to the file, using '/' separators.</param>
        /// <param name="type">The type of the entry ("file").</param>
        public DirectoryEntry(string name, string type)
        {
            Name = name;
            Type = type;
        }

        /// <summary>
        /// Gets the relative path (from the requested directory) to the file, using '/' separators.
        /// </summary>
        public string Name { get; init; }

        /// <summary>
        /// Gets the type of the entry ("file").
        /// </summary>
        public string Type { get; init; }
    }

    private static readonly JsonSerializerOptions s_jsonSerializerOptions =
        FileStoreHttpClient.BuildJsonSerializerOptions();
}

/// <summary>
/// ASP.NET Core middleware wrapper for FileStoreHttpServer
/// </summary>
public class FileStoreHttpServerMiddleware(RequestDelegate next, IFileStore fileStore)
{
    private readonly RequestDelegate _next = next;
    private readonly FileStoreHttpServer _server = new(fileStore);

    public async Task InvokeAsync(HttpContext context)
    {
        var handled = await _server.TryHandleRequestAsync(context);

        if (!handled)
        {
            // Pass the request to the next middleware
            await _next(context);
        }
    }
}
