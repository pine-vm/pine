using AwesomeAssertions;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.TestHost;
using Microsoft.Extensions.DependencyInjection;
using Pine.Core.Http;
using Pine.Core.IO;
using System;
using System.Collections.Immutable;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class FileStoreHttpServerTests
{
    private class TestSetup : IDisposable
    {
        public TestServer Server { get; }
        public HttpClient Client { get; }
        public IFileStore FileStore { get; }

        public TestSetup()
        {
            FileStore = new FileStoreFromConcurrentDictionary();

            var builder = new WebHostBuilder()
                .ConfigureServices(services =>
                {
                    services.AddSingleton(FileStore);
                })
                .Configure(app =>
                {
                    app.UseMiddleware<FileStoreHttpServerMiddleware>();
                });

            Server = new TestServer(builder);
            Client = Server.CreateClient();
        }

        public void Dispose()
        {
            Client?.Dispose();
            Server?.Dispose();
        }
    }

    [Fact]
    public async Task GET_files_returns_404_for_nonexistent_file()
    {
        using var setup = new TestSetup();
        var response = await setup.Client.GetAsync("/files/nonexistent.txt");

        response.StatusCode.Should().Be(System.Net.HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task GET_files_returns_file_content()
    {
        // Arrange
        using var setup = new TestSetup();
        var path = ImmutableList.Create("test", "file.txt");
        var content = "Hello, World!"u8.ToArray();
        setup.FileStore.SetFileContent(path, content);

        // Act
        var response = await setup.Client.GetAsync("/files/test/file.txt");

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
        response.Content.Headers.ContentType?.MediaType.Should().Be("application/octet-stream");

        var responseContent = await response.Content.ReadAsByteArrayAsync();
        responseContent.Should().BeEquivalentTo(content);
    }

    [Fact]
    public async Task HEAD_files_returns_headers_for_existing_file()
    {
        // Arrange
        using var setup = new TestSetup();
        var path = ImmutableList.Create("test", "file.txt");
        var content = "Hello, World!"u8.ToArray();
        setup.FileStore.SetFileContent(path, content);

        // Act
        var request = new HttpRequestMessage(HttpMethod.Head, "/files/test/file.txt");
        var response = await setup.Client.SendAsync(request);

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
        response.Content.Headers.ContentLength.Should().Be(content.Length);
        response.Headers.ETag.Should().NotBeNull();
    }

    [Fact]
    public async Task HEAD_files_returns_404_for_nonexistent_file()
    {
        using var setup = new TestSetup();
        var request = new HttpRequestMessage(HttpMethod.Head, "/files/nonexistent.txt");
        var response = await setup.Client.SendAsync(request);

        response.StatusCode.Should().Be(System.Net.HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task GET_dirs_returns_directory_listing()
    {
        // Arrange
        using var setup = new TestSetup();
        setup.FileStore.SetFileContent(ImmutableList.Create("dir1", "file1.txt"), "content1"u8.ToArray());
        setup.FileStore.SetFileContent(ImmutableList.Create("dir1", "file2.txt"), "content2"u8.ToArray());
        setup.FileStore.SetFileContent(ImmutableList.Create("dir1", "subdir", "file3.txt"), "content3"u8.ToArray());

        // Act
        var response = await setup.Client.GetAsync("/dirs/dir1");

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
        response.Content.Headers.ContentType?.MediaType.Should().Be("application/json");

        var responseContent = await response.Content.ReadAsStringAsync();
        var result = JsonSerializer.Deserialize<DirectoryListingResponse>(responseContent, JsonSerializerOptions.Web);

        result.Should().NotBeNull();
        result!.Entries.Should().NotBeEmpty();
    }

    [Fact]
    public async Task PUT_files_creates_new_file()
    {
        // Arrange
        using var setup = new TestSetup();
        var content = "New file content"u8.ToArray();

        // Act
        var response = await setup.Client.PutAsync("/files/new/file.txt", new ByteArrayContent(content));

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.Created);
        response.Headers.ETag.Should().NotBeNull();

        // Verify file was created
        var storedContent = setup.FileStore.GetFileContent(ImmutableList.Create("new", "file.txt"));
        storedContent.Should().NotBeNull();
        storedContent!.Value.ToArray().Should().BeEquivalentTo(content);
    }

    [Fact]
    public async Task PUT_files_replaces_existing_file()
    {
        // Arrange
        using var setup = new TestSetup();
        var path = ImmutableList.Create("existing", "file.txt");
        setup.FileStore.SetFileContent(path, "original content"u8.ToArray());
        var newContent = "updated content"u8.ToArray();

        // Act
        var response = await setup.Client.PutAsync("/files/existing/file.txt", new ByteArrayContent(newContent));

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.NoContent);
        response.Headers.ETag.Should().NotBeNull();

        // Verify file was updated
        var storedContent = setup.FileStore.GetFileContent(path);
        storedContent.Should().NotBeNull();
        storedContent!.Value.ToArray().Should().BeEquivalentTo(newContent);
    }

    [Fact]
    public async Task POST_files_appends_to_existing_file()
    {
        // Arrange
        using var setup = new TestSetup();
        var path = ImmutableList.Create("append", "file.txt");
        var originalContent = "Hello"u8.ToArray();
        setup.FileStore.SetFileContent(path, originalContent);
        var appendContent = " World"u8.ToArray();

        // Act
        var request = new HttpRequestMessage(HttpMethod.Post, "/files/append/file.txt")
        {
            Content = new ByteArrayContent(appendContent)
        };
        request.Headers.Add("X-Operation", "append");
        var response = await setup.Client.SendAsync(request);

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);

        // Verify file was appended
        var storedContent = setup.FileStore.GetFileContent(path);
        storedContent.Should().NotBeNull();
        Encoding.UTF8.GetString(storedContent!.Value.Span).Should().Be("Hello World");
    }

    [Fact]
    public async Task DELETE_files_removes_existing_file()
    {
        // Arrange
        using var setup = new TestSetup();
        var path = ImmutableList.Create("delete", "file.txt");
        setup.FileStore.SetFileContent(path, "content to delete"u8.ToArray());

        // Act
        var response = await setup.Client.DeleteAsync("/files/delete/file.txt");

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.NoContent);

        // Verify file was deleted
        var storedContent = setup.FileStore.GetFileContent(path);
        storedContent.Should().BeNull();
    }

    [Fact]
    public async Task DELETE_files_returns_404_for_nonexistent_file()
    {
        using var setup = new TestSetup();
        var response = await setup.Client.DeleteAsync("/files/nonexistent.txt");

        response.StatusCode.Should().Be(System.Net.HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task PUT_files_with_if_none_match_star_fails_when_file_exists()
    {
        // Arrange
        using var setup = new TestSetup();
        var path = ImmutableList.Create("existing", "file.txt");
        setup.FileStore.SetFileContent(path, "existing content"u8.ToArray());
        var newContent = "should not be saved"u8.ToArray();

        // Act
        var request = new HttpRequestMessage(HttpMethod.Put, "/files/existing/file.txt")
        {
            Content = new ByteArrayContent(newContent)
        };
        request.Headers.Add("If-None-Match", "*");
        var response = await setup.Client.SendAsync(request);

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.PreconditionFailed);

        // Verify file was not changed
        var storedContent = setup.FileStore.GetFileContent(path);
        Encoding.UTF8.GetString(storedContent!.Value.Span).Should().Be("existing content");
    }

    [Fact]
    public async Task GET_files_with_accept_json_returns_base64_content()
    {
        // Arrange
        using var setup = new TestSetup();
        var path = ImmutableList.Create("test", "file.txt");
        var content = "Hello, World!"u8.ToArray();
        setup.FileStore.SetFileContent(path, content);

        // Act
        var request = new HttpRequestMessage(HttpMethod.Get, "/files/test/file.txt");
        request.Headers.Accept.Add(new System.Net.Http.Headers.MediaTypeWithQualityHeaderValue("application/json"));
        var response = await setup.Client.SendAsync(request);

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
        response.Content.Headers.ContentType?.MediaType.Should().Be("application/json");

        var responseContent = await response.Content.ReadAsStringAsync();
        var jsonResponse = JsonSerializer.Deserialize<JsonObject>(responseContent, JsonSerializerOptions.Web);
        jsonResponse.Should().NotBeNull();
    }

    [Fact]
    public async Task GET_dirs_returns_empty_for_nonexistent_directory()
    {
        using var setup = new TestSetup();
        var response = await setup.Client.GetAsync("/dirs/nonexistent");

        response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
        response.Content.Headers.ContentType?.MediaType.Should().Be("application/json");

        var responseContent = await response.Content.ReadAsStringAsync();
        var result = JsonSerializer.Deserialize<DirectoryListingResponse>(responseContent, JsonSerializerOptions.Web);

        result.Should().NotBeNull();
        result!.Entries.Should().BeEmpty();
    }

    [Fact]
    public async Task POST_files_requires_x_operation_append_header()
    {
        // Arrange
        using var setup = new TestSetup();
        var content = "some content"u8.ToArray();

        // Act
        var request = new HttpRequestMessage(HttpMethod.Post, "/files/test/file.txt")
        {
            Content = new ByteArrayContent(content)
        };
        // Don't add X-Operation header
        var response = await setup.Client.SendAsync(request);

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.BadRequest);
    }

    private record DirectoryListingResponse(DirectoryEntry[] Entries);
    private record DirectoryEntry(string Name, string Type);
}
