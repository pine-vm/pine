using AwesomeAssertions;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.TestHost;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Pine.Core.Http;
using Pine.Core.IO;
using System;
using System.Collections.Immutable;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class FileStoreHttpClientTests
{
    private sealed class TestSetup : IAsyncDisposable
    {
        public WebApplication App { get; }
        public TestServer Server { get; }
        public HttpClient HttpClient { get; }
        public IFileStore ServerFileStore { get; }
        public FileStoreHttpClient Client { get; }

        public TestSetup()
        {
            ServerFileStore = new FileStoreFromConcurrentDictionary();

            var builder = WebApplication.CreateBuilder();
            builder.WebHost.UseTestServer();
            builder.Services.AddSingleton(ServerFileStore);

            var app = builder.Build();
            app.UseMiddleware<FileStoreHttpServerMiddleware>();

            App = app;

            app.Start();

            Server = app.GetTestServer();
            HttpClient = Server.CreateClient();
            Client = new FileStoreHttpClient(HttpClient);
        }

        public async ValueTask DisposeAsync()
        {
            HttpClient?.Dispose();
            Server?.Dispose();

            if (App is { } app)
            {
                await app.StopAsync();
                await app.DisposeAsync();
            }
        }
    }

    [Fact]
    public async Task GetFileContent_returns_null_for_nonexistent_file()
    {
        // Arrange
        await using var setup = new TestSetup();

        // Act
        var content = await setup.Client.GetFileContentAsync(ImmutableList.Create("nonexistent.txt"));

        // Assert
        content.Should().BeNull();
    }

    [Fact]
    public async Task GetFileContent_returns_file_content_for_existing_file()
    {
        // Arrange
        await using var setup = new TestSetup();
        var path = ImmutableList.Create("test", "file.txt");
        var expectedContent = "Hello, World!"u8.ToArray();
        setup.ServerFileStore.SetFileContent(path, expectedContent);

        // Act
        var content = await setup.Client.GetFileContentAsync(path);

        // Assert
        content.Should().NotBeNull();
        content.Value.ToArray().Should().BeEquivalentTo(expectedContent);
    }

    [Fact]
    public async Task ListFilesInDirectory_returns_files_in_directory()
    {
        // Arrange
        await using var setup = new TestSetup();
        setup.ServerFileStore.SetFileContent(ImmutableList.Create("dir", "file1.txt"), "content1"u8.ToArray());
        setup.ServerFileStore.SetFileContent(ImmutableList.Create("dir", "file2.txt"), "content2"u8.ToArray());
        setup.ServerFileStore.SetFileContent(ImmutableList.Create("dir", "subdir", "file3.txt"), "content3"u8.ToArray());

        // Act
        var files = await setup.Client.ListFilesInDirectoryAsync(ImmutableList.Create("dir"));

        // Assert
        files.Should().NotBeEmpty();
    }

    [Fact]
    public async Task SetFileContent_creates_new_file()
    {
        // Arrange
        await using var setup = new TestSetup();
        var path = ImmutableList.Create("new", "file.txt");
        var content = "New file content"u8.ToArray();

        // Act
        await setup.Client.SetFileContentAsync(path, content);

        // Assert
        var storedContent = setup.ServerFileStore.GetFileContent(path);
        storedContent.Should().NotBeNull();
        storedContent.Value.ToArray().Should().BeEquivalentTo(content);
    }

    [Fact]
    public async Task SetFileContent_replaces_existing_file()
    {
        // Arrange
        await using var setup = new TestSetup();
        var path = ImmutableList.Create("existing", "file.txt");
        setup.ServerFileStore.SetFileContent(path, "original content"u8.ToArray());
        var newContent = "updated content"u8.ToArray();

        // Act
        await setup.Client.SetFileContentAsync(path, newContent);

        // Assert
        var storedContent = setup.ServerFileStore.GetFileContent(path);
        storedContent.Should().NotBeNull();
        storedContent.Value.ToArray().Should().BeEquivalentTo(newContent);
    }

    [Fact]
    public async Task AppendFileContent_appends_to_existing_file()
    {
        // Arrange
        await using var setup = new TestSetup();

        var path = ImmutableList.Create("append", "file.txt");
        var originalContent = "Hello"u8.ToArray();
        setup.ServerFileStore.SetFileContent(path, originalContent);
        var appendContent = " World"u8.ToArray();

        // Act
        await setup.Client.AppendFileContentAsync(path, appendContent);

        // Assert
        var storedContent = setup.ServerFileStore.GetFileContent(path);
        storedContent.Should().NotBeNull();
        Encoding.UTF8.GetString(storedContent.Value.Span).Should().Be("Hello World");
    }

    [Fact]
    public async Task AppendFileContent_creates_file_if_not_exists()
    {
        // Arrange
        await using var setup = new TestSetup();

        var path = ImmutableList.Create("append", "new_file.txt");
        var appendContent = "New content via append"u8.ToArray();

        // Verify file doesn't exist initially
        var initialContent = setup.ServerFileStore.GetFileContent(path);
        initialContent.Should().BeNull();

        // Act
        await setup.Client.AppendFileContentAsync(path, appendContent);

        // Assert
        var storedContent = setup.ServerFileStore.GetFileContent(path);
        storedContent.Should().NotBeNull();
        Encoding.UTF8.GetString(storedContent.Value.Span).Should().Be("New content via append");
    }

    [Fact]
    public async Task DeleteFile_removes_existing_file()
    {
        // Arrange
        await using var setup = new TestSetup();

        var path = ImmutableList.Create("delete", "file.txt");
        setup.ServerFileStore.SetFileContent(path, "content to delete"u8.ToArray());

        // Act
        await setup.Client.DeleteFileAsync(path);

        // Assert
        var storedContent = setup.ServerFileStore.GetFileContent(path);
        storedContent.Should().BeNull();
    }

    [Fact]
    public async Task DeleteFile_does_not_fail_for_nonexistent_file()
    {
        // Arrange
        await using var setup = new TestSetup();

        // Act & Assert - should not throw
        await setup.Client.DeleteFileAsync(ImmutableList.Create("nonexistent.txt"));
    }

    [Fact]
    public async Task Server_handles_lowercase_get_verb()
    {
        // Arrange
        await using var setup = new TestSetup();

        var path = ImmutableList.Create("test", "file.txt");
        var expectedContent = "Hello, World!"u8.ToArray();
        setup.ServerFileStore.SetFileContent(path, expectedContent);

        // Act - Make direct HTTP request with lowercase verb
        var request = new HttpRequestMessage(new HttpMethod("get"), "/files/test/file.txt");
        var response = await setup.HttpClient.SendAsync(request);

        // Assert
        response.IsSuccessStatusCode.Should().BeTrue();
        var content = await response.Content.ReadAsByteArrayAsync();
        content.Should().BeEquivalentTo(expectedContent);
    }

    [Fact]
    public async Task Server_handles_lowercase_put_verb()
    {
        // Arrange
        await using var setup = new TestSetup();

        var path = ImmutableList.Create("new", "file.txt");
        var content = "New file content"u8.ToArray();

        // Act - Make direct HTTP request with lowercase verb
        var request = new HttpRequestMessage(new HttpMethod("put"), "/files/new/file.txt")
        {
            Content = new ByteArrayContent(content)
        };
        request.Content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/octet-stream");
        var response = await setup.HttpClient.SendAsync(request);

        // Assert
        response.IsSuccessStatusCode.Should().BeTrue();
        var storedContent = setup.ServerFileStore.GetFileContent(path);
        storedContent.Should().NotBeNull();
        storedContent.Value.ToArray().Should().BeEquivalentTo(content);
    }

    [Fact]
    public async Task Server_handles_lowercase_post_verb_for_append()
    {
        // Arrange
        await using var setup = new TestSetup();

        var path = ImmutableList.Create("append", "file.txt");
        var originalContent = "Hello"u8.ToArray();
        setup.ServerFileStore.SetFileContent(path, originalContent);
        var appendContent = " World"u8.ToArray();

        // Act - Make direct HTTP request with lowercase verb
        var request = new HttpRequestMessage(new HttpMethod("post"), "/files/append/file.txt")
        {
            Content = new ByteArrayContent(appendContent)
        };
        request.Content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/octet-stream");
        request.Headers.Add("X-Operation", "append");
        var response = await setup.HttpClient.SendAsync(request);

        // Assert
        response.IsSuccessStatusCode.Should().BeTrue();
        var storedContent = setup.ServerFileStore.GetFileContent(path);
        storedContent.Should().NotBeNull();
        Encoding.UTF8.GetString(storedContent.Value.Span).Should().Be("Hello World");
    }

    [Fact]
    public async Task Server_handles_lowercase_delete_verb()
    {
        // Arrange
        await using var setup = new TestSetup();

        var path = ImmutableList.Create("delete", "file.txt");
        setup.ServerFileStore.SetFileContent(path, "content to delete"u8.ToArray());

        // Act - Make direct HTTP request with lowercase verb
        var request = new HttpRequestMessage(new HttpMethod("delete"), "/files/delete/file.txt");
        var response = await setup.HttpClient.SendAsync(request);

        // Assert
        response.IsSuccessStatusCode.Should().BeTrue();
        var storedContent = setup.ServerFileStore.GetFileContent(path);
        storedContent.Should().BeNull();
    }

    [Fact]
    public async Task Server_handles_lowercase_get_verb_for_directory_listing()
    {
        // Arrange
        await using var setup = new TestSetup();

        setup.ServerFileStore.SetFileContent(ImmutableList.Create("dir", "file1.txt"), "content1"u8.ToArray());
        setup.ServerFileStore.SetFileContent(ImmutableList.Create("dir", "file2.txt"), "content2"u8.ToArray());

        // Act - Make direct HTTP request with lowercase verb
        var request = new HttpRequestMessage(new HttpMethod("get"), "/dirs/dir");
        var response = await setup.HttpClient.SendAsync(request);

        // Assert
        response.IsSuccessStatusCode.Should().BeTrue();
        var responseContent = await response.Content.ReadAsStringAsync();
        responseContent.Should().NotBeEmpty();
        responseContent.Should().Contain("file1.txt");
        responseContent.Should().Contain("file2.txt");
    }
}
