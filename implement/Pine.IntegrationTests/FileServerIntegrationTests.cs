using AwesomeAssertions;
using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

/// <summary>
/// Integration tests for the run-file-server CLI command.
/// These tests start the actual pine executable and test both public and authenticated access.
/// </summary>
public class FileServerIntegrationTests
{
    private class TestSetup : IDisposable
    {
        public string TempDirectory { get; }
        public int Port { get; }
        public Process? ServerProcess { get; private set; }
        public HttpClient HttpClient { get; }

        public TestSetup()
        {
            TempDirectory = Path.Combine(Path.GetTempPath(), "pine-fileserver-test-" + Guid.NewGuid().ToString("N")[..8]);
            Directory.CreateDirectory(TempDirectory);

            // Find an available port
            Port = GetAvailablePort();

            HttpClient = new HttpClient
            {
                Timeout = TimeSpan.FromSeconds(30)
            };
        }

        public async Task StartServerAsync(string? authPassword = null, bool useFileStore = true)
        {
            var pineExecutablePath = GetPineExecutablePath();

            var args = new StringBuilder();
            args.Append("run-file-server");
            args.Append($" --port {Port}");

            if (useFileStore)
            {
                args.Append($" --store \"{TempDirectory}\"");
            }

            if (authPassword is not null)
            {
                args.Append($" --auth-password \"{authPassword}\"");
            }

            var processStartInfo = new ProcessStartInfo
            {
                FileName = pineExecutablePath,
                Arguments = args.ToString(),
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                CreateNoWindow = true
            };

            ServerProcess = Process.Start(processStartInfo);

            if (ServerProcess is null)
            {
                throw new InvalidOperationException("Failed to start server process");
            }

            // Wait for server to start up
            await WaitForServerToStart();
        }

        private async Task WaitForServerToStart()
        {
            const int MaxAttempts = 30; // 30 seconds
            const int DelayMs = 1000;

            for (var attempt = 0; attempt < MaxAttempts; attempt++)
            {
                try
                {
                    var response = await HttpClient.GetAsync($"http://localhost:{Port}/files/health-check");
                    // Any response (even 404) means the server is up
                    return;
                }
                catch (HttpRequestException)
                {
                    // Server not ready yet
                    await Task.Delay(DelayMs);
                }
            }

            throw new TimeoutException("Server did not start within the expected time");
        }

        private static string GetPineExecutablePath()
        {
            // Look for the pine executable in the build output
            var implementDir = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "pine"));
            var executableName = Environment.OSVersion.Platform is PlatformID.Win32NT ? "pine.exe" : "pine";

            // Check bin/Debug and bin/Release directories
            var debugPath = Path.Combine(implementDir, "bin", "Debug", "net10.0", executableName);
            var releasePath = Path.Combine(implementDir, "bin", "Release", "net10.0", executableName);

            if (File.Exists(debugPath))
                return debugPath;

            if (File.Exists(releasePath))
                return releasePath;

            throw new FileNotFoundException($"Pine executable not found. Looked for: {debugPath}, {releasePath}");
        }

        private static int GetAvailablePort()
        {
            // Bind to port 0 to let the OS pick an ephemeral free port, then release it.
            // This avoids proxy/HTTP interception issues seen with HTTP probing.
            var listener = new TcpListener(IPAddress.Loopback, 0);
            listener.Start();
            try
            {
                return ((IPEndPoint)listener.LocalEndpoint).Port;
            }
            finally
            {
                listener.Stop();
            }
        }

        public void Dispose()
        {
            HttpClient?.Dispose();

            if (ServerProcess is not null && !ServerProcess.HasExited)
            {
                try
                {
                    ServerProcess.Kill();
                    ServerProcess.WaitForExit(5000);
                }
                catch
                {
                    // Ignore errors during cleanup
                }
                ServerProcess?.Dispose();
            }

            if (Directory.Exists(TempDirectory))
            {
                try
                {
                    Directory.Delete(TempDirectory, recursive: true);
                }
                catch
                {
                    // Ignore errors during cleanup
                }
            }
        }
    }

    [Fact]
    public async Task Server_starts_successfully_with_file_store()
    {
        // Arrange
        using var setup = new TestSetup();

        // Act
        await setup.StartServerAsync(useFileStore: true);

        // Assert
        // If we get here without exception, the server started successfully
        var response = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/files/nonexistent.txt");
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task Server_starts_successfully_with_in_memory_store()
    {
        // Arrange
        using var setup = new TestSetup();

        // Act
        await setup.StartServerAsync(useFileStore: false);

        // Assert
        // If we get here without exception, the server started successfully
        var response = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/files/nonexistent.txt");
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task Public_access_allows_file_operations()
    {
        // Arrange
        using var setup = new TestSetup();
        await setup.StartServerAsync(useFileStore: true);

        var filePath = "test/file.txt";
        var originalContent = "Hello, World!"u8.ToArray();

        // Act & Assert - Create file
        using (var content = new ByteArrayContent(originalContent))
        {
            content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/octet-stream");
            var response = await setup.HttpClient.PutAsync($"http://localhost:{setup.Port}/files/{filePath}", content);
            response.EnsureSuccessStatusCode();
        }

        // Verify file was created via HTTP
        var getResponse = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/files/{filePath}");
        getResponse.EnsureSuccessStatusCode();
        var retrievedContent = await getResponse.Content.ReadAsByteArrayAsync();
        retrievedContent.Should().BeEquivalentTo(originalContent);

        // Verify file was created on disk
        var diskFilePath = Path.Combine(setup.TempDirectory, "test", "file.txt");
        File.Exists(diskFilePath).Should().BeTrue();
        var diskContent = await File.ReadAllBytesAsync(diskFilePath);
        diskContent.Should().BeEquivalentTo(originalContent);
    }

    [Fact]
    public async Task Append_creates_file_if_not_exists_and_verifies_consistency()
    {
        // Arrange
        using var setup = new TestSetup();
        await setup.StartServerAsync(useFileStore: true);

        var filePath = "append/new_file.txt";
        var appendContent = "New content via append"u8.ToArray();

        // Act - Append to non-existent file
        using (var content = new ByteArrayContent(appendContent))
        {
            content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/octet-stream");
            var request = new HttpRequestMessage(HttpMethod.Post, $"http://localhost:{setup.Port}/files/{filePath}")
            {
                Content = content
            };
            request.Headers.Add("X-Operation", "append");

            var response = await setup.HttpClient.SendAsync(request);
            response.EnsureSuccessStatusCode();
        }

        // Assert - Verify via ListFiles
        var listResponse = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/dirs/append");
        listResponse.EnsureSuccessStatusCode();
        var listContent = await listResponse.Content.ReadAsStringAsync();
        listContent.Should().Contain("new_file.txt");

        // Verify file content via HTTP
        var getResponse = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/files/{filePath}");
        getResponse.EnsureSuccessStatusCode();
        var retrievedContent = await getResponse.Content.ReadAsByteArrayAsync();
        retrievedContent.Should().BeEquivalentTo(appendContent);

        // Verify file content on disk
        var diskFilePath = Path.Combine(setup.TempDirectory, "append", "new_file.txt");
        File.Exists(diskFilePath).Should().BeTrue();
        var diskContent = await File.ReadAllBytesAsync(diskFilePath);
        diskContent.Should().BeEquivalentTo(appendContent);
    }

    [Fact]
    public async Task Authenticated_access_requires_password()
    {
        // Arrange
        using var setup = new TestSetup();
        const string Password = "test-password-123";

        await setup.StartServerAsync(authPassword: Password, useFileStore: true);

        // Act & Assert - Request without authentication should fail
        var response = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/files/test.txt");
        response.StatusCode.Should().Be(HttpStatusCode.Unauthorized);

        // Request with wrong authentication should fail
        setup.HttpClient.DefaultRequestHeaders.Authorization =
            new System.Net.Http.Headers.AuthenticationHeaderValue("Basic",
                Convert.ToBase64String(Encoding.UTF8.GetBytes("user:wrong-password")));

        response = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/files/test.txt");
        response.StatusCode.Should().Be(HttpStatusCode.Unauthorized);

        // Request with correct authentication should succeed (even if file doesn't exist)
        setup.HttpClient.DefaultRequestHeaders.Authorization =
            new System.Net.Http.Headers.AuthenticationHeaderValue("Basic",
                Convert.ToBase64String(Encoding.UTF8.GetBytes($"user:{Password}")));

        response = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/files/test.txt");
        response.StatusCode.Should().Be(HttpStatusCode.NotFound); // File doesn't exist, but auth passed
    }

    [Fact]
    public async Task Authenticated_access_allows_file_operations_with_correct_password()
    {
        // Arrange
        using var setup = new TestSetup();
        const string Password = "secure-password-456";

        await setup.StartServerAsync(authPassword: Password, useFileStore: true);

        // Set up authentication
        setup.HttpClient.DefaultRequestHeaders.Authorization =
            new System.Net.Http.Headers.AuthenticationHeaderValue("Basic",
                Convert.ToBase64String(Encoding.UTF8.GetBytes($"testuser:{Password}")));

        var filePath = "authenticated/file.txt";
        var originalContent = "Authenticated content"u8.ToArray();
        var appendContent = " - appended"u8.ToArray();

        // Act & Assert - Create file
        using (var content = new ByteArrayContent(originalContent))
        {
            content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/octet-stream");
            var response = await setup.HttpClient.PutAsync($"http://localhost:{setup.Port}/files/{filePath}", content);
            response.EnsureSuccessStatusCode();
        }

        // Append to file
        using (var content = new ByteArrayContent(appendContent))
        {
            content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/octet-stream");
            var request = new HttpRequestMessage(HttpMethod.Post, $"http://localhost:{setup.Port}/files/{filePath}")
            {
                Content = content
            };
            request.Headers.Add("X-Operation", "append");

            var response = await setup.HttpClient.SendAsync(request);
            response.EnsureSuccessStatusCode();
        }

        // Verify via ListFiles
        var listResponse = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/dirs/authenticated");
        listResponse.EnsureSuccessStatusCode();
        var listContent = await listResponse.Content.ReadAsStringAsync();
        listContent.Should().Contain("file.txt");

        // Verify combined content
        var getResponse = await setup.HttpClient.GetAsync($"http://localhost:{setup.Port}/files/{filePath}");
        getResponse.EnsureSuccessStatusCode();
        var retrievedContent = await getResponse.Content.ReadAsByteArrayAsync();
        var expectedContent = originalContent.Concat(appendContent).ToArray();
        retrievedContent.Should().BeEquivalentTo(expectedContent);

        // Verify on disk
        var diskFilePath = Path.Combine(setup.TempDirectory, "authenticated", "file.txt");
        File.Exists(diskFilePath).Should().BeTrue();
        var diskContent = await File.ReadAllBytesAsync(diskFilePath);
        diskContent.Should().BeEquivalentTo(expectedContent);
    }
}
