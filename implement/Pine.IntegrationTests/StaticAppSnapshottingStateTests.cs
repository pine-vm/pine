using AwesomeAssertions;
using ElmTime.Platform.WebService;
using Microsoft.AspNetCore.Http;
using Pine.Core.Files;
using Pine.Core.IO;
using Pine.Elm.Platform;
using Pine.Platform.WebService;
using System.Collections.Concurrent;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class StaticAppSnapshottingStateTests
{
    [Fact]
    public async Task StaticAppSnapshottingState_integration_test()
    {
        var fileStoreDict = new FileStoreFromConcurrentDictionary();

        var fileStore =
            new FileStoreFromWriterAndReader(fileStoreDict, fileStoreDict);

        var webServiceCompiled =
            WebServiceInterface.CompiledModulesFromSourceFilesAndEntryFileName(
                FileTree.FromSetOfFilesWithStringPath(TestSetup.CounterElmWebApp),
                entryFileName: ["src", "Backend", "Main.elm"]);

        var logMessages = new ConcurrentQueue<string>();

        void LogMessage(string message) => logMessages.Enqueue(message);

        using var cancellationTokenSource = new CancellationTokenSource();

        // Phase 1: Create first instance and establish state
        {
            // Create first instance of app
            await using var appInstance =
                StaticAppSnapshottingState.Create(
                    webServiceCompiledModules: webServiceCompiled,
                    serverConfig: null,
                    fileStore: fileStore,
                    logMessage: LogMessage,
                    cancellationToken: cancellationTokenSource.Token);

            // Verify initial state - should indicate no snapshot file not found
            logMessages.Should().ContainMatch("*snapshot file not found*");
            logMessages.Should().ContainMatch("*initializing from default*");

            logMessages.Clear();

            // Test the async fix by making a request
            var context = new DefaultHttpContext();
            context.Request.Method = "POST";
            context.Request.Path = "/";
            context.Request.ContentType = "text/plain";

            var requestJson = System.Text.Json.JsonSerializer.Serialize(new { addition = 5 });
            var requestBytes = Encoding.UTF8.GetBytes(requestJson);

            context.Request.Body = new MemoryStream(requestBytes);
            context.Request.ContentLength = requestBytes.Length;
            context.Response.Body = new MemoryStream();

            await appInstance.HandleRequestAsync(context, LogMessage);

            // Verify that the async continuation completed
            logMessages.Should().NotBeEmpty("HandleRequestAsync should have logged something from the async continuation");

            logMessages.Should().ContainMatch("*App state snapshot updated*");

            // Verify HTTP response was processed
            context.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(context.Response.Body);
            var responseBody = reader.ReadToEnd();

            context.Response.StatusCode.Should().Be(200);
            responseBody.Should().Be("5", "Counter app should return the added value");
        }

        // Phase 2: Setup new instance using same store and verify restoration
        {
            logMessages.Clear();

            await using var appInstance =
                StaticAppSnapshottingState.Create(
                    webServiceCompiledModules: webServiceCompiled,
                    serverConfig: null,
                    fileStore: fileStore,
                    logMessage: LogMessage,
                    cancellationToken: cancellationTokenSource.Token);

            // Assert log entries indicating restoration of app state
            logMessages.Should().ContainMatch("*App state snapshot file found*");

            logMessages.Clear();

            // Simulate HTTP requests and assert responses according to restored state (starting with adding zero)
            var context = new DefaultHttpContext();
            context.Request.Method = "POST";
            context.Request.Path = "/";
            context.Request.ContentType = "text/plain";

            var requestJson = System.Text.Json.JsonSerializer.Serialize(new { addition = 0 });
            var requestBytes = Encoding.UTF8.GetBytes(requestJson);

            context.Request.Body = new MemoryStream(requestBytes);
            context.Request.ContentLength = requestBytes.Length;
            context.Response.Body = new MemoryStream();

            await appInstance.HandleRequestAsync(context, LogMessage);

            // Verify HTTP response shows restored state (should be 5 + 0 = 5)
            context.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(context.Response.Body);
            var responseBody = reader.ReadToEnd();

            context.Response.StatusCode.Should().Be(200);
            responseBody.Should().Be("5", "Counter app should return the previous state value (5) plus zero");
        }

        // Phase 3: Corrupt file store and test failed restoration
        {
            foreach (var filePath in fileStoreDict.ListFiles())
            {
                fileStoreDict.SetFileContent(filePath, "invalid-content"u8.ToArray());
            }

            logMessages.Clear();

            await using var appInstance =
                StaticAppSnapshottingState.Create(
                    webServiceCompiledModules: webServiceCompiled,
                    serverConfig: null,
                    fileStore: fileStore,
                    logMessage: LogMessage,
                    cancellationToken: cancellationTokenSource.Token);

            logMessages.Should().ContainMatch("*App state snapshot file found*");

            var allLogs = string.Join(", ", logMessages);
            (allLogs.Contains("Failed to deserialize app state snapshot") || allLogs.Contains("Failed to set app state from snapshot"))
                .Should().BeTrue("Expected either deserialization failure or state setting failure");

            logMessages.Clear();

            var context = new DefaultHttpContext();
            context.Request.Method = "POST";
            context.Request.Path = "/";
            context.Request.ContentType = "text/plain";

            var requestJson = System.Text.Json.JsonSerializer.Serialize(new { addition = 3 });
            var requestBytes = Encoding.UTF8.GetBytes(requestJson);

            context.Request.Body = new MemoryStream(requestBytes);
            context.Request.ContentLength = requestBytes.Length;
            context.Response.Body = new MemoryStream();

            await appInstance.HandleRequestAsync(context, LogMessage);

            context.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(context.Response.Body);
            var responseBody = reader.ReadToEnd();

            context.Response.StatusCode.Should().Be(200);
            responseBody.Should().Be("3", "Counter app should start fresh from initial state after failed restoration");
        }
    }

    [Fact]
    public async Task HttpRequestSizeLimit_integration_test()
    {
        var fileStoreDict = new FileStoreFromConcurrentDictionary();
        var fileStore = new FileStoreFromWriterAndReader(fileStoreDict, fileStoreDict);

        var serverConfig = new WebServiceConfigJson(httpRequestEventSizeLimit: 10_000);

        var webServiceCompiled =
            WebServiceInterface.CompiledModulesFromSourceFilesAndEntryFileName(
                FileTree.FromSetOfFilesWithStringPath(TestSetup.CounterElmWebApp),
                entryFileName: ["src", "Backend", "Main.elm"]);

        var logMessages = new ConcurrentQueue<string>();
        void LogMessage(string message) => logMessages.Enqueue(message);

        using var cancellationTokenSource = new CancellationTokenSource();

        await using var appInstance =
            StaticAppSnapshottingState.Create(
                webServiceCompiledModules: webServiceCompiled,
                serverConfig: serverConfig,
                fileStore: fileStore,
                logMessage: LogMessage,
                cancellationToken: cancellationTokenSource.Token);

        // First request: Small request (should succeed)
        {
            var context = new DefaultHttpContext();
            context.Request.Method = "POST";
            context.Request.Path = "/";
            context.Request.ContentType = "text/plain";

            var requestJson =
                System.Text.Json.JsonSerializer.Serialize(
                    new
                    {
                        addition = 1,
                        other = new string('x', 1_000)
                    });

            var requestBytes = Encoding.UTF8.GetBytes(requestJson);

            context.Request.Body = new MemoryStream(requestBytes);
            context.Request.ContentLength = requestBytes.Length;
            context.Response.Body = new MemoryStream();

            await appInstance.HandleRequestAsync(context, LogMessage);

            context.Response.StatusCode.Should().Be(200, "Small HTTP request should succeed");

            context.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(context.Response.Body);
            var responseBody = reader.ReadToEnd();
            responseBody.Should().Be("1", "Counter app should return the added value");
        }

        // Second request: Large request (should be rejected)
        {
            var context = new DefaultHttpContext();
            context.Request.Method = "POST";
            context.Request.Path = "/";
            context.Request.ContentType = "text/plain";

            var largeContent = new string('x', 10_000);

            var requestJson =
                System.Text.Json.JsonSerializer.Serialize(
                    new
                    {
                        addition = 2,
                        other = largeContent
                    });

            var requestBytes = Encoding.UTF8.GetBytes(requestJson);

            context.Request.Body = new MemoryStream(requestBytes);
            context.Request.ContentLength = requestBytes.Length;
            context.Response.Body = new MemoryStream();

            await appInstance.HandleRequestAsync(context, LogMessage);

            context.Response.StatusCode.Should().Be(413, "Large HTTP request should be rejected with 413 Request Entity Too Large");

            context.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(context.Response.Body);
            var responseBody = reader.ReadToEnd();
            responseBody.Should().Be("Request is too large.", "Large request should return expected error message");
        }

        // Third request: Small request again (should succeed)
        {
            var context = new DefaultHttpContext();
            context.Request.Method = "POST";
            context.Request.Path = "/";
            context.Request.ContentType = "text/plain";

            var requestJson = System.Text.Json.JsonSerializer.Serialize(new { addition = 3 });
            var requestBytes = Encoding.UTF8.GetBytes(requestJson);

            context.Request.Body = new MemoryStream(requestBytes);
            context.Request.ContentLength = requestBytes.Length;
            context.Response.Body = new MemoryStream();

            await appInstance.HandleRequestAsync(context, LogMessage);

            context.Response.StatusCode.Should().Be(200, "Small HTTP request should succeed after large request rejection");

            context.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(context.Response.Body);
            var responseBody = reader.ReadToEnd();
            responseBody.Should().Be("4", "Counter app should return 1 + 3 = 4 (large request was rejected so counter state is preserved)");
        }
    }

    [Fact]
    public async Task Configure_Http_headers_integration_test()
    {
        var fileStoreDict = new FileStoreFromConcurrentDictionary();
        var fileStore = new FileStoreFromWriterAndReader(fileStoreDict, fileStoreDict);

        var webServiceCompiled =
            WebServiceInterface.CompiledModulesFromSourceFilesAndEntryFileName(
                FileTree.FromSetOfFilesWithStringPath(TestSetup.CrossPropagateHttpHeadersToAndFromBodyElmWebApp),
                entryFileName: ["src", "Backend", "Main.elm"]);

        var logMessages = new ConcurrentQueue<string>();
        void LogMessage(string message) => logMessages.Enqueue(message);

        using var cancellationTokenSource = new CancellationTokenSource();

        await using var appInstance =
            StaticAppSnapshottingState.Create(
                webServiceCompiledModules: webServiceCompiled,
                serverConfig: null,
                fileStore: fileStore,
                logMessage: LogMessage,
                cancellationToken: cancellationTokenSource.Token);

        var context = new DefaultHttpContext();
        context.Request.Method = "POST";
        context.Request.Path = "/";
        context.Request.ContentType = "text/plain";

        // Start with a header we plan to remove
        context.Request.Headers["X-Remove-Me"] = "bye";

        var requestBody = Encoding.UTF8.GetBytes("hello-world");
        context.Request.Body = new MemoryStream(requestBody);
        context.Request.ContentLength = requestBody.Length;
        context.Response.Body = new MemoryStream();

        // Prepare incoming request headers: add one, remove one
        WebServiceInterface.HttpRequestProperties PrepareRequest(WebServiceInterface.HttpRequestProperties req)
        {
            var headers = req.Headers
                .Where(h => !h.Name.Equals("X-Remove-Me", System.StringComparison.OrdinalIgnoreCase))
                .ToList();

            headers.Add(new WebServiceInterface.HttpHeader("X-Added-Request", ["added-value"]));

            return req with { Headers = headers };
        }

        // Prepare outgoing response headers: add one, remove one set by the app
        WebServiceInterface.HttpResponse FinalizeResponse(WebServiceInterface.HttpResponse resp)
        {
            var contentType =
                resp.HeadersToAdd.FirstOrDefault(h => h.Name.Equals("content-type", System.StringComparison.OrdinalIgnoreCase));

            var others =
                resp.HeadersToAdd
                .Where(h =>
                !h.Name.Equals("response-header-name", System.StringComparison.OrdinalIgnoreCase) &&
                !h.Name.Equals("content-type", System.StringComparison.OrdinalIgnoreCase))
                .ToList();

            // Add our custom header before content-type so it is applied
            others.Add(new WebServiceInterface.HttpHeader("X-Added-Response", ["42"]));

            if (contentType is not null)
            {
                others.Add(contentType);
            }

            return resp with { HeadersToAdd = others };
        }

        await appInstance.HandleRequestAsync(context, LogMessage, prepareRequest: PrepareRequest, finalizeResponse: FinalizeResponse);

        // Validate status
        context.Response.StatusCode.Should().Be(200);

        // Validate response headers reflect finalizeResponse configuration
        context.Response.Headers.ContainsKey("response-header-name").Should().BeFalse("finalizeResponse should remove this header");
        context.Response.Headers["X-Added-Response"].ToString().Should().Be("42", "finalizeResponse should add this header");

        // Validate the app saw configured request headers via JSON body echo
        context.Response.Body.Seek(0, SeekOrigin.Begin);

        using var reader = new StreamReader(context.Response.Body);
        var responseBody = reader.ReadToEnd();

        using var json = System.Text.Json.JsonDocument.Parse(responseBody);

        var headersArray = json.RootElement;
        headersArray.ValueKind.Should().Be(System.Text.Json.JsonValueKind.Array);

        bool HasHeader(string name) =>
            headersArray.EnumerateArray()
                .Any(h => h.TryGetProperty("name", out var n) && n.GetString()?.Equals(name, System.StringComparison.OrdinalIgnoreCase) == true);

        string[]? GetHeaderValues(string name) =>
            [.. headersArray
                .EnumerateArray()
                .Where(h => h.TryGetProperty("name", out var n) && n.GetString()?.Equals(name, System.StringComparison.OrdinalIgnoreCase) == true)
                .SelectMany(h => h.GetProperty("values").EnumerateArray().Select(v => v.GetString() ?? string.Empty))];

        HasHeader("X-Added-Request").Should().BeTrue("prepareRequest should add header visible to app");

        var addedValues = GetHeaderValues("X-Added-Request");

        addedValues.Should().NotBeNull();
        addedValues!.Should().Contain("added-value");

        HasHeader("X-Remove-Me").Should().BeFalse("prepareRequest should remove header before app");

        // Content-Type from app should still be present
        context.Response.ContentType.Should().Be("application/json");
    }

    [Fact]
    public async Task StateChanged_indicator_integration_test()
    {
        var fileStoreDict = new FileStoreFromConcurrentDictionary();
        var fileStore = new FileStoreFromWriterAndReader(fileStoreDict, fileStoreDict);

        var webServiceCompiled =
            WebServiceInterface.CompiledModulesFromSourceFilesAndEntryFileName(
                FileTree.FromSetOfFilesWithStringPath(TestSetup.CounterElmWebApp),
                entryFileName: ["src", "Backend", "Main.elm"]);

        using var cancellationTokenSource = new CancellationTokenSource();

        await using var appInstance =
            StaticAppSnapshottingState.Create(
                webServiceCompiledModules: webServiceCompiled,
                serverConfig: null,
                fileStore: fileStore,
                logMessage: _ => { },
                cancellationToken: cancellationTokenSource.Token);

        async Task<(StaticAppSnapshottingState.HandleRequestReport report, string responseText, int status)> Send(int addition)
        {
            var ctx = new DefaultHttpContext();
            ctx.Request.Method = "POST";
            ctx.Request.Path = "/";
            ctx.Request.ContentType = "text/plain";

            var bodyJson = System.Text.Json.JsonSerializer.Serialize(new { addition });
            var requestBytes = Encoding.UTF8.GetBytes(bodyJson);
            ctx.Request.Body = new MemoryStream(requestBytes);
            ctx.Request.ContentLength = requestBytes.Length;
            ctx.Response.Body = new MemoryStream();

            var report = await appInstance.HandleRequestAsync(ctx, logMessage: null);

            ctx.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(ctx.Response.Body);
            var responseBody = reader.ReadToEnd();

            return (report, responseBody, ctx.Response.StatusCode);
        }

        // Start with a mutation
        var r1 = await Send(2);
        r1.status.Should().Be(200);
        r1.responseText.Should().Be("2");
        r1.report.StateChanged.Should().BeTrue();

        // No-op (add 0) should NOT change state
        var r2 = await Send(0);
        r2.status.Should().Be(200);
        r2.responseText.Should().Be("2");
        r2.report.StateChanged.Should().BeFalse();

        // Mutation again
        var r3 = await Send(3);
        r3.status.Should().Be(200);
        r3.responseText.Should().Be("5");
        r3.report.StateChanged.Should().BeTrue();

        // No-op again
        var r4 = await Send(0);
        r4.status.Should().Be(200);
        r4.responseText.Should().Be("5");
        r4.report.StateChanged.Should().BeFalse();

        // Verify RequestEvent reflects the inserted request (basic checks)
        r3.report.RequestEvent.Request.Method.Should().Be("POST");
        r3.report.RequestEvent.Request.Body.Should().NotBeNull();
    }
}
