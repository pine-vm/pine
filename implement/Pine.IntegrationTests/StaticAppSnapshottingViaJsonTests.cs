using AwesomeAssertions;
using Microsoft.AspNetCore.Http;
using Pine.Core;
using Pine.Platform.WebService;
using System.Collections.Concurrent;
using System.IO;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class StaticAppSnapshottingViaJsonTests
{
    [Fact]
    public async Task StaticAppSnapshottingViaJson_integration_test()
    {
        var fileStoreDict = new FileStoreFromConcurrentDictionary();

        var fileStore =
            new FileStoreFromWriterAndReader(fileStoreDict, fileStoreDict);

        var webServiceAppSourceFiles =
            PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(TestSetup.CounterElmWebApp);

        var logMessages = new ConcurrentQueue<string>();

        void LogMessage(string message) => logMessages.Enqueue(message);

        using var cancellationTokenSource = new CancellationTokenSource();

        // Phase 1: Create first instance and establish state
        {
            // Create first instance of StaticAppSnapshottingViaJson
            await using var appInstance =
                StaticAppSnapshottingState.Create(
                    webServiceAppSourceFiles: webServiceAppSourceFiles,
                    fileStore: fileStore,
                    logMessage: LogMessage,
                    cancellationToken: cancellationTokenSource.Token);

            // Verify initial state - should indicate no snapshot file found
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

            // Verify that the async continuation completed - we should see either:
            // 1. An app state snapshot update message, OR
            // 2. A failure message about getting the app state
            // The key point is that the async continuation should execute and log something
            logMessages.Should().NotBeEmpty("HandleRequestAsync should have logged something from the async continuation");

            // The async fix ensures that either the snapshot is saved OR we get an error message
            var allLogs = string.Join(", ", logMessages);

            logMessages.Should().ContainMatch("*App state snapshot updated*");

            // Verify HTTP response was processed
            context.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(context.Response.Body);
            var responseBody = reader.ReadToEnd();

            // The HTTP request processing should work regardless of the state serialization issue
            context.Response.StatusCode.Should().Be(200);
            responseBody.Should().Be("5", "Counter app should return the added value");
        }

        // Phase 2: Setup new instance using same store and verify restoration
        {
            logMessages.Clear();

            await using var appInstance =
                StaticAppSnapshottingState.Create(
                    webServiceAppSourceFiles: webServiceAppSourceFiles,
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
            // Corrupt file store content so that restore should fail

            foreach (var filePath in fileStoreDict.ListFiles())
            {
                fileStoreDict.SetFileContent(filePath, "invalid-content"u8.ToArray());
            }

            logMessages.Clear();

            // Setup new instance using same store
            await using var appInstance =
                StaticAppSnapshottingState.Create(
                    webServiceAppSourceFiles: webServiceAppSourceFiles,
                    fileStore: fileStore,
                    logMessage: LogMessage,
                    cancellationToken: cancellationTokenSource.Token);

            // Assert log entries indicating how restoration failed
            logMessages.Should().ContainMatch("*App state snapshot file found*");

            var allLogs = string.Join(", ", logMessages);
            (allLogs.Contains("Failed to deserialize app state snapshot") || allLogs.Contains("Failed to set app state from snapshot"))
                .Should().BeTrue("Expected either deserialization failure or state setting failure");

            logMessages.Clear();

            // Simulate HTTP requests to verify normal operation, based on new initial app state
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

            // Verify HTTP response shows new initial state (should be 0 + 3 = 3)
            context.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(context.Response.Body);
            var responseBody = reader.ReadToEnd();

            context.Response.StatusCode.Should().Be(200);
            responseBody.Should().Be("3", "Counter app should start fresh from initial state after failed restoration");
        }
    }
}
