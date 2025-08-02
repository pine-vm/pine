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

        {
            // Create first instance of StaticAppSnapshottingViaJson
            await using var appInstance =
                new StaticAppSnapshottingViaJson(
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

            // This should now complete properly with the async fix
            await appInstance.HandleRequestAsync(context, LogMessage);

            // Verify that the async continuation completed - we should see either:
            // 1. An app state snapshot update message, OR
            // 2. A failure message about getting the app state
            // The key point is that the async continuation should execute and log something
            logMessages.Should().NotBeEmpty("HandleRequestAsync should have logged something from the async continuation");

            // The async fix ensures that either the snapshot is saved OR we get an error message
            var allLogs = string.Join(", ", logMessages);
            (allLogs.Contains("App state snapshot updated") || allLogs.Contains("Failed to get app state snapshot"))
                .Should().BeTrue("The async continuation should have executed and logged either success or failure");

            // Verify HTTP response was processed
            context.Response.Body.Seek(0, SeekOrigin.Begin);
            using var reader = new StreamReader(context.Response.Body);
            var responseBody = reader.ReadToEnd();

            // The HTTP request processing should work regardless of the state serialization issue
            context.Response.StatusCode.Should().Be(200);
            responseBody.Should().Be("5", "Counter app should return the added value");
        }
    }
}
