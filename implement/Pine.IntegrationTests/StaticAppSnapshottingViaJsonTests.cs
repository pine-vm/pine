using AwesomeAssertions;
using Microsoft.AspNetCore.Http;
using Pine.Core;
using Pine.Platform.WebService;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class StaticAppSnapshottingViaJsonTests
{
    [Fact]
    public async Task StaticAppSnapshottingViaJson_debug_snapshot_saving()
    {
        // Set up file store in memory using RecordingFileStoreWriter
        var recordingFileStoreWriter = new RecordingFileStoreWriter();
        var fileStore = new FileStoreFromWriterAndReader(
            writer: recordingFileStoreWriter,
            reader: recordingFileStoreWriter.ReaderFromAppliedOperationsOnEmptyStore());

        // Load demo app CounterWebApp
        var counterWebAppSourceFiles = TestSetup.CounterElmWebApp;
        var webServiceAppSourceFiles = PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(counterWebAppSourceFiles);

        var logMessages = new ConcurrentQueue<string>();
        Action<string> logMessage = message => 
        {
            logMessages.Enqueue(message);
            Console.WriteLine("[TEST LOG] " + message);
        };

        using var cancellationTokenSource = new CancellationTokenSource();

        // Create instance
        var instance = new StaticAppSnapshottingViaJson(
            webServiceAppSourceFiles: webServiceAppSourceFiles,
            fileStore: fileStore,
            logMessage: logMessage,
            cancellationToken: cancellationTokenSource.Token);

        logMessage("=== Created instance, checking initial state ===");

        // Check what's in the file store initially
        var initialFiles = fileStore.ListFilesInDirectory([]);
        logMessage("Initial files in store: " + string.Join(", ", initialFiles.Select(f => string.Join("/", f))));

        logMessage("=== Making HTTP request ===");

        // Create HTTP request  
        var context = new DefaultHttpContext();
        context.Request.Method = "POST";
        context.Request.Path = "/";
        context.Request.ContentType = "text/plain";
        
        var requestJson = System.Text.Json.JsonSerializer.Serialize(new { addition = 5 });
        var requestBytes = Encoding.UTF8.GetBytes(requestJson);
        context.Request.Body = new MemoryStream(requestBytes);
        context.Request.ContentLength = requestBytes.Length;
        context.Response.Body = new MemoryStream();

        // Custom log action that logs everything
        Action<string> verboseLogMessage = message => 
        {
            logMessages.Enqueue("HTTP_LOG: " + message);
            Console.WriteLine("[HTTP LOG] " + message);
        };

        // Make the request
        var handleTask = instance.HandleRequestAsync(context, verboseLogMessage);
        
        logMessage("HandleRequestAsync returned, waiting for completion...");
        
        await handleTask;
        
        logMessage("HandleRequestAsync completed");

        // Check response
        context.Response.Body.Seek(0, SeekOrigin.Begin);
        using var reader = new StreamReader(context.Response.Body);
        var responseBody = reader.ReadToEnd();
        
        logMessage("Response: " + responseBody + " (Status: " + context.Response.StatusCode + ")");

        // Wait longer for any background tasks
        logMessage("Waiting for background operations...");
        await Task.Delay(5000);

        // Check file store after request
        var filesAfterRequest = fileStore.ListFilesInDirectory([]);
        logMessage("Files after request: " + string.Join(", ", filesAfterRequest.Select(f => string.Join("/", f))));

        // Check if snapshot file was created
        var snapshotPath = ImmutableList.Create("app-state-snapshot.json");
        var snapshotContent = fileStore.GetFileContent(snapshotPath);
        if (snapshotContent.HasValue)
        {
            logMessage("Snapshot file found! Content: " + Encoding.UTF8.GetString(snapshotContent.Value.Span));
        }
        else
        {
            logMessage("No snapshot file found");
        }

        logMessage("=== All logs ===");
        foreach (var msg in logMessages.ToArray())
        {
            logMessage("FINAL: " + msg);
        }

        // Basic assertions
        responseBody.Should().Be("5");
        
        // The main assertion - snapshot should have been saved
        snapshotContent.Should().NotBeNull("Snapshot file should have been created after HTTP request");
    }

    private static HttpContext CreateHttpContextForCounterRequest(int addition)
    {
        var context = new DefaultHttpContext();
        
        // Set up the request
        context.Request.Method = "POST";
        context.Request.Path = "/";
        context.Request.ContentType = "text/plain"; // Use text/plain like existing tests
        
        // Create the JSON request body for the counter
        var requestJson = System.Text.Json.JsonSerializer.Serialize(new { addition });
        var requestBytes = Encoding.UTF8.GetBytes(requestJson);
        context.Request.Body = new MemoryStream(requestBytes);
        context.Request.ContentLength = requestBytes.Length;

        // Set up the response
        context.Response.Body = new MemoryStream();

        return context;
    }

    private static string GetResponseBodyAsString(HttpContext context)
    {
        context.Response.Body.Seek(0, SeekOrigin.Begin);
        using var reader = new StreamReader(context.Response.Body);
        return reader.ReadToEnd();
    }
}