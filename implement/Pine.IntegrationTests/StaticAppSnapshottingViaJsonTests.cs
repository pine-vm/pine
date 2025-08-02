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
    public async Task StaticAppSnapshottingViaJson_basic_functionality_test()
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

        // Create first instance of StaticAppSnapshottingViaJson
        var firstInstance = new StaticAppSnapshottingViaJson(
            webServiceAppSourceFiles: webServiceAppSourceFiles,
            fileStore: fileStore,
            logMessage: logMessage,
            cancellationToken: cancellationTokenSource.Token);

        // Check initial log messages
        logMessage("=== INITIAL LOGS ===");
        foreach (var msg in logMessages.ToArray())
        {
            logMessage("INITIAL: " + msg);
        }

        // Clear log messages for next phase
        logMessages.Clear();

        // Create the JSON request body like TestSetup does
        var addition = 5;
        var requestJson = System.Text.Json.JsonSerializer.Serialize(new { addition });
        logMessage("REQUEST JSON: " + requestJson);

        // Create minimal HTTP request using the same format as existing tests
        var context = new DefaultHttpContext();
        context.Request.Method = "POST";
        context.Request.Path = "/";
        context.Request.ContentType = "text/plain"; // Use text/plain like the existing tests
        
        var requestBytes = Encoding.UTF8.GetBytes(requestJson);
        context.Request.Body = new MemoryStream(requestBytes);
        context.Request.ContentLength = requestBytes.Length;
        context.Response.Body = new MemoryStream();

        logMessage("=== BEFORE HTTP REQUEST ===");
        
        // Test the HTTP request
        await firstInstance.HandleRequestAsync(context, logMessage);

        logMessage("=== AFTER HTTP REQUEST ===");

        // Check response
        context.Response.Body.Seek(0, SeekOrigin.Begin);
        using var reader = new StreamReader(context.Response.Body);
        var responseBody = reader.ReadToEnd();
        
        logMessage("RESPONSE BODY: " + responseBody);
        logMessage("RESPONSE STATUS: " + context.Response.StatusCode);

        // Wait for async operations
        await Task.Delay(2000);

        logMessage("=== FINAL LOGS ===");
        foreach (var msg in logMessages.ToArray())
        {
            logMessage("FINAL: " + msg);
        }

        // Basic assertion that something happened
        responseBody.Should().NotBeNullOrEmpty();
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