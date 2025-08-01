using ElmTime.Platform.WebService;
using AwesomeAssertions;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Pine.Elm.Platform;
using Pine.Core;
using System;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

public class HttpRequestHandlerTests
{
    [Fact]
    public async Task HttpRequestHandler_can_be_used_independently_with_minimal_API()
    {
        var requestCount = 0;
        var receivedRequests = new System.Collections.Concurrent.ConcurrentQueue<WebServiceInterface.HttpRequestEventStruct>();

        // Create a mock process function
        async Task<WebServiceInterface.HttpResponse> mockProcessRequest(WebServiceInterface.HttpRequestEventStruct requestEvent)
        {
            Interlocked.Increment(ref requestCount);
            receivedRequests.Enqueue(requestEvent);

            // Simple echo response
            var responseBody = Encoding.UTF8.GetBytes($"Echo: {requestEvent.Request.Method} {requestEvent.Request.Uri}");

            return await Task.FromResult(new WebServiceInterface.HttpResponse(
                StatusCode: 200,
                Body: responseBody,
                HeadersToAdd: [
                    new WebServiceInterface.HttpHeader("Content-Type", ["text/plain"])
                ]));
        }

        // Create HttpRequestHandler
        var getDateTimeOffset = () => DateTimeOffset.UtcNow;
        var httpRequestHandler = new HttpRequestHandler(
            mockProcessRequest,
            getDateTimeOffset,
            httpRequestEventSizeLimit: 1_000_000);

        // Build minimal WebApplication manually
        var builder = WebApplication.CreateBuilder();
        builder.Services.AddLogging(logging =>
        {
            logging.AddConsole();
            logging.SetMinimumLevel(LogLevel.Warning); // Reduce test noise
        });

        // Register the DateTimeOffset service that Asp middleware expects
        builder.Services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset);

        // Configure rate limiting services like the original PublicAppState
        Asp.ConfigureServices(builder.Services);

        var app = builder.Build();

        // Wire up the HttpRequestHandler using minimal API
        app.Run(async context =>
        {
            await Asp.MiddlewareFromWebServiceConfig(
                serverConfig: null, // No server config for this test
                context,
                async () => await httpRequestHandler.HandleRequestAsync(context));
        });

        // Start the app on a specific port to avoid conflicts
        var testPort = 5000 + new Random().Next(1000, 9999);
        app.Urls.Add($"http://localhost:{testPort}");
        await app.StartAsync();

        try
        {
            var baseUrl = $"http://localhost:{testPort}";
            using var httpClient = new HttpClient();

            // Test 1: Simple GET request
            var getResponse = await httpClient.GetAsync($"{baseUrl}/test");
            var getContent = await getResponse.Content.ReadAsStringAsync();

            getResponse.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            getContent.Should().Contain("Echo: GET");
            getContent.Should().Contain("/test");

            // Test 2: POST request with body
            var postContent = new StringContent("test body", Encoding.UTF8, "text/plain");
            var postResponse = await httpClient.PostAsync($"{baseUrl}/api/test", postContent);
            var postResponseContent = await postResponse.Content.ReadAsStringAsync();

            postResponse.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            postResponseContent.Should().Contain("Echo: POST");
            postResponseContent.Should().Contain("/api/test");

            // Verify that both requests were processed
            requestCount.Should().Be(2);
            receivedRequests.Should().HaveCount(2);

            // Verify request details
            receivedRequests.TryDequeue(out var firstRequest).Should().BeTrue();
            firstRequest.Should().NotBeNull();
            firstRequest!.Request.Method.Should().Be("GET");
            firstRequest.Request.Uri.Should().Contain("/test");

            receivedRequests.TryDequeue(out var secondRequest).Should().BeTrue();
            secondRequest.Should().NotBeNull();
            secondRequest!.Request.Method.Should().Be("POST");
            secondRequest.Request.Uri.Should().Contain("/api/test");
            secondRequest.Request.Body.Should().NotBeNull();
        }
        finally
        {
            await app.StopAsync();
        }
    }

    [Fact]
    public async Task HttpRequestHandler_respects_size_limits()
    {
        var requestProcessed = false;

        // Create a mock process function
        async Task<WebServiceInterface.HttpResponse> mockProcessRequest(WebServiceInterface.HttpRequestEventStruct requestEvent)
        {
            requestProcessed = true;
            return await Task.FromResult(new WebServiceInterface.HttpResponse(
                StatusCode: 200,
                Body: Encoding.UTF8.GetBytes("OK"),
                HeadersToAdd: new WebServiceInterface.HttpHeader[0]));
        }

        // Create HttpRequestHandler with very small size limit
        var getDateTimeOffset = () => DateTimeOffset.UtcNow;
        var httpRequestHandler = new HttpRequestHandler(
            mockProcessRequest,
            getDateTimeOffset,
            httpRequestEventSizeLimit: 50); // Very small limit

        var builder = WebApplication.CreateBuilder();
        builder.Services.AddLogging(logging => logging.SetMinimumLevel(LogLevel.Warning));
        builder.Services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset);
        Asp.ConfigureServices(builder.Services);

        var app = builder.Build();

        app.Run(async context =>
        {
            await httpRequestHandler.HandleRequestAsync(context);
        });

        var testPort = 5000 + new Random().Next(1000, 9999);
        app.Urls.Add($"http://localhost:{testPort}");
        await app.StartAsync();

        try
        {
            var baseUrl = $"http://localhost:{testPort}";
            using var httpClient = new HttpClient();

            // Send a request that exceeds the size limit
            var largeContent = new StringContent(new string('x', 1000), Encoding.UTF8, "text/plain");
            var response = await httpClient.PostAsync($"{baseUrl}/test", largeContent);

            // Should get 413 Request Entity Too Large
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.RequestEntityTooLarge);

            var content = await response.Content.ReadAsStringAsync();
            content.Should().Be("Request is too large.");

            // The mock process function should not have been called
            requestProcessed.Should().BeFalse();
        }
        finally
        {
            await app.StopAsync();
        }
    }

    [Fact]
    public void HttpRequestHandler_FromServerAndElmAppConfig_creates_instance_correctly()
    {
        var mockProcessRequest = new Func<WebServiceInterface.HttpRequestEventStruct, Task<WebServiceInterface.HttpResponse>>(
            async (request) => await Task.FromResult(new WebServiceInterface.HttpResponse(200, null, new WebServiceInterface.HttpHeader[0])));

        var serverConfig = new WebServiceConfigJson
        {
            httpRequestEventSizeLimit = 500_000
        };

        var serverAndElmAppConfig = new ServerAndElmAppConfig(
            ServerConfig: serverConfig,
            ProcessHttpRequestAsync: mockProcessRequest,
            SourceComposition: PineValue.EmptyList,
            InitOrMigrateCmds: null,
            DisableLetsEncrypt: true,
            DisableHttps: true);

        var getDateTimeOffset = () => DateTimeOffset.UtcNow;

        var handler = HttpRequestHandler.FromServerAndElmAppConfig(serverAndElmAppConfig, getDateTimeOffset);

        handler.Should().NotBeNull();
    }

    [Fact]
    public void EstimateHttpRequestEventSize_calculates_size_correctly()
    {
        var httpRequest = new WebServiceInterface.HttpRequestProperties(
            Method: "POST",
            Uri: "https://example.com/api/test",
            Body: Encoding.UTF8.GetBytes("test body"),
            Headers: [
                new WebServiceInterface.HttpHeader("Content-Type", ["application/json"]),
                new WebServiceInterface.HttpHeader("Authorization", ["Bearer", "token123"])
            ]);

        var estimatedSize = HttpRequestHandler.EstimateHttpRequestEventSize(httpRequest);

        // Should include method + uri + headers + body
        var expectedSize =
            "POST".Length +
            "https://example.com/api/test".Length +
            "Content-Type".Length + "application/json".Length +
            "Authorization".Length + "Bearer".Length + "token123".Length +
            "test body".Length;

        estimatedSize.Should().Be(expectedSize);
    }
}
