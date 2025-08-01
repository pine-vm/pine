using Microsoft.AspNetCore.Http;
using Pine.Elm.Platform;
using System;
using System.Linq;
using System.Threading.Tasks;

namespace ElmTime.Platform.WebService;

/// <summary>
/// A reusable component for handling HTTP requests that can be used independently of WebApplication creation.
/// Extracted from PublicAppState to enable flexible integration patterns.
/// </summary>
public class HttpRequestHandler
{
    private long _nextHttpRequestIndex = 0;

    private readonly Func<WebServiceInterface.HttpRequestEventStruct, Task<WebServiceInterface.HttpResponse>> _processHttpRequestAsync;
    private readonly Func<DateTimeOffset> _getDateTimeOffset;
    private readonly long? _httpRequestEventSizeLimit;

    public HttpRequestHandler(
        Func<WebServiceInterface.HttpRequestEventStruct, Task<WebServiceInterface.HttpResponse>> processHttpRequestAsync,
        Func<DateTimeOffset> getDateTimeOffset,
        long? httpRequestEventSizeLimit = null)
    {
        _processHttpRequestAsync = processHttpRequestAsync ?? throw new ArgumentNullException(nameof(processHttpRequestAsync));
        _getDateTimeOffset = getDateTimeOffset ?? throw new ArgumentNullException(nameof(getDateTimeOffset));
        _httpRequestEventSizeLimit = httpRequestEventSizeLimit;
    }

    /// <summary>
    /// Creates an HttpRequestHandler from a ServerAndElmAppConfig.
    /// </summary>
    public static HttpRequestHandler FromServerAndElmAppConfig(
        ServerAndElmAppConfig serverAndElmAppConfig,
        Func<DateTimeOffset> getDateTimeOffset)
    {
        return new HttpRequestHandler(
            serverAndElmAppConfig.ProcessHttpRequestAsync,
            getDateTimeOffset,
            serverAndElmAppConfig.ServerConfig?.httpRequestEventSizeLimit);
    }

    /// <summary>
    /// Handles an HTTP request from an ASP.NET Core HttpContext.
    /// This method contains the core request processing logic extracted from PublicAppState.
    /// </summary>
    public async Task HandleRequestAsync(HttpContext context)
    {
        var currentDateTime = _getDateTimeOffset();
        var timeMilli = currentDateTime.ToUnixTimeMilliseconds();
        var httpRequestIndex = System.Threading.Interlocked.Increment(ref _nextHttpRequestIndex);

        var httpRequestId = timeMilli + "-" + httpRequestIndex;

        var httpRequest =
            await Asp.AsInterfaceHttpRequestAsync(context.Request);

        if (_httpRequestEventSizeLimit is { } httpRequestEventSizeLimit)
        {
            if (httpRequestEventSizeLimit < EstimateHttpRequestEventSize(httpRequest))
            {
                context.Response.StatusCode = StatusCodes.Status413RequestEntityTooLarge;
                await context.Response.WriteAsync("Request is too large.");
                return;
            }
        }

        var httpRequestEvent =
            new WebServiceInterface.HttpRequestEventStruct(
                HttpRequestId: httpRequestId.ToString(),
                PosixTimeMilli: timeMilli,
                new WebServiceInterface.HttpRequestContext(
                    ClientAddress: context.Connection.RemoteIpAddress?.ToString()),
                Request: httpRequest);

        var task = _processHttpRequestAsync(httpRequestEvent);

        var waitForHttpResponseClock = System.Diagnostics.Stopwatch.StartNew();

        WebServiceInterface.HttpResponse? GetResponseFromAppOrTimeout(TimeSpan timeout)
        {
            if (task.IsCompleted)
            {
                return task.Result;
            }

            if (timeout <= waitForHttpResponseClock.Elapsed)
            {
                return
                    new WebServiceInterface.HttpResponse(
                        StatusCode: 500,
                        Body: System.Text.Encoding.UTF8.GetBytes(
                            "The app did not return an HTTP response within " +
                            (int)waitForHttpResponseClock.Elapsed.TotalSeconds +
                            " seconds."),
                        HeadersToAdd: []);
            }

            return null;
        }

        while (true)
        {
            var httpResponse =
                GetResponseFromAppOrTimeout(TimeSpan.FromMinutes(4));

            if (httpResponse is null)
            {
                await Task.Delay(TimeSpan.FromMilliseconds(30));
                continue;
            }

            var headerContentType =
                httpResponse.HeadersToAdd
                ?.FirstOrDefault(header => header.Name?.Equals("content-type", StringComparison.OrdinalIgnoreCase) ?? false)
                ?.Values?.FirstOrDefault();

            context.Response.StatusCode = httpResponse.StatusCode;

            foreach (var headerToAdd in httpResponse.HeadersToAdd ?? [])
            {
                context.Response.Headers[headerToAdd.Name] =
                    new Microsoft.Extensions.Primitives.StringValues([.. headerToAdd.Values]);
            }

            if (headerContentType is not null)
                context.Response.ContentType = headerContentType;

            context.Response.Headers.XPoweredBy = "Pine";

            var contentAsByteArray = httpResponse.Body;

            context.Response.ContentLength = contentAsByteArray?.Length ?? 0;

            if (contentAsByteArray is not null)
                await context.Response.Body.WriteAsync(contentAsByteArray.Value);

            break;
        }
    }

    /// <summary>
    /// Estimates the size of an HTTP request event for size limit checks.
    /// </summary>
    public static long EstimateHttpRequestEventSize(
        WebServiceInterface.HttpRequestProperties httpRequest)
    {
        var headersSize =
            httpRequest.Headers
            .Select(header => header.Name.Length + header.Values.Sum(value => value.Length))
            .Sum();

        var bodySize = httpRequest.Body?.Length ?? 0;

        return
            httpRequest.Method.Length +
            httpRequest.Uri.Length +
            headersSize +
            bodySize;
    }
}
