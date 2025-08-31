using Microsoft.AspNetCore.Http;
using System;
using System.Text;
using System.Threading.Tasks;

namespace Pine.Core.Http;

/// <summary>
/// Configuration for Basic HTTP authentication.
/// </summary>
public record BasicAuthenticationConfig(string ExpectedPassword, string Realm = "Protected Area");

/// <summary>
/// Middleware that provides Basic HTTP authentication for all requests.
/// </summary>
public class BasicAuthenticationMiddleware(RequestDelegate next, BasicAuthenticationConfig config)
{
    private readonly RequestDelegate _next =
        next ?? throw new ArgumentNullException(nameof(next));

    private readonly string _expectedPassword =
        config?.ExpectedPassword ?? throw new ArgumentNullException(nameof(config));

    private readonly string _realm =
        config?.Realm ?? throw new ArgumentNullException(nameof(config));

    /// <summary>
    /// Handles the HTTP request and enforces Basic authentication.
    /// </summary>
    /// <param name="context">The HTTP context for the current request.</param>
    /// <returns>A task that represents the completion of request processing.</returns>
    public async Task InvokeAsync(HttpContext context)
    {
        if (!IsAuthorized(context))
        {
            context.Response.StatusCode = 401;
            context.Response.Headers.Append("WWW-Authenticate", $"Basic realm=\"{_realm}\"");
            await context.Response.WriteAsync("Unauthorized");
            return;
        }

        await _next(context);
    }

    private bool IsAuthorized(HttpContext context)
    {
        var authorizationHeader = context.Request.Headers["Authorization"].ToString();

        if (string.IsNullOrEmpty(authorizationHeader) || !authorizationHeader.StartsWith("Basic ", StringComparison.OrdinalIgnoreCase))
        {
            return false;
        }

        try
        {
            var encodedCredentials = authorizationHeader["Basic ".Length..].Trim();
            var decodedCredentials = Encoding.UTF8.GetString(Convert.FromBase64String(encodedCredentials));

            // For Basic auth format: username:password
            // We'll accept any username with the correct password
            var colonIndex = decodedCredentials.IndexOf(':');
            if (colonIndex < 0)
            {
                return false;
            }

            var password = decodedCredentials[(colonIndex + 1)..];
            return password == _expectedPassword;
        }
        catch
        {
            return false;
        }
    }
}
