using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Pine.Core.Http;
using Pine.Core.IO;
using System;
using System.CommandLine;
using System.IO;

namespace Pine.CLI;

public static class RunFileServerCommand
{
    public static Command Create()
    {
        var command =
            new Command("run-file-server", "Run an HTTP server that provides a REST API for file operations.");

        var storeOption = new Option<string?>("--store");

        var portOption = new Option<int?>("--port");

        var authPasswordOption = new Option<string?>("--auth-password");

        command.Add(storeOption);
        command.Add(portOption);
        command.Add(authPasswordOption);

        command.SetAction(
            (parseResult) =>
            {
                var store = parseResult.GetValue(storeOption);
                var port = parseResult.GetValue(portOption);
                var authPassword = parseResult.GetValue(authPasswordOption);

                // Determine file store
                IFileStore fileStore;

                if (store == null)
                {
                    Console.WriteLine("Warning: No --store option specified. Using in-memory store.");
                    fileStore = new FileStoreFromConcurrentDictionary();
                }
                else
                {
                    var absoluteDirectoryPath = Path.GetFullPath(store);

                    Console.WriteLine($"Using store directory: {store}");
                    Console.WriteLine($"Absolute directory path: {absoluteDirectoryPath}");

                    // Create directory if it doesn't exist
                    Directory.CreateDirectory(absoluteDirectoryPath);

                    // Use common retry options for file operations
                    var retryOptions =
                        new FileStoreFromSystemIOFile.FileStoreRetryOptions(
                            MaxRetryAttempts: 3,
                            InitialRetryDelay: TimeSpan.FromMilliseconds(100),
                            MaxRetryDelay: TimeSpan.FromSeconds(1));

                    fileStore = new FileStoreFromSystemIOFile(absoluteDirectoryPath, retryOptions);
                }

                // Determine port
                var actualPort = port ?? 8080;

                Console.WriteLine($"Starting FileStore HTTP server on port {actualPort}...");

                if (authPassword != null)
                {
                    Console.WriteLine("Basic authentication is enabled.");
                }

                try
                {
                    return RunFileStoreHttpServer(fileStore, actualPort, authPassword);
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Failed to start server: {ex.Message}");
                    return 1;
                }
            });

        return command;
    }

    private static int RunFileStoreHttpServer(IFileStore fileStore, int port, string? authPassword)
    {
        var builder = WebApplication.CreateBuilder();

        builder.WebHost.UseKestrel(
            options =>
            {
                options.ListenAnyIP(port);
            });

        builder.Services.AddSingleton(fileStore);

        if (authPassword is not null)
        {
            // Store password for BasicAuthenticationMiddleware
            builder.Services.AddSingleton(provider => new BasicAuthenticationConfig(authPassword, "FileStore API"));
        }

        using var app = builder.Build();

        if (authPassword is not null)
        {
            app.UseMiddleware<BasicAuthenticationMiddleware>();
        }

        app.UseMiddleware<FileStoreHttpServerMiddleware>();

        Console.WriteLine($"Server started. Listening on http://localhost:{port}");
        Console.WriteLine("Press Ctrl+C to stop the server.");

        app.StartAsync().Wait();

        // Wait for shutdown signal
        var cancellationTokenSource = new System.Threading.CancellationTokenSource();

        Console.CancelKeyPress +=
            (_, e) =>
            {
                e.Cancel = true;
                cancellationTokenSource.Cancel();
            };

        try
        {
            cancellationTokenSource.Token.WaitHandle.WaitOne();
        }
        catch (OperationCanceledException)
        {
            // Expected when Ctrl+C is pressed
        }

        Console.WriteLine("Shutting down...");
        app.StopAsync().Wait();

        return 0;
    }
}
