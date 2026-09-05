using Pine.Core;
using Pine.Elm;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.CommandLine;
using System.IO;
using System.Text;
using System.Threading;

namespace Pine.CLI;

public static class LangServerCommand
{
    public static Command Create()
    {
        var command =
            new Command("lang-server", "Language server for Elm development environments.")
            {
                Aliases = { "lsp" }
            };

        /*
         * TODO: Consider log details for unrecognized args to make integration with tools easier.
         * */

        var logFileDirOption = new Option<string?>("--log-dir");

        var stdioOption =
            /*
             * The client in VSCode extension sample was observed to add this option automatically:
             * https://github.com/microsoft/vscode-extension-samples/tree/7ce43a47d7a53935b093a0e10fc490ea6a3cec32/lsp-sample
             * */
            new Option<bool>("--stdio");

        command.Add(logFileDirOption);
        command.Add(stdioOption);

        command.SetAction(
            (parseResult) =>
            {
                var logFileDirFromOption = parseResult.GetValue(logFileDirOption);

                static string? LogFileDirFromEnv()
                {
                    if (LogFileDirFromEnvironmentVariable() is not { } general)
                    {
                        return null;
                    }

                    return Path.Combine(general, "lang-server");
                }

                IReadOnlyList<string> logFileDirs =
                    [
                    .. new[]
                    {
                        logFileDirFromOption,
                        LogFileDirFromEnv()
                    }.WhereNotNull()
                    ];

                List<Stream> logFileStreams = [];

                var logFileName =
                    DateTimeOffset.UtcNow.ToString("yyyy-MM-dd-HH-mm-ss") + "-" + Environment.ProcessId + ".log";

                Console.Error.WriteLine(
                    "Got " + logFileDirs.Count + " log file directories: " +
                    string.Join(", ", logFileDirs));

                foreach (var logFileDir in logFileDirs)
                {
                    var logFilePath = Path.Combine(logFileDir, logFileName);

                    Console.Error.WriteLine("Creating log file at " + logFilePath);

                    try
                    {
                        Directory.CreateDirectory(logFileDir);

                        logFileStreams.Add(
                            new FileStream(path: logFilePath, FileMode.Create, FileAccess.ReadWrite, FileShare.Read));
                    }
                    catch (Exception exception)
                    {
                        Console.Error.WriteLine(
                            "Failed creating optional language-server log file at " +
                            logFilePath + ": " + exception);
                    }
                }

                const int LogQueueCapacity = 4096;

                var logLines = new BlockingCollection<string>(LogQueueCapacity);
                long droppedLogLineCount = 0;

                void WriteLogLine(string lineContent)
                {
                    try
                    {
                        Console.Error.WriteLine(lineContent);
                    }
                    catch
                    {
                    }

                    for (var index = logFileStreams.Count - 1; 0 <= index; --index)
                    {
                        try
                        {
                            logFileStreams[index].Write(Encoding.UTF8.GetBytes(lineContent + "\n"));
                            logFileStreams[index].Flush();
                        }
                        catch (Exception exception)
                        {
                            try
                            {
                                logFileStreams[index].Dispose();
                            }
                            catch
                            {
                            }

                            logFileStreams.RemoveAt(index);

                            try
                            {
                                Console.Error.WriteLine(
                                    DateTimeOffset.UtcNow.ToString("HH-mm-ss.fff") +
                                    ": Disabled failed language-server log file sink: " + exception);
                            }
                            catch
                            {
                            }
                        }
                    }
                }

                _ =
                    System.Threading.Tasks.Task.Factory.StartNew(
                        () =>
                        {
                            foreach (var lineContent in logLines.GetConsumingEnumerable())
                            {
                                var dropped = Interlocked.Exchange(ref droppedLogLineCount, 0);

                                if (0 < dropped)
                                {
                                    WriteLogLine(
                                        DateTimeOffset.UtcNow.ToString("HH-mm-ss.fff") +
                                        ": Language-server log queue dropped " + dropped +
                                        " lines after reaching capacity " + LogQueueCapacity);
                                }

                                WriteLogLine(lineContent);
                            }
                        },
                        CancellationToken.None,
                        System.Threading.Tasks.TaskCreationOptions.DenyChildAttach |
                        System.Threading.Tasks.TaskCreationOptions.LongRunning,
                        System.Threading.Tasks.TaskScheduler.Default);

                void Log(string content)
                {
                    var timeText = DateTimeOffset.UtcNow.ToString("HH-mm-ss.fff");

                    var lineContent = timeText + ": " + content;

                    if (!logLines.TryAdd(lineContent))
                    {
                        Interlocked.Increment(ref droppedLogLineCount);
                    }
                }

                AppDomain.CurrentDomain.UnhandledException +=
                    (sender, args) =>
                    {
                        Log("Unhandled exception: " + args.ExceptionObject);
                    };

                System.Threading.Tasks.TaskScheduler.UnobservedTaskException +=
                    (sender, args) =>
                    {
                        Log("Unobserved task exception: " + args.Exception);
                    };

                Log("Pine version " + PineCliCommand.AppVersionId + " starting language server...");

                var languageServer =
                    global::Pine.Elm.LanguageServerAdapters.LanguageServerComposition.CreateLanguageServer(
                        pineAppVersionId: PineCliCommand.AppVersionId,
                        logDelegate: Log);

                var rpcHandler =
                    new StreamJsonRpc.HeaderDelimitedMessageHandler(
                        sendingStream: Console.OpenStandardOutput(),
                        receivingStream: Console.OpenStandardInput(),
                        formatter: LanguageServerRpcTarget.JsonRpcMessageFormatterDefault(Log));

                var jsonRpcTarget = new LanguageServerRpcTarget(languageServer, LogDelegate: Log);

                using var jsonRpc =
                    new StreamJsonRpc.JsonRpc(
                        rpcHandler,
                        target: jsonRpcTarget);

                jsonRpcTarget.JsonRpc = jsonRpc;

                jsonRpc.StartListening();

                while (true)
                {
                    Thread.Sleep(TimeSpan.FromSeconds(1));
                }

                return 0;
            });

        return command;
    }

    private static string? LogFileDirFromEnvironmentVariable() =>
        Environment.GetEnvironmentVariable("PINE_LOG_DIR");
}
