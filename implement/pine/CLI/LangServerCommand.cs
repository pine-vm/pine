using Pine.Core;
using Pine.Elm;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.IO;
using System.Text;

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

                    Directory.CreateDirectory(logFileDir);

                    logFileStreams.Add(
                        new FileStream(path: logFilePath, FileMode.Create, FileAccess.ReadWrite, FileShare.Read));
                }

                void Log(string content)
                {
                    var timeText = DateTimeOffset.UtcNow.ToString("HH-mm-ss.fff");

                    var lineContent = timeText + ": " + content;

                    Console.Error.WriteLine(lineContent);

                    foreach (var logFileStream in logFileStreams)
                    {
                        logFileStream.Write(Encoding.UTF8.GetBytes(lineContent + "\n"));
                        logFileStream.Flush();
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
                    Elm.LanguageServerAdapters.LanguageServerComposition.CreateLanguageServer(
                        pineAppVersionId: PineCliCommand.AppVersionId,
                        logDelegate: Log);

                var rpcHandler =
                    new StreamJsonRpc.HeaderDelimitedMessageHandler(
                        sendingStream: Console.OpenStandardOutput(),
                        receivingStream: Console.OpenStandardInput(),
                        formatter: LanguageServerRpcTarget.JsonRpcMessageFormatterDefault());

                var jsonRpcTarget = new LanguageServerRpcTarget(languageServer, LogDelegate: Log);

                using var jsonRpc =
                    new StreamJsonRpc.JsonRpc(
                        rpcHandler,
                        target: jsonRpcTarget);

                jsonRpcTarget.JsonRpc = jsonRpc;

                jsonRpc.StartListening();

                while (true)
                {
                    System.Threading.Thread.Sleep(TimeSpan.FromSeconds(1));
                }

                return 0;
            });

        return command;
    }

    private static string? LogFileDirFromEnvironmentVariable() =>
        Environment.GetEnvironmentVariable("PINE_LOG_DIR");
}
