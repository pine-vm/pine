using Pine.Core;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Elm.Platform;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text;

namespace Pine.CLI;

public static class RunCommand
{
    public static Command Create()
    {
        var command = new Command("run", "Run an Elm app.");

        var entryPointArgument = PineCliCommand.RequiredArgument("entry-point-module");

        var inputDirectoryOption = new Option<string?>("--input-directory");

        command.Add(entryPointArgument);
        command.Add(inputDirectoryOption);

        command.SetAction(
            (parseResult) =>
            {
                var entryPoint = parseResult.GetRequiredValue(entryPointArgument);
                var inputDirectory = parseResult.GetValue(inputDirectoryOption);

                var actualInputDirectory = inputDirectory ?? Environment.CurrentDirectory;

                try
                {
                    return
                        RunElmAppOnCommandLine(actualInputDirectory, entryPoint)
                        .Extract(
                            err =>
                            {
                                Console.Error.WriteLine(err);
                                return -1;
                            });
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine("Failed to run app with runtime exception: " + ex);
                    return -2;
                }
            });

        return command;
    }

    private static Result<string, int> RunElmAppOnCommandLine(
        string inputDirectory,
        string entryPoint)
    {
        var entryPointFilePath = entryPoint.Split(['/', '\\']);

        var loadInputDirectoryFailedFiles =
            new Dictionary<IReadOnlyList<string>, IOException>(
                comparer: EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        var loadInputDirectoryResult =
            LoadComposition.LoadFromPathResolvingNetworkDependencies(
                inputDirectory,
                ignoreFileOnIOException: (filePath, ioException) =>
                {
                    loadInputDirectoryFailedFiles[filePath] = ioException;
                    return true;
                })
            .LogToActions(Console.WriteLine);

        if (loadInputDirectoryResult.IsErrOrNull() is { } loadErr)
        {
            Console.WriteLine("Failed loading: " + loadErr);

            return 1;
        }

        if (loadInputDirectoryResult.IsOkOrNullable() is not { } loadOk)
        {
            throw new Exception(
                "Unexpected result type: " + loadInputDirectoryResult.GetType());
        }

        return RunElmAppOnCommandLine(loadOk.tree, entryPointFilePath);
    }

    private static Result<string, int> RunElmAppOnCommandLine(
        FileTree sourceFiles,
        IReadOnlyList<string> entryPointFilePath)
    {
        if (sourceFiles.GetNodeAtPath(entryPointFilePath) is not FileTree entryPointNode)
        {
            return
                Result<string, int>.err(
                    "Did not find the entry point '" + string.Join("/", entryPointFilePath) +
                    "' in the input directory.");
        }

        if (entryPointNode is not FileTree.FileNode entryPointBlob)
        {
            return
                "The entry point module '" + string.Join("/", entryPointFilePath) +
                "' is not a file in the input directory.";
        }

        var entryPointFileText = Encoding.UTF8.GetString(entryPointBlob.Bytes.Span);

        var parseModuleNameResult =
            ElmModule.ParseModuleName(entryPointFileText);

        if (parseModuleNameResult.IsErrOrNull() is { } err)
        {
            return
                "Failed to parse the module name from the entry point module '" +
                string.Join("/", entryPointFilePath) + "': " + err;
        }

        if (parseModuleNameResult.IsOkOrNull() is not { } elmModuleName)
        {
            return "Unexpected return type parsing module name: " + parseModuleNameResult.GetType();
        }

        var envVarDict = Environment.GetEnvironmentVariables();

        var environmentVariables =
            envVarDict.Keys.OfType<string>()
            .Select(envVarKey => new KeyValuePair<string, string>(envVarKey, envVarDict[envVarKey].ToString()))
            .ToImmutableArray();

        Console.WriteLine(
            "Starting Elm app from " + string.Join("/", entryPointFilePath) +
            " using runtime version " + PineCliCommand.AppVersionId + " ...");

        var appConfig =
            CommandLineAppConfig.ConfigFromSourceFilesAndModuleName(
                sourceFiles,
                elmModuleName);

        var mutatingCliApp =
            new MutatingCommandLineApp(
                appConfig,
                environment: new CommandLineAppConfig.CommandLineAppInitEnvironment(
                    CommandLine: Environment.CommandLine,
                    EnvironmentVariables: environmentVariables));

        // using var standardInput = Console.OpenStandardInput();
        using var standardOutput = Console.OpenStandardOutput();
        using var standardError = Console.OpenStandardError();

        void ProcessStandardInput(ReadOnlyMemory<byte> bytes)
        {
            if (bytes.Span.Length is not 0)
            {
                var appEventResponse = mutatingCliApp.EventStdIn(bytes);
            }

            foreach (var outputItem in mutatingCliApp.DequeueStdOut())
            {
                standardOutput.Write(outputItem.Span);
            }

            standardOutput.Flush();

            foreach (var outputItem in mutatingCliApp.DequeueStdErr())
            {
                standardError.Write(outputItem.Span);
            }

            standardError.Flush();
        }

        ProcessStandardInput(ReadOnlyMemory<byte>.Empty);

        if (mutatingCliApp.ExitCode is { } initExitCode)
        {
            return initExitCode;
        }

        var buffer = new byte[0x100_000];

        /*
         * When the standard input has been redirected (for example when this process
         * is launched as a subprocess by another program or by an integration test),
         * `Console.ReadKey` throws `InvalidOperationException`. In that case we read
         * raw bytes from the standard-input stream instead. The `Console.ReadKey`
         * path is preserved for the interactive terminal case where, on Windows,
         * reading from the standard-input stream blocks until a line break.
         * */

        if (Console.IsInputRedirected)
        {
            using var standardInput = Console.OpenStandardInput();

            while (true)
            {
                var readCount = standardInput.Read(buffer);

                if (readCount < 1)
                {
                    /*
                     * EOF on standard input: forward to the Elm app as a zero-length
                     * event so it can flush any buffered output, then exit if the
                     * app has not requested an exit code on its own.
                     * */

                    ProcessStandardInput(ReadOnlyMemory<byte>.Empty);

                    return mutatingCliApp.ExitCode ?? 0;
                }

                ProcessStandardInput(buffer.AsMemory()[..readCount]);

                if (mutatingCliApp.ExitCode is { } exitCode)
                {
                    return exitCode;
                }
            }
        }

        while (true)
        {
            var keys = new List<ReadOnlyMemory<byte>>(capacity: 100);

            void readKey()
            {
                var keyInfo = Console.ReadKey(intercept: true);

                keys.Add(Encoding.UTF8.GetBytes([keyInfo.KeyChar]));
            }

            readKey();

            while (Console.KeyAvailable)
            {
                readKey();
            }

            var asStandardInput = BytesConversions.Concat(keys);

            ProcessStandardInput(asStandardInput);

            if (mutatingCliApp.ExitCode is { } exitCode)
            {
                return exitCode;
            }
        }
    }
}
