using ElmTime.Elm019;
using ElmTime.ElmInteractive;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Pine;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.Elm019;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.Http;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.IO;
using Pine.Elm;
using Pine.Elm.CLI;
using Pine.Elm.CommonBinaries;
using Pine.Elm.Platform;
using Pine.IntermediateVM;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;
using static ElmTime.Platform.WebService.Configuration;

namespace ElmTime;

public class Program
{
    public static string AppVersionId => "0.4.28";

    private static int AdminInterfaceDefaultPort => 4000;

    private static int Main(string[] args)
    {
        return MainLessDispose(args, dynamicPGOShare: null);
    }

    private static int MainLessDispose(
        string[] args,
        DynamicPGOShare? dynamicPGOShare)
    {
        LoadFromGitHubOrGitLab.RepositoryFilesPartialForCommitCacheDefault =
            new CacheByFileName(new FileStoreFromSystemIOFile(Path.Combine(Filesystem.CacheDirectory, "git", "partial-for-commit", "zip")));

        var rootCommand = new RootCommand("Pine: Elm DevTools and runtime\nTo get help or report an issue, see https://github.com/pine-vm/pine/discussions");

        // Custom -v version option that shows "pine X.X.X" format
        var versionOption = new Option<bool>("-v")
        {
            Description = "Show version information"
        };
        rootCommand.Add(versionOption);

        // Install command
        var installCommand = CreateInstallCommand();
        rootCommand.Add(installCommand);

        // Core commands
        rootCommand.Add(CreateSelfTestCommand());
        rootCommand.Add(CreateRunCommand());
        rootCommand.Add(CreateRunServerCommand());
        rootCommand.Add(CreateDeployCommand());
        rootCommand.Add(CreateCopyAppStateCommand());
        rootCommand.Add(CreateCopyProcessCommand());
        rootCommand.Add(CreateListFunctionsCommand());
        rootCommand.Add(CreateApplyFunctionCommand());
        rootCommand.Add(CreateTruncateProcessHistoryCommand());
        rootCommand.Add(CreateInteractiveCommand(dynamicPGOShare));
        rootCommand.Add(CreateCompileCommand());
        rootCommand.Add(CreateElmTestRsCommand());
        rootCommand.Add(CreateMakeCommand());
        rootCommand.Add(ElmFormatCommand.CreateElmFormatCommand());
        rootCommand.Add(CreateDescribeCommand());
        rootCommand.Add(CreateRunCacheServerCommand());
        rootCommand.Add(CreateRunFileServerCommand());
        rootCommand.Add(CreateCompileInteractiveEnvCommand());
        rootCommand.Add(CreateLanguageServerCommand());
        rootCommand.Add(CreateUserSecretsCommand());
        rootCommand.Add(CreateHelpCommand(rootCommand));

        // Root command handler (show help when no command specified)
        rootCommand.SetAction((parseResult) =>
        {
            var showVersion = parseResult.GetValue(versionOption);

            if (showVersion)
            {
                Console.WriteLine("pine " + AppVersionId);
                return 0;
            }

            // Show help when no command is specified
            ShowCustomHelp(rootCommand);

            return 0;
        });

        var parseResult = rootCommand.Parse(args);
        return parseResult.Invoke();
    }

    private static Command CreateInstallCommand()
    {
        var (commandName, checkInstallation) = CheckIfExecutableIsRegisteredOnPath();

        var command = new Command("install", "Install the '" + commandName + "' command for the current user account.");

        command.SetAction((parseResult) =>
        {
            checkInstallation().registerExecutableDirectoryOnPath();
        });

        return command;
    }

    private static Command CreateUserSecretsCommand()
    {
        var command = new Command("user-secrets", "Manage passwords for accessing the admin interfaces of servers.");

        var siteArgument = new Argument<string>("site");

        var passwordArgument = new Argument<string>("password");

        var storeCommand = new Command("store", "Store a password for a site");
        storeCommand.Add(siteArgument);
        storeCommand.Add(passwordArgument);

        storeCommand.SetAction((parseResult) =>
        {
            var site = parseResult.GetValue(siteArgument);
            var password = parseResult.GetValue(passwordArgument);

            UserSecrets.StorePasswordForSite(site, password);

            return 0;
        });

        command.Add(storeCommand);

        command.SetAction((parseResult) =>
        {
            Console.WriteLine("Please specify a subcommand.");
            Console.WriteLine("Available subcommands:");
            Console.WriteLine("  store - Store a password for a site");
        });

        return command;
    }

    private static Command CreateHelpCommand(RootCommand rootCommand)
    {
        var command = new Command("help", "Explain available commands and how to use the command-line interface.");

        var allOption = new Option<bool>("--all", ["-a"])
        {
            Description = "Show all commands including hidden ones"
        };

        command.Add(allOption);

        command.SetAction((parseResult) =>
        {
            var showAll = parseResult.GetValue(allOption);

            if (showAll)
            {
                ShowAllCommands(rootCommand);
            }
            else
            {
                ShowCustomHelp(rootCommand);
            }

            return 0;
        });

        return command;
    }

    private static void ShowCustomHelp(RootCommand rootCommand)
    {
        var checkedInstallation = CheckIfExecutableIsRegisteredOnPath().checkInstallation();
        var elmFsCommandName = CheckIfExecutableIsRegisteredOnPath().commandName;

        // Optional short descriptions for overview display
        // When null, uses the command's full Description property
        var shortDescriptions = new Dictionary<string, string?>
        {
            ["install"] = "Install the command for the current user account.",
            ["interactive"] = null,
            ["compile"] = "Compile app source code.",
            ["elm-test-rs"] = "Compile and run tests.",
            ["make"] = "Compile Elm code.",
            ["elm-format"] = "Format Elm module files.",
            ["describe"] = "Describe a composition.",
            ["run"] = null,
            ["run-server"] = "Run a server with a web-based admin interface.",
            ["deploy"] = "Deploy an app to an Elm backend process.",
            ["copy-app-state"] = "Copy the state of an Elm backend app.",
            ["copy-process"] = "Copy all files needed to restore a process.",
            ["list-functions"] = "List the functions exposed by an Elm app.",
            ["apply-function"] = "Apply an Elm function on a database.",
            ["truncate-process-history"] = "Remove parts of the process history.",
            ["run-file-server"] = null,
        };

        var commandsByName = rootCommand.Subcommands.ToDictionary(c => c.Name, c => c);

        string GetDisplayDescription(string commandName)
        {
            if (!commandsByName.TryGetValue(commandName, out var command))
                return "";

            // Use short description if provided, otherwise fall back to full description
            return shortDescriptions.TryGetValue(commandName, out var shortDesc) && shortDesc != null
                ? shortDesc
                : command.Description ?? "";
        }

        var setupGroupCommandNames = new List<string>();
        if (!checkedInstallation.executableIsRegisteredOnPath)
        {
            setupGroupCommandNames.Add("install");
        }

        var developCommandNames = new List<string>
        {
            "interactive",
            "compile",
            "elm-test-rs",
            "make",
            "elm-format",
            "describe",
        };

        var operateCommandNames = new List<string>
        {
            "run",
            "run-server",
            "deploy",
            "copy-app-state",
            "copy-process",
            "list-functions",
            "apply-function",
            "truncate-process-history",
            "run-file-server",
        };

        Console.WriteLine(rootCommand.Description);
        Console.WriteLine($"\nUsage: {elmFsCommandName} [command] [options]");
        Console.WriteLine("\nThese are common pine commands used in various situations:");

        if (setupGroupCommandNames.Count is not 0)
        {
            Console.WriteLine("\nSet up your development environment:");
            foreach (var name in setupGroupCommandNames)
            {
                Console.WriteLine($"   {name,-30} {GetDisplayDescription(name)}");
            }
        }

        Console.WriteLine("\nDevelop and learn:");
        foreach (var name in developCommandNames)
        {
            Console.WriteLine($"   {name,-30} {GetDisplayDescription(name)}");
        }

        Console.WriteLine("\nRun apps, operate servers and maintain live systems:");
        foreach (var name in operateCommandNames)
        {
            Console.WriteLine($"   {name,-30} {GetDisplayDescription(name)}");
        }

        Console.WriteLine($"\n'{elmFsCommandName} help -a' lists available subcommands.");
        Console.WriteLine($"See '{elmFsCommandName} help <command>' to read about a specific subcommand.");
    }

    private static void ShowAllCommands(RootCommand rootCommand)
    {
        Console.WriteLine(rootCommand.Description);
        Console.WriteLine($"\nUsage: pine [command] [options]");
        Console.WriteLine("\nCommands:");

        foreach (var command in rootCommand.Subcommands.OrderBy(c => c.Name))
        {
            Console.WriteLine($"  {command.Name,-25} {command.Description}");
        }

        Console.WriteLine("\nOptions:");
        foreach (var option in rootCommand.Options)
        {
            var aliases = string.Join(", ", option.Aliases);
            Console.WriteLine($"  {aliases,-25} {option.Description}");
        }
    }

    private static Command CreateSelfTestCommand()
    {
        var command = new Command("self-test", "Tests integration of native dependencies");

        command.SetAction((parseResult) =>
        {
            return Test.SelfTest.RunAllTestsAndPrintToConsole();
        });

        return command;
    }

    private static Command CreateRunServerCommand()
    {
        var command = new Command("run-server", "Run a server with a web-based admin interface. The HTTP API supports deployments, migrations, and other operations to manage your app.");

        var adminUrlsDefault = "http://*:" + AdminInterfaceDefaultPort;

        var processStoreOption = new Option<string?>("--process-store");

        var processStoreReadonlyOption = new Option<string?>("--process-store-readonly");

        var deletePreviousProcessOption = new Option<bool>("--delete-previous-process");

        var adminUrlsOption = new Option<string?>("--admin-urls")
        {
            Description = "Defaults to '" + adminUrlsDefault + "'."
        };

        var adminPasswordOption = new Option<string?>("--admin-password");

        var publicAppUrlsOption = new Option<string?>("--public-urls")
        {
            Description = "Defaults to '" + string.Join(",", PublicWebHostUrlsDefault) + "'."
        };

        var copyProcessOption = new Option<string?>("--copy-process");

        var deployOption = new Option<string?>("--deploy");

        command.Add(processStoreOption);
        command.Add(processStoreReadonlyOption);
        command.Add(deletePreviousProcessOption);
        command.Add(adminUrlsOption);
        command.Add(adminPasswordOption);
        command.Add(publicAppUrlsOption);
        command.Add(copyProcessOption);
        command.Add(deployOption);

        command.SetAction((parseResult) =>
        {
            var processStorePath = parseResult.GetValue(processStoreOption);
            var processStoreReadonlyPath = parseResult.GetValue(processStoreReadonlyOption);
            var deletePreviousProcess = parseResult.GetValue(deletePreviousProcessOption);
            var adminUrls = parseResult.GetValue(adminUrlsOption);
            var adminPassword = parseResult.GetValue(adminPasswordOption);
            var publicUrls = parseResult.GetValue(publicAppUrlsOption);
            var copyProcess = parseResult.GetValue(copyProcessOption);
            var deploy = parseResult.GetValue(deployOption);

            var publicAppUrls =
                publicUrls?.Split(',').Select(url => url.Trim()).ToArray() ??
                PublicWebHostUrlsDefault;

            var adminInterfaceUrls = adminUrls ?? adminUrlsDefault;

            var webHost =
            RunServer.BuildWebHostToRunServer(
                processStorePath: processStorePath,
                processStoreReadonlyPath: processStoreReadonlyPath,
                adminInterfaceUrls: adminInterfaceUrls,
                adminPassword: adminPassword,
                publicAppUrls: publicAppUrls,
                deletePreviousProcess: deletePreviousProcess,
                copyProcess: copyProcess,
                deployApp: deploy);

            Console.WriteLine("Starting web server with admin interface...");

            webHost.StartAsync().Wait();

            Console.WriteLine("Completed starting the web server with the admin interface at '" + adminInterfaceUrls + "'.");

            webHost.WaitForShutdownAsync().Wait();

            return 0;
        });

        return command;
    }

    private static Command CreateRunCommand()
    {
        var command = new Command("run", "Run an Elm app.");

        var entryPointArgument = new Argument<string>("entry-point-module");

        var inputDirectoryOption = new Option<string?>("--input-directory");

        command.Add(entryPointArgument);
        command.Add(inputDirectoryOption);

        command.SetAction((parseResult) =>
        {
            var entryPoint = parseResult.GetValue(entryPointArgument);
            var inputDirectory = parseResult.GetValue(inputDirectoryOption);

            var actualInputDirectory = inputDirectory ?? Environment.CurrentDirectory;

            try
            {
                return
                RunElmAppOnCommandLine(actualInputDirectory, entryPoint)
                .Extract(err =>
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
            return Result<string, int>.err(
                "Did not find the entry point '" + string.Join("/", entryPointFilePath) + "' in the input directory.");
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
            " using runtime version " + AppVersionId + " ...");

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

        void processStandardInput(ReadOnlyMemory<byte> bytes)
        {
            if (bytes.Span.Length is not 0)
            {
                var appEventResponse = mutatingCliApp.EventStdIn(bytes);
            }

            foreach (var outputItem in mutatingCliApp.DequeueStdOut())
            {
                standardOutput.Write(outputItem.Span);
            }

            foreach (var outputItem in mutatingCliApp.DequeueStdErr())
            {
                standardError.Write(outputItem.Span);
            }
        }

        processStandardInput(ReadOnlyMemory<byte>.Empty);

        var buffer = new byte[0x100_000];

        while (true)
        {
            /*
             * 2024-10-24:
             * When testing on Windows 11, observed that `Read` on the stream obtained via Console.OpenStandardInput()
             * Blocked until the user entered a line-break.
             * We have not yet found a way to switch the interface into a 'raw' mode to avoid such a block.
             * To avoid this block, we switch to Console.ReadKey for now.
             *

            var readCount = standardInput.Read(buffer);

            if (readCount < 1)
            {
                continue;
            }

            var maybeExitCode = processStandardInput(buffer.AsMemory()[..readCount]);
            */

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

            processStandardInput(asStandardInput);

            if (mutatingCliApp.ExitCode is { } exitCode)
            {
                return exitCode;
            }
        }
    }

    private static Command CreateDeployCommand()
    {
        var command = new Command("deploy", "Deploy an app to an Elm backend process. Deployment implies migration from the previous app state if not specified otherwise.");

        var sourceArgument = new Argument<string>("source");

        var siteArgument = new Argument<string>("process-site");

        var sitePasswordOption = new Option<string?>("--site-password");

        var initAppStateOption = new Option<bool>("--init-app-state");

        command.Add(sourceArgument);
        command.Add(siteArgument);
        command.Add(sitePasswordOption);
        command.Add(initAppStateOption);

        command.SetAction((parseResult) =>
        {
            var source = parseResult.GetValue(sourceArgument);
            var site = parseResult.GetValue(siteArgument);
            var sitePassword = parseResult.GetValue(sitePasswordOption);
            var initAppState = parseResult.GetValue(initAppStateOption);

            var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(site);

            var deployReport =
                DeployApp(
                    sourcePath: source,
                    site: site,
                    siteDefaultPassword: actualPassword,
                    initElmAppState: initAppState,
                    promptForPasswordOnConsole: true);

            WriteReportToFileInReportDirectory(
                reportContent: System.Text.Json.JsonSerializer.Serialize(
                    deployReport,
                    reportJsonSerializerOptions),
                reportKind: "deploy.json");

            return 0;
        });

        return command;
    }

    private static Command CreateCopyAppStateCommand()
    {
        var command = new Command("copy-app-state", "Copy the state of an Elm backend app.");

        var sourceArgument = new Argument<string>("source");

        var destinationArgument = new Argument<string?>("destination")
        {
            Arity = ArgumentArity.ZeroOrOne
        };

        var sourcePasswordOption = new Option<string?>("--source-password");

        var destinationPasswordOption = new Option<string?>("--destination-password");

        command.Add(sourceArgument);
        command.Add(destinationArgument);
        command.Add(sourcePasswordOption);
        command.Add(destinationPasswordOption);

        command.SetAction((parseResult) =>
        {
            var source = parseResult.GetValue(sourceArgument);
            var destination = parseResult.GetValue(destinationArgument);
            var sourcePassword = parseResult.GetValue(sourcePasswordOption);
            var destinationPassword = parseResult.GetValue(destinationPasswordOption);

            var actualSourcePassword = sourcePassword ?? UserSecrets.LoadPasswordForSite(source);
            var actualDestinationPassword = destination != null ? (destinationPassword ?? UserSecrets.LoadPasswordForSite(destination)) : null;

            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var report = CopyElmAppState(
                source: source,
                sourceDefaultPassword: actualSourcePassword,
                destination: destination,
                destinationDefaultPassword: actualDestinationPassword)
            with
            {
                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds
            };

            WriteReportToFileInReportDirectory(
                reportContent: System.Text.Json.JsonSerializer.Serialize(
                    report,
                    reportJsonSerializerOptions),
                reportKind: "copy-app-state.json");

            return 0;
        });

        return command;
    }

    private static Command CreateCopyProcessCommand()
    {
        var command = new Command("copy-process", "Copy all files needed to restore a process and store them in a zip archive.");

        var siteArgument = new Argument<string>("process-site");

        var sitePasswordOption = new Option<string?>("--site-password");

        command.Add(siteArgument);
        command.Add(sitePasswordOption);

        command.SetAction((parseResult) =>
        {
            var site = parseResult.GetValue(siteArgument);
            var sitePassword = parseResult.GetValue(sitePasswordOption);

            var actualSite = MapSiteForCommandLineArgument(site);
            var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(actualSite);

            actualPassword =
                AttemptHttpRequest(
                () => new System.Net.Http.HttpRequestMessage { RequestUri = new Uri(actualSite) },
                defaultPassword: actualPassword,
                promptForPasswordOnConsole: true).Result.enteredPassword ?? actualPassword;

            Console.WriteLine("Begin reading process history from '" + actualSite + "' ...");

            var (files, lastCompositionLogRecordHashBase16) =
                RunServer.ReadFilesForRestoreProcessFromAdminInterface(actualSite, actualPassword!);

            Console.WriteLine("Completed reading files to restore process " + lastCompositionLogRecordHashBase16 + ". Read " + files.Count + " files from '" + actualSite + "'.");

            var zipArchive = ZipArchive.ZipArchiveFromFiles(files);

            var fileName = "process-" + lastCompositionLogRecordHashBase16 + ".zip";
            var filePath = Path.Combine(Environment.CurrentDirectory, fileName);

            File.WriteAllBytes(filePath, zipArchive);

            Console.WriteLine("Saved process archive to file '" + filePath + "'.");

            return 0;
        });

        return command;
    }

    private static Command CreateListFunctionsCommand()
    {
        var command = new Command("list-functions", "List the functions exposed by an Elm app for application on a database.");

        var siteArgument = new Argument<string>("process-site");

        var sitePasswordOption = new Option<string?>("--site-password");

        command.Add(siteArgument);
        command.Add(sitePasswordOption);

        command.SetAction((parseResult) =>
        {
            var site = parseResult.GetValue(siteArgument);
            var sitePassword = parseResult.GetValue(sitePasswordOption);

            var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(site);

            var listFunctionsResult =
                ListFunctions(
                    site: site,
                    siteDefaultPassword: actualPassword,
                    promptForPasswordOnConsole: true);

            var console = (IConsole)StaticConsole.Instance;

            return
            listFunctionsResult
            // For now, only show functions with a normal module prefix
            .Map(functions => functions.Where(f => f.functionName.Contains('.')).ToImmutableList())
            .Unpack(
                fromErr:
                err =>
                {
                    console.WriteLine("Failed to list functions at " + site + ": " + err, IConsole.TextColor.Red);

                    return 2;
                },
                fromOk:
                functions =>
                {
                    static string describeFunction(
                        StateShim.InterfaceToHost.NamedExposedFunction databaseFunction)
                    {
                        var commentOnReturnType =
                        "-- (return type " +
                        (databaseFunction.functionDescription.returnType.containsAppStateType ?
                        "contains app state type" : "does not contain app state type")
                        + ")";

                        return
                        "Function " + databaseFunction.functionName +
                        " has " + databaseFunction.functionDescription.parameters.Count + " parameters:\n" +
                        databaseFunction.functionName.Split('.').LastOrDefault(databaseFunction.functionName) + " :\n" +
                        string.Join("\n",
                        string.Join("", databaseFunction.functionDescription.parameters.Select(p => p.typeSourceCodeText)
                        .Concat([
                            databaseFunction.functionDescription.returnType.sourceCodeText +
                            " " + commentOnReturnType])
                        .Intersperse("\n-> "))
                        .Split("\n")
                        .Select(line => "    " + line));
                    }

                    console.WriteLine(
                        "Site " + site + " exposes " + functions.Count + " database functions:\n----------\n" +
                        string.Join("\n\n", functions.Select(describeFunction)) +
                        "\n----------\n");

                    return 0;
                });
        });

        return command;
    }

    private static Command CreateApplyFunctionCommand()
    {
        var command = new Command("apply-function", "Apply an Elm function on a database containing the state of an Elm app.");

        var siteArgument = new Argument<string>("process-site");

        var functionNameArgument = new Argument<string>("function-name");

        var sitePasswordOption = new Option<string?>("--site-password");

        var argumentOption = new Option<string[]>("--argument")
        {
            AllowMultipleArgumentsPerToken = true,
            Arity = ArgumentArity.ZeroOrMore
        };

        var commitResultingStateOption = new Option<bool>("--commit-resulting-state");

        command.Add(siteArgument);
        command.Add(functionNameArgument);
        command.Add(sitePasswordOption);
        command.Add(argumentOption);
        command.Add(commitResultingStateOption);

        command.SetAction((parseResult) =>
        {
            var site = parseResult.GetValue(siteArgument);
            var functionName = parseResult.GetValue(functionNameArgument);
            var sitePassword = parseResult.GetValue(sitePasswordOption);
            var arguments = parseResult.GetValue(argumentOption);
            var commitResultingState = parseResult.GetValue(commitResultingStateOption);

            var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(site);

            var serializedArgumentsJson = arguments.Select(LoadArgumentFromUserInterfaceAsJsonOrFileTextContext).ToImmutableList();

            var applyFunctionReport =
                ApplyFunction(
                    site: site,
                    functionName: functionName,
                    serializedArgumentsJson: serializedArgumentsJson,
                    commitResultingState: commitResultingState,
                    siteDefaultPassword: actualPassword,
                    promptForPasswordOnConsole: true);

            WriteReportToFileInReportDirectory(
                reportContent: System.Text.Json.JsonSerializer.Serialize(
                    applyFunctionReport,
                    reportJsonSerializerOptions),
                reportKind: "apply-function.json");

            return 0;
        });

        return command;
    }

    private static Command CreateLanguageServerCommand()
    {
        var command = new Command("lang-server", "Language server for Elm development environments.")
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

        command.SetAction((parseResult) =>
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
            [.. new[]
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

            AppDomain.CurrentDomain.UnhandledException += (sender, args) =>
            {
                Log("Unhandled exception: " + args.ExceptionObject);
            };

            System.Threading.Tasks.TaskScheduler.UnobservedTaskException += (sender, args) =>
            {
                Log("Unobserved task exception: " + args.Exception);
            };

            Log("Pine version " + AppVersionId + " starting language server...");

            var languageServer =
            new LanguageServer(
                logDelegate: Log,
                elmPackagesSearchDirectories:
                [Path.Combine(Elm019Binaries.GetElmHomeDirectory(), "0.19.1", "packages")]);

            var rpcHandler =
                new StreamJsonRpc.HeaderDelimitedMessageHandler(
                    sendingStream: Console.OpenStandardOutput(),
                    receivingStream: Console.OpenStandardInput(),
                    formatter: LanguageServerRpcTarget.JsonRpcMessageFormatterDefault());

            var jsonRpcTarget = new LanguageServerRpcTarget(languageServer, LogDelegate: Log);

            using var jsonRpc = new StreamJsonRpc.JsonRpc(
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

    static string? LogFileDirFromEnvironmentVariable() =>
        Environment.GetEnvironmentVariable("PINE_LOG_DIR");

    private static Command CreateCompileInteractiveEnvCommand()
    {
        var command = new Command("compile-interactive-env", "Compile an interactive environment from Elm modules into a Pine value");

        var envSourceOption =
            new Option<string[]>("--env-source")
            {
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var outputCompactBuildOption =
            new Option<string?>("--output-compact-build")
            {
                Arity = ArgumentArity.ZeroOrOne
            };

        var rootModuleFilePathOption =
            new Option<string[]>("--root-file-path")
            {
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var skipLoweringOption = new Option<bool>("--skip-lowering");

        var gzipOption = new Option<bool>("--gzip");

        var overrideCompilerOption = new Option<string?>("--override-compiler");

        command.Add(envSourceOption);
        command.Add(outputCompactBuildOption);
        command.Add(rootModuleFilePathOption);
        command.Add(skipLoweringOption);
        command.Add(gzipOption);
        command.Add(overrideCompilerOption);

        command.SetAction((parseResult) =>
        {
            var envSources = parseResult.GetValue(envSourceOption);
            var outputCompactBuild = parseResult.GetValue(outputCompactBuildOption);
            var rootModuleFilePaths = parseResult.GetValue(rootModuleFilePathOption);
            var skipLowering = parseResult.GetValue(skipLoweringOption);
            var gzip = parseResult.GetValue(gzipOption);
            var overrideCompiler = parseResult.GetValue(overrideCompilerOption);

            IReadOnlyList<IReadOnlyList<string>> rootFilePaths =
                [..(rootModuleFilePaths ?? [])
                .WhereNotNull()
                .Select(flat => flat.Split('/', '\\'))
                ];

            var environmentsSourceTrees =
            envSources
            .Select(sourcePath =>
            {
                var loadCompositionResult =
                    LoadComposition.LoadFromPathResolvingNetworkDependencies(sourcePath)
                    .LogToActions(Console.WriteLine)
                    .Extract(error => throw new Exception("Failed to load from path '" + sourcePath + "': " + error));

                var fileTree = loadCompositionResult.tree;

                if (fileTree is FileTree.FileNode sourceBlob)
                {
                    var zipEntries = ZipArchive.EntriesFromZipArchive(sourceBlob.Bytes);

                    fileTree = FileTree.FromSetOfFilesWithCommonFilePath(zipEntries);
                }

                return fileTree;
            })
            .ToImmutableArray();

            var aggregateElmModuleFiles =
            environmentsSourceTrees
            .SelectMany(tree => tree.EnumerateFilesTransitive())
            .Where(f => f.path.LastOrDefault()?.EndsWith(".elm", StringComparison.OrdinalIgnoreCase) ?? false)
            .ToImmutableArray();

            Console.WriteLine(
                "Loaded " + environmentsSourceTrees.Length + " source trees with " +
                aggregateElmModuleFiles.Length + " aggregate Elm module files.");

            Console.WriteLine(
                "Compiling with lowering " +
                (skipLowering ? "disabled" : "enabled"));

            Console.WriteLine(
                "Limiting the compilation to " + rootFilePaths.Count + " root files: " +
                string.Join(
                    ", ",
                    rootFilePaths.Select(path => string.Join("/", path))));

            ElmCompilerInElm? overrideElmCompiler = null;

            if (overrideCompiler is { } overrideCompilerPath)
            {
                Console.WriteLine("Using Elm compiler from " + overrideCompilerPath);

                overrideElmCompiler =
                ElmCompilerInElm.LoadCompilerFromBundleFile(overrideCompilerPath)
                .Extract(err => throw new Exception("Failed to load Elm compiler from " + overrideCompilerPath + ": " + err));

                var elmCompilerHash =
                new ConcurrentPineValueHashCache()
                .GetHash(overrideElmCompiler.CompilerEnvironment);

                Console.WriteLine(
                    "Loaded Elm compiler with hash " + Convert.ToHexStringLower(elmCompilerHash.Span)[..8]);
            }

            var compiledEnvironments =
            environmentsSourceTrees
            .Select(sourceTree =>
            {
                var compiledEnv =
                ElmCompilerInElm.LoadOrCompileInteractiveEnvironment(
                    sourceTree,
                    rootFilePaths: rootFilePaths,
                    skipLowering: skipLowering,
                    overrideElmCompiler: overrideElmCompiler)
                .Extract(err => throw new Exception("Failed compilation: " + err));

                return new KeyValuePair<FileTree, PineValue>(sourceTree, compiledEnv);
            })
            .ToImmutableDictionary();

            foreach (var (sourceTree, compiledEnv) in compiledEnvironments)
            {
                var sourceTreeHash = PineValueHashTree.ComputeHashSorted(sourceTree);

                var sourceTreeAllFiles =
                sourceTree
                .EnumerateFilesTransitive()
                .ToImmutableArray();

                var sourceTreeElmModules =
                sourceTreeAllFiles
                .Where(f => f.path?.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase) ?? false)
                .ToImmutableArray();

                var environmentNodesCount =
                compiledEnv is PineValue.ListValue compiledEnvList
                ?
                compiledEnvList.NodesCount
                :
                0;

                Console.WriteLine(
                    "Compiled source tree " + Convert.ToHexStringLower(sourceTreeHash.Span)[..8] +
                    " containing " + sourceTreeAllFiles.Length +
                    " files and " + sourceTreeElmModules.Length +
                    " Elm modules into environment with " +
                    CommandLineInterface.FormatIntegerForDisplay(environmentNodesCount) + " nodes.");
            }

            var (allComponents, bundleResourceFile) =
                BundledDeclarations.BuildBundleFile(
                    compiledEnvironments: compiledEnvironments,
                    otherReusedValues: ImmutableDictionary<string, PineValue>.Empty);

            Console.WriteLine(
                "Built bundle containing " +
                CommandLineInterface.FormatIntegerForDisplay(allComponents.Count) +
                " component entries in " +
                CommandLineInterface.FormatIntegerForDisplay(bundleResourceFile.Length) + " bytes.");

            var fileContent = bundleResourceFile;

            if (gzip)
            {
                fileContent = BundledDeclarations.CompressResourceFile(fileContent);

                Console.WriteLine(
                    "Applied gzip and compressed from " +
                    CommandLineInterface.FormatIntegerForDisplay(bundleResourceFile.Length) +
                    " to " +
                    CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) + " bytes");
            }

            if (outputCompactBuild != null)
            {
                var destFilePath = outputCompactBuild.Length > 0 ? outputCompactBuild : "compact-build.bin";

                if (Path.GetDirectoryName(destFilePath) is { } destDirectory && destDirectory.Length is not 0)
                {
                    Directory.CreateDirectory(destDirectory);
                }

                File.WriteAllBytes(
                    destFilePath,
                    fileContent.ToArray());

                Console.WriteLine(
                    "Saved compact build with " +
                    CommandLineInterface.FormatIntegerForDisplay(allComponents.Count) +
                    " component entries in " +
                    CommandLineInterface.FormatIntegerForDisplay(fileContent.Length) +
                    " bytes to " + destFilePath);
            }

            return 0;
        });

        return command;
    }

    private static string LoadArgumentFromUserInterfaceAsJsonOrFileTextContext(string argumentFromCLI)
    {
        try
        {
            var asJson = System.Text.Json.JsonSerializer.Deserialize<object>(argumentFromCLI);

            return argumentFromCLI;
        }
        catch { }

        return File.ReadAllText(argumentFromCLI);
    }

    private static Command CreateTruncateProcessHistoryCommand()
    {
        var command = new Command("truncate-process-history", "Remove parts of the process history that are not needed to restore the process.");

        var siteArgument = new Argument<string>("process-site");

        var sitePasswordOption = new Option<string?>("--site-password");

        command.Add(siteArgument);
        command.Add(sitePasswordOption);

        command.SetAction((parseResult) =>
        {
            var site = parseResult.GetValue(siteArgument);
            var sitePassword = parseResult.GetValue(sitePasswordOption);

            var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(site);

            var report =
                TruncateProcessHistory(
                    site: site,
                    siteDefaultPassword: actualPassword,
                    promptForPasswordOnConsole: true);

            WriteReportToFileInReportDirectory(
                reportContent: System.Text.Json.JsonSerializer.Serialize(
                    report,
                    reportJsonSerializerOptions),
                reportKind: "truncate-process-history.json");

            return 0;
        });

        return command;
    }

    private static Command CreateCompileCommand()
    {
        var command = new Command("compile", "Compile app source code the same way as would be done when deploying a web service.");

        var sourceArgument = new Argument<string>("source");

        command.Add(sourceArgument);

        command.SetAction((parseResult) =>
        {
            var source = parseResult.GetValue(sourceArgument);

            var compileReport = CompileAppAndSaveCompositionToZipArchive(source).report;

            WriteReportToFileInReportDirectory(
                reportContent: System.Text.Json.JsonSerializer.Serialize(
                    compileReport,
                    reportJsonSerializerOptions),
                reportKind: "compile.json");

            return 0;
        });

        return command;
    }

    private static Command CreateElmTestRsCommand()
    {
        var command = new Command("elm-test-rs", "Compile and run tests using the interface of elm-test-rs. The compilation integrates interfaces such as SourceFiles.");

        var sourceArgument = new Argument<string?>("source")
        {
            Arity = ArgumentArity.ZeroOrOne
        };

        var elmTestRsOutputOption = new Option<string?>("--elm-test-rs-output");

        command.Add(sourceArgument);
        command.Add(elmTestRsOutputOption);

        command.SetAction((parseResult) =>
        {
            var source = parseResult.GetValue(sourceArgument);
            var elmTestRsOutput = parseResult.GetValue(elmTestRsOutputOption);

            var elmTestResult = CompileAndElmTestRs(source: source ?? Environment.CurrentDirectory);

            static void saveTextToFileAndReportToConsole(string filePath, string text)
            {
                filePath = Path.GetFullPath(filePath);

                Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

                File.WriteAllText(filePath, text ?? "", Encoding.UTF8);
                Console.WriteLine("Saved " + text?.Length + " characters to " + filePath);
            }

            if (elmTestRsOutput != null)
            {
                saveTextToFileAndReportToConsole(elmTestRsOutput + ".stdout", elmTestResult.ProcessOutput.StandardOutput ?? "");
                saveTextToFileAndReportToConsole(elmTestRsOutput + ".stderr", elmTestResult.ProcessOutput.StandardError ?? "");
            }

            if (0 < elmTestResult.ProcessOutput.StandardError?.Length)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine(elmTestResult.ProcessOutput.StandardError);
                Console.ResetColor();
            }

            var eventsOutputs =
                ElmTestRs.OutputFromEvent(elmTestResult.ParseOutputResult);

            foreach (var eventOutput in eventsOutputs)
            {
                if (eventOutput.text.Any())
                    Console.WriteLine("");

                foreach (var coloredText in eventOutput.text)
                {
                    switch (coloredText.color)
                    {
                        case ElmTestRsConsoleOutputColor.RedColor:
                            Console.ForegroundColor = ConsoleColor.Red;
                            break;
                        case ElmTestRsConsoleOutputColor.GreenColor:
                            Console.ForegroundColor = ConsoleColor.Green;
                            break;
                        default:
                            Console.ResetColor();
                            break;
                    }

                    Console.Write(coloredText.text);
                }
            }

            Console.WriteLine("");

            // TODO: Report more details on timing.

            return elmTestResult.ProcessOutput.ExitCode;
        });

        return command;
    }

    private static (CompileAppReport report, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? compiledAppFiles)
        CompileAppAndSaveCompositionToZipArchive(string sourcePath)
    {
        var compileResult = CompileApp(sourcePath);

        if (compileResult.compiledAppFiles != null)
        {
            var compiledTree = FileTree.FromSetOfFilesWithStringPath(compileResult.compiledAppFiles);
            var compiledFiles = FileTreeExtensions.ToFlatDictionaryWithPathComparer(compiledTree);

            var compiledCompositionArchive = ZipArchive.ZipArchiveFromFiles(compiledFiles);

            var outputCompositionFileName = compileResult.report.compiledCompositionId + ".zip";

            var outputCompositionFilePath = Path.Combine(ReportFilePath, outputCompositionFileName);

            Directory.CreateDirectory(Path.GetDirectoryName(outputCompositionFilePath)!);
            File.WriteAllBytes(outputCompositionFilePath, compiledCompositionArchive);

            Console.WriteLine("\nSaved compiled composition " + compileResult.report.compiledCompositionId + " to '" + outputCompositionFilePath + "'.");
        }

        return compileResult;
    }

    public static (CompileAppReport report, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? compiledAppFiles)
        CompileApp(string sourcePath)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var report = new CompileAppReport
        (
            beginTime: BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow),
            engineVersion: AppVersionId,
            sourcePath: sourcePath,
            sourceCompositionId: null,
            sourceSummary: null,
            compilationIterationsReports: null,
            compilationErrors: null,
            compilationException: null,
            compilationTimeSpentMilli: null,
            compiledCompositionId: null,
            totalTimeSpentMilli: null
        );

        var loadCompositionResult =
            LoadComposition.LoadFromPathResolvingNetworkDependencies(sourcePath)
            .LogToActions(Console.WriteLine)
            .Extract(error => throw new Exception("Failed to load from path '" + sourcePath + "': " + error));

        var (sourceCompositionId, sourceSummary) = CompileSourceSummary(loadCompositionResult.tree);

        report = report with { sourceCompositionId = sourceCompositionId, sourceSummary = sourceSummary };

        Console.WriteLine("Loaded source composition " + sourceCompositionId + " from '" + sourcePath + "'. Starting to compile...");

        var compilationStopwatch = System.Diagnostics.Stopwatch.StartNew();

        IReadOnlyList<IReadOnlyList<string>>? readElmJsonSourceDirectories()
        {
            if (loadCompositionResult.tree.GetNodeAtPath(["elm.json"]) is not
                FileTree.FileNode elmJsonFile)
            {
                return null;
            }

            var elmJsonFileParsed =
                System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Bytes.Span);

            if (elmJsonFileParsed?.SourceDirectories is not { } sourceDirs)
            {
                return null;
            }

            return
                [..sourceDirs
                .Select(flat => flat.Split('/', '\\'))
                ];
        }

        try
        {
            var filteredSourceTree =
                loadCompositionResult.origin is LoadCompositionOrigin.FromLocalFileSystem
                ?
                LoadFromLocalFilesystem.RemoveNoiseFromTree(
                    loadCompositionResult.tree,
                    discardGitDirectory: true)
                :
                loadCompositionResult.tree;

            var discardedFiles =
                loadCompositionResult.tree
                .EnumerateFilesTransitive()
                .Where(originalBlob => filteredSourceTree.GetNodeAtPath(originalBlob.path) is not FileTree.FileNode)
                .ToImmutableArray();

            if (0 < discardedFiles.Length)
            {
                Console.WriteLine("Discarded " + discardedFiles.Length + " files from the input directory.");
            }

            var sourceFiles =
                FileTreeExtensions.ToFlatDictionaryWithPathComparer(filteredSourceTree);

            var elmJsonSourceDirectories =
                readElmJsonSourceDirectories() ?? [];

            bool filePathIsUnderElmJsonSourceDirectories(IReadOnlyList<string> filePath)
            {
                return
                    elmJsonSourceDirectories
                    .Any(sourceDir => filePath.Take(sourceDir.Count).SequenceEqual(sourceDir));
            }

            var compilationRootFilePath =
                sourceFiles.ContainsKey(ElmAppInterfaceConfig.Default.CompilationRootFilePath)
                ?
                ElmAppInterfaceConfig.Default.CompilationRootFilePath
                :
                sourceFiles
                .Where(c => c.Key[c.Key.Count - 1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
                .OrderBy(c => c.Key.Count)
                .OrderBy(c => filePathIsUnderElmJsonSourceDirectories(c.Key) ? 0 : 1)
                .FirstOrDefault()
                .Key;

            var interfaceConfig =
                ElmAppInterfaceConfig.Default with
                {
                    CompilationRootFilePath = compilationRootFilePath
                };

            var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
                sourceFiles: sourceFiles,
                workingDirectoryRelative: [],
                interfaceConfig: interfaceConfig);

            var compilationTimeSpentMilli = compilationStopwatch.ElapsedMilliseconds;

            report = report with { compilationTimeSpentMilli = (int)compilationTimeSpentMilli };

            return
                compilationResult
                .Unpack(
                    fromErr: compilationErrors =>
                    {
                        Console.WriteLine("\n" + ElmAppCompilation.CompileCompilationErrorsDisplayText(compilationErrors) + "\n");

                        return (report with { compilationErrors = compilationErrors, totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds }, null);
                    },
                    fromOk: compilationOk =>
                    {
                        var compiledAppFiles = compilationOk.Result.CompiledFiles;

                        var compiledTree = FileTree.FromSetOfFilesWithStringPath(compiledAppFiles);
                        var compiledComposition = FileTreeEncoding.Encode(compiledTree);
                        var compiledCompositionId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(compiledComposition).Span);

                        compilationStopwatch.Stop();

                        Console.WriteLine(
                            "\nCompilation completed in " + (int)compilationStopwatch.Elapsed.TotalSeconds +
                            " seconds, resulting in composition " + compiledCompositionId + ".");

                        return (report with
                        {
                            compilationIterationsReports = compilationOk.IterationsReports,
                            compiledCompositionId = compiledCompositionId,
                            totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds
                        }, compiledAppFiles);
                    });
        }
        catch (Exception e)
        {
            report = report with { compilationTimeSpentMilli = (int)compilationStopwatch.Elapsed.TotalMilliseconds };

            Console.WriteLine("Compilation failed with runtime exception: " + e);

            return
                (report with { compilationException = e.ToString(), totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds },
                null);
        }
    }

    private static Command CreateDescribeCommand()
    {
        var command = new Command("describe", "Describe the artifact at the given location. Valid locations can also be URLs into git repositories or paths in the local file system.");

        var sourcePathParameter = new Argument<string>("source-path");

        var listBlobsOption = new Option<bool>("--list-blobs");

        var compileZipArchiveOption = new Option<string?>("--compile-zip-archive")
        {
            Arity = ArgumentArity.ZeroOrOne
        };

        command.Add(sourcePathParameter);
        command.Add(listBlobsOption);
        command.Add(compileZipArchiveOption);

        command.SetAction((parseResult) =>
        {
            var sourcePath = parseResult.GetValue(sourcePathParameter);
            var listBlobs = parseResult.GetValue(listBlobsOption);
            var compileZipArchive = parseResult.GetValue(compileZipArchiveOption);

            var loadCompositionResult =
                LoadComposition.LoadFromPathResolvingNetworkDependencies(sourcePath)
                .LogToActions(Console.WriteLine)
                .Extract(error => throw new Exception("Failed to load from path '" + sourcePath + "': " + error));

            var composition = FileTreeEncoding.Encode(loadCompositionResult.tree);

            var compositionId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(composition).Span);

            Console.WriteLine("Loaded composition " + compositionId + " from '" + sourcePath + "'.");

            var compositionDescription =
                string.Join(
                    "\n",
                    FileTreeExtensions.DescribeFileTreeForHumans(
                        loadCompositionResult.tree,
                        listFiles: listBlobs,
                        extractFileName: sourcePath.Split('\\', '/').Last()));

            Console.WriteLine("Composition " + compositionId + " is " + compositionDescription);

            if (compileZipArchive != null)
            {
                var asZipArchive = ZipArchive.ZipArchiveFromFiles(
                    loadCompositionResult.tree.EnumerateFilesTransitive()
                    .Select(entry => (string.Join("/", entry.path), entry.fileContent)));

                var defaultFileName = compositionId + ".zip";

                var destinationPath = compileZipArchive.Length > 0 ? compileZipArchive : defaultFileName;

                if (Directory.Exists(destinationPath))
                    destinationPath = Path.Combine(destinationPath, defaultFileName);

                File.WriteAllBytes(destinationPath, asZipArchive);
                Console.WriteLine("Saved " + compositionId[..10] + " to " + destinationPath);
            }

            return 0;
        });

        return command;
    }

    private static Command CreateMakeCommand()
    {
        var command = new Command("make", "Compile Elm code into JavaScript, HTML, or other files.");

        var entryPointElmFileArgument = new Argument<string>("path-to-elm-file");

        var outputOption = new Option<string?>("--output");

        var inputDirectoryOption = new Option<string?>("--input-directory");

        var debugOption = new Option<bool>("--debug");

        var optimizeOption = new Option<bool>("--optimize");

        command.Add(entryPointElmFileArgument);
        command.Add(outputOption);
        command.Add(inputDirectoryOption);
        command.Add(debugOption);
        command.Add(optimizeOption);

        command.SetAction((parseResult) =>
        {
            var entryPoint = parseResult.GetValue(entryPointElmFileArgument);
            var output = parseResult.GetValue(outputOption);
            var inputDirectory = parseResult.GetValue(inputDirectoryOption);
            var debug = parseResult.GetValue(debugOption);
            var optimize = parseResult.GetValue(optimizeOption);

            var actualInputDirectory = inputDirectory ?? Environment.CurrentDirectory;
            var actualOutput = output ?? "make-default-output.html";

            return ElmMakeCommandExecute(
                inputDirectory: actualInputDirectory,
                entryPointElmFile: entryPoint,
                outputPathArgument: actualOutput,
                enableDebug: debug,
                enableOptimize: optimize);
        });

        return command;
    }

    private static int ElmMakeCommandExecute(
        string inputDirectory,
        string entryPointElmFile,
        string outputPathArgument,
        bool enableDebug,
        bool enableOptimize)
    {
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

        var elmMakeCommandOptions = new List<string>();
        if (enableDebug)
            elmMakeCommandOptions.Add("--debug");
        if (enableOptimize)
            elmMakeCommandOptions.Add("--optimize");

        var elmMakeCommandAppendix = string.Join(" ", elmMakeCommandOptions);

        int returnLoadingFromSourceError(string errorDetail)
        {
            DotNetConsoleWriteProblemCausingAbort("Failed to load from path '" + inputDirectory + "': " + errorDetail);
            return 10;
        }

        if (loadInputDirectoryResult.IsErrOrNull() is { } loadInputDirErr)
        {
            return returnLoadingFromSourceError(loadInputDirErr);
        }

        if (loadInputDirectoryResult.IsOkOrNullable() is not { } loadInputDirectoryOk)
        {
            throw new NotImplementedException(
                "Unexpected result type from loading input directory: " + loadInputDirectoryResult);
        }

        Result<string, LoadForMakeResult> loadForElmMake()
        {
            if (0 < loadInputDirectoryFailedFiles.Count)
            {
                var shownPaths =
                    loadInputDirectoryFailedFiles
                    .Take(3)
                    .Select(pathAndException =>
                    string.Join("/", pathAndException.Key) + " (" + pathAndException.Value.Message + ")")
                    .ToImmutableList();

                Console.WriteLine(
                    string.Join(
                        "\n",
                        "Ignored " + loadInputDirectoryFailedFiles.Count + " files due to IO exceptions:",
                        string.Join(
                            "\n",
                            [.. shownPaths,
                                            shownPaths.Count < loadInputDirectoryFailedFiles.Count ? "..." : null]
                            )));
            }

            var filteredSourceTree =
                loadInputDirectoryOk.origin is LoadCompositionOrigin.FromLocalFileSystem
                ?
                LoadFromLocalFilesystem.RemoveNoiseFromTree(
                    loadInputDirectoryOk.tree,
                    discardGitDirectory: true)
                :
                loadInputDirectoryOk.tree;

            var discardedFiles =
                loadInputDirectoryOk.tree
                .EnumerateFilesTransitive()
                .Where(originalBlob => filteredSourceTree.GetNodeAtPath(originalBlob.path) is not FileTree.FileNode)
                .ToImmutableArray();

            if (0 < discardedFiles.Length)
            {
                Console.WriteLine("Discarded " + discardedFiles.Length + " files from the input directory.");
            }

            if (filteredSourceTree.GetNodeAtPath(["elm.json"]) is not
                FileTree.FileNode elmJsonFile)
            {
                return "Did not find elm.json file in that directory.";
            }

            var elmJsonFileParsed =
                System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Bytes.Span);

            if (elmJsonFileParsed is null)
            {
                return "Failed to parse elm.json file.";
            }

            var elmJsonSourceDirectories =
                elmJsonFileParsed.ParsedSourceDirectories.ToImmutableList();

            var sourceDirectoriesNotInInputDirectory =
                elmJsonSourceDirectories
                .Where(relativeSourceDir => 0 < relativeSourceDir.ParentLevel)
                .ToImmutableList();

            var pathToElmFile = entryPointElmFile;

            if (string.IsNullOrEmpty(pathToElmFile))
            {
                return "The path to the entry point Elm file is empty.";
            }

            if (pathToElmFile.StartsWith("./"))
                pathToElmFile = pathToElmFile[2..];

            if (sourceDirectoriesNotInInputDirectory.IsEmpty)
            {
                return
                    new LoadForMakeResult(
                        filteredSourceTree,
                        [],
                        pathToElmFile.Replace('\\', '/').Split('/'));
            }

            if (loadInputDirectoryOk.origin is not LoadCompositionOrigin.FromLocalFileSystem)
            {
                return
                    "Failed to work with elm.json file containing directory which is not contained in input directory: This configuration is only supported when loading from a local file system";
            }

            string AbsoluteSourceDirectoryFromRelative(ElmJsonStructure.RelativeDirectory relDir)
            {
                var path = inputDirectory;

                if (!Path.IsPathFullyQualified(path))
                {
                    path = Path.GetFullPath(path);
                }

                for (var i = 0; i < relDir.ParentLevel; ++i)
                {
                    path =
                        Path.GetDirectoryName(path.TrimEnd('/', '\\'))
                        ??
                        throw new Exception("Failed to compute parent directory for " + path);
                }

                return
                    Path.Combine(path, string.Join('/', relDir.Subdirectories));
            }

            var outerSourceDirectoriesAbsolute =
                sourceDirectoriesNotInInputDirectory
                .Select(AbsoluteSourceDirectoryFromRelative)
                .ToImmutableList();

            var maxParentLevel =
                sourceDirectoriesNotInInputDirectory.Max(sd => sd.ParentLevel);

            var commonParentDirectory =
                AbsoluteSourceDirectoryFromRelative(
                    new ElmJsonStructure.RelativeDirectory(
                        ParentLevel: maxParentLevel,
                        Subdirectories: []));

            IReadOnlyList<string> PathRelativeToCommonParentFromAbsolute(string absolutePath) =>
                absolutePath[commonParentDirectory.Length..].Replace('\\', '/').Trim('/').Split('/');

            var inputDirectoryAbsolute = Path.GetFullPath(inputDirectory);

            var workingDirectoryRelative =
                PathRelativeToCommonParentFromAbsolute(inputDirectoryAbsolute);

            var pathToElmFileAbsolute = Path.GetFullPath(pathToElmFile);

            var pathToFileWithElmEntryPoint =
                PathRelativeToCommonParentFromAbsolute(pathToElmFileAbsolute);

            return
                outerSourceDirectoriesAbsolute
                    .Select(outerSourceDirectory =>
                    {
                        return
                            LoadComposition.LoadFromPathResolvingNetworkDependencies(outerSourceDirectory)
                                .LogToActions(Console.WriteLine)
                                .Map(outerSourceDirLoadOk =>
                                    (outerSourceDirLoadOk.tree,
                                        relativePath: PathRelativeToCommonParentFromAbsolute(outerSourceDirectory)));
                    })
                    .ListCombine()
                    .Map(outerSourceDirectories =>
                    {
                        var combinedTree =
                            outerSourceDirectories
                                .Aggregate(
                                    seed:
                                    FileTree.EmptyTree
                                        .SetNodeAtPathSorted(workingDirectoryRelative, filteredSourceTree),
                                    func:
                                    (aggregate, nextSourceDir) =>
                                        aggregate.SetNodeAtPathSorted(nextSourceDir.relativePath,
                                            nextSourceDir.tree));
                        return
                            new LoadForMakeResult(
                                SourceFiles: combinedTree,
                                workingDirectoryRelative,
                                pathToFileWithElmEntryPoint);
                    });
        }

        var loadSourceFilesResult = loadForElmMake();

        if (loadSourceFilesResult.IsErrOrNull() is { } loadSourceFilesErr)
        {
            return returnLoadingFromSourceError(loadSourceFilesErr);
        }

        if (loadSourceFilesResult.IsOkOrNull() is not { } loadSourceFilesOk)
        {
            throw new NotImplementedException(
                "Unexpected result type from loading source files: " + loadSourceFilesResult);
        }

        var inputHash = Convert.ToHexStringLower(PineValueHashTree.ComputeHashSorted(loadSourceFilesOk.SourceFiles).Span);

        Console.WriteLine(
            "Loaded " + inputHash[..10] + " as input: " +
            string.Join(
                "\n",
                FileTreeExtensions.DescribeFileTreeForHumans(
                    loadSourceFilesOk.SourceFiles,
                    listFiles: false,
                    extractFileName: null)));

        var makeResult =
            Make(
                sourceFiles: FileTreeExtensions.ToFlatDictionaryWithPathComparer(loadSourceFilesOk.SourceFiles),
                workingDirectoryRelative: loadSourceFilesOk.WorkingDirectoryRelative,
                pathToFileWithElmEntryPoint: loadSourceFilesOk.PathToFileWithElmEntryPoint,
                outputFileName: Path.GetFileName(outputPathArgument),
                elmMakeCommandAppendix: elmMakeCommandAppendix);

        if (makeResult.IsErrOrNull() is { } makeErr)
        {
            DotNetConsoleWriteProblemCausingAbort(
                "Failed to make " + entryPointElmFile + ":\n" + makeErr);

            return 20;
        }

        if (makeResult.IsOkOrNull() is not { } makeOk)
        {
            throw new NotImplementedException(
                "Unexpected make result type: " + makeResult);
        }

        ReadOnlyMemory<byte> ComputeOutputFileContent()
        {
            if (makeOk.ProducedFiles is FileTree.FileNode blobNode)
            {
                Console.WriteLine(
                    "Make command produced a single blob with " +
                    CommandLineInterface.FormatIntegerForDisplay(blobNode.Bytes.Length) + " bytes.");

                return blobNode.Bytes;
            }

            if (makeOk.ProducedFiles is FileTree.DirectoryNode treeNode)
            {
                var blobs =
                    treeNode.EnumerateFilesTransitive()
                    .Select(entry => (string.Join("/", entry.path), entry.fileContent))
                    .ToImmutableList();

                Console.WriteLine(
                    "Make command produced tree node with " +
                    blobs.Count + " blobs (" +
                    CommandLineInterface.FormatIntegerForDisplay(blobs.Sum(entry => entry.fileContent.Length)) +
                    " aggregate bytes). Packaging these into zip archive...");

                var zipArchive = ZipArchive.ZipArchiveFromFiles(blobs);

                return zipArchive;
            }

            throw new NotImplementedException(
                "Unexpected produced files type: " + makeOk.ProducedFiles);
        }

        var outputFileContent = ComputeOutputFileContent();

        var outputPath = Path.GetFullPath(outputPathArgument);

        var outputDirectory = Path.GetDirectoryName(outputPath);

        if (outputDirectory is not null)
            Directory.CreateDirectory(outputDirectory);

        File.WriteAllBytes(outputPath, outputFileContent.Span);
        Console.WriteLine("Saved the output to " + outputPath);

        return 0;
    }

    private record LoadForMakeResult(
        FileTree SourceFiles,
        IReadOnlyList<string> WorkingDirectoryRelative,
        IReadOnlyList<string> PathToFileWithElmEntryPoint);

    /// <summary>
    /// Compiles Elm code as offered with the 'make' command on the CLI.
    /// </summary>
    public static Result<string, Elm019Binaries.ElmMakeOk> Make(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> sourceFiles,
        IReadOnlyList<string>? workingDirectoryRelative,
        IReadOnlyList<string> pathToFileWithElmEntryPoint,
        string outputFileName,
        string? elmMakeCommandAppendix)
    {
        workingDirectoryRelative ??= [];

        IReadOnlyList<string> pathToFileWithElmEntryPointFromWorkingDir =
            [.. pathToFileWithElmEntryPoint.Skip(workingDirectoryRelative.Count)];

        var loweringResult =
            ElmAppCompilation.AsCompletelyLoweredElmApp(
                sourceFiles: sourceFiles.ToImmutableDictionary(),
                workingDirectoryRelative: workingDirectoryRelative,
                interfaceConfig: new ElmAppInterfaceConfig(CompilationRootFilePath: pathToFileWithElmEntryPoint));

        var entryPointSourceFile =
            sourceFiles[pathToFileWithElmEntryPoint];

        var entryPointSourceFileText =
            Encoding.UTF8.GetString(entryPointSourceFile.Span);

        var entryPointModuleNameResult =
            ElmModule.ParseModuleName(entryPointSourceFileText);

        if (entryPointModuleNameResult.IsErrOrNull() is { } entryPointModuleNameErr)
        {
            return
                "Failed to parse module name from entry point file: " + entryPointModuleNameErr;
        }

        if (entryPointModuleNameResult.IsOkOrNull() is not { } entryPointModuleNameOk)
        {
            throw new Exception(
                "Unexpected entry point module name result type: " + entryPointModuleNameResult);
        }

        if (loweringResult.IsErrOrNull() is { } loweringErr)
        {
            return
                "Failed lowering Elm code with " + loweringErr.Count + " error(s):\n" +
                ElmAppCompilation.CompileCompilationErrorsDisplayText(loweringErr);
        }

        if (loweringResult.IsOkOrNull() is not { } loweringOk)
        {
            throw new Exception("Unexpected lowering result type: " + loweringResult);
        }

        var sourceFilesAfterLowering = loweringOk.Result.CompiledFiles;

        Result<string, Elm019Binaries.ElmMakeOk> continueWithClassicEntryPoint()
        {
            return
                Elm019Binaries.ElmMake(
                    sourceFilesAfterLowering,
                    workingDirectoryRelative: workingDirectoryRelative,
                    pathToFileWithElmEntryPoint: pathToFileWithElmEntryPointFromWorkingDir,
                    outputFileName: outputFileName.Replace('\\', '/').Split('/').Last(),
                    elmMakeCommandAppendix: elmMakeCommandAppendix);
        }

        Result<string, Elm019Binaries.ElmMakeOk> continueWithBlobEntryPoint()
        {
            var sourceFilesWithMergedPackages =
                ElmAppDependencyResolution.AppCompilationUnitsForEntryPoint(
                    FileTree.FromSetOfFilesWithStringPath(sourceFilesAfterLowering),
                    entryPointFilePath: pathToFileWithElmEntryPoint);

            var elmCompilerFromBundle =
                BundledElmEnvironments.BundledElmCompilerCompiledEnvValue()
                ??
                throw new Exception("Failed to load Elm compiler from bundle.");

            var elmCompiler =
                ElmCompilerInElm.ElmCompilerFromEnvValue(elmCompilerFromBundle)
                .Extract(err => throw new Exception(err));

            var pineVMCache = new InvocationCache();

            var pineVM =
                SetupVM.Create(evalCache: pineVMCache);

            var parseCache = new Pine.Core.CodeAnalysis.PineVMParseCache();

            var elmCompilerCache = new ElmCompilerCache();

            var compileResult =
                InteractiveSessionPine.CompileInteractiveEnvironment(
                    appCodeTree: sourceFilesWithMergedPackages.files,
                    overrideSkipLowering: true,
                    entryPointsFilePaths: [pathToFileWithElmEntryPoint],
                    skipFilteringForSourceDirs: false,
                    elmCompiler: elmCompiler);

            if (compileResult.IsErrOrNull() is { } compileErr)
            {
                return
                    "Failed to compile Elm interactive env: " + compileErr;
            }

            if (compileResult.IsOkOrNull() is not { } compileOk)
            {
                throw new Exception("Unexpected compile result type: " + compileResult);
            }

            var parseFromEnvResult =
                Pine.Core.CodeAnalysis.ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                    interactiveEnvironment: compileOk,
                    moduleName: string.Join(".", entryPointModuleNameOk.ToArray()),
                    "blobMain",
                    parseCache);

            {
                if (parseFromEnvResult.IsErrOrNull() is { } parseErr)
                {
                    return "Failed to parse Elm module: " + parseErr;
                }
            }

            if (parseFromEnvResult.IsOkOrNullable() is not { } parseFromEnvOk)
            {
                throw new Exception("Unexpected parse result type: " + parseFromEnvResult);
            }

            var elmBytesValue = parseFromEnvOk.declValue;

            if (parseFromEnvOk.functionRecord.ParameterCount is 1)
            {
                /*
                 * Support alternative form as following to avoid eager (expensive) evaluation in the compiler:
                 * 
                 * blobMain : () -> Bytes.Bytes
                 * */

                var applyMainResult =
                    Pine.Core.CodeAnalysis.ElmInteractiveEnvironment.ApplyFunction(
                        pineVM,
                        functionRecord: parseFromEnvOk.functionRecord,
                        arguments: [PineValue.EmptyList]);

                if (applyMainResult.IsErrOrNull() is { } applyErr)
                {
                    return "Failed to apply Elm function: " + applyErr;
                }

                if (applyMainResult.IsOkOrNull() is not { } applyOk)
                {
                    throw new Exception("Unexpected apply result type: " + applyMainResult);
                }

                elmBytesValue = applyOk;
            }

            var parseDeclResult =
                ElmValueEncoding.PineValueAsElmValue(elmBytesValue, null, null);

            if (parseDeclResult.IsErrOrNull() is { } parseDeclErr)
            {
                return "Failed to parse Elm value: " + parseDeclErr;
            }

            if (parseDeclResult.IsOkOrNull() is not { } parseDeclOk)
            {
                throw new Exception("Unexpected parse result type: " + parseDeclResult);
            }

            return TryParseMakeOutput(parseDeclOk);
        }

        if (loweringOk.Result.RootModuleEntryPointKind.IsErrOrNull() is { } rootModuleEntryPointKindErr)
        {
            return "Failed to get entry point main declaration: " + rootModuleEntryPointKindErr;
        }

        if (loweringOk.Result.RootModuleEntryPointKind.IsOkOrNull() is not { } rootModuleEntryPointKind)
        {
            throw new NotImplementedException(
                "Unexpected root module entry point result type: " + loweringOk.Result.RootModuleEntryPointKind);
        }

        return
            rootModuleEntryPointKind switch
            {
                CompilerSerialInterface.ElmMakeEntryPointKind.ClassicMakeEntryPoint =>
                    continueWithClassicEntryPoint(),

                CompilerSerialInterface.ElmMakeEntryPointKind.BlobMakeEntryPoint blob =>
                    continueWithBlobEntryPoint(),

                _ =>
                throw new NotImplementedException(
                    "Unexpected root module entry point kind: " + rootModuleEntryPointKind),
            };
    }

    private static Result<string, Elm019Binaries.ElmMakeOk> TryParseMakeOutput(ElmValue elmValue)
    {
        if (elmValue is ElmValue.ElmBytes bytesValue)
        {
            return new Elm019Binaries.ElmMakeOk(ProducedFiles: FileTree.File(bytesValue.Value));
        }

        if (elmValue is ElmValue.ElmTag)
        {
            var asTreeResult = ParseAsFileTree(elmValue);

            if (asTreeResult.IsErrOrNull() is { } asTreeErr)
            {
                return "Failed to parse Elm tag value as file tree: " + asTreeErr;
            }

            if (asTreeResult.IsOkOrNull() is not { } asTreeOk)
            {
                throw new NotImplementedException("Unexpected result type: " + asTreeResult);
            }

            return new Elm019Binaries.ElmMakeOk(ProducedFiles: asTreeOk);
        }

        return "Unexpected Elm value type: " + elmValue;
    }

    private static Result<string, FileTree> ParseAsFileTree(ElmValue elmValue)
    {
        /*
         * Type declaration on Elm side looks like this:
         * 
        type FileTreeNode blobStructure
            = BlobNode blobStructure
            | TreeNode (TreeNodeStructure blobStructure)


        type alias TreeNodeStructure blobStructure =
            List (TreeNodeEntryStructure blobStructure)


        type alias TreeNodeEntryStructure blobStructure =
            ( String, FileTreeNode blobStructure )

         * */

        if (elmValue is not ElmValue.ElmTag elmTag)
        {
            return "Expected Elm tag value, but got: " + elmValue;
        }

        if (elmTag.TagName.StartsWith("Blob", StringComparison.OrdinalIgnoreCase))
        {
            if (elmTag.Arguments.Count is not 1)
            {
                return "Expected Elm tag with one argument, but got: " + elmTag.Arguments.Count;
            }

            var blob = elmTag.Arguments[0];

            if (elmTag.Arguments[0] is not ElmValue.ElmBytes bytes)
            {
                return "Expected Elm bytes value, but got: " + blob;
            }

            return FileTree.File(bytes.Value);
        }

        if (elmTag.TagName.StartsWith("Tree", StringComparison.OrdinalIgnoreCase))
        {
            if (elmTag.Arguments.Count is not 1)
            {
                return "Expected Elm tag with one argument, but got: " + elmTag.Arguments.Count;
            }

            if (elmTag.Arguments[0] is not ElmValue.ElmList elmList)
            {
                return "Expected Elm list value, but got: " + elmTag.Arguments[0];
            }

            var children = new (string name, FileTree item)[elmList.Items.Count];

            for (var i = 0; i < elmList.Items.Count; ++i)
            {
                var child = elmList.Items[i];

                if (child is not ElmValue.ElmList tuple)
                {
                    return "Child [" + i + "] is not a tuple: " + child;
                }

                if (tuple.Items.Count is not 2)
                {
                    return "Child [" + i + "]: Expected Elm tuple with two elements, but got: " + tuple.Items.Count;
                }

                if (tuple.Items[0] is not ElmValue.ElmString name)
                {
                    return "Child [" + i + "]: Expected Elm string value, but got: " + tuple.Items[0];
                }

                var childTreeResult = ParseAsFileTree(tuple.Items[1]);

                if (childTreeResult.IsErrOrNull() is { } childTreeErr)
                {
                    return "Child [" + i + "] (" + name.Value + "): Failed to parse Elm tag value as file tree: " + childTreeErr;
                }

                if (childTreeResult.IsOkOrNull() is not { } childTreeOk)
                {
                    throw new NotImplementedException("Unexpected result type: " + childTreeResult);
                }

                children[i] = (name.Value, childTreeOk);
            }

            var treeNode = FileTree.NonSortedDirectory(children);

            return treeNode;
        }

        return "Unexpected Elm tag value type: " + elmTag;
    }

    private static Command CreateRunCacheServerCommand()
    {
        var command = new Command("run-cache-server", "Run an HTTP server to cache popular parts of git repositories.")
        {
            Hidden = true  // Equivalent to ShowInHelpText = false
        };

        var gitCloneUrlPrefixOption = new Option<string[]>("--git-clone-prefix")
        {
            AllowMultipleArgumentsPerToken = true,
            Arity = ArgumentArity.ZeroOrMore
        };

        var urlOption = new Option<string[]>("--url")
        {
            AllowMultipleArgumentsPerToken = true,
            Arity = ArgumentArity.ZeroOrMore
        };

        var fileCacheDirectoryOption = new Option<string>("--file-cache-directory");

        command.Add(gitCloneUrlPrefixOption);
        command.Add(urlOption);
        command.Add(fileCacheDirectoryOption);

        command.SetAction((parseResult) =>
        {
            var urls = parseResult.GetValue(urlOption);
            var gitCloneUrlPrefixes = parseResult.GetValue(gitCloneUrlPrefixOption);
            var fileCacheDirectory = parseResult.GetValue(fileCacheDirectoryOption);

            Console.WriteLine("Starting HTTP server with git cache...");

            var serverTask = GitPartialForCommitServer.Run(
                urls: urls!,
                gitCloneUrlPrefixes: gitCloneUrlPrefixes!,
                fileCacheDirectory: fileCacheDirectory);

            Console.WriteLine("Completed starting HTTP server with git cache at '" + string.Join(", ", urls) + "'.");

            serverTask.Wait();

            return 0;
        });

        return command;
    }

    private static Command CreateRunFileServerCommand()
    {
        var command = new Command("run-file-server", "Run an HTTP server that provides a REST API for file operations.");

        var storeOption = new Option<string?>("--store");

        var portOption = new Option<int?>("--port");

        var authPasswordOption = new Option<string?>("--auth-password");

        command.Add(storeOption);
        command.Add(portOption);
        command.Add(authPasswordOption);

        command.SetAction((parseResult) =>
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
                var retryOptions = new FileStoreFromSystemIOFile.FileStoreRetryOptions(
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

        builder.WebHost.UseKestrel(options =>
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
        Console.CancelKeyPress += (_, e) =>
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

    private static (Argument<string> siteArgument, Option<string?> passwordOption, Func<ParseResult, string?, string?> getPassword)
        ProcessSiteArgumentAndPasswordOption(Command cmd, string? siteName = null)
    {
        siteName ??= "site";

        var siteArgument = new Argument<string>("process-site");

        var sitePasswordOption = new Option<string?>(
            name: "--" + siteName + "-password")
        {
            Description = "Password for " + siteName + "."
        };

        cmd.Add(siteArgument);
        cmd.Add(sitePasswordOption);

        Func<ParseResult, string?, string?> getPassword = (parseResult, site) =>
            site == null ? null : (parseResult.GetValue(sitePasswordOption) ?? UserSecrets.LoadPasswordForSite(site));

        return (siteArgument, sitePasswordOption, getPassword);
    }

    private static (Option<string?> elmEngineOption, Func<ElmEngineType> parseElmEngineTypeFromOption)
        CreateElmEngineOption(
        DynamicPGOShare? dynamicPGOShare,
        string? defaultFromEnvironmentVariablePrefix,
        Func<ElmEngineTypeCLI?, ElmEngineTypeCLI> defaultEngineConsideringEnvironmentVariable)
    {
        var defaultEngineFromEnvironmentVariable =
            defaultFromEnvironmentVariablePrefix switch
            {
                { } variablePrefix => ElmEngineFromEnvironmentVariableWithPrefix(variablePrefix),
                null => null
            };

        var defaultEngine = defaultEngineConsideringEnvironmentVariable(defaultEngineFromEnvironmentVariable);

        var elmEngineOption = new Option<string?>(
            name: "--elm-engine" + string.Join(", ", Enum.GetNames<ElmEngineTypeCLI>()) + "). Defaults to " + defaultEngine);

        ElmEngineType parseElmEngineTypeFromOption()
        {
            var cliName =
                defaultEngine;  // Simplified for now - would need to get actual option value

            return
                ParseElmEngineType(
                    dynamicPGOShare,
                    cliName);
        }

        return (elmEngineOption, parseElmEngineTypeFromOption);
    }

    private static (Option<string?> elmCompilerOption, Func<IConsole, FileTree> loadElmCompilerFromOption)
        CreateElmCompilerOption()
    {
        var defaultCompiler = ElmCompilerInElm.CompilerSourceFilesDefault.Value;

        var elmCompilerOption = new Option<string?>("--elm-compiler");

        FileTree parseElmCompilerFromOption(IConsole console)
        {
            // Simplified - would need to handle option value
            return defaultCompiler;
        }

        return (elmCompilerOption, parseElmCompilerFromOption);
    }

    private static Command CreateInteractiveCommand(DynamicPGOShare? dynamicPGOShare)
    {
        var command = new Command("interactive", "Enter environment for interactive exploration and composition of Elm programs.")
        {
            Aliases = { "repl" }
        };

        // Options
        var contextAppOption = new Option<string[]>("--context-app")
        {
            Description = "Path to an app to use as context. The Elm modules from this app will be available in the interactive environment.",
            AllowMultipleArgumentsPerToken = true,
            Arity = ArgumentArity.ZeroOrMore
        };

        var contextAppModuleNameFilterOption = new Option<string?>("--context-app-module-name-filter")
        {
            Description = "Filter on module names to apply on modules loaded via the '--context-app' option."
        };

        var initStepsOption = new Option<string?>("--init-steps")
        {
            Description = "Path to a list of submissions to start the session with."
        };

        var enableInspectionOption = new Option<bool>("--enable-inspection")
        {
            Description = "Display additional information to inspect the implementation."
        };

        var submitOption = new Option<string[]>("--submit")
        {
            Description = "Option to submit a string as if entered during the interactive session.",
            AllowMultipleArgumentsPerToken = true,
            Arity = ArgumentArity.ZeroOrMore
        };

        var saveToFileOption = new Option<string?>("--save-to-file")
        {
            Description = "Path to a file to save the session state to, after compiling context app and initial submissions."
        };

        var (elmCompilerOption, loadElmCompilerFromOption) = CreateElmCompilerOption(command);
        var (elmEngineOption, parseElmEngineTypeFromOption) = CreateElmEngineOption(
            dynamicPGOShare: null,
            command,
            defaultFromEnvironmentVariablePrefix: "interactive",
            defaultEngineConsideringEnvironmentVariable: fromEnv => fromEnv ?? IInteractiveSession.DefaultImplementation);

        command.Add(contextAppOption);
        command.Add(contextAppModuleNameFilterOption);
        command.Add(initStepsOption);
        command.Add(enableInspectionOption);
        command.Add(submitOption);
        command.Add(saveToFileOption);
        command.Add(elmCompilerOption);
        command.Add(elmEngineOption);

        // Test subcommand
        var testCommand = new Command("test", "Test the interactive automatically with given scenarios and reports timings.");

        var scenarioOption = new Option<string[]>("--scenario")
        {
            Description = "Test an interactive scenario from the given path. The scenario specifies the submissions and can also specify expectations.",
            AllowMultipleArgumentsPerToken = true,
            Arity = ArgumentArity.ZeroOrMore
        };

        var scenariosOption = new Option<string[]>("--scenarios")
        {
            Description = "Test a list of interactive scenarios from the given directory. Each scenario specifies the submissions and can also specify expectations.",
            AllowMultipleArgumentsPerToken = true,
            Arity = ArgumentArity.ZeroOrMore
        };

        testCommand.Add(scenarioOption);
        testCommand.Add(scenariosOption);

        testCommand.SetAction((parseResult) =>
        {
            var scenarioSources = parseResult.GetValue(scenarioOption) ?? [];
            var scenariosSources = parseResult.GetValue(scenariosOption) ?? [];

            var console = (IConsole)StaticConsole.Instance;

            console.WriteLine("Got " + scenarioSources.Length + " source(s) for an individual scenario to load...");
            console.WriteLine("Got " + scenariosSources.Length + " source(s) for a directory of scenarios to load...");

            var scenarioLoadResults =
                scenarioSources
                .ToImmutableDictionary(
                    scenarioSource => scenarioSource!,
                    scenarioSource => LoadComposition.LoadFromPathResolvingNetworkDependencies(scenarioSource!).LogToList());

            var scenariosLoadResults =
                scenariosSources
                .ToImmutableDictionary(
                    scenariosSource => scenariosSource!,
                    scenariosSource => LoadComposition.LoadFromPathResolvingNetworkDependencies(scenariosSource!).LogToList());

            var failedLoads =
                scenarioLoadResults.Concat(scenariosLoadResults)
                .Where(r => !r.Value.result.IsOk())
                .ToImmutableList();

            if (!failedLoads.IsEmpty)
            {
                var failedLoad = failedLoads.First();

                console.WriteLine(
                    string.Join(
                        "\n",
                            "Failed to load from " + failedLoad.Key + ":",
                            string.Join("\n", failedLoad.Value.log),
                            failedLoad.Value.result.Unpack(fromErr: error => error, fromOk: _ => throw new NotImplementedException())),
                    color: IConsole.TextColor.Red);

                return 1;
            }

            var namedDistinctScenarios =
                scenarioLoadResults
                .Select(scenarioLoadResult =>
                (name: scenarioLoadResult.Key.Split('/', '\\').Last(),
                component: scenarioLoadResult.Value.result.Extract(error => throw new Exception(error)).tree))
                .Concat(scenariosLoadResults.SelectMany(scenariosComposition =>
                {
                    var asTree =
                    scenariosComposition.Value.result.Extract(error => throw new Exception(error)).tree switch
                    {
                        FileTree.DirectoryNode tree => tree,
                        _ => null
                    };

                    if (asTree is null)
                        return ImmutableList<(string, FileTree)>.Empty;

                    return
                    asTree.Items
                    .Where(entry => entry.component is FileTree.DirectoryNode scenarioTree);
                }))
                .Select(loadedScenario =>
                {
                    var asComposition = FileTreeEncoding.Encode(loadedScenario.component);

                    var hashBase16 = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(asComposition).Span);

                    return new
                    {
                        loadedScenario,
                        asComposition,
                        hashBase16
                    };
                })
                .DistinctBy(loadedScenario => loadedScenario.hashBase16)
                .ToImmutableDictionary(
                    keySelector: scenario => scenario.loadedScenario.name + "-" + scenario.hashBase16[..10],
                    elementSelector: scenario => scenario);

            var compileElmProgramCodeFiles = loadElmCompilerFromOption(parseResult, console);
            var elmEngineType = parseElmEngineTypeFromOption(parseResult);

            var aggregateCompositionTree =
                FileTree.SortedDirectory(
                    [.. namedDistinctScenarios.Select(scenario => (scenario.Key, scenario.Value.loadedScenario.component))]);

            var parsedScenarios =
            TestElmInteractive.ParseElmInteractiveScenarios(
                aggregateCompositionTree,
                console);

            IInteractiveSession newInteractiveSessionFromAppCode(FileTree? appCodeTree)
            {
                return IInteractiveSession.Create(
                    compilerSourceFiles: compileElmProgramCodeFiles,
                    appCodeTree: appCodeTree,
                    elmEngineType);
            }

            var interactiveConfig = new InteractiveSessionConfig(
                CompilerId:
                Convert.ToHexStringLower(PineValueHashTree.ComputeHashSorted(compileElmProgramCodeFiles).Span)[..8],
                newInteractiveSessionFromAppCode);

            {
                var warmupStopwatch = System.Diagnostics.Stopwatch.StartNew();

                using var session = interactiveConfig.SessionFromAppCode(null);

                session.Submit("1 + 3");

                console.WriteLine(
                    "Warmup completed in " +
                    warmupStopwatch.Elapsed.TotalSeconds.ToString("0.##") + " seconds.");
            }

            var scenariosResults =
            TestElmInteractive.TestElmInteractiveScenarios(
                parsedScenarios,
                interactiveConfig,
                console: console,
                asyncLogDelegate: null);

            return 0;
        });

        command.Add(testCommand);

        // Main interactive command handler
        command.SetAction((parseResult) =>
        {
            var contextAppPaths = parseResult.GetValue(contextAppOption) ?? [];
            var contextAppModuleNameFilterPattern = parseResult.GetValue(contextAppModuleNameFilterOption);
            var initStepsPath = parseResult.GetValue(initStepsOption);
            var enableInspection = parseResult.GetValue(enableInspectionOption);
            var submitsFromOption = parseResult.GetValue(submitOption) ?? [];
            var saveToFile = parseResult.GetValue(saveToFileOption);

            var console = (IConsole)StaticConsole.Instance;

            var compileElmProgramCodeFiles = loadElmCompilerFromOption(parseResult, console);
            var elmEngineType = parseElmEngineTypeFromOption(parseResult);

            console.WriteLine(
                "---- Elm Interactive v" + AppVersionId + " ----");

            FileTree loadContextAppCodeTreeFromPath(string contextAppPath)
            {
                return
                LoadComposition.LoadFromPathResolvingNetworkDependencies(contextAppPath)
                .LogToActions(console.WriteLine)
                .Map(loaded => loaded.tree)
                .Unpack(
                    fromErr: error => throw new Exception("Failed to load from path '" + contextAppPath + "': " + error),
                    fromOk: tree =>
                    {
                        if (!tree.EnumerateFilesTransitive().Take(1).Any())
                            throw new Exception("Found no files under context app path '" + contextAppPath + "'.");

                        return tree;
                    });
            }

            var contextAppCodeTreeBeforeFilter =
            contextAppPaths.Length == 0
            ?
            null
            :
            FileTreeExtensions.Union(contextAppPaths.Select(loadContextAppCodeTreeFromPath!));

            var contextAppModuleNameFilterIncluded =
                contextAppModuleNameFilterPattern is null
                ?
                []
                :
                contextAppModuleNameFilterPattern
                .Split(',')
                .Select(moduleName => moduleName.ToLowerInvariant())
                .ToImmutableHashSet();

            bool contextAppModuleNameFilter(IReadOnlyList<string> moduleName)
            {
                if (contextAppModuleNameFilterPattern is null)
                    return true;

                var flatModuleName = string.Join('.', moduleName).ToLowerInvariant();

                return contextAppModuleNameFilterIncluded.Contains(flatModuleName);
            }

            var contextAppCodeTree =
            contextAppCodeTreeBeforeFilter is null
            ?
            null
            :
            ElmModule.FilterAppCodeTreeForRootModulesAndDependencies(
                contextAppCodeTreeBeforeFilter,
                moduleNameIsRootModule: contextAppModuleNameFilter);

            var initStepsSubmission =
            initStepsPath switch
            {
                null =>
                [],

                not null =>
                LoadComposition.LoadFromPathResolvingNetworkDependencies(initStepsPath)
                .LogToActions(console.WriteLine)
                .Map(loaded => loaded.tree)
                .Unpack(
                    fromErr: error => throw new Exception("Failed to load from path '" + initStepsPath + "': " + error),
                    fromOk: treeNode =>
                    {
                        if (!treeNode.EnumerateFilesTransitive().Take(1).Any())
                            throw new Exception("Found no files under context app path '" + initStepsPath + "'.");

                        return
                        treeNode
                        .Map(
                            fromFile: _ => throw new Exception("Unexpected blob"),
                            fromDirectory: tree =>
                            tree.Select(stepDirectory =>
                            TestElmInteractive.ParseScenarioStep(stepDirectory.itemValue)
                            .Extract(fromErr: error => throw new Exception(error)).Submission))
                            .ToImmutableList();
                    })
            };

            using var interactiveSession = IInteractiveSession.Create(
                compilerSourceFiles: compileElmProgramCodeFiles,
                appCodeTree: contextAppCodeTree,
                engineType: elmEngineType);

            string? processSubmission(string submission)
            {
                if (!(0 < submission?.Trim()?.Length))
                    return null;

                var evalStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var evalResult = interactiveSession.Submit(submission);

                evalStopwatch.Stop();

                return
                evalResult
                .Unpack(
                    fromErr: error =>
                    {
                        console.WriteLine("Failed to evaluate: " + error);
                        return submission;
                    },
                    fromOk: evalOk =>
                    {
                        if (enableInspection)
                        {
                            console.WriteLine(
                                "Processing this submission took " +
                                CommandLineInterface.FormatIntegerForDisplay(evalStopwatch.ElapsedMilliseconds) + " ms.");

                            console.WriteLine(
                                "Inspection log has " + (evalOk.InspectionLog?.Count ?? 0) + " entries:\n" +
                                string.Join("\n", evalOk.InspectionLog.EmptyIfNull()));
                        }

                        console.WriteLine(evalOk.InteractiveResponse.DisplayText);

                        return submission;
                    });
            }

            var promptPrefix = "> ";

            var allSubmissionsFromArguments =
            initStepsSubmission
            .Concat(submitsFromOption.EmptyIfNull()).WhereNotNull()
            .ToImmutableList();

            if (0 < allSubmissionsFromArguments.Count)
            {
                console.WriteLine(allSubmissionsFromArguments.Count + " initial submission(s) from arguments in total...");
            }

            foreach (var submission in allSubmissionsFromArguments)
            {
                console.WriteLine(promptPrefix + submission);

                processSubmission(submission);
            }

            if (saveToFile is not null)
            {
                console.WriteLine("Got option to save session state to " + saveToFile + "...");

                if (interactiveSession is not InteractiveSessionPine pineSession)
                {
                    console.WriteLine("Cannot save session state for this engine type: " + interactiveSession.GetType().Name);
                }
                else
                {
                    var sessionState = pineSession.CurrentEnvironmentValue();

                    var (environmentJson, _) =
                    ElmInteractive.ElmInteractive.FromPineValueBuildingDictionary(
                        sessionState,
                        ElmInteractive.ElmInteractive.CompilationCache.Empty);

                    var environmentJsonString =
                        System.Text.Json.JsonSerializer.Serialize(environmentJson.json,
                            options: ElmInteractive.ElmInteractive.compilerInterfaceJsonSerializerOptions);

                    File.WriteAllText(saveToFile, environmentJsonString);

                    console.WriteLine(
                        "Saved session state to " + saveToFile + ", as JSON with total length of " +
                        environmentJsonString.Length);
                }

                return 0;
            }

            ReadLine.HistoryEnabled = true;

            while (true)
            {
                var submission = ReadLine.Read(promptPrefix);

                processSubmission(submission);
            }
        });

        return command;
    }

    private static (Option<string?> elmEngineOption, Func<ParseResult, ElmEngineType> parseElmEngineTypeFromOption)
        CreateElmEngineOption(
        DynamicPGOShare? dynamicPGOShare,
        Command cmd,
        string? defaultFromEnvironmentVariablePrefix,
        Func<ElmEngineTypeCLI?, ElmEngineTypeCLI> defaultEngineConsideringEnvironmentVariable)
    {
        var defaultEngineFromEnvironmentVariable =
            defaultFromEnvironmentVariablePrefix switch
            {
                { } variablePrefix => ElmEngineFromEnvironmentVariableWithPrefix(variablePrefix),
                null => null
            };

        var defaultEngine = defaultEngineConsideringEnvironmentVariable(defaultEngineFromEnvironmentVariable);

        var elmEngineOption = new Option<string?>("--elm-engine")
        {
            Description = "Select the engine for running Elm programs (" + string.Join(", ", Enum.GetNames<ElmEngineTypeCLI>()) + "). Defaults to " + defaultEngine,
            Arity = ArgumentArity.ZeroOrOne
        };

        ElmEngineType parseElmEngineTypeFromOption(ParseResult parseResult)
        {
            var cliName =
                parseResult.GetValue(elmEngineOption) switch
                {
                    { } asString => Enum.Parse<ElmEngineTypeCLI>(asString, ignoreCase: true),
                    null => defaultEngine,
                };

            return
                ParseElmEngineType(
                    dynamicPGOShare,
                    cliName);
        }

        return (elmEngineOption, parseElmEngineTypeFromOption);
    }

    private static (Option<string?> elmCompilerOption, Func<ParseResult, IConsole, FileTree> loadElmCompilerFromOption)
        CreateElmCompilerOption(Command cmd)
    {
        var defaultCompiler = ElmCompilerInElm.CompilerSourceFilesDefault.Value;

        var elmCompilerOption = new Option<string?>("--elm-compiler")
        {
            Description = "Select a program for compiling Elm programs. Defaults to the version integrated with Pine.",
            Arity = ArgumentArity.ZeroOrOne
        };

        FileTree parseElmCompilerFromOption(ParseResult parseResult, IConsole console)
        {
            if (parseResult.GetValue(elmCompilerOption) is { } compilerAsString)
            {
                console.WriteLine("Loading Elm compiler from " + compilerAsString);

                return
                    LoadComposition.LoadFromPathResolvingNetworkDependencies(compilerAsString)
                    .LogToActions(console.WriteLine)
                    .Extract(error => throw new Exception("Failed to load from path '" + compilerAsString + "': " + error))
                    .tree;
            }

            return ElmCompilerInElm.CompilerSourceFilesDefault.Value;
        }

        return (elmCompilerOption, parseElmCompilerFromOption);
    }

    public static ElmEngineTypeCLI? ElmEngineFromEnvironmentVariableWithPrefix(string? environmentVariablePrefix)
    {
        var environmentVariable =
            environmentVariablePrefix?.TrimEnd('_') +
            (environmentVariablePrefix is null ? "" : "_") +
            "elm_engine";

        if (Environment.GetEnvironmentVariable(environmentVariable) is not { } asString)
            return null;

        if (Enum.TryParse<ElmEngineTypeCLI>(asString, ignoreCase: true, out var cliName))
            return cliName;

        return null;
    }

    public static ElmEngineType ParseElmEngineType(
        DynamicPGOShare? dynamicPGOShare,
        ElmEngineTypeCLI elmEngineTypeCLI) =>
        elmEngineTypeCLI switch
        {
            ElmEngineTypeCLI.Pine =>
            new ElmEngineType.Pine(
                Caching: true,
                DynamicPGOShare: null),

            ElmEngineTypeCLI.Pine_without_cache =>
            new ElmEngineType.Pine(
                Caching: false,
                DynamicPGOShare: null),

            _ =>
            throw new NotImplementedException($"Unexpected engine type value: {elmEngineTypeCLI}"),
        };

    public static void DotNetConsoleWriteLineUsingColor(string line, ConsoleColor color)
    {
        var colorBefore = Console.ForegroundColor;

        Console.ForegroundColor = color;

        Console.WriteLine(line);

        Console.ForegroundColor = colorBefore;
    }

    public static void DotNetConsoleWriteProblemCausingAbort(string line)
    {
        Console.WriteLine("");

        DotNetConsoleWriteLineUsingColor(line, ConsoleColor.Yellow);
    }

    private static (string compositionId, SourceSummaryStructure summary) CompileSourceSummary(FileTree sourceTree)
    {
        var compositionId = Convert.ToHexStringLower(PineValueHashTree.ComputeHashSorted(sourceTree).Span);

        var allBlobs = sourceTree.EnumerateFilesTransitive().ToImmutableList();

        return (compositionId, summary: new SourceSummaryStructure
        (
            numberOfFiles: allBlobs.Count,
            totalSizeOfFilesContents: allBlobs.Sum(blob => blob.fileContent.Length)
        ));
    }

    public record CompileAppReport(
        string engineVersion,
        string beginTime,
        string sourcePath,
        string? sourceCompositionId,
        SourceSummaryStructure? sourceSummary,
        IReadOnlyList<ElmAppCompilation.CompilationIterationReport>? compilationIterationsReports,
        IReadOnlyList<ElmAppCompilation.LocatedCompilationError>? compilationErrors,
        string? compilationException,
        int? compilationTimeSpentMilli,
        string? compiledCompositionId,
        int? totalTimeSpentMilli);

    public record SourceSummaryStructure(
        int numberOfFiles,
        int totalSizeOfFilesContents);

    public record DeployAppReport(
        bool initElmAppState,
        string site,
        string beginTime,
        string sourcePath,
        string sourceCompositionId,
        SourceSummaryStructure sourceSummary,
        string filteredSourceCompositionId,
        ResponseFromServerReport? responseFromServer,
        string? deployException,
        int totalTimeSpentMilli);


    public record ApplyFunctionReport(
        string site,
        AdminInterface.ApplyDatabaseFunctionRequest applyFunctionRequest,
        string beginTime,
        ResponseFromServerReport? responseFromServer,
        string? runtimeException,
        int totalTimeSpentMilli);

    public record ResponseFromServerReport(
        int? statusCode,
        object body);

    public static DeployAppReport DeployApp(
        string sourcePath,
        string site,
        string? siteDefaultPassword,
        bool initElmAppState,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        Console.WriteLine("Beginning to build configuration...");

        var buildResult =
            Platform.WebService.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                sourcePath: sourcePath);

        var (sourceCompositionId, sourceSummary) = CompileSourceSummary(buildResult.sourceTree);

        var appConfigZipArchive = buildResult.configZipArchive;

        var compiledCompositionId =
            Convert.ToHexStringLower(
                PineValueHashTree.ComputeHashSorted(FileTree.FromSetOfFilesWithCommonFilePath(
                    ZipArchive.EntriesFromZipArchive(appConfigZipArchive))).Span);

        Console.WriteLine("Built app config " + compiledCompositionId + " from " + sourceCompositionId + ".");

        ResponseFromServerReport? responseFromServer = null;

        Exception? deployException = null;

        try
        {
            if (!LooksLikeLocalSite(site))
            {
                var deployAddress =
                    site.TrimEnd('/') +
                    (initElmAppState
                    ?
                    Platform.WebService.StartupAdminInterface.PathApiDeployAndInitAppState
                    :
                    Platform.WebService.StartupAdminInterface.PathApiDeployAndMigrateAppState);

                Console.WriteLine("Attempting to deploy app '" + compiledCompositionId + "' to '" + deployAddress + "'...");

                var httpResponse = AttemptHttpRequest(() =>
                    {
                        var httpContent = new System.Net.Http.ByteArrayContent(appConfigZipArchive);

                        httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/zip");
                        httpContent.Headers.ContentDisposition =
                            new System.Net.Http.Headers.ContentDispositionHeaderValue("attachment") { FileName = compiledCompositionId + ".zip" };

                        return new System.Net.Http.HttpRequestMessage
                        {
                            Method = System.Net.Http.HttpMethod.Post,
                            RequestUri = MapUriForForAdminInterface(deployAddress),
                            Content = httpContent,
                        };
                    },
                    defaultPassword: siteDefaultPassword,
                    promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

                var responseContentString = httpResponse.Content.ReadAsStringAsync().Result;

                Console.WriteLine(
                    "Server response: " + httpResponse.StatusCode + "\n" + responseContentString);

                object responseBodyReport = responseContentString;

                try
                {
                    responseBodyReport =
                        System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonObject>(responseContentString)!;
                }
                catch { }

                responseFromServer = new ResponseFromServerReport
                (
                    statusCode: (int)httpResponse.StatusCode,
                    body: responseBodyReport
                );
            }
            else
            {
                var processStoreFileStore = new FileStoreFromSystemIOFile(site);

                var processStoreWriter =
                    new Platform.WebService.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                        processStoreFileStore,
                        getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                        processStoreFileStore,
                        skipWritingComponentSecondTime: true);

                var appConfigTree =
                    FileTree.FromSetOfFilesWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

                var appConfigComponent = FileTreeEncoding.Encode(appConfigTree);

                processStoreWriter.StoreComponent(appConfigComponent);

                var appConfigValueInFile =
                    new Platform.WebService.ProcessStoreSupportingMigrations.ValueInFileStructure
                    {
                        HashBase16 = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(appConfigComponent).Span)
                    };

                var compositionLogEvent =
                    Platform.WebService.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                        appConfigValueInFile: appConfigValueInFile,
                        initElmAppState: initElmAppState);

                var (statusCode, responseReport) =
                    Platform.WebService.StartupAdminInterface.AttemptContinueWithCompositionEventAndCommit(
                        compositionLogEvent,
                        processStoreFileStore);

                responseFromServer = new ResponseFromServerReport
                (
                    statusCode: statusCode,
                    body: responseReport
                );
            }
        }
        catch (Exception e)
        {
            Console.WriteLine("Failed with exception: " + e.Message);

            deployException = e;
        }

        return new DeployAppReport
        (
            initElmAppState: initElmAppState,
            site: site,
            beginTime: beginTime,
            sourcePath: sourcePath,
            sourceCompositionId: sourceCompositionId,
            sourceSummary: sourceSummary,
            filteredSourceCompositionId: compiledCompositionId,
            responseFromServer: responseFromServer,
            deployException: deployException?.ToString(),
            totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds
        );
    }

    public static Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>> ListFunctions(
        string site,
        string? siteDefaultPassword,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        if (LooksLikeLocalSite(site))
        {
            return
                Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>>.err(
                    "Not implemented for local site");
        }

        try
        {
            var httpRequestUri =
                site.TrimEnd('/') + Platform.WebService.StartupAdminInterface.PathApiListDatabaseFunctions;

            var httpResponse = AttemptHttpRequest(() =>
            {
                return new System.Net.Http.HttpRequestMessage
                {
                    Method = System.Net.Http.HttpMethod.Get,
                    RequestUri = MapUriForForAdminInterface(httpRequestUri),
                };
            },
            defaultPassword: siteDefaultPassword,
            promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

            var responseContentString = httpResponse.Content.ReadAsStringAsync().Result;

            if (!httpResponse.IsSuccessStatusCode)
            {
                return "HTTP response status code not OK: " + httpResponse.StatusCode + ", content:\n" + responseContentString;
            }

            return
                System.Text.Json.JsonSerializer.Deserialize<Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>>>(responseContentString)!
                .MapError(err => "Server returned error: " + err);
        }
        catch (Exception e)
        {
            return "Failed with runtime exception:\n" + e;
        }
    }

    public static ApplyFunctionReport ApplyFunction(
        string site,
        string functionName,
        IReadOnlyList<string> serializedArgumentsJson,
        bool commitResultingState,
        string? siteDefaultPassword,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        ResponseFromServerReport? responseFromServer = null;

        Exception? runtimeException = null;

        var applyFunctionRequest =
            new AdminInterface.ApplyDatabaseFunctionRequest(
                functionName: functionName,
                serializedArgumentsJson: serializedArgumentsJson,
                commitResultingState: commitResultingState);

        try
        {
            if (LooksLikeLocalSite(site))
            {
                throw new NotImplementedException("Not implemented for local site");
            }

            var applyAddress =
                site.TrimEnd('/') + Platform.WebService.StartupAdminInterface.PathApiApplyDatabaseFunction;

            Console.WriteLine("Attempting to apply function '" + functionName + "' at '" + applyAddress + "'...");

            var httpResponse = AttemptHttpRequest(() =>
            {
                var httpContent = System.Net.Http.Json.JsonContent.Create(applyFunctionRequest);

                return new System.Net.Http.HttpRequestMessage
                {
                    Method = System.Net.Http.HttpMethod.Post,
                    RequestUri = MapUriForForAdminInterface(applyAddress),
                    Content = httpContent,
                };
            },
            defaultPassword: siteDefaultPassword,
            promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

            var responseContentString = httpResponse.Content.ReadAsStringAsync().Result;

            Console.WriteLine(
                "Server response: " + httpResponse.StatusCode + "\n" + responseContentString);

            object responseBodyReport = responseContentString;

            try
            {
                responseBodyReport =
                    System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonObject>(responseContentString)!;
            }
            catch { }

            responseFromServer = new ResponseFromServerReport
            (
                statusCode: (int)httpResponse.StatusCode,
                body: responseBodyReport
            );
        }
        catch (Exception e)
        {
            Console.WriteLine("Failed with exception: " + e.Message);

            runtimeException = e;
        }

        return new ApplyFunctionReport
        (
            site: site,
            applyFunctionRequest: applyFunctionRequest,
            beginTime: beginTime,
            responseFromServer: responseFromServer,
            runtimeException: runtimeException?.ToString(),
            totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds
        );
    }

    private static async System.Threading.Tasks.Task<(System.Net.Http.HttpResponseMessage httpResponse, string? enteredPassword)>
        AttemptHttpRequest(
        Func<System.Net.Http.HttpRequestMessage> buildRequestBeforeAddingCommonHeaders,
        string? defaultPassword,
        bool promptForPasswordOnConsole)
    {
        System.Net.Http.HttpRequestMessage buildRequest() =>
            AddUserAgentHeader(buildRequestBeforeAddingCommonHeaders());

        using var httpClient = new System.Net.Http.HttpClient();

        httpClient.Timeout = TimeSpan.FromMinutes(4);

        void setHttpClientPassword(string? password)
        {
            httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                "Basic",
                Convert.ToBase64String(Encoding.UTF8.GetBytes(BasicAuthenticationForAdmin(password))));
        }

        setHttpClientPassword(defaultPassword);

        var httpResponse = await httpClient.SendAsync(buildRequest());

        string? enteredPassword = null;

        if (promptForPasswordOnConsole &&
            httpResponse.StatusCode == System.Net.HttpStatusCode.Unauthorized &&
             httpResponse.Headers.WwwAuthenticate.Any())
        {
            Console.WriteLine("The server at '" + httpResponse.RequestMessage?.RequestUri + "' is asking for authentication. Please enter the password we should use to authenticate there:");

            enteredPassword = ReadLine.ReadPassword("> ").Trim();

            Console.WriteLine("I retry using this password...");

            setHttpClientPassword(enteredPassword);

            httpResponse = await httpClient.SendAsync(buildRequest());
        }

        return (httpResponse, enteredPassword);
    }

    private static string MapSiteForCommandLineArgument(string siteArgument)
    {
        if (LooksLikeLocalSite(siteArgument))
            return siteArgument;

        return MapUriForForAdminInterface(siteArgument).ToString();
    }

    private static Uri MapUriForForAdminInterface(string uriString)
    {
        if (!Uri.IsWellFormedUriString(uriString, UriKind.Absolute))
        {
            if (!(uriString.StartsWith("http://", StringComparison.InvariantCultureIgnoreCase) ||
                uriString.StartsWith("https://", StringComparison.InvariantCultureIgnoreCase)))
            {
                uriString = "http://" + uriString;
            }
        }

        return MapUriForDefaultPort(uriString, AdminInterfaceDefaultPort);
    }

    private static Uri MapUriForDefaultPort(string uriString, int defaultPort)
    {
        var uri = new Uri(uriString);

        if (!uri.Authority.Contains(':'))
            return WithPort(uri, defaultPort);

        return uri;
    }

    public static bool LooksLikeLocalSite(string site)
    {
        if (site.StartsWith(".") || site.StartsWith("/"))
            return true;

        if (Regex.IsMatch(site, "^http(|s)://", RegexOptions.IgnoreCase))
            return false;

        try
        {
            return Directory.Exists(site) || File.Exists(site);
        }
        catch { }

        return false;
    }

    public static Uri WithPort(Uri uri, int newPort)
    {
        var builder = new UriBuilder(uri)
        {
            Port = newPort
        };
        return builder.Uri;
    }

    private record CopyElmAppStateReport(
        string beginTime,
        string source,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        string? destination,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        AppStateSummary? appStateSummary = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        ResponseFromServerReport? destinationResponseFromServer = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        string? destinationFileReport = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        int? totalTimeSpentMilli = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        object? error = null);

    public record AppStateSummary(string hash, int length);

    private static CopyElmAppStateReport CopyElmAppState(
        string source,
        string? sourceDefaultPassword,
        string? destination,
        string? destinationDefaultPassword)
    {
        var report = new CopyElmAppStateReport
        (
            source: source,
            destination: destination,
            beginTime: BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow)
        );

        CopyElmAppStateReport returnWithErrorMessage(string error)
        {
            Console.WriteLine("Error: " + error);
            return report with { error = error };
        }

        byte[] appStateSerial;

        if (LooksLikeLocalSite(source))
        {
            if (File.Exists(source))
            {
                appStateSerial = File.ReadAllBytes(source);
            }
            else
            {
                return returnWithErrorMessage("Source looks like a local site, but I did not find a file at " + source);
            }
        }
        else
        {
            appStateSerial = GetElmAppStateViaAdminInterface(source, sourceDefaultPassword, promptForPasswordOnConsole: true);
        }

        if (appStateSerial == null)
        {
            return returnWithErrorMessage("Failed to read from source.");
        }

        var appStateComponent = PineValue.Blob(appStateSerial);
        var appStateId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(appStateComponent).Span);

        report = report with { appStateSummary = new AppStateSummary(hash: appStateId, length: appStateSerial.Length) };

        Console.WriteLine("Got app state " + appStateId + " from the source. It is " + appStateSerial.Length + " bytes long.");

        string saveToFile(string filePath)
        {
            Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

            File.WriteAllBytes(filePath, appStateSerial);

            var message = "Saved to file '" + filePath + "'";

            Console.WriteLine(message);

            return message;
        }

        if (destination == null)
        {
            return returnWithErrorMessage("I got no argument for the destination. To copy the app state to a file or a live process, Run the copy command with an argument for the destination.");
        }

        if (LooksLikeLocalSite(destination))
        {
            var filePath =
                Directory.Exists(destination)
                ?
                Path.Combine(destination, appStateId + "app-state.json")
                :
                destination;

            return report with { destinationFileReport = saveToFile(filePath) };
        }

        return
            report with
            {
                destinationResponseFromServer =
                    SetElmAppStateViaAdminInterface(
                        site: destination,
                        siteDefaultPassword: destinationDefaultPassword,
                        elmAppStateSerialized: appStateSerial,
                        promptForPasswordOnConsole: true)
            };

    }

    private static ResponseFromServerReport SetElmAppStateViaAdminInterface(
        string site,
        string? siteDefaultPassword,
        byte[] elmAppStateSerialized,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var elmAppStateComponent = PineValue.Blob(elmAppStateSerialized);

        var elmAppStateId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(elmAppStateComponent).Span);

        var httpResponse = AttemptHttpRequest(() =>
            {
                var httpContent = new System.Net.Http.ByteArrayContent(elmAppStateSerialized);

                httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/json");

                return new System.Net.Http.HttpRequestMessage
                {
                    Method = System.Net.Http.HttpMethod.Post,
                    RequestUri = MapUriForForAdminInterface(site.TrimEnd('/') + Platform.WebService.StartupAdminInterface.PathApiElmAppState),
                    Content = httpContent,
                };
            },
            defaultPassword: siteDefaultPassword,
            promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

        var responseContentString = httpResponse.Content.ReadAsStringAsync().Result;

        Console.WriteLine(
            "Server response: " + httpResponse.StatusCode + "\n" +
             responseContentString);

        object responseBodyReport = responseContentString;

        try
        {
            responseBodyReport =
                System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonObject>((string)responseBodyReport)!;
        }
        catch { }

        return new ResponseFromServerReport
        (
            statusCode: (int)httpResponse.StatusCode,
            body: responseBodyReport
        );
    }

    private static byte[] GetElmAppStateViaAdminInterface(
        string site,
        string? siteDefaultPassword,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var httpResponse = AttemptHttpRequest(() =>
            {
                return new System.Net.Http.HttpRequestMessage
                {
                    Method = System.Net.Http.HttpMethod.Get,
                    RequestUri = MapUriForForAdminInterface(site.TrimEnd('/') + Platform.WebService.StartupAdminInterface.PathApiElmAppState),
                };
            },
            defaultPassword: siteDefaultPassword,
            promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

        Console.WriteLine("Server response status code: " + httpResponse.StatusCode);

        var elmAppStateSerialized = httpResponse.Content.ReadAsByteArrayAsync().Result;

        var elmAppStateComponent = PineValue.Blob(elmAppStateSerialized);
        var elmAppStateId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(elmAppStateComponent).Span);

        return elmAppStateSerialized;
    }

    private record TruncateProcessHistoryReport(
        string beginTime,
        string site,
        ResponseFromServerReport responseFromServer,
        int totalTimeSpentMilli);

    private static TruncateProcessHistoryReport TruncateProcessHistory(
        string site,
        string? siteDefaultPassword,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var requestUrl =
            site.TrimEnd('/') + Platform.WebService.StartupAdminInterface.PathApiTruncateProcessHistory;

        Console.WriteLine("Beginning to truncate process history at '" + site + "'...");

        var httpResponse = AttemptHttpRequest(() =>
                new System.Net.Http.HttpRequestMessage
                {
                    Method = System.Net.Http.HttpMethod.Post,
                    RequestUri = MapUriForForAdminInterface(requestUrl),
                },
            defaultPassword: siteDefaultPassword,
            promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

        var responseContentString = httpResponse.Content.ReadAsStringAsync().Result;

        Console.WriteLine(
            "Server response: " + httpResponse.StatusCode + "\n" +
             responseContentString);

        object responseBodyReport = responseContentString;

        try
        {
            responseBodyReport =
                System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonObject>(responseContentString)!;
        }
        catch { }

        var responseFromServer = new ResponseFromServerReport
        (
            statusCode: (int)httpResponse.StatusCode,
            body: responseBodyReport
        );

        return new TruncateProcessHistoryReport
        (
            beginTime: beginTime,
            site: site,
            responseFromServer: responseFromServer,
            totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds
        );
    }

    private static (string commandName, Func<(bool executableIsRegisteredOnPath, Action registerExecutableDirectoryOnPath)> checkInstallation)
        CheckIfExecutableIsRegisteredOnPath()
    {
        var environmentVariableName = "PATH";

        var environmentVariableScope = EnvironmentVariableTarget.User;

        string? getCurrentValueOfEnvironmentVariable() =>
            Environment.GetEnvironmentVariable(environmentVariableName, environmentVariableScope);

        var executableFilePath = GetCurrentProcessExecutableFilePath()!;

        var executableDirectoryPath = Path.GetDirectoryName(executableFilePath);

        var commandName = Regex.Match(Path.GetFileName(executableFilePath)!, @"(.+?)(?=\.exe$|$)").Groups[1].Value;

        (bool executableIsRegisteredOnPath, Action registerExecutableDirectoryOnPath) checkInstallation()
        {
            if (System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows))
            {
                var executableIsRegisteredOnPath =
                    (getCurrentValueOfEnvironmentVariable() ?? "")
                    .Split(Path.PathSeparator).Contains(executableDirectoryPath);

                var registerExecutableForCurrentUser = new Action(() =>
                {
                    var newValueForPathEnv =
                        executableDirectoryPath +
                        Path.PathSeparator +
                        getCurrentValueOfEnvironmentVariable();

                    Environment.SetEnvironmentVariable(environmentVariableName, newValueForPathEnv, environmentVariableScope);

                    //  https://stackoverflow.com/questions/32650063/get-environment-variable-out-of-new-process-in-c-sharp/32650213#32650213
                    //  https://devblogs.microsoft.com/oldnewthing/?p=91591
                    //  https://docs.microsoft.com/en-us/previous-versions//cc723564(v=technet.10)?redirectedfrom=MSDN#XSLTsection127121120120

                    Console.WriteLine(
                    "I added the path '" + executableDirectoryPath + "' to the '" + environmentVariableName +
                    "' environment variable for the current user account. You will be able to use the '" + commandName + "' command in newer instances of the Command Prompt.");
                });

                return (executableIsRegisteredOnPath, registerExecutableForCurrentUser);
            }
            else
            {
                var destinationExecutableFilePath = "/usr/local/bin/" + commandName;

                byte[]? currentRegisteredFileContent = null;

                if (File.Exists(destinationExecutableFilePath))
                {
                    currentRegisteredFileContent = File.ReadAllBytes(destinationExecutableFilePath);
                }

                var currentExecuableFileContent = File.ReadAllBytes(executableFilePath);

                var executableIsRegisteredOnPath =
                    currentRegisteredFileContent != null &&
                    currentRegisteredFileContent.SequenceEqual(currentExecuableFileContent);

                var registerExecutableForCurrentUser = new Action(() =>
                {
                    ExecutableFile.CreateAndWriteFileToPath(
                        destinationExecutableFilePath,
                        currentExecuableFileContent,
                        makeExecutable: true);

                    Console.WriteLine(
                        "I copied the executable file to '" + destinationExecutableFilePath +
                        "'. You will be able to use the '" + commandName + "' command in newer terminal instances.");
                });

                return (executableIsRegisteredOnPath, registerExecutableForCurrentUser);
            }
        }

        return (commandName, checkInstallation);
    }

    private static string ReportFilePath => Path.Combine(Environment.CurrentDirectory, "pine-tool", "report");

    static readonly System.Text.Json.JsonSerializerOptions reportJsonSerializerOptions = new()
    {
        WriteIndented = true
    };

    private static void WriteReportToFileInReportDirectory(string reportContent, string reportKind)
    {
        var fileName = BytesConversions.TimeStringViewForReport(programStartTime) + "_" + reportKind;

        var filePath = Path.Combine(ReportFilePath, fileName);

        Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

        File.WriteAllBytes(filePath, Encoding.UTF8.GetBytes(reportContent));

        Console.WriteLine("Saved report to file '" + filePath + "'.");
    }

    private static string? GetCurrentProcessExecutableFilePath() =>
        System.Diagnostics.Process.GetCurrentProcess().MainModule?.FileName;

    private static System.Net.Http.HttpRequestMessage AddUserAgentHeader(
        System.Net.Http.HttpRequestMessage httpRequest)
    {
        httpRequest.Headers.UserAgent.Add(
            new System.Net.Http.Headers.ProductInfoHeaderValue(
                new System.Net.Http.Headers.ProductHeaderValue("pine-cli", AppVersionId)));

        return httpRequest;
    }

    private static readonly DateTimeOffset programStartTime = DateTimeOffset.UtcNow;

    public static ElmTestRs.ElmTestRsRunReport CompileAndElmTestRs(string source)
    {
        var (_, compiledAppFiles) = CompileApp(source);

        if (compiledAppFiles == null)
            throw new Exception("Compilation failed");

        return ElmTestRs.Run(compiledAppFiles);
    }
}
