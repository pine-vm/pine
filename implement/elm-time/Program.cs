using ElmTime.Elm019;
using McMaster.Extensions.CommandLineUtils;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Pine;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;
using static ElmTime.Platform.WebServer.Configuration;

namespace ElmTime;

public class Program
{
    public static string AppVersionId => "2023-05-08";

    private static int AdminInterfaceDefaultPort => 4000;

    private static int Main(string[] args)
    {
        Elm019Binaries.OverrideElmMakeHomeDirectory = ElmMakeHomeDirectoryPath;
        Elm019Binaries.ElmMakeResultCacheFileStoreDefault = ElmMakeResultCacheFileStoreDefault;

        LoadFromGitHubOrGitLab.RepositoryFilesPartialForCommitCacheDefault =
            new CacheByFileName(new FileStoreFromSystemIOFile(Path.Combine(Filesystem.CacheDirectory, "git", "partial-for-commit", "zip")));

        var app = new CommandLineApplication
        {
            Name = "elm-time",
            Description = "Elm-Time - runtime environment for Elm.\nTo get help or report an issue, see https://github.com/elm-time/elm-time/discussions",
            HelpTextGenerator =
            new McMaster.Extensions.CommandLineUtils.HelpText.DefaultHelpTextGenerator { SortCommandsByName = false }
        };

        app.VersionOption(template: "-v|--version", shortFormVersion: "version " + AppVersionId);

        var installCommand = app.Command("install", installCommand =>
        {
            var (commandName, checkInstallation) = CheckIfExecutableIsRegisteredOnPath();

            installCommand.Description = "Install the '" + commandName + "' command for the current user account.";
            installCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            installCommand.OnExecute(() =>
            {
                checkInstallation().registerExecutableDirectoryOnPath();
            });
        });

        var runServerCommand = AddRunServerCommand(app);

        var deployCommand = AddDeployCommand(app);
        var copyAppStateCommand = AddCopyAppStateCommand(app);
        var copyProcessCommand = AddCopyProcessCommand(app);
        var listFunctionsCommand = AddListFunctionsCommand(app);
        var applyFunctionCommand = AddApplyFunctionCommand(app);
        var truncateProcessHistoryCommand = AddTruncateProcessHistoryCommand(app);

        var compileCommand = AddCompileCommand(app);
        var interactiveCommand = AddInteractiveCommand(app);
        var describeCommand = AddDescribeCommand(app);
        var elmTestRsCommand = AddElmTestRsCommand(app);
        var makeCommand = AddMakeCommand(app);

        var runCacheServerCmd = AddRunCacheServerCmd(app);

        app.Command("user-secrets", userSecretsCmd =>
        {
            userSecretsCmd.Description = "Manage passwords for accessing the admin interfaces of servers.";
            userSecretsCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            userSecretsCmd.Command("store", storeCmd =>
            {
                storeCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var siteArgument = storeCmd.Argument("site", "Site where to use this secret as password.", multipleValues: false).IsRequired(allowEmptyStrings: false);
                var passwordArgument = storeCmd.Argument("password", "Password to use for authentication.", multipleValues: false).IsRequired(allowEmptyStrings: false);

                storeCmd.OnExecute(() =>
                {
                    UserSecrets.StorePasswordForSite(siteArgument.Value!, passwordArgument.Value!);
                });
            });

            userSecretsCmd.OnExecute(() =>
            {
                Console.WriteLine("Please specify a subcommand.");
                userSecretsCmd.ShowHelp();

                return 1;
            });
        });

        var helpCmd = app.Command("help", helpCmd =>
        {
            helpCmd.Description = "Explain available commands and how to use the command-line interface.";
            helpCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var allOption = helpCmd.Option("--all", "List all commands", CommandOptionType.NoValue);

            allOption.ShortName = "a";

            var checkedInstallation = CheckIfExecutableIsRegisteredOnPath().checkInstallation();

            var setupGroupCommands =
                checkedInstallation.executableIsRegisteredOnPath
                ?
                Array.Empty<CommandLineApplication>() :
                new[]
                {
                    installCommand,
                };

            var commonCmdGroups = new[]
            {
                new
                {
                    title = "Set up your development environment:",
                    commands = setupGroupCommands,
                },
                new
                {
                    title = "Operate servers and maintain live systems:",
                    commands = new[]
                    {
                        runServerCommand,
                        deployCommand,
                        copyAppStateCommand,
                        copyProcessCommand,
                        listFunctionsCommand,
                        applyFunctionCommand,
                        truncateProcessHistoryCommand,
                    }
                },
                new
                {
                    title = "Develop and learn:",
                    commands = new[]
                    {
                        compileCommand,
                        interactiveCommand,
                        describeCommand,
                        elmTestRsCommand,
                        makeCommand,
                    }
                },
            }
            .Where(group => 0 < group.commands.Length)
            .Select(group => new
            {
                group.title,
                commands = group.commands.Select(cmd =>
                new
                {
                    nameColumn = (cmd.FullName ?? cmd.Name)!,
                    descriptionColumn = cmd.Description,
                }).ToImmutableList(),
            });

            foreach (var topLevelCmd in app.Commands)
            {
                var cmdPrimaryName = topLevelCmd.Names.FirstOrDefault()!;

                helpCmd.Command(cmdPrimaryName, helpForAppCmd =>
                {
                    foreach (var additionalName in topLevelCmd.Names.Except(new[] { cmdPrimaryName }))
                        helpForAppCmd.AddName(additionalName);

                    CommandExtension.ConfigureHelpCommandForCommand(helpForAppCmd, topLevelCmd);
                });
            }

            helpCmd.OnExecute(() =>
            {
                if (allOption.HasValue())
                {
                    app.ShowHelp();

                    return 0;
                }

                var longestCmdNameLength =
                    commonCmdGroups.SelectMany(group => group.commands)
                    .Max(cmd => cmd.nameColumn.Length);

                var cmdDescriptionIndent = longestCmdNameLength + 4;

                var groupsTexts =
                    commonCmdGroups
                    .Select(group =>
                        group.title + "\n" +
                        string.Join("\n", group.commands.Select(cmd =>
                        "   " +
                        cmd.nameColumn + new string(' ', cmdDescriptionIndent - cmd.nameColumn.Length) +
                        cmd.descriptionColumn)));

                var elmFsCommandName = CheckIfExecutableIsRegisteredOnPath().commandName;

                var overviewText =
                    string.Join("\n\n", new[]
                    {
                        app.Description,
                        "Usage: " + elmFsCommandName + " [command] [options]",
                        "These are common elm-time commands used in various situations:",
                        string.Join("\n\n", groupsTexts),
                        "'" + elmFsCommandName + " help -a' lists available subcommands.\nSee '" + elmFsCommandName + " help <command>' to read about a specific subcommand.",
                    });

                Console.WriteLine(overviewText);

                return 0;
            });
        });

        app.OnExecute(() =>
        {
            helpCmd.Execute();

            return 0;
        });

        int executeAndGuideInCaseOfException()
        {
            try
            {
                return app.Execute(args);
            }
            catch (CommandParsingException ex)
            {
                var message = ex.Message;

                if (ex is UnrecognizedCommandParsingException uex && uex.NearestMatches.Any())
                {
                    message = message?.TrimEnd() + "\nDid you mean '" + uex.NearestMatches.FirstOrDefault() + "'?";
                }

                DotNetConsoleWriteProblemCausingAbort(message);

                return 430;
            }
        }

        return executeAndGuideInCaseOfException();
    }

    private static CommandLineApplication AddRunServerCommand(CommandLineApplication app) =>
        app.Command("run-server", runServerCommand =>
        {
            runServerCommand.Description = "Run a server with a web-based admin interface. The HTTP API supports deployments, migrations, and other operations to manage your app.";
            runServerCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var adminUrlsDefault = "http://*:" + AdminInterfaceDefaultPort;

            var processStoreOption = runServerCommand.Option("--process-store", "Directory in the file system to contain the process store.", CommandOptionType.SingleValue);
            var deletePreviousProcessOption = runServerCommand.Option("--delete-previous-process", "Delete the previous backend process found in the given store. If you don't use this option, the server restores the process from the persistent store on startup.", CommandOptionType.NoValue);
            var adminUrlsOption = runServerCommand.Option("--admin-urls", "URLs for the admin interface. The default is " + adminUrlsDefault + ".", CommandOptionType.SingleValue);
            var adminPasswordOption = runServerCommand.Option("--admin-password", "Password for the admin interface at '--admin-urls'.", CommandOptionType.SingleValue);
            var publicAppUrlsOption = runServerCommand.Option("--public-urls", "URLs to serve the public app from. The default is '" + string.Join(",", PublicWebHostUrlsDefault) + "'.", CommandOptionType.SingleValue);
            var copyProcessOption = runServerCommand.Option("--copy-process", "Path to a process to copy. Can be a URL to an admin interface of a server or a path to an archive containing files representing the process state. This option also implies '--delete-previous-process'.", CommandOptionType.SingleValue);
            var deployOption = runServerCommand.Option("--deploy", "Path to an app to deploy on startup, analogous to the 'source' path on the `deploy` command. Can be combined with '--copy-process'.", CommandOptionType.SingleValue);
            var elmEngineOption = AddElmEngineOptionOnCommand(
                runServerCommand,
                defaultFromEnvironmentVariablePrefix: "web_server",
                defaultEngineConsideringEnvironmentVariable: fromEnv => fromEnv ?? ElmInteractive.ElmEngineType.JavaScript_Jint);

            runServerCommand.OnExecute(() =>
            {
                var processStorePath = processStoreOption.Value();

                var publicAppUrls =
                    publicAppUrlsOption.Value()?.Split(',').Select(url => url.Trim()).ToArray() ??
                    PublicWebHostUrlsDefault;

                var copyProcess = copyProcessOption.Value();

                if ((deletePreviousProcessOption.HasValue() || copyProcess != null) && processStorePath != null)
                {
                    Console.WriteLine("Deleting the previous process state from '" + processStorePath + "'...");

                    if (Directory.Exists(processStorePath))
                        Directory.Delete(processStorePath, true);

                    Console.WriteLine("Completed deleting the previous process state from '" + processStorePath + "'.");
                }

                IFileStore buildProcessStoreFileStore()
                {
                    if (processStorePath == null)
                    {
                        Console.WriteLine("I got no path to a persistent store for the process. This process will not be persisted!");

                        var files = new System.Collections.Concurrent.ConcurrentDictionary<IImmutableList<string>, IReadOnlyList<byte>>(EnumerableExtension.EqualityComparer<IImmutableList<string>>());

                        var fileStoreWriter = new DelegatingFileStoreWriter
                        (
                            SetFileContentDelegate: file => files[file.path] = file.fileContent,
                            AppendFileContentDelegate: file => files.AddOrUpdate(
                               file.path, _ => file.fileContent,
                               (_, fileBefore) => fileBefore.Concat(file.fileContent).ToArray()),
                            DeleteFileDelegate: path => files.Remove(path, out var _)
                        );

                        var fileStoreReader = new DelegatingFileStoreReader
                        (
                            ListFilesInDirectoryDelegate: path =>
                                files.Select(file =>
                                {
                                    if (!file.Key.Take(path.Count).SequenceEqual(path))
                                        return null;

                                    return file.Key.Skip(path.Count).ToImmutableList();
                                }).WhereNotNull(),
                            GetFileContentDelegate: path =>
                            {
                                files.TryGetValue(path, out var fileContent);

                                return fileContent;
                            }
                        );

                        return new FileStoreFromWriterAndReader(fileStoreWriter, fileStoreReader);
                    }
                    else
                    {
                        return new FileStoreFromSystemIOFile(processStorePath);
                    }
                }

                var processStoreFileStore = buildProcessStoreFileStore();

                if (copyProcess != null)
                {
                    var copyFiles =
                        LoadFilesForRestoreFromPathAndLogToConsole(
                            sourcePath: copyProcess,
                            sourcePassword: null);

                    foreach (var file in copyFiles)
                        processStoreFileStore.SetFileContent(file.Key.ToImmutableList(), file.Value.ToArray());
                }

                var adminInterfaceUrls = adminUrlsOption.Value() ?? adminUrlsDefault;

                var deployOptionValue = deployOption.Value();

                if (deployOptionValue != null)
                {
                    Console.WriteLine("Loading app config to deploy...");

                    var appConfigZipArchive =
                        Platform.WebServer.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                            sourcePath: deployOptionValue).configZipArchive;

                    var appConfigTree =
                        PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                            ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

                    var appConfigComponent = PineValueComposition.FromTreeWithStringPath(appConfigTree);

                    var processStoreWriter =
                        new Platform.WebServer.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                            processStoreFileStore,
                            getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                            processStoreFileStore);

                    processStoreWriter.StoreComponent(appConfigComponent);

                    var appConfigValueInFile =
                        new Platform.WebServer.ProcessStoreSupportingMigrations.ValueInFileStructure
                        {
                            HashBase16 = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(appConfigComponent))
                        };

                    var initElmAppState =
                        (deletePreviousProcessOption.HasValue() || processStorePath == null) && !copyProcessOption.HasValue();

                    var compositionLogEvent =
                        Platform.WebServer.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                            appConfigValueInFile: appConfigValueInFile,
                            initElmAppState: initElmAppState);

                    var testDeployResult = Platform.WebServer.PersistentProcessLiveRepresentation.TestContinueWithCompositionEvent(
                        compositionLogEvent: compositionLogEvent,
                        fileStoreReader: processStoreFileStore)
                    .Extract(error => throw new Exception("Attempt to deploy app config failed: " + error));

                    foreach (var (filePath, fileContent) in testDeployResult.projectedFiles)
                        processStoreFileStore.SetFileContent(filePath, fileContent);
                }

                var elmEngineType = elmEngineOption.parseElmEngineTypeFromOption();

                var jsEngineFactory =
                    elmEngineType switch
                    {
                        ElmInteractive.ElmEngineType.JavaScript_Jint => new Func<IJsEngine>(JsEngineJintOptimizedForElmApps.Create),
                        ElmInteractive.ElmEngineType.JavaScript_V8 => new Func<IJsEngine>(JsEngineFromJavaScriptEngineSwitcher.ConstructJsEngine),

                        object other => throw new NotImplementedException("Engine type not implemented here: " + other)
                    };

                var webHostBuilder =
                    Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                    .ConfigureAppConfiguration(builder => builder.AddEnvironmentVariables("APPSETTING_"))
                    .UseUrls(adminInterfaceUrls)
                    .UseStartup<Platform.WebServer.StartupAdminInterface>()
                    .WithSettingPublicWebHostUrls(publicAppUrls)
                    .WithJsEngineFactory(jsEngineFactory)
                    .WithProcessStoreFileStore(processStoreFileStore);

                if (adminPasswordOption.HasValue())
                    webHostBuilder = webHostBuilder.WithSettingAdminPassword(adminPasswordOption.Value());

                var webHost = webHostBuilder.Build();

                Console.WriteLine("Starting web server with admin interface (using engine " + elmEngineType + ")...");

                webHost.Start();

                Console.WriteLine("Completed starting the web server with the admin interface at '" + adminInterfaceUrls + "'.");

                WebHostExtensions.WaitForShutdown(webHost);
            });
        });

    private static CommandLineApplication AddDeployCommand(CommandLineApplication app) =>
        app.Command("deploy", deployCommand =>
        {
            deployCommand.Description = "Deploy an app to an Elm backend process. Deployment implies migration from the previous app state if not specified otherwise.";
            deployCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var sourceArgument = deployCommand.Argument("source", "Path to the app program code to deploy.").IsRequired(allowEmptyStrings: false);

            var siteArgument = ProcessSiteArgumentOnCommand(deployCommand);
            var passwordFromSite = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(deployCommand);

            var initAppStateOption = deployCommand.Option("--init-app-state", "Do not attempt to migrate the Elm app state but use the state from the init function.", CommandOptionType.NoValue);

            deployCommand.OnExecute(() =>
            {
                var site = siteArgument.Value!;
                var sitePassword = passwordFromSite(site);

                var deployReport =
                    DeployApp(
                        sourcePath: sourceArgument.Value!,
                        site: site,
                        siteDefaultPassword: sitePassword,
                        initElmAppState: initAppStateOption.HasValue(),
                        promptForPasswordOnConsole: true);

                WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        deployReport,
                        new System.Text.Json.JsonSerializerOptions
                        {
                            WriteIndented = true
                        }),
                    reportKind: "deploy.json");
            });
        });

    private static CommandLineApplication AddCopyAppStateCommand(CommandLineApplication app) =>
        app.Command("copy-app-state", copyAppStateCommand =>
        {
            copyAppStateCommand.Description = "Copy the state of an Elm backend app.";
            copyAppStateCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var sourceArgument = copyAppStateCommand.Argument("source", "Can be a URL to an admin interface or a file with a serialized representation.").IsRequired(allowEmptyStrings: false);
            var destinationArgument = copyAppStateCommand.Argument("destination", "Can be a URL to an admin interface or a file path.");

            var passwordFromSource = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(copyAppStateCommand, "source");
            var passwordFromDestination = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(copyAppStateCommand, "destination");

            copyAppStateCommand.OnExecute(() =>
            {
                var source = sourceArgument.Value!;
                var sourcePassword = passwordFromSource(source);

                var destination = destinationArgument.Value;
                var destinationPassword = passwordFromSource(destination);

                var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var report = CopyElmAppState(
                    source: source,
                    sourceDefaultPassword: sourcePassword,
                    destination: destination,
                    destinationDefaultPassword: destinationPassword)
                with
                {
                    totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds
                };

                WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        report,
                        new System.Text.Json.JsonSerializerOptions
                        {
                            WriteIndented = true
                        }),
                    reportKind: "copy-app-state.json");
            });
        });

    private static CommandLineApplication AddCopyProcessCommand(CommandLineApplication app) =>
        app.Command("copy-process", copyProcessCommand =>
        {
            copyProcessCommand.Description = "Copy all files needed to restore a process and store them in a zip archive.";
            copyProcessCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var siteArgument = ProcessSiteArgumentOnCommand(copyProcessCommand);
            var passwordFromSite = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(copyProcessCommand);

            copyProcessCommand.OnExecute(() =>
            {
                var site = MapSiteForCommandLineArgument(siteArgument.Value!);
                var sitePassword = passwordFromSite(site);

                sitePassword =
                    AttemptHttpRequest(
                    () => new System.Net.Http.HttpRequestMessage { RequestUri = new Uri(site) },
                    defaultPassword: sitePassword,
                    promptForPasswordOnConsole: true).Result.enteredPassword ?? sitePassword;

                Console.WriteLine("Begin reading process history from '" + site + "' ...");

                var (files, lastCompositionLogRecordHashBase16) =
                    ReadFilesForRestoreProcessFromAdminInterface(site, sitePassword!);

                Console.WriteLine("Completed reading files to restore process " + lastCompositionLogRecordHashBase16 + ". Read " + files.Count + " files from '" + site + "'.");

                var zipArchive = ZipArchive.ZipArchiveFromEntries(files);

                var fileName = "process-" + lastCompositionLogRecordHashBase16 + ".zip";
                var filePath = Path.Combine(Environment.CurrentDirectory, fileName);

                File.WriteAllBytes(filePath, zipArchive);

                Console.WriteLine("Saved process archive to file '" + filePath + "'.");
            });
        });

    private static CommandLineApplication AddListFunctionsCommand(CommandLineApplication app) =>
        app.Command("list-functions", listFunctionsCommand =>
        {
            listFunctionsCommand.Description = "List the functions exposed by an Elm app for application on a database.";
            listFunctionsCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var siteArgument = ProcessSiteArgumentOnCommand(listFunctionsCommand);
            var passwordFromSite = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(listFunctionsCommand);

            listFunctionsCommand.OnExecute(() =>
            {
                var site = siteArgument.Value!;
                var sitePassword = passwordFromSite(site);

                var listFunctionsResult =
                    ListFunctions(
                        site: site,
                        siteDefaultPassword: sitePassword,
                        promptForPasswordOnConsole: true);

                var console = (Pine.IConsole)StaticConsole.Instance;

                return
                listFunctionsResult
                // For now, only show functions with a normal module prefix
                .Map(functions => functions.Where(f => f.functionName.Contains('.')).ToImmutableList())
                .Unpack(
                    fromErr:
                    err =>
                    {
                        console.WriteLine("Failed to list functions at " + site + ": " + err, Pine.IConsole.TextColor.Red);

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
                            .Concat(new[] {
                                databaseFunction.functionDescription.returnType.sourceCodeText +
                                " " + commentOnReturnType })
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
        });

    private static CommandLineApplication AddApplyFunctionCommand(CommandLineApplication app) =>
        app.Command("apply-function", applyFunctionCommand =>
        {
            applyFunctionCommand.Description = "Apply an Elm function on a database containing the state of an Elm app.";
            applyFunctionCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var siteArgument = ProcessSiteArgumentOnCommand(applyFunctionCommand);
            var passwordFromSite = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(applyFunctionCommand);
            var functionNameArgument = applyFunctionCommand.Argument("function-name", "Name of the function to apply.").IsRequired();
            var argumentOption = applyFunctionCommand.Option(
                "--argument",
                "an argument for the function, encoded as JSON. Can be either a literal or a file name.",
                optionType: CommandOptionType.MultipleValue);

            var commitResultingStateOption = applyFunctionCommand.Option(
                "--commit-resulting-state",
                "If the applied function returns a new application state, this option enables committing that new state to the database.",
                CommandOptionType.NoValue);

            applyFunctionCommand.OnExecute(() =>
            {
                var site = siteArgument.Value!;
                var sitePassword = passwordFromSite(site);

                var serializedArgumentsJson = argumentOption.Values.Select(LoadArgumentFromUserInterfaceAsJsonOrFileTextContext).ToImmutableList();

                var applyFunctionReport =
                    ApplyFunction(
                        site: site,
                        functionName: functionNameArgument.Value,
                        serializedArgumentsJson: serializedArgumentsJson,
                        commitResultingState: commitResultingStateOption.HasValue(),
                        siteDefaultPassword: sitePassword,
                        promptForPasswordOnConsole: true);

                WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        applyFunctionReport,
                        new System.Text.Json.JsonSerializerOptions
                        {
                            WriteIndented = true
                        }),
                    reportKind: "apply-function.json");
            });
        });

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

    private static CommandLineApplication AddTruncateProcessHistoryCommand(CommandLineApplication app) =>
        app.Command("truncate-process-history", truncateProcessHistoryCommand =>
        {
            truncateProcessHistoryCommand.Description = "Remove parts of the process history that are not needed to restore the process.";
            truncateProcessHistoryCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var siteArgument = ProcessSiteArgumentOnCommand(truncateProcessHistoryCommand);
            var passwordFromSite = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(truncateProcessHistoryCommand);

            truncateProcessHistoryCommand.OnExecute(() =>
            {
                var site = siteArgument.Value!;
                var sitePassword = passwordFromSite(site);

                var report =
                    TruncateProcessHistory(
                        site: site,
                        siteDefaultPassword: sitePassword,
                        promptForPasswordOnConsole: true);

                WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        report,
                        new System.Text.Json.JsonSerializerOptions
                        {
                            WriteIndented = true
                        }),
                    reportKind: "truncate-process-history.json");
            });
        });

    private static CommandLineApplication AddCompileCommand(CommandLineApplication app) =>
        app.Command("compile", compileCommand =>
        {
            compileCommand.Description = "Compile app source code the same way as would be done when deploying.";
            compileCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var sourceArgument = compileCommand.Argument("source", "Path to the app program code to compile.").IsRequired(allowEmptyStrings: false);

            compileCommand.OnExecute(() =>
            {
                var compileReport = CompileAppAndSaveCompositionToZipArchive(sourceArgument.Value!).report;

                WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        compileReport,
                        new System.Text.Json.JsonSerializerOptions
                        {
                            WriteIndented = true
                        }),
                    reportKind: "compile.json");
            });
        });

    private static CommandLineApplication AddElmTestRsCommand(CommandLineApplication app) =>
        app.Command("elm-test-rs", elmTestRsCommand =>
        {
            elmTestRsCommand.Description = "Compile and run tests using the interface of elm-test-rs. The compilation integrates interfaces such as SourceFiles.";
            elmTestRsCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var sourceArgument = elmTestRsCommand.Argument("source", "path to the Elm project containing the tests to run");

            var elmTestRsOutputOption =
                elmTestRsCommand.Option(
                    "--elm-test-rs-output",
                    "Where to save the output (via stdout and stderr) from the elm-test-rs tool.",
                    CommandOptionType.SingleValue);

            elmTestRsCommand.OnExecute(() =>
            {
                var elmTestResult = CompileAndElmTestRs(source: sourceArgument.Value ?? Environment.CurrentDirectory);

                static void saveTextToFileAndReportToConsole(string filePath, string text)
                {
                    Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

                    File.WriteAllText(filePath, text ?? "", Encoding.UTF8);
                    Console.WriteLine("Saved " + text?.Length + " characters to " + filePath);
                }

                var elmTestRsOutput = elmTestRsOutputOption.Value();

                if (elmTestRsOutput != null)
                {
                    saveTextToFileAndReportToConsole(elmTestRsOutput + ".stdout", elmTestResult.processOutput.StandardOutput ?? "");
                    saveTextToFileAndReportToConsole(elmTestRsOutput + ".stderr", elmTestResult.processOutput.StandardError ?? "");
                }

                if (0 < elmTestResult.processOutput.StandardError?.Length)
                {
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.WriteLine(elmTestResult.processOutput.StandardError);
                    Console.ResetColor();
                }

                var eventsOutputs =
                    elmTestResult.stdoutLines
                    .Select(l => ElmTestRs.OutputFromEvent(l.parsedLine))
                    .ToImmutableList();

                foreach (var eventOutput in eventsOutputs)
                {
                    if (eventOutput.text.Any())
                        Console.WriteLine("");

                    foreach (var coloredText in eventOutput.text)
                    {
                        if (coloredText.color.Red != null)
                        {
                            Console.ForegroundColor = ConsoleColor.Red;
                        }
                        else if (coloredText.color.Green != null)
                        {
                            Console.ForegroundColor = ConsoleColor.Green;
                        }
                        else
                        {
                            Console.ResetColor();
                        }

                        Console.Write(coloredText.text);
                    }
                }

                Console.WriteLine("");

                // TODO: Report more details on timing.

                return elmTestResult.processOutput.ExitCode;
            });
        });

    private static (CompileAppReport report, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? compiledAppFiles)
        CompileAppAndSaveCompositionToZipArchive(string sourcePath)
    {
        var compileResult = CompileApp(sourcePath);

        if (compileResult.compiledAppFiles != null)
        {
            var compiledTree = PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(compileResult.compiledAppFiles);
            var compiledFiles = PineValueComposition.TreeToFlatDictionaryWithPathComparer(compiledTree);

            var compiledCompositionArchive = ZipArchive.ZipArchiveFromEntries(compiledFiles);

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
            beginTime: CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow),
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

        try
        {
            var sourceFiles =
                PineValueComposition.TreeToFlatDictionaryWithPathComparer(loadCompositionResult.tree);

            var interfaceConfig =
                ElmAppInterfaceConfig.Default with
                {
                    compilationRootFilePath = sourceFiles.ContainsKey(ElmAppInterfaceConfig.Default.compilationRootFilePath) ?
                    ElmAppInterfaceConfig.Default.compilationRootFilePath :
                    sourceFiles.Keys.Where(name => name.Last().EndsWith(".elm")).First()
                };

            var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
                sourceFiles: sourceFiles,
                workingDirectoryRelative: ImmutableList<string>.Empty,
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
                        var compiledAppFiles = compilationOk.result.compiledFiles;

                        var compiledTree = PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(compiledAppFiles);
                        var compiledComposition = PineValueComposition.FromTreeWithStringPath(compiledTree);
                        var compiledCompositionId = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(compiledComposition));

                        compilationStopwatch.Stop();

                        Console.WriteLine(
                            "\nCompilation completed in " + (int)compilationStopwatch.Elapsed.TotalSeconds +
                            " seconds, resulting in composition " + compiledCompositionId + ".");

                        return (report with
                        {
                            compilationIterationsReports = compilationOk.iterationsReports,
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

    private static CommandLineApplication AddInteractiveCommand(CommandLineApplication app) =>
        app.Command("interactive", interactiveCommand =>
        {
            interactiveCommand.Description = "Enter an environment for interactive exploration and composition of Elm programs.";

            var contextAppOption =
                interactiveCommand
                .Option(
                    template: "--context-app",
                    description: "Path to an app to use as context. The Elm modules from this app will be available in the interactive environment.",
                    optionType: CommandOptionType.SingleValue);

            var initStepsOption =
                interactiveCommand
                .Option(
                    template: "--init-steps",
                    description: "Path to a list of submissions to start the session with.",
                    optionType: CommandOptionType.SingleValue);

            var enableInspectionOption =
                interactiveCommand
                .Option(
                    template: "--enable-inspection",
                    description: "Display additional information to inspect the implementation.",
                    optionType: CommandOptionType.NoValue);

            var elmEngineOption = AddElmEngineOptionOnCommand(
                interactiveCommand,
                defaultFromEnvironmentVariablePrefix: "interactive",
                defaultEngineConsideringEnvironmentVariable: fromEnv => fromEnv ?? ElmInteractive.IInteractiveSession.DefaultImplementation);

            var submitOption =
                interactiveCommand.Option(
                    template: "--submit",
                    description: "Option to submit a string as if entered during the interactive session.",
                    optionType: CommandOptionType.MultipleValue);

            var testCommand =
                interactiveCommand.Command("test", testCommand =>
                {
                    testCommand.Description = "Test the interactive automatically with given scenarios and reports timings.";

                    var scenarioOption =
                        testCommand
                        .Option(
                            template: "--scenario",
                            description: "Test an interactive scenario from the given path. The scenario specifies the submissions and can also specify expectations.",
                            optionType: CommandOptionType.MultipleValue);

                    var scenariosOption =
                        testCommand
                        .Option(
                            template: "--scenarios",
                            description: "Test a list of interactive scenarios from the given directory. Each scenario specifies the submissions and can also specify expectations.",
                            optionType: CommandOptionType.MultipleValue);

                    var compileToOption =
                        testCommand.Option(
                            template: "--compile-to",
                            description: "Option to compile frequently used expressions to a C# file",
                            optionType: CommandOptionType.SingleValue);

                    testCommand.OnExecute(() =>
                    {
                        var console = (Pine.IConsole)StaticConsole.Instance;

                        var scenarioSources = scenarioOption.Values;
                        var scenariosSources = scenariosOption.Values;

                        console.WriteLine("Got " + scenarioSources.Count + " source(s) for an individual scenario to load...");
                        console.WriteLine("Got " + scenariosSources.Count + " source(s) for a directory of scenarios to load...");

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

                        if (failedLoads.Any())
                        {
                            var failedLoad = failedLoads.First();

                            console.WriteLine(
                                string.Join(
                                    "\n",
                                        "Failed to load from " + failedLoad.Key + ":",
                                        string.Join("\n", failedLoad.Value.log),
                                        failedLoad.Value.result.Unpack(fromErr: error => error, fromOk: _ => throw new NotImplementedException())),
                                color: Pine.IConsole.TextColor.Red);

                            return;
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
                                    TreeNodeWithStringPath.TreeNode tree => tree,
                                    _ => null
                                };

                                if (asTree is null)
                                    return ImmutableList<(string, TreeNodeWithStringPath)>.Empty;

                                return
                                asTree.Elements
                                .Where(entry => entry.component is TreeNodeWithStringPath.TreeNode scenarioTree);
                            }))
                            .Select(loadedScenario =>
                            {
                                var asComposition = PineValueComposition.FromTreeWithStringPath(loadedScenario.component);

                                var hashBase16 = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(asComposition));

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

                        var aggregateCompositionTree =
                            namedDistinctScenarios.Count == 1
                            ?
                            namedDistinctScenarios.Single().Value.loadedScenario.component
                            :
                            TreeNodeWithStringPath.SortedTree(
                                namedDistinctScenarios.Select(scenario => (scenario.Key, scenario.Value.loadedScenario.component)).ToImmutableList());

                        var aggregateComposition =
                            PineValueComposition.FromTreeWithStringPath(aggregateCompositionTree);

                        var aggregateCompositionHash =
                            CommonConversion.StringBase16(PineValueHashTree.ComputeHash(aggregateComposition));

                        console.WriteLine(
                            "Successfully loaded " + namedDistinctScenarios.Count +
                            " distinct scenario(s) with an aggregate hash of " + aggregateCompositionHash + ".");

                        var exceptLoadingStopwatch = System.Diagnostics.Stopwatch.StartNew();

                        var scenariosResults =
                            ElmInteractive.TestElmInteractive.TestElmInteractiveScenarios(
                                namedDistinctScenarios,
                                namedScenario => namedScenario.Value.loadedScenario.component,
                                elmEngineOption.parseElmEngineTypeFromOption());

                        var allSteps =
                            scenariosResults
                            .SelectMany(scenario => scenario.Value.stepsReports.Select(step => (scenario, step)))
                            .ToImmutableList();

                        console.WriteLine(scenariosResults.Count + " scenario(s) resulted in " + allSteps.Count + " steps.");

                        var passedSteps =
                            allSteps.Where(step => step.step.result.IsOk()).ToImmutableList();

                        var failedSteps =
                            allSteps.Where(step => !step.step.result.IsOk()).ToImmutableList();

                        var aggregateDuration =
                            scenariosResults
                            .Select(scenario => scenario.Value.elapsedTime)
                            .Aggregate(seed: TimeSpan.Zero, func: (aggregate, scenarioTime) => aggregate + scenarioTime);

                        var overallStats = new[]
                        {
                            (label : "Failed", value : failedSteps.Count.ToString()),
                            (label : "Passed", value : passedSteps.Count.ToString()),
                            (label : "Total", value : allSteps.Count.ToString()),
                            (label : "Duration", value : CommandLineInterface.FormatIntegerForDisplay((long)aggregateDuration.TotalMilliseconds) + " ms"),
                        };

                        console.WriteLine(
                            string.Join(
                                " - ",
                                (failedSteps.Any() ? "Failed" : "Passed") + "!",
                                string.Join(", ", overallStats.Select(stat => stat.label + ": " + stat.value)),
                                aggregateCompositionHash[..10] + " (elm-time " + AppVersionId + ")"),
                            color: failedSteps.Any() ? Pine.IConsole.TextColor.Red : Pine.IConsole.TextColor.Green);

                        var failedScenarios =
                            failedSteps
                            .GroupBy(failedStep => failedStep.scenario.Key.Key)
                            .ToImmutableSortedDictionary(
                                keySelector: failedScenario => failedScenario.Key,
                                elementSelector: failedScenario => failedScenario);

                        foreach (var failedScenario in failedScenarios)
                        {
                            var scenarioId = failedScenario.Value.Key;

                            console.WriteLine(
                                "Failed " + failedScenario.Value.Count() + " step(s) in scenario " + scenarioId + ":",
                                color: Pine.IConsole.TextColor.Red);

                            foreach (var failedStep in failedScenario.Value)
                            {
                                console.WriteLine(
                                    "Failed step '" + failedStep.step.name + "':\n" +
                                    failedStep.step.result.Unpack(fromErr: error => error, fromOk: _ => throw new Exception()).errorAsText,
                                    color: Pine.IConsole.TextColor.Red);
                            }
                        }

                        if (compileToOption.Value() is string compileTo)
                        {
                            var profilingScenarios =
                            scenariosResults
                            .Select(sr => sr.Value.scenario)
                            .ToImmutableList();

                            console.WriteLine("Starting to compile for " + profilingScenarios.Count + " scenarios...");

                            var syntaxContainerConfig =
                            new PineCompileToDotNet.SyntaxContainerConfig(
                                containerTypeName: "container_type",
                                dictionaryMemberName: "compiled_expressions_dictionary");

                            var compileResult =
                            ElmInteractive.InteractiveSessionPine.CompileForProfiledScenarios(
                                profilingScenarios,
                                syntaxContainerConfig: syntaxContainerConfig,
                                limitNumber: 10);

                            var compileToFileResult =
                            compileResult
                            .AndThen(compiledClass =>
                            PineVMConfiguration.GenerateCSharpFile(
                                syntaxContainerConfig: syntaxContainerConfig,
                                compileCSharpClassResult: compiledClass));

                            var returnValue =
                            compileToFileResult
                            .Unpack(
                                fromErr: err =>
                                {
                                    console.WriteLine("Failed compilation:\n" + err, color: Pine.IConsole.TextColor.Red);

                                    return 1;
                                },
                                fromOk: compileSuccess =>
                                {
                                    var outputPath = Path.GetFullPath(compileTo);

                                    var outputDirectory = Path.GetDirectoryName(outputPath);

                                    if (outputDirectory is not null)
                                        Directory.CreateDirectory(outputDirectory);

                                    File.WriteAllText(outputPath, compileSuccess);
                                    console.WriteLine("Saved the compiled code to " + outputPath, color: Pine.IConsole.TextColor.Green);

                                    return 0;
                                });
                        }
                    });
                });

            interactiveCommand.OnExecute(() =>
            {
                var elmEngineType = elmEngineOption.parseElmEngineTypeFromOption();

                Console.WriteLine(
                    "---- Elm Interactive v" + AppVersionId + " using engine based on " + elmEngineType + " ----");

                var contextAppPath = contextAppOption.Value();

                var initStepsPath = initStepsOption.Value();

                TreeNodeWithStringPath? contextAppCodeTree =
                contextAppPath switch
                {
                    null => null,
                    not null =>
                    LoadComposition.LoadFromPathResolvingNetworkDependencies(contextAppPath)
                    .LogToActions(Console.WriteLine)
                    .Map(loaded => loaded.tree)
                    .Unpack(
                        fromErr: error => throw new Exception("Failed to load from path '" + contextAppPath + "': " + error),
                        fromOk: tree =>
                        {
                            if (!tree.EnumerateBlobsTransitive().Take(1).Any())
                                throw new Exception("Found no files under context app path '" + contextAppPath + "'.");

                            return tree;
                        })
                };

                var initStepsSubmission =
                initStepsPath switch
                {
                    null => ImmutableList<string>.Empty,
                    not null =>
                    LoadComposition.LoadFromPathResolvingNetworkDependencies(initStepsPath)
                    .LogToActions(Console.WriteLine)
                    .Map(loaded => loaded.tree)
                    .Unpack(
                        fromErr: error => throw new Exception("Failed to load from path '" + initStepsPath + "': " + error),
                        fromOk: treeNode =>
                        {
                            if (!treeNode.EnumerateBlobsTransitive().Take(1).Any())
                                throw new Exception("Found no files under context app path '" + initStepsPath + "'.");

                            return
                            treeNode
                            .Map(
                                fromBlob: _ => throw new Exception("Unexpected blob"),
                                fromTree: tree =>
                                tree.Select(stepDirectory =>
                                ElmInteractive.TestElmInteractive.ParseScenarioStep(stepDirectory.itemValue)
                                .Extract(fromErr: error => throw new Exception(error)).submission))
                                .ToImmutableList();
                        })
                };

                using var interactiveSession = ElmInteractive.IInteractiveSession.Create(
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
                            Console.WriteLine("Failed to evaluate: " + error);
                            return submission;
                        },
                        fromOk: evalOk =>
                        {
                            if (enableInspectionOption.HasValue())
                            {
                                Console.WriteLine(
                                    "Processing this submission took " +
                                    CommandLineInterface.FormatIntegerForDisplay(evalStopwatch.ElapsedMilliseconds) + " ms.");

                                Console.WriteLine(
                                    "Inspection log has " + (evalOk.inspectionLog?.Count ?? 0) + " entries:\n" +
                                    string.Join("\n", evalOk.inspectionLog.EmptyIfNull()));
                            }

                            Console.WriteLine(evalOk.interactiveResponse.displayText);

                            return submission;
                        });
                }

                var promptPrefix = "> ";

                var allSubmissionsFromArguments =
                initStepsSubmission
                .Concat(submitOption.Values.EmptyIfNull()).WhereNotNull()
                .ToImmutableList();

                if (0 < allSubmissionsFromArguments.Count)
                {
                    Console.WriteLine(allSubmissionsFromArguments.Count + " initial submission(s) from arguments in total...");
                }

                foreach (var submission in allSubmissionsFromArguments)
                {
                    Console.WriteLine(promptPrefix + submission);

                    processSubmission(submission);
                }

                ReadLine.HistoryEnabled = true;

                while (true)
                {
                    var submission = ReadLine.Read(promptPrefix);

                    processSubmission(submission);
                }
            });
        });

    private static CommandLineApplication AddDescribeCommand(CommandLineApplication app) =>
        app.Command("describe", describeCommand =>
        {
            describeCommand.Description = "Describe the artifact at the given location. Valid locations can also be URLs into git repositories or paths in the local file system.";
            describeCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var sourcePathParameter =
                describeCommand
                .Argument("source-path", "Path to the artifact. This can be a local directory or a URL.")
                .IsRequired(allowEmptyStrings: false, errorMessage: "The source argument is missing. From where should I load the artifact?");

            var listBlobsOption =
                describeCommand.Option("--list-blobs", "List blobs in the artifact", CommandOptionType.NoValue);

            var compileZipArchiveOption =
            describeCommand.Option(
                "--compile-zip-archive", "Bundle the composition into a zip-archive.",
                CommandOptionType.SingleOrNoValue);

            describeCommand.OnExecute(() =>
            {
                var sourcePath = sourcePathParameter.Value!;

                var loadCompositionResult =
                    LoadComposition.LoadFromPathResolvingNetworkDependencies(sourcePath)
                    .LogToActions(Console.WriteLine)
                    .Extract(error => throw new Exception("Failed to load from path '" + sourcePath + "': " + error));

                var composition = PineValueComposition.FromTreeWithStringPath(loadCompositionResult.tree);

                var compositionId = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(composition));

                Console.WriteLine("Loaded composition " + compositionId + " from '" + sourcePath + "'.");

                var compositionDescription =
                    string.Join(
                        "\n",
                        DescribeCompositionForHumans(
                            loadCompositionResult.tree,
                            listBlobs: listBlobsOption.HasValue(),
                            extractBlobName: sourcePath.Split('\\', '/').Last()));

                Console.WriteLine("Composition " + compositionId + " is " + compositionDescription);

                if (compileZipArchiveOption.HasValue())
                {
                    var asZipArchive = ZipArchive.ZipArchiveFromEntries(
                        loadCompositionResult.tree.EnumerateBlobsTransitive()
                        .Select(entry => (string.Join("/", entry.path), entry.blobContent)));

                    var defaultFileName = compositionId + ".zip";

                    var destinationPath = compileZipArchiveOption.Value() ?? defaultFileName;

                    if (Directory.Exists(destinationPath))
                        destinationPath = Path.Combine(destinationPath, defaultFileName);

                    File.WriteAllBytes(destinationPath, asZipArchive);
                    Console.WriteLine("Saved " + compositionId[..10] + " to " + destinationPath);
                }

                return 0;
            });
        });

    private static CommandLineApplication AddMakeCommand(CommandLineApplication app) =>
        app.Command("make", makeCommand =>
        {
            makeCommand.Description = "The `make` command compiles Elm code into JS, HTML, or other files.";
            makeCommand.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

            var pathToElmFileArgument =
            makeCommand.Argument("path-to-elm-file", "path to the Elm module file to compile")
            .IsRequired(allowEmptyStrings: false);

            var outputOption =
            makeCommand.Option(
                "--output",
                "Specify the name of the resulting HTML or JS file.",
                CommandOptionType.SingleValue);

            var inputDirectoryOption =
            makeCommand.Option(
                "--input-directory",
                "Specify the input directory containing the Elm files. Defaults to the current working directory.",
                CommandOptionType.SingleValue);

            var debugOption =
            makeCommand.Option(
                "--debug",
                description: "Turn on the time-travelling debugger.",
                CommandOptionType.NoValue);

            var optimizeOption =
            makeCommand.Option(
                "--optimize",
                description: "Turn on optimizations to make code smaller and faster.",
                CommandOptionType.NoValue);

            makeCommand.OnExecute(() =>
            {
                var inputDirectory = inputDirectoryOption.Value() ?? Environment.CurrentDirectory;

                var outputPathArgument = outputOption.Value() ?? "make-default-output.html";

                var loadInputDirectoryResult =
                    LoadComposition.LoadFromPathResolvingNetworkDependencies(inputDirectory)
                    .LogToActions(Console.WriteLine);

                var elmMakeCommandOptions =
                    new[] { debugOption, optimizeOption }
                    .SelectMany(optionToForward =>
                    optionToForward.HasValue() ?
                    new[] { "--" + optionToForward.LongName } :
                    Array.Empty<string>())
                    .ToImmutableList();

                var elmMakeCommandAppendix = string.Join(" ", elmMakeCommandOptions);

                return
                loadInputDirectoryResult
                    .AndThen(loadInputDirectoryOk =>
                    {
                        if (loadInputDirectoryOk.tree.GetNodeAtPath(ImmutableList.Create("elm.json")) is not
                            TreeNodeWithStringPath.BlobNode elmJsonFile)
                            return Result<string, LoadForMakeResult>.err(
                                "Did not find elm.json file in that directory.");

                        var elmJsonFileParsed = System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Bytes.Span);

                        var elmJsonSourceDirectories =
                            elmJsonFileParsed.ParsedSourceDirectories.ToImmutableList();

                        var sourceDirectoriesNotInInputDirectory =
                            elmJsonSourceDirectories
                                .Where(relativeSourceDir => 0 < relativeSourceDir.ParentLevel)
                                .ToImmutableList();

                        var pathToElmFile = pathToElmFileArgument.Value;

                        if (pathToElmFile.StartsWith("./"))
                            pathToElmFile = pathToElmFile.Substring(2);

                        if (!sourceDirectoriesNotInInputDirectory.Any())
                        {
                            return
                                Result<string, LoadForMakeResult>.ok(
                                    new LoadForMakeResult(loadInputDirectoryOk.tree,
                                        ImmutableList<string>.Empty,
                                        pathToElmFile.Replace('\\', '/').Split('/')));
                        }

                        if (loadInputDirectoryOk.origin.FromLocalFileSystem is null)
                        {
                            return
                                Result<string, LoadForMakeResult>.err(
                                    "Failed to work with elm.json file containing directory which is not contained in input directory: This configuration is only supported when loading from a local file system");
                        }

                        string absoluteSourceDirectoryFromRelative(ElmJsonStructure.RelativeDirectory relDir)
                        {
                            var path = inputDirectory;

                            for (var i = 0; i < relDir.ParentLevel; ++i)
                                path = Path.GetDirectoryName(path);

                            return
                                Path.Combine(path, string.Join('/', relDir.Subdirectories));
                        }

                        var outerSourceDirectoriesAbsolute =
                            sourceDirectoriesNotInInputDirectory
                                .Select(absoluteSourceDirectoryFromRelative)
                                .ToImmutableList();

                        var maxParentLevel =
                            sourceDirectoriesNotInInputDirectory.Max(sd => sd.ParentLevel);

                        var commonParentDirectory =
                            absoluteSourceDirectoryFromRelative(
                                new ElmJsonStructure.RelativeDirectory(
                                    ParentLevel: maxParentLevel,
                                    Subdirectories: ImmutableList<string>.Empty));

                        IReadOnlyList<string> pathRelativeToCommonParentFromAbsolute(string absolutePath) =>
                            absolutePath[commonParentDirectory.Length..].Replace('\\', '/').Trim('/').Split('/');

                        var workingDirectoryRelative =
                            pathRelativeToCommonParentFromAbsolute(inputDirectory);

                        var pathToElmFileAbsolute =
                            Path.GetFullPath(pathToElmFile);

                        var pathToFileWithElmEntryPoint =
                            pathRelativeToCommonParentFromAbsolute(pathToElmFileAbsolute)
                                .ToImmutableList();

                        return
                            outerSourceDirectoriesAbsolute
                                .Select(outerSourceDirectory =>
                                {
                                    return
                                        LoadComposition.LoadFromPathResolvingNetworkDependencies(outerSourceDirectory)
                                            .LogToActions(Console.WriteLine)
                                            .Map(outerSourceDirLoadOk =>
                                                (outerSourceDirLoadOk.tree,
                                                    relativePath: pathRelativeToCommonParentFromAbsolute(outerSourceDirectory)));
                                })
                                .ListCombine()
                                .Map(outerSourceDirectories =>
                                {
                                    var combinedTree =
                                        outerSourceDirectories
                                            .Aggregate(
                                                seed:
                                                TreeNodeWithStringPath.EmptyTree
                                                    .SetNodeAtPathSorted(workingDirectoryRelative,
                                                        loadInputDirectoryOk.tree),
                                                func:
                                                (aggregate, nextSourceDir) =>
                                                    aggregate.SetNodeAtPathSorted(nextSourceDir.relativePath,
                                                        nextSourceDir.tree));
                                    return
                                        new LoadForMakeResult(
                                            sourceFiles: combinedTree,
                                            workingDirectoryRelative,
                                            pathToFileWithElmEntryPoint);
                                });
                    })
                .Unpack(
                    fromErr: error =>
                    {
                        DotNetConsoleWriteProblemCausingAbort("Failed to load from path '" + inputDirectory + "': " + error);

                        return 10;
                    },
                    fromOk: loadSourceFilesOk =>
                    {
                        var inputHash = CommonConversion.StringBase16(PineValueHashTree.ComputeHashSorted(loadSourceFilesOk.sourceFiles));

                        Console.WriteLine(
                            "Loaded " + inputHash[..10] + " as input: " +
                            string.Join("\n", DescribeCompositionForHumans(loadSourceFilesOk.sourceFiles, listBlobs: false, extractBlobName: null)));

                        return
                            Make(
                                    sourceFiles: PineValueComposition.TreeToFlatDictionaryWithPathComparer(loadSourceFilesOk.sourceFiles),
                                    workingDirectoryRelative: loadSourceFilesOk.workingDirectoryRelative,
                                    pathToFileWithElmEntryPoint: loadSourceFilesOk.pathToFileWithElmEntryPoint,
                                    outputFileName: Path.GetFileName(outputPathArgument),
                                    elmMakeCommandAppendix: elmMakeCommandAppendix)
                                .Unpack(
                                    fromErr: error =>
                                    {
                                        DotNetConsoleWriteProblemCausingAbort(
                                            "Failed to make " + pathToElmFileArgument.Value + ":\n" + error);
                                        return 20;
                                    },
                                    fromOk: makeOk =>
                                    {
                                        var outputPath = Path.GetFullPath(outputPathArgument);

                                        var outputDirectory = Path.GetDirectoryName(outputPath);

                                        if (outputDirectory is not null)
                                            Directory.CreateDirectory(outputDirectory);

                                        File.WriteAllBytes(outputPath, makeOk.producedFile.ToArray());
                                        Console.WriteLine("Saved the output to " + outputPath);

                                        return 0;
                                    });
                    });
            });
        });

    record LoadForMakeResult(
        TreeNodeWithStringPath sourceFiles,
        IReadOnlyList<string> workingDirectoryRelative,
        IReadOnlyList<string> pathToFileWithElmEntryPoint);

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
        workingDirectoryRelative ??= ImmutableList<string>.Empty;

        var pathToFileWithElmEntryPointFromWorkingDir =
            pathToFileWithElmEntryPoint.Skip(workingDirectoryRelative.Count).ToImmutableList();

        return
            ElmAppCompilation.AsCompletelyLoweredElmApp(
                    sourceFiles: sourceFiles.ToImmutableDictionary(),
                    workingDirectoryRelative: workingDirectoryRelative,
                    interfaceConfig: new ElmAppInterfaceConfig(compilationRootFilePath: pathToFileWithElmEntryPoint))
                .MapError(err =>
                    "Failed lowering Elm code with " + err.Count + " error(s):\n" +
                    ElmAppCompilation.CompileCompilationErrorsDisplayText(err))
                .AndThen(loweringOk =>
                {
                    var sourceFilesAfterLowering = loweringOk.result.compiledFiles;

                    Result<string, Elm019Binaries.ElmMakeOk> continueWithClassicEntryPoint()
                    {
                        return
                            Elm019Binaries.ElmMake(
                                sourceFilesAfterLowering.ToImmutableDictionary(),
                                workingDirectoryRelative: workingDirectoryRelative,
                                pathToFileWithElmEntryPoint: pathToFileWithElmEntryPointFromWorkingDir,
                                outputFileName: outputFileName.Replace('\\', '/').Split('/').Last(),
                                elmMakeCommandAppendix: elmMakeCommandAppendix);
                    }

                    Result<string, Elm019Binaries.ElmMakeOk> continueWithBlobEntryPoint(
                        CompilerSerialInterface.ElmMakeEntryPointStruct entryPointStruct)
                    {
                        return
                            Elm019Binaries.ElmMakeToJavascript(
                                    sourceFilesAfterLowering.ToImmutableDictionary(),
                                    workingDirectoryRelative: workingDirectoryRelative,
                                    pathToFileWithElmEntryPoint: pathToFileWithElmEntryPointFromWorkingDir,
                                    elmMakeCommandAppendix: elmMakeCommandAppendix)
                                .AndThen(makeJavascriptOk =>
                                {
                                    var javascriptFromElmMake =
                                        Encoding.UTF8.GetString(makeJavascriptOk.producedFile.Span);

                                    var javascriptMinusCrashes =
                                        ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

                                    var functionNameInElm = entryPointStruct.elmMakeJavaScriptFunctionName;

                                    var listFunctionToPublish =
                                        new[]
                                        {
                                            (functionNameInElm: functionNameInElm,
                                                publicName: "blob_main_as_base64",
                                                arity: 0),
                                        };

                                    var finalJs =
                                        ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                                            javascriptMinusCrashes,
                                            listFunctionToPublish);

                                    using var javascriptEngine = IJsEngine.BuildJsEngine();

                                    javascriptEngine.Evaluate(finalJs);

                                    var blobBase64 = (string)javascriptEngine.Evaluate("blob_main_as_base64");

                                    return
                                        Result<string, Elm019Binaries.ElmMakeOk>.ok(
                                            new Elm019Binaries.ElmMakeOk(
                                                producedFile: Convert.FromBase64String(blobBase64)));
                                });
                    }

                    return
                        loweringOk.result.rootModuleEntryPointKind
                            .MapError(err => "Failed to get entry point main declaration: " + err)
                            .AndThen(rootModuleEntryPointKind =>
                                rootModuleEntryPointKind switch
                                {
                                    CompilerSerialInterface.ElmMakeEntryPointKind.ClassicMakeEntryPoint =>
                                        continueWithClassicEntryPoint(),
                                    CompilerSerialInterface.ElmMakeEntryPointKind.BlobMakeEntryPoint blob =>
                                        continueWithBlobEntryPoint(blob.EntryPointStruct),

                                    _ => throw new NotImplementedException()
                                });
                });
    }

    public static IEnumerable<string> DescribeCompositionForHumans(
        TreeNodeWithStringPath composition,
        bool listBlobs,
        string? extractBlobName)
    {
        if (composition is TreeNodeWithStringPath.TreeNode tree)
        {
            var blobs = composition.EnumerateBlobsTransitive().ToImmutableList();

            yield return
                "a tree containing " + blobs.Count + " blobs with an aggregate size of " +
                CommandLineInterface.FormatIntegerForDisplay(blobs.Sum(blob => (long)blob.blobContent.Length)) + " bytes.";

            if (listBlobs)
                yield return
                    "blobs paths, sizes and hashes:\n" +
                    string.Join(
                        "\n",
                        blobs.Select(blobAtPath =>
                        string.Join("/", blobAtPath.path) + " : " +
                        blobAtPath.blobContent.Length + " bytes, " +
                        CommonConversion.StringBase16(PineValueHashTree.ComputeHash(PineValue.Blob(blobAtPath.blobContent)))[..10]));

            yield break;
        }

        if (composition is TreeNodeWithStringPath.BlobNode blob)
        {
            yield return "a blob containing " + blob.Bytes.Length + " bytes";

            if (extractBlobName != null)
            {
                foreach (var extractedTree in BlobLibrary.ExtractTreesFromNamedBlob(extractBlobName, blob.Bytes))
                {
                    var extractedTreeCompositionId =
                        CommonConversion.StringBase16(PineValueHashTree.ComputeHashNotSorted(extractedTree));

                    var compositionDescription =
                        string.Join(
                            "\n",
                            DescribeCompositionForHumans(
                                extractedTree,
                                listBlobs: listBlobs,
                                extractBlobName: null));

                    yield return "Extracted composition " + extractedTreeCompositionId + ", which is " + compositionDescription;
                }
            }
        }
    }

    private static CommandLineApplication AddRunCacheServerCmd(CommandLineApplication app) =>
        app.Command("run-cache-server", runCacheServerCmd =>
        {
            runCacheServerCmd.Description = "Run an HTTP server to cache popular parts of git repositories.";
            runCacheServerCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;
            runCacheServerCmd.ShowInHelpText = false;

            var gitCloneUrlPrefixOption =
                runCacheServerCmd
                .Option("--git-clone-prefix", "Prefix of URL from which git cloning is enabled.", CommandOptionType.MultipleValue);

            var urlOption =
                runCacheServerCmd
                .Option("--url", "URL for the HTTP server", CommandOptionType.MultipleValue);

            var fileCacheDirectoryOption =
                runCacheServerCmd
                .Option("--file-cache-directory", "Directory in the file system to store cache entries.", CommandOptionType.SingleValue);

            runCacheServerCmd.OnExecute(() =>
            {
                var urls = urlOption.Values!;
                var gitCloneUrlPrefixes = gitCloneUrlPrefixOption.Values!;
                var fileCacheDirectory = fileCacheDirectoryOption.Value()!;

                Console.WriteLine("Starting HTTP server with git cache...");

                var serverTask = GitPartialForCommitServer.Run(
                    urls: urls!,
                    gitCloneUrlPrefixes: gitCloneUrlPrefixes!,
                    fileCacheDirectory: fileCacheDirectory);

                Console.WriteLine("Completed starting HTTP server with git cache at '" + string.Join(", ", urls) + "'.");

                serverTask.Wait();
            });
        });

    private static Func<string?, string?> SitePasswordFromSiteFromOptionOnCommandOrFromSettings(
        CommandLineApplication cmd, string? siteName = null)
    {
        siteName ??= "site";

        var sitePasswordOption = cmd.Option("--" + siteName + "-password", "Password to access the " + siteName + ".", CommandOptionType.SingleValue);

        return site => site == null ? null : sitePasswordOption.Value() ?? UserSecrets.LoadPasswordForSite(site);
    }

    private static CommandArgument ProcessSiteArgumentOnCommand(CommandLineApplication cmd) =>
        cmd
        .Argument("process-site", "Path to the admin interface of the server running the process.")
        .IsRequired(allowEmptyStrings: false);

    private static (CommandOption elmEngineOption, Func<ElmInteractive.ElmEngineType> parseElmEngineTypeFromOption) AddElmEngineOptionOnCommand(
        CommandLineApplication cmd,
        string? defaultFromEnvironmentVariablePrefix,
        Func<ElmInteractive.ElmEngineType?, ElmInteractive.ElmEngineType> defaultEngineConsideringEnvironmentVariable)
    {
        var defaultEngineFromEnvironmentVarible =
            defaultFromEnvironmentVariablePrefix switch
            {
                string variablePrefix => ElmEngineFromEnvironmentVariableWithPrefix(variablePrefix),
                null => null
            };

        var defaultEngine = defaultEngineConsideringEnvironmentVariable(defaultEngineFromEnvironmentVarible);

        var elmEngineOption =
            cmd.Option(
                template: "--elm-engine",
                description: "Select the engine for running Elm programs (" + string.Join(", ", Enum.GetNames<ElmInteractive.ElmEngineType>()) + "). Defaults to " + defaultEngine,
                optionType: CommandOptionType.SingleValue,
                inherited: true);

        ElmInteractive.ElmEngineType parseElmEngineTypeFromOption()
        {
            if (elmEngineOption?.Value() is string implementationAsString)
                return Enum.Parse<ElmInteractive.ElmEngineType>(implementationAsString, ignoreCase: true);

            return defaultEngine;
        }

        return (elmEngineOption, parseElmEngineTypeFromOption);
    }

    public static ElmInteractive.ElmEngineType? ElmEngineFromEnvironmentVariableWithPrefix(string? environmentVariablePrefix)
    {
        var environmentVariable =
            environmentVariablePrefix?.TrimEnd('_') +
            (environmentVariablePrefix is null ? "" : "_") +
            "elm_engine";

        if (Environment.GetEnvironmentVariable(environmentVariable) is not string asString)
            return null;

        if (Enum.TryParse<ElmInteractive.ElmEngineType>(asString, ignoreCase: true, out var result))
            return result;

        return null;
    }

    public static string ElmMakeHomeDirectoryPath =>
        Path.Combine(Filesystem.CacheDirectory, "elm-make-home");

    public static IFileStore ElmMakeResultCacheFileStoreDefault =>
        new FileStoreFromSystemIOFile(
            Path.Combine(Filesystem.CacheDirectory, "elm-make-result-cache", AppVersionId));

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

    private static (string compositionId, SourceSummaryStructure summary) CompileSourceSummary(TreeNodeWithStringPath sourceTree)
    {
        var compositionId = CommonConversion.StringBase16(PineValueHashTree.ComputeHashSorted(sourceTree));

        var allBlobs = sourceTree.EnumerateBlobsTransitive().ToImmutableList();

        return (compositionId, summary: new SourceSummaryStructure
        (
            numberOfFiles: allBlobs.Count,
            totalSizeOfFilesContents: allBlobs.Select(blob => blob.blobContent.Length).Sum()
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
        var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        Console.WriteLine("Beginning to build configuration...");

        var buildResult =
            Platform.WebServer.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                sourcePath: sourcePath);

        var (sourceCompositionId, sourceSummary) = CompileSourceSummary(buildResult.sourceTree);

        var appConfigZipArchive = buildResult.configZipArchive;

        var compiledCompositionId =
            CommonConversion.StringBase16(
                PineValueHashTree.ComputeHashSorted(PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                    ZipArchive.EntriesFromZipArchive(appConfigZipArchive))));

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
                    Platform.WebServer.StartupAdminInterface.PathApiDeployAndInitAppState
                    :
                    Platform.WebServer.StartupAdminInterface.PathApiDeployAndMigrateAppState);

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
                    new Platform.WebServer.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                        processStoreFileStore,
                        getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                        processStoreFileStore);

                var appConfigTree =
                    PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

                var appConfigComponent = PineValueComposition.FromTreeWithStringPath(appConfigTree);

                processStoreWriter.StoreComponent(appConfigComponent);

                var appConfigValueInFile =
                    new Platform.WebServer.ProcessStoreSupportingMigrations.ValueInFileStructure
                    {
                        HashBase16 = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(appConfigComponent))
                    };

                var compositionLogEvent =
                    Platform.WebServer.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                        appConfigValueInFile: appConfigValueInFile,
                        initElmAppState: initElmAppState);

                var (statusCode, responseReport) =
                    Platform.WebServer.StartupAdminInterface.AttemptContinueWithCompositionEventAndCommit(
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
        var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

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
                site.TrimEnd('/') + Platform.WebServer.StartupAdminInterface.PathApiListDatabaseFunctions;

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
                return
                    Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>>.err(
                        "HTTP response status code not OK: " + httpResponse.StatusCode + ", content:\n" +
                        responseContentString);
            }

            return
                System.Text.Json.JsonSerializer.Deserialize<Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>>>(responseContentString)!
                .MapError(err => "Server returned error: " + err);
        }
        catch (Exception e)
        {
            return
                Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>>.err(
                    "Failed with runtime exception:\n" + e);
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
        var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

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
                site.TrimEnd('/') + Platform.WebServer.StartupAdminInterface.PathApiApplyDatabaseFunction;

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

    private static bool LooksLikeLocalSite(string site)
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
            beginTime: CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow)
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
        var appStateId = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(appStateComponent));

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
        var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var elmAppStateComponent = PineValue.Blob(elmAppStateSerialized);

        var elmAppStateId = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(elmAppStateComponent));

        var httpResponse = AttemptHttpRequest(() =>
            {
                var httpContent = new System.Net.Http.ByteArrayContent(elmAppStateSerialized);

                httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/json");

                return new System.Net.Http.HttpRequestMessage
                {
                    Method = System.Net.Http.HttpMethod.Post,
                    RequestUri = MapUriForForAdminInterface(site.TrimEnd('/') + Platform.WebServer.StartupAdminInterface.PathApiElmAppState),
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
        var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var httpResponse = AttemptHttpRequest(() =>
            {
                return new System.Net.Http.HttpRequestMessage
                {
                    Method = System.Net.Http.HttpMethod.Get,
                    RequestUri = MapUriForForAdminInterface(site.TrimEnd('/') + Platform.WebServer.StartupAdminInterface.PathApiElmAppState),
                };
            },
            defaultPassword: siteDefaultPassword,
            promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

        Console.WriteLine("Server response status code: " + httpResponse.StatusCode);

        var elmAppStateSerialized = httpResponse.Content.ReadAsByteArrayAsync().Result;

        var elmAppStateComponent = PineValue.Blob(elmAppStateSerialized);
        var elmAppStateId = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(elmAppStateComponent));

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
        var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var requestUrl =
            site.TrimEnd('/') + Platform.WebServer.StartupAdminInterface.PathApiTruncateProcessHistory;

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

    private static (IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> files, string lastCompositionLogRecordHashBase16) ReadFilesForRestoreProcessFromAdminInterface(
        string sourceAdminInterface,
        string? sourceAdminPassword)
    {
        using var sourceHttpClient = new System.Net.Http.HttpClient { BaseAddress = new Uri(sourceAdminInterface) };

        sourceHttpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
            "Basic",
            Convert.ToBase64String(Encoding.UTF8.GetBytes(BasicAuthenticationForAdmin(sourceAdminPassword))));

        var processHistoryFileStoreRemoteReader = new DelegatingFileStoreReader
        (
            ListFilesInDirectoryDelegate: directoryPath =>
            {
                var httpRequestPath =
                    Platform.WebServer.StartupAdminInterface.PathApiProcessHistoryFileStoreListFilesInDirectory + "/" +
                    string.Join("/", directoryPath);

                var response = sourceHttpClient.GetAsync(httpRequestPath).Result;

                if (!response.IsSuccessStatusCode)
                    throw new Exception("Unexpected response status code: " + (int)response.StatusCode + " (" + response.StatusCode + ").");

                return
                    response.Content.ReadAsStringAsync().Result.Split('\n', StringSplitOptions.RemoveEmptyEntries)
                    .Select(path => path.Split('/').ToImmutableList());
            },
            GetFileContentDelegate: filePath =>
            {
                var httpRequestPath =
                    Platform.WebServer.StartupAdminInterface.PathApiProcessHistoryFileStoreGetFileContent + "/" +
                    string.Join("/", filePath);

                var response = sourceHttpClient.GetAsync(httpRequestPath).Result;

                if (response.StatusCode == System.Net.HttpStatusCode.NotFound)
                    return null;

                if (!response.IsSuccessStatusCode)
                    throw new Exception("Unexpected response status code: " + (int)response.StatusCode + " (" + response.StatusCode + ").");

                return response.Content.ReadAsByteArrayAsync().Result;
            }
        );

        return Platform.WebServer.PersistentProcessLiveRepresentation.GetFilesForRestoreProcess(processHistoryFileStoreRemoteReader);
    }

    private static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> LoadFilesForRestoreFromPathAndLogToConsole(
        string sourcePath, string? sourcePassword)
    {
        if (!LooksLikeLocalSite(sourcePath))
        {
            Console.WriteLine("Begin reading process history from '" + sourcePath + "' ...");

            var (files, lastCompositionLogRecordHashBase16) = ReadFilesForRestoreProcessFromAdminInterface(
                sourceAdminInterface: sourcePath,
                sourceAdminPassword: sourcePassword);

            Console.WriteLine("Completed reading files to restore process " + lastCompositionLogRecordHashBase16 + ". Read " + files.Count + " files from '" + sourcePath + "'.");

            return files;
        }

        var archive = File.ReadAllBytes(sourcePath);

        var zipArchiveEntries = ZipArchive.EntriesFromZipArchive(archive);

        return
            PineValueComposition.ToFlatDictionaryWithPathComparer(
                PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(zipArchiveEntries)
                .EnumerateBlobsTransitive());
    }

    public static void ReplicateProcessAndLogToConsole(
        string site,
        string sitePassword,
        string sourcePath,
        string sourcePassword)
    {
        var restoreFiles =
            LoadFilesForRestoreFromPathAndLogToConsole(sourcePath: sourcePath, sourcePassword: sourcePassword);

        var processHistoryTree =
            PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(restoreFiles);

        var processHistoryComponentHash = PineValueHashTree.ComputeHashNotSorted(processHistoryTree);
        var processHistoryComponentHashBase16 = CommonConversion.StringBase16(processHistoryComponentHash);

        var processHistoryZipArchive = ZipArchive.ZipArchiveFromEntries(restoreFiles);

        using var httpClient = new System.Net.Http.HttpClient();

        httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
            "Basic",
            Convert.ToBase64String(Encoding.UTF8.GetBytes(BasicAuthenticationForAdmin(sitePassword))));

        var deployAddress =
            site.TrimEnd('/') +
            Platform.WebServer.StartupAdminInterface.PathApiReplaceProcessHistory;

        Console.WriteLine("Beginning to place process history '" + processHistoryComponentHashBase16 + "' at '" + deployAddress + "'...");

        var httpContent = new System.Net.Http.ByteArrayContent(processHistoryZipArchive);

        httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/zip");
        httpContent.Headers.ContentDisposition =
            new System.Net.Http.Headers.ContentDispositionHeaderValue("attachment") { FileName = processHistoryComponentHashBase16 + ".zip" };

        var httpResponse = httpClient.PostAsync(deployAddress, httpContent).Result;

        Console.WriteLine(
            "Server response: " + httpResponse.StatusCode + "\n" +
             httpResponse.Content.ReadAsStringAsync().Result);
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
                var destinationExecutableFilePath = "/bin/" + commandName;

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
                    File.WriteAllBytes(destinationExecutableFilePath, currentExecuableFileContent);

                    var unixFileInfo = new Mono.Unix.UnixFileInfo(destinationExecutableFilePath);

                    unixFileInfo.FileAccessPermissions |=
                        Mono.Unix.FileAccessPermissions.GroupExecute | Mono.Unix.FileAccessPermissions.UserExecute | Mono.Unix.FileAccessPermissions.OtherExecute |
                        Mono.Unix.FileAccessPermissions.GroupRead | Mono.Unix.FileAccessPermissions.UserRead | Mono.Unix.FileAccessPermissions.OtherRead;

                    Console.WriteLine(
                        "I copied the executable file to '" + destinationExecutableFilePath +
                        "'. You will be able to use the '" + commandName + "' command in newer terminal instances.");
                });

                return (executableIsRegisteredOnPath, registerExecutableForCurrentUser);
            }
        };

        return (commandName, checkInstallation);
    }

    private static string ReportFilePath => Path.Combine(Environment.CurrentDirectory, "elm-time-tool", "report");

    private static void WriteReportToFileInReportDirectory(string reportContent, string reportKind)
    {
        var fileName = CommonConversion.TimeStringViewForReport(programStartTime) + "_" + reportKind;

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
                new System.Net.Http.Headers.ProductHeaderValue("elm-time-cli", AppVersionId)));

        return httpRequest;
    }

    private static readonly DateTimeOffset programStartTime = DateTimeOffset.UtcNow;

    public static (ExecutableFile.ProcessOutput processOutput, IReadOnlyList<(string rawLine, ElmTestRsReportJsonEntry parsedLine)> stdoutLines)
        CompileAndElmTestRs(string source)
    {
        var (_, compiledAppFiles) = CompileApp(source);

        if (compiledAppFiles == null)
            throw new Exception("Compilation failed");

        return ElmTestRs.Run(compiledAppFiles);
    }
}
