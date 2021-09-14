using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using McMaster.Extensions.CommandLineUtils;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Pine;
using static ElmFullstack.WebHost.Configuration;

namespace elm_fullstack
{
    public class Program
    {
        static public string AppVersionId => "2021-09-13";

        static int AdminInterfaceDefaultPort => 4000;

        static int Main(string[] args)
        {
            ElmFullstack.ProcessFromElm019Code.overrideElmMakeHomeDirectory = ElmMakeHomeDirectoryPath;

            var app = new CommandLineApplication
            {
                Name = "elm-fs",
                Description = "Elm Fullstack - full-stack web apps made simple.\nTo get help or report an issue, see https://github.com/elm-fullstack/elm-fullstack/discussions",
            };

            app.HelpTextGenerator =
                new McMaster.Extensions.CommandLineUtils.HelpText.DefaultHelpTextGenerator { SortCommandsByName = false };

            app.VersionOption(template: "-v|--version", shortFormVersion: "version " + AppVersionId);

            var installCmd = app.Command("install", installCmd =>
            {
                var (commandName, checkInstallation) = CheckIfExecutableIsRegisteredOnPath();

                installCmd.Description = "Installs the '" + commandName + "' command for the current user account.";
                installCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                installCmd.OnExecute(() =>
                {
                    checkInstallation().registerExecutableDirectoryOnPath();
                });
            });

            var runServerCmd = AddRunServerCmd(app);

            var deployAppCmd = AddDeployCmd(app);
            var copyAppStateCmd = AddCopyAppStateCmd(app);
            var copyProcessCmd = AddCopyProcessCmd(app);
            var truncateProcessHistoryCmd = AddTruncateProcessHistoryCmd(app);

            var compileAppCmd = AddCompileCmd(app);
            var enterInteractiveCmd = AddInteractiveCmd(app);
            var describeCmd = AddDescribeCmd(app);

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
                        UserSecrets.StorePasswordForSite(siteArgument.Value, passwordArgument.Value);
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
                    new CommandLineApplication[0] :
                    new[]
                    {
                        installCmd,
                    };

                var commonCmdGroups = new[]
                {
                    new
                    {
                        title = "Set up your development environment",
                        commands = setupGroupCommands,
                    },
                    new
                    {
                        title = "operate servers and maintain live systems",
                        commands = new[]
                        {
                            runServerCmd,
                            deployAppCmd,
                            copyAppStateCmd,
                            copyProcessCmd,
                            truncateProcessHistoryCmd,
                        }
                    },
                    new
                    {
                        title = "develop and learn",
                        commands = new[]
                        {
                            compileAppCmd,
                            enterInteractiveCmd,
                            describeCmd,
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
                        nameColumn = cmd.FullName ?? cmd.Name,
                        descriptionColumn = cmd.Description,
                    }).ToImmutableList(),
                });

                foreach (var appCmd in app.Commands)
                {
                    var cmdPrimaryName = appCmd.Names.FirstOrDefault();

                    helpCmd.Command(cmdPrimaryName, helpForAppCmd =>
                    {
                        foreach (var additionalName in appCmd.Names.Except(new[] { cmdPrimaryName }))
                            helpForAppCmd.AddName(additionalName);

                        CommandExtension.ConfigureHelpCommandForCommand(helpForAppCmd, appCmd);
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
                            "These are common elm-fs commands used in various situations:",
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

        static CommandLineApplication AddRunServerCmd(CommandLineApplication app) =>
            app.Command("run-server", runServerCmd =>
            {
                runServerCmd.Description = "Run a server with a web-based admin interface. This admin interface supports deployments, migrations, etc.";
                runServerCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var adminUrlsDefault = "http://*:" + AdminInterfaceDefaultPort;
                string[] publicWebHostUrlsDefault = new[] { "http://*", "https://*" };


                var processStorePathOption = runServerCmd.Option("--process-store-path", "Directory in the file system to contain the process store.", CommandOptionType.SingleValue);
                var deletePreviousProcessOption = runServerCmd.Option("--delete-previous-process", "Delete the previous backend process found in the given store. If you don't use this option, the server restores the process from the persistent store on startup.", CommandOptionType.NoValue);
                var adminUrlsOption = runServerCmd.Option("--admin-urls", "URLs for the admin interface. The default is " + adminUrlsDefault.ToString() + ".", CommandOptionType.SingleValue);
                var adminPasswordOption = runServerCmd.Option("--admin-password", "Password for the admin interface at '--admin-urls'.", CommandOptionType.SingleValue);
                var publicAppUrlsOption = runServerCmd.Option("--public-urls", "URLs to serve the public app from. The default is '" + string.Join(",", publicWebHostUrlsDefault) + "'.", CommandOptionType.SingleValue);
                var copyProcessOption = runServerCmd.Option("--copy-process", "Path to a process to copy. Can be a URL to an admin interface of a server or a path to an archive containing files representing the process state. This option also implies '--delete-previous-process'.", CommandOptionType.SingleValue);
                var deployOption = runServerCmd.Option("--deploy", "Path to an app to deploy on startup, analogous to the 'source' path on the `deploy` command. Can be combined with '--copy-process'.", CommandOptionType.SingleValue);

                runServerCmd.OnExecute(() =>
                {
                    var processStorePath = processStorePathOption.Value();

                    var publicAppUrls =
                        publicAppUrlsOption.Value()?.Split(',').Select(url => url.Trim()).ToArray() ??
                        publicWebHostUrlsDefault;

                    var copyProcess = copyProcessOption.Value();

                    if ((deletePreviousProcessOption.HasValue() || copyProcess != null) && processStorePath != null)
                    {
                        Console.WriteLine("Deleting the previous process state from '" + processStorePath + "'...");

                        if (System.IO.Directory.Exists(processStorePath))
                            System.IO.Directory.Delete(processStorePath, true);

                        Console.WriteLine("Completed deleting the previous process state from '" + processStorePath + "'.");
                    }

                    IFileStore processStoreFileStore = null;

                    if (processStorePath == null)
                    {
                        Console.WriteLine("I got no path to a persistent store for the process. This process will not be persisted!");

                        var files = new System.Collections.Concurrent.ConcurrentDictionary<IImmutableList<string>, IReadOnlyList<byte>>(EnumerableExtension.EqualityComparer<string>());

                        var fileStoreWriter = new DelegatingFileStoreWriter
                        {
                            SetFileContentDelegate = file => files[file.path] = file.fileContent,
                            AppendFileContentDelegate = file => files.AddOrUpdate(
                                file.path, _ => file.fileContent,
                                (_, fileBefore) => fileBefore.Concat(file.fileContent).ToArray()),
                            DeleteFileDelegate = path => files.Remove(path, out var _)
                        };

                        var fileStoreReader = new DelegatingFileStoreReader
                        {
                            ListFilesInDirectoryDelegate = path =>
                                files.Select(file =>
                                {
                                    if (!file.Key.Take(path.Count).SequenceEqual(path))
                                        return null;

                                    return file.Key.Skip(path.Count).ToImmutableList();
                                }).WhereNotNull(),
                            GetFileContentDelegate = path =>
                            {
                                files.TryGetValue(path, out var fileContent);

                                return fileContent;
                            }
                        };

                        processStoreFileStore = new FileStoreFromWriterAndReader(fileStoreWriter, fileStoreReader);
                    }
                    else
                    {
                        processStoreFileStore = new FileStoreFromSystemIOFile(processStorePath);
                    }

                    if (copyProcess != null)
                    {
                        var copyFiles =
                            LoadFilesForRestoreFromPathAndLogToConsole(
                                sourcePath: copyProcess,
                                sourcePassword: null);

                        foreach (var file in copyFiles)
                            processStoreFileStore.SetFileContent(file.Key, file.Value.ToArray());
                    }

                    var adminInterfaceUrls = adminUrlsOption.Value() ?? adminUrlsDefault;

                    if (deployOption.HasValue())
                    {
                        Console.WriteLine("Loading app config to deploy...");

                        var appConfigZipArchive =
                            ElmFullstack.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                                sourcePath: deployOption.Value()).configZipArchive;

                        var appConfigTree =
                            Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                                ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

                        var appConfigComponent = Composition.FromTreeWithStringPath(appConfigTree);

                        var processStoreWriter =
                            new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                                processStoreFileStore,
                                getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                                processStoreFileStore);

                        processStoreWriter.StoreComponent(appConfigComponent);

                        var appConfigValueInFile =
                            new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfigComponent))
                            };

                        var initElmAppState =
                            deletePreviousProcessOption.HasValue() && !copyProcessOption.HasValue();

                        var compositionLogEvent =
                            ElmFullstack.WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                                appConfigValueInFile: appConfigValueInFile,
                                initElmAppState: initElmAppState);

                        var testDeployResult = ElmFullstack.WebHost.PersistentProcess.PersistentProcessLiveRepresentation.TestContinueWithCompositionEvent(
                            compositionLogEvent: compositionLogEvent,
                            fileStoreReader: processStoreFileStore);

                        if (testDeployResult.Ok.projectedFiles == null)
                        {
                            throw new Exception("Attempt to deploy app config failed: " + testDeployResult.Err);
                        }

                        foreach (var (filePath, fileContent) in testDeployResult.Ok.projectedFiles)
                            processStoreFileStore.SetFileContent(filePath, fileContent);
                    }

                    var webHostBuilder =
                        Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                        .ConfigureAppConfiguration(builder => builder.AddEnvironmentVariables("APPSETTING_"))
                        .UseUrls(adminInterfaceUrls)
                        .UseStartup<ElmFullstack.WebHost.StartupAdminInterface>()
                        .WithSettingPublicWebHostUrls(publicAppUrls)
                        .WithProcessStoreFileStore(processStoreFileStore);

                    if (adminPasswordOption.HasValue())
                        webHostBuilder = webHostBuilder.WithSettingAdminPassword(adminPasswordOption.Value());

                    var webHost = webHostBuilder.Build();

                    Console.WriteLine("Starting the web server with the admin interface...");

                    webHost.Start();

                    Console.WriteLine("Completed starting the web server with the admin interface at '" + adminInterfaceUrls + "'.");

                    WebHostExtensions.WaitForShutdown(webHost);
                });
            });

        static CommandLineApplication AddDeployCmd(CommandLineApplication app) =>
            app.Command("deploy", deployCmd =>
            {
                deployCmd.Description = "Deploy an app to an Elm backend process. Deployment implies migration from the previous app state if not specified otherwise.";
                deployCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var sourceArgument = deployCmd.Argument("source", "Path to the app program code to deploy.").IsRequired(allowEmptyStrings: false);

                var siteArgument = ProcessSiteArgumentOnCommand(deployCmd);
                var passwordFromSite = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(deployCmd);

                var initAppStateOption = deployCmd.Option("--init-app-state", "Do not attempt to migrate the Elm app state but use the state from the init function.", CommandOptionType.NoValue);

                deployCmd.OnExecute(() =>
                {
                    var site = siteArgument.Value;
                    var sitePassword = passwordFromSite(site);

                    var deployReport =
                        DeployApp(
                            sourcePath: sourceArgument.Value,
                            site: site,
                            siteDefaultPassword: sitePassword,
                            initElmAppState: initAppStateOption.HasValue(),
                            promptForPasswordOnConsole: true);

                    WriteReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(deployReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "deploy.json");
                });
            });

        static CommandLineApplication AddCopyAppStateCmd(CommandLineApplication app) =>
            app.Command("copy-app-state", copyAppStateCmd =>
            {
                copyAppStateCmd.Description = "Copy the state of an Elm backend app.";
                copyAppStateCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var sourceArgument = copyAppStateCmd.Argument("source", "Can be a URL to an admin interface or a file with a serialized representation.").IsRequired(allowEmptyStrings: false);
                var destinationArgument = copyAppStateCmd.Argument("destination", "Can be a URL to an admin interface or a file path.");

                var passwordFromSource = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(copyAppStateCmd, "source");
                var passwordFromDestination = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(copyAppStateCmd, "destination");

                copyAppStateCmd.OnExecute(() =>
                {
                    var source = sourceArgument.Value;
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
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(report, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "copy-app-state.json");
                });
            });

        static CommandLineApplication AddCopyProcessCmd(CommandLineApplication app) =>
            app.Command("copy-process", copyProcessCmd =>
            {
                copyProcessCmd.Description = "Copy all files needed to restore a process and store them in a zip archive.";
                copyProcessCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var siteArgument = ProcessSiteArgumentOnCommand(copyProcessCmd);
                var passwordFromSite = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(copyProcessCmd);

                copyProcessCmd.OnExecute(() =>
                {
                    var site = MapSiteForCommandLineArgument(siteArgument.Value);
                    var sitePassword = passwordFromSite(site);

                    sitePassword =
                        AttemptHttpRequest(
                        () => new System.Net.Http.HttpRequestMessage { RequestUri = new Uri(site) },
                        defaultPassword: sitePassword,
                        promptForPasswordOnConsole: true).Result.enteredPassword ?? sitePassword;

                    Console.WriteLine("Begin reading process history from '" + site + "' ...");

                    var (files, lastCompositionLogRecordHashBase16) =
                        ReadFilesForRestoreProcessFromAdminInterface(site, sitePassword);

                    Console.WriteLine("Completed reading files to restore process " + lastCompositionLogRecordHashBase16 + ". Read " + files.Count + " files from '" + site + "'.");

                    var zipArchive = ZipArchive.ZipArchiveFromEntries(files);

                    var fileName = "process-" + lastCompositionLogRecordHashBase16 + ".zip";
                    var filePath = Path.Combine(Environment.CurrentDirectory, fileName);

                    File.WriteAllBytes(filePath, zipArchive);

                    Console.WriteLine("Saved process archive to file '" + filePath + "'.");
                });
            });

        static CommandLineApplication AddTruncateProcessHistoryCmd(CommandLineApplication app) =>
            app.Command("truncate-process-history", truncateProcessHistoryCmd =>
            {
                truncateProcessHistoryCmd.Description = "Remove parts of the process history that are not needed to restore the process.";
                truncateProcessHistoryCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var siteArgument = ProcessSiteArgumentOnCommand(truncateProcessHistoryCmd);
                var passwordFromSite = SitePasswordFromSiteFromOptionOnCommandOrFromSettings(truncateProcessHistoryCmd);

                truncateProcessHistoryCmd.OnExecute(() =>
                {
                    var site = siteArgument.Value;
                    var sitePassword = passwordFromSite(site);

                    var report =
                        TruncateProcessHistory(
                            site: site,
                            siteDefaultPassword: sitePassword,
                            promptForPasswordOnConsole: true);

                    WriteReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(report, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "truncate-process-history.json");
                });
            });

        static CommandLineApplication AddCompileCmd(CommandLineApplication app) =>
            app.Command("compile", compileCmd =>
            {
                compileCmd.Description = "Compile app source code the same way as would be done when deploying.";
                compileCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var sourceArgument = compileCmd.Argument("source", "Path to the app program code to compile.").IsRequired(allowEmptyStrings: false);

                compileCmd.OnExecute(() =>
                {
                    var compileReport = CompileApp(sourceArgument.Value);

                    WriteReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(compileReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "compile.json");
                });
            });

        static CompileAppReport CompileApp(string sourcePath)
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

            var loadFromPathResult = LoadFromPath.LoadTreeFromPath(sourcePath);

            if (loadFromPathResult?.Ok == null)
            {
                throw new Exception("Failed to load from path '" + sourcePath + "': " + loadFromPathResult?.Err);
            }

            var (sourceCompositionId, sourceSummary) = CompileSourceSummary(loadFromPathResult.Ok.tree);

            report = report with { sourceCompositionId = sourceCompositionId, sourceSummary = sourceSummary };

            Console.WriteLine("Loaded source composition " + sourceCompositionId + " from '" + sourcePath + "'. Starting to compile...");

            var compilationStopwatch = System.Diagnostics.Stopwatch.StartNew();

            try
            {
                var sourceFiles =
                    Composition.TreeToFlatDictionaryWithPathComparer(loadFromPathResult.Ok.tree);

                var compilationResult = ElmFullstack.ElmAppCompilation.AsCompletelyLoweredElmApp(
                    sourceFiles: sourceFiles,
                    ElmFullstack.ElmAppInterfaceConfig.Default);

                var compilationTimeSpentMilli = compilationStopwatch.ElapsedMilliseconds;

                report = report with { compilationTimeSpentMilli = (int)compilationTimeSpentMilli };

                if (compilationResult.Ok == null)
                {
                    Console.WriteLine("\n" + ElmFullstack.ElmAppCompilation.CompileCompilationErrorsDisplayText(compilationResult.Err) + "\n");

                    return report with { compilationErrors = compilationResult.Err, totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds };
                }

                var compiledTree = Composition.SortedTreeFromSetOfBlobsWithStringPath(compilationResult.Ok.compiledAppFiles);
                var compiledComposition = Composition.FromTreeWithStringPath(compiledTree);
                var compiledCompositionId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(compiledComposition));

                compilationStopwatch.Stop();

                Console.WriteLine(
                    "\nCompilation completed in " + (int)compilationStopwatch.Elapsed.TotalSeconds +
                    " seconds, resulting in composition " + compiledCompositionId + ".");

                var compiledFiles =
                    Composition.TreeToFlatDictionaryWithPathComparer(compiledTree);

                var compiledCompositionArchive =
                    ZipArchive.ZipArchiveFromEntries(compiledFiles);

                var outputCompositionFileName = compiledCompositionId + ".zip";

                var outputCompositionFilePath = Path.Combine(ReportFilePath, outputCompositionFileName);

                Directory.CreateDirectory(Path.GetDirectoryName(outputCompositionFilePath));
                File.WriteAllBytes(outputCompositionFilePath, compiledCompositionArchive);
                Console.WriteLine("\nSaved compiled composition " + compiledCompositionId + " to '" + outputCompositionFilePath + "'.");

                return report with
                {
                    compilationIterationsReports = compilationResult.Ok.iterationsReports,
                    compiledCompositionId = compiledCompositionId,
                    totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds
                };
            }
            catch (Exception e)
            {
                report = report with { compilationTimeSpentMilli = (int)compilationStopwatch.Elapsed.TotalMilliseconds };

                Console.WriteLine("Compilation failed with runtime exception: " + e.ToString());
                return report with { compilationException = e.ToString(), totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds };
            }
        }

        static CommandLineApplication AddInteractiveCmd(CommandLineApplication app) =>
            app.Command("interactive", enterInteractiveCmd =>
            {
                enterInteractiveCmd.Description = "Enter an environment for interactive exploration and composition of Elm programs.";

                var contextAppOption =
                    enterInteractiveCmd
                    .Option(
                        template: "--context-app",
                        description: "Path to an app to use as context. The Elm modules from this app will be available in the interactive environment.",
                        optionType: CommandOptionType.SingleValue);

                var enableInspectionOption =
                    enterInteractiveCmd
                    .Option(
                        template: "--enable-inspection",
                        description: "Display additional information to inspect the implementation.",
                        optionType: CommandOptionType.NoValue);

                enterInteractiveCmd.OnExecute(() =>
                {
                    ReadLine.HistoryEnabled = true;

                    Console.WriteLine(
                        "---- Elm Interactive v" + AppVersionId + " ----");

                    Composition.TreeWithStringPath contextAppCodeTree = null;

                    var contextAppPath = contextAppOption.Value();

                    if (contextAppPath != null)
                    {
                        var loadContextAppResult = LoadFromPath.LoadTreeFromPath(contextAppPath);

                        if (loadContextAppResult?.Ok == null)
                        {
                            throw new Exception("Failed to load from path '" + contextAppPath + "': " + loadContextAppResult?.Err);
                        }

                        contextAppCodeTree = loadContextAppResult.Ok.tree;

                        if (!(0 < contextAppCodeTree?.EnumerateBlobsTransitive().Take(1).Count()))
                            throw new Exception("Found no files under context app path '" + contextAppPath + "'.");
                    }

                    using var interactiveSession = new ElmInteractive.InteractiveSession(appCodeTree: contextAppCodeTree);

                    while (true)
                    {
                        var submission = ReadLine.Read("> ");

                        if (!(0 < submission?.Trim()?.Length))
                            continue;

                        var evalStopwatch = System.Diagnostics.Stopwatch.StartNew();

                        var evalResult =
                            interactiveSession.SubmitAndGetResultingValue(submission);

                        evalStopwatch.Stop();

                        if (evalResult.Ok == null)
                        {
                            Console.WriteLine("Failed to evaluate: " + evalResult.Err);
                            continue;
                        }

                        if (enableInspectionOption.HasValue())
                        {
                            Console.WriteLine(
                                "Evaluation took " +
                                evalStopwatch.ElapsedMilliseconds.ToString("### ### ###") + " ms.");
                        }

                        Console.WriteLine(evalResult.Ok.valueAsElmExpressionText);
                    }
                });
            });

        static CommandLineApplication AddDescribeCmd(CommandLineApplication app) =>
            app.Command("describe", describeCmd =>
            {
                describeCmd.Description = "Describe the artifact at the given path. Valid paths can be URLs into git repositories or in the local file system.";
                describeCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var sourcePathParameter =
                    describeCmd
                    .Argument("source-path", "Path to the artifact. This can be a local directory or a URL.")
                    .IsRequired(allowEmptyStrings: false, errorMessage: "The source argument is missing. From where should I load the artifact?");

                describeCmd.OnExecute(() =>
                {
                    var sourcePath = sourcePathParameter.Value;

                    var loadFromPathResult = LoadFromPath.LoadTreeFromPath(sourcePath);

                    if (loadFromPathResult?.Ok.tree == null)
                    {
                        throw new Exception("Failed to load from path '" + sourcePath + "': " + loadFromPathResult?.Err);
                    }

                    var composition = Composition.FromTreeWithStringPath(loadFromPathResult?.Ok.tree);

                    var compositionId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(composition));

                    Console.WriteLine("Loaded composition " + compositionId + " from '" + sourcePath + "'.");

                    var blobs =
                        loadFromPathResult?.Ok.tree.EnumerateBlobsTransitive()
                        .ToImmutableList();

                    var compositionDescription =
                        loadFromPathResult?.Ok.tree.BlobContent == null
                        ?
                        ("a tree containing " + blobs.Count + " blobs:\n" +
                        string.Join("\n", blobs.Select(blobAtPath => string.Join("/", blobAtPath.path))))
                        :
                        "a blob containing " + loadFromPathResult?.Ok.tree.BlobContent.Length + " bytes";

                    Console.WriteLine(
                        "Composition " + compositionId + " is " + compositionDescription);

                    return 0;
                });
            });

        static Func<string, string> SitePasswordFromSiteFromOptionOnCommandOrFromSettings(CommandLineApplication cmd, string siteName = null)
        {
            siteName ??= "site";

            var sitePasswordOption = cmd.Option("--" + siteName + "-password", "Password to access the " + siteName + ".", CommandOptionType.SingleValue);

            return site => sitePasswordOption.Value() ?? UserSecrets.LoadPasswordForSite(site);
        }

        static CommandArgument ProcessSiteArgumentOnCommand(CommandLineApplication cmd) =>
            cmd
            .Argument("process-site", "Path to the admin interface of the server running the process.")
            .IsRequired(allowEmptyStrings: false);

        static public string ElmMakeHomeDirectoryPath =>
            Path.Combine(Filesystem.CacheDirectory, "elm-make-home");

        static public void DotNetConsoleWriteLineUsingColor(string line, ConsoleColor color)
        {
            var colorBefore = Console.ForegroundColor;

            Console.ForegroundColor = color;

            Console.WriteLine(line);

            Console.ForegroundColor = colorBefore;
        }

        static public void DotNetConsoleWriteProblemCausingAbort(string line)
        {
            Console.WriteLine("");

            DotNetConsoleWriteLineUsingColor(line, ConsoleColor.Yellow);
        }

        static (string compositionId, SourceSummaryStructure summary) CompileSourceSummary(Composition.TreeWithStringPath sourceTree)
        {
            var compositionId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(sourceTree));

            var allBlobs = sourceTree.EnumerateBlobsTransitive().ToImmutableList();

            return (compositionId, summary: new SourceSummaryStructure
            {
                numberOfFiles = allBlobs.Count,
                totalSizeOfFilesContents = allBlobs.Select(blob => blob.blobContent.Count).Sum(),
            });
        }

        public record CompileAppReport(
            string engineVersion,
            string beginTime,
            string sourcePath,
            string sourceCompositionId,
            SourceSummaryStructure sourceSummary,
            IReadOnlyList<ElmFullstack.ElmAppCompilation.CompilationIterationReport> compilationIterationsReports,
            IReadOnlyList<ElmFullstack.ElmAppCompilation.LocatedCompilationError> compilationErrors,
            string compilationException,
            int? compilationTimeSpentMilli,
            string compiledCompositionId,
            int? totalTimeSpentMilli);

        public class SourceSummaryStructure
        {
            public int numberOfFiles;

            public int totalSizeOfFilesContents;
        }

        public class DeployAppReport
        {
            public bool initElmAppState;

            public string site;

            public string beginTime;

            public string sourcePath;

            public string sourceCompositionId;

            public SourceSummaryStructure sourceSummary;

            public string filteredSourceCompositionId;

            public ResponseFromServerStruct responseFromServer;

            public string deployException;

            public int totalTimeSpentMilli;

            public class ResponseFromServerStruct
            {
                public int? statusCode;

                public object body;
            }
        }

        static public DeployAppReport DeployApp(
            string sourcePath,
            string site,
            string siteDefaultPassword,
            bool initElmAppState,
            bool promptForPasswordOnConsole)
        {
            var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            Console.WriteLine("Beginning to build configuration...");

            var buildResult =
                ElmFullstack.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                    sourcePath: sourcePath);

            var (sourceCompositionId, sourceSummary) = CompileSourceSummary(buildResult.sourceTree);

            var appConfigZipArchive = buildResult.configZipArchive;

            var appConfigZipArchiveFileId =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(appConfigZipArchive));

            var filteredSourceCompositionId =
                CommonConversion.StringBase16FromByteArray(
                    Composition.GetHash(Composition.FromTreeWithStringPath(Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(appConfigZipArchive)))));

            Console.WriteLine(
                "Built app config " + filteredSourceCompositionId + " from " + sourceCompositionId + ".");

            DeployAppReport.ResponseFromServerStruct responseFromServer = null;

            Exception deployException = null;

            try
            {
                if (!LooksLikeLocalSite(site))
                {
                    var deployAddress =
                        (site.TrimEnd('/')) +
                        (initElmAppState
                        ?
                        ElmFullstack.WebHost.StartupAdminInterface.PathApiDeployAndInitAppState
                        :
                        ElmFullstack.WebHost.StartupAdminInterface.PathApiDeployAndMigrateAppState);

                    Console.WriteLine("Attempting to deploy app '" + filteredSourceCompositionId + "' to '" + deployAddress + "'...");

                    var httpResponse = AttemptHttpRequest(() =>
                        {
                            var httpContent = new System.Net.Http.ByteArrayContent(appConfigZipArchive);

                            httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/zip");
                            httpContent.Headers.ContentDisposition =
                                new System.Net.Http.Headers.ContentDispositionHeaderValue("attachment") { FileName = filteredSourceCompositionId + ".zip" };

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
                            Newtonsoft.Json.JsonConvert.DeserializeObject<Newtonsoft.Json.Linq.JObject>((string)responseBodyReport);
                    }
                    catch { }

                    responseFromServer = new DeployAppReport.ResponseFromServerStruct
                    {
                        statusCode = (int)httpResponse.StatusCode,
                        body = responseBodyReport,
                    };
                }
                else
                {
                    var processStoreFileStore = new FileStoreFromSystemIOFile(site);

                    var processStoreWriter =
                        new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                            processStoreFileStore,
                            getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                            processStoreFileStore);

                    var appConfigTree =
                        Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                            ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

                    var appConfigComponent = Composition.FromTreeWithStringPath(appConfigTree);

                    processStoreWriter.StoreComponent(appConfigComponent);

                    var appConfigValueInFile =
                        new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ValueInFileStructure
                        {
                            HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfigComponent))
                        };

                    var compositionLogEvent =
                        ElmFullstack.WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                            appConfigValueInFile: appConfigValueInFile,
                            initElmAppState: initElmAppState);

                    var (statusCode, responseReport) =
                        ElmFullstack.WebHost.StartupAdminInterface.AttemptContinueWithCompositionEventAndCommit(
                            compositionLogEvent,
                            processStoreFileStore);

                    responseFromServer = new DeployAppReport.ResponseFromServerStruct
                    {
                        statusCode = statusCode,
                        body = responseReport,
                    };
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Failed with exception: " + e.Message);

                deployException = e;
            }

            return new DeployAppReport
            {
                initElmAppState = initElmAppState,
                site = site,
                beginTime = beginTime,
                sourcePath = sourcePath,
                sourceCompositionId = sourceCompositionId,
                sourceSummary = sourceSummary,
                filteredSourceCompositionId = filteredSourceCompositionId,
                responseFromServer = responseFromServer,
                deployException = deployException?.ToString(),
                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
            };
        }

        static async System.Threading.Tasks.Task<(System.Net.Http.HttpResponseMessage httpResponse, string enteredPassword)>
            AttemptHttpRequest(
            Func<System.Net.Http.HttpRequestMessage> buildRequestBeforeAddingCommonHeaders,
            string defaultPassword,
            bool promptForPasswordOnConsole)
        {
            System.Net.Http.HttpRequestMessage buildRequest() =>
                AddUserAgentHeader(buildRequestBeforeAddingCommonHeaders());

            using var httpClient = new System.Net.Http.HttpClient();

            httpClient.Timeout = TimeSpan.FromMinutes(4);

            void setHttpClientPassword(string password)
            {
                httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(BasicAuthenticationForAdmin(password))));
            }

            setHttpClientPassword(defaultPassword);

            var httpResponse = await httpClient.SendAsync(buildRequest());

            string enteredPassword = null;

            if (promptForPasswordOnConsole &&
                httpResponse.StatusCode == System.Net.HttpStatusCode.Unauthorized &&
                 httpResponse.Headers.WwwAuthenticate.Any())
            {
                Console.WriteLine("The server at '" + httpResponse.RequestMessage.RequestUri.ToString() + "' is asking for authentication. Please enter the password we should use to authenticate there:");

                enteredPassword = ReadLine.ReadPassword("> ").Trim();

                Console.WriteLine("I retry using this password...");

                setHttpClientPassword(enteredPassword);

                httpResponse = await httpClient.SendAsync(buildRequest());
            }

            return (httpResponse, enteredPassword);
        }

        static string MapSiteForCommandLineArgument(string siteArgument)
        {
            if (LooksLikeLocalSite(siteArgument))
                return siteArgument;

            return MapUriForForAdminInterface(siteArgument).ToString();
        }

        static Uri MapUriForForAdminInterface(string uriString)
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

        static Uri MapUriForDefaultPort(string uriString, int defaultPort)
        {
            var uri = new Uri(uriString);

            if (!uri.Authority.Contains(":"))
                return WithPort(uri, defaultPort);

            return uri;
        }

        static bool LooksLikeLocalSite(string site)
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

        static public Uri WithPort(Uri uri, int newPort)
        {
            var builder = new UriBuilder(uri)
            {
                Port = newPort
            };
            return builder.Uri;
        }

        [Newtonsoft.Json.JsonObject(ItemNullValueHandling = Newtonsoft.Json.NullValueHandling.Ignore)]
        record CopyElmAppStateReport(
            string beginTime,
            string source,
            string destination,
            AppStateSummary appStateSummary = null,
            ResponseFromServerStruct destinationResponseFromServer = null,
            string destinationFileReport = null,
            int? totalTimeSpentMilli = null,
            object error = null);

        public record ResponseFromServerStruct(int? statusCode, object body);

        public record AppStateSummary(string hash, int length);

        static CopyElmAppStateReport CopyElmAppState(
            string source,
            string sourceDefaultPassword,
            string destination,
            string destinationDefaultPassword)
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

            var appStateComponent = Composition.Component.Blob(appStateSerial);
            var appStateId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appStateComponent));

            report = report with { appStateSummary = new AppStateSummary(hash: appStateId, length: appStateSerial.Length) };

            Console.WriteLine("Got app state " + appStateId + " from the source. It is " + appStateSerial.Length + " bytes long.");

            string saveToFile(string filePath)
            {
                Directory.CreateDirectory(Path.GetDirectoryName(filePath));

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

        static ResponseFromServerStruct SetElmAppStateViaAdminInterface(
            string site,
            string siteDefaultPassword,
            byte[] elmAppStateSerialized,
            bool promptForPasswordOnConsole)
        {
            var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var elmAppStateComponent = Composition.Component.Blob(elmAppStateSerialized);

            var elmAppStateId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppStateComponent));

            var httpResponse = AttemptHttpRequest(() =>
                {
                    var httpContent = new System.Net.Http.ByteArrayContent(elmAppStateSerialized);

                    httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/json");

                    return new System.Net.Http.HttpRequestMessage
                    {
                        Method = System.Net.Http.HttpMethod.Post,
                        RequestUri = MapUriForForAdminInterface(site.TrimEnd('/') + ElmFullstack.WebHost.StartupAdminInterface.PathApiElmAppState),
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
                    Newtonsoft.Json.JsonConvert.DeserializeObject<Newtonsoft.Json.Linq.JObject>((string)responseBodyReport);
            }
            catch { }

            return new ResponseFromServerStruct
            (
                statusCode: (int)httpResponse.StatusCode,
                body: responseBodyReport
            );
        }

        static byte[] GetElmAppStateViaAdminInterface(
            string site,
            string siteDefaultPassword,
            bool promptForPasswordOnConsole)
        {
            var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

            var httpResponse = AttemptHttpRequest(() =>
                {
                    return new System.Net.Http.HttpRequestMessage
                    {
                        Method = System.Net.Http.HttpMethod.Get,
                        RequestUri = MapUriForForAdminInterface(site.TrimEnd('/') + ElmFullstack.WebHost.StartupAdminInterface.PathApiElmAppState),
                    };
                },
                defaultPassword: siteDefaultPassword,
                promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

            Console.WriteLine("Server response status code: " + httpResponse.StatusCode);

            var elmAppStateSerialized = httpResponse.Content.ReadAsByteArrayAsync().Result;

            var elmAppStateComponent = Composition.Component.Blob(elmAppStateSerialized);
            var elmAppStateId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppStateComponent));

            return elmAppStateSerialized;
        }

        class TruncateProcessHistoryReport
        {
            public string beginTime;

            public string site;

            public ResponseFromServerStruct responseFromServer;

            public int totalTimeSpentMilli;

            public class ResponseFromServerStruct
            {
                public int? statusCode;

                public object body;
            }
        }

        static TruncateProcessHistoryReport TruncateProcessHistory(
            string site,
            string siteDefaultPassword,
            bool promptForPasswordOnConsole)
        {
            var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);
            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var requestUrl =
                site.TrimEnd('/') + ElmFullstack.WebHost.StartupAdminInterface.PathApiTruncateProcessHistory;

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
                    Newtonsoft.Json.JsonConvert.DeserializeObject<Newtonsoft.Json.Linq.JObject>(responseContentString);
            }
            catch { }

            var responseFromServer = new TruncateProcessHistoryReport.ResponseFromServerStruct
            {
                statusCode = (int)httpResponse.StatusCode,
                body = responseBodyReport,
            };

            return new TruncateProcessHistoryReport
            {
                beginTime = beginTime,
                site = site,
                responseFromServer = responseFromServer,
                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
            };
        }

        static (IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> files, string lastCompositionLogRecordHashBase16) ReadFilesForRestoreProcessFromAdminInterface(
            string sourceAdminInterface,
            string sourceAdminPassword)
        {
            using var sourceHttpClient = new System.Net.Http.HttpClient { BaseAddress = new Uri(sourceAdminInterface) };

            sourceHttpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                "Basic",
                Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(BasicAuthenticationForAdmin(sourceAdminPassword))));

            var processHistoryFileStoreRemoteReader = new DelegatingFileStoreReader
            {
                ListFilesInDirectoryDelegate = directoryPath =>
                {
                    var httpRequestPath =
                        ElmFullstack.WebHost.StartupAdminInterface.PathApiProcessHistoryFileStoreListFilesInDirectory + "/" +
                        string.Join("/", directoryPath);

                    var response = sourceHttpClient.GetAsync(httpRequestPath).Result;

                    if (!response.IsSuccessStatusCode)
                        throw new Exception("Unexpected response status code: " + ((int)response.StatusCode) + " (" + response.StatusCode + ").");

                    return
                        response.Content.ReadAsStringAsync().Result.Split('\n', StringSplitOptions.RemoveEmptyEntries)
                        .Select(path => path.Split('/').ToImmutableList());
                },
                GetFileContentDelegate = filePath =>
                {
                    var httpRequestPath =
                        ElmFullstack.WebHost.StartupAdminInterface.PathApiProcessHistoryFileStoreGetFileContent + "/" +
                        string.Join("/", filePath);

                    var response = sourceHttpClient.GetAsync(httpRequestPath).Result;

                    if (response.StatusCode == System.Net.HttpStatusCode.NotFound)
                        return null;

                    if (!response.IsSuccessStatusCode)
                        throw new Exception("Unexpected response status code: " + ((int)response.StatusCode) + " (" + response.StatusCode + ").");

                    return response.Content.ReadAsByteArrayAsync().Result;
                }
            };

            return ElmFullstack.WebHost.PersistentProcess.PersistentProcessLiveRepresentation.GetFilesForRestoreProcess(processHistoryFileStoreRemoteReader);
        }

        static IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> LoadFilesForRestoreFromPathAndLogToConsole(
            string sourcePath, string sourcePassword)
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
                Composition.ToFlatDictionaryWithPathComparer(
                    Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(zipArchiveEntries)
                    .EnumerateBlobsTransitive());
        }

        static public void ReplicateProcessAndLogToConsole(
            string site,
            string sitePassword,
            string sourcePath,
            string sourcePassword)
        {
            var restoreFiles =
                LoadFilesForRestoreFromPathAndLogToConsole(sourcePath: sourcePath, sourcePassword: sourcePassword);

            var processHistoryTree =
                Composition.SortedTreeFromSetOfBlobsWithStringPath(restoreFiles);

            var processHistoryComponentHash = Composition.GetHash(Composition.FromTreeWithStringPath(processHistoryTree));
            var processHistoryComponentHashBase16 = CommonConversion.StringBase16FromByteArray(processHistoryComponentHash);

            var processHistoryZipArchive = ZipArchive.ZipArchiveFromEntries(restoreFiles);

            using var httpClient = new System.Net.Http.HttpClient();

            httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                "Basic",
                Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(BasicAuthenticationForAdmin(sitePassword))));

            var deployAddress =
                site.TrimEnd('/') +
                ElmFullstack.WebHost.StartupAdminInterface.PathApiReplaceProcessHistory;

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

        static (string commandName, Func<(bool executableIsRegisteredOnPath, Action registerExecutableDirectoryOnPath)> checkInstallation)
            CheckIfExecutableIsRegisteredOnPath()
        {
            var environmentVariableName = "PATH";

            var environmentVariableScope = EnvironmentVariableTarget.User;

            string getCurrentValueOfEnvironmentVariable() =>
                Environment.GetEnvironmentVariable(environmentVariableName, environmentVariableScope);

            var (executableFilePath, executableDirectoryPath, executableFileName) = GetCurrentProcessExecutableFilePathAndComponents();

            var commandName = Regex.Match(executableFileName, @"(.+?)(?=\.exe$|$)").Groups[1].Value;

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

                    byte[] currentRegisteredFileContent = null;

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

        static public void BuildConfiguration(
            string sourcePath,
            string outputOption)
        {
            var buildResult =
                ElmFullstack.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                    sourcePath: sourcePath);

            var configZipArchive = buildResult.configZipArchive;

            var configZipArchiveFileId =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(configZipArchive));

            var webAppConfigFileId =
                CommonConversion.StringBase16FromByteArray(
                    Composition.GetHash(Composition.FromTreeWithStringPath(Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(configZipArchive)))));

            Console.WriteLine(
                "I built zip archive " + configZipArchiveFileId + " containing web app config " + webAppConfigFileId + ".");

            if (outputOption == null)
            {
                Console.WriteLine("I did not see a path for output, so I don't attempt to save the configuration to a file.");
            }
            else
            {
                var directory = Path.GetDirectoryName(outputOption);

                if (0 < directory?.Length)
                    Directory.CreateDirectory(directory);

                File.WriteAllBytes(outputOption, configZipArchive);

                Console.WriteLine("I saved zip archive " + configZipArchiveFileId + " to '" + outputOption + "'");
            }
        }

        static string ReportFilePath => Path.Combine(Environment.CurrentDirectory, "elm-fullstack-tool", "report");

        static void WriteReportToFileInReportDirectory(string reportContent, string reportKind)
        {
            var fileName = CommonConversion.TimeStringViewForReport(programStartTime) + "_" + reportKind;

            var filePath = Path.Combine(ReportFilePath, fileName);

            Directory.CreateDirectory(Path.GetDirectoryName(filePath));

            File.WriteAllBytes(filePath, System.Text.Encoding.UTF8.GetBytes(reportContent));

            Console.WriteLine("Saved report to file '" + filePath + "'.");
        }

        static string GetCurrentProcessExecutableFilePath() =>
            System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName;

        static (string filePath, string directoryPath, string fileName) GetCurrentProcessExecutableFilePathAndComponents()
        {
            var filePath = GetCurrentProcessExecutableFilePath();

            return (filePath, System.IO.Path.GetDirectoryName(filePath), System.IO.Path.GetFileName(filePath));
        }

        static System.Net.Http.HttpRequestMessage AddUserAgentHeader(
            System.Net.Http.HttpRequestMessage httpRequest)
        {
            httpRequest.Headers.UserAgent.Add(
                new System.Net.Http.Headers.ProductInfoHeaderValue(
                    new System.Net.Http.Headers.ProductHeaderValue("elm-fullstack-cli", AppVersionId)));

            return httpRequest;
        }

        static readonly DateTimeOffset programStartTime = DateTimeOffset.UtcNow;
    }
}
