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
        static public string AppVersionId => "2021-05-27";

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
                var (commandName, _, registerExecutableDirectoryOnPath) = CheckIfExecutableIsRegisteredOnPath();

                installCmd.Description = "Installs the '" + commandName + "' command for the current user account.";
                installCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                installCmd.OnExecute(() =>
                {
                    registerExecutableDirectoryOnPath();
                });
            });

            var runServerCmd = AddRunServerCmd(app);

            var deployAppCmd = AddDeployAppCmd(app);
            var setAppStateCmd = AddSetAppStateCmd(app);
            var archiveProcessCmd = AddArchiveProcessCmd(app);
            var truncateProcessHistoryCmd = AddTruncateProcessHistoryCmd(app);

            var compileAppCmd = AddCompileAppCmd(app);
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

                var setupGroupCommands =
                    CheckIfExecutableIsRegisteredOnPath().executableIsRegisteredOnPath
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
                        title = "operate servers and maintain production systems",
                        commands = new[]
                        {
                            runServerCmd,
                            deployAppCmd,
                            setAppStateCmd,
                            archiveProcessCmd,
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
                            "These are common Elm-fs commands used in various situations:",
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


                var processStoreDirectoryPathOption = runServerCmd.Option("--process-store-directory-path", "Directory in the file system to contain the process store.", CommandOptionType.SingleValue);
                var deletePreviousProcessOption = runServerCmd.Option("--delete-previous-process", "Delete the previous backend process found in the given store. If you don't use this option, the server restores the process from the persistent store on startup.", CommandOptionType.NoValue);
                var adminUrlsOption = runServerCmd.Option("--admin-urls", "URLs for the admin interface. The default is " + adminUrlsDefault.ToString() + ".", CommandOptionType.SingleValue);
                var adminPasswordOption = runServerCmd.Option("--admin-password", "Password for the admin interface at '--admin-urls'.", CommandOptionType.SingleValue);
                var publicAppUrlsOption = runServerCmd.Option("--public-urls", "URLs to serve the public app from. The default is '" + string.Join(",", publicWebHostUrlsDefault) + "'.", CommandOptionType.SingleValue);
                var replicateProcessFromOption = runServerCmd.Option("--replicate-process-from", "Path to a process to replicate. Can be a URL to an admin interface of a server or a path to an archive containing files representing the process state. This option also implies '--delete-previous-process'.", CommandOptionType.SingleValue);
                var replicateProcessAdminPasswordOption = runServerCmd.Option("--replicate-process-admin-password", "Used together with '--replicate-process-from' if that location requires a password to authenticate.", CommandOptionType.SingleValue);
                var deployAppFromOption = runServerCmd.Option("--deploy-app-from", "Path to an app to deploy on startup, analogous to the '--from' path on the `deploy-app` command. Can be combined with '--replicate-process-from'.", CommandOptionType.SingleValue);

                runServerCmd.OnExecute(() =>
                {
                    var processStoreDirectoryPath = processStoreDirectoryPathOption.Value();

                    var publicAppUrls =
                        publicAppUrlsOption.Value()?.Split(',').Select(url => url.Trim()).ToArray() ??
                        publicWebHostUrlsDefault;

                    var replicateProcessFrom = replicateProcessFromOption.Value();

                    var replicateProcessAdminPassword =
                        replicateProcessAdminPasswordOption.Value() ?? UserSecrets.LoadPasswordForSite(replicateProcessFrom);

                    if ((deletePreviousProcessOption.HasValue() || replicateProcessFrom != null) && processStoreDirectoryPath != null)
                    {
                        Console.WriteLine("Deleting the previous process state from '" + processStoreDirectoryPath + "'...");

                        if (System.IO.Directory.Exists(processStoreDirectoryPath))
                            System.IO.Directory.Delete(processStoreDirectoryPath, true);

                        Console.WriteLine("Completed deleting the previous process state from '" + processStoreDirectoryPath + "'.");
                    }

                    IFileStore processStoreFileStore = null;

                    if (processStoreDirectoryPath == null)
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
                        processStoreFileStore = new FileStoreFromSystemIOFile(processStoreDirectoryPath);
                    }

                    if (replicateProcessFrom != null)
                    {
                        var replicateFiles =
                            LoadFilesForRestoreFromPathAndLogToConsole(
                                sourcePath: replicateProcessFrom,
                                sourcePassword: replicateProcessAdminPassword);

                        foreach (var file in replicateFiles)
                            processStoreFileStore.SetFileContent(file.Key, file.Value.ToArray());
                    }

                    var adminInterfaceUrls = adminUrlsOption.Value() ?? adminUrlsDefault;

                    if (deployAppFromOption.HasValue())
                    {
                        Console.WriteLine("Loading app config to deploy...");

                        var appConfigZipArchive =
                            ElmFullstack.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                                sourcePath: deployAppFromOption.Value()).configZipArchive;

                        var appConfigTree =
                            Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                                ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

                        var appConfigComponent = Composition.FromTreeWithStringPath(appConfigTree);

                        var processStoreWriter =
                            new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                                processStoreFileStore);

                        processStoreWriter.StoreComponent(appConfigComponent);

                        var appConfigValueInFile =
                            new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfigComponent))
                            };

                        var initElmAppState =
                            (deletePreviousProcessOption.HasValue() && !replicateProcessFromOption.HasValue()) ||
                            processStoreDirectoryPath == null;

                        var compositionLogEvent =
                            ElmFullstack.WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                                appConfigValueInFile: appConfigValueInFile,
                                initElmAppState: initElmAppState);

                        var testDeployResult = ElmFullstack.WebHost.PersistentProcess.PersistentProcessVolatileRepresentation.TestContinueWithCompositionEvent(
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

                CommandExtension.AddHelpCommandOnCommand(runServerCmd);
            });

        static CommandLineApplication AddDeployAppCmd(CommandLineApplication app) =>
            app.Command("deploy-app", deployAppCmd =>
            {
                deployAppCmd.Description = "Deploy an app to an Elm Fullstack process. Deployment implies migration from the previous app state if not specified otherwise.";
                deployAppCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var getSiteAndPasswordFromOptions = SiteAndSitePasswordOptionsOnCommand(deployAppCmd);
                var fromOption = deployAppCmd.Option("--from", "Path to the app to deploy.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);

                var initElmAppStateOption = deployAppCmd.Option("--init-app-state", "Do not attempt to migrate the Elm app state but use the state from the init function.", CommandOptionType.NoValue);

                deployAppCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = getSiteAndPasswordFromOptions();

                    var deployReport =
                        DeployApp(
                            sourcePath: fromOption.Value(),
                            site: site,
                            siteDefaultPassword: sitePassword,
                            initElmAppState: initElmAppStateOption.HasValue(),
                            promptForPasswordOnConsole: true);

                    WriteReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(deployReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "deploy-app.json");
                });

                CommandExtension.AddHelpCommandOnCommand(deployAppCmd);
            });

        static CommandLineApplication AddSetAppStateCmd(CommandLineApplication app) =>
            app.Command("set-app-state", setElmAppStateCmd =>
            {
                setElmAppStateCmd.Description = "Set the state of a backend Elm app.";
                setElmAppStateCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var siteAndPasswordFromCmd = SiteAndSitePasswordOptionsOnCommand(setElmAppStateCmd);

                var fromOption = setElmAppStateCmd.Option("--from", "Path to the serialized state representation to load.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);

                setElmAppStateCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

                    var attemptReport =
                        SetElmAppState(
                            site: site,
                            siteDefaultPassword: sitePassword,
                            sourcePath: fromOption.Value(),
                            promptForPasswordOnConsole: true);

                    WriteReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(attemptReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "set-app-state.json");
                });

                CommandExtension.AddHelpCommandOnCommand(setElmAppStateCmd);
            });

        static CommandLineApplication AddArchiveProcessCmd(CommandLineApplication app) =>
            app.Command("archive-process", archiveProcessCmd =>
            {
                archiveProcessCmd.Description = "Copy all files needed to restore or replicate a process and store them in a zip archive.";
                archiveProcessCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var siteAndPasswordFromCmd = SiteAndSitePasswordOptionsOnCommand(archiveProcessCmd);

                archiveProcessCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

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

                CommandExtension.AddHelpCommandOnCommand(archiveProcessCmd);
            });

        static CommandLineApplication AddTruncateProcessHistoryCmd(CommandLineApplication app) =>
            app.Command("truncate-process-history", truncateProcessHistoryCmd =>
            {
                truncateProcessHistoryCmd.Description = "Remove parts of the process history that are not needed to restore the process.";
                truncateProcessHistoryCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var siteAndPasswordFromCmd = SiteAndSitePasswordOptionsOnCommand(truncateProcessHistoryCmd);

                truncateProcessHistoryCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

                    var report =
                        TruncateProcessHistory(
                            site: site,
                            siteDefaultPassword: sitePassword,
                            promptForPasswordOnConsole: true);

                    WriteReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(report, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "truncate-process-history.json");
                });

                CommandExtension.AddHelpCommandOnCommand(truncateProcessHistoryCmd);
            });

        static CommandLineApplication AddCompileAppCmd(CommandLineApplication app) =>
            app.Command("compile-app", compileAppCmd =>
            {
                compileAppCmd.Description = "Compile app source code the same way as would be done when deploying.";
                compileAppCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var fromOption = compileAppCmd.Option("--from", "Path to the app source code.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);

                compileAppCmd.OnExecute(() =>
                {
                    var sourcePath = fromOption.Value();

                    var loadFromPathResult = LoadFromPath.LoadTreeFromPath(sourcePath);

                    if (loadFromPathResult?.Ok == null)
                    {
                        throw new Exception("Failed to load from path '" + sourcePath + "': " + loadFromPathResult?.Err);
                    }

                    var sourceComposition = Composition.FromTreeWithStringPath(loadFromPathResult.Ok.tree);

                    var (sourceCompositionId, sourceSummary) = CompileSourceSummary(loadFromPathResult.Ok.tree);

                    Console.WriteLine("Loaded source composition " + sourceCompositionId + " from '" + sourcePath + "'. Starting to compile...");

                    var compilationStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    var sourceFiles =
                        Composition.TreeToFlatDictionaryWithPathComparer(loadFromPathResult.Ok.tree);

                    string compilationException = null;
                    Composition.TreeWithStringPath compiledTree = null;
                    IImmutableList<ElmFullstack.ElmApp.CompilationIterationReport> compilationIterationsReports = null;

                    try
                    {
                        var (compiledAppFiles, iterationsReports) = ElmFullstack.ElmApp.AsCompletelyLoweredElmApp(
                            sourceFiles: sourceFiles,
                            ElmFullstack.ElmAppInterfaceConfig.Default);

                        compilationIterationsReports = iterationsReports;

                        compiledTree =
                            Composition.SortedTreeFromSetOfBlobsWithStringPath(compiledAppFiles);
                    }
                    catch (Exception e)
                    {
                        compilationException = e.ToString();
                    }

                    compilationStopwatch.Stop();

                    Console.WriteLine("Compilation completed in " + (int)compilationStopwatch.Elapsed.TotalSeconds + " seconds.");

                    var compilationTimeSpentMilli = compilationStopwatch.ElapsedMilliseconds;

                    var compiledComposition =
                        compiledTree == null ? null : Composition.FromTreeWithStringPath(compiledTree);

                    var compiledCompositionId =
                        compiledComposition == null ? null :
                        CommonConversion.StringBase16FromByteArray(Composition.GetHash(compiledComposition));

                    if (compiledTree != null)
                    {
                        var compiledFiles =
                            Composition.TreeToFlatDictionaryWithPathComparer(compiledTree);

                        var compiledCompositionArchive =
                            ZipArchive.ZipArchiveFromEntries(compiledFiles);

                        var outputCompositionFileName = compiledCompositionId + ".zip";

                        var outputCompositionFilePath = Path.Combine(ReportFilePath, outputCompositionFileName);

                        Directory.CreateDirectory(Path.GetDirectoryName(outputCompositionFilePath));
                        File.WriteAllBytes(outputCompositionFilePath, compiledCompositionArchive);
                        Console.WriteLine("Saved compiled composition " + compiledCompositionId + " to '" + outputCompositionFilePath + "'.");
                    }

                    var compileReport = new CompileAppReport
                    {
                        engineVersion = AppVersionId,
                        sourcePath = sourcePath,
                        sourceCompositionId = sourceCompositionId,
                        sourceSummary = sourceSummary,
                        compilationIterationsReports = compilationIterationsReports,
                        compilationException = compilationException,
                        compilationTimeSpentMilli = (int)compilationTimeSpentMilli,
                        compiledCompositionId = compiledCompositionId,
                    };

                    WriteReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(compileReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "compile-app.json");
                });

                CommandExtension.AddHelpCommandOnCommand(compileAppCmd);
            });

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

                CommandExtension.AddHelpCommandOnCommand(enterInteractiveCmd);
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

                CommandExtension.AddHelpCommandOnCommand(describeCmd);
            });

        static Func<(string site, string sitePassword)> SiteAndSitePasswordOptionsOnCommand(CommandLineApplication cmd)
        {
            var siteOption = cmd.Option("--site", "Site where to apply the changes. Can be an URL to the admin interface of a server or a path in the local file system.", CommandOptionType.SingleValue).IsRequired();
            var sitePasswordOption = cmd.Option("--site-password", "Password to access the site where to apply the changes.", CommandOptionType.SingleValue);

            return () =>
            {
                var site = siteOption.Value();

                var sitePassword =
                    sitePasswordOption.Value() ?? UserSecrets.LoadPasswordForSite(site);

                return (site, sitePassword);
            };
        }

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

        public class CompileAppReport
        {
            public string engineVersion;

            public string beginTime;

            public string sourcePath;

            public string sourceCompositionId;

            public SourceSummaryStructure sourceSummary;

            public IImmutableList<ElmFullstack.ElmApp.CompilationIterationReport> compilationIterationsReports;

            public string compilationException;

            public int compilationTimeSpentMilli;

            public string compiledCompositionId;

            public int totalTimeSpentMilli;
        }

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
                if (Regex.IsMatch(site, "^http(|s)\\:"))
                {
                    var deployAddress =
                        (site.TrimEnd('/')) +
                        (initElmAppState
                        ?
                        ElmFullstack.WebHost.StartupAdminInterface.PathApiDeployAppConfigAndInitElmAppState
                        :
                        ElmFullstack.WebHost.StartupAdminInterface.PathApiDeployAppConfigAndMigrateElmAppState);

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
                                RequestUri = MapUriForDefaultPortForAdminInterface(deployAddress),
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

        static Uri MapUriForDefaultPortForAdminInterface(string uriString) =>
                MapUriForDefaultPort(uriString, AdminInterfaceDefaultPort);

        static Uri MapUriForDefaultPort(string uriString, int defaultPort)
        {
            var uri = new Uri(uriString);

            if (!uri.Authority.Contains(":"))
                return WithPort(uri, defaultPort);

            return uri;
        }

        static public Uri WithPort(Uri uri, int newPort)
        {
            var builder = new UriBuilder(uri)
            {
                Port = newPort
            };
            return builder.Uri;
        }

        class SetElmAppStateReport
        {
            public string beginTime;

            public string elmAppStateSourcePath;

            public string elmAppStateId;

            public string site;

            public ResponseFromServerStruct responseFromServer;

            public int totalTimeSpentMilli;

            public class ResponseFromServerStruct
            {
                public int? statusCode;

                public object body;
            }
        }

        static SetElmAppStateReport SetElmAppState(
            string site,
            string siteDefaultPassword,
            string sourcePath,
            bool promptForPasswordOnConsole)
        {
            var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            // For now only support a file as source.

            var elmAppStateSerialized = File.ReadAllBytes(sourcePath);

            var elmAppStateComponent = Composition.Component.Blob(elmAppStateSerialized);

            var elmAppStateId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppStateComponent));

            SetElmAppStateReport.ResponseFromServerStruct responseFromServer = null;

            var httpResponse = AttemptHttpRequest(() =>
                {
                    var httpContent = new System.Net.Http.ByteArrayContent(elmAppStateSerialized);

                    httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/json");

                    return new System.Net.Http.HttpRequestMessage
                    {
                        Method = System.Net.Http.HttpMethod.Post,
                        RequestUri = MapUriForDefaultPortForAdminInterface(site.TrimEnd('/') + ElmFullstack.WebHost.StartupAdminInterface.PathApiElmAppState),
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

            responseFromServer = new SetElmAppStateReport.ResponseFromServerStruct
            {
                statusCode = (int)httpResponse.StatusCode,
                body = responseBodyReport,
            };

            return new SetElmAppStateReport
            {
                beginTime = beginTime,
                elmAppStateSourcePath = sourcePath,
                elmAppStateId = elmAppStateId,
                site = site,
                responseFromServer = responseFromServer,
                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
            };
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
                        RequestUri = MapUriForDefaultPortForAdminInterface(requestUrl),
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

            return ElmFullstack.WebHost.PersistentProcess.PersistentProcessVolatileRepresentation.GetFilesForRestoreProcess(processHistoryFileStoreRemoteReader);
        }

        static IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> LoadFilesForRestoreFromPathAndLogToConsole(
            string sourcePath, string sourcePassword)
        {
            if (Regex.IsMatch(sourcePath, "http(|s)://", RegexOptions.IgnoreCase))
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

        static (string commandName, bool executableIsRegisteredOnPath, Action registerExecutableDirectoryOnPath)
            CheckIfExecutableIsRegisteredOnPath()
        {
            var environmentVariableName = "PATH";

            var environmentVariableScope = EnvironmentVariableTarget.User;

            string getCurrentValueOfEnvironmentVariable() =>
                Environment.GetEnvironmentVariable(environmentVariableName, environmentVariableScope);

            var (executableFilePath, executableDirectoryPath, executableFileName) = GetCurrentProcessExecutableFilePathAndComponents();

            var commandName = Regex.Match(executableFileName, @"(.+?)(?=\.exe$|$)").Groups[1].Value;

            var registerExecutableForCurrentUser = new Action(() =>
            {
                var newValueForPathEnv =
                    executableDirectoryPath +
                    System.IO.Path.PathSeparator +
                    getCurrentValueOfEnvironmentVariable();

                Environment.SetEnvironmentVariable(environmentVariableName, newValueForPathEnv, environmentVariableScope);

                //  https://stackoverflow.com/questions/32650063/get-environment-variable-out-of-new-process-in-c-sharp/32650213#32650213
                //  https://devblogs.microsoft.com/oldnewthing/?p=91591
                //  https://docs.microsoft.com/en-us/previous-versions//cc723564(v=technet.10)?redirectedfrom=MSDN#XSLTsection127121120120

                Console.WriteLine(
        "I added the path '" + executableDirectoryPath + "' to the '" + environmentVariableName +
        "' environment variable for the current user account. You will be able to use the '" + commandName + "' command in newer instances of the Command Prompt.");
            });

            var executableIsRegisteredOnPath =
                (getCurrentValueOfEnvironmentVariable() ?? "")
                .Split(Path.PathSeparator).Contains(executableDirectoryPath);

            return (commandName, executableIsRegisteredOnPath, registerExecutableForCurrentUser);
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
