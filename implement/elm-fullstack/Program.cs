using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Kalmit;
using Kalmit.PersistentProcess.WebHost;
using McMaster.Extensions.CommandLineUtils;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;

namespace elm_fullstack
{
    public class Program
    {
        static int Main(string[] args)
        {
            Kalmit.ProcessFromElm019Code.overrideElmMakeHomeDirectory = ElmMakeHomeDirectoryPath;

            var app = new CommandLineApplication
            {
                Name = "elm-fullstack",
                Description = "Welcome to Elm fullstack! This tool helps you build and run full stack web applications using the Elm programming language.\nTo get help or report an issue, see the project website at http://elm-fullstack.org/",
            };

            app.HelpOption(inherited: true);
            app.HelpTextGenerator =
                new McMaster.Extensions.CommandLineUtils.HelpText.DefaultHelpTextGenerator { SortCommandsByName = false };

            app.VersionOption(template: "-v|--version", shortFormVersion: "version " + Kalmit.PersistentProcess.WebHost.Program.AppVersionId);

            app.Command("install-command", installCmd =>
            {
                var (commandName, _, registerExecutableDirectoryOnPath) = CheckIfExecutableIsRegisteredOnPath();

                installCmd.Description = "Installs the '" + commandName + "' command for the current user account.";
                installCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                installCmd.OnExecute(() =>
                {
                    registerExecutableDirectoryOnPath();
                });
            });

            app.Command("run-server", runServerCmd =>
            {
                runServerCmd.Description = "Run a web server supporting administration of an Elm-fullstack process via HTTP. This HTTP interface supports deployments, migrations, etc.";
                runServerCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var adminUrlsDefault = "http://*:4000";
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

                        var files = new System.Collections.Concurrent.ConcurrentDictionary<IImmutableList<string>, byte[]>(EnumerableExtension.EqualityComparer<string>());

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
                            Kalmit.PersistentProcess.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                                sourcePath: deployAppFromOption.Value()).configZipArchive;

                        var appConfigTree =
                            Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                                ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

                        var appConfigComponent = Composition.FromTree(appConfigTree);

                        var processStoreWriter =
                            new Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                                processStoreFileStore);

                        processStoreWriter.StoreComponent(appConfigComponent);

                        var appConfigValueInFile =
                            new Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations.ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfigComponent))
                            };

                        var initElmAppState =
                            (deletePreviousProcessOption.HasValue() && !replicateProcessFromOption.HasValue()) ||
                            processStoreDirectoryPath == null;

                        var compositionLogEvent =
                            Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                                appConfigValueInFile: appConfigValueInFile,
                                initElmAppState: initElmAppState);

                        var testDeployResult = Kalmit.PersistentProcess.WebHost.PersistentProcess.PersistentProcessVolatileRepresentation.TestContinueWithCompositionEvent(
                            compositionLogEvent: compositionLogEvent,
                            fileStoreReader: processStoreFileStore);

                        if (testDeployResult.Ok.projectedFiles == null)
                        {
                            throw new Exception("Attempt to deploy app config failed: " + testDeployResult.Err);
                        }

                        foreach (var projectedFilePathAndContent in testDeployResult.Ok.projectedFiles)
                            processStoreFileStore.SetFileContent(
                                projectedFilePathAndContent.filePath, projectedFilePathAndContent.fileContent);
                    }

                    var webHostBuilder =
                        Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                        .ConfigureAppConfiguration(builder => builder.AddEnvironmentVariables("APPSETTING_"))
                        .UseUrls(adminInterfaceUrls)
                        .UseStartup<StartupAdminInterface>()
                        .WithSettingPublicWebHostUrls(publicAppUrls)
                        .WithProcessStoreFileStore(processStoreFileStore);

                    if (adminPasswordOption.HasValue())
                        webHostBuilder = webHostBuilder.WithSettingAdminPassword(adminPasswordOption.Value());

                    var webHost = webHostBuilder.Build();

                    Console.WriteLine("Starting the web server with the admin interface...");

                    webHost.Start();

                    Console.WriteLine("Completed starting the web server with the admin interface at '" + adminInterfaceUrls + "'.");

                    Microsoft.AspNetCore.Hosting.WebHostExtensions.WaitForShutdown(webHost);
                });
            });

            Func<(string site, string sitePassword)> siteAndSitePasswordOptionsOnCommand(CommandLineApplication cmd)
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

            app.Command("deploy-app", deployAppCmd =>
            {
                deployAppCmd.Description = "Deploy an app to an Elm-fullstack process. By default, migrates from the previous app state using the `migrate` function in the Elm app code.";
                deployAppCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var getSiteAndPasswordFromOptions = siteAndSitePasswordOptionsOnCommand(deployAppCmd);
                var fromOption = deployAppCmd.Option("--from", "Path to the app to deploy.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);

                var initElmAppStateOption = deployAppCmd.Option("--init-elm-app-state", "Do not attempt to migrate the Elm app state but use the state from the init function.", CommandOptionType.NoValue);

                deployAppCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = getSiteAndPasswordFromOptions();

                    var deployReport =
                        deployApp(
                            sourcePath: fromOption.Value(),
                            site: site,
                            siteDefaultPassword: sitePassword,
                            initElmAppState: initElmAppStateOption.HasValue(),
                            promptForPasswordOnConsole: true);

                    writeReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(deployReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "deploy-app.json");
                });
            });

            app.Command("set-elm-app-state", setElmAppStateCmd =>
            {
                setElmAppStateCmd.Description = "Set the state of a backend Elm app.";
                setElmAppStateCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var siteAndPasswordFromCmd = siteAndSitePasswordOptionsOnCommand(setElmAppStateCmd);

                var fromOption = setElmAppStateCmd.Option("--from", "Path to the serialized state representation to load.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);

                setElmAppStateCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

                    var attemptReport =
                        setElmAppState(
                            site: site,
                            siteDefaultPassword: sitePassword,
                            sourcePath: fromOption.Value(),
                            promptForPasswordOnConsole: true);

                    writeReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(attemptReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "set-elm-app-state.json");
                });
            });

            app.Command("truncate-process-history", truncateProcessHistoryCmd =>
            {
                truncateProcessHistoryCmd.Description = "Remove parts of the process history that are not needed to restore the process.";
                truncateProcessHistoryCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var siteAndPasswordFromCmd = siteAndSitePasswordOptionsOnCommand(truncateProcessHistoryCmd);

                truncateProcessHistoryCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

                    var report =
                        truncateProcessHistory(
                            site: site,
                            siteDefaultPassword: sitePassword,
                            promptForPasswordOnConsole: true);

                    writeReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(report, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "truncate-process-history.json");
                });
            });

            app.Command("archive-process", archiveProcessCmd =>
            {
                archiveProcessCmd.Description = "Copy the files needed to restore the process and store those in a zip-archive.";
                archiveProcessCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                var siteAndPasswordFromCmd = siteAndSitePasswordOptionsOnCommand(archiveProcessCmd);

                archiveProcessCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

                    sitePassword =
                        AttemptHttpRequest(
                        () => new System.Net.Http.HttpRequestMessage { RequestUri = new Uri(site) },
                        defaultPassword: sitePassword,
                        promptForPasswordOnConsole: true).Result.enteredPassword ?? sitePassword;

                    Console.WriteLine("Begin reading process history from '" + site + "' ...");

                    var restoreResult =
                        readFilesForRestoreProcessFromAdminInterface(site, sitePassword);

                    Console.WriteLine("Completed reading files to restore process " + restoreResult.lastCompositionLogRecordHashBase16 + ". Read " + restoreResult.files.Count + " files from '" + site + "'.");

                    var zipArchive = ZipArchive.ZipArchiveFromEntries(restoreResult.files);

                    var fileName = "process-" + restoreResult.lastCompositionLogRecordHashBase16 + ".zip";
                    var filePath = Path.Combine(Environment.CurrentDirectory, fileName);

                    File.WriteAllBytes(filePath, zipArchive);

                    Console.WriteLine("Saved process archive to file '" + filePath + "'.");
                });
            });

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

                    var sourceComposition = Composition.FromTree(loadFromPathResult.Ok.tree);

                    var (sourceCompositionId, sourceSummary) = compileSourceSummary(loadFromPathResult.Ok.tree);

                    Console.WriteLine("Loaded source composition " + sourceCompositionId + " from '" + sourcePath + "'. Starting to compile...");

                    var compilationStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    var sourceFiles =
                        Kalmit.PersistentProcess.WebHost.PersistentProcess.PersistentProcessVolatileRepresentation.TreeToFlatDictionaryWithPathComparer(
                            loadFromPathResult.Ok.tree);

                    var compilationLog = new List<string>();

                    string compilationException = null;
                    Composition.TreeComponent compiledTree = null;

                    try
                    {
                        var loweredAppFiles = ElmApp.AsCompletelyLoweredElmApp(
                            sourceFiles: sourceFiles,
                            ElmAppInterfaceConfig.Default,
                            compilationLog.Add);

                        compiledTree =
                            Composition.SortedTreeFromSetOfBlobsWithStringPath(loweredAppFiles);
                    }
                    catch (Exception e)
                    {
                        compilationException = e.ToString();
                    }

                    compilationStopwatch.Stop();

                    Console.WriteLine("Compilation completed in " + (int)compilationStopwatch.Elapsed.TotalSeconds + " seconds.");

                    var compilationTimeSpentMilli = compilationStopwatch.ElapsedMilliseconds;

                    var compiledComposition =
                        compiledTree == null ? null : Composition.FromTree(compiledTree);

                    var compiledCompositionId =
                        compiledComposition == null ? null :
                        CommonConversion.StringBase16FromByteArray(Composition.GetHash(compiledComposition));

                    if (compiledTree != null)
                    {
                        var compiledFiles =
                            ElmApp.ToFlatDictionaryWithPathComparer(
                                compiledTree.EnumerateBlobsTransitive()
                                .Select(filePathAndContent =>
                                    (path: (IImmutableList<string>)filePathAndContent.path.Select(pathComponent => System.Text.Encoding.UTF8.GetString(pathComponent.ToArray())).ToImmutableList(),
                                    filePathAndContent.blobContent))
                                    .ToImmutableList());

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
                        sourcePath = sourcePath,
                        sourceCompositionId = sourceCompositionId,
                        sourceSummary = sourceSummary,
                        compilationException = compilationException,
                        compilationLog = compilationLog.ToImmutableList(),
                        compilationTimeSpentMilli = (int)compilationTimeSpentMilli,
                        compiledCompositionId = compiledCompositionId,
                    };

                    writeReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(compileReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "compile-app.json");
                });
            });

            app.Command("devtools", devtoolsCmd =>
            {
                devtoolsCmd.Description = "Collection of development tools.";
                devtoolsCmd.UnrecognizedArgumentHandling = UnrecognizedArgumentHandling.Throw;

                devtoolsCmd.Command("eval-expression", evalExpressionCmd =>
                {
                    evalExpressionCmd.Description = "Evaluate an expression of Elm syntax.";

                    var expressionArgument =
                        evalExpressionCmd
                        .Argument("elm-expression", "The text of the expression to evaluate")
                        .IsRequired(allowEmptyStrings: false);

                    evalExpressionCmd.OnExecute(() =>
                    {
                        var expression = expressionArgument.Value;

                        Console.WriteLine("Got this expression:\n" + expression + "\nStarting evaluation...");

                        try
                        {
                            var evalResult =
                                ElmEngine.EvaluateElm.EvaluateSubmissionAndGetResultingValueJsonString(
                                    appCodeTree: null, submission: expression);

                            if (evalResult.Ok == null)
                                Console.WriteLine("Failed to evaluate: " + evalResult.Err);
                            else
                                Console.WriteLine("Evaluation result as JSON:\n" + evalResult.Ok);
                        }
                        catch (Exception evalException)
                        {
                            Console.WriteLine("Evaluation failed with exception:\n" + evalException.ToString());
                        }
                    });
                });

                devtoolsCmd.Command("enter-interactive", enterInteractiveCmd =>
                {
                    enterInteractiveCmd.Description = "Enter an environment supporting interactive exploration and composition of Elm programs.";

                    var contextAppOption =
                        enterInteractiveCmd
                        .Option(
                            template: "--context-app",
                            description: "Path to an app to use as context. The Elm modules from this app will be available in the interactive environment.",
                            optionType: CommandOptionType.SingleValue);

                    enterInteractiveCmd.OnExecute(() =>
                    {
                        Console.WriteLine(
                            "---- Elm-fullstack " + Kalmit.PersistentProcess.WebHost.Program.AppVersionId + " interactive (REPL) ----");

                        Composition.TreeComponent contextAppCodeTree = null;

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

                        var previousSubmissions = new List<string>();

                        while (true)
                        {
                            var submission = ReadLine.Read("> ");

                            var evalResult =
                                elm_fullstack.ElmEngine.EvaluateElm.EvaluateSubmissionAndGetResultingValueJsonString(
                                    appCodeTree: contextAppCodeTree,
                                    submission: submission,
                                    previousLocalSubmissions: previousSubmissions);

                            if (evalResult.Ok == null)
                            {
                                Console.WriteLine("Failed to evaluate: " + evalResult.Err);
                                continue;
                            }

                            previousSubmissions.Add(submission);

                            Console.WriteLine(evalResult.Ok);
                        }
                    });
                });

                devtoolsCmd.OnExecute(() =>
                {
                    Console.WriteLine("Please specify a subcommand.");
                    devtoolsCmd.ShowHelp();

                    return 1;
                });
            });

            app.OnExecute(() =>
            {
                Console.WriteLine("Please specify a subcommand.");
                app.ShowHelp();

                return 1;
            });

            int executeAndGuideInCaseOfException()
            {
                try
                {
                    return app.Execute(args);
                }
                catch (CommandParsingException ex)
                {
                    DotNetConsoleWriteProblemCausingAbort(ex.Message);

                    if (ex is UnrecognizedCommandParsingException uex && uex.NearestMatches.Any())
                    {
                        DotNetConsoleWriteProblemCausingAbort("\nDid you mean '" + uex.NearestMatches.FirstOrDefault() + "'?");
                    }

                    return 430;
                }
            }

            return executeAndGuideInCaseOfException();
        }

        static public string ElmMakeHomeDirectoryPath =>
            System.IO.Path.Combine(
                Kalmit.Filesystem.CacheDirectory, "elm-make-home");

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

        static (string compositionId, SourceSummaryStructure summary) compileSourceSummary(Composition.TreeComponent sourceTree)
        {
            var compositionId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(sourceTree));

            var allBlobs = sourceTree.EnumerateBlobsTransitive().ToImmutableList();

            return (compositionId: compositionId, summary: new SourceSummaryStructure
            {
                numberOfFiles = allBlobs.Count,
                totalSizeOfFilesContents = allBlobs.Select(blob => blob.blobContent.Count).Sum(),
            });
        }

        public class CompileAppReport
        {
            public string beginTime;

            public string sourcePath;

            public string sourceCompositionId;

            public SourceSummaryStructure sourceSummary;

            public IReadOnlyList<string> compilationLog;

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

        static public DeployAppReport deployApp(
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
                Kalmit.PersistentProcess.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                    sourcePath: sourcePath);

            var (sourceCompositionId, sourceSummary) = compileSourceSummary(buildResult.sourceTree);

            var appConfigZipArchive = buildResult.configZipArchive;

            var appConfigZipArchiveFileId =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(appConfigZipArchive));

            var filteredSourceCompositionId =
                Kalmit.CommonConversion.StringBase16FromByteArray(
                    Composition.GetHash(Composition.FromTree(Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        Kalmit.ZipArchive.EntriesFromZipArchive(appConfigZipArchive)))));

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
                        StartupAdminInterface.PathApiDeployAppConfigAndInitElmAppState
                        :
                        StartupAdminInterface.PathApiDeployAppConfigAndMigrateElmAppState);

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
                                RequestUri = new Uri(deployAddress),
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
                    var processStoreFileStore = new Kalmit.FileStoreFromSystemIOFile(site);

                    var processStoreWriter =
                        new Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                            processStoreFileStore);

                    var appConfigTree =
                        Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                            ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

                    var appConfigComponent = Composition.FromTree(appConfigTree);

                    processStoreWriter.StoreComponent(appConfigComponent);

                    var appConfigValueInFile =
                        new Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations.ValueInFileStructure
                        {
                            HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfigComponent))
                        };

                    var compositionLogEvent =
                        Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                            appConfigValueInFile: appConfigValueInFile,
                            initElmAppState: initElmAppState);

                    var attemptDeployReport =
                        Kalmit.PersistentProcess.WebHost.StartupAdminInterface.AttemptContinueWithCompositionEventAndCommit(
                            compositionLogEvent,
                            processStoreFileStore);

                    responseFromServer = new DeployAppReport.ResponseFromServerStruct
                    {
                        statusCode = attemptDeployReport.statusCode,
                        body = attemptDeployReport.responseReport,
                    };
                }
            }
            catch (Exception e)
            {
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

            using (var httpClient = new System.Net.Http.HttpClient())
            {
                httpClient.Timeout = TimeSpan.FromMinutes(4);

                void setHttpClientPassword(string password)
                {
                    httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                        "Basic",
                        Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                            Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdmin(password))));
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

        static SetElmAppStateReport setElmAppState(
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
                        RequestUri = new Uri(site.TrimEnd('/') + StartupAdminInterface.PathApiElmAppState),
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

        static TruncateProcessHistoryReport truncateProcessHistory(
            string site,
            string siteDefaultPassword,
            bool promptForPasswordOnConsole)
        {
            var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);
            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var requestUrl =
                site.TrimEnd('/') + StartupAdminInterface.PathApiTruncateProcessHistory;

            Console.WriteLine("Beginning to truncate process history at '" + site + "'...");

            var httpResponse = AttemptHttpRequest(() =>
                    new System.Net.Http.HttpRequestMessage
                    {
                        Method = System.Net.Http.HttpMethod.Post,
                        RequestUri = new Uri(requestUrl),
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

        static (IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> files, string lastCompositionLogRecordHashBase16) readFilesForRestoreProcessFromAdminInterface(
            string sourceAdminInterface,
            string sourceAdminPassword)
        {
            using (var sourceHttpClient = new System.Net.Http.HttpClient { BaseAddress = new Uri(sourceAdminInterface) })
            {
                sourceHttpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdmin(sourceAdminPassword))));

                var processHistoryFileStoreRemoteReader = new Kalmit.DelegatingFileStoreReader
                {
                    GetFileContentDelegate = filePath =>
                    {
                        var httpRequestPath =
                            Kalmit.PersistentProcess.WebHost.StartupAdminInterface.PathApiProcessHistoryFileStoreGetFileContent + "/" +
                            string.Join("/", filePath);

                        var response = sourceHttpClient.GetAsync(httpRequestPath).Result;

                        if (response.StatusCode == System.Net.HttpStatusCode.NotFound)
                            return null;

                        if (!response.IsSuccessStatusCode)
                            throw new Exception("Unexpected response status code: " + ((int)response.StatusCode) + " (" + response.StatusCode + ").");

                        return response.Content.ReadAsByteArrayAsync().Result;
                    }
                };

                return Kalmit.PersistentProcess.WebHost.PersistentProcess.PersistentProcessVolatileRepresentation.GetFilesForRestoreProcess(processHistoryFileStoreRemoteReader);
            }
        }

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> LoadFilesForRestoreFromPathAndLogToConsole(
            string sourcePath, string sourcePassword)
        {
            if (Regex.IsMatch(sourcePath, "http(|s)://", RegexOptions.IgnoreCase))
            {
                Console.WriteLine("Begin reading process history from '" + sourcePath + "' ...");

                var restoreResult = readFilesForRestoreProcessFromAdminInterface(
                    sourceAdminInterface: sourcePath,
                    sourceAdminPassword: sourcePassword);

                Console.WriteLine("Completed reading files to restore process " + restoreResult.lastCompositionLogRecordHashBase16 + ". Read " + restoreResult.files.Count + " files from '" + sourcePath + "'.");

                return restoreResult.files;
            }

            var archive = File.ReadAllBytes(sourcePath);

            var zipArchiveEntries = ZipArchive.EntriesFromZipArchive(archive);

            return
                ElmApp.ToFlatDictionaryWithPathComparer(
                    Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(zipArchiveEntries)
                    .EnumerateBlobsTransitive()
                    .Select(blobPathAndContent => (
                        fileName: (IImmutableList<string>)blobPathAndContent.path.Select(name => System.Text.Encoding.UTF8.GetString(name.ToArray())).ToImmutableList(),
                        fileContent: blobPathAndContent.blobContent)));
        }

        static public void replicateProcessAndLogToConsole(
            string site,
            string sitePassword,
            string sourcePath,
            string sourcePassword)
        {
            var restoreFiles =
                LoadFilesForRestoreFromPathAndLogToConsole(sourcePath: sourcePath, sourcePassword: sourcePassword);

            var processHistoryTree =
                Composition.SortedTreeFromSetOfBlobsWithStringPath(restoreFiles);

            var processHistoryComponentHash = Composition.GetHash(Composition.FromTree(processHistoryTree));
            var processHistoryComponentHashBase16 = CommonConversion.StringBase16FromByteArray(processHistoryComponentHash);

            var processHistoryZipArchive = ZipArchive.ZipArchiveFromEntries(restoreFiles);

            using (var httpClient = new System.Net.Http.HttpClient())
            {
                httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdmin(sitePassword))));

                var deployAddress =
                    site.TrimEnd('/') +
                    Kalmit.PersistentProcess.WebHost.StartupAdminInterface.PathApiReplaceProcessHistory;

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
                .Split(System.IO.Path.PathSeparator).Contains(executableDirectoryPath);

            return (commandName, executableIsRegisteredOnPath, registerExecutableForCurrentUser);
        }

        static public void BuildConfiguration(
            string sourcePath,
            string outputOption)
        {
            var buildResult =
                Kalmit.PersistentProcess.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                    sourcePath: sourcePath);

            var configZipArchive = buildResult.configZipArchive;

            var configZipArchiveFileId =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(configZipArchive));

            var webAppConfigFileId =
                Kalmit.CommonConversion.StringBase16FromByteArray(
                    Composition.GetHash(Composition.FromTree(Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        Kalmit.ZipArchive.EntriesFromZipArchive(configZipArchive)))));

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

        static void writeReportToFileInReportDirectory(string reportContent, string reportKind)
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
                    new System.Net.Http.Headers.ProductHeaderValue("elm-fullstack-cli", Kalmit.PersistentProcess.WebHost.Program.AppVersionId)));

            return httpRequest;
        }

        static readonly DateTimeOffset programStartTime = DateTimeOffset.UtcNow;
    }
}
