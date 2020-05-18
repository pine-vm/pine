using System;
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
            var app = new CommandLineApplication
            {
                Name = "elm-fullstack",
                Description = "Welcome to Elm fullstack! This tool helps you build and run full stack web applications using the Elm programming language.\nTo get help or report an issue, see the project website at http://elm-fullstack.org/",
            };

            app.HelpOption(inherited: true);

            app.VersionOption(template: "-v|--version", shortFormVersion: "version " + Kalmit.PersistentProcess.WebHost.Program.AppVersionId);

            CommandOption verboseLogOptionFromCommand(CommandLineApplication command) =>
                command.Option("--verbose-log", "", CommandOptionType.NoValue);

            app.Command("build-config", buildConfigCmd =>
            {
                buildConfigCmd.Description = "Build a configuration file that can be used to run a server.";

                var verboseLogOption = verboseLogOptionFromCommand(buildConfigCmd);
                var outputOption = buildConfigCmd.Option("--output", "Path to write the zip-archive to.", CommandOptionType.SingleValue);
                var loweredElmOutputOption = buildConfigCmd.Option("--lowered-elm-output", "Path to a directory to write the lowered Elm app files.", CommandOptionType.SingleValue);

                buildConfigCmd.ThrowOnUnexpectedArgument = false;

                buildConfigCmd.OnExecute(() =>
                {
                    var verboseLogWriteLine =
                        verboseLogOption.HasValue() ?
                        (Action<string>)Console.WriteLine
                        :
                        null;

                    BuildConfiguration(
                        outputOption: outputOption.Value(),
                        loweredElmOutputOption: loweredElmOutputOption.Value(),
                        verboseLogWriteLine: verboseLogWriteLine);
                });
            });

            app.Command("install-command", installCmd =>
            {
                var (commandName, _, registerExecutableDirectoryOnPath) = CheckIfExecutableIsRegisteredOnPath();

                installCmd.Description = "Installs the '" + commandName + "' command for the current user account.";
                installCmd.ThrowOnUnexpectedArgument = true;

                installCmd.OnExecute(() =>
                {
                    registerExecutableDirectoryOnPath();
                });
            });

            app.Command("run-server", runServerCmd =>
            {
                runServerCmd.Description = "Run a web server supporting administration of an Elm-fullstack process via HTTP. This HTTP interface supports deployments, migrations, etc.";
                runServerCmd.ThrowOnUnexpectedArgument = true;

                var adminInterfaceHttpPortDefault = 4000;

                var processStoreDirectoryPathOption = runServerCmd.Option("--process-store-directory-path", "Directory in the file system to contain the process store.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);
                var deletePreviousProcessOption = runServerCmd.Option("--delete-previous-process", "Delete the previous backend process found in the given store. If you don't use this option, the server restores the process from the persistent store on startup.", CommandOptionType.NoValue);
                var adminInterfaceHttpPortOption = runServerCmd.Option("--admin-interface-http-port", "Port for the admin interface HTTP web host. The default is " + adminInterfaceHttpPortDefault.ToString() + ".", CommandOptionType.SingleValue);
                var adminRootPasswordOption = runServerCmd.Option("--admin-root-password", "Password to access the admin interface with the username 'root'.", CommandOptionType.SingleValue);
                var publicAppUrlsOption = runServerCmd.Option("--public-urls", "URLs to serve the public app from. The default is '" + string.Join(",", Kalmit.PersistentProcess.WebHost.StartupAdminInterface.PublicWebHostUrlsDefault) + "'.", CommandOptionType.SingleValue);
                var replicateProcessFromOption = runServerCmd.Option("--replicate-process-from", "Source to replicate a process from. Can be a URL to a another host admin interface or a path to an archive containing files representing the process state. This option also erases any previously-stored history like '--delete-previous-process'.", CommandOptionType.SingleValue);
                var replicateProcessAdminPasswordOption = runServerCmd.Option("--replicate-process-admin-password", "Used together with '--replicate-process-from' if the source requires a password to authenticate.", CommandOptionType.SingleValue);
                var deployAppConfigOption = runServerCmd.Option("--deploy-app-config", "Perform a deployment on startup, analogous to deploying with the `deploy-app-config` command. Can be combined with '--replicate-process-from'.", CommandOptionType.NoValue);

                runServerCmd.OnExecute(() =>
                {
                    var processStoreDirectoryPath = processStoreDirectoryPathOption.Value();

                    var publicAppUrls =
                        publicAppUrlsOption.Value()?.Split(',').Select(url => url.Trim()).ToArray() ??
                        Kalmit.PersistentProcess.WebHost.StartupAdminInterface.PublicWebHostUrlsDefault;

                    var replicateProcessSource = replicateProcessFromOption.Value();

                    var replicateProcessAdminPassword =
                        replicateProcessAdminPasswordOption.Value() ?? UserSecrets.LoadPasswordForSite(replicateProcessSource);

                    if (deletePreviousProcessOption.HasValue() || replicateProcessFromOption.HasValue())
                    {
                        Console.WriteLine("Deleting the previous process state from '" + processStoreDirectoryPath + "'...");

                        if (System.IO.Directory.Exists(processStoreDirectoryPath))
                            System.IO.Directory.Delete(processStoreDirectoryPath, true);

                        Console.WriteLine("Completed deleting the previous process state from '" + processStoreDirectoryPath + "'.");
                    }

                    var processStoreFileStore = new FileStoreFromSystemIOFile(processStoreDirectoryPath);

                    if (replicateProcessSource != null)
                    {
                        var replicateFiles =
                            LoadFilesForRestoreFromSourceAndLogToConsole(
                                replicateProcessSource, replicateProcessAdminPassword);

                        foreach (var file in replicateFiles)
                            processStoreFileStore.SetFileContent(file.Key, file.Value.ToArray());
                    }

                    var adminInterfaceHttpPort =
                        int.Parse(adminInterfaceHttpPortOption.Value() ?? adminInterfaceHttpPortDefault.ToString());

                    var adminInterfaceUrl = "http://*:" + adminInterfaceHttpPort.ToString();

                    if (deployAppConfigOption.HasValue())
                    {
                        Console.WriteLine("Loading app config to deploy...");

                        var appConfigZipArchive =
                            Kalmit.PersistentProcess.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchive(
                                _ => { }).compileConfigZipArchive();

                        var appConfigTree =
                            Composition.TreeFromSetOfBlobsWithCommonFilePath(
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

                        var compositionLogEvent =
                            Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                                appConfigValueInFile: appConfigValueInFile,
                                initElmAppState: deletePreviousProcessOption.HasValue() && !replicateProcessFromOption.HasValue());

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
                        .UseUrls(adminInterfaceUrl)
                        .UseStartup<StartupAdminInterface>()
                        .WithSettingPublicWebHostUrls(publicAppUrls)
                        .WithProcessStoreFileStore(processStoreFileStore);

                    if (adminRootPasswordOption.HasValue())
                        webHostBuilder = webHostBuilder.WithSettingAdminRootPassword(adminRootPasswordOption.Value());

                    var webHost = webHostBuilder.Build();

                    Console.WriteLine("Starting the web server with the admin interface...");

                    webHost.Start();

                    Console.WriteLine("Completed starting the web server with the admin interface at '" + adminInterfaceUrl + "'.");

                    Microsoft.AspNetCore.Hosting.WebHostExtensions.WaitForShutdown(webHost);
                });
            });

            Func<(string site, string sitePassword)> siteAndSitePasswordOptionsOnCommand(CommandLineApplication cmd)
            {
                var siteOption = cmd.Option("--site", "Site where to apply the changes. Usually an URL to the admin interface of a server.", CommandOptionType.SingleValue).IsRequired();
                var sitePasswordOption = cmd.Option("--site-password", "Password to access the site where to apply the changes.", CommandOptionType.SingleValue);

                return () =>
                {
                    var site = siteOption.Value();

                    var sitePassword =
                        sitePasswordOption.Value() ?? UserSecrets.LoadPasswordForSite(site);

                    return (site, sitePassword);
                };
            }

            app.Command("deploy-app-config", deployAppConfigCmd =>
            {
                deployAppConfigCmd.Description = "Deploy an app to a server that was started with the `run-server` command. By default, migrates from the previous Elm app state using the `migrate` function in the Elm app code.";
                deployAppConfigCmd.ThrowOnUnexpectedArgument = true;

                var siteAndPasswordFromCmd = siteAndSitePasswordOptionsOnCommand(deployAppConfigCmd);

                var initElmAppStateOption = deployAppConfigCmd.Option("--init-elm-app-state", "Do not attempt to migrate the Elm app state but use the state from the init function. Defaults to false.", CommandOptionType.NoValue);

                deployAppConfigCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

                    var deployReport =
                        deployAppConfig(
                            site: site,
                            sitePassword: sitePassword,
                            initElmAppState: initElmAppStateOption.HasValue());

                    writeReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(deployReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "deploy-app-config.json");
                });
            });

            app.Command("set-elm-app-state", setElmAppStateCmd =>
            {
                setElmAppStateCmd.Description = "Attempt to set the state of a backend Elm app using the common serialized representation.";
                setElmAppStateCmd.ThrowOnUnexpectedArgument = true;

                var siteAndPasswordFromCmd = siteAndSitePasswordOptionsOnCommand(setElmAppStateCmd);

                var sourceOption = setElmAppStateCmd.Option("--source", "Source to load the serialized state representation from.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);

                setElmAppStateCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

                    var attemptReport =
                        setElmAppState(
                            site: site,
                            sitePassword: sitePassword,
                            source: sourceOption.Value());

                    writeReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(attemptReport, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "set-elm-app-state.json");
                });
            });

            app.Command("truncate-process-history", truncateProcessHistoryCmd =>
            {
                truncateProcessHistoryCmd.Description = "Remove parts of the process history from the persistent store, which are not needed to restore the process.";
                truncateProcessHistoryCmd.ThrowOnUnexpectedArgument = true;

                var siteAndPasswordFromCmd = siteAndSitePasswordOptionsOnCommand(truncateProcessHistoryCmd);

                truncateProcessHistoryCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

                    var report =
                        truncateProcessHistory(
                            site: site,
                            sitePassword: sitePassword);

                    writeReportToFileInReportDirectory(
                        reportContent: Newtonsoft.Json.JsonConvert.SerializeObject(report, Newtonsoft.Json.Formatting.Indented),
                        reportKind: "truncate-process-history.json");
                });
            });

            app.Command("archive-process", archiveProcessCmd =>
            {
                archiveProcessCmd.Description = "Copy the files needed to restore the process and store those in a zip-archive.";
                archiveProcessCmd.ThrowOnUnexpectedArgument = true;

                var siteAndPasswordFromCmd = siteAndSitePasswordOptionsOnCommand(archiveProcessCmd);

                archiveProcessCmd.OnExecute(() =>
                {
                    var (site, sitePassword) = siteAndPasswordFromCmd();

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
                userSecretsCmd.ThrowOnUnexpectedArgument = true;

                userSecretsCmd.Command("store", storeCmd =>
                {
                    storeCmd.ThrowOnUnexpectedArgument = true;

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

            app.OnExecute(() =>
            {
                Console.WriteLine("Please specify a subcommand.");
                app.ShowHelp();

                return 1;
            });

            return app.Execute(args);
        }

        class DeployAppConfigReport
        {
            public string beginTime;

            public string appConfigSource;

            public string appConfigId;

            public bool initElmAppState;

            public string site;

            public ResponseFromServerStruct responseFromServer;

            public int totalTimeSpentMilli;

            public class ResponseFromServerStruct
            {
                public int? statusCode;

                public object body;
            }
        }

        static DeployAppConfigReport deployAppConfig(string site, string sitePassword, bool initElmAppState)
        {
            var beginTime = DateTimeOffset.UtcNow.ToString("yyyy-MM-ddTHH-mm-ss");

            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var buildConfigurationLog = new System.Collections.Generic.List<String>();

            Console.WriteLine("Beginning to build configuration...");

            var (compileConfigZipArchive, loweredElmAppFiles) =
                Kalmit.PersistentProcess.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchive(
                    buildConfigurationLog.Add);

            var appConfigZipArchive = compileConfigZipArchive();

            var appConfigZipArchiveFileId =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(appConfigZipArchive));

            var appConfigId =
                Kalmit.CommonConversion.StringBase16FromByteArray(
                    Composition.GetHash(Composition.FromTree(Composition.TreeFromSetOfBlobsWithCommonFilePath(
                        Kalmit.ZipArchive.EntriesFromZipArchive(appConfigZipArchive)))));

            Console.WriteLine(
                "Built zip archive " + appConfigZipArchiveFileId + " containing app config " + appConfigId + ".");

            DeployAppConfigReport.ResponseFromServerStruct responseFromServer = null;

            using (var httpClient = new System.Net.Http.HttpClient())
            {
                httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdminRoot(sitePassword))));

                var deployAddress =
                    (site.TrimEnd('/')) +
                    (initElmAppState
                    ?
                    StartupAdminInterface.PathApiDeployAppConfigAndInitElmAppState
                    :
                    StartupAdminInterface.PathApiDeployAppConfigAndMigrateElmAppState);

                Console.WriteLine("Beginning to deploy app '" + appConfigId + "' to '" + deployAddress + "'...");

                var httpContent = new System.Net.Http.ByteArrayContent(appConfigZipArchive);

                httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/zip");
                httpContent.Headers.ContentDisposition =
                    new System.Net.Http.Headers.ContentDispositionHeaderValue("attachment") { FileName = appConfigId + ".zip" };

                var httpResponse = httpClient.PostAsync(deployAddress, httpContent).Result;

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

                responseFromServer = new DeployAppConfigReport.ResponseFromServerStruct
                {
                    statusCode = (int)httpResponse.StatusCode,
                    body = responseBodyReport,
                };
            }

            return new DeployAppConfigReport
            {
                beginTime = beginTime,
                appConfigSource = "current-directory",
                appConfigId = appConfigId,
                initElmAppState = initElmAppState,
                site = site,
                responseFromServer = responseFromServer,
                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
            };
        }

        class SetElmAppStateReport
        {
            public string beginTime;

            public string elmAppStateSource;

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

        static SetElmAppStateReport setElmAppState(string site, string sitePassword, string source)
        {
            var beginTime = DateTimeOffset.UtcNow.ToString("yyyy-MM-ddTHH-mm-ss");

            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            // For now only support a file path as source.

            var elmAppStateSerialized = File.ReadAllBytes(source);

            var elmAppStateComponent = Composition.Component.Blob(elmAppStateSerialized);

            var elmAppStateId = CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppStateComponent));

            SetElmAppStateReport.ResponseFromServerStruct responseFromServer = null;

            using (var httpClient = new System.Net.Http.HttpClient())
            {
                httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdminRoot(sitePassword))));

                var httpContent = new System.Net.Http.ByteArrayContent(elmAppStateSerialized);

                httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/json");

                var httpResponse = httpClient.PostAsync(
                    site.TrimEnd('/') + StartupAdminInterface.PathApiElmAppState, httpContent).Result;

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

                responseFromServer = new SetElmAppStateReport.ResponseFromServerStruct
                {
                    statusCode = (int)httpResponse.StatusCode,
                    body = responseBodyReport,
                };
            }

            return new SetElmAppStateReport
            {
                beginTime = beginTime,
                elmAppStateSource = source,
                elmAppStateId = elmAppStateId,
                site = site,
                responseFromServer = responseFromServer,
                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
            };
        }

        class TruncateProcessHistoryReport
        {
            public string site;

            public ResponseFromServerStruct responseFromServer;

            public int totalTimeSpentMilli;

            public class ResponseFromServerStruct
            {
                public int? statusCode;

                public object body;
            }
        }

        static TruncateProcessHistoryReport truncateProcessHistory(string site, string sitePassword)
        {
            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            using (var httpClient = new System.Net.Http.HttpClient())
            {
                httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdminRoot(sitePassword))));

                var requestUrl =
                    site.TrimEnd('/') + StartupAdminInterface.PathApiTruncateProcessHistory;

                Console.WriteLine("Beginning to truncate process history at '" + site + "'...");

                var httpResponse = httpClient.PostAsync(requestUrl, null).Result;

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
                    site = site,
                    responseFromServer = responseFromServer,
                    totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
                };
            }
        }

        static (IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> files, string lastCompositionLogRecordHashBase16) readFilesForRestoreProcessFromAdminInterface(
            string sourceAdminInterface,
            string sourceAdminRootPassword)
        {
            using (var sourceHttpClient = new System.Net.Http.HttpClient { BaseAddress = new Uri(sourceAdminInterface) })
            {
                sourceHttpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdminRoot(sourceAdminRootPassword))));

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

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> LoadFilesForRestoreFromSourceAndLogToConsole(
            string source, string sourcePassword)
        {
            if (Regex.IsMatch(source, "http(|s)://", RegexOptions.IgnoreCase))
            {
                Console.WriteLine("Begin reading process history from '" + source + "' ...");

                var restoreResult = readFilesForRestoreProcessFromAdminInterface(
                    sourceAdminInterface: source,
                    sourceAdminRootPassword: sourcePassword);

                Console.WriteLine("Completed reading files to restore process " + restoreResult.lastCompositionLogRecordHashBase16 + ". Read " + restoreResult.files.Count + " files from '" + source + "'.");

                return restoreResult.files;
            }

            var archive = File.ReadAllBytes(source);

            var zipArchiveEntries = ZipArchive.EntriesFromZipArchive(archive);

            return
                ElmApp.ToFlatDictionaryWithPathComparer(
                    Composition.TreeFromSetOfBlobsWithCommonFilePath(zipArchiveEntries)
                    .EnumerateBlobsTransitive()
                    .Select(blobPathAndContent => (
                        fileName: (IImmutableList<string>)blobPathAndContent.path.Select(name => System.Text.Encoding.UTF8.GetString(name.ToArray())).ToImmutableList(),
                        fileContent: blobPathAndContent.blobContent)));
        }

        static public void replicateProcessAndLogToConsole(
            string site,
            string sitePassword,
            string source,
            string sourcePassword)
        {
            var restoreFiles =
                LoadFilesForRestoreFromSourceAndLogToConsole(source, sourcePassword);

            var processHistoryTree =
                Composition.TreeFromSetOfBlobsWithStringPath(restoreFiles);

            var processHistoryComponentHash = Composition.GetHash(Composition.FromTree(processHistoryTree));
            var processHistoryComponentHashBase16 = CommonConversion.StringBase16FromByteArray(processHistoryComponentHash);

            var processHistoryZipArchive = ZipArchive.ZipArchiveFromEntries(restoreFiles);

            using (var httpClient = new System.Net.Http.HttpClient())
            {
                httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdminRoot(sitePassword))));

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
            string outputOption,
            string loweredElmOutputOption,
            Action<string> verboseLogWriteLine)
        {
            var (compileConfigZipArchive, loweredElmAppFiles) =
                Kalmit.PersistentProcess.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchive(
                    verboseLogWriteLine);

            if (0 < loweredElmOutputOption?.Length)
            {
                Console.WriteLine("I write the lowered Elm app to '" + loweredElmOutputOption + "'.");

                foreach (var file in loweredElmAppFiles)
                {
                    var outputPath = Path.Combine(new[] { loweredElmOutputOption }.Concat(file.Key).ToArray());
                    Directory.CreateDirectory(Path.GetDirectoryName(outputPath));
                    File.WriteAllBytes(outputPath, file.Value.ToArray());
                }
            }

            var configZipArchive = compileConfigZipArchive();

            var configZipArchiveFileId =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(configZipArchive));

            var webAppConfigFileId =
                Kalmit.CommonConversion.StringBase16FromByteArray(
                    Composition.GetHash(Composition.FromTree(Composition.TreeFromSetOfBlobsWithCommonFilePath(
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

        static void writeReportToFileInReportDirectory(string reportContent, string reportKind)
        {
            var fileName = DateTimeOffset.UtcNow.ToString("yyyy-MM-ddTHH-mm-ss") + "_" + reportKind;

            var filePath = Path.Combine(Environment.CurrentDirectory, "elm-fullstack-tool", "report", fileName);

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
    }
}
