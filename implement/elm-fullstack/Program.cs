using System;
using System.Collections.Concurrent;
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
                runServerCmd.Description = "Run a web server supporting administration of an Elm-fullstack app via HTTP. Deployments and migrations of an app usually go through this HTTP server.";

                var adminInterfaceHttpPortDefault = 4000;

                var processStoreDirectoryPathOption = runServerCmd.Option("--process-store-directory-path", "Directory in the file system to contain the process store.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);
                var deletePreviousBackendStateOption = runServerCmd.Option("--delete-previous-backend-state", "Delete the previous state of the backend process. If you don't use this option, the server restores the last state backend on startup.", CommandOptionType.NoValue);
                var adminInterfaceHttpPortOption = runServerCmd.Option("--admin-interface-http-port", "Port for the admin interface HTTP web host. The default is " + adminInterfaceHttpPortDefault.ToString() + ".", CommandOptionType.SingleValue);
                var adminRootPasswordOption = runServerCmd.Option("--admin-root-password", "Password to access the admin interface with the username 'root'.", CommandOptionType.SingleValue);
                var publicAppUrlsOption = runServerCmd.Option("--public-urls", "URLs to serve the public app from. The default is '" + string.Join(",", Kalmit.PersistentProcess.WebHost.StartupAdminInterface.PublicWebHostUrlsDefault) + "'.", CommandOptionType.SingleValue);
                var replicateProcessFromOption = runServerCmd.Option("--replicate-process-from", "Source to replicate a process from. Can be a URL to a another host admin interface or a path to an archive containing files representing the process state. This option also erases any previously-stored history like '--delete-previous-backend-state'.", CommandOptionType.SingleValue);
                var replicateProcessAdminPassword = runServerCmd.Option("--replicate-process-admin-password", "Used together with '--replicate-process-from' if the source requires a password to authenticate.", CommandOptionType.SingleValue);
                var deployOption = runServerCmd.Option("--deploy", "Perform a deployment on startup, analogous to deploying with the `deploy` command. Can be combined with '--replicate-process-from'.", CommandOptionType.NoValue);

                runServerCmd.OnExecute(() =>
                {
                    var processStoreDirectoryPath = processStoreDirectoryPathOption.Value();

                    var publicAppUrls =
                        publicAppUrlsOption.Value()?.Split(',').Select(url => url.Trim()).ToArray() ??
                        Kalmit.PersistentProcess.WebHost.StartupAdminInterface.PublicWebHostUrlsDefault;

                    if (deletePreviousBackendStateOption.HasValue() || replicateProcessFromOption.HasValue())
                    {
                        Console.WriteLine("Deleting the previous process state from '" + processStoreDirectoryPath + "'...");

                        if (System.IO.Directory.Exists(processStoreDirectoryPath))
                            System.IO.Directory.Delete(processStoreDirectoryPath, true);

                        Console.WriteLine("Completed deleting the previous process state from '" + processStoreDirectoryPath + "'.");
                    }

                    var processStoreFileStore = new FileStoreFromSystemIOFile(processStoreDirectoryPath);

                    if (replicateProcessFromOption.HasValue())
                    {
                        var replicatedFiles = readFilesForRestoreProcessFromAdminInterface(
                            sourceAdminInterface: replicateProcessFromOption.Value(),
                            sourceAdminRootPassword: replicateProcessAdminPassword.Value());

                        foreach (var file in replicatedFiles)
                            processStoreFileStore.SetFileContent(file.Key, file.Value.ToArray());
                    }

                    var adminInterfaceHttpPort =
                        int.Parse(adminInterfaceHttpPortOption.Value() ?? adminInterfaceHttpPortDefault.ToString());

                    var adminInterfaceUrl = "http://*:" + adminInterfaceHttpPort.ToString();

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

                    if (deployOption.HasValue())
                    {
                        System.Threading.Tasks.Task.Delay(1000).Wait();

                        /*
                        TODO:
                        Make deploy synchronous: Inject this in the host to be processed at startup, so that no public app is started for the pre-deploy state.
                        Also, to prevent confusion, we might want to fail starting the server completely in case the deployment fails.
                        */

                        deploy(
                            adminInterface: "http://localhost:" + adminInterfaceHttpPort,
                            adminRootPassword: adminRootPasswordOption.Value(),
                            initElmAppState: deletePreviousBackendStateOption.HasValue() && !replicateProcessFromOption.HasValue());
                    }

                    Microsoft.AspNetCore.Hosting.WebHostExtensions.WaitForShutdown(webHost);
                });
            });

            app.Command("deploy", deployCmd =>
            {
                deployCmd.Description = "Deploy an app to a server that was started with the `run-server` command. By default, migrates from the previous Elm app state using the `migrate` function in the Elm app code.";

                var adminInterfaceOption = deployCmd.Option("--admin-interface", "Address to the admin interface of the server to deploy to.", CommandOptionType.SingleValue).IsRequired();
                var adminRootPasswordOption = deployCmd.Option("--admin-root-password", "Password to access the admin interface with the username 'root'.", CommandOptionType.SingleValue).IsRequired();
                var initElmAppStateOption = deployCmd.Option("--init-elm-app-state", "Do not attempt to migrate the Elm app state but use the state from the init function. Defaults to false.", CommandOptionType.NoValue);

                deployCmd.OnExecute(() =>
                {
                    deploy(
                        adminInterface: adminInterfaceOption.Value(),
                        adminRootPassword: adminRootPasswordOption.Value(),
                        initElmAppState: initElmAppStateOption.HasValue());
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

        static void deploy(string adminInterface, string adminRootPassword, bool initElmAppState)
        {
            var buildConfigurationLog = new System.Collections.Generic.List<String>();

            Console.WriteLine("Beginning to build configuration...");

            var (compileConfigZipArchive, loweredElmAppFiles) =
                Kalmit.PersistentProcess.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchive(
                    buildConfigurationLog.Add);

            var webAppConfigZipArchive = compileConfigZipArchive();

            var webAppConfigZipArchiveFileId =
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(webAppConfigZipArchive));

            var webAppConfigFileId =
                Kalmit.CommonConversion.StringBase16FromByteArray(
                    Composition.GetHash(Composition.FromTree(Composition.TreeFromSetOfBlobsWithCommonFilePath(
                        Kalmit.ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive)))));

            Console.WriteLine(
                "Built zip archive " + webAppConfigZipArchiveFileId + " containing web app config " + webAppConfigFileId + ".");

            using (var httpClient = new System.Net.Http.HttpClient())
            {
                httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdminRoot(adminRootPassword))));

                var deployAddress =
                    (adminInterface.TrimEnd('/')) +
                    (initElmAppState
                    ?
                    StartupAdminInterface.PathApiDeployAppConfigAndInitElmAppState
                    :
                    StartupAdminInterface.PathApiDeployAppConfigAndMigrateElmAppState);

                Console.WriteLine("Beginning to deploy app '" + webAppConfigFileId + "' to '" + deployAddress + "'...");

                var httpContent = new System.Net.Http.ByteArrayContent(webAppConfigZipArchive);

                httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/zip");
                httpContent.Headers.ContentDisposition =
                    new System.Net.Http.Headers.ContentDispositionHeaderValue("attachment") { FileName = webAppConfigFileId + ".zip" };

                var httpResponse = httpClient.PostAsync(deployAddress, httpContent).Result;

                Console.WriteLine(
                    "Server response: " + httpResponse.StatusCode + "\n" +
                     httpResponse.Content.ReadAsStringAsync().Result);
            }
        }

        static IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> readFilesForRestoreProcessFromAdminInterface(
            string sourceAdminInterface,
            string sourceAdminRootPassword)
        {
            var processHistoryfilesFromRemoteHost = ImmutableDictionary<IImmutableList<string>, IImmutableList<byte>>.Empty;

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

                        var fileContent = response.Content.ReadAsByteArrayAsync().Result;

                        processHistoryfilesFromRemoteHost =
                            processHistoryfilesFromRemoteHost.SetItem(filePath, fileContent.ToImmutableList());

                        return fileContent;
                    }
                };

                using (var processVolatileRepresentation =
                    Kalmit.PersistentProcess.WebHost.PersistentProcess.PersistentProcessVolatileRepresentation
                    .Restore(new Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore(
                        processHistoryFileStoreRemoteReader), _ => { }))
                {
                }
            }

            return processHistoryfilesFromRemoteHost;
        }

        static public void replicateProcess(
            string destinationAdminInterface,
            string destinationAdminRootPassword,
            string sourceAdminInterface,
            string sourceAdminRootPassword)
        {
            Console.WriteLine("Begin reading process history from '" + sourceAdminInterface + "' ...");

            var processHistoryfilesFromRemoteHost =
                readFilesForRestoreProcessFromAdminInterface(sourceAdminInterface, sourceAdminRootPassword);

            Console.WriteLine("Completed reading part of process history for restore. Read " + processHistoryfilesFromRemoteHost.Count + " files from " + sourceAdminInterface + " during restore.");

            var processHistoryTree =
                Composition.TreeFromSetOfBlobsWithStringPath(processHistoryfilesFromRemoteHost);

            var processHistoryComponentHash = Composition.GetHash(Composition.FromTree(processHistoryTree));
            var processHistoryComponentHashBase16 = CommonConversion.StringBase16FromByteArray(processHistoryComponentHash);

            var processHistoryZipArchive = ZipArchive.ZipArchiveFromEntries(processHistoryfilesFromRemoteHost);

            using (var httpClient = new System.Net.Http.HttpClient())
            {
                httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
                    "Basic",
                    Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        Kalmit.PersistentProcess.WebHost.Configuration.BasicAuthenticationForAdminRoot(destinationAdminRootPassword))));

                var deployAddress =
                    destinationAdminInterface.TrimEnd('/') +
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

        static string GetCurrentProcessExecutableFilePath() =>
            System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName;

        static (string filePath, string directoryPath, string fileName) GetCurrentProcessExecutableFilePathAndComponents()
        {
            var filePath = GetCurrentProcessExecutableFilePath();

            return (filePath, System.IO.Path.GetDirectoryName(filePath), System.IO.Path.GetFileName(filePath));
        }
    }
}
