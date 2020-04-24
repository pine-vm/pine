using System;
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
    class Program
    {
        static string AppVersionId => "2020-04-24";

        static int Main(string[] args)
        {
            var app = new CommandLineApplication
            {
                Name = "elm-fullstack",
                Description = "Welcome to Elm fullstack! This tool helps you build and run full stack web applications using the Elm programming language.\nTo get help or report an issue, see the project website at http://elm-fullstack.org/",
            };

            app.HelpOption(inherited: true);

            app.VersionOption(template: "-v|--version", shortFormVersion: "version " + AppVersionId);

            CommandOption verboseLogOptionFromCommand(CommandLineApplication command) =>
                command.Option("--verbose-log", "", CommandOptionType.NoValue);

            app.Command("run-server", runServerCmd =>
            {
                runServerCmd.Description = "Run a web server with your Elm app.";

                runServerCmd.ThrowOnUnexpectedArgument = false;

                var processStoreDirectoryPathOption = runServerCmd.Option("--process-store-directory-path", "Directory in the file system to contain the backend process store.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);
                var processStoreSeparateReaderDirectoryPathOption = runServerCmd.Option("--process-store-separate-reader-directory-path", "Directory in the file system to read the backend process store to continue from, separate from the directory to write new store entries to. Typically used to test new versions before deploying to production.", CommandOptionType.SingleValue);
                var webAppConfigurationFilePathOption = runServerCmd.Option("--web-app-configuration-file-path", "Path to a file containing the complete configuration in a zip-archive. If you don't use this option, the server uses the current directory as the source.", CommandOptionType.SingleValue);
                var deletePreviousBackendStateOption = runServerCmd.Option("--delete-previous-backend-state", "Delete the previous state of the backend process. If you don't use this option, the server restores the last state backend on startup.", CommandOptionType.NoValue);
                var frontendWebElmMakeAppendixOption = runServerCmd.Option("--frontend-web-elm-make-appendix", "Arguments to add when using elm make to build the frontend app.", CommandOptionType.SingleValue);

                runServerCmd.OnExecute(() =>
                {
                    var processStoreDirectoryPath = processStoreDirectoryPathOption.Value();
                    var processStoreSeparateReaderDirectoryPath = processStoreSeparateReaderDirectoryPathOption.Value();

                    if (deletePreviousBackendStateOption.HasValue())
                    {
                        Console.WriteLine("Deleting the previous backend state from '" + processStoreDirectoryPath + "'");

                        if (System.IO.Directory.Exists(processStoreDirectoryPath))
                            System.IO.Directory.Delete(processStoreDirectoryPath, true);
                    }

                    var webHostBuilder = Kalmit.PersistentProcess.WebHost.Program.CreateWebHostBuilder(runServerCmd.RemainingArguments.ToArray());

                    webHostBuilder.WithSettingProcessStoreDirectoryPath(processStoreDirectoryPath);

                    if (0 < processStoreSeparateReaderDirectoryPath?.Length)
                        webHostBuilder.WithSettingProcessStoreSeparateReaderDirectoryPath(processStoreSeparateReaderDirectoryPath);

                    var webAppConfigurationFilePath = webAppConfigurationFilePathOption.Value();

                    if (webAppConfigurationFilePath != null)
                        webHostBuilder.WithWebAppConfigurationZipArchiveFromFilePath(webAppConfigurationFilePath);

                    webHostBuilder.WithSettingFrontendWebElmMakeAppendix(frontendWebElmMakeAppendixOption.Value());

                    Microsoft.AspNetCore.Hosting.WebHostExtensions.Run(webHostBuilder.Build());
                });
            });

            app.Command("build-config", buildConfigCmd =>
            {
                buildConfigCmd.Description = "Build a configuration file that can be used to run a server.";

                var verboseLogOption = verboseLogOptionFromCommand(buildConfigCmd);
                var outputOption = buildConfigCmd.Option("--output", "Path to write the zip-archive to.", CommandOptionType.SingleValue);
                var loweredElmOutputOption = buildConfigCmd.Option("--lowered-elm-output", "Path to a directory to write the lowered Elm app files.", CommandOptionType.SingleValue);
                var frontendWebElmMakeCommandAppendixOption = buildConfigCmd.Option("--frontend-web-elm-make-appendix", "Text to append when invoking Elm make.", CommandOptionType.SingleValue);

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
                        frontendWebElmMakeCommandAppendixOption: frontendWebElmMakeCommandAppendixOption.Value(),
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

            app.Command("run-server-supporting-migrations", runServerSupportingMigrationsCmd =>
            {
                runServerSupportingMigrationsCmd.Description = "Run a web server that supports migrations between Elm apps. This encapsulates the functionality you get with the `run-server` command.";

                var adminInterfaceHttpPortDefault = 4000;

                var processStoreDirectoryPathOption = runServerSupportingMigrationsCmd.Option("--process-store-directory-path", "Directory in the file system to contain the process store.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);
                var deletePreviousBackendStateOption = runServerSupportingMigrationsCmd.Option("--delete-previous-backend-state", "Delete the previous state of the backend process. If you don't use this option, the server restores the last state backend on startup.", CommandOptionType.NoValue);
                var adminInterfaceHttpPortOption = runServerSupportingMigrationsCmd.Option("--admin-interface-http-port", "Port for the admin interface HTTP web host. The default is " + adminInterfaceHttpPortDefault.ToString() + ".", CommandOptionType.SingleValue);
                var adminRootPasswordOption = runServerSupportingMigrationsCmd.Option("--admin-root-password", "Password to access the admin interface with the username 'root'.", CommandOptionType.SingleValue);

                runServerSupportingMigrationsCmd.OnExecute(() =>
                {
                    var processStoreDirectoryPath = processStoreDirectoryPathOption.Value();

                    if (deletePreviousBackendStateOption.HasValue())
                    {
                        Console.WriteLine("Deleting the previous process state from '" + processStoreDirectoryPath + "'");

                        if (System.IO.Directory.Exists(processStoreDirectoryPath))
                            System.IO.Directory.Delete(processStoreDirectoryPath, true);
                    }

                    var adminInterfaceHttpPort =
                        int.Parse(adminInterfaceHttpPortOption.Value() ?? adminInterfaceHttpPortDefault.ToString());

                    var webHostBuilder =
                        Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                        .ConfigureAppConfiguration(builder => builder.AddEnvironmentVariables("APPSETTING_"))
                        .UseUrls("http://*:" + adminInterfaceHttpPort.ToString())
                        .UseStartup<StartupSupportingMigrations>()
                        .WithSettingProcessStoreDirectoryPath(processStoreDirectoryPath);

                    if (adminRootPasswordOption.HasValue())
                        webHostBuilder = webHostBuilder.WithSettingAdminRootPassword(adminRootPasswordOption.Value());

                    Microsoft.AspNetCore.Hosting.WebHostExtensions.Run(webHostBuilder.Build());
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
            string frontendWebElmMakeCommandAppendixOption,
            Action<string> verboseLogWriteLine)
        {
            var (compileConfigZipArchive, loweredElmAppFiles) =
                Kalmit.PersistentProcess.WebHost.BuildConfigurationFromArguments.BuildConfigurationZipArchive(
                    frontendWebElmMakeCommandAppendixOption, verboseLogWriteLine);

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
                Composition.GetHash(Composition.FromTree(Composition.TreeFromSetOfBlobsWithCommonFilePath(
                    Kalmit.ZipArchive.EntriesFromZipArchive(configZipArchive))));

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
