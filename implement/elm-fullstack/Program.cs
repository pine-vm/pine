using System;
using System.Linq;
using System.Text.RegularExpressions;
using Kalmit.PersistentProcess.WebHost;
using McMaster.Extensions.CommandLineUtils;

namespace elm_fullstack
{
    class Program
    {
        static string AppVersionId => "2020-02-20";

        static int Main(string[] args)
        {
            var app = new CommandLineApplication
            {
                Name = "elm-fullstack",
                Description = "Welcome to Elm fullstack! This tool helps you build and run full stack web applications using the Elm programming language.\nTo get help or report an issue, see the project website at http://elm-fullstack.org/",
            };

            app.HelpOption(inherited: true);

            app.VersionOption(template: "-v|--version", shortFormVersion: "version " + AppVersionId);

            app.Command("run-server", runServerCmd =>
            {
                runServerCmd.Description = "Run a web server with your Elm app.";

                runServerCmd.ThrowOnUnexpectedArgument = false;

                var processStoreDirectoryPathOption = runServerCmd.Option("--process-store-directory-path", "Directory in the file system to contain the backend process store.", CommandOptionType.SingleValue).IsRequired(allowEmptyStrings: false);
                var webAppConfigurationFilePathOption = runServerCmd.Option("--web-app-configuration-file-path", "Path to a file containing the complete configuration in a zip-archive. If you don't use this option, the server uses the current directory as the source.", CommandOptionType.SingleValue);
                var deletePreviousBackendStateOption = runServerCmd.Option("--delete-previous-backend-state", "Delete the previous state of the backend process. If you don't use this option, the server restores the last state backend on startup.", CommandOptionType.NoValue);
                var frontendWebElmMakeAppendixOption = runServerCmd.Option("--frontend-web-elm-make-appendix", "Arguments to add when using elm make to build the frontend app.", CommandOptionType.SingleValue);

                runServerCmd.OnExecute(() =>
                {
                    var processStoreDirectoryPath = processStoreDirectoryPathOption.Value();

                    if (deletePreviousBackendStateOption.HasValue())
                    {
                        Console.WriteLine("Deleting the previous backend state from '" + processStoreDirectoryPath + "'");

                        if (System.IO.Directory.Exists(processStoreDirectoryPath))
                            System.IO.Directory.Delete(processStoreDirectoryPath, true);
                    }

                    var webHostBuilder = Kalmit.PersistentProcess.WebHost.Program.CreateWebHostBuilder(runServerCmd.RemainingArguments.ToArray());

                    webHostBuilder.WithSettingProcessStoreDirectoryPath(processStoreDirectoryPath);

                    webHostBuilder.WithSettingWebAppConfigurationFilePath(webAppConfigurationFilePathOption.Value());

                    webHostBuilder.WithSettingFrontendWebElmMakeAppendix(frontendWebElmMakeAppendixOption.Value());

                    Microsoft.AspNetCore.Hosting.WebHostExtensions.Run(webHostBuilder.Build());
                });
            });

            app.Command("build-config", buildConfigCmd =>
            {
                buildConfigCmd.Description = "Build a configuration file that can be used to run a server.";

                buildConfigCmd.ThrowOnUnexpectedArgument = false;

                buildConfigCmd.OnExecute(() =>
                {
                    Kalmit.PersistentProcess.WebHost.BuildConfigurationFromArguments.BuildConfiguration(args);
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

        static string GetCurrentProcessExecutableFilePath() =>
            System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName;

        static (string filePath, string directoryPath, string fileName) GetCurrentProcessExecutableFilePathAndComponents()
        {
            var filePath = GetCurrentProcessExecutableFilePath();

            return (filePath, System.IO.Path.GetDirectoryName(filePath), System.IO.Path.GetFileName(filePath));
        }
    }
}
