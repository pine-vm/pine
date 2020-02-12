using System;
using Kalmit.PersistentProcess.WebHost;
using McMaster.Extensions.CommandLineUtils;

namespace elm_fullstack
{
    class Program
    {
        static string AppVersionId => "2020-02-12";

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

                    if (webAppConfigurationFilePathOption.HasValue())
                    {
                        webHostBuilder.WithSettingWebAppConfigurationFilePath(webAppConfigurationFilePathOption.Value());
                    }

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

            app.OnExecute(() =>
            {
                Console.WriteLine("Please specify a subcommand.");
                app.ShowHelp();

                return 1;
            });

            return app.Execute(args);
        }
    }
}
