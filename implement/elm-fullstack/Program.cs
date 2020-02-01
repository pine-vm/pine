using System;
using McMaster.Extensions.CommandLineUtils;

namespace elm_fullstack
{
    class Program
    {
        static string AppVersionId => "2020-02-01";

        static int Main(string[] args)
        {
            var app = new CommandLineApplication
            {
                Name = "elm-fullstack",
                Description = "Welcome to Elm fullstack! This tool helps you build and run fullstack web applications using the Elm programming language.\nTo get help or report an issue, see the project website at http://elm-fullstack.org/",
            };

            app.HelpOption(inherited: true);

            app.VersionOption(template: "-v|--version", shortFormVersion: "version " + AppVersionId);

            app.Command("run-server", runServerCmd =>
            {
                runServerCmd.Description = "Run a web server with your Elm app.";

                runServerCmd.OnExecute(() =>
                {
                    var webHostBuilder = Kalmit.PersistentProcess.WebHost.Program.CreateWebHostBuilder(runServerCmd.RemainingArguments.ToArray());
                    Microsoft.AspNetCore.Hosting.WebHostExtensions.Run(webHostBuilder.Build());
                });
            });

            app.Command("build-config", buildConfigCmd =>
            {
                buildConfigCmd.Description = "Build a configuration file that can be used to run a server.";

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
