using System;
using System.Collections.Generic;
using System.CommandLine;
using System.Linq;

namespace Pine.CLI;

public static class HelpCommand
{
    public static Command Create(RootCommand rootCommand)
    {
        var command = new Command("help", "Explain available commands and how to use the command-line interface.");

        var allOption =
            new Option<bool>("--all", ["-a"])
            {
                Description =
                "Show all commands including hidden ones"
            };

        command.Add(allOption);

        command.SetAction(
            (parseResult) =>
            {
                var showAll = parseResult.GetValue(allOption);

                if (showAll)
                {
                    ShowAllCommands(rootCommand);
                }
                else
                {
                    ShowCustomHelp(rootCommand);
                }

                return 0;
            });

        return command;
    }


    internal static void ShowCustomHelp(RootCommand rootCommand)
    {
        var checkedInstallation = PineCliCommand.CheckIfExecutableIsRegisteredOnPath().checkInstallation();
        var elmFsCommandName = PineCliCommand.CheckIfExecutableIsRegisteredOnPath().commandName;

        // Optional short descriptions for overview display
        // When null, uses the command's full Description property
        var shortDescriptions =
            new Dictionary<string, string?>
            {
                ["install"] = "Install the command for the current user account.",
                ["interactive"] = null,
                ["compile"] = "Compile app source code.",
                ["elm-test-rs"] = "Compile and run tests.",
                ["make"] = "Compile Elm code.",
                ["screenshot"] = "Render an HTML, SVG, or Elm entry point to an image.",
                ["elm-format"] = "Format Elm module files.",
                ["dotnet"] = ".NET and C# development tools.",
                ["describe"] = "Describe a composition.",
                ["run"] = null,
                ["run-server"] = "Run a server with a web-based admin interface.",
                ["deploy"] = "Deploy an app to an Elm backend process.",
                ["copy-app-state"] = "Copy the state of an Elm backend app.",
                ["copy-process"] = "Copy all files needed to restore a process.",
                ["list-functions"] = "List the functions exposed by an Elm app.",
                ["apply-function"] = "Apply an Elm function on a database.",
                ["truncate-process-history"] = "Remove parts of the process history.",
                ["run-file-server"] = null,
            };

        var commandsByName = rootCommand.Subcommands.ToDictionary(c => c.Name, c => c);

        string GetDisplayDescription(string commandName)
        {
            if (!commandsByName.TryGetValue(commandName, out var command))
                return "";

            // Use short description if provided, otherwise fall back to full description
            return
                shortDescriptions.TryGetValue(commandName, out var shortDesc) && shortDesc is not null
                ?
                shortDesc
                :
                command.Description ?? "";
        }

        var setupGroupCommandNames = new List<string>();

        if (!checkedInstallation.executableIsRegisteredOnPath)
        {
            setupGroupCommandNames.Add("install");
        }

        var developCommandNames =
            new List<string>
            {
                "interactive",
                "compile",
                "elm",
                "elm-test-rs",
                "make",
                "screenshot",
                "elm-format",
                "dotnet",
                "describe",
            };

        var operateCommandNames =
            new List<string>
            {
                "run",
                "run-server",
                "deploy",
                "copy-app-state",
                "copy-process",
                "list-functions",
                "apply-function",
                "truncate-process-history",
                "run-file-server",
            };

        Console.WriteLine(rootCommand.Description);
        Console.WriteLine($"\nUsage: {elmFsCommandName} [command] [options]");
        Console.WriteLine("\nThese are common pine commands used in various situations:");

        if (setupGroupCommandNames.Count is not 0)
        {
            Console.WriteLine("\nSet up your development environment:");

            foreach (var name in setupGroupCommandNames)
            {
                Console.WriteLine($"   {name,-30} {GetDisplayDescription(name)}");
            }
        }

        Console.WriteLine("\nDevelop and learn:");

        foreach (var name in developCommandNames)
        {
            Console.WriteLine($"   {name,-30} {GetDisplayDescription(name)}");
        }

        Console.WriteLine("\nRun apps, operate servers and maintain live systems:");

        foreach (var name in operateCommandNames)
        {
            Console.WriteLine($"   {name,-30} {GetDisplayDescription(name)}");
        }

        Console.WriteLine($"\n'{elmFsCommandName} help -a' lists available subcommands.");
        Console.WriteLine($"See '{elmFsCommandName} help <command>' to read about a specific subcommand.");
    }


    private static void ShowAllCommands(RootCommand rootCommand)
    {
        Console.WriteLine(rootCommand.Description);
        Console.WriteLine($"\nUsage: pine [command] [options]");
        Console.WriteLine("\nCommands:");

        foreach (var command in rootCommand.Subcommands.OrderBy(c => c.Name))
        {
            Console.WriteLine($"  {command.Name,-25} {command.Description}");
        }

        Console.WriteLine("\nOptions:");

        foreach (var option in rootCommand.Options)
        {
            var aliases = string.Join(", ", option.Aliases);
            Console.WriteLine($"  {aliases,-25} {option.Description}");
        }
    }
}
