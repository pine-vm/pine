using McMaster.Extensions.CommandLineUtils;
using System.Linq;

namespace ElmTime;

public static class CommandExtension
{
    public static void ConfigureHelpCommandForCommand(CommandLineApplication helpCommand, CommandLineApplication commandToDescribe)
    {
        helpCommand.Description = "Show help for the '" + commandToDescribe.Names.FirstOrDefault() + "' command";

        helpCommand.OnExecute(commandToDescribe.ShowHelp);
    }
}
