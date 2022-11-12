using McMaster.Extensions.CommandLineUtils;
using System.Linq;

namespace ElmFullstack;

static public class CommandExtension
{
    static public void ConfigureHelpCommandForCommand(CommandLineApplication helpCommand, CommandLineApplication commandToDescribe)
    {
        helpCommand.Description = "Show help for the '" + commandToDescribe.Names.FirstOrDefault() + "' command";

        helpCommand.OnExecute(commandToDescribe.ShowHelp);
    }
}
