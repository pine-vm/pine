using McMaster.Extensions.CommandLineUtils;
using System.Linq;

namespace elm_fullstack
{
    static public class CommandExtension
    {
        static public CommandLineApplication AddHelpCommandOnCommand(CommandLineApplication command) =>
            command.Command("help", helpCommand => ConfigureHelpCommandForCommand(helpCommand: helpCommand, commandToDescribe: command));

        static public void ConfigureHelpCommandForCommand(CommandLineApplication helpCommand, CommandLineApplication commandToDescribe)
        {
            helpCommand.Description = "Show help for the '" + commandToDescribe.Names.FirstOrDefault() + "' command";

            helpCommand.OnExecute(() => commandToDescribe.ShowHelp());
        }
    }
}
