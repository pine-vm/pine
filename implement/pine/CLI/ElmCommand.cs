using System.CommandLine;

namespace Pine.CLI;

public static class ElmCommand
{
    public static Command Create()
    {
        var command =
            new Command("elm", "Elm development tools.")
            {
                Elm.FormatCommand.Create(),
                Elm.TestCommand.Create()
            };

        return command;
    }
}
