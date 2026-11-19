using System.CommandLine;

namespace Pine.CLI;

public static class ElmCommand
{
    public static Command Create()
    {
        var command =
            new Command("elm", "Elm development tools.")
            {
                InteractiveCommand.Create(),
                MakeCommand.Create(),
                Elm.CompileCommand.Create(),
                Elm.FormatCommand.Create(),
                Elm.TestCommand.Create(),
                Elm.MaintainCommand.Create()
            };

        return command;
    }
}
