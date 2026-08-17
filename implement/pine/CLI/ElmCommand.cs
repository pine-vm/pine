using System.CommandLine;

namespace Pine.CLI;

public static class ElmCommand
{
    public static Command Create()
    {
        var command = new Command("elm", "Elm development tools.");

        command.Add(Elm.TestCommand.Create());

        return command;
    }
}
