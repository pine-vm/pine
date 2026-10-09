using Pine.PineVM;
using System.CommandLine;

namespace Pine.CLI;

public static class ElmCommand
{
    public static Command Create(DynamicPGOShare? dynamicPGOShare = null)
    {
        var command =
            new Command("elm", "Elm development tools.")
            {
                InteractiveCommand.Create(dynamicPGOShare),
                MakeCommand.Create(),
                Elm.FormatCommand.Create(),
                Elm.TestCommand.Create()
            };

        return command;
    }
}
