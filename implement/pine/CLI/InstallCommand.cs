using System.CommandLine;

namespace Pine.CLI;

public static class InstallCommand
{
    public static Command Create()
    {
        var (commandName, checkInstallation) = PineCliCommand.CheckIfExecutableIsRegisteredOnPath();

        var command =
            new Command("install", "Install the '" + commandName + "' command for the current user account.");

        command.SetAction(
            (parseResult) =>
            {
                checkInstallation().registerExecutableDirectoryOnPath();
            });

        return command;
    }
}
