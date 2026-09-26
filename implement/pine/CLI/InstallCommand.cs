using System;
using System.CommandLine;
using System.IO;

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
                try
                {
                    checkInstallation().registerExecutableDirectoryOnPath();
                    return 0;
                }
                catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or InvalidOperationException)
                {
                    Console.Error.WriteLine("Installation failed: " + exception.Message);
                    return 1;
                }
            });

        return command;
    }
}
