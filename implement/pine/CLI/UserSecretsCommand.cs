using ElmTime;
using System;
using System.CommandLine;

namespace Pine.CLI;

public static class UserSecretsCommand
{
    public static Command Create()
    {
        var command = new Command("user-secrets", "Manage passwords for accessing the admin interfaces of servers.");

        var siteArgument = new Argument<string>("site");

        var passwordArgument = new Argument<string>("password");

        var storeCommand =
            new Command("store", "Store a password for a site")
            {
                siteArgument,
                passwordArgument
            };

        storeCommand.SetAction(
            (parseResult) =>
            {
                var site = parseResult.GetValue(siteArgument);
                var password = parseResult.GetValue(passwordArgument);

                UserSecrets.StorePasswordForSite(site, password);

                return 0;
            });

        command.Add(storeCommand);

        command.SetAction(
            (parseResult) =>
            {
                Console.WriteLine("Please specify a subcommand.");
                Console.WriteLine("Available subcommands:");
                Console.WriteLine("  store - Store a password for a site");
            });

        return command;
    }
}
