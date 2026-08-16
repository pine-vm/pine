using ElmTime;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Hosting;
using System;
using System.CommandLine;
using System.Linq;

using static ElmTime.Platform.WebService.Configuration;

namespace Pine.CLI;

public static class RunServerCommand
{
    public static Command Create()
    {
        var command =
            new Command(
                "run-server",
                "Run a server with a web-based admin interface. The HTTP API supports deployments, migrations, and other operations to manage your app.");

        var adminUrlsDefault = "http://*:" + PineCliCommand.AdminInterfaceDefaultPort;

        var processStoreOption = new Option<string?>("--process-store");

        var processStoreReadonlyOption = new Option<string?>("--process-store-readonly");

        var deletePreviousProcessOption = new Option<bool>("--delete-previous-process");

        var adminUrlsOption =
            new Option<string?>("--admin-urls")
            {
                Description =
                "Defaults to '" + adminUrlsDefault + "'."
            };

        var adminPasswordOption = new Option<string?>("--admin-password");

        var publicAppUrlsOption =
            new Option<string?>("--public-urls")
            {
                Description =
                "Defaults to '" + string.Join(",", PublicWebHostUrlsDefault) + "'."
            };

        var copyProcessOption = new Option<string?>("--copy-process");

        var deployOption = new Option<string?>("--deploy");

        command.Add(processStoreOption);
        command.Add(processStoreReadonlyOption);
        command.Add(deletePreviousProcessOption);
        command.Add(adminUrlsOption);
        command.Add(adminPasswordOption);
        command.Add(publicAppUrlsOption);
        command.Add(copyProcessOption);
        command.Add(deployOption);

        command.SetAction(
            (parseResult) =>
            {
                var processStorePath = parseResult.GetValue(processStoreOption);
                var processStoreReadonlyPath = parseResult.GetValue(processStoreReadonlyOption);
                var deletePreviousProcess = parseResult.GetValue(deletePreviousProcessOption);
                var adminUrls = parseResult.GetValue(adminUrlsOption);
                var adminPassword = parseResult.GetValue(adminPasswordOption);
                var publicUrls = parseResult.GetValue(publicAppUrlsOption);
                var copyProcess = parseResult.GetValue(copyProcessOption);
                var deploy = parseResult.GetValue(deployOption);

                var publicAppUrls =
                    publicUrls?.Split(',').Select(url => url.Trim()).ToArray() ??
                    PublicWebHostUrlsDefault;

                var adminInterfaceUrls = adminUrls ?? adminUrlsDefault;

                var webHost =
                    RunServer.BuildWebHostToRunServer(
                        processStorePath: processStorePath,
                        processStoreReadonlyPath: processStoreReadonlyPath,
                        adminInterfaceUrls: adminInterfaceUrls,
                        adminPassword: adminPassword,
                        publicAppUrls: publicAppUrls,
                        deletePreviousProcess: deletePreviousProcess,
                        copyProcess: copyProcess,
                        deployApp: deploy);

                Console.WriteLine("Starting web server with admin interface...");

                webHost.StartAsync().Wait();

                Console.WriteLine(
                    "Completed starting the web server with the admin interface at '" + adminInterfaceUrls + "'.");

                webHost.WaitForShutdownAsync().Wait();

                return 0;
            });

        return command;
    }
}
