using ElmTime;
using Pine.Core.Files;
using System;
using System.CommandLine;
using System.IO;

namespace Pine.CLI;

public static class CopyProcessCommand
{
    public static Command Create()
    {
        var command =
            new Command("copy-process", "Copy all files needed to restore a process and store them in a zip archive.");

        var siteArgument = new Argument<string>("process-site");

        var sitePasswordOption = new Option<string?>("--site-password");

        command.Add(siteArgument);
        command.Add(sitePasswordOption);

        command.SetAction(
            (parseResult) =>
            {
                var site = parseResult.GetValue(siteArgument);
                var sitePassword = parseResult.GetValue(sitePasswordOption);

                var actualSite = MapSiteForCommandLineArgument(site);
                var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(actualSite);

                actualPassword =
                    PineCliCommand.AttemptHttpRequest(
                        () => new System.Net.Http.HttpRequestMessage { RequestUri = new Uri(actualSite) },
                        defaultPassword: actualPassword,
                        promptForPasswordOnConsole: true).Result.enteredPassword ?? actualPassword;

                Console.WriteLine("Begin reading process history from '" + actualSite + "' ...");

                var (files, lastCompositionLogRecordHashBase16) =
                    RunServer.ReadFilesForRestoreProcessFromAdminInterface(actualSite, actualPassword!);

                Console.WriteLine(
                    "Completed reading files to restore process " + lastCompositionLogRecordHashBase16 + ". Read " +
                    files.Count +
                    " files from '" +
                    actualSite +
                    "'.");

                var zipArchive = ZipArchive.ZipArchiveFromFiles(files);

                var fileName = "process-" + lastCompositionLogRecordHashBase16 + ".zip";
                var filePath = Path.Combine(Environment.CurrentDirectory, fileName);

                File.WriteAllBytes(filePath, zipArchive);

                Console.WriteLine("Saved process archive to file '" + filePath + "'.");

                return 0;
            });

        return command;
    }

    private static string MapSiteForCommandLineArgument(string siteArgument)
    {
        if (PineCliCommand.LooksLikeLocalSite(siteArgument))
            return siteArgument;

        return PineCliCommand.MapUriForForAdminInterface(siteArgument).ToString();
    }
}
