using ElmTime;
using Pine.Core;
using System;
using System.CommandLine;

namespace Pine.CLI;

public static class TruncateProcessHistoryCommand
{
    public static Command Create()
    {
        var command =
            new Command(
                "truncate-process-history",
                "Remove parts of the process history that are not needed to restore the process.");

        var siteArgument = PineCliCommand.RequiredArgument("process-site");

        var sitePasswordOption = new Option<string?>("--site-password");

        command.Add(siteArgument);
        command.Add(sitePasswordOption);

        command.SetAction(
            (parseResult) =>
            {
                var site = parseResult.GetRequiredValue(siteArgument);
                var sitePassword = parseResult.GetValue(sitePasswordOption);

                var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(site);

                var report =
                    TruncateProcessHistory(
                        site: site,
                        siteDefaultPassword: actualPassword,
                        promptForPasswordOnConsole: true);

                PineCliCommand.WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        report,
                        PineCliCommand.ReportJsonSerializerOptions),
                    reportKind: "truncate-process-history.json");

                return 0;
            });

        return command;
    }

    private record TruncateProcessHistoryReport(
        string beginTime,
        string site,
        PineCliCommand.ResponseFromServerReport responseFromServer,
        int totalTimeSpentMilli);

    private static TruncateProcessHistoryReport TruncateProcessHistory(
        string site,
        string? siteDefaultPassword,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var requestUrl =
            site.TrimEnd('/') + ElmTime.Platform.WebService.StartupAdminInterface.PathApiTruncateProcessHistory;

        Console.WriteLine("Beginning to truncate process history at '" + site + "'...");

        var httpResponse =
            PineCliCommand.AttemptHttpRequest(
                () =>
                new System.Net.Http.HttpRequestMessage
                {
                    Method = System.Net.Http.HttpMethod.Post,
                    RequestUri = PineCliCommand.MapUriForForAdminInterface(requestUrl),
                },
                defaultPassword: siteDefaultPassword,
                promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

        var responseContentString = httpResponse.Content.ReadAsStringAsync().Result;

        Console.WriteLine(
            "Server response: " + httpResponse.StatusCode + "\n" +
            responseContentString);

        object responseBodyReport = responseContentString;

        try
        {
            responseBodyReport =
                System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonObject>(responseContentString)!;
        }
        catch { }

        var responseFromServer =
            new PineCliCommand.ResponseFromServerReport(
                statusCode: (int)httpResponse.StatusCode,
                body: responseBodyReport);

        return
            new TruncateProcessHistoryReport(
                beginTime: beginTime,
                site: site,
                responseFromServer: responseFromServer,
                totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds);
    }
}
