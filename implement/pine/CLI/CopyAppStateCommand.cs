using ElmTime;
using Pine.Core;
using Pine.Core.Addressing;
using System;
using System.CommandLine;
using System.Diagnostics;
using System.IO;
using System.Text.Json.Serialization;

using ElmTimePlatform = ElmTime.Platform;

namespace Pine.CLI;

public static class CopyAppStateCommand
{
    public static Command Create()
    {
        var command = new Command("copy-app-state", "Copy the state of an Elm backend app.");

        var sourceArgument = PineCliCommand.RequiredArgument("source");

        var destinationArgument =
            new Argument<string?>("destination")
            {
                Arity = ArgumentArity.ZeroOrOne
            };

        var sourcePasswordOption = new Option<string?>("--source-password");

        var destinationPasswordOption = new Option<string?>("--destination-password");

        command.Add(sourceArgument);
        command.Add(destinationArgument);
        command.Add(sourcePasswordOption);
        command.Add(destinationPasswordOption);

        command.SetAction(
            (parseResult) =>
            {
                var source = parseResult.GetRequiredValue(sourceArgument);
                var destination = parseResult.GetValue(destinationArgument);
                var sourcePassword = parseResult.GetValue(sourcePasswordOption);
                var destinationPassword = parseResult.GetValue(destinationPasswordOption);

                var actualSourcePassword = sourcePassword ?? UserSecrets.LoadPasswordForSite(source);

                var actualDestinationPassword =
                    destination != null ? (destinationPassword ?? UserSecrets.LoadPasswordForSite(destination)) : null;

                var totalStopwatch = Stopwatch.StartNew();

                var report =
                    CopyElmAppState(
                        source: source,
                        sourceDefaultPassword: actualSourcePassword,
                        destination: destination,
                        destinationDefaultPassword: actualDestinationPassword)
                    with
                    {
                        totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds
                    };

                PineCliCommand.WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        report,
                        PineCliCommand.ReportJsonSerializerOptions),
                    reportKind: "copy-app-state.json");

                return 0;
            });

        return command;
    }

    private record CopyElmAppStateReport(
        string beginTime,
        string source,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        string? destination,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        AppStateSummary? appStateSummary = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        PineCliCommand.ResponseFromServerReport? destinationResponseFromServer = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        string? destinationFileReport = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        int? totalTimeSpentMilli = null,

        [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        object? error = null);

    public record AppStateSummary(string hash, int length);

    private static CopyElmAppStateReport CopyElmAppState(
        string source,
        string? sourceDefaultPassword,
        string? destination,
        string? destinationDefaultPassword)
    {
        var report =
            new CopyElmAppStateReport(
                source: source,
                destination: destination,
                beginTime: BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow));

        CopyElmAppStateReport ReturnWithErrorMessage(string error)
        {
            Console.WriteLine("Error: " + error);
            return report with { error = error };
        }

        byte[] appStateSerial;

        if (PineCliCommand.LooksLikeLocalSite(source))
        {
            if (File.Exists(source))
            {
                appStateSerial = File.ReadAllBytes(source);
            }
            else
            {
                return ReturnWithErrorMessage("Source looks like a local site, but I did not find a file at " + source);
            }
        }
        else
        {
            appStateSerial =
                GetElmAppStateViaAdminInterface(source, sourceDefaultPassword, promptForPasswordOnConsole: true);
        }

        if (appStateSerial == null)
        {
            return ReturnWithErrorMessage("Failed to read from source.");
        }

        var appStateComponent = PineValue.Blob(appStateSerial);
        var appStateId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(appStateComponent).Span);

        report = report with { appStateSummary = new AppStateSummary(hash: appStateId, length: appStateSerial.Length) };

        Console.WriteLine(
            "Got app state " + appStateId + " from the source. It is " + appStateSerial.Length + " bytes long.");

        string SaveToFile(string filePath)
        {
            Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

            File.WriteAllBytes(filePath, appStateSerial);

            var message = "Saved to file '" + filePath + "'";

            Console.WriteLine(message);

            return message;
        }

        if (destination is null)
        {
            return ReturnWithErrorMessage("I got no argument for the destination. To copy the app state to a file or a live process, Run the copy command with an argument for the destination.");
        }

        if (PineCliCommand.LooksLikeLocalSite(destination))
        {
            var filePath =
                Directory.Exists(destination)
                ?
                Path.Combine(destination, appStateId + "app-state.json")
                :
                destination;

            return report with { destinationFileReport = SaveToFile(filePath) };
        }

        return
            report with
            {
                destinationResponseFromServer =
                SetElmAppStateViaAdminInterface(
                    site: destination,
                    siteDefaultPassword: destinationDefaultPassword,
                    elmAppStateSerialized: appStateSerial,
                    promptForPasswordOnConsole: true)
            };

    }

    private static PineCliCommand.ResponseFromServerReport SetElmAppStateViaAdminInterface(
        string site,
        string? siteDefaultPassword,
        byte[] elmAppStateSerialized,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = Stopwatch.StartNew();

        var elmAppStateComponent = PineValue.Blob(elmAppStateSerialized);

        var elmAppStateId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(elmAppStateComponent).Span);

        var httpResponse =
            PineCliCommand.AttemptHttpRequest(
                () =>
                {
                    var httpContent = new System.Net.Http.ByteArrayContent(elmAppStateSerialized);

                    httpContent.Headers.ContentType =
                        new System.Net.Http.Headers.MediaTypeHeaderValue("application/json");

                    return
                        new System.Net.Http.HttpRequestMessage
                        {
                            Method = System.Net.Http.HttpMethod.Post,
                            RequestUri =
                            PineCliCommand.MapUriForForAdminInterface(
                                site.TrimEnd('/') + ElmTimePlatform.WebService.StartupAdminInterface.PathApiElmAppState),
                            Content = httpContent,
                        };
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
                System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonObject>((string)responseBodyReport)!;
        }
        catch { }

        return
            new PineCliCommand.ResponseFromServerReport(
                statusCode: (int)httpResponse.StatusCode,
                body: responseBodyReport);
    }

    private static byte[] GetElmAppStateViaAdminInterface(
        string site,
        string? siteDefaultPassword,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var httpResponse =
            PineCliCommand.AttemptHttpRequest(
                () =>
                {
                    return
                        new System.Net.Http.HttpRequestMessage
                        {
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri =
                            PineCliCommand.MapUriForForAdminInterface(
                                site.TrimEnd('/') + ElmTimePlatform.WebService.StartupAdminInterface.PathApiElmAppState),
                        };
                },
                defaultPassword: siteDefaultPassword,
                promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

        Console.WriteLine("Server response status code: " + httpResponse.StatusCode);

        var elmAppStateSerialized = httpResponse.Content.ReadAsByteArrayAsync().Result;

        var elmAppStateComponent = PineValue.Blob(elmAppStateSerialized);
        var elmAppStateId = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(elmAppStateComponent).Span);

        return elmAppStateSerialized;
    }
}
