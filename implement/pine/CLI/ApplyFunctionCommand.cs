using ElmTime;
using Pine.Core;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.IO;
using System.Linq;

using AdminInterface = ElmTime.AdminInterface;

namespace Pine.CLI;

public static class ApplyFunctionCommand
{
    public static Command Create()
    {
        var command =
            new Command("apply-function", "Apply an Elm function on a database containing the state of an Elm app.");

        var siteArgument = PineCliCommand.RequiredArgument("process-site");

        var functionNameArgument = PineCliCommand.RequiredArgument("function-name");

        var sitePasswordOption = new Option<string?>("--site-password");

        var argumentOption =
            new Option<string[]>("--argument")
            {
                AllowMultipleArgumentsPerToken = true,
                Arity = ArgumentArity.ZeroOrMore
            };

        var commitResultingStateOption = new Option<bool>("--commit-resulting-state");

        command.Add(siteArgument);
        command.Add(functionNameArgument);
        command.Add(sitePasswordOption);
        command.Add(argumentOption);
        command.Add(commitResultingStateOption);

        command.SetAction(
            (parseResult) =>
            {
                var site = parseResult.GetRequiredValue(siteArgument);
                var functionName = parseResult.GetRequiredValue(functionNameArgument);
                var sitePassword = parseResult.GetValue(sitePasswordOption);
                var arguments = parseResult.GetValue(argumentOption) ?? [];
                var commitResultingState = parseResult.GetValue(commitResultingStateOption);

                var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(site);

                var serializedArgumentsJson =
                    arguments.Select(LoadArgumentFromUserInterfaceAsJsonOrFileTextContext).ToImmutableList();

                var applyFunctionReport =
                    ApplyFunction(
                        site: site,
                        functionName: functionName,
                        serializedArgumentsJson: serializedArgumentsJson,
                        commitResultingState: commitResultingState,
                        siteDefaultPassword: actualPassword,
                        promptForPasswordOnConsole: true);

                PineCliCommand.WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        applyFunctionReport,
                        PineCliCommand.ReportJsonSerializerOptions),
                    reportKind: "apply-function.json");

                return 0;
            });

        return command;
    }

    private static string LoadArgumentFromUserInterfaceAsJsonOrFileTextContext(string argumentFromCLI)
    {
        try
        {
            var asJson = System.Text.Json.JsonSerializer.Deserialize<object>(argumentFromCLI);

            return argumentFromCLI;
        }
        catch { }

        return File.ReadAllText(argumentFromCLI);
    }

    public record ApplyFunctionReport(
        string site,
        AdminInterface.ApplyDatabaseFunctionRequest applyFunctionRequest,
        string beginTime,
        PineCliCommand.ResponseFromServerReport? responseFromServer,
        string? runtimeException,
        int totalTimeSpentMilli);

    public static ApplyFunctionReport ApplyFunction(
        string site,
        string functionName,
        IReadOnlyList<string> serializedArgumentsJson,
        bool commitResultingState,
        string? siteDefaultPassword,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        PineCliCommand.ResponseFromServerReport? responseFromServer = null;

        Exception? runtimeException = null;

        var applyFunctionRequest =
            new AdminInterface.ApplyDatabaseFunctionRequest(
                functionName: functionName,
                serializedArgumentsJson: serializedArgumentsJson,
                commitResultingState: commitResultingState);

        try
        {
            if (PineCliCommand.LooksLikeLocalSite(site))
            {
                throw new NotImplementedException("Not implemented for local site");
            }

            var applyAddress =
                site.TrimEnd('/') + ElmTime.Platform.WebService.StartupAdminInterface.PathApiApplyDatabaseFunction;

            Console.WriteLine("Attempting to apply function '" + functionName + "' at '" + applyAddress + "'...");

            var httpResponse =
                PineCliCommand.AttemptHttpRequest(
                    () =>
                    {
                        var httpContent = System.Net.Http.Json.JsonContent.Create(applyFunctionRequest);

                        return
                            new System.Net.Http.HttpRequestMessage
                            {
                                Method = System.Net.Http.HttpMethod.Post,
                                RequestUri = PineCliCommand.MapUriForForAdminInterface(applyAddress),
                                Content = httpContent,
                            };
                    },
                    defaultPassword: siteDefaultPassword,
                    promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

            var responseContentString = httpResponse.Content.ReadAsStringAsync().Result;

            Console.WriteLine(
                "Server response: " + httpResponse.StatusCode + "\n" + responseContentString);

            object responseBodyReport = responseContentString;

            try
            {
                responseBodyReport =
                    System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonObject>(responseContentString)!;
            }
            catch { }

            responseFromServer =
                new PineCliCommand.ResponseFromServerReport(
                    statusCode: (int)httpResponse.StatusCode,
                    body: responseBodyReport);
        }
        catch (Exception e)
        {
            Console.WriteLine("Failed with exception: " + e.Message);

            runtimeException = e;
        }

        return
            new ApplyFunctionReport(
                site: site,
                applyFunctionRequest: applyFunctionRequest,
                beginTime: beginTime,
                responseFromServer: responseFromServer,
                runtimeException: runtimeException?.ToString(),
                totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds);
    }
}
