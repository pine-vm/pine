using ElmTime;
using Pine.Core;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.Linq;

using StateShim = ElmTime.StateShim;

namespace Pine.CLI;

public static class ListFunctionsCommand
{
    public static Command Create()
    {
        var command =
            new Command("list-functions", "List the functions exposed by an Elm app for application on a database.");

        var siteArgument = new Argument<string>("process-site");

        var sitePasswordOption = new Option<string?>("--site-password");

        command.Add(siteArgument);
        command.Add(sitePasswordOption);

        command.SetAction(
            (parseResult) =>
            {
                var site = parseResult.GetValue(siteArgument);
                var sitePassword = parseResult.GetValue(sitePasswordOption);

                var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(site);

                var listFunctionsResult =
                    ListFunctions(
                        site: site,
                        siteDefaultPassword: actualPassword,
                        promptForPasswordOnConsole: true);

                var console = (IConsole)StaticConsole.Instance;

                return
                    listFunctionsResult
                    // For now, only show functions with a normal module prefix
                    .Map(functions => functions.Where(f => f.functionName.Contains('.')).ToImmutableList())
                    .Unpack(
                        fromErr:
                        err =>
                        {
                            console.WriteLine(
                                "Failed to list functions at " + site + ": " + err,
                                IConsole.TextColor.Red);

                            return 2;
                        },
                        fromOk:
                        functions =>
                        {
                            static string DescribeFunction(
                                StateShim.InterfaceToHost.NamedExposedFunction databaseFunction)
                            {
                                var commentOnReturnType =
                                    "-- (return type " +
                                    (databaseFunction.functionDescription.returnType.containsAppStateType
                                    ?
                                    "contains app state type"
                                    :
                                    "does not contain app state type")
                                    + ")";

                                return
                                    "Function " + databaseFunction.functionName +
                                    " has " + databaseFunction.functionDescription.parameters.Count + " parameters:\n" +
                                    databaseFunction.functionName.Split('.').LastOrDefault(
                                        databaseFunction.functionName) + " :\n" +
                                    string.Join(
                                        "\n",
                                        string.Join(
                                            "",
                                            databaseFunction.functionDescription
                                            .parameters.Select(p => p.typeSourceCodeText)
                                            .Concat(
                                                [
                                                databaseFunction.functionDescription.returnType.sourceCodeText +
                                                " " + commentOnReturnType
                                                ])
                                            .Intersperse("\n-> "))
                                        .Split("\n")
                                        .Select(line => "    " + line));
                            }

                            console.WriteLine(
                                "Site " + site + " exposes " + functions.Count + " database functions:\n----------\n" +
                                string.Join("\n\n", functions.Select(DescribeFunction)) +
                                "\n----------\n");

                            return 0;
                        });
            });

        return command;
    }

    public static Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>> ListFunctions(
        string site,
        string? siteDefaultPassword,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(System.DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        if (PineCliCommand.LooksLikeLocalSite(site))
        {
            return
                Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>>.err(
                    "Not implemented for local site");
        }

        try
        {
            var httpRequestUri =
                site.TrimEnd('/') + ElmTime.Platform.WebService.StartupAdminInterface.PathApiListDatabaseFunctions;

            var httpResponse =
                PineCliCommand.AttemptHttpRequest(
                    () =>
                    {
                        return
                            new System.Net.Http.HttpRequestMessage
                            {
                                Method = System.Net.Http.HttpMethod.Get,
                                RequestUri = PineCliCommand.MapUriForForAdminInterface(httpRequestUri),
                            };
                    },
                    defaultPassword: siteDefaultPassword,
                    promptForPasswordOnConsole: promptForPasswordOnConsole).Result.httpResponse;

            var responseContentString = httpResponse.Content.ReadAsStringAsync().Result;

            if (!httpResponse.IsSuccessStatusCode)
            {
                return
                    "HTTP response status code not OK: " + httpResponse.StatusCode + ", content:\n" +
                    responseContentString;
            }

            return
                System.Text.Json.JsonSerializer.Deserialize<Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>>>(responseContentString)!
                .MapError(err => "Server returned error: " + err);
        }
        catch (System.Exception e)
        {
            return "Failed with runtime exception:\n" + e;
        }
    }
}
