using ElmTime;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.CommandLine;

using ElmTimePlatform = ElmTime.Platform;

namespace Pine.CLI;

public static class DeployCommand
{
    public static Command Create()
    {
        var command =
            new Command(
                "deploy",
                "Deploy an app to an Elm backend process. Deployment implies migration from the previous app state if not specified otherwise.");

        var sourceArgument = PineCliCommand.RequiredArgument("source");

        var siteArgument = PineCliCommand.RequiredArgument("process-site");

        var sitePasswordOption = new Option<string?>("--site-password");

        var initAppStateOption = new Option<bool>("--init-app-state");

        command.Add(sourceArgument);
        command.Add(siteArgument);
        command.Add(sitePasswordOption);
        command.Add(initAppStateOption);

        command.SetAction(
            (parseResult) =>
            {
                var source = parseResult.GetRequiredValue(sourceArgument);
                var site = parseResult.GetRequiredValue(siteArgument);
                var sitePassword = parseResult.GetValue(sitePasswordOption);
                var initAppState = parseResult.GetValue(initAppStateOption);

                var actualPassword = sitePassword ?? UserSecrets.LoadPasswordForSite(site);

                var deployReport =
                    DeployApp(
                        sourcePath: source,
                        site: site,
                        siteDefaultPassword: actualPassword,
                        initElmAppState: initAppState,
                        promptForPasswordOnConsole: true);

                PineCliCommand.WriteReportToFileInReportDirectory(
                    reportContent: System.Text.Json.JsonSerializer.Serialize(
                        deployReport,
                        PineCliCommand.ReportJsonSerializerOptions),
                    reportKind: "deploy.json");

                return 0;
            });

        return command;
    }

    public record DeployAppReport(
        bool initElmAppState,
        string site,
        string beginTime,
        string sourcePath,
        string sourceCompositionId,
        CompileCommand.SourceSummaryStructure sourceSummary,
        string filteredSourceCompositionId,
        PineCliCommand.ResponseFromServerReport? responseFromServer,
        string? deployException,
        int totalTimeSpentMilli);

    public static DeployAppReport DeployApp(
        string sourcePath,
        string site,
        string? siteDefaultPassword,
        bool initElmAppState,
        bool promptForPasswordOnConsole)
    {
        var beginTime = BytesConversions.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        Console.WriteLine("Beginning to build configuration...");

        var buildResult =
            ElmTimePlatform.WebService.BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                sourcePath: sourcePath);

        var (sourceCompositionId, sourceSummary) = CompileCommand.CompileSourceSummary(buildResult.sourceTree);

        var appConfigZipArchive = buildResult.configZipArchive;

        var compiledCompositionId =
            Convert.ToHexStringLower(
                PineValueHashTree.ComputeHashSorted(
                    FileTree.FromSetOfFilesWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(appConfigZipArchive))).Span);

        Console.WriteLine("Built app config " + compiledCompositionId + " from " + sourceCompositionId + ".");

        PineCliCommand.ResponseFromServerReport? responseFromServer = null;

        Exception? deployException = null;

        try
        {
            if (!PineCliCommand.LooksLikeLocalSite(site))
            {
                var deployAddress =
                    site.TrimEnd('/') +
                    (initElmAppState
                    ?
                    ElmTimePlatform.WebService.StartupAdminInterface.PathApiDeployAndInitAppState
                    :
                    ElmTimePlatform.WebService.StartupAdminInterface.PathApiDeployAndMigrateAppState);

                Console.WriteLine(
                    "Attempting to deploy app '" + compiledCompositionId + "' to '" + deployAddress + "'...");

                var httpResponse =
                    PineCliCommand.AttemptHttpRequest(
                        () =>
                        {
                            var httpContent = new System.Net.Http.ByteArrayContent(appConfigZipArchive);

                            httpContent.Headers.ContentType =
                                new System.Net.Http.Headers.MediaTypeHeaderValue("application/zip");

                            httpContent.Headers.ContentDisposition =
                                new System.Net.Http.Headers.ContentDispositionHeaderValue("attachment") { FileName = compiledCompositionId + ".zip" };

                            return
                                new System.Net.Http.HttpRequestMessage
                                {
                                    Method = System.Net.Http.HttpMethod.Post,
                                    RequestUri = PineCliCommand.MapUriForForAdminInterface(deployAddress),
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
            else
            {
                var processStoreFileStore = new FileStoreFromSystemIOFile(site);

                var processStoreWriter =
                    new ElmTimePlatform.WebService.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                        processStoreFileStore,
                        getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                        processStoreFileStore,
                        skipWritingComponentSecondTime: true);

                var appConfigTree =
                    FileTree.FromSetOfFilesWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

                var appConfigComponent = FileTreeEncoding.Encode(appConfigTree);

                processStoreWriter.StoreComponent(appConfigComponent);

                var appConfigValueInFile =
                    new ElmTimePlatform.WebService.ProcessStoreSupportingMigrations.ValueInFileStructure
                    {
                        HashBase16 = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(appConfigComponent).Span)
                    };

                var compositionLogEvent =
                    ElmTimePlatform.WebService
                    .ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent
                    .EventForDeployAppConfig(
                        appConfigValueInFile: appConfigValueInFile,
                        initElmAppState: initElmAppState);

                var (statusCode, responseReport) =
                    ElmTimePlatform.WebService.StartupAdminInterface.AttemptContinueWithCompositionEventAndCommit(
                        compositionLogEvent,
                        processStoreFileStore);

                responseFromServer =
                    new PineCliCommand.ResponseFromServerReport(
                        statusCode: statusCode,
                        body: responseReport);
            }
        }
        catch (Exception e)
        {
            Console.WriteLine("Failed with exception: " + e.Message);

            deployException = e;
        }

        return
            new DeployAppReport(
                initElmAppState: initElmAppState,
                site: site,
                beginTime: beginTime,
                sourcePath: sourcePath,
                sourceCompositionId: sourceCompositionId,
                sourceSummary: sourceSummary,
                filteredSourceCompositionId: compiledCompositionId,
                responseFromServer: responseFromServer,
                deployException: deployException?.ToString(),
                totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds);
    }
}
