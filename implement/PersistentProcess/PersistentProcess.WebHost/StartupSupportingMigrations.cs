using Kalmit.ProcessStore;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;
using System.Text.RegularExpressions;

namespace Kalmit.PersistentProcess.WebHost
{
    public class StartupSupportingMigrations
    {
        static public string PathApiSetAppConfigAndInitState => "/api/set-app-config-and-init-state";

        static public string PathApiSetAppConfigAndContinueState => "/api/set-app-config-and-continue-state";

        static public string PathApiMigrateElmState => "/api/migrate-elm-state";

        static public string PathApiGetAppConfig => "/api/get-app-config";

        static public string MigrationElmAppInterfaceModuleName => "MigrateBackendState";

        static string MigrationElmAppCompilationRootModuleName => MigrationElmAppInterfaceModuleName + "Root";

        static string MigrateElmFunctionNameInModule => "migrate";

        public StartupSupportingMigrations()
        {
        }

        public void ConfigureServices(IServiceCollection services)
        {
            var serviceProvider = services.BuildServiceProvider();

            var getDateTimeOffset = serviceProvider.GetService<Func<DateTimeOffset>>();

            if (getDateTimeOffset == null)
            {
                getDateTimeOffset = () => DateTimeOffset.UtcNow;
                services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset);
            }
        }

        class PublicHostConfiguration
        {
            public SyncPersistentProcess syncPersistentProcess;

            public IWebHost webHost;
        }

        public void Configure(
            IApplicationBuilder app,
            IWebHostEnvironment env,
            IHostApplicationLifetime appLifetime,
            Func<DateTimeOffset> getDateTimeOffset)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            var configuration = app.ApplicationServices.GetService<IConfiguration>();

            var rootPassword = configuration.GetValue<string>(Configuration.AdminRootPasswordSettingKey);
            var publicWebHostUrls = configuration.GetValue<string>(Configuration.PublicWebHostUrlsSettingKey)?.Split(new[] { ',', ';' });

            var processStoreFileStore = app.ApplicationServices.GetService<FileStoreForProcessStore>().fileStore;

            var pathToPublicAppConfigFile = ImmutableList.Create("public-web-app-config.zip");

            var elmAppProcessStore = new FileStoreFromSubdirectory(processStoreFileStore, "elm-app-process-store");

            object publicAppLock = new object();

            PublicHostConfiguration publicAppHost = null;

            void stopPublicApp()
            {
                lock (publicAppLock)
                {
                    if (publicAppHost != null)
                    {
                        publicAppHost?.webHost.StopAsync(TimeSpan.FromSeconds(10)).Wait();
                        publicAppHost?.webHost.Dispose();
                        publicAppHost = null;
                    }
                }
            }

            appLifetime.ApplicationStopping.Register(() =>
            {
                stopPublicApp();
            });

            void setAndStartPublicApp(byte[] webAppConfigZipArchive, bool elmAppInitState)
            {
                lock (publicAppLock)
                {
                    processStoreFileStore.SetFileContent(pathToPublicAppConfigFile, webAppConfigZipArchive);

                    startPublicApp(webAppConfigZipArchive, elmAppInitState);
                }
            }

            void startPublicApp(byte[] webAppConfigZipArchive, bool elmAppInitState)
            {
                lock (publicAppLock)
                {
                    stopPublicApp();

                    if (elmAppInitState)
                    {
                        foreach (var filePath in elmAppProcessStore.ListFilesInDirectory(ImmutableList<string>.Empty).ToImmutableList())
                            elmAppProcessStore.DeleteFile(filePath);
                    }

                    var newPublicAppConfig = new PublicHostConfiguration { };

                    var webHost =
                        Program.CreateWebHostBuilder(null, overrideDefaultUrls: publicWebHostUrls)
                        .WithSettingAdminRootPassword(rootPassword)
                        .WithSettingDateTimeOffsetDelegate(getDateTimeOffset)
                        .WithWebAppConfigurationZipArchive(webAppConfigZipArchive)
                        .WithProcessStoreFileStore(elmAppProcessStore)
                        .ConfigureServices(services => services.AddSingleton(new PersistentProcessMap
                        {
                            mapPersistentProcess = originalPersistentProcess =>
                            {
                                return newPublicAppConfig.syncPersistentProcess = new SyncPersistentProcess(originalPersistentProcess);
                            }
                        }))
                        .Build();

                    newPublicAppConfig.webHost = webHost;

                    webHost.StartAsync(appLifetime.ApplicationStopping).Wait();
                    publicAppHost = newPublicAppConfig;
                }
            }

            byte[] getPublicAppConfigFileFromStore() =>
                processStoreFileStore.GetFileContent(pathToPublicAppConfigFile);

            {
                var publicAppConfigFile = getPublicAppConfigFileFromStore();

                if (publicAppConfigFile != null)
                    startPublicApp(publicAppConfigFile, elmAppInitState: false);
            }

            app.Run(async (context) =>
                {
                    var syncIOFeature = context.Features.Get<Microsoft.AspNetCore.Http.Features.IHttpBodyControlFeature>();
                    if (syncIOFeature != null)
                    {
                        syncIOFeature.AllowSynchronousIO = true;
                    }

                    {
                        var expectedAuthorization = Configuration.BasicAuthenticationForAdminRoot(rootPassword);

                        context.Request.Headers.TryGetValue("Authorization", out var requestAuthorizationHeaderValue);

                        AuthenticationHeaderValue.TryParse(
                            requestAuthorizationHeaderValue.FirstOrDefault(), out var requestAuthorization);

                        if (!(0 < rootPassword?.Length))
                        {
                            context.Response.StatusCode = 403;
                            await context.Response.WriteAsync("Forbidden");
                            return;
                        }

                        var buffer = new byte[400];

                        var decodedRequestAuthorizationParameter =
                            Convert.TryFromBase64String(requestAuthorization?.Parameter ?? "", buffer, out var bytesWritten) ?
                            Encoding.UTF8.GetString(buffer, 0, bytesWritten) : null;

                        if (!(string.Equals(expectedAuthorization, decodedRequestAuthorizationParameter) &&
                            string.Equals("basic", requestAuthorization?.Scheme, StringComparison.OrdinalIgnoreCase)))
                        {
                            context.Response.StatusCode = 401;
                            context.Response.Headers.Add(
                                "WWW-Authenticate",
                                @"Basic realm=""" + context.Request.Host + Configuration.AdminPath + @""", charset=""UTF-8""");
                            await context.Response.WriteAsync("Unauthorized");
                            return;
                        }
                    }

                    var requestPathIsSetAppAndInitState =
                        context.Request.Path.StartsWithSegments(new PathString(PathApiSetAppConfigAndInitState));

                    var requestPathIsSetAppAndContinueState =
                        context.Request.Path.StartsWithSegments(new PathString(PathApiSetAppConfigAndContinueState));

                    if (context.Request.Path.StartsWithSegments(new PathString(PathApiGetAppConfig)))
                    {
                        if (!string.Equals(context.Request.Method, "get", StringComparison.InvariantCultureIgnoreCase))
                        {
                            context.Response.StatusCode = 405;
                            await context.Response.WriteAsync("Method not supported.");
                            return;
                        }

                        var publicAppConfigFile = getPublicAppConfigFileFromStore();

                        if (publicAppConfigFile == null)
                        {
                            context.Response.StatusCode = 200;
                            await context.Response.WriteAsync("I did not find an app config file in the store.");
                            return;
                        }

                        context.Response.StatusCode = 200;
                        context.Response.Headers.ContentLength = publicAppConfigFile.LongLength;
                        context.Response.Headers.Add("Content-Disposition", new ContentDispositionHeaderValue("attachment") { FileName = pathToPublicAppConfigFile.Last() }.ToString());
                        context.Response.Headers.Add("Content-Type", new MediaTypeHeaderValue("application/zip").ToString());

                        await context.Response.Body.WriteAsync(publicAppConfigFile);
                        return;
                    }

                    if (requestPathIsSetAppAndInitState || requestPathIsSetAppAndContinueState)
                    {
                        if (!string.Equals(context.Request.Method, "post", StringComparison.InvariantCultureIgnoreCase))
                        {
                            context.Response.StatusCode = 405;
                            await context.Response.WriteAsync("Method not supported.");
                            return;
                        }

                        var memoryStream = new MemoryStream();
                        context.Request.Body.CopyTo(memoryStream);

                        var webAppConfigZipArchive = memoryStream.ToArray();

                        {
                            try
                            {
                                var filesFromZipArchive = ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive).ToImmutableList();

                                if (filesFromZipArchive.Count < 1)
                                    throw new Exception("Contains no files.");
                            }
                            catch (Exception e)
                            {
                                context.Response.StatusCode = 400;
                                await context.Response.WriteAsync("Malformed web app config zip-archive:\n" + e);
                                return;
                            }
                        }

                        setAndStartPublicApp(
                            webAppConfigZipArchive,
                            elmAppInitState: requestPathIsSetAppAndInitState);

                        context.Response.StatusCode = 200;
                        await context.Response.WriteAsync("Successfully set the app and started the web server.");
                        return;
                    }

                    if (context.Request.Path.StartsWithSegments(new PathString(PathApiMigrateElmState)))
                    {
                        if (!string.Equals(context.Request.Method, "post", StringComparison.InvariantCultureIgnoreCase))
                        {
                            context.Response.StatusCode = 405;
                            await context.Response.WriteAsync("Method not supported.");
                            return;
                        }

                        var memoryStream = new MemoryStream();
                        context.Request.Body.CopyTo(memoryStream);

                        var migrateElmAppConfigZipArchive = memoryStream.ToArray();

                        var prepareMigrateResult = PrepareMigrateSerializedValueWithoutChangingElmType(
                            migrateElmAppConfigZipArchive,
                            publicAppConfigZipArchive: getPublicAppConfigFileFromStore());

                        if (prepareMigrateResult?.Ok == null)
                        {
                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Failed to prepare migration with this Elm app:\n" + prepareMigrateResult?.Err);
                            return;
                        }

                        if (publicAppHost == null)
                        {
                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Migration not possible because there is no app (state).");
                            return;
                        }

                        if (publicAppHost.syncPersistentProcess == null)
                        {
                            context.Response.StatusCode = 500;
                            await context.Response.WriteAsync("syncPersistentProcess == null");
                            return;
                        }

                        Result<string, string> attemptMigrateResult = null;

                        publicAppHost.syncPersistentProcess.RunInLock(persistentProcess =>
                        {
                            var elmAppStateSerializedBefore = persistentProcess.ReductionRecordForCurrentState().ReducedValueLiteralString;

                            attemptMigrateResult = prepareMigrateResult.Ok(elmAppStateSerializedBefore);

                            if (attemptMigrateResult?.Ok == null)
                                return;

                            //  TODO: Add write event to history.

                            persistentProcess.SetState(attemptMigrateResult.Ok);
                        });

                        if (attemptMigrateResult?.Ok == null)
                        {
                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Attempt to migrate failed:\n" + attemptMigrateResult?.Err);
                            return;
                        }

                        context.Response.StatusCode = 200;
                        await context.Response.WriteAsync("Completed migration.");
                        return;
                    }

                    context.Response.StatusCode = 404;
                    await context.Response.WriteAsync("Not Found");
                    return;
                });
        }

        class Result<ErrT, OkT>
        {
            public ErrT Err;

            public OkT Ok;
        }

        static Result<string, Func<string, Result<string, string>>> PrepareMigrateSerializedValueWithoutChangingElmType(
            byte[] migrateElmAppZipArchive,
            byte[] publicAppConfigZipArchive)
        {
            var migrateElmAppOriginalFiles =
                ElmApp.ToFlatDictionaryWithPathComparer(
                    ZipArchive.EntriesFromZipArchive(migrateElmAppZipArchive)
                    .Select(entry =>
                        (filePath: (IImmutableList<string>)entry.name.Split(new[] { '/', '\\' }).ToImmutableList(),
                        fileContent: (IImmutableList<byte>)entry.content.ToImmutableList()))
                    .ToImmutableList());

            var pathToInterfaceModuleFile = ElmApp.FilePathFromModuleName(MigrationElmAppInterfaceModuleName);
            var pathToCompilationRootModuleFile = ElmApp.FilePathFromModuleName(MigrationElmAppCompilationRootModuleName);

            migrateElmAppOriginalFiles.TryGetValue(pathToInterfaceModuleFile, out var migrateElmAppInterfaceModuleOriginalFile);

            if (migrateElmAppInterfaceModuleOriginalFile == null)
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Did not find interface module at '" + string.Join("/", pathToInterfaceModuleFile) + "'",
                };

            var migrateElmAppInterfaceModuleOriginalText =
                Encoding.UTF8.GetString(migrateElmAppInterfaceModuleOriginalFile.ToArray());

            var migrateFunctionTypeAnnotation =
                CompileElm.TypeAnnotationFromFunctionName(MigrateElmFunctionNameInModule, migrateElmAppInterfaceModuleOriginalText);

            if (migrateFunctionTypeAnnotation == null)
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Did not find type annotation for function '" + MigrateElmFunctionNameInModule + "'"
                };

            var typeAnnotationMatch = Regex.Match(migrateFunctionTypeAnnotation, @"^\s*([\w\d_\.]+)\s*->\s*([\w\d_\.]+)\s*$");

            if (!typeAnnotationMatch.Success)
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Type annotation did not match expected pattern: '" + migrateFunctionTypeAnnotation + "'"
                };

            var inputTypeText = typeAnnotationMatch.Groups[1].Value;
            var returnTypeText = typeAnnotationMatch.Groups[2].Value;

            if (!inputTypeText.Equals(returnTypeText))
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Return type text is not equal input type text: '" + inputTypeText + "', '" + returnTypeText + "'"
                };

            var stateTypeCanonicalName =
                inputTypeText.Contains(".") ?
                inputTypeText :
                MigrationElmAppInterfaceModuleName + "." + inputTypeText;

            var compilationRootModuleInitialText = @"
module " + MigrationElmAppCompilationRootModuleName + @" exposing(decodeMigrateAndEncodeAndSerializeResult, main)

import " + MigrationElmAppInterfaceModuleName + @"
import Json.Decode
import Json.Encode


decodeMigrateAndEncode : String -> Result String String
decodeMigrateAndEncode =
    Json.Decode.decodeString jsonDecodeBackendState
        >> Result.map (" + MigrationElmAppInterfaceModuleName + "." + MigrateElmFunctionNameInModule + @" >> jsonEncodeBackendState >> Json.Encode.encode 0)
        >> Result.mapError Json.Decode.errorToString


decodeMigrateAndEncodeAndSerializeResult : String -> String
decodeMigrateAndEncodeAndSerializeResult =
    decodeMigrateAndEncode
        >> jsonEncodeResult Json.Encode.string Json.Encode.string
        >> Json.Encode.encode 0


jsonEncodeResult : (err -> Json.Encode.Value) -> (ok -> Json.Encode.Value) -> Result err ok -> Json.Encode.Value
jsonEncodeResult encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( ""Err"", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( ""Ok"", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object


main : Program Int {} String
main =
    Platform.worker
        { init = \_ -> ( {}, Cmd.none )
        , update =
            \_ _ ->
                ( decodeMigrateAndEncodeAndSerializeResult |> always {}, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
";
            var compileCodingFunctionsLogLines = new System.Collections.Generic.List<string>();

            try
            {
                var migrateElmAppFilesBeforeAddingCodingSupport =
                    migrateElmAppOriginalFiles.SetItem(
                        pathToCompilationRootModuleFile,
                        Encoding.UTF8.GetBytes(compilationRootModuleInitialText).ToImmutableList());

                var appFilesWithSupportAdded =
                    ElmApp.WithSupportForCodingElmType(
                        migrateElmAppFilesBeforeAddingCodingSupport,
                        stateTypeCanonicalName,
                        MigrationElmAppCompilationRootModuleName,
                        compileCodingFunctionsLogLines.Add,
                        out var functionNames);

                var rootModuleTextWithSupportAdded =
                    Encoding.UTF8.GetString(appFilesWithSupportAdded[pathToCompilationRootModuleFile].ToArray());

                var rootModuleText =
                    new[]
                    {
                    "jsonEncodeBackendState = " + functionNames.encodeFunctionName,
                    "jsonDecodeBackendState = " + functionNames.decodeFunctionName
                    }
                    .Aggregate(rootModuleTextWithSupportAdded, (intermediateModuleText, functionToAdd) =>
                        CompileElm.WithFunctionAdded(intermediateModuleText, functionToAdd));

                var migrateElmAppFiles = appFilesWithSupportAdded.SetItem(
                    pathToCompilationRootModuleFile,
                    Encoding.UTF8.GetBytes(rootModuleText).ToImmutableList());

                var javascriptFromElmMake = Kalmit.ProcessFromElm019Code.CompileElmToJavascript(
                    migrateElmAppFiles,
                    pathToCompilationRootModuleFile);

                var javascriptMinusCrashes = Kalmit.ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

                var javascriptToRun =
                    Kalmit.ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                        javascriptMinusCrashes,
                        new[]
                        {
                            (functionNameInElm: MigrationElmAppCompilationRootModuleName + ".decodeMigrateAndEncodeAndSerializeResult",
                            publicName: "interface_migrate",
                            arity: 1),
                        });

                var attemptMigrateFunc = new Func<string, Result<string, string>>(elmAppStateBeforeSerialized =>
                {
                    string migrateResultString = null;

                    //  TODO: For build JS engine, reuse impl from Common.

                    using (var javascriptEngine = new JavaScriptEngineSwitcher.ChakraCore.ChakraCoreJsEngine(
                            new JavaScriptEngineSwitcher.ChakraCore.ChakraCoreSettings
                            {
                                DisableEval = true,
                                EnableExperimentalFeatures = true,
                                MaxStackSize = 10_000_000,
                            }
                            ))
                    {
                        var initAppResult = javascriptEngine.Evaluate(javascriptToRun);

                        var migrateResult = javascriptEngine.CallFunction(
                            "interface_migrate", elmAppStateBeforeSerialized);

                        migrateResultString = migrateResult.ToString();
                    }

                    var migrateResultStructure =
                        Newtonsoft.Json.JsonConvert.DeserializeObject<Kalmit.ElmValueCommonJson.Result<string, string>>(migrateResultString);

                    var elmAppStateMigratedSerialized = migrateResultStructure?.Ok?.FirstOrDefault();

                    if (elmAppStateMigratedSerialized == null)
                    {
                        return new Result<string, string>
                        {
                            Err = "Decoding of serialized state failed for this migration:\n" + migrateResultStructure?.Err?.FirstOrDefault()
                        };
                    }

                    var publicAppConfigElmApp =
                        ElmApp.ToFlatDictionaryWithPathComparer(
                            WebAppConfiguration.FromFiles(
                                ZipArchive.EntriesFromZipArchive(publicAppConfigZipArchive)
                                .Select(entry =>
                                    (path: (IImmutableList<string>)entry.name.Split(new[] { '/', '\\' }).ToImmutableList(),
                                    content: (IImmutableList<byte>)entry.content.ToImmutableList())
                                    ).ToImmutableList()).ElmAppFiles);

                    using (var testProcess = new PersistentProcessWithHistoryOnFileFromElm019Code(
                        new EmptyProcessStoreReader(),
                        publicAppConfigElmApp,
                        logger: logEntry => { }))
                    {
                        testProcess.SetState(elmAppStateMigratedSerialized);

                        var resultingState = testProcess.ReductionRecordForCurrentState()?.ReducedValueLiteralString;

                        if (resultingState != elmAppStateMigratedSerialized)
                            return new Result<string, string>
                            {
                                Err = "Failed to load the migrated serialized state with the current public app configuration. resulting State:\n" + resultingState
                            };
                    }

                    return new Result<string, string>
                    {
                        Ok = elmAppStateMigratedSerialized
                    };
                });

                return new Result<string, Func<string, Result<string, string>>>
                {
                    Ok = attemptMigrateFunc
                };
            }
            catch (Exception e)
            {
                return new Result<string, Func<string, Result<string, string>>>
                {
                    Err = "Failed with exception:\n" + e.ToString() + "\ncompileCodingFunctionsLogLines:\n" + String.Join("\n", compileCodingFunctionsLogLines)
                };
            }
        }
    }

    public class SyncPersistentProcess : IPersistentProcess
    {
        readonly object @lock = new object();

        readonly IPersistentProcess persistentProcess;

        public SyncPersistentProcess(IPersistentProcess persistentProcess)
        {
            this.persistentProcess = persistentProcess;
        }

        public void RunInLock(Action<IPersistentProcess> action)
        {
            lock (@lock)
            {
                action(persistentProcess);
            }
        }

        public (IReadOnlyList<string> responses, (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash)) ProcessEvents(IReadOnlyList<string> serializedEvents)
        {
            lock (@lock)
            {
                return persistentProcess.ProcessEvents(serializedEvents);
            }
        }

        public ReductionRecord ReductionRecordForCurrentState()
        {
            lock (@lock)
            {
                return persistentProcess.ReductionRecordForCurrentState();
            }
        }

        public (byte[] serializedCompositionRecord, byte[] serializedCompositionRecordHash) SetState(string state)
        {
            lock (@lock)
            {
                return persistentProcess.SetState(state);
            }
        }
    }
}
