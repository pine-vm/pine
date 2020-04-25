using Kalmit.ProcessStore;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
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
        static public string PathApiSetAppConfigAndInitElmState => "/api/set-app-config-and-init-elm-state";

        static public string PathApiSetAppConfigAndContinueElmState => "/api/set-app-config-and-continue-elm-state";

        static public string PathApiSetAppConfigAndMigrateElmState => "/api/set-app-config-and-migrate-elm-state";

        static public string PathApiMigrateElmState => "/api/migrate-elm-state";

        static public string PathApiElmAppState => "/api/elm-app-state";

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

            void stopPublicApp(out ReductionRecord processStateReduction)
            {
                lock (publicAppLock)
                {
                    processStateReduction = null;

                    if (publicAppHost != null)
                    {
                        publicAppHost?.webHost.StopAsync(TimeSpan.FromSeconds(10)).Wait();

                        ReductionRecord lambdaLimit_processStateReduction = null;

                        publicAppHost.syncPersistentProcess.RunInLock(persistentProcess =>
                        {
                            lambdaLimit_processStateReduction = persistentProcess.ReductionRecordForCurrentState();
                        });

                        processStateReduction = lambdaLimit_processStateReduction;

                        publicAppHost?.webHost.Dispose();
                        publicAppHost = null;
                    }
                }
            }

            appLifetime.ApplicationStopping.Register(() =>
            {
                stopPublicApp(out var _);
            });

            void setAndStartPublicApp(
                byte[] webAppConfigZipArchive,
                bool elmAppClearProcessStore,
                string elmAppSetProcessState)
            {
                lock (publicAppLock)
                {
                    processStoreFileStore.SetFileContent(pathToPublicAppConfigFile, webAppConfigZipArchive);

                    startPublicApp(
                        webAppConfigZipArchive,
                        elmAppClearProcessStore: elmAppClearProcessStore,
                        elmAppSetProcessState: elmAppSetProcessState);
                }
            }

            void startPublicApp(
                byte[] webAppConfigZipArchive,
                bool elmAppClearProcessStore,
                string elmAppSetProcessState)
            {
                lock (publicAppLock)
                {
                    stopPublicApp(out var _);

                    if (elmAppClearProcessStore)
                    {
                        foreach (var filePath in elmAppProcessStore.ListFilesInDirectory(ImmutableList<string>.Empty).ToImmutableList())
                            elmAppProcessStore.DeleteFile(filePath);
                    }

                    var newPublicAppConfig = new PublicHostConfiguration { };

                    var webHost =
                        Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                        .ConfigureLogging((hostingContext, logging) =>
                        {
                            logging.AddConfiguration(hostingContext.Configuration.GetSection("Logging"));
                            logging.AddConsole();
                            logging.AddDebug();
                        })
                        .ConfigureKestrel(kestrelOptions =>
                        {
                            kestrelOptions.ConfigureHttpsDefaults(httpsOptions =>
                            {
                                httpsOptions.ServerCertificateSelector = (c, s) => FluffySpoon.AspNet.LetsEncrypt.LetsEncryptRenewalService.Certificate;
                            });
                        })
                        .UseUrls(publicWebHostUrls ?? new[] { "http://*", "https://*" })
                        .UseStartup<Startup>()

                        //  TODO: Remove WithSettingAdminRootPassword when apps in production are migrated to new admin interface.
                        .WithSettingAdminRootPassword(rootPassword)
                        .WithSettingDateTimeOffsetDelegate(getDateTimeOffset)
                        .WithWebAppConfigurationZipArchive(webAppConfigZipArchive)
                        .WithProcessStoreFileStore(elmAppProcessStore)
                        .ConfigureServices(services => services.AddSingleton(new PersistentProcessMap
                        {
                            mapPersistentProcess = originalPersistentProcess =>
                            {
                                if (elmAppSetProcessState != null)
                                    originalPersistentProcess.SetState(elmAppSetProcessState);

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

            void startPublicAppForConfigFromStore()
            {
                var publicAppConfigFile = getPublicAppConfigFileFromStore();

                if (publicAppConfigFile != null)
                    startPublicApp(publicAppConfigFile, elmAppClearProcessStore: false, elmAppSetProcessState: null);
            }

            startPublicAppForConfigFromStore();

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
                        context.Request.Path.Equals(new PathString(PathApiSetAppConfigAndInitElmState));

                    var requestPathIsSetAppAndContinueState =
                        context.Request.Path.Equals(new PathString(PathApiSetAppConfigAndContinueElmState));

                    if (context.Request.Path.Equals(new PathString(PathApiGetAppConfig)))
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
                            elmAppClearProcessStore: requestPathIsSetAppAndInitState,
                            elmAppSetProcessState: null);

                        context.Response.StatusCode = 200;
                        await context.Response.WriteAsync("Successfully set the app and started the web server.");
                        return;
                    }

                    if (context.Request.Path.Equals(new PathString(PathApiMigrateElmState)))
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

                        var destinationAppConfigZipArchive = getPublicAppConfigFileFromStore();

                        if (destinationAppConfigZipArchive == null)
                        {
                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Migration not possible because there is no app (state).");
                            return;
                        }

                        await attemptMigrateElmStateAndSetAppConfigAndSendHttpResponse(
                            migrateElmAppConfigZipArchive: migrateElmAppConfigZipArchive,
                            webAppConfigZipArchive: destinationAppConfigZipArchive);

                        return;
                    }

                    if (context.Request.Path.Equals(new PathString(PathApiSetAppConfigAndMigrateElmState)))
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

                        var elmAppFiles =
                            ElmApp.ToFlatDictionaryWithPathComparer(
                                WebAppConfiguration.FromFiles(
                                    ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive)
                                    .Select(entry =>
                                        (path: (IImmutableList<string>)entry.name.Split(new[] { '/', '\\' }).ToImmutableList(),
                                        content: (IImmutableList<byte>)entry.content.ToImmutableList())
                                        ).ToImmutableList()).ElmAppFiles);

                        var migrateElmAppConfigZipArchive = ZipArchive.ZipArchiveFromEntries(elmAppFiles);

                        await attemptMigrateElmStateAndSetAppConfigAndSendHttpResponse(
                            migrateElmAppConfigZipArchive: migrateElmAppConfigZipArchive,
                            webAppConfigZipArchive: webAppConfigZipArchive);

                        return;
                    }

                    if (context.Request.Path.Equals(new PathString(PathApiElmAppState)))
                    {
                        if (publicAppHost == null)
                        {
                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Not possible because there is no app (state).");
                            return;
                        }

                        if (string.Equals(context.Request.Method, "get", StringComparison.InvariantCultureIgnoreCase))
                        {
                            var syncPersistentProcess = publicAppHost.syncPersistentProcess;

                            if (syncPersistentProcess == null)
                            {
                                context.Response.StatusCode = 500;
                                await context.Response.WriteAsync("syncPersistentProcess == null");
                                return;
                            }

                            var reducedValueLiteralString =
                                syncPersistentProcess.ReductionRecordForCurrentState().ReducedValueLiteralString;

                            context.Response.StatusCode = 200;
                            context.Response.ContentType = "application/json";
                            await context.Response.WriteAsync(reducedValueLiteralString);
                            return;
                        }
                        else
                        {
                            if (string.Equals(context.Request.Method, "post", StringComparison.InvariantCultureIgnoreCase))
                            {
                                var stateToSet = new StreamReader(context.Request.Body, System.Text.Encoding.UTF8).ReadToEndAsync().Result;

                                startPublicApp(
                                    webAppConfigZipArchive: getPublicAppConfigFileFromStore(),
                                    elmAppClearProcessStore: false,
                                    elmAppSetProcessState: stateToSet);

                                context.Response.StatusCode = 200;
                                await context.Response.WriteAsync("Successfully set elm app state.");
                                return;
                            }
                            else
                            {
                                context.Response.StatusCode = 405;
                                await context.Response.WriteAsync("Method not supported.");
                                return;
                            }
                        }
                    }

                    async System.Threading.Tasks.Task attemptMigrateElmStateAndSetAppConfigAndSendHttpResponse(
                        byte[] migrateElmAppConfigZipArchive,
                        byte[] webAppConfigZipArchive)
                    {
                        var prepareMigrateResult = PrepareMigrateSerializedValue(
                            migrateElmAppConfigZipArchive,
                            destinationAppConfigZipArchive: webAppConfigZipArchive);

                        if (prepareMigrateResult?.Ok == null)
                        {
                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Failed to prepare migration with this Elm app:\n" + prepareMigrateResult?.Err);
                            return;
                        }

                        if (publicAppHost.syncPersistentProcess == null)
                        {
                            context.Response.StatusCode = 500;
                            await context.Response.WriteAsync("syncPersistentProcess == null");
                            return;
                        }

                        stopPublicApp(out var elmAppStateBefore);

                        if (elmAppStateBefore == null)
                        {
                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Migration not possible because there is no Elm app state.");
                            return;
                        }

                        var attemptMigrateResult = prepareMigrateResult.Ok(elmAppStateBefore.ReducedValueLiteralString);

                        if (attemptMigrateResult?.Ok == null)
                        {
                            startPublicAppForConfigFromStore();

                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Attempt to migrate failed:\n" + attemptMigrateResult?.Err);
                            return;
                        }

                        //  TODO: Add write event to history.

                        setAndStartPublicApp(
                            webAppConfigZipArchive,
                            elmAppClearProcessStore: false,
                            elmAppSetProcessState: attemptMigrateResult.Ok);

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

        static Result<string, Func<string, Result<string, string>>> PrepareMigrateSerializedValue(
            byte[] migrateElmAppZipArchive,
            byte[] destinationAppConfigZipArchive)
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

            var inputTypeCanonicalName =
                inputTypeText.Contains(".") ?
                inputTypeText :
                MigrationElmAppInterfaceModuleName + "." + inputTypeText;

            var returnTypeCanonicalName =
                returnTypeText.Contains(".") ?
                returnTypeText :
                MigrationElmAppInterfaceModuleName + "." + returnTypeText;

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

                var appFilesWithInputCodingFunctions =
                    ElmApp.WithSupportForCodingElmType(
                        migrateElmAppFilesBeforeAddingCodingSupport,
                        inputTypeCanonicalName,
                        MigrationElmAppCompilationRootModuleName,
                        compileCodingFunctionsLogLines.Add,
                        out var inputTypeFunctionNames);

                var appFilesWithCodingFunctions =
                    ElmApp.WithSupportForCodingElmType(
                        appFilesWithInputCodingFunctions,
                        returnTypeCanonicalName,
                        MigrationElmAppCompilationRootModuleName,
                        compileCodingFunctionsLogLines.Add,
                        out var returnTypeFunctionNames);

                var rootModuleTextWithSupportAdded =
                    Encoding.UTF8.GetString(appFilesWithCodingFunctions[pathToCompilationRootModuleFile].ToArray());

                var rootModuleText =
                    new[]
                    {
                        "jsonDecodeBackendState = " + inputTypeFunctionNames.decodeFunctionName,
                        "jsonEncodeBackendState = " + returnTypeFunctionNames.encodeFunctionName
                    }
                    .Aggregate(rootModuleTextWithSupportAdded, (intermediateModuleText, functionToAdd) =>
                        CompileElm.WithFunctionAdded(intermediateModuleText, functionToAdd));

                var migrateElmAppFiles = appFilesWithCodingFunctions.SetItem(
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

                    var destinationAppConfigElmApp =
                        ElmApp.ToFlatDictionaryWithPathComparer(
                            WebAppConfiguration.FromFiles(
                                ZipArchive.EntriesFromZipArchive(destinationAppConfigZipArchive)
                                .Select(entry =>
                                    (path: (IImmutableList<string>)entry.name.Split(new[] { '/', '\\' }).ToImmutableList(),
                                    content: (IImmutableList<byte>)entry.content.ToImmutableList())
                                    ).ToImmutableList()).ElmAppFiles);

                    using (var testProcess = new PersistentProcessWithHistoryOnFileFromElm019Code(
                        new EmptyProcessStoreReader(),
                        destinationAppConfigElmApp,
                        logger: logEntry => { }))
                    {
                        testProcess.SetState(elmAppStateMigratedSerialized);

                        var resultingState = testProcess.ReductionRecordForCurrentState()?.ReducedValueLiteralString;

                        if (resultingState != elmAppStateMigratedSerialized)
                            return new Result<string, string>
                            {
                                Err = "Failed to load the migrated serialized state with the destination public app configuration. resulting State:\n" + resultingState
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
