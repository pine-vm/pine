using Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations;
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

namespace Kalmit.PersistentProcess.WebHost
{
    public class StartupAdminInterface
    {
        static public string PathApiDeployAppConfigAndInitElmAppState => "/api/deploy-app-config-and-init-elm-app-state";

        static public string PathApiDeployAppConfigAndMigrateElmAppState => "/api/deploy-app-config-and-migrate-elm-app-state";

        static public string PathApiElmAppState => "/api/elm-app-state";

        static public string PathApiGetAppConfig => "/api/get-app-config";

        public StartupAdminInterface()
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

            object publicAppLock = new object();

            PublicHostConfiguration publicAppHost = null;

            void stopPublicApp()
            {
                lock (publicAppLock)
                {
                    if (publicAppHost != null)
                    {
                        publicAppHost?.webHost?.StopAsync(TimeSpan.FromSeconds(10)).Wait();
                        publicAppHost?.webHost?.Dispose();
                        publicAppHost = null;
                    }
                }
            }

            appLifetime.ApplicationStopping.Register(() =>
            {
                stopPublicApp();
            });

            var processStoreWriter =
                new ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                processStoreFileStore,
                () =>
                {
                    var time = getDateTimeOffset();
                    var directoryName = time.ToString("yyyy-MM-dd");
                    return ImmutableList.Create(directoryName, directoryName + "T" + time.ToString("HH") + ".composition.jsonl");
                });

            void startPublicApp()
            {
                lock (publicAppLock)
                {
                    stopPublicApp();

                    var newPublicAppConfig = new PublicHostConfiguration { };

                    var processStoreReader =
                        new ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore(processStoreFileStore);

                    using (var restoredProcess = PersistentProcess.PersistentProcessVolatileRepresentation.Restore(
                            processStoreReader,
                            _ => { }))
                    {
                        var appConfigComponent =
                            restoredProcess?.lastAppConfig?.appConfigComponent;

                        var webHost =
                            appConfigComponent == null
                            ?
                            null
                            :
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
                            .UseStartup<StartupPublicApp>()
                            .WithSettingDateTimeOffsetDelegate(getDateTimeOffset)
                            .ConfigureServices(services =>
                            {
                                services.AddSingleton<ProcessStoreSupportingMigrations.IProcessStoreReader>(processStoreReader);
                                services.AddSingleton<ProcessStoreSupportingMigrations.IProcessStoreWriter>(processStoreWriter);
                            })
                            .ConfigureServices(services => services.AddSingleton(new PersistentProcessMap
                            {
                                mapPersistentProcess = originalPersistentProcess =>
                                {
                                    return newPublicAppConfig.syncPersistentProcess = new SyncPersistentProcess(originalPersistentProcess);
                                }
                            }))
                            .Build();

                        newPublicAppConfig.webHost = webHost;

                        webHost?.StartAsync(appLifetime.ApplicationStopping).Wait();
                        publicAppHost = newPublicAppConfig;
                    }
                }
            }

            startPublicApp();

            byte[] getPublicAppConfigFromStoreAsZipArchive()
            {
                using (var restoredProcess = PersistentProcess.PersistentProcessVolatileRepresentation.Restore(
                        new ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore(processStoreFileStore),
                        _ => { }))
                {
                    var appConfigComponent =
                        restoredProcess?.lastAppConfig?.appConfigComponent;

                    var appConfigTree = Composition.ParseAsTree(appConfigComponent).ok;

                    var appConfigFilesNamesAndContents =
                        appConfigTree.EnumerateBlobsTransitive()
                        .Select(blobPathAndContent => (
                            fileName: (IImmutableList<string>)blobPathAndContent.path.Select(name => Encoding.UTF8.GetString(name.ToArray())).ToImmutableList(),
                            fileContent: blobPathAndContent.blobContent))
                        .ToImmutableList();

                    return
                        ZipArchive.ZipArchiveFromEntries(
                            ElmApp.ToFlatDictionaryWithPathComparer(appConfigFilesNamesAndContents));
                }
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
                                @"Basic realm=""" + context.Request.Host + @""", charset=""UTF-8""");
                            await context.Response.WriteAsync("Unauthorized");
                            return;
                        }
                    }

                    var requestPathIsDeployAppConfigAndInitElmAppState =
                        context.Request.Path.Equals(new PathString(PathApiDeployAppConfigAndInitElmAppState));

                    if (context.Request.Path.Equals(new PathString(PathApiGetAppConfig)))
                    {
                        if (!string.Equals(context.Request.Method, "get", StringComparison.InvariantCultureIgnoreCase))
                        {
                            context.Response.StatusCode = 405;
                            await context.Response.WriteAsync("Method not supported.");
                            return;
                        }

                        var appConfigZipArchive = getPublicAppConfigFromStoreAsZipArchive();

                        if (appConfigZipArchive == null)
                        {
                            context.Response.StatusCode = 200;
                            await context.Response.WriteAsync("I did not find an app config file in the store.");
                            return;
                        }

                        context.Response.StatusCode = 200;
                        context.Response.Headers.ContentLength = appConfigZipArchive.LongLength;
                        context.Response.Headers.Add("Content-Disposition", new ContentDispositionHeaderValue("attachment") { FileName = "web-app-config.zip" }.ToString());
                        context.Response.Headers.Add("Content-Type", new MediaTypeHeaderValue("application/zip").ToString());

                        await context.Response.Body.WriteAsync(appConfigZipArchive);
                        return;
                    }

                    if (requestPathIsDeployAppConfigAndInitElmAppState ||
                        context.Request.Path.Equals(new PathString(PathApiDeployAppConfigAndMigrateElmAppState)))
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

                        var appConfigTree =
                            Composition.TreeFromSetOfBlobsWithCommonFilePath(
                                ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive));

                        var appConfigComponent = Composition.FromTree(appConfigTree);

                        processStoreWriter.StoreComponent(appConfigComponent);

                        var appConfigValueInFile =
                            new ProcessStoreSupportingMigrations.ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfigComponent))
                            };

                        var compositionLogEvent =
                            requestPathIsDeployAppConfigAndInitElmAppState
                            ?
                            new ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent
                            {
                                DeployAppConfigAndInitElmAppState = appConfigValueInFile,
                            }
                            :
                            new ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent
                            {
                                DeployAppConfigAndMigrateElmAppState = appConfigValueInFile,
                            };

                        await attemptContinueWithCompositionEventAndSendHttpResponse(compositionLogEvent);
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
                            var syncPersistentProcess = publicAppHost?.syncPersistentProcess;

                            if (syncPersistentProcess == null)
                            {
                                context.Response.StatusCode = 500;
                                await context.Response.WriteAsync("Not possible because there is no Elm app deployed at the moment.");
                                return;
                            }

                            var components = new List<Composition.Component>();

                            var storeWriter = new DelegatingProcessStoreWriter
                            {
                                StoreComponentDelegate = components.Add,
                                StoreProvisionalReductionDelegate = _ => { },
                                AppendSerializedCompositionLogRecordDelegate = _ => { }
                            };

                            var reductionRecord =
                                syncPersistentProcess.StoreReductionRecordForCurrentState(storeWriter);

                            var elmAppStateReductionHashBase16 = reductionRecord.elmAppState?.HashBase16;

                            var elmAppStateReductionComponent =
                                components.First(c => CommonConversion.StringBase16FromByteArray(Composition.GetHash(c)) == elmAppStateReductionHashBase16);

                            var elmAppStateReductionString =
                                Encoding.UTF8.GetString(elmAppStateReductionComponent.BlobContent.ToArray());

                            context.Response.StatusCode = 200;
                            context.Response.ContentType = "application/json";
                            await context.Response.WriteAsync(elmAppStateReductionString);
                            return;
                        }
                        else
                        {
                            if (string.Equals(context.Request.Method, "post", StringComparison.InvariantCultureIgnoreCase))
                            {
                                var elmAppStateToSet = new StreamReader(context.Request.Body, System.Text.Encoding.UTF8).ReadToEndAsync().Result;

                                var elmAppStateComponent = Composition.Component.Blob(Encoding.UTF8.GetBytes(elmAppStateToSet));

                                var appConfigValueInFile =
                                    new ProcessStoreSupportingMigrations.ValueInFileStructure
                                    {
                                        HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppStateComponent))
                                    };

                                processStoreWriter.StoreComponent(elmAppStateComponent);

                                await attemptContinueWithCompositionEventAndSendHttpResponse(
                                    new ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent
                                    {
                                        SetElmAppState = appConfigValueInFile
                                    });
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

                    async System.Threading.Tasks.Task attemptContinueWithCompositionEventAndSendHttpResponse(
                        ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
                    {
                        var projectedStoreReader = IProcessStoreReader.ProjectReaderForAppendedCompositionLogEvent(
                            originalStore: new ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore(processStoreFileStore),
                            compositionLogEvent: compositionLogEvent);

                        using (var projectedProcess =
                            PersistentProcess.PersistentProcessVolatileRepresentation.Restore(
                                projectedStoreReader,
                                _ => { }))
                        {
                            if (compositionLogEvent.DeployAppConfigAndMigrateElmAppState != null ||
                                compositionLogEvent.SetElmAppState != null)
                            {
                                if (projectedProcess.lastSetElmAppStateResult?.Ok == null)
                                {
                                    context.Response.StatusCode = 400;
                                    await context.Response.WriteAsync("Failed to migrate Elm app state for this deployment: " + projectedProcess.lastSetElmAppStateResult?.Err);
                                    return;
                                }
                            }
                        }

                        processStoreWriter.AppendSerializedCompositionLogRecord(
                            projectedStoreReader.EnumerateSerializedCompositionLogRecordsReverse().First());

                        startPublicApp();

                        context.Response.StatusCode = 200;
                        await context.Response.WriteAsync("Successfully deployed this configuration and started the web server.");
                        return;
                    }

                    if (context.Request.Path.Equals(PathString.Empty) || context.Request.Path.Equals(new PathString("/")))
                    {
                        context.Response.StatusCode = 200;
                        await context.Response.WriteAsync(
                            "Welcome to Elm-fullstack version " + Program.AppVersionId + ".\n" +
                            "To learn about this admin interface, see http://elm-fullstack.org/");
                        return;
                    }

                    context.Response.StatusCode = 404;
                    await context.Response.WriteAsync("Not Found");
                    return;
                });
        }
    }

    public class SyncPersistentProcess : PersistentProcess.IPersistentProcess
    {
        readonly object @lock = new object();

        readonly PersistentProcess.IPersistentProcess persistentProcess;

        public SyncPersistentProcess(PersistentProcess.IPersistentProcess persistentProcess)
        {
            this.persistentProcess = persistentProcess;
        }

        public void RunInLock(Action<PersistentProcess.IPersistentProcess> action)
        {
            lock (@lock)
            {
                action(persistentProcess);
            }
        }

        public IImmutableList<string> ProcessElmAppEvents(
            ProcessStoreSupportingMigrations.IProcessStoreWriter storeWriter, IReadOnlyList<string> serializedEvents)
        {
            lock (@lock)
            {
                return persistentProcess.ProcessElmAppEvents(storeWriter, serializedEvents);
            }
        }

        public ProcessStoreSupportingMigrations.ProvisionalReductionRecordInFile StoreReductionRecordForCurrentState(
            ProcessStoreSupportingMigrations.IProcessStoreWriter storeWriter)
        {
            lock (@lock)
            {
                return persistentProcess.StoreReductionRecordForCurrentState(storeWriter);
            }
        }
    }
}
