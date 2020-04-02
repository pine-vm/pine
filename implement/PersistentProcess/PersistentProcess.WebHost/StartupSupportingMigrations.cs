using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;

namespace Kalmit.PersistentProcess.WebHost
{
    public class StartupSupportingMigrations
    {
        static public string PathApiSetAppConfigAndInitState => "/api/set-app-config-and-init-state";

        static public string PathApiSetAppConfigAndContinueState => "/api/set-app-config-and-continue-state";

        static public string PathApiMigrateElmState => "/api/migrate-elm-state";

        static public string PathApiGetAppConfig => "/api/get-app-config";

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

            IWebHost publicAppWebHost = null;

            void stopPublicApp()
            {
                lock (publicAppLock)
                {
                    if (publicAppWebHost != null)
                    {
                        publicAppWebHost.StopAsync(TimeSpan.FromSeconds(10)).Wait();
                        publicAppWebHost.Dispose();
                        publicAppWebHost = null;
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

                    var webHost =
                        Program.CreateWebHostBuilder(null, overrideDefaultUrls: publicWebHostUrls)
                        .WithSettingAdminRootPassword(rootPassword)
                        .WithSettingDateTimeOffsetDelegate(getDateTimeOffset)
                        .WithWebAppConfigurationZipArchive(webAppConfigZipArchive)
                        .WithProcessStoreFileStore(elmAppProcessStore)
                        .Build();

                    webHost.StartAsync(appLifetime.ApplicationStopping).Wait();
                    publicAppWebHost = webHost;
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

                        var migrateElmAppFiles =
                            ZipArchive.EntriesFromZipArchive(migrateElmAppConfigZipArchive)
                            .Select(entry =>
                                (filePath: (IImmutableList<string>)entry.name.Split(new[] { '/', '\\' }).ToImmutableList(),
                                fileContent: (IImmutableList<byte>)entry.content.ToImmutableList()))
                            .ToImmutableList();

                        if (publicAppWebHost == null)
                        {
                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Migration not possible because there is no app (state).");
                            return;
                        }

                        var publicAppWebHostAddresses =
                            publicAppWebHost.ServerFeatures.Get<Microsoft.AspNetCore.Hosting.Server.Features.IServerAddressesFeature>().Addresses;

                        string elmAppStateSerializedBefore = null;

                        var httpPathToProcessState = Configuration.AdminPath + Configuration.ApiPersistentProcessStatePath;

                        System.Net.Http.HttpClient createPublicHostClientWithAuthorizationHeader()
                        {
                            var adminClient = new System.Net.Http.HttpClient()
                            {
                                BaseAddress = new Uri(publicAppWebHostAddresses.First())
                            };

                            adminClient.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue(
                                "Basic",
                                Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                                    WebHost.Configuration.BasicAuthenticationForAdminRoot(rootPassword))));

                            return adminClient;
                        }

                        using (var publicAppClient = createPublicHostClientWithAuthorizationHeader())
                        {
                            var getStateResponse = publicAppClient.GetAsync(httpPathToProcessState).Result;

                            elmAppStateSerializedBefore = getStateResponse.Content.ReadAsStringAsync().Result;
                        }

                        if (!(0 < elmAppStateSerializedBefore?.Length))
                        {
                            context.Response.StatusCode = 500;
                            await context.Response.WriteAsync("Failed to read the current app state.");
                            return;
                        }

                        var javascriptFromElmMake = Kalmit.ProcessFromElm019Code.CompileElmToJavascript(
                            ElmApp.ToFlatDictionaryWithPathComparer(migrateElmAppFiles),
                            ImmutableList.Create("src", "Main.elm"));

                        var javascriptMinusCrashes = Kalmit.ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

                        var listFunctionToPublish =
                            new[]
                            {
                                (functionNameInElm: "Main.migrate",
                                publicName: "interface_migrate",
                                arity: 1),
                            };

                        var javascriptToRun =
                            Kalmit.ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                                javascriptMinusCrashes,
                                listFunctionToPublish);

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
                                "interface_migrate", elmAppStateSerializedBefore);

                            migrateResultString = migrateResult.ToString();
                        }

                        using (var publicAppClient = createPublicHostClientWithAuthorizationHeader())
                        {
                            var setStateResponse = publicAppClient.PostAsync(
                                httpPathToProcessState, new System.Net.Http.StringContent(migrateResultString)).Result;

                            if (!setStateResponse.IsSuccessStatusCode)
                            {
                                context.Response.StatusCode = 500;
                                await context.Response.WriteAsync("Failed to set the migrated state.");
                                return;

                            }
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
    }
}
