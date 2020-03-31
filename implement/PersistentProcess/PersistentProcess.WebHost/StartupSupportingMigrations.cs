using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
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

        static public string PathApiSetAppConfigAndContinueState => "/api/set-app-and-continue-state";

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
            var publicWebHostHttpPortString = configuration.GetValue<string>(Configuration.PublicWebHostHttpPortSettingKey);

            int? publicWebHostHttpPort = null;

            if (0 < publicWebHostHttpPortString?.Length)
                publicWebHostHttpPort = int.Parse(publicWebHostHttpPortString);

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
                        Program.CreateWebHostBuilder(null, httpPort: publicWebHostHttpPort)
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

                    context.Response.StatusCode = 404;
                    await context.Response.WriteAsync("Not Found");
                    return;
                });
        }
    }
}
