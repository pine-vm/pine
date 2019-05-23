using System;
using System.Linq;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;

namespace Kalmit.PersistentProcess.WebHost
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var webHostBuilder = CreateWebHostBuilder(args);

            if (args.Contains("build-config"))
            {
                var currentDirectory = Environment.CurrentDirectory;

                Console.WriteLine("I build the configuration before starting the server. The currentDirectory is '" + currentDirectory + "'");

                var tempConfigDirectory = System.IO.Path.Combine(currentDirectory, ".kalmit", "temp-config");
                var webAppConfigFilePath = System.IO.Path.Combine(tempConfigDirectory, "web-app-config.zip");
                var processStoreDirectoryPathDefault = System.IO.Path.Combine(tempConfigDirectory, "process-store");

                System.IO.Directory.CreateDirectory(tempConfigDirectory);

                webHostBuilder.WithSettingWebAppConfigurationFilePath(webAppConfigFilePath);

                //  Provide a default location for the process store, which can be overridden.
                webHostBuilder.WithSettingProcessStoreDirectoryPathDefault(processStoreDirectoryPathDefault);

                var elmAppfiles =
                    ElmAppWithEntryConfig
                    .FromFilesFilteredForElmApp(Filesystem.GetAllFilesFromDirectory(currentDirectory))
                    .AsFiles();

                Console.WriteLine("I found " + elmAppfiles.Count + " Elm app files to build the server app.");

                var webAppConfig =
                    new WebAppConfiguration()
                    .WithElmApp(ZipArchive.ZipArchiveFromEntries(elmAppfiles));

                var webAppConfigFile = ZipArchive.ZipArchiveFromEntries(webAppConfig.AsFiles());

                Console.WriteLine("I built web app config " +
                    CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(webAppConfigFile)) +
                    " to use for the server.");

                System.IO.File.WriteAllBytes(webAppConfigFilePath, webAppConfigFile);
            }

            webHostBuilder.Build().Run();
        }

        public static IWebHostBuilder CreateWebHostBuilder(string[] args) =>
            Microsoft.AspNetCore.WebHost.CreateDefaultBuilder(args)
                .ConfigureAppConfiguration((hostingContext, config) =>
                {
                    config.AddJsonFile("appsettings.json", optional: true, reloadOnChange: true);
                })
                .ConfigureLogging((hostingContext, logging) =>
                {
                    logging.AddConfiguration(hostingContext.Configuration.GetSection("Logging"));
                    logging.AddConsole();
                    logging.AddDebug();
                })
                .UseKestrel(kestrelOptions =>
                {
                    kestrelOptions.ConfigureHttpsDefaults(httpsOptions =>
                    {
                        httpsOptions.ServerCertificateSelector = (c, s) => FluffySpoon.AspNet.LetsEncrypt.LetsEncryptRenewalService.Certificate;
                    });
                })
                .ConfigureAppConfiguration(builder => builder.AddEnvironmentVariables("APPSETTING_"))
                .UseUrls("http://*", "https://*")
                .UseStartup<Startup>();
    }
}
