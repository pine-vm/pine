using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using System;
using System.Linq;

namespace Kalmit.PersistentProcess.WebHost
{
    public class Program
    {
        public static void Main(string[] args)
        {
            if (args.Contains("build-config"))
            {
                Console.WriteLine("I build the configuration.");
                BuildConfigurationFromArguments.BuildConfiguration(args);
                return;
            }

            if (args.Contains("start-server"))
            {
                Console.WriteLine("I start a server.");
                var webHostBuilder = CreateWebHostBuilder(args);
                webHostBuilder.Build().Run();
                return;
            }
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
                .ConfigureKestrel(kestrelOptions =>
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
