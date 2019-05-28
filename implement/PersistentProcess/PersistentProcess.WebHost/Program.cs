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
            var webHostBuilder = CreateWebHostBuilder(args);

            if (args.Contains("build-config"))
                webHostBuilder = webHostBuilder.BuildConfiguration(args);

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
