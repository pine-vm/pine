using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;

namespace Kalmit.PersistentProcess.WebHost
{
    public class Program
    {
        static string AppSettingsToEnvironmentVariablesPrefixObservedInAzure => "APPSETTING_";

        public static void Main(string[] args)
        {
            CreateWebHostBuilder(args).Build().Run();
        }

        public static IWebHostBuilder CreateWebHostBuilder(string[] args) =>
            Microsoft.AspNetCore.WebHost.CreateDefaultBuilder(args)
                .ConfigureLogging(l => l.AddConsole(x => x.IncludeScopes = true))
                .UseKestrel(kestrelOptions =>
                {
                    kestrelOptions.ConfigureHttpsDefaults(httpsOptions =>
                    {
                        httpsOptions.ServerCertificateSelector = (c, s) => FluffySpoon.AspNet.LetsEncrypt.LetsEncryptRenewalService.Certificate;
                    });
                })
                .ConfigureAppConfiguration(builder =>
                    builder.AddEnvironmentVariables(AppSettingsToEnvironmentVariablesPrefixObservedInAzure))
                .UseUrls("http://*", "https://*")
                .UseStartup<Startup>();
    }
}
