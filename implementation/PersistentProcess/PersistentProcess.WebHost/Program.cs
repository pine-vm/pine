using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;

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
                .ConfigureAppConfiguration(builder =>
                    builder.AddEnvironmentVariables(AppSettingsToEnvironmentVariablesPrefixObservedInAzure))
                .UseStartup<Startup>();
    }
}
