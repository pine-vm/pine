using Microsoft.AspNetCore.Hosting;

namespace Kalmit.PersistentProcess.WebHost
{
    public class Program
    {
        public static void Main(string[] args)
        {
            CreateWebHostBuilder(args).Build().Run();
        }

        public static IWebHostBuilder CreateWebHostBuilder(string[] args) =>
            Microsoft.AspNetCore.WebHost.CreateDefaultBuilder(args)
                .UseStartup<Startup>();
    }
}
