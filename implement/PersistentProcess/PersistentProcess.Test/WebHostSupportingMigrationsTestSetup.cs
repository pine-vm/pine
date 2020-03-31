using Kalmit.PersistentProcess.WebHost;
using Microsoft.AspNetCore.Hosting;
using System;
using System.IO;

namespace Kalmit.PersistentProcess.Test
{
    public class WebHostSupportingMigrationsTestSetup : IDisposable
    {
        static int PublicWebHostHttpPort => 53491;

        readonly string testDirectory;

        readonly Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap;

        public string ProcessStoreDirectory => Path.Combine(testDirectory, "process-store");

        public Microsoft.AspNetCore.TestHost.TestServer BuildServer(
             Func<IFileStore, IFileStore> processStoreFileStoreMap = null)
        {
            var defaultFileStore = new FileStoreFromSystemIOFile(ProcessStoreDirectory);

            return
                new Microsoft.AspNetCore.TestHost.TestServer(
                    (webHostBuilderMap ?? (builder => builder))
                    (Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                    .UseUrls("http://*:19372")
                    .WithSettingPublicWebHostHttpPort(PublicWebHostHttpPort)
                    .UseStartup<StartupSupportingMigrations>()
                    .WithProcessStoreFileStore(processStoreFileStoreMap?.Invoke(defaultFileStore) ?? defaultFileStore)));
        }

        static public WebHostSupportingMigrationsTestSetup Setup(
            Func<DateTimeOffset> persistentProcessHostDateTime = null) =>
            Setup(
                builder => builder.WithSettingDateTimeOffsetDelegate(persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow)));

        static public WebHostSupportingMigrationsTestSetup Setup(
            Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap)
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var setup = new WebHostSupportingMigrationsTestSetup(testDirectory, webHostBuilderMap);

            return setup;
        }

        public System.Net.Http.HttpClient BuildPublicAppHttpClient()
        {
            return new System.Net.Http.HttpClient
            {
                BaseAddress = new Uri("http://localhost:" + PublicWebHostHttpPort),
            };
        }

        public void Dispose()
        {
            Directory.Delete(testDirectory, true);
        }

        WebHostSupportingMigrationsTestSetup(string testDirectory, Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap)
        {
            this.testDirectory = testDirectory;
            this.webHostBuilderMap = webHostBuilderMap;
        }
    }
}