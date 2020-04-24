using Kalmit.PersistentProcess.WebHost;
using Microsoft.AspNetCore.Hosting;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.IO;
using System.Net.Http;
using System.Net.Http.Headers;

namespace Kalmit.PersistentProcess.Test
{
    public class WebHostSupportingMigrationsTestSetup : IDisposable
    {
        static string PublicWebHostUrl => "http://localhost:35491";

        static string AdminWebHostUrl => "http://localhost:19372";

        readonly string testDirectory;

        readonly string adminRootPassword;

        readonly Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap;

        public string ProcessStoreDirectory => Path.Combine(testDirectory, "process-store");

        public Microsoft.AspNetCore.TestHost.TestServer BuildServer(
             WebAppConfiguration setAppConfigAndInitElmState) =>
             BuildServer(
                 setAppConfigAndInitElmState:
                    setAppConfigAndInitElmState == null
                    ?
                    null
                    :
                    ZipArchive.ZipArchiveFromEntries(setAppConfigAndInitElmState.AsFiles()));

        public Microsoft.AspNetCore.TestHost.TestServer BuildServer(
             Func<IFileStore, IFileStore> processStoreFileStoreMap = null,
             byte[] setAppConfigAndInitElmState = null)
        {
            var defaultFileStore = new FileStoreFromSystemIOFile(ProcessStoreDirectory);

            var server =
                new Microsoft.AspNetCore.TestHost.TestServer(
                    (webHostBuilderMap ?? (builder => builder))
                    (Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                    .UseUrls(AdminWebHostUrl)
                    .WithSettingPublicWebHostUrls(new[] { PublicWebHostUrl })
                    .WithSettingAdminRootPassword(adminRootPassword)
                    .UseStartup<StartupSupportingMigrations>()
                    .WithProcessStoreFileStore(processStoreFileStoreMap?.Invoke(defaultFileStore) ?? defaultFileStore)));

            if (setAppConfigAndInitElmState != null)
            {
                using (var adminClient = SetDefaultRequestHeaderAuthorizeForAdminRoot(server.CreateClient()))
                {
                    var setAppConfigResponse = adminClient.PostAsync(
                        StartupSupportingMigrations.PathApiSetAppConfigAndInitElmState,
                        new ByteArrayContent(setAppConfigAndInitElmState)).Result;

                    Assert.IsTrue(
                        setAppConfigResponse.IsSuccessStatusCode,
                        "set-app response IsSuccessStatusCode (" + setAppConfigResponse.StatusCode + ")");
                }
            }

            return server;
        }

        static public WebHostSupportingMigrationsTestSetup Setup(
            Func<DateTimeOffset> persistentProcessHostDateTime = null) =>
            Setup(
                builder => builder.WithSettingDateTimeOffsetDelegate(persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow)));

        static public WebHostSupportingMigrationsTestSetup Setup(
            Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap = null,
            string adminRootPassword = null)
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var setup = new WebHostSupportingMigrationsTestSetup(
                testDirectory,
                adminRootPassword: adminRootPassword,
                webHostBuilderMap: webHostBuilderMap);

            return setup;
        }

        public System.Net.Http.HttpClient BuildPublicAppHttpClient()
        {
            return new System.Net.Http.HttpClient
            {
                BaseAddress = new Uri(PublicWebHostUrl),
            };
        }

        public System.Net.Http.HttpClient SetDefaultRequestHeaderAuthorizeForAdminRoot(System.Net.Http.HttpClient client)
        {
            if (adminRootPassword == null)
                return null;

            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue(
                "Basic",
                Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                    WebHost.Configuration.BasicAuthenticationForAdminRoot(adminRootPassword))));

            return client;
        }

        public void Dispose()
        {
            Directory.Delete(testDirectory, true);
        }

        WebHostSupportingMigrationsTestSetup(
            string testDirectory,
            string adminRootPassword,
            Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap)
        {
            this.testDirectory = testDirectory;
            this.adminRootPassword = adminRootPassword;
            this.webHostBuilderMap = webHostBuilderMap;
        }
    }
}