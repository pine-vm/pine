using Kalmit.PersistentProcess.WebHost;
using Microsoft.AspNetCore.Hosting;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;

namespace Kalmit.PersistentProcess.Test
{
    public class WebHostAdminInterfaceTestSetup : IDisposable
    {
        static string PublicWebHostUrl => "http://localhost:35491";

        static string AdminWebHostUrl => "http://localhost:19372";

        readonly string testDirectory;

        readonly string adminRootPassword;

        readonly byte[] setAppConfigAndInitElmState;

        readonly Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap;

        public string ProcessStoreDirectory => Path.Combine(testDirectory, "process-store");

        public Microsoft.AspNetCore.TestHost.TestServer BuildServer(
             Func<IFileStore, IFileStore> processStoreFileStoreMap = null)
        {
            var defaultFileStore = new FileStoreFromSystemIOFile(ProcessStoreDirectory);

            var server =
                new Microsoft.AspNetCore.TestHost.TestServer(
                    (webHostBuilderMap ?? (builder => builder))
                    (Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                    .UseUrls(AdminWebHostUrl)
                    .WithSettingPublicWebHostUrls(new[] { PublicWebHostUrl })
                    .WithSettingAdminRootPassword(adminRootPassword)
                    .UseStartup<StartupAdminInterface>()
                    .WithProcessStoreFileStore(processStoreFileStoreMap?.Invoke(defaultFileStore) ?? defaultFileStore)));

            return server;
        }

        static public WebHostAdminInterfaceTestSetup Setup(
            WebAppConfiguration setAppConfigAndInitElmState,
            Func<DateTimeOffset> persistentProcessHostDateTime = null,
            string adminRootPassword = null) =>
            Setup(
                persistentProcessHostDateTime: persistentProcessHostDateTime,
                adminRootPassword: adminRootPassword,
                setAppConfigAndInitElmState:
                    setAppConfigAndInitElmState == null
                    ?
                    null
                    :
                    ZipArchive.ZipArchiveFromEntries(setAppConfigAndInitElmState.AsFiles()));

        static public WebHostAdminInterfaceTestSetup Setup(
            Func<DateTimeOffset> persistentProcessHostDateTime = null,
            string adminRootPassword = null,
            byte[] setAppConfigAndInitElmState = null) =>
            Setup(
                adminRootPassword: adminRootPassword,
                setAppConfigAndInitElmState: setAppConfigAndInitElmState,
                webHostBuilderMap: builder => builder.WithSettingDateTimeOffsetDelegate(persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow)));

        static public WebHostAdminInterfaceTestSetup Setup(
            Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap,
            string adminRootPassword = null,
            byte[] setAppConfigAndInitElmState = null)
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var setup = new WebHostAdminInterfaceTestSetup(
                testDirectory,
                adminRootPassword: adminRootPassword,
                setAppConfigAndInitElmState: setAppConfigAndInitElmState,
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
                return client;

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

        WebHostAdminInterfaceTestSetup(
            string testDirectory,
            string adminRootPassword,
            byte[] setAppConfigAndInitElmState,
            Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap)
        {
            this.testDirectory = testDirectory;
            this.adminRootPassword = adminRootPassword ?? "notempty";
            this.setAppConfigAndInitElmState = setAppConfigAndInitElmState;
            this.webHostBuilderMap = webHostBuilderMap;

            if (setAppConfigAndInitElmState != null)
            {
                using (var server = BuildServer())
                {
                    using (var adminClient = SetDefaultRequestHeaderAuthorizeForAdminRoot(server.CreateClient()))
                    {
                        var deployAppConfigResponse = adminClient.PostAsync(
                            StartupAdminInterface.PathApiDeployAppConfigAndInitElmAppState,
                            new ByteArrayContent(setAppConfigAndInitElmState)).Result;

                        Assert.IsTrue(
                            deployAppConfigResponse.IsSuccessStatusCode,
                            "Deploy response IsSuccessStatusCode (" + deployAppConfigResponse.StatusCode + ")");
                    }
                }
            }
        }

        public WebHost.ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore BuildProcessStoreReaderInFileDirectory() =>
            new WebHost.ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore(
                new FileStoreFromSystemIOFile(ProcessStoreDirectory));

        public IEnumerable<PersistentProcess.InterfaceToHost.Event> EnumerateStoredUpdateElmAppStateForEvents()
        {
            var processStoreReader = BuildProcessStoreReaderInFileDirectory();

            PersistentProcess.InterfaceToHost.Event eventFromHash(string eventComponentHash)
            {
                var component = processStoreReader.LoadComponent(eventComponentHash);

                if (component == null)
                    throw new Exception("component == null");

                if (component.BlobContent == null)
                    throw new Exception("component.BlobContent == null");

                var eventString = Encoding.UTF8.GetString(component.BlobContent.ToArray());

                return Newtonsoft.Json.JsonConvert.DeserializeObject<PersistentProcess.InterfaceToHost.Event>(eventString);
            }

            return
                BuildProcessStoreReaderInFileDirectory()
                .EnumerateSerializedCompositionLogRecordsReverse()
                .Select(Encoding.UTF8.GetString)
                .Select(JsonConvert.DeserializeObject<Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile>)
                .SelectMany(compositionLogRecord =>
                    compositionLogRecord.events.Reverse()
                    .Select(record => record.UpdateElmAppStateForEvent)
                    .WhereNotNull())
                .Select(updateElmAppStateForEvent => eventFromHash(updateElmAppStateForEvent.HashBase16));
        }
    }
}