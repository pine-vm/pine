using Kalmit.PersistentProcess.WebHost;
using Microsoft.AspNetCore.Hosting;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;

namespace Kalmit.PersistentProcess.Test
{
    public class WebHostAdminInterfaceTestSetup : IDisposable
    {
        static string PublicWebHostUrlDefault => "http://localhost:35491";

        static string AdminWebHostUrlDefault => "http://localhost:19372";

        readonly string publicWebHostUrlOverride;

        readonly string adminWebHostUrlOverride;

        public string PublicWebHostUrl => publicWebHostUrlOverride ?? PublicWebHostUrlDefault;

        public string AdminWebHostUrl => adminWebHostUrlOverride ?? AdminWebHostUrlDefault;

        readonly string testDirectory;

        readonly string adminRootPassword;

        readonly Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap;

        public string ProcessStoreDirectory => Path.Combine(testDirectory, "process-store");

        IFileStore defaultFileStore => new FileStoreFromSystemIOFile(ProcessStoreDirectory);

        public IWebHost StartWebHost(
             Func<IFileStore, IFileStore> processStoreFileStoreMap = null)
        {
            var webHost =
                (webHostBuilderMap ?? (builder => builder))
                (Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                .UseUrls(AdminWebHostUrl)
                .WithSettingPublicWebHostUrls(new[] { PublicWebHostUrl })
                .WithSettingAdminRootPassword(adminRootPassword)
                .UseStartup<StartupAdminInterface>()
                .WithProcessStoreFileStore(processStoreFileStoreMap?.Invoke(defaultFileStore) ?? defaultFileStore))
                .Build();

            webHost?.StartAsync().Wait();

            return webHost;
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
                    Composition.FromTree(Composition.TreeFromSetOfBlobsWithStringPath(setAppConfigAndInitElmState.AsFiles())));

        static public WebHostAdminInterfaceTestSetup Setup(
            Func<DateTimeOffset> persistentProcessHostDateTime = null,
            string adminRootPassword = null,
            Composition.Component setAppConfigAndInitElmState = null) =>
            Setup(
                adminRootPassword: adminRootPassword,
                setAppConfigAndInitElmState: setAppConfigAndInitElmState,
                webHostBuilderMap: builder => builder.WithSettingDateTimeOffsetDelegate(persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow)));

        static public WebHostAdminInterfaceTestSetup Setup(
            Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap,
            string adminRootPassword = null,
            Composition.Component setAppConfigAndInitElmState = null,
            string adminWebHostUrlOverride = null,
            string publicWebHostUrlOverride = null)
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var setup = new WebHostAdminInterfaceTestSetup(
                testDirectory,
                adminRootPassword: adminRootPassword,
                setAppConfigAndInitElmState: setAppConfigAndInitElmState,
                webHostBuilderMap: webHostBuilderMap,
                adminWebHostUrlOverride: adminWebHostUrlOverride,
                publicWebHostUrlOverride: publicWebHostUrlOverride);

            return setup;
        }

        public System.Net.Http.HttpClient BuildPublicAppHttpClient()
        {
            return new System.Net.Http.HttpClient
            {
                BaseAddress = new Uri(PublicWebHostUrl),
            };
        }

        public System.Net.Http.HttpClient BuildAdminInterfaceHttpClient()
        {
            return new System.Net.Http.HttpClient
            {
                BaseAddress = new Uri(AdminWebHostUrl),
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
            Composition.Component setAppConfigAndInitElmState,
            Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap,
            string adminWebHostUrlOverride,
            string publicWebHostUrlOverride)
        {
            this.testDirectory = testDirectory;
            this.adminRootPassword = adminRootPassword ?? "notempty";
            this.webHostBuilderMap = webHostBuilderMap;
            this.adminWebHostUrlOverride = adminWebHostUrlOverride;
            this.publicWebHostUrlOverride = publicWebHostUrlOverride;

            if (setAppConfigAndInitElmState != null)
            {
                var compositionLogEvent =
                    new WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent
                    {
                        DeployAppConfigAndInitElmAppState =
                            new WebHost.ProcessStoreSupportingMigrations.ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(setAppConfigAndInitElmState))
                            }
                    };

                var processStoreWriter =
                    new WebHost.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                    defaultFileStore);

                processStoreWriter.StoreComponent(setAppConfigAndInitElmState);

                var compositionRecord = new WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile
                {
                    parentHashBase16 = WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.compositionLogFirstRecordParentHashBase16,
                    events = ImmutableList.Create(compositionLogEvent)
                };

                var serializedCompositionLogRecord =
                    WebHost.ProcessStoreSupportingMigrations.ProcessStoreInFileStore.Serialize(compositionRecord);

                processStoreWriter.SetCompositionLogHeadRecord(serializedCompositionLogRecord);
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