using Kalmit.PersistentProcess.WebHost;
using Kalmit.ProcessStore;
using Microsoft.AspNetCore.Hosting;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace Kalmit.PersistentProcess.Test
{
    public class WebHostTestSetup : IDisposable
    {
        readonly string testDirectory;

        readonly Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap;

        readonly WebAppConfiguration webAppConfig;

        public string ProcessStoreDirectory => Path.Combine(testDirectory, "process-store");

        public Microsoft.AspNetCore.TestHost.TestServer BuildServer(
             Func<IFileStore, IFileStore> processStoreFileStoreMap = null)
        {
            var defaultFileStore = new FileStoreFromSystemIOFile(ProcessStoreDirectory);

            return
                new Microsoft.AspNetCore.TestHost.TestServer(
                    (webHostBuilderMap ?? (builder => builder))
                    (Kalmit.PersistentProcess.WebHost.Program.CreateWebHostBuilder(null)
                    .WithProcessStoreFileStore(processStoreFileStoreMap?.Invoke(defaultFileStore) ?? defaultFileStore)
                    .WithWebAppConfigurationZipArchive(ZipArchive.ZipArchiveFromEntries(webAppConfig.AsFiles()))));
        }

        static public WebHostTestSetup Setup(
            WebAppConfiguration webAppConfig,
            Func<DateTimeOffset> persistentProcessHostDateTime = null) =>
            Setup(
                webAppConfig,
                builder => builder.WithSettingDateTimeOffsetDelegate(persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow)));

        static public WebHostTestSetup Setup(
            WebAppConfiguration webAppConfig,
            Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap)
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var setup = new WebHostTestSetup(testDirectory, webAppConfig, webHostBuilderMap);

            return setup;
        }

        public void Dispose()
        {
            Directory.Delete(testDirectory, true);
        }

        WebHostTestSetup(string testDirectory, WebAppConfiguration webAppConfig, Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap)
        {
            this.webAppConfig = webAppConfig;
            this.testDirectory = testDirectory;
            this.webHostBuilderMap = webHostBuilderMap;
        }

        public ProcessStore.ProcessStoreReaderInFileStore BuildProcessStoreReaderInFileDirectory() =>
            new ProcessStore.ProcessStoreReaderInFileStore(new FileStoreFromSystemIOFile(ProcessStoreDirectory));

        public IEnumerable<PersistentProcess.InterfaceToHost.Event> EnumerateStoredProcessEventsReverse() =>
            BuildProcessStoreReaderInFileDirectory()
            .EnumerateSerializedCompositionsRecordsReverse()
            .Select(Encoding.UTF8.GetString)
            .Select(JsonConvert.DeserializeObject<CompositionRecordInFile>)
            .SelectMany(compositionRecord => compositionRecord.AppendedEvents.Reverse().Select(record => record.LiteralString))
            .Select(JsonConvert.DeserializeObject<PersistentProcess.InterfaceToHost.Event>);
    }
}