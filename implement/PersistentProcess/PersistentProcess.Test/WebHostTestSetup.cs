using System.IO;
using System.Linq;
using Kalmit.PersistentProcess.WebHost;
using System.Collections.Generic;
using System;
using Newtonsoft.Json;
using Kalmit.ProcessStore;
using System.Text;
using Microsoft.AspNetCore.Hosting;

namespace Kalmit.PersistentProcess.Test
{
    public class WebHostTestSetup : IDisposable
    {
        readonly string testDirectory;

        readonly Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap;

        string WebAppConfigFilePath => Path.Combine(testDirectory, "web-app");

        string ProcessStoreDirectory => Path.Combine(testDirectory, "process-store");

        public Microsoft.AspNetCore.TestHost.TestServer BuildServer() =>
            new Microsoft.AspNetCore.TestHost.TestServer(
                (webHostBuilderMap ?? (builder => builder))
                (Kalmit.PersistentProcess.WebHost.Program.CreateWebHostBuilder(null)
                .WithSettingProcessStoreDirectoryPath(ProcessStoreDirectory)
                .WithSettingWebAppConfigurationFilePath(WebAppConfigFilePath)));

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

            var setup = new WebHostTestSetup(testDirectory, webHostBuilderMap);

            var webAppConfigFilePath = setup.WebAppConfigFilePath;

            Directory.CreateDirectory(Path.GetDirectoryName(webAppConfigFilePath));

            File.WriteAllBytes(webAppConfigFilePath, ZipArchive.ZipArchiveFromEntries(webAppConfig.AsFiles()));

            return setup;
        }

        public void Dispose()
        {
            Directory.Delete(testDirectory, true);
        }

        WebHostTestSetup(string testDirectory, Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap)
        {
            this.testDirectory = testDirectory;
            this.webHostBuilderMap = webHostBuilderMap;
        }

        public ProcessStore.ProcessStoreInFileDirectory BuildProcessStoreInFileDirectory() =>
            new ProcessStore.ProcessStoreInFileDirectory(ProcessStoreDirectory, null);

        public IEnumerable<PersistentProcess.InterfaceToHost.Event> EnumerateStoredProcessEventsReverse() =>
            BuildProcessStoreInFileDirectory()
            .EnumerateSerializedCompositionsRecordsReverse()
            .Select(Encoding.UTF8.GetString)
            .Select(JsonConvert.DeserializeObject<CompositionRecordInFile>)
            .SelectMany(compositionRecord => compositionRecord.AppendedEvents.Reverse().Select(record => record.LiteralString))
            .Select(JsonConvert.DeserializeObject<PersistentProcess.InterfaceToHost.Event>);
    }
}