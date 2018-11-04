using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Kalmit.PersistentProcess.WebHost;
using MoreLinq;
using System.Collections.Generic;
using System;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestWebHost
    {
        [TestMethod]
        public void Web_host_stores_reduction_every_hour()
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var processStoreDirectory = Path.Combine(testDirectory, "process-store");

            var elmAppFilePath = Path.Combine(testDirectory, "elm-app");
            File.WriteAllBytes(elmAppFilePath, TestSetup.CounterElmWebAppFile);

            var allEventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (3, 4),
                        (5, 9),
                        (7, 16),
                        (11, 27),
                        (-13, 14),
                    }).ToList();

            var persistentProcessHostDateTime = new DateTimeOffset(2018, 11, 3, 19, 4, 13, TimeSpan.Zero);

            void letTimePassInPersistentProcessHost(TimeSpan amount) =>
                persistentProcessHostDateTime = persistentProcessHostDateTime + amount;

            Microsoft.AspNetCore.TestHost.TestServer buildServer() =>
                new Microsoft.AspNetCore.TestHost.TestServer(
                    Kalmit.PersistentProcess.WebHost.Program.CreateWebHostBuilder(null)
                    .WithSettingProcessStoreDirectoryPath(processStoreDirectory)
                    .WithSettingElmAppFilePath(elmAppFilePath)
                    .WithSettingDateTimeOffsetDelegate(() => persistentProcessHostDateTime));

            var eventsAndExpectedResponsesBatches = allEventsAndExpectedResponses.Batch(3).ToList();

            Assert.IsTrue(2 < eventsAndExpectedResponsesBatches.Count, "More than two batches of events to test with.");

            IEnumerable<string> ReadStoredReductionFileRelativePaths()
            {
                System.Threading.Thread.Sleep(1111);  //  Storing reduction may be completed after client has received response.
                return new ProcessStore.ProcessStoreInFileDirectory(processStoreDirectory, null).ReductionsFilesNames();
            }

            using (var server = buildServer())
            {
                //  Do not depend on an exact number of reductions stored on initialization: Send one request before measuring the number of stored reductions.
                using (var client = server.CreateClient())
                {
                    var httpResponse =
                        client.PostAsync("", new System.Net.Http.StringContent("", System.Text.Encoding.UTF8)).Result;

                    var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;
                }

                foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
                {
                    var beforeBatchStoredReductionsCount = ReadStoredReductionFileRelativePaths().Count();

                    letTimePassInPersistentProcessHost(TimeSpan.FromHours(1));

                    foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
                    {
                        letTimePassInPersistentProcessHost(TimeSpan.FromSeconds(4));

                        using (var client = server.CreateClient())
                        {
                            var httpResponse =
                                client.PostAsync("", new System.Net.Http.StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                            var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                        }

                        Assert.AreEqual(
                            beforeBatchStoredReductionsCount + 1,
                            ReadStoredReductionFileRelativePaths().Count(),
                            "Number of stored reductions has increased by one since previous batch.");
                    }
                }
            }

            Directory.Delete(testDirectory, true);
        }

        [TestMethod]
        public void Web_host_continues_composition_log_on_new_file_every_calendar_day()
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var processStoreDirectory = Path.Combine(testDirectory, "process-store");

            var elmAppFilePath = Path.Combine(testDirectory, "elm-app");
            File.WriteAllBytes(elmAppFilePath, TestSetup.CounterElmWebAppFile);

            var allEventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (3, 4),
                        (5, 9),
                        (7, 16),
                        (11, 27),
                        (-13, 14),
                    }).ToList();

            var persistentProcessHostDateTime = new DateTimeOffset(2018, 11, 4, 8, 17, 13, TimeSpan.Zero);

            void letTimePassInPersistentProcessHost(TimeSpan amount) =>
                persistentProcessHostDateTime = persistentProcessHostDateTime + amount;

            Microsoft.AspNetCore.TestHost.TestServer buildServer() =>
                new Microsoft.AspNetCore.TestHost.TestServer(
                    Kalmit.PersistentProcess.WebHost.Program.CreateWebHostBuilder(null)
                    .WithSettingProcessStoreDirectoryPath(processStoreDirectory)
                    .WithSettingElmAppFilePath(elmAppFilePath)
                    .WithSettingDateTimeOffsetDelegate(() => persistentProcessHostDateTime));

            var eventsAndExpectedResponsesBatches = allEventsAndExpectedResponses.Batch(3).ToList();

            Assert.IsTrue(2 < eventsAndExpectedResponsesBatches.Count, "More than two batches of events to test with.");

            ProcessStore.ProcessStoreInFileDirectory BuildProcessStore() =>
                new ProcessStore.ProcessStoreInFileDirectory(processStoreDirectory, null);

            IEnumerable<string> ReadCompositionLogFilesNames() =>
                BuildProcessStore().EnumerateCompositionsLogFilesNames();

            string FirstReductionRecordFile = null;

            void deleteAllReductionRecordFilesExceptTheFirstOne()
            {
                var filesNamesExceptTheFirst =
                    BuildProcessStore().ReductionsFilesNames().Except(new[] { FirstReductionRecordFile }).ToList();

                filesNamesExceptTheFirst.ForEach(File.Delete);

                Assert.IsTrue(BuildProcessStore().ReductionsFilesNames().Count() <= 1);
            }

            string PostStringContentAndGetResponseStringFromServer(
                Microsoft.AspNetCore.TestHost.TestServer server, string requestContent)
            {
                try
                {
                    using (var client = server.CreateClient())
                    {
                        var httpResponse =
                            client.PostAsync("", new System.Net.Http.StringContent(requestContent, System.Text.Encoding.UTF8)).Result;

                        return httpResponse.Content.ReadAsStringAsync().Result;
                    }
                }
                finally
                {
                    FirstReductionRecordFile =
                        FirstReductionRecordFile ?? BuildProcessStore().ReductionsFilesNames().FirstOrDefault();
                }
            }

            using (var server = buildServer())
            {
                //  Do not depend on an exact number of files stored on initialization: Send one request before measuring the number of stored files.
                PostStringContentAndGetResponseStringFromServer(server, "");
            }

            foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
            {
                var beforeBatchCompositionLogFiles = ReadCompositionLogFilesNames().ToList();

                letTimePassInPersistentProcessHost(TimeSpan.FromDays(1));

                var batchDayCompositionLogFileNameExpectedStart =
                    persistentProcessHostDateTime.ToString("yyyy-MM-dd");

                //  Test that restoring works over all composition log files.
                deleteAllReductionRecordFilesExceptTheFirstOne();

                using (var server = buildServer())
                {
                    foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
                    {
                        letTimePassInPersistentProcessHost(TimeSpan.FromMinutes(4));

                        var httpResponseContent = PostStringContentAndGetResponseStringFromServer(server, serializedEvent);

                        Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");

                        var inBatchNewCompositionLogFiles =
                            ReadCompositionLogFilesNames().Except(beforeBatchCompositionLogFiles).ToList();

                        Assert.AreEqual(
                            1,
                            inBatchNewCompositionLogFiles.Count,
                            "Exactly one new composition log files since start of batch.");

                        var newCompositionLogFileName = Path.GetFileName(inBatchNewCompositionLogFiles.Single());

                        Assert.IsTrue(
                            newCompositionLogFileName.StartsWith(batchDayCompositionLogFileNameExpectedStart),
                            "File name of new composition log file ('" + newCompositionLogFileName +
                            "') starts with '" + batchDayCompositionLogFileNameExpectedStart + "')");
                    }
                }
            }

            Directory.Delete(testDirectory, true);
        }
    }
}
