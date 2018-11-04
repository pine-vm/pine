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
                return new ProcessStore.ProcessStoreInFileDirectory(processStoreDirectory).ReductionsFilePaths();
            }

            using (var server = buildServer())
            {
                //  Do not depend on exact number of reductions stored on initialization: Send one request before measuring number of stored reductions.
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
    }
}
