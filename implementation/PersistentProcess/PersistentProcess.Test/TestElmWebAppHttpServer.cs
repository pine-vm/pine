using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Kalmit.PersistentProcess.WebHost;
using MoreLinq;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestElmWebAppHttpServer
    {
        [TestMethod]
        public void Restore_counter_http_web_app_on_server_restart()
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var processStoreDirectory = Path.Combine(testDirectory, "process-store");

            var elmAppFilePath = Path.Combine(testDirectory, "elm-app");
            File.WriteAllBytes(elmAppFilePath, TestSetup.CounterElmWebAppFile);

            var eventsAndExpectedResponses =
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

            Microsoft.AspNetCore.TestHost.TestServer buildServer() =>
                new Microsoft.AspNetCore.TestHost.TestServer(
                    Kalmit.PersistentProcess.WebHost.Program.CreateWebHostBuilder(null)
                    .WithSettingProcessStoreDirectoryPath(processStoreDirectory)
                    .WithSettingElmAppFilePath(elmAppFilePath));

            var eventsAndExpectedResponsesBatches = eventsAndExpectedResponses.Batch(3).ToList();

            Assert.IsTrue(2 < eventsAndExpectedResponsesBatches.Count, "More than two batches of events to test with.");

            foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
            {
                using (var server = buildServer())
                {
                    foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
                    {
                        using (var client = server.CreateClient())
                        {
                            var httpResponse =
                                client.PostAsync("", new System.Net.Http.StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                            var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                            Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                        }
                    }
                }
            }

            Directory.Delete(testDirectory, true);
        }
    }
}
