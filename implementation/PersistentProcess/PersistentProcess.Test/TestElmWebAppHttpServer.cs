using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Kalmit.PersistentProcess.WebHost;
using MoreLinq;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestElmWebAppHttpServer
    {
        static public WebAppConfiguration CounterWebApp =>
            new WebAppConfiguration()
            .WithElmApp(ZipArchive.ZipArchiveFromEntries(TestSetup.CounterElmWebApp.AsFiles()));

        static public WebAppConfiguration StringBuilderWebApp =>
            new WebAppConfiguration()
            .WithElmApp(ZipArchive.ZipArchiveFromEntries(TestSetup.StringBuilderElmWebApp.AsFiles()));

        [TestMethod]
        public void Restore_counter_http_web_app_on_server_restart()
        {
            using (var testSetup = WebHostTestSetup.Setup(CounterWebApp))
            {
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

                var eventsAndExpectedResponsesBatches = eventsAndExpectedResponses.Batch(3).ToList();

                Assert.IsTrue(2 < eventsAndExpectedResponsesBatches.Count, "More than two batches of events to test with.");

                foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
                {
                    using (var server = testSetup.BuildServer())
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
            }
        }
    }
}
