using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MoreLinq;
using Pine;

namespace test_elm_fullstack
{
    [TestClass]
    public class TestElmWebAppHttpServer
    {
        static public Composition.Component CounterWebApp =>
            TestSetup.AppConfigComponentFromFiles(TestSetup.CounterElmWebApp);

        static public Composition.Component StringBuilderWebApp =>
            TestSetup.AppConfigComponentFromFiles(TestSetup.StringBuilderElmWebApp);

        static public Composition.Component CrossPropagateHttpHeadersToAndFromBody =>
            TestSetup.AppConfigComponentFromFiles(TestSetup.CrossPropagateHttpHeadersToAndFromBodyElmWebApp);

        static public Composition.Component HttpProxyWebApp =>
            TestSetup.AppConfigComponentFromFiles(TestSetup.HttpProxyWebApp);

        [TestMethod]
        public void Restore_counter_http_web_app_on_server_restart()
        {
            using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: CounterWebApp);

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
                using var server = testSetup.StartWebHost();

                foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
                {
                    using var client = testSetup.BuildPublicAppHttpClient();

                    var httpResponse =
                        client.PostAsync("", new System.Net.Http.StringContent(serializedEvent, System.Text.Encoding.UTF8)).Result;

                    var httpResponseContent = httpResponse.Content.ReadAsStringAsync().Result;

                    Assert.AreEqual(expectedResponse, httpResponseContent, false, "server response");
                }
            }
        }
    }
}
