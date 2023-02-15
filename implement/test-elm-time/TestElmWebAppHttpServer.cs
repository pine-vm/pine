using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MoreLinq;
using Pine;

namespace TestElmTime;

[TestClass]
public class TestElmWebAppHttpServer
{
    static public PineValue CounterWebApp =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.CounterElmWebApp);

    static public PineValue StringBuilderWebApp =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.StringBuilderElmWebApp);

    static public PineValue CrossPropagateHttpHeadersToAndFromBody =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.CrossPropagateHttpHeadersToAndFromBodyElmWebApp);

    static public PineValue HttpProxyWebApp =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.HttpProxyWebApp);

    [TestMethod]
    public void Restore_counter_http_web_app_on_server_restart()
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

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: CounterWebApp);

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
