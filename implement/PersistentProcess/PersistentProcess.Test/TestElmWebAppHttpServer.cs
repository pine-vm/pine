using Microsoft.VisualStudio.TestTools.UnitTesting;
using MoreLinq;
using System.Collections.Immutable;
using System.Linq;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestElmWebAppHttpServer
    {
        static public Composition.Component AppConfigComponentFromElmAppFiles(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> elmAppFiles) =>
            Composition.FromTree(Composition.TreeFromSetOfBlobsWithStringPath(AppConfigFilesFromElmAppFiles(elmAppFiles)));

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> AppConfigFilesFromElmAppFiles(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> elmAppFiles)
        {
            var dict = ImmutableDictionary<IImmutableList<string>, IImmutableList<byte>>.Empty;

            foreach (var elmAppFile in elmAppFiles)
                dict = dict.SetItem(
                    elmAppFile.Key.Insert(0, WebHost.BuildConfigurationFromArguments.ElmAppSubdirectoryName), elmAppFile.Value);

            return dict;
        }

        static public Composition.Component CounterWebApp =>
            AppConfigComponentFromElmAppFiles(TestSetup.CounterElmWebApp);

        static public Composition.Component StringBuilderWebApp =>
            AppConfigComponentFromElmAppFiles(TestSetup.StringBuilderElmWebApp);

        static public Composition.Component CrossPropagateHttpHeadersToAndFromBody =>
            AppConfigComponentFromElmAppFiles(TestSetup.CrossPropagateHttpHeadersToAndFromBodyElmWebApp);

        static public Composition.Component HttpProxyWebApp =>
            AppConfigComponentFromElmAppFiles(TestSetup.HttpProxyWebApp);

        [TestMethod]
        public void Restore_counter_http_web_app_on_server_restart()
        {
            using (var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppConfigAndInitElmState: CounterWebApp))
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
                    using (var server = testSetup.StartWebHost())
                    {
                        foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
                        {
                            using (var client = testSetup.BuildPublicAppHttpClient())
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
