using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace PersistentAppFromElmCode.Test
{
    [TestClass]
    public class TestContainer
    {
        static string PathToExampleElmApps => "./../../../../example-elm-apps";

        [TestMethod]
        public void Get_echo_from_elm_app()
        {
            var elmCodeFiles =
                Filesystem.GetAllFilesFromDirectory(Path.Combine(PathToExampleElmApps, "echo"))
                .Where(file => PersistentAppFromElmCode.Common.PersistentAppFromElm019Code.FilePathMatchesPatternOfFilesInElmApp(file.name))
                .ToList();

            using (var app = Common.PersistentAppFromElm019Code.WithCustomSerialization(
                elmCodeFiles: elmCodeFiles,
                pathToFileWithElmEntryPoint: "Echo.elm",
                pathToInitialStateFunction: "Echo.initState",
                pathToSerializedRequestFunction: "Echo.serializedRequest",
                pathToSerializeStateFunction: "Echo.serializeState",
                pathToDeserializeStateFunction: "Echo.deserializeState"))
            {
                var response = app.Request("Hello!");

                Assert.AreEqual("Echo from Elm:Hello!", response);
            }
        }

        static Common.IDisposableAppWithCustomSerialization BuildInstanceOfCounterApp() =>
            Common.PersistentAppFromElm019Code.WithCustomSerialization(
                elmCodeFiles:
                Filesystem.GetAllFilesFromDirectory(Path.Combine(PathToExampleElmApps, "counter"))
                .Where(file => PersistentAppFromElmCode.Common.PersistentAppFromElm019Code.FilePathMatchesPatternOfFilesInElmApp(file.name))
                .ToList(),
                pathToFileWithElmEntryPoint: "Counter.elm",
                pathToInitialStateFunction: "Counter.initState",
                pathToSerializedRequestFunction: "Counter.processSerializedEvent",
                pathToSerializeStateFunction: "Counter.serializeState",
                pathToDeserializeStateFunction: "Counter.deserializeState");

        static IEnumerable<(string serializedEvent, string expectedResponse)> CounterAppTestEventsAndExpectedResponses(
            IEnumerable<(int addition, int expectedResponse)> additionsAndExpectedResponses) =>
            additionsAndExpectedResponses
            .Select(additionAndExpectedResponse =>
                (Newtonsoft.Json.JsonConvert.SerializeObject(new { addition = additionAndExpectedResponse.addition }),
                additionAndExpectedResponse.expectedResponse.ToString()));

        [TestMethod]
        public void Restore_counter_app_state()
        {
            AssertAppRestoresStateWithSequenceOfEventsAndExpectedResponses(
                BuildInstanceOfCounterApp,
                CounterAppTestEventsAndExpectedResponses(
                    new(int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (1, 2),
                        (2, 4),
                        (-10, -6),
                    })
                );
        }

        [TestMethod]
        public void Method_to_test_state_restoration_throws_exception_on_unexpected_response()
        {
            var reasonableExpectation =
                new(int addition, int expectedResponse)[]
                {
                    (0, 0),
                    (1, 1),
                    (1, 2),
                    (2, 4),
                    (-10, -6),
                };

            AssertAppRestoresStateWithSequenceOfEventsAndExpectedResponses(
                BuildInstanceOfCounterApp,
                CounterAppTestEventsAndExpectedResponses(reasonableExpectation));

            var unreasonableExpectation =
                reasonableExpectation.Concat(new[] { (0, 12345678) });

            Assert.ThrowsException<AssertFailedException>(() =>
                AssertAppRestoresStateWithSequenceOfEventsAndExpectedResponses(
                    BuildInstanceOfCounterApp,
                    CounterAppTestEventsAndExpectedResponses(unreasonableExpectation)));
        }

        void AssertAppRestoresStateWithSequenceOfEventsAndExpectedResponses(
            Func<Common.IDisposableAppWithCustomSerialization> buildNewAppInstance,
            IEnumerable<(string serializedEvent, string expectedResponse)> eventsAndExpectedResponses)
        {
            using (var baseApp = buildNewAppInstance())
            {
                foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponses)
                {
                    var iterationInitialState = baseApp.GetSerializedState();

                    var baseAppResponse = baseApp.Request(serializedEvent);

                    Assert.AreEqual(expectedResponse.ToString(), baseAppResponse, false, "base app response");

                    using (var restoredApp = buildNewAppInstance())
                    {
                        restoredApp.SetSerializedState(iterationInitialState);

                        var restoredAppResponse = restoredApp.Request(serializedEvent);

                        Assert.AreEqual(expectedResponse.ToString(), restoredAppResponse, false, "restored app response");
                    }
                }
            }
        }
    }
}
