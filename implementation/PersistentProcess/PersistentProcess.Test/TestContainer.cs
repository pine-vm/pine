using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Kalmit.ProcessStore;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestContainer
    {
        static string PathToExampleElmApps => "./../../../../example-elm-apps";

        [TestMethod]
        public void Get_echo_from_elm_process()
        {
            var elmApp = GetElmAppWithEntryConfigFromExampleName("echo");

            using (var process = Kalmit.ProcessFromElm019Code.WithCustomSerialization(
                elmCodeFiles: elmApp.ElmAppFiles,
                entryConfig: elmApp.EntryConfig.Value.WithCustomSerialization.Value))
            {
                var response = process.ProcessEvent("Hello!");

                Assert.AreEqual("Echo from Elm:Hello!", response);
            }
        }

        static IEnumerable<(string serializedEvent, string expectedResponse)> CounterProcessTestEventsAndExpectedResponses(
            IEnumerable<(int addition, int expectedResponse)> additionsAndExpectedResponses) =>
            additionsAndExpectedResponses
            .Select(additionAndExpectedResponse =>
                (Newtonsoft.Json.JsonConvert.SerializeObject(new { addition = additionAndExpectedResponse.addition }),
                additionAndExpectedResponse.expectedResponse.ToString()));

        [TestMethod]
        public void Restore_counter_process_state()
        {
            AssertProcessRestoresStateWithSequenceOfEventsAndExpectedResponses(
                BuildInstanceOfCounterProcess,
                CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
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
                new (int addition, int expectedResponse)[]
                {
                    (0, 0),
                    (1, 1),
                    (1, 2),
                    (2, 4),
                    (-10, -6),
                };

            AssertProcessRestoresStateWithSequenceOfEventsAndExpectedResponses(
                BuildInstanceOfCounterProcess,
                CounterProcessTestEventsAndExpectedResponses(reasonableExpectation));

            var unreasonableExpectation =
                reasonableExpectation.Concat(new[] { (0, 12345678) });

            Assert.ThrowsException<AssertFailedException>(() =>
                AssertProcessRestoresStateWithSequenceOfEventsAndExpectedResponses(
                    BuildInstanceOfCounterProcess,
                    CounterProcessTestEventsAndExpectedResponses(unreasonableExpectation)));
        }

        [TestMethod]
        public void Restore_counter_process_state_over_file()
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var processStoreDirectory = Path.Combine(testDirectory, "process-store");

            var eventsAndExpectedResponses =
                CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (1, 2),
                        (2, 4),
                        (-10, -6),
                    }).ToList();

            IDisposableProcessWithCustomSerialization InstantiatePersistentProcess()
            {
                var store = new ProcessStore.ProcessStoreInFileDirectory(processStoreDirectory);

                return new PersistentProcessWithControlFlowOverStoreWriter(
                    new PersistentProcessWithHistoryOnFileFromElm019Code(store, CounterElmAppFile), store);
            }

            foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponses)
            {
                using (var processInstance = InstantiatePersistentProcess())
                {
                    var processResponse = processInstance.ProcessEvent(serializedEvent);

                    Assert.AreEqual(expectedResponse, processResponse, false, "process response");
                }
            }

            Directory.Delete(testDirectory, true);
        }

        [TestMethod]
        public void Restore_counter_process_state_over_compositions()
        {
            var eventsAndExpectedResponses =
                CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (1, 2),
                        (2, 4),
                        (-10, -6),
                    }).ToList();

            var processStoreCompositions = new List<byte[]>();

            PersistentProcessWithHistoryOnFileFromElm019Code InstantiatePersistentProcess()
            {
                var storeReader = new ProcessStoreReaderFromDelegates
                {
                    EnumerateSerializedCompositionsRecordsReverseDelegate =
                        processStoreCompositions.AsEnumerable().Reverse,

                    GetReductionDelegate = hash => null,
                };

                return new PersistentProcessWithHistoryOnFileFromElm019Code(storeReader, CounterElmAppFile);
            }

            foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponses)
            {
                using (var processInstance = InstantiatePersistentProcess())
                {
                    var (processResponses, compositionRecord) = processInstance.ProcessEvents(new[] { serializedEvent });

                    var processResponse = processResponses.Single();

                    processStoreCompositions.Add(compositionRecord.serializedCompositionRecord);

                    Assert.AreEqual(expectedResponse, processResponse, false, "process response");
                }
            }
        }

        class ProcessStoreReaderFromDelegates : IProcessStoreReader
        {
            public Func<IEnumerable<byte[]>> EnumerateSerializedCompositionsRecordsReverseDelegate;

            public Func<byte[], ReductionRecord> GetReductionDelegate;

            public IEnumerable<byte[]> EnumerateSerializedCompositionsRecordsReverse() =>
                EnumerateSerializedCompositionsRecordsReverseDelegate();

            public ReductionRecord GetReduction(byte[] reducedCompositionHash) =>
                GetReductionDelegate(reducedCompositionHash);
        }

        static Kalmit.IDisposableProcessWithCustomSerialization BuildInstanceOfCounterProcess() =>
            Kalmit.ProcessFromElm019Code.WithCustomSerialization(
                CounterElmApp.ElmAppFiles,
                CounterElmApp.EntryConfig.Value.WithCustomSerialization.Value);

        static ElmAppWithEntryConfig CounterElmApp =
            GetElmAppWithEntryConfigFromExampleName("counter");

        static byte[] CounterElmAppFile => ZipArchive.ZipArchiveFromEntries(CounterElmApp.AsFiles());

        static ElmAppWithEntryConfig GetElmAppWithEntryConfigFromExampleName(string exampleName) =>
            ElmAppWithEntryConfig.FromFiles(
                Filesystem.GetAllFilesFromDirectory(Path.Combine(PathToExampleElmApps, exampleName)));

        void AssertProcessRestoresStateWithSequenceOfEventsAndExpectedResponses(
            Func<Kalmit.IDisposableProcessWithCustomSerialization> buildNewProcessInstance,
            IEnumerable<(string serializedEvent, string expectedResponse)> eventsAndExpectedResponses)
        {
            using (var baseProcess = buildNewProcessInstance())
            {
                foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponses)
                {
                    var iterationInitialState = baseProcess.GetSerializedState();

                    var baseProcessResponse = baseProcess.ProcessEvent(serializedEvent);

                    Assert.AreEqual(expectedResponse.ToString(), baseProcessResponse, false, "primary process response");

                    using (var restoredProcess = buildNewProcessInstance())
                    {
                        restoredProcess.SetSerializedState(iterationInitialState);

                        var restoredProcessResponse = restoredProcess.ProcessEvent(serializedEvent);

                        Assert.AreEqual(expectedResponse.ToString(), restoredProcessResponse, false, "restored process response");
                    }
                }
            }
        }
    }
}
