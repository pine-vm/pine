using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using FluentAssertions;
using Kalmit.ProcessStore;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using MoreLinq;

namespace Kalmit.PersistentProcess.Test
{
    [TestClass]
    public class TestPersistentProcess
    {
        [TestMethod]
        public void Get_echo_from_elm_process()
        {
            var elmApp = TestSetup.GetElmAppWithEntryConfigFromExampleName("echo");

            using (var process = Kalmit.ProcessFromElm019Code.WithCustomSerialization(
                elmCodeFiles: elmApp.ElmAppFiles,
                entryConfig: elmApp.EntryConfig.Value.WithCustomSerialization.Value))
            {
                var response = process.ProcessEvent("Hello!");

                Assert.AreEqual("Echo from Elm:Hello!", response);
            }
        }

        [TestMethod]
        public void Restore_counter_process_state()
        {
            AssertProcessRestoresStateWithSequenceOfEventsAndExpectedResponses(
                TestSetup.BuildInstanceOfCounterProcess,
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
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
                TestSetup.BuildInstanceOfCounterProcess,
                TestSetup.CounterProcessTestEventsAndExpectedResponses(reasonableExpectation));

            var unreasonableExpectation =
                reasonableExpectation.Concat(new[] { (0, 12345678) });

            Assert.ThrowsException<AssertFailedException>(() =>
                AssertProcessRestoresStateWithSequenceOfEventsAndExpectedResponses(
                    TestSetup.BuildInstanceOfCounterProcess,
                    TestSetup.CounterProcessTestEventsAndExpectedResponses(unreasonableExpectation)));
        }

        [TestMethod]
        public void Restore_counter_process_state_over_file()
        {
            var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var processStoreDirectory = Path.Combine(testDirectory, "process-store");

            var eventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
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
                var store = new ProcessStore.ProcessStoreInFileDirectory(processStoreDirectory, null);

                return new PersistentProcessWithControlFlowOverStoreWriter(
                    new PersistentProcessWithHistoryOnFileFromElm019Code(store, TestSetup.CounterElmAppFile), store);
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
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
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

                return new PersistentProcessWithHistoryOnFileFromElm019Code(storeReader, TestSetup.CounterElmAppFile);
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

        [TestMethod]
        public void Restore_counter_process_state_from_combination_of_reduction_and_compositions()
        {
            var eventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(
                    new (int addition, int expectedResponse)[]
                    {
                        (0, 0),
                        (1, 1),
                        (1, 2),
                        (2, 4),
                        (3, 7),
                        (4, 11),
                        (5, 16),
                        (-10, 6),
                    }).ToList();

            var processStoreCompositions = new List<byte[]>();

            ReductionRecord reductionRecordAvailableFromStore = null;

            PersistentProcessWithHistoryOnFileFromElm019Code InstantiatePersistentProcess()
            {
                var storeReader = new ProcessStoreReaderFromDelegates
                {
                    EnumerateSerializedCompositionsRecordsReverseDelegate =
                        processStoreCompositions.AsEnumerable().Reverse,

                    GetReductionDelegate = compositionHash =>
                        (reductionRecordAvailableFromStore?.ReducedCompositionHash?.SequenceEqual(compositionHash) ?? false) ?
                        reductionRecordAvailableFromStore : null,
                };

                return new PersistentProcessWithHistoryOnFileFromElm019Code(storeReader, TestSetup.CounterElmAppFile);
            }

            var eventsAndExpectedResponsesBatches = eventsAndExpectedResponses.Batch(3).ToList();

            Assert.IsTrue(2 < eventsAndExpectedResponsesBatches.Count, "More than two batches of events to test with.");

            foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
            {
                ReductionRecord lastEventReductionRecord = null;

                foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
                {
                    using (var processInstance = InstantiatePersistentProcess())
                    {
                        var (processResponses, compositionRecord) = processInstance.ProcessEvents(new[] { serializedEvent });

                        var processResponse = processResponses.Single();

                        processStoreCompositions.Add(compositionRecord.serializedCompositionRecord);
                        lastEventReductionRecord = processInstance.ReductionRecordForCurrentState();

                        Assert.AreEqual(expectedResponse, processResponse, false, "process response");
                    }
                }

                reductionRecordAvailableFromStore = lastEventReductionRecord;
                processStoreCompositions = processStoreCompositions.AsEnumerable().Reverse().Take(1).ToList();
            }
        }

        [TestMethod]
        public void Restore_process_still_works_after_shuffling_records_in_process_store()
        {
            int fibonacci(int index) =>
                index <= 1 ? 1 : fibonacci(index - 2) + fibonacci(index - 1);

            var processAdditions =
                Enumerable.Range(0, 10).Select(fibonacci).Concat(new[] { 0 }).ToList();

            var eventsAndExpectedResponses =
                TestSetup.CounterProcessTestEventsAndExpectedResponses(processAdditions).ToList();

            var lastIdempotentEventAndExpectedResponse = eventsAndExpectedResponses.Last();

            var processStoreCompositionsBeforeShuffle = new List<byte[]>();

            PersistentProcessWithHistoryOnFileFromElm019Code InstantiatePersistentProcess(IProcessStoreReader storeReader) =>
                new PersistentProcessWithHistoryOnFileFromElm019Code(storeReader, TestSetup.CounterElmAppFile);

            void AssertProcessIsInFinalState(IPersistentProcess process, IList<byte[]> processStoreCompositions)
            {
                var (processResponses, compositionRecord) = process.ProcessEvents(
                    new[] { lastIdempotentEventAndExpectedResponse.serializedEvent });

                processStoreCompositions?.Add(compositionRecord.serializedCompositionRecord);

                var processResponse = processResponses.Single();

                Assert.AreEqual(
                    lastIdempotentEventAndExpectedResponse.expectedResponse, processResponse, false,
                    "process response");
            }

            using (var processInstance = InstantiatePersistentProcess(TestSetup.EmptyProcessStoreReader()))
            {
                Assert.ThrowsException<AssertFailedException>(
                    () => AssertProcessIsInFinalState(processInstance, processStoreCompositionsBeforeShuffle));

                foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponses)
                {
                    var (processResponses, compositionRecord) = processInstance.ProcessEvents(
                        new[] { serializedEvent });

                    var processResponse = processResponses.Single();

                    Assert.AreEqual(expectedResponse, processResponse, false, "process response");

                    processStoreCompositionsBeforeShuffle.Add(compositionRecord.serializedCompositionRecord);
                }

                AssertProcessIsInFinalState(processInstance, processStoreCompositionsBeforeShuffle);

                processStoreCompositionsBeforeShuffle.Count.Should()
                    .BeGreaterOrEqualTo(
                        eventsAndExpectedResponses.Count,
                        "All events should have been stored.");
            }

            var shuffledRecords =
                processStoreCompositionsBeforeShuffle
                .Batch(4)
                .Select((batch, batchIndex) => (batchIndex % 2 == 0) ? batch : batch.Reverse())
                .SelectMany(batch => batch)
                .ToList();

            CollectionAssert.AreNotEqual(processStoreCompositionsBeforeShuffle, shuffledRecords);

            var shuffledStoreReader = new ProcessStoreReaderFromDelegates
            {
                EnumerateSerializedCompositionsRecordsReverseDelegate = shuffledRecords.AsEnumerable().Reverse,
                GetReductionDelegate = compositionHash => null,
            };

            using (var processInstance = InstantiatePersistentProcess(shuffledStoreReader))
            {
                AssertProcessIsInFinalState(processInstance, null);

                Assert.AreEqual(
                    processAdditions.Sum().ToString(),
                    processInstance.ReductionRecordForCurrentState().ReducedValue,
                    "Sum of all additions should equal reduced state of restored process.");
            }
        }

        static void AssertProcessRestoresStateWithSequenceOfEventsAndExpectedResponses(
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
