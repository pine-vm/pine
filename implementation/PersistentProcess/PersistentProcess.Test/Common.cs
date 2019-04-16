using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Kalmit.ProcessStore;

namespace Kalmit.PersistentProcess.Test
{
    public class TestSetup
    {
        static public string PathToExampleElmApps => "./../../../../example-elm-apps";

        static public IEnumerable<(string serializedEvent, string expectedResponse)> CounterProcessTestEventsAndExpectedResponses(
            IEnumerable<(int addition, int expectedResponse)> additionsAndExpectedResponses) =>
            additionsAndExpectedResponses
            .Select(additionAndExpectedResponse =>
                (Newtonsoft.Json.JsonConvert.SerializeObject(new { addition = additionAndExpectedResponse.addition }),
                additionAndExpectedResponse.expectedResponse.ToString()));

        static public IEnumerable<(string serializedEvent, string expectedResponse)> CounterProcessTestEventsAndExpectedResponses(
            IEnumerable<int> additions, int previousValue = 0)
        {
            IEnumerable<(int addition, int expectedResponse)> enumerateWithExplicitExpectedResult()
            {
                var currentValue = previousValue;

                foreach (var addition in additions)
                {
                    currentValue += addition;

                    yield return (addition, currentValue);
                }
            }

            return CounterProcessTestEventsAndExpectedResponses(enumerateWithExplicitExpectedResult());
        }

        static public Kalmit.IDisposableProcessWithCustomSerialization BuildInstanceOfCounterProcess() =>
            Kalmit.ProcessFromElm019Code.WithCustomSerialization(
                CounterElmApp.ElmAppFiles,
                CounterElmApp.EntryConfig.Value.WithCustomSerialization.Value);

        static ElmAppWithEntryConfig CounterElmApp =
            GetElmAppWithEntryConfigFromExampleName("counter");

        static public ElmAppWithEntryConfig CounterElmWebApp =
            GetElmAppWithEntryConfigFromExampleName("counter-webapp");

        static public ElmAppWithEntryConfig StringBuilderElmWebApp =
            GetElmAppWithEntryConfigFromExampleName("string-builder-webapp");

         static public ElmAppWithEntryConfig CrossPropagateHttpHeadersToAndFromBodyElmWebApp =
            GetElmAppWithEntryConfigFromExampleName("cross-propagate-http-headers-to-and-from-body");

       static public byte[] CounterElmAppFile => ZipArchive.ZipArchiveFromEntries(CounterElmApp.AsFiles());

        static public ElmAppWithEntryConfig GetElmAppWithEntryConfigFromExampleName(string exampleName) =>
            ElmAppWithEntryConfig.FromFiles(
                Filesystem.GetAllFilesFromDirectory(Path.Combine(PathToExampleElmApps, exampleName)));

        static public IProcessStoreReader EmptyProcessStoreReader() =>
            new ProcessStoreReaderFromDelegates
            {
                EnumerateSerializedCompositionsRecordsReverseDelegate = () => Array.Empty<byte[]>(),
                GetReductionDelegate = _ => null,
            };
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
}
