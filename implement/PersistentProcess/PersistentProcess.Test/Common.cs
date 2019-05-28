using Kalmit.ProcessStore;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

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

        static public Kalmit.IDisposableProcessWithStringInterface BuildInstanceOfCounterProcess() =>
            Kalmit.ProcessFromElm019Code.ProcessFromElmCodeFiles(CounterElmApp).process;

        static IReadOnlyCollection<(string filePath, byte[] fileContent)> CounterElmApp =
            GetElmAppFromExampleName("counter");

        static public IReadOnlyCollection<(string filePath, byte[] fileContent)> CounterElmWebApp =
            GetElmAppFromExampleName("counter-webapp");

        static public IReadOnlyCollection<(string filePath, byte[] fileContent)> StringBuilderElmWebApp =
            GetElmAppFromExampleName("string-builder-webapp");

        static public IReadOnlyCollection<(string filePath, byte[] fileContent)> CrossPropagateHttpHeadersToAndFromBodyElmWebApp =
           GetElmAppFromExampleName("cross-propagate-http-headers-to-and-from-body");

        static public IReadOnlyCollection<(string filePath, byte[] fileContent)> HttpProxyWebApp =
           GetElmAppFromExampleName("http-proxy");

        static public byte[] CounterElmAppFile => ZipArchive.ZipArchiveFromEntries(CounterElmApp);

        static public IReadOnlyCollection<(string filePath, byte[] fileContent)> GetElmAppFromExampleName(
            string exampleName) =>
            ElmApp.FilesFilteredForElmApp(
                Filesystem.GetAllFilesFromDirectory(Path.Combine(PathToExampleElmApps, exampleName)))
            .ToImmutableList();

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
