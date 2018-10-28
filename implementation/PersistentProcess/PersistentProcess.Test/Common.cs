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

        static public Kalmit.IDisposableProcessWithCustomSerialization BuildInstanceOfCounterProcess() =>
            Kalmit.ProcessFromElm019Code.WithCustomSerialization(
                CounterElmApp.ElmAppFiles,
                CounterElmApp.EntryConfig.Value.WithCustomSerialization.Value);

        static ElmAppWithEntryConfig CounterElmApp =
            GetElmAppWithEntryConfigFromExampleName("counter");

        static ElmAppWithEntryConfig CounterElmWebApp =
            GetElmAppWithEntryConfigFromExampleName("counter-webapp");

        static public byte[] CounterElmAppFile => ZipArchive.ZipArchiveFromEntries(CounterElmApp.AsFiles());

        static public byte[] CounterElmWebAppFile => ZipArchive.ZipArchiveFromEntries(CounterElmWebApp.AsFiles());

        static public ElmAppWithEntryConfig GetElmAppWithEntryConfigFromExampleName(string exampleName) =>
            ElmAppWithEntryConfig.FromFiles(
                Filesystem.GetAllFilesFromDirectory(Path.Combine(PathToExampleElmApps, exampleName)));
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
