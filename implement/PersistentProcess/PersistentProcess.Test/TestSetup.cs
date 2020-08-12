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

        static public Composition.Component AppConfigComponentFromFiles(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> appFiles) =>
            Composition.FromTree(Composition.SortedTreeFromSetOfBlobsWithStringPath(appFiles));

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
            Kalmit.ProcessFromElm019Code.ProcessFromElmCodeFiles(AsLoweredElmApp(CounterElmApp)).process;

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> CounterElmApp =
            GetElmAppFromExampleName("counter");

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> CounterElmWebApp =
            GetElmAppFromExampleName("counter-webapp");

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> ReadSourceFileWebApp =
            GetElmAppFromExampleName("read-source-file-webapp");

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> StringBuilderElmWebApp =
            GetElmAppFromExampleName("string-builder-webapp");

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> CrossPropagateHttpHeadersToAndFromBodyElmWebApp =
           GetElmAppFromExampleName("cross-propagate-http-headers-to-and-from-body");

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> HttpProxyWebApp =
           GetElmAppFromExampleName("http-proxy");

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> WithElmFullstackJson(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> originalWebAppConfig,
            WebAppConfigurationJsonStructure jsonStructure)
        {
            var filePath = WebHost.StartupAdminInterface.JsonFilePath;

            return
                jsonStructure == null ?
                originalWebAppConfig.Remove(filePath)
                :
                originalWebAppConfig
                .SetItem(filePath, System.Text.Encoding.UTF8.GetBytes(Newtonsoft.Json.JsonConvert.SerializeObject(jsonStructure)).ToImmutableList());
        }

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> GetElmAppFromExampleName(
            string exampleName) => GetElmAppFromDirectoryPath(Path.Combine(PathToExampleElmApps, exampleName));

        static string FilePathStringFromPath(IImmutableList<string> path) =>
            Path.Combine(path.ToArray());

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> GetElmAppFromDirectoryPath(
            IImmutableList<string> directoryPath) =>
            GetElmAppFromDirectoryPath(FilePathStringFromPath(directoryPath));

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> GetElmAppFromDirectoryPath(
            string directoryPath) =>
                ElmApp.ToFlatDictionaryWithPathComparer(
                    ElmApp.FilesFilteredForElmApp(
                        Filesystem.GetAllFilesFromDirectory(directoryPath))
                        .OrderBy(file => file.filePath)
                        .Select(filePathAndContent => ((IImmutableList<string>)filePathAndContent.filePath.Split(new[] { '/', '\\' }).ToImmutableList(), filePathAndContent.fileContent)));

        static public IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> AsLoweredElmApp(
            IImmutableDictionary<IImmutableList<string>, IImmutableList<byte>> originalAppFiles) =>
                ElmApp.AsCompletelyLoweredElmApp(
                    sourceFiles: originalAppFiles,
                    ElmAppInterfaceConfig.Default,
                    Console.WriteLine);

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
