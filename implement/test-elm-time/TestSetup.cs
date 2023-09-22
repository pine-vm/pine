using ElmTime;
using ElmTime.Platform.WebService;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace TestElmTime;

public class TestSetup
{
    public static string PathToExampleElmApps => "./../../../example-elm-apps";

    public static PineValue AppConfigComponentFromFiles(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> appFiles) =>
        PineValueComposition.FromTreeWithStringPath(PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(appFiles))!;

    public static IEnumerable<(string serializedEvent, string expectedResponse)> CounterProcessTestEventsAndExpectedResponses(
        IEnumerable<(int addition, int expectedResponse)> additionsAndExpectedResponses) =>
        additionsAndExpectedResponses
        .Select(additionAndExpectedResponse =>
            (System.Text.Json.JsonSerializer.Serialize(new { addition = additionAndExpectedResponse.addition }),
            additionAndExpectedResponse.expectedResponse.ToString()));

    public static IEnumerable<(string serializedEvent, string expectedResponse)> CounterProcessTestEventsAndExpectedResponses(
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

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> CounterElmWebApp =
        GetElmAppFromExampleName("counter-webapp");

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> CalculatorWebApp =
        GetElmAppFromExampleName("calculator-webapp");

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> ReadSourceFileWebApp =
        GetElmAppFromExampleName("read-source-file-webapp");

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> StringBuilderElmWebApp =
        GetElmAppFromExampleName("string-builder-webapp");

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> CrossPropagateHttpHeadersToAndFromBodyElmWebApp =
       GetElmAppFromExampleName("cross-propagate-http-headers-to-and-from-body");

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> HttpProxyWebApp =
       GetElmAppFromExampleName("http-proxy");

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> VolatileProcessNativeWebApp =>
       GetElmAppFromExampleName("volatile-process-native");

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> WithWebServiceConfigJson(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> originalDeploymentFiles,
        WebServiceConfigJson jsonStructure)
    {
        return
            jsonStructure == null ?
            originalDeploymentFiles.RemoveRange(StartupAdminInterface.WebServiceConfigFilePathAlternatives)
            :
            originalDeploymentFiles
            .SetItem(
                StartupAdminInterface.WebServiceConfigFilePathDefault,
                System.Text.Encoding.UTF8.GetBytes(System.Text.Json.JsonSerializer.Serialize(jsonStructure)));
    }

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> GetElmAppFromExampleName(
        string exampleName) => GetElmAppFromDirectoryPath(Path.Combine(PathToExampleElmApps, exampleName));

    private static string FilePathStringFromPath(IReadOnlyList<string> path) =>
        Path.Combine([.. path]);

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> GetElmAppFromDirectoryPath(
        IReadOnlyList<string> directoryPath) =>
        GetElmAppFromDirectoryPath(FilePathStringFromPath(directoryPath));

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> GetElmAppFromDirectoryPath(
        string directoryPath) =>
            PineValueComposition.ToFlatDictionaryWithPathComparer(
                Filesystem.GetAllFilesFromDirectory(directoryPath)
                .OrderBy(file => string.Join('/', file.path)));

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> AsLoweredElmApp(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> originalAppFiles,
        IReadOnlyList<string> compilationRootFilePath)
    {
        var compilationResult =
            ElmAppCompilation.AsCompletelyLoweredElmApp(
                sourceFiles: originalAppFiles,
                workingDirectoryRelative: ImmutableList<string>.Empty,
                ElmAppInterfaceConfig.Default with { compilationRootFilePath = compilationRootFilePath })
            .Extract(error => throw new Exception(ElmAppCompilation.CompileCompilationErrorsDisplayText(error)));

        return compilationResult.result.compiledFiles;
    }
}
