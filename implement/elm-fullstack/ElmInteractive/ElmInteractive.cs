using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using Pine;
using static Pine.Composition;

namespace ElmFullstack.ElmInteractive;

public class ElmInteractive
{
    static public readonly Lazy<string> JavascriptToEvaluateElm = new(PrepareJavascriptToEvaluateElm, System.Threading.LazyThreadSafetyMode.ExecutionAndPublication);

    static public Result<string, EvaluatedSctructure> EvaluateSubmissionAndGetResultingValue(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        TreeNodeWithStringPath? appCodeTree,
        string submission,
        IReadOnlyList<string>? previousLocalSubmissions = null)
    {
        var modulesTexts = ModulesTextsFromAppCodeTree(appCodeTree);

        var argumentsJson = System.Text.Json.JsonSerializer.Serialize(
            new
            {
                modulesTexts = modulesTexts ?? ImmutableList<string>.Empty,
                submission = submission,
                previousLocalSubmissions = previousLocalSubmissions ?? ImmutableList<string>.Empty,
            }
        );

        var responseJson =
            evalElmPreparedJsEngine.CallFunction("evaluateSubmissionInInteractive", argumentsJson).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<EvaluateSubmissionResponseStructure>(responseJson)!;

        if (responseStructure.DecodedArguments == null)
            throw new Exception("Failed to decode arguments: " + responseStructure.FailedToDecodeArguments);

        if (responseStructure.DecodedArguments.Evaluated == null)
            return Result<string, EvaluatedSctructure>.err(responseStructure.DecodedArguments.FailedToEvaluate!);

        return Result<string, EvaluatedSctructure>.ok(
            responseStructure.DecodedArguments.Evaluated);
    }

    static public Result<string, PineValue> CompileEvalContextForElmInteractive(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        TreeNodeWithStringPath? appCodeTree)
    {
        var modulesTexts = ModulesTextsFromAppCodeTree(appCodeTree);

        var argumentsJson = System.Text.Json.JsonSerializer.Serialize(modulesTexts ?? ImmutableList<string>.Empty);

        var responseJson =
            evalElmPreparedJsEngine.CallFunction("compileEvalContextForElmInteractive", argumentsJson).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<Result<string, PineValueJson>>(
                responseJson,
                new System.Text.Json.JsonSerializerOptions { MaxDepth = 1000 })!;

        return
            responseStructure
            .Map(fromJson => ParsePineValueFromJson(fromJson!));
    }

    static internal Result<string, (PineValue compiledValue, CompilationCache cache)> CompileInteractiveSubmission(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        PineValue environment,
        string submission,
        Action<string>? addInspectionLogEntry,
        CompilationCache? compilationCacheBefore)
    {
        var clock = System.Diagnostics.Stopwatch.StartNew();

        void logDuration(string label) =>
            addInspectionLogEntry?.Invoke(
                label + " duration: " + CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) + " ms");

        var (environmentJson, compilationCacheTask) =
            PineValueJson.FromPineValueBuildingDictionary(environment, compilationCacheBefore);

        var requestJson =
            System.Text.Json.JsonSerializer.Serialize(
                new CompileInteractiveSubmissionRequest
                (
                    environment: environmentJson,
                    submission: submission
                ),
                options: new System.Text.Json.JsonSerializerOptions
                {
                    DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
                    MaxDepth = 1000
                });

        logDuration("Serialize to JSON (" + CommandLineInterface.FormatIntegerForDisplay(requestJson.Length) + " chars)");

        clock.Restart();

        var responseJson =
            evalElmPreparedJsEngine.CallFunction("compileInteractiveSubmission", requestJson).ToString()!;

        logDuration("JavaScript function");

        clock.Restart();

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<Result<string, PineValueJson>>(
                responseJson,
                options: new System.Text.Json.JsonSerializerOptions { MaxDepth = 1000 })!;

        var response =
            responseStructure
            .Map(fromJson => ParsePineValueFromJson(fromJson));

        logDuration("Deserialize (from " + CommandLineInterface.FormatIntegerForDisplay(responseJson.Length) + " chars) and " + nameof(ParsePineValueFromJson));

        return response.Map(value => (value, compilationCacheTask.Result));
    }

    internal record CompilationCache(
        IImmutableDictionary<PineValue, PineValueJson.PineValueMappedForTransport> valueMappedForTransportCache,
        IImmutableDictionary<PineValue, PineValueJson> valueJsonCache);

    record CompileInteractiveSubmissionRequest(
        PineValueJson environment,
        string submission);

    static public Result<string, EvaluatedSctructure> SubmissionResponseFromResponsePineValue(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        PineValue response)
    {
        var responseJson =
            evalElmPreparedJsEngine.CallFunction(
                "submissionResponseFromResponsePineValue",
                System.Text.Json.JsonSerializer.Serialize(PineValueJson.FromPineValueWithoutBuildingDictionary(response))).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<Result<string, EvaluatedSctructure>>(responseJson)!;

        return responseStructure;
    }

    internal record PineValueJson
    {
        public IReadOnlyCollection<DictionaryEntry>? Dictionary { init; get; } = null;

        public IReadOnlyList<PineValueJson>? List { init; get; }

        public IReadOnlyList<int>? Blob { init; get; }

        public string? ListAsString { init; get; }

        public string? Reference { init; get; }

        public record DictionaryEntry(string key, PineValueJson value);

        internal record PineValueMappedForTransport(
            string? ListAsString,
            IReadOnlyList<PineValueMappedForTransport>? List,
            PineValue origin)
            : IEquatable<PineValueMappedForTransport>
        {
            public virtual bool Equals(PineValueMappedForTransport? other) =>
                Equals(this, other);

            public override int GetHashCode()
            {
                if (ListAsString is not null)
                    return ListAsString.GetHashCode();

                return origin.GetHashCode();
            }

            static public bool Equals(PineValueMappedForTransport? left, PineValueMappedForTransport? right)
            {
                if (left is null && right is null)
                    return true;

                if (left is null || right is null)
                    return false;

                if (left.ListAsString is string leftString && right.ListAsString is string rightString)
                    return leftString == rightString;

                return left.origin.Equals(right.origin);
            }

            static public PineValueMappedForTransport FromPineValue(
                PineValue pineValue,
                IDictionary<PineValue, PineValueMappedForTransport>? cache)
            {
                if (cache?.TryGetValue(pineValue, out var mapped) ?? false)
                    return mapped;

                mapped = FromPineValueIgnoringCacheForCurrent(pineValue, cache);

                cache?.Add(pineValue, mapped);

                return mapped;
            }

            static private PineValueMappedForTransport FromPineValueIgnoringCacheForCurrent(
                PineValue pineValue,
                IDictionary<PineValue, PineValueMappedForTransport>? cache)
            {
                if (StringFromComponent(pineValue) is Result<string, string>.Ok asString)
                    return new PineValueMappedForTransport(ListAsString: asString.Value, List: null, origin: pineValue);

                if (pineValue is PineValue.ListValue listComponent)
                    return new PineValueMappedForTransport(
                        ListAsString: null,
                        List: listComponent.Elements.Select(item => FromPineValue(item, cache)).ToList(),
                        origin: pineValue);

                return new PineValueMappedForTransport(ListAsString: null, List: null, origin: pineValue);
            }
        }

        static public (PineValueJson, System.Threading.Tasks.Task<CompilationCache>) FromPineValueBuildingDictionary(
            PineValue pineValue,
            CompilationCache? compilationCache)
        {
            if (compilationCache is CompilationCache compilationCacheNotNull)
                if (compilationCacheNotNull.valueJsonCache.TryGetValue(pineValue, out var cached))
                    return (cached, System.Threading.Tasks.Task.FromResult(compilationCacheNotNull));

            var valueMappedForTransportCache = new Dictionary<PineValue, PineValueMappedForTransport>(
                compilationCache?.valueMappedForTransportCache ??
                ImmutableDictionary<PineValue, PineValueMappedForTransport>.Empty);

            var intermediate = PineValueMappedForTransport.FromPineValue(pineValue, cache: valueMappedForTransportCache);

            var usageCountLowerBoundDictionary = new Dictionary<PineValueMappedForTransport, int>();

            void mutatingCountUsagesRecursive(PineValueMappedForTransport mappedForTransport)
            {
                if (!usageCountLowerBoundDictionary.TryGetValue(mappedForTransport, out var usageCountLowerBound))
                    usageCountLowerBound = 0;

                ++usageCountLowerBound;

                usageCountLowerBoundDictionary[mappedForTransport] = usageCountLowerBound;

                if (1 < usageCountLowerBound)
                    return;

                if (mappedForTransport.ListAsString is null && mappedForTransport.List is IReadOnlyList<PineValueMappedForTransport> asList)
                {
                    foreach (var item in asList)
                    {
                        mutatingCountUsagesRecursive(item);
                    }
                }
            }

            mutatingCountUsagesRecursive(intermediate);

            var valuesUsedMultipleTimes =
                usageCountLowerBoundDictionary
                .Where(count => count.Value > 1)
                .ToImmutableDictionary(x => x.Key, x => x.Value);

            int keyIndex = 0;

            var dictionary = new Dictionary<PineValueMappedForTransport, string>();

            void mutatingBuildDictionaryRecursive(PineValueMappedForTransport mappedForTransport)
            {
                if (dictionary.ContainsKey(mappedForTransport))
                    return;

                if (valuesUsedMultipleTimes!.ContainsKey(mappedForTransport))
                {
                    dictionary[mappedForTransport] = keyIndex++.ToString();
                    return;
                }

                if (mappedForTransport.List is IReadOnlyList<PineValueMappedForTransport> isList)
                {
                    foreach (var item in isList)
                    {
                        mutatingBuildDictionaryRecursive(item);
                    }
                }
            }

            mutatingBuildDictionaryRecursive(intermediate);

            var dictionaryForSerial =
                dictionary
                .Select(entry => new DictionaryEntry(
                    key: entry.Value,
                    value: FromPineValueWithoutBuildingDictionary(entry.Key, dictionary, doNotDictionaryOnFirstLevel: true)))
                .ToImmutableArray();

            var pineValueJson = FromPineValueWithoutBuildingDictionary(
                intermediate,
                dictionary: dictionary)
                with
            { Dictionary = dictionaryForSerial };

            var compilationCacheTask = System.Threading.Tasks.Task.Run(() =>
            {
                var valueJsonCache = new Dictionary<PineValue, PineValueJson>(
                    compilationCache?.valueJsonCache ??
                    ImmutableDictionary<PineValue, PineValueJson>.Empty)
                {
                    [pineValue] = pineValueJson
                };

                return new CompilationCache(
                    valueMappedForTransportCache: valueMappedForTransportCache.ToImmutableDictionary(),
                    valueJsonCache: valueJsonCache.ToImmutableDictionary());
            });

            return (pineValueJson, compilationCacheTask);
        }

        static public PineValueJson FromPineValueWithoutBuildingDictionary(PineValue pineValue) =>
            FromPineValueWithoutBuildingDictionary(PineValueMappedForTransport.FromPineValue(pineValue, cache: null));

        static PineValueJson FromPineValueWithoutBuildingDictionary(
            PineValueMappedForTransport pineValue,
            IReadOnlyDictionary<PineValueMappedForTransport, string>? dictionary = null,
            bool doNotDictionaryOnFirstLevel = false)
        {
            if (!doNotDictionaryOnFirstLevel && (dictionary?.TryGetValue(pineValue, out var result) ?? false))
                return new PineValueJson { Reference = result };

            if (pineValue.ListAsString is string asString)
                return new PineValueJson { ListAsString = asString };

            if (pineValue.List is IReadOnlyList<PineValueMappedForTransport> asList)
            {
                return new PineValueJson
                {
                    List = asList.Select(e => FromPineValueWithoutBuildingDictionary(e, dictionary)).ToList()
                };
            }

            if (pineValue.origin is PineValue.BlobValue blobComponent)
                return new PineValueJson { Blob = blobComponent.Bytes.ToArray().Select(b => (int)b).ToImmutableArray() };

            throw new NotImplementedException("Unexpected shape");
        }
    }

    static PineValue ParsePineValueFromJson(PineValueJson fromJson)
    {
        if (fromJson.List != null)
            return PineValue.List(fromJson.List.Select(ParsePineValueFromJson).ToImmutableList());

        if (fromJson.Blob != null)
            return PineValue.Blob(fromJson.Blob.Select(b => (byte)b).ToArray());

        if (fromJson.ListAsString != null)
            return ComponentFromString(fromJson.ListAsString);

        throw new NotImplementedException("Unexpected shape of Pine value from JSON");
    }

    static IReadOnlyCollection<string>? ModulesTextsFromAppCodeTree(TreeNodeWithStringPath? appCodeTree) =>
        appCodeTree == null ? null
        :
        TreeToFlatDictionaryWithPathComparer(compileTree(appCodeTree)!)
        .Select(appCodeFile => appCodeFile.Key.Last().EndsWith(".elm") ? Encoding.UTF8.GetString(appCodeFile.Value.ToArray()) : null)
        .WhereNotNull()
        .ToImmutableList();

    static TreeNodeWithStringPath? compileTree(TreeNodeWithStringPath? sourceTree)
    {
        if (sourceTree == null)
            return null;

        var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
            sourceFiles: TreeToFlatDictionaryWithPathComparer(sourceTree),
            ElmAppInterfaceConfig.Default);

        return
            compilationResult
            .Unpack(
                fromErr: compilationError =>
                {
                    var errorMessage = "\n" + ElmAppCompilation.CompileCompilationErrorsDisplayText(compilationError) + "\n";

                    Console.WriteLine(errorMessage);

                    throw new Exception(errorMessage);
                },
                fromOk: compilationOk => SortedTreeFromSetOfBlobsWithStringPath(compilationOk.compiledAppFiles));
    }

    static public JavaScriptEngineSwitcher.Core.IJsEngine PrepareJsEngineToEvaluateElm()
    {
        var javascriptEngine = ProcessHostedWithV8.ConstructJsEngine();

        javascriptEngine.Evaluate(JavascriptToEvaluateElm.Value);

        return javascriptEngine;
    }

    static public string PrepareJavascriptToEvaluateElm()
    {
        var parseElmAppCodeFiles = ParseElmSyntaxAppCodeFiles();

        var javascriptFromElmMake =
            ProcessFromElm019Code.CompileElmToJavascript(
                parseElmAppCodeFiles,
                ImmutableList.Create("src", "Main.elm"));

        var javascriptMinusCrashes = ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

        var listFunctionToPublish =
            new[]
            {
                (functionNameInElm: "Main.evaluateSubmissionInInteractive",
                publicName: "evaluateSubmissionInInteractive",
                arity: 1),

                (functionNameInElm: "Main.compileEvalContextForElmInteractive",
                publicName: "compileEvalContextForElmInteractive",
                arity: 1),

                (functionNameInElm: "Main.compileInteractiveSubmission",
                publicName: "compileInteractiveSubmission",
                arity: 1),

                (functionNameInElm: "Main.submissionResponseFromResponsePineValue",
                publicName: "submissionResponseFromResponsePineValue",
                arity: 1),
            };

        return
            ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                javascriptMinusCrashes,
                listFunctionToPublish);
    }

    static public IImmutableDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> ParseElmSyntaxAppCodeFiles() =>
        DotNetAssembly.LoadFromAssemblyManifestResourceStreamContents(
            filePaths: new[]
            {
                ImmutableList.Create("elm.json"),
                ImmutableList.Create("src", "Pine.elm"),
                ImmutableList.Create("src", "ElmInteractive.elm"),
                ImmutableList.Create("src", "Main.elm")
            },
            resourceNameCommonPrefix: "ElmFullstack.ElmInteractive.interpret_elm_program.",
            assembly: typeof(ElmInteractive).Assembly)
        .Extract(error => throw new NotImplementedException(nameof(ParseElmSyntaxAppCodeFiles) + ": " + error));

    record EvaluateSubmissionResponseStructure
        (string? FailedToDecodeArguments = null,
        DecodedArgumentsSctructure? DecodedArguments = null);

    record DecodedArgumentsSctructure(
        string? FailedToEvaluate = null,
        EvaluatedSctructure? Evaluated = null);

    public record EvaluatedSctructure(
        string displayText);
}
