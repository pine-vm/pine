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
            System.Text.Json.JsonSerializer.Deserialize<Result<string, PineValueFromJson>>(
                responseJson,
                new System.Text.Json.JsonSerializerOptions { MaxDepth = 1000 })!;

        return
            responseStructure
            .Map(fromJson => ParsePineComponentFromJson(fromJson!));
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
            PineValueFromJson.FromComponentBuildingDictionary(environment, compilationCacheBefore);

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
            System.Text.Json.JsonSerializer.Deserialize<Result<string, PineValueFromJson>>(
                responseJson,
                options: new System.Text.Json.JsonSerializerOptions { MaxDepth = 1000 })!;

        var response =
            responseStructure
            .Map(fromJson => ParsePineComponentFromJson(fromJson));

        logDuration("Deserialize (from " + CommandLineInterface.FormatIntegerForDisplay(responseJson.Length) + " chars) and " + nameof(ParsePineComponentFromJson));

        return response.Map(value => (value, compilationCacheTask.Result));
    }

    internal record CompilationCache(
        IImmutableDictionary<PineValue, PineValueFromJson.ComponentMappedForTransport> valueCache);

    record CompileInteractiveSubmissionRequest(
        PineValueFromJson environment,
        string submission);

    static public Result<string, EvaluatedSctructure> SubmissionResponseFromResponsePineValue(
        JavaScriptEngineSwitcher.Core.IJsEngine evalElmPreparedJsEngine,
        PineValue response)
    {
        var responseJson =
            evalElmPreparedJsEngine.CallFunction(
                "submissionResponseFromResponsePineValue",
                System.Text.Json.JsonSerializer.Serialize(PineValueFromJson.FromComponentWithoutBuildingDictionary(response))).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<Result<string, EvaluatedSctructure>>(responseJson)!;

        return responseStructure;
    }

    internal record PineValueFromJson
    {
        public IReadOnlyCollection<DictionaryEntry>? Dictionary { init; get; } = null;

        public IReadOnlyList<PineValueFromJson>? List { init; get; }

        public IReadOnlyList<int>? Blob { init; get; }

        public string? ListAsString { init; get; }

        public string? Reference { init; get; }

        public record DictionaryEntry(string key, PineValueFromJson value);

        internal record ComponentMappedForTransport(
            string? ListAsString,
            IReadOnlyList<ComponentMappedForTransport>? List,
            PineValue origin)
            : IEquatable<ComponentMappedForTransport>
        {
            public virtual bool Equals(ComponentMappedForTransport? other) =>
                Equals(this, other);

            public override int GetHashCode()
            {
                if (ListAsString is not null)
                    return ListAsString.GetHashCode();

                return origin.GetHashCode();
            }

            static public bool Equals(ComponentMappedForTransport? left, ComponentMappedForTransport? right)
            {
                if (left is null && right is null)
                    return true;

                if (left is null || right is null)
                    return false;

                if (left.ListAsString is string leftString && right.ListAsString is string rightString)
                    return leftString == rightString;

                return left.origin.Equals(right.origin);
            }

            static public ComponentMappedForTransport FromComponent(
                PineValue component,
                IDictionary<PineValue, ComponentMappedForTransport>? cache)
            {
                if (cache?.TryGetValue(component, out var mapped) ?? false)
                    return mapped;

                mapped = FromComponentIgnoringCacheForCurrent(component, cache);

                cache?.Add(component, mapped);

                return mapped;
            }

            static private ComponentMappedForTransport FromComponentIgnoringCacheForCurrent(
                PineValue component,
                IDictionary<PineValue, ComponentMappedForTransport>? cache)
            {
                if (StringFromComponent(component) is Result<string, string>.Ok asString)
                    return new ComponentMappedForTransport(ListAsString: asString.Value, List: null, origin: component);

                if (component is PineValue.ListValue listComponent)
                    return new ComponentMappedForTransport(
                        ListAsString: null,
                        List: listComponent.Elements.Select(item => FromComponent(item, cache)).ToList(),
                        origin: component);

                return new ComponentMappedForTransport(ListAsString: null, List: null, origin: component);
            }
        }

        static public (PineValueFromJson, System.Threading.Tasks.Task<CompilationCache>) FromComponentBuildingDictionary(
            PineValue component,
            CompilationCache? compilationCache)
        {
            var intermediateStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var valueCache = new Dictionary<PineValue, ComponentMappedForTransport>(
                compilationCache?.valueCache ?? ImmutableDictionary<PineValue, ComponentMappedForTransport>.Empty);

            var intermediate = ComponentMappedForTransport.FromComponent(component, cache: valueCache);

            var compilationCacheTask = System.Threading.Tasks.Task.Run(() =>
            {
                return new CompilationCache(valueCache: valueCache.ToImmutableDictionary());
            });

            var usageCountLowerBoundDictionary = new Dictionary<ComponentMappedForTransport, int>();

            void mutatingCountUsagesRecursive(ComponentMappedForTransport component)
            {
                if (!usageCountLowerBoundDictionary.TryGetValue(component, out var usageCountLowerBound))
                    usageCountLowerBound = 0;

                ++usageCountLowerBound;

                usageCountLowerBoundDictionary[component] = usageCountLowerBound;

                if (1 < usageCountLowerBound)
                    return;

                if (component.ListAsString is null && component.List is IReadOnlyList<ComponentMappedForTransport> asList)
                {
                    foreach (var item in asList)
                    {
                        mutatingCountUsagesRecursive(item);
                    }
                }
            }

            mutatingCountUsagesRecursive(intermediate);

            var componentsUsedMultipleTimes =
                usageCountLowerBoundDictionary
                .Where(count => count.Value > 1)
                .ToImmutableDictionary(x => x.Key, x => x.Value);

            int keyIndex = 0;

            var dictionary = new Dictionary<ComponentMappedForTransport, string>();

            void mutatingBuildDictionaryRecursive(ComponentMappedForTransport component)
            {
                if (dictionary.ContainsKey(component))
                    return;

                if (componentsUsedMultipleTimes!.ContainsKey(component))
                {
                    dictionary[component] = keyIndex++.ToString();
                    return;
                }

                if (component.List is IReadOnlyList<ComponentMappedForTransport> isList)
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
                    value: FromComponentWithoutBuildingDictionary(entry.Key, dictionary, doNotDictionaryOnFirstLevel: true)))
                .ToImmutableArray();

            var componentForJson = FromComponentWithoutBuildingDictionary(
                intermediate,
                dictionary: dictionary)
                with
            { Dictionary = dictionaryForSerial };

            return (componentForJson, compilationCacheTask);
        }

        static public PineValueFromJson FromComponentWithoutBuildingDictionary(PineValue component) =>
            FromComponentWithoutBuildingDictionary(ComponentMappedForTransport.FromComponent(component, cache: null));

        static PineValueFromJson FromComponentWithoutBuildingDictionary(
            ComponentMappedForTransport component,
            IReadOnlyDictionary<ComponentMappedForTransport, string>? dictionary = null,
            bool doNotDictionaryOnFirstLevel = false)
        {
            if (!doNotDictionaryOnFirstLevel && (dictionary?.TryGetValue(component, out var result) ?? false))
                return new PineValueFromJson { Reference = result };

            if (component.ListAsString is string asString)
                return new PineValueFromJson { ListAsString = asString };

            if (component.List is IReadOnlyList<ComponentMappedForTransport> asList)
            {
                return new PineValueFromJson
                {
                    List = asList.Select(e => FromComponentWithoutBuildingDictionary(e, dictionary)).ToList()
                };
            }

            if (component.origin is PineValue.BlobValue blobComponent)
                return new PineValueFromJson { Blob = blobComponent.Bytes.ToArray().Select(b => (int)b).ToImmutableArray() };

            throw new NotImplementedException("Unexpected shape");
        }
    }

    static PineValue ParsePineComponentFromJson(PineValueFromJson fromJson)
    {
        if (fromJson.List != null)
            return PineValue.List(fromJson.List.Select(ParsePineComponentFromJson).ToImmutableList());

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
