using ElmTime.Elm019;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using static Pine.PineValueComposition;

namespace ElmTime.ElmInteractive;

public class ElmInteractive
{
    public static readonly Lazy<string> JavascriptToEvaluateElm = new(PrepareJavascriptToEvaluateElm, System.Threading.LazyThreadSafetyMode.ExecutionAndPublication);

    public static Result<string, EvaluatedSctructure> EvaluateSubmissionAndGetResultingValue(
        IJsEngine evalElmPreparedJsEngine,
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

    public static IReadOnlyList<string> GetDefaultElmCoreModulesTexts(
        IJsEngine evalElmPreparedJsEngine)
    {
        var responseJson =
            evalElmPreparedJsEngine.CallFunction("getDefaultElmCoreModulesTexts", 0).ToString()!;

        return
            System.Text.Json.JsonSerializer.Deserialize<IReadOnlyList<string>>(responseJson)!;
    }

    internal static Result<string, (PineValue compileResult, CompilationCache compilationCache)> CompileInteractiveEnvironment(
        IJsEngine evalElmPreparedJsEngine,
        TreeNodeWithStringPath? appCodeTree,
        CompilationCache compilationCacheBefore)
    {
        var allModulesTexts =
            GetDefaultElmCoreModulesTexts(evalElmPreparedJsEngine)
            .Concat(ModulesTextsFromAppCodeTree(appCodeTree).EmptyIfNull())
            .ToImmutableList();

        return
            CompileInteractiveEnvironmentForModulesCachingIncrements(
                elmModulesTexts: allModulesTexts,
                evalElmPreparedJsEngine,
                compilationCacheBefore)
            .Map(compileResultAndCache =>
            (compileResult: compileResultAndCache.compileResult.environmentPineValue,
            compileResultAndCache.compilationCache));
    }

    private static Result<string, (CompileInteractiveEnvironmentResult compileResult, CompilationCache compilationCache)> CompileInteractiveEnvironmentForModulesCachingIncrements(
        IReadOnlyList<string> elmModulesTexts,
        IJsEngine evalElmPreparedJsEngine,
        CompilationCache compilationCacheBefore)
    {
        var baseResults =
            compilationCacheBefore.compileInteractiveEnvironmentResults
            .Where(cachedResult =>
            {
                var cachedResultAllModules = cachedResult.AllModulesTextsList;

                return elmModulesTexts.Take(cachedResultAllModules.Count).SequenceEqual(cachedResultAllModules);
            })
            .ToImmutableList();

        var closestBase =
            baseResults
            .OrderByDescending(c => c.AllModulesTextsList.Count)
            .OfType<CompileInteractiveEnvironmentResult?>()
            .FirstOrDefault();

        var elmModulesTextsFromBase =
            closestBase is null ?
            elmModulesTexts :
            elmModulesTexts.Skip(closestBase.AllModulesTextsList.Count).ToImmutableList();

        var initResult =
            closestBase is not null
            ?
            Result<string, (CompileInteractiveEnvironmentResult compileResult, CompilationCache compilationCache)>.ok(
                (closestBase, compilationCacheBefore))
            :
            CompileInteractiveEnvironmentForModules(
                elmModulesTexts: ImmutableList<string>.Empty,
                evalElmPreparedJsEngine: evalElmPreparedJsEngine,
                parentEnvironment: null,
                compilationCacheBefore: compilationCacheBefore);

        return
            initResult
            .AndThen(seed =>
            {
                return
                    ResultExtension.AggregateExitingOnFirstError(
                        sequence: elmModulesTextsFromBase,
                        aggregateFunc: (prev, elmCoreModuleText) =>
                        {
                            var resultBeforeCache =
                            CompileInteractiveEnvironmentForModules(
                                elmModulesTexts: ImmutableList.Create(elmCoreModuleText),
                                evalElmPreparedJsEngine: evalElmPreparedJsEngine,
                                parentEnvironment: prev.compileResult,
                                compilationCacheBefore: prev.compilationCache);

                            return
                            resultBeforeCache
                            .Map(beforeCache =>
                            {
                                var compileInteractiveEnvironmentResults =
                                beforeCache.compilationCache.compileInteractiveEnvironmentResults.Add(beforeCache.compileResult);

                                var cache = beforeCache.compilationCache
                                with
                                {
                                    compileInteractiveEnvironmentResults = compileInteractiveEnvironmentResults
                                };

                                return (beforeCache.compileResult, cache);
                            });
                        },
                        aggregateSeed: seed);
            });
    }

    private static Result<string, (CompileInteractiveEnvironmentResult compileResult, CompilationCache compilationCache)> CompileInteractiveEnvironmentForModules(
        IReadOnlyList<string> elmModulesTexts,
        CompileInteractiveEnvironmentResult? parentEnvironment,
        IJsEngine evalElmPreparedJsEngine,
        CompilationCache compilationCacheBefore)
    {
        var environmentBefore =
            parentEnvironment is null ? PineValueJson.FromPineValueWithoutBuildingDictionary(PineValue.EmptyList) :
            parentEnvironment.environmentPineValueJson;

        var argumentsJson = System.Text.Json.JsonSerializer.Serialize(
            new CompileElmInteractiveEnvironmentRequest(
                modulesTexts: elmModulesTexts,
                environmentBefore: environmentBefore),
            options: new System.Text.Json.JsonSerializerOptions { MaxDepth = 1000 });

        var responseJson =
            evalElmPreparedJsEngine.CallFunction("compileInteractiveEnvironment", argumentsJson).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<Result<string, PineValueJson>>(
                responseJson,
                new System.Text.Json.JsonSerializerOptions { MaxDepth = 1000 })!;

        return
            responseStructure
            .Map(fromJson =>
            {
                var environmentPineValue = ParsePineValueFromJson(fromJson!, dictionary: parentEnvironment?.environmentDictionary);

                var (environmentJson, compilationCacheTask) =
                PineValueJson.FromPineValueBuildingDictionary(
                    environmentPineValue, compilationCache: compilationCacheBefore);

                return
                (new CompileInteractiveEnvironmentResult(
                    lastIncrementModulesTexts: elmModulesTexts.ToImmutableList(),
                    environmentPineValueJson: environmentJson.json,
                    environmentPineValue: environmentPineValue,
                    environmentDictionary: environmentJson.dictionary,
                    parent: parentEnvironment),
                    compilationCacheTask.Result);
            });
    }

    internal record CompileInteractiveEnvironmentResult(
        IImmutableList<string> lastIncrementModulesTexts,
        PineValueJson environmentPineValueJson,
        PineValue environmentPineValue,
        IReadOnlyDictionary<string, PineValue> environmentDictionary,
        CompileInteractiveEnvironmentResult? parent) : IEquatable<CompileInteractiveEnvironmentResult>
    {
        public IImmutableList<string> AllModulesTextsList =>
            parent is null ? lastIncrementModulesTexts : parent.AllModulesTextsList.AddRange(lastIncrementModulesTexts);

        public ReadOnlyMemory<byte> Hash => HashCache.Value;

        private Lazy<ReadOnlyMemory<byte>> HashCache => new(ComputeHash);

        private ReadOnlyMemory<byte> ComputeHash()
        {
            var lastIncrementModulesBlobs =
                lastIncrementModulesTexts.Select(Encoding.UTF8.GetBytes).ToArray();

            var selfValue = PineValue.List(lastIncrementModulesBlobs.Select(blob => PineValue.Blob(blob)).ToImmutableList());

            var selfHash = PineValueHashTree.ComputeHash(selfValue);

            if (parent is null)
                return selfHash;

            return PineValueHashTree.ComputeHash(PineValue.List(new[] { PineValue.Blob(selfHash), PineValue.Blob(parent.Hash) }));
        }

        public virtual bool Equals(CompileInteractiveEnvironmentResult? other)
        {
            if (other is null)
                return false;

            return Hash.Span.SequenceEqual(other.Hash.Span);
        }

        public override int GetHashCode()
        {
            return Hash.GetHashCode();
        }
    }

    internal static Result<string, (PineValue compiledValue, CompilationCache cache)> CompileInteractiveSubmission(
        IJsEngine evalElmPreparedJsEngine,
        PineValue environment,
        string submission,
        Action<string>? addInspectionLogEntry,
        CompilationCache compilationCacheBefore)
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
                    environment: environmentJson.json,
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
            .Map(fromJson => ParsePineValueFromJson(fromJson, dictionary: environmentJson.dictionary));

        logDuration("Deserialize (from " + CommandLineInterface.FormatIntegerForDisplay(responseJson.Length) + " chars) and " + nameof(ParsePineValueFromJson));

        return response.Map(value => (value, compilationCacheTask.Result));
    }

    internal record CompilationCache(
        IImmutableDictionary<PineValue, PineValueJson.PineValueMappedForTransport> valueMappedForTransportCache,
        IImmutableDictionary<PineValue, (PineValueJson, IReadOnlyDictionary<string, PineValue>)> valueJsonCache,
        IImmutableSet<CompileInteractiveEnvironmentResult> compileInteractiveEnvironmentResults)
    {
        internal static CompilationCache Empty =>
            new(
                ImmutableDictionary<PineValue, PineValueJson.PineValueMappedForTransport>.Empty,
                ImmutableDictionary<PineValue, (PineValueJson, IReadOnlyDictionary<string, PineValue>)>.Empty,
                ImmutableHashSet<CompileInteractiveEnvironmentResult>.Empty);
    }

    private record CompileElmInteractiveEnvironmentRequest(
        PineValueJson environmentBefore,
        IReadOnlyList<string> modulesTexts);

    private record CompileInteractiveSubmissionRequest(
        PineValueJson environment,
        string submission);

    public static Result<string, EvaluatedSctructure> SubmissionResponseFromResponsePineValue(
        IJsEngine evalElmPreparedJsEngine,
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

            public static bool Equals(PineValueMappedForTransport? left, PineValueMappedForTransport? right)
            {
                if (left is null && right is null)
                    return true;

                if (left is null || right is null)
                    return false;

                if (left.ListAsString is string leftString && right.ListAsString is string rightString)
                    return leftString == rightString;

                return left.origin.Equals(right.origin);
            }

            public static PineValueMappedForTransport FromPineValue(
                PineValue pineValue,
                IDictionary<PineValue, PineValueMappedForTransport>? cache)
            {
                if (cache?.TryGetValue(pineValue, out var mapped) ?? false)
                    return mapped;

                mapped = FromPineValueIgnoringCacheForCurrent(pineValue, cache);

                cache?.Add(pineValue, mapped);

                return mapped;
            }

            private static PineValueMappedForTransport FromPineValueIgnoringCacheForCurrent(
                PineValue pineValue,
                IDictionary<PineValue, PineValueMappedForTransport>? cache)
            {
                if (PineValueAsString.StringFromValue(pineValue) is Result<string, string>.Ok asString)
                    return new PineValueMappedForTransport(ListAsString: asString.Value, List: null, origin: pineValue);

                if (pineValue is PineValue.ListValue listComponent)
                    return new PineValueMappedForTransport(
                        ListAsString: null,
                        List: listComponent.Elements.Select(item => FromPineValue(item, cache)).ToList(),
                        origin: pineValue);

                return new PineValueMappedForTransport(ListAsString: null, List: null, origin: pineValue);
            }
        }

        public static ((PineValueJson json, IReadOnlyDictionary<string, PineValue> dictionary), System.Threading.Tasks.Task<CompilationCache>)
            FromPineValueBuildingDictionary(
            PineValue pineValue,
            CompilationCache compilationCache)
        {
            if (compilationCache.valueJsonCache.TryGetValue(pineValue, out var cached))
                return (cached, System.Threading.Tasks.Task.FromResult(compilationCache));

            var valueMappedForTransportCache = new Dictionary<PineValue, PineValueMappedForTransport>(
                compilationCache.valueMappedForTransportCache);

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

            var decodeResponseDictionary =
                dictionary
                .ToImmutableDictionary(
                    keySelector: entry => entry.Value,
                    elementSelector: entry => entry.Key.origin);

            var cacheEntry = (json: pineValueJson, dictionary: decodeResponseDictionary);

            var compilationCacheTask = System.Threading.Tasks.Task.Run(() =>
            {
                var valueJsonCache = new Dictionary<PineValue, (PineValueJson, IReadOnlyDictionary<string, PineValue>)>(
                    compilationCache.valueJsonCache)
                {
                    [pineValue] = cacheEntry
                };

                return
                    compilationCache
                    with
                    {
                        valueMappedForTransportCache = valueMappedForTransportCache.ToImmutableDictionary(),
                        valueJsonCache = valueJsonCache.ToImmutableDictionary()
                    };
            });

            return (cacheEntry, compilationCacheTask);
        }

        public static PineValueJson FromPineValueWithoutBuildingDictionary(PineValue pineValue) =>
            FromPineValueWithoutBuildingDictionary(PineValueMappedForTransport.FromPineValue(pineValue, cache: null));

        private static PineValueJson FromPineValueWithoutBuildingDictionary(
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

    private static PineValue ParsePineValueFromJson(PineValueJson fromJson, IReadOnlyDictionary<string, PineValue>? dictionary)
    {
        if (fromJson.List is IReadOnlyList<PineValueJson> list)
            return PineValue.List(list.Select(item => ParsePineValueFromJson(item, dictionary)).ToImmutableList());

        if (fromJson.Blob is IReadOnlyList<int> blob)
            return PineValue.Blob(blob.Select(b => (byte)b).ToArray());

        if (fromJson.ListAsString is string listAsString)
            return PineValueAsString.ValueFromString(listAsString);

        if (fromJson.Reference is string reference)
        {
            return
                dictionary switch
                {
                    null => throw new Exception("Cannot resolve reference '" + reference + "' because dictionary is null"),
                    not null => dictionary[reference]
                };
        }

        throw new NotImplementedException("Unexpected shape of Pine value from JSON");
    }

    private static IReadOnlyList<string>? ModulesTextsFromAppCodeTree(TreeNodeWithStringPath? appCodeTree) =>
        appCodeTree == null ? null
        :
        TreeToFlatDictionaryWithPathComparer(CompileTree(appCodeTree)!)
        .Select(appCodeFile => appCodeFile.Key.Last().EndsWith(".elm") ? Encoding.UTF8.GetString(appCodeFile.Value.ToArray()) : null)
        .WhereNotNull()
        .ToImmutableList();

    private static TreeNodeWithStringPath? CompileTree(TreeNodeWithStringPath? sourceTree)
    {
        if (sourceTree == null)
            return null;

        var sourceFiles = TreeToFlatDictionaryWithPathComparer(sourceTree);

        var compilationRootFilePath = sourceFiles.FirstOrDefault(c => c.Key[c.Key.Count - 1].EndsWith(".elm")).Key;

        if (compilationRootFilePath.Count == 0)
            return null;

        var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
            sourceFiles: TreeToFlatDictionaryWithPathComparer(sourceTree),
            workingDirectoryRelative: ImmutableList<string>.Empty,
            ElmAppInterfaceConfig.Default with { compilationRootFilePath = compilationRootFilePath });

        return
            compilationResult
            .Unpack(
                fromErr: compilationError =>
                {
                    var errorMessage = "\n" + ElmAppCompilation.CompileCompilationErrorsDisplayText(compilationError) + "\n";

                    Console.WriteLine(errorMessage);

                    throw new Exception(errorMessage);
                },
                fromOk: compilationOk => SortedTreeFromSetOfBlobsWithStringPath(compilationOk.result.compiledFiles));
    }

    public static IJsEngine PrepareJsEngineToEvaluateElm(InteractiveSessionJavaScript.JavaScriptEngineFlavor javaScriptEngineFlavor) =>
        PrepareJsEngineToEvaluateElm(
            jsEngineFactory: javaScriptEngineFlavor switch
            {
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.Jint => JsEngineJintOptimizedForElmApps.Create,
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8 => JsEngineFromJavaScriptEngineSwitcher.ConstructJsEngine,

                _ => throw new NotImplementedException("Not implemented: " + javaScriptEngineFlavor)
            });

    public static IJsEngine PrepareJsEngineToEvaluateElm(Func<IJsEngine> jsEngineFactory)
    {
        var javascriptEngine = jsEngineFactory();

        javascriptEngine.Evaluate(JavascriptToEvaluateElm.Value);

        return javascriptEngine;
    }

    public static string PrepareJavascriptToEvaluateElm()
    {
        var compileElmProgramCodeFiles = LoadCompileElmProgramCodeFiles();

        var elmMakeResult =
            Elm019Binaries.ElmMakeToJavascript(
                compileElmProgramCodeFiles
                .Extract(error => throw new NotImplementedException(nameof(LoadCompileElmProgramCodeFiles) + ": " + error)),
                workingDirectoryRelative: null,
                ImmutableList.Create("src", "ElmInteractiveMain.elm"));

        var javascriptFromElmMake =
            Encoding.UTF8.GetString(
                elmMakeResult.Extract(err => throw new Exception("Failed elm make: " + err)).producedFile.Span);

        var javascriptMinusCrashes = ProcessFromElm019Code.JavascriptMinusCrashes(javascriptFromElmMake);

        var listFunctionToPublish =
            new[]
            {
                (functionNameInElm: "ElmInteractiveMain.evaluateSubmissionInInteractive",
                publicName: "evaluateSubmissionInInteractive",
                arity: 1),

                (functionNameInElm: "ElmInteractiveMain.getDefaultElmCoreModulesTexts",
                publicName: "getDefaultElmCoreModulesTexts",
                arity: 1),

                (functionNameInElm: "ElmInteractiveMain.compileInteractiveEnvironment",
                publicName: "compileInteractiveEnvironment",
                arity: 1),

                (functionNameInElm: "ElmInteractiveMain.compileInteractiveSubmission",
                publicName: "compileInteractiveSubmission",
                arity: 1),

                (functionNameInElm: "ElmInteractiveMain.submissionResponseFromResponsePineValue",
                publicName: "submissionResponseFromResponsePineValue",
                arity: 1),
            };

        return
            ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                javascriptMinusCrashes,
                listFunctionToPublish);
    }

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadCompileElmProgramCodeFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ImmutableList.Create("ElmTime", "compile-elm-program"),
            assembly: typeof(ElmInteractive).Assembly);

    private record EvaluateSubmissionResponseStructure
        (string? FailedToDecodeArguments = null,
        DecodedArgumentsSctructure? DecodedArguments = null);

    private record DecodedArgumentsSctructure(
        string? FailedToEvaluate = null,
        EvaluatedSctructure? Evaluated = null);

    public record EvaluatedSctructure(
        string displayText);
}
