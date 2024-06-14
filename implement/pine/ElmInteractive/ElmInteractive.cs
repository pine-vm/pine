using ElmTime.Elm019;
using ElmTime.JavaScript;
using Pine;
using Pine.ElmInteractive;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using static Pine.PineValueComposition;

namespace ElmTime.ElmInteractive;

public class ElmInteractive
{
    public static IFileStore CompiledModulesCacheFileStoreDefault =>
        new FileStoreFromSystemIOFile(
            Path.Combine(Filesystem.CacheDirectory, "elm-interactive-compiled-modules", Program.AppVersionId));

    public static ICompiledModulesCache CompiledModulesCacheDefault =>
        new CompiledModulesFileCache(CompiledModulesCacheFileStoreDefault);

    public static readonly ConcurrentDictionary<TreeNodeWithStringPath, System.Threading.Tasks.Task<string>>
        JavaScriptToEvaluateElmFromCompilerTask = new();

    public static System.Threading.Tasks.Task<string> JavaScriptToEvaluateElmFromCompilerCachedTask(
        TreeNodeWithStringPath compileElmProgramCodeFiles) =>
        JavaScriptToEvaluateElmFromCompilerTask.GetOrAdd(
            compileElmProgramCodeFiles,
            valueFactory:
            compileElmProgramCodeFiles =>
            System.Threading.Tasks.Task.Run(() => PrepareJavaScriptToEvaluateElm(compileElmProgramCodeFiles)));

    public static Result<string, EvaluatedStruct> EvaluateSubmissionAndGetResultingValue(
        IJavaScriptEngine evalElmPreparedJavaScriptEngine,
        TreeNodeWithStringPath? appCodeTree,
        string submission,
        IReadOnlyList<string>? previousLocalSubmissions = null)
    {
        var modulesTexts = ModulesTextsFromAppCodeTree(appCodeTree);

        var argumentsJson = System.Text.Json.JsonSerializer.Serialize(
            new
            {
                modulesTexts = modulesTexts ?? [],
                submission = submission,
                previousLocalSubmissions = previousLocalSubmissions ?? [],
            }
        );

        var responseJson =
            evalElmPreparedJavaScriptEngine.CallFunction("evaluateSubmissionInInteractive", argumentsJson).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<EvaluateSubmissionResponseStructure>(responseJson)!;

        if (responseStructure.DecodedArguments is null)
            throw new Exception("Failed to decode arguments: " + responseStructure.FailedToDecodeArguments);

        if (responseStructure.DecodedArguments.Evaluated is null)
            return responseStructure.DecodedArguments.FailedToEvaluate!;

        return
            responseStructure.DecodedArguments.Evaluated;
    }

    public static IReadOnlyList<string> GetDefaultElmCoreModulesTexts(
        IJavaScriptEngine evalElmPreparedJavaScriptEngine)
    {
        var responseJson =
            evalElmPreparedJavaScriptEngine.CallFunction("getDefaultElmCoreModulesTexts", 0).ToString()!;

        return
            System.Text.Json.JsonSerializer.Deserialize<IReadOnlyList<string>>(responseJson)!;
    }

    internal static Result<string, (PineValue compileResult, CompilationCache compilationCache)> CompileInteractiveEnvironment(
        IJavaScriptEngine evalElmPreparedJavaScriptEngine,
        PineValue? initialState,
        TreeNodeWithStringPath? appCodeTree,
        CompilationCache compilationCacheBefore)
    {
        var allModulesTexts =
            GetDefaultElmCoreModulesTexts(evalElmPreparedJavaScriptEngine)
            .Concat(ModulesTextsFromAppCodeTree(appCodeTree) ?? [])
            .ToImmutableList();

        return
            CompileInteractiveEnvironmentForModulesCachingIncrements(
                initialState: initialState,
                elmModulesTextsBeforeSort: allModulesTexts,
                evalElmPreparedJavaScriptEngine,
                compilationCacheBefore)
            .Map(compileResultAndCache =>
            (compileResult: compileResultAndCache.compileResult.environmentPineValue,
            compileResultAndCache.compilationCache));
    }

    private static Result<string, (CompileInteractiveEnvironmentResult compileResult, CompilationCache compilationCache)>
        CompileInteractiveEnvironmentForModulesCachingIncrements(
        PineValue? initialState,
        IReadOnlyList<string> elmModulesTextsBeforeSort,
        IJavaScriptEngine evalElmPreparedJavaScriptEngine,
        CompilationCache compilationCacheBefore)
    {
        if (initialState is not null)
            throw new NotImplementedException("initialState is not implemented");

        var elmModulesTexts =
            ElmSyntax.ElmModule.ModulesTextOrderedForCompilationByDependencies(elmModulesTextsBeforeSort)
            .ToImmutableList();

        var baseResults =
            compilationCacheBefore.CompileInteractiveEnvironmentResults
            .Where(cachedResult =>
            {
                var cachedResultAllModules = cachedResult.AllModulesTextsList;

                return elmModulesTexts.Take(cachedResultAllModules.Count).SequenceEqual(cachedResultAllModules);
            })
            .ToImmutableList();

        var closestBaseInMemory =
            baseResults
            .OrderByDescending(c => c.AllModulesTextsList.Count)
            .OfType<CompileInteractiveEnvironmentResult?>()
            .FirstOrDefault();

        var elmModulesTextsFromBaseInMemory =
            closestBaseInMemory is null ?
            elmModulesTexts :
            elmModulesTexts
            .Skip(closestBaseInMemory.AllModulesTextsList.Count)
            .ToImmutableList();

        var closestBaseFromFile =
            CompiledModulesCacheDefault.GetClosestBase(elmModulesTexts);

        var elmModulesTextsFromBaseFromFile =
            closestBaseFromFile.HasValue ?
            elmModulesTexts
            .Skip(closestBaseFromFile.Value.CompiledModules.Count)
            .ToImmutableList() :
            elmModulesTexts;

        Result<string, (CompileInteractiveEnvironmentResult compileResult, CompilationCache compilationCache)> chooseInitResult()
        {
            if (closestBaseFromFile.HasValue)
            {
                return (new CompileInteractiveEnvironmentResult(
                    lastIncrementModulesTexts: closestBaseFromFile.Value.CompiledModules,
                    environmentPineValueJson: closestBaseFromFile.Value.CompiledValueJson,
                    environmentPineValue: closestBaseFromFile.Value.CompiledValue,
                    environmentDictionary: ImmutableDictionary<string, PineValue>.Empty,
                    parent: null),
                    compilationCacheBefore);
            }

            if (closestBaseInMemory is not null)
            {
                return (closestBaseInMemory, compilationCacheBefore);
            }

            return
                CompileInteractiveEnvironmentForModules(
                    elmModulesTexts: [],
                    evalElmPreparedJavaScriptEngine: evalElmPreparedJavaScriptEngine,
                    parentEnvironment: null,
                    compilationCacheBefore: compilationCacheBefore);
        }

        return
            chooseInitResult()
            .AndThen(seed =>
            {
                var elmModulesTextsFromBase =
                    elmModulesTexts
                    .Skip(seed.compileResult.AllModulesTextsList.Count)
                    .ToImmutableList();

                return
                    ResultExtension.AggregateExitingOnFirstError(
                        sequence: elmModulesTextsFromBase,
                        aggregateFunc: (prev, elmCoreModuleText) =>
                        {
                            var resultBeforeCache =
                            CompileInteractiveEnvironmentForModules(
                                elmModulesTexts: [elmCoreModuleText],
                                evalElmPreparedJavaScriptEngine: evalElmPreparedJavaScriptEngine,
                                parentEnvironment: prev.compileResult,
                                compilationCacheBefore: prev.compilationCache);

                            return
                            resultBeforeCache
                            .Map(beforeCache =>
                            {
                                CompiledModulesCacheDefault.Set(
                                    beforeCache.compileResult.AllModulesTextsList,
                                    beforeCache.compileResult.environmentPineValue);

                                var compileInteractiveEnvironmentResults =
                                beforeCache.compilationCache.CompileInteractiveEnvironmentResults.Add(beforeCache.compileResult);

                                var cache = beforeCache.compilationCache
                                with
                                {
                                    CompileInteractiveEnvironmentResults = compileInteractiveEnvironmentResults
                                };

                                return (beforeCache.compileResult, cache);
                            });
                        },
                        aggregateSeed: seed);
            });
    }

    private static Result<string, (CompileInteractiveEnvironmentResult compileResult, CompilationCache compilationCache)>
        CompileInteractiveEnvironmentForModules(
        IReadOnlyList<string> elmModulesTexts,
        CompileInteractiveEnvironmentResult? parentEnvironment,
        IJavaScriptEngine evalElmPreparedJavaScriptEngine,
        CompilationCache compilationCacheBefore)
    {
        var environmentBefore =
            parentEnvironment is null ?
            FromPineValueWithoutBuildingDictionary(PineValue.EmptyList) :
            parentEnvironment.environmentPineValueJson;

        var argumentsJson = System.Text.Json.JsonSerializer.Serialize(
            new CompileElmInteractiveEnvironmentRequest(
                modulesTexts: elmModulesTexts,
                environmentBefore: environmentBefore),
            options: compilerInterfaceJsonSerializerOptions);

        var responseJson =
            evalElmPreparedJavaScriptEngine.CallFunction("compileInteractiveEnvironment", argumentsJson).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<Result<string, PineValueJson>>(
                responseJson,
                compilerInterfaceJsonSerializerOptions)!;

        return
            responseStructure
            .Map(fromJson =>
            {
                var parentEnvDictionary =
                MergePineValueFromJsonDictionary(
                    environmentBefore!,
                    parentEnvironment?.environmentDictionary.ToImmutableDictionary());

                var environmentPineValue =
                ParsePineValueFromJson(
                    fromJson!,
                    parentDictionary: parentEnvDictionary);

                var (environmentJson, compilationCacheTask) =
                FromPineValueBuildingDictionary(
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

    public static long EstimatePineValueMemoryUsage(PineValue pineValue)
    {
        if (pineValue is PineValue.BlobValue blobValue)
            return blobValue.Bytes.Length + 100;

        if (pineValue is PineValue.ListValue listValue)
        {
            long sum = 0;

            for (int i = 0; i < listValue.Elements.Count; i++)
            {
                sum += EstimatePineValueMemoryUsage(listValue.Elements[i]);
            }

            return sum + 100;
        }

        throw new NotImplementedException("Not implemented for value type: " + pineValue.GetType().FullName);
    }

    public record CompileInteractiveEnvironmentResult(
        IReadOnlyList<string> lastIncrementModulesTexts,
        PineValueJson environmentPineValueJson,
        PineValue environmentPineValue,
        IReadOnlyDictionary<string, PineValue> environmentDictionary,
        CompileInteractiveEnvironmentResult? parent) : IEquatable<CompileInteractiveEnvironmentResult>
    {
        public IReadOnlyList<string> AllModulesTextsList =>
            parent is null ? lastIncrementModulesTexts : [.. parent.AllModulesTextsList, .. lastIncrementModulesTexts];

        public ReadOnlyMemory<byte> Hash => HashCache.Value;

        private Lazy<ReadOnlyMemory<byte>> HashCache => new(ComputeHash);

        private ReadOnlyMemory<byte> ComputeHash()
        {
            var lastIncrementModulesBlobs =
                lastIncrementModulesTexts.Select(Encoding.UTF8.GetBytes).ToArray();

            var selfValue = PineValue.List(lastIncrementModulesBlobs.Select(PineValue.Blob).ToImmutableList());

            var selfHash = PineValueHashTree.ComputeHash(selfValue);

            if (parent is null)
                return selfHash;

            return PineValueHashTree.ComputeHash(PineValue.List([PineValue.Blob(selfHash), PineValue.Blob(parent.Hash)]));
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
        IJavaScriptEngine evalElmPreparedJavaScriptEngine,
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
            FromPineValueBuildingDictionary(environment, compilationCacheBefore);

        var requestJson =
            System.Text.Json.JsonSerializer.Serialize(
                new CompileInteractiveSubmissionRequest
                (
                    environment: environmentJson.json,
                    submission: submission
                ),
                options: compilerInterfaceJsonSerializerOptions);

        logDuration("Serialize to JSON (" + CommandLineInterface.FormatIntegerForDisplay(requestJson.Length) + " chars)");

        clock.Restart();

        var responseJson =
            evalElmPreparedJavaScriptEngine.CallFunction("compileInteractiveSubmission", requestJson).ToString()!;

        logDuration("JavaScript function");

        clock.Restart();

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<Result<string, PineValueJson>>(
                responseJson,
                options: compilerInterfaceJsonSerializerOptions)!;

        var response =
            responseStructure
            .Map(fromJson => ParsePineValueFromJson(
                fromJson,
                parentDictionary: environmentJson.dictionary.ToImmutableDictionary()));

        logDuration("Deserialize (from " + CommandLineInterface.FormatIntegerForDisplay(responseJson.Length) + " chars) and " + nameof(ParsePineValueFromJson));

        return response.Map(value => (value, compilationCacheTask.Result));
    }

    public record CompilationCache(
        IImmutableDictionary<PineValue, PineValueMappedForTransport> ValueMappedForTransportCache,
        IImmutableDictionary<PineValue, (PineValueJson, IReadOnlyDictionary<string, PineValue>)> ValueJsonCache,
        IImmutableSet<CompileInteractiveEnvironmentResult> CompileInteractiveEnvironmentResults)
    {
        public static CompilationCache Empty =>
            new(
                ImmutableDictionary<PineValue, PineValueMappedForTransport>.Empty,
                ImmutableDictionary<PineValue, (PineValueJson, IReadOnlyDictionary<string, PineValue>)>.Empty,
                ImmutableHashSet<CompileInteractiveEnvironmentResult>.Empty);
    }

    private record CompileElmInteractiveEnvironmentRequest(
        PineValueJson environmentBefore,
        IReadOnlyList<string> modulesTexts);

    private record CompileInteractiveSubmissionRequest(
        PineValueJson environment,
        string submission);

    public static Result<string, EvaluatedStruct> SubmissionResponseFromResponsePineValue(
        IJavaScriptEngine evalElmPreparedJavaScriptEngine,
        PineValue response)
    {
        var responseJson =
            evalElmPreparedJavaScriptEngine.CallFunction(
                "submissionResponseFromResponsePineValue",
                System.Text.Json.JsonSerializer.Serialize(
                    FromPineValueWithoutBuildingDictionary(response),
                    options: compilerInterfaceJsonSerializerOptions)).ToString()!;

        var responseStructure =
            System.Text.Json.JsonSerializer.Deserialize<Result<string, EvaluatedStruct>>(responseJson)!;

        return responseStructure;
    }

    public static Result<string, EvaluatedStruct> SubmissionResponseFromResponsePineValue(
        PineValue response) =>
        ElmValueEncoding.PineValueAsElmValue(response)
        .MapError(err => "Failed to convert Pine value to Elm value: " + err)
        .Map(elmValue => new EvaluatedStruct(ElmValue.ElmValueAsExpression(elmValue).expressionString));

    public record PineValueJson
    {
        public IReadOnlyCollection<DictionaryEntry>? Dictionary { init; get; } = null;

        public IReadOnlyList<PineValueJson>? List { init; get; }

        public IReadOnlyList<int>? Blob { init; get; }

        public string? ListAsString { init; get; }

        public int? BlobAsInt { init; get; }

        public string? Reference { init; get; }

        public record DictionaryEntry(string key, PineValueJson value);
    }

    public record PineValueMappedForTransport(
        string? ListAsString,
        int? BlobAsInt,
        IReadOnlyList<PineValueMappedForTransport>? List,
        PineValue Origin)
        : IEquatable<PineValueMappedForTransport>
    {
        public virtual bool Equals(PineValueMappedForTransport? other) =>
            Equals(this, other);

        public override int GetHashCode()
        {
            if (ListAsString is { } asString)
                return asString.GetHashCode();

            if (BlobAsInt is { } asInt)
                return asInt;

            return Origin.GetHashCode();
        }

        public static bool Equals(PineValueMappedForTransport? left, PineValueMappedForTransport? right)
        {
            if (ReferenceEquals(left, right))
                return true;

            if (left is null || right is null)
                return false;

            if (left.ListAsString is { } leftString && right.ListAsString is { } rightString)
                return leftString == rightString;

            if (left.BlobAsInt != right.BlobAsInt)
                return false;

            return left.Origin.Equals(right.Origin);
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
            if (pineValue is PineValue.ListValue listComponent)
            {
                if (0 < listComponent.Elements.Count)
                {
                    if (PineValueAsString.StringFromValue(pineValue) is Result<string, string>.Ok asString)
                        return new PineValueMappedForTransport(
                            ListAsString: asString.Value,
                            BlobAsInt: null,
                            List: null,
                            Origin: pineValue);
                }

                return new PineValueMappedForTransport(
                    ListAsString: null,
                    BlobAsInt: null,
                    List: listComponent.Elements.Select(item => FromPineValue(item, cache)).ToList(),
                    Origin: pineValue);
            }

            if (pineValue is PineValue.BlobValue blobValue)
            {
                if (1 < blobValue.Bytes.Length && blobValue.Bytes.Length < 3)
                {
                    if (PineValueAsInteger.SignedIntegerFromBlobValueStrict(blobValue.Bytes.Span) is Result<string, System.Numerics.BigInteger>.Ok asInt)
                    {
                        return new PineValueMappedForTransport(
                            ListAsString: null,
                            BlobAsInt: (int)asInt.Value,
                            List: null,
                            Origin: pineValue);
                    }
                }
            }

            return new PineValueMappedForTransport(
                ListAsString: null,
                BlobAsInt: null,
                List: null,
                Origin: pineValue);
        }
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

        if (pineValue.ListAsString is { } asString)
            return new PineValueJson { ListAsString = asString };

        if (pineValue.BlobAsInt is { } asInt)
            return new PineValueJson { BlobAsInt = asInt };

        if (pineValue.List is { } asList)
        {
            return new PineValueJson
            {
                List = [.. asList.Select(e => FromPineValueWithoutBuildingDictionary(e, dictionary))]
            };
        }

        if (pineValue.Origin is PineValue.BlobValue blobComponent)
            return new PineValueJson { Blob = [.. blobComponent.Bytes.ToArray()] };

        throw new NotImplementedException("Unexpected shape");
    }

    public static ((PineValueJson json, IReadOnlyDictionary<string, PineValue> dictionary), System.Threading.Tasks.Task<CompilationCache>)
        FromPineValueBuildingDictionary(
        PineValue pineValue,
        CompilationCache compilationCache)
    {
        if (compilationCache.ValueJsonCache.TryGetValue(pineValue, out var cached))
            return (cached, System.Threading.Tasks.Task.FromResult(compilationCache));

        var valueMappedForTransportCache = new Dictionary<PineValue, PineValueMappedForTransport>(
            compilationCache.ValueMappedForTransportCache);

        var intermediate = PineValueMappedForTransport.FromPineValue(pineValue, cache: valueMappedForTransportCache);

        var usageCountLowerBoundDictionary = new Dictionary<PineValueMappedForTransport, int>();

        void mutatingCountUsagesRecursive(PineValueMappedForTransport mappedForTransport)
        {
            {
                if (mappedForTransport.Origin is PineValue.BlobValue blob)
                    if (blob.Bytes.Length < 3)
                        return;

                if (mappedForTransport.Origin is PineValue.ListValue list)
                    if (list.Elements.Count < 1)
                        return;
            }

            if (!usageCountLowerBoundDictionary.TryGetValue(mappedForTransport, out var usageCountLowerBound))
                usageCountLowerBound = 0;

            ++usageCountLowerBound;

            usageCountLowerBoundDictionary[mappedForTransport] = usageCountLowerBound;

            if (1 < usageCountLowerBound)
                return;

            if (mappedForTransport.ListAsString is null && mappedForTransport.List is { } asList)
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

            if (mappedForTransport.List is { } isList)
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
            /*
             * Order the dictionary entries so that earlier entries do not reference later ones.
             * */
            .OrderBy(entry => EstimatePineValueMemoryUsage(entry.Key.Origin))
            .Select(entry => new PineValueJson.DictionaryEntry(
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
                elementSelector: entry => entry.Key.Origin);

        var cacheEntry = (json: pineValueJson, dictionary: decodeResponseDictionary);

        var compilationCacheTask = System.Threading.Tasks.Task.Run(() =>
        {
            var valueJsonCache = new Dictionary<PineValue, (PineValueJson, IReadOnlyDictionary<string, PineValue>)>(
                compilationCache.ValueJsonCache)
            {
                [pineValue] = cacheEntry
            };

            return
                compilationCache
                with
                {
                    ValueMappedForTransportCache = valueMappedForTransportCache.ToImmutableDictionary(),
                    ValueJsonCache = valueJsonCache.ToImmutableDictionary()
                };
        });

        return (cacheEntry, compilationCacheTask);
    }

    public static PineValue ParsePineValueFromJson(
        PineValueJson fromJson,
        ImmutableDictionary<string, PineValue>? parentDictionary)
    {
        var dictionary = MergePineValueFromJsonDictionary(fromJson, parentDictionary);

        if (fromJson.List is { } list)
            return PineValue.List([.. list.Select(item => ParsePineValueFromJson(item, dictionary))]);

        if (fromJson.Blob is { } blob)
            return PineValue.Blob([.. blob.Select(b => (byte)b)]);

        if (fromJson.ListAsString is { } listAsString)
            return PineValueAsString.ValueFromString(listAsString);

        if (fromJson.BlobAsInt is { } asInt)
            return PineValueAsInteger.ValueFromSignedInteger(asInt);

        if (fromJson.Reference is { } reference)
        {
            return
                dictionary switch
                {
                    null =>
                    throw new Exception("Cannot resolve reference '" + reference + "' because dictionary is null"),

                    not null =>
                    dictionary[reference]
                };
        }

        throw new NotImplementedException("Unexpected shape of Pine value from JSON");
    }


    public static ImmutableDictionary<string, PineValue>? MergePineValueFromJsonDictionary(
        PineValueJson fromJson,
        ImmutableDictionary<string, PineValue>? parentDictionary)
    {
        var dictionary = parentDictionary;

        if (fromJson.Dictionary is { } dictionaryEntries)
        {
            dictionary =
                dictionaryEntries
                .Aggregate(
                    seed: dictionary ?? ImmutableDictionary<string, PineValue>.Empty,
                    func: (dictionary, entry) =>
                    dictionary.Add(entry.key, ParsePineValueFromJson(entry.value, dictionary)));
        }

        return dictionary;
    }

    private static IReadOnlyList<string>? ModulesTextsFromAppCodeTree(TreeNodeWithStringPath? appCodeTree) =>
        appCodeTree == null ?
        null
        :
        CompileTree(appCodeTree) is { } compiledTree ?
        TreeToFlatDictionaryWithPathComparer(compiledTree)
        .Select(appCodeFile => appCodeFile.Key.Last().EndsWith(".elm") ? Encoding.UTF8.GetString(appCodeFile.Value.Span) : null)
        .WhereNotNull()
        .ToImmutableList()
        :
        null;

    private static TreeNodeWithStringPath? CompileTree(TreeNodeWithStringPath? sourceTree)
    {
        if (sourceTree is null)
            return null;

        var sourceFiles = TreeToFlatDictionaryWithPathComparer(sourceTree);

        if (sourceFiles.Count == 0)
            return null;

        var compilationRootFilePath = sourceFiles.FirstOrDefault(c => c.Key[c.Key.Count - 1].EndsWith(".elm")).Key;

        if (compilationRootFilePath.Count == 0)
            return null;

        var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
            sourceFiles: TreeToFlatDictionaryWithPathComparer(sourceTree),
            workingDirectoryRelative: [],
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

    public static IJavaScriptEngine PrepareJavaScriptEngineToEvaluateElm(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        InteractiveSessionJavaScript.JavaScriptEngineFlavor javaScriptEngineFlavor) =>
        PrepareJavaScriptEngineToEvaluateElm(
            compileElmProgramCodeFiles,
            javaScriptEngineFactory: javaScriptEngineFlavor switch
            {
                InteractiveSessionJavaScript.JavaScriptEngineFlavor.Jint =>
                JavaScriptEngineJintOptimizedForElmApps.Create,

                InteractiveSessionJavaScript.JavaScriptEngineFlavor.V8 =>
                JavaScriptEngineFromJavaScriptEngineSwitcher.ConstructJavaScriptEngine,

                _ => throw new NotImplementedException("Not implemented: " + javaScriptEngineFlavor)
            });

    public static IJavaScriptEngine PrepareJavaScriptEngineToEvaluateElm(
        TreeNodeWithStringPath compileElmProgramCodeFiles,
        Func<IJavaScriptEngine> javaScriptEngineFactory)
    {
        var javaScriptEngine = javaScriptEngineFactory();

        javaScriptEngine.Evaluate(JavaScriptToEvaluateElmFromCompilerCachedTask(compileElmProgramCodeFiles).Result);

        return javaScriptEngine;
    }

    public static string PrepareJavaScriptToEvaluateElm(TreeNodeWithStringPath compileElmProgramCodeFiles) =>
        PrepareJavaScriptToEvaluateElm(TreeToFlatDictionaryWithPathComparer(compileElmProgramCodeFiles));

    public static string PrepareJavaScriptToEvaluateElm(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> compileElmProgramCodeFiles)
    {
        var elmMakeResult =
        Elm019Binaries.ElmMakeToJavascript(
            compileElmProgramCodeFiles,
            workingDirectoryRelative: null,
            ["src", "ElmInteractiveMain.elm"]);

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

                (functionNameInElm: "ElmInteractiveMain.parseElmModuleTextToPineValue",
                publicName: "parseElmModuleTextToPineValue",
                arity: 1),
            };

        return
            ProcessFromElm019Code.PublishFunctionsFromJavascriptFromElmMake(
                javascriptMinusCrashes,
                listFunctionToPublish);
    }

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadCompileElmProgramCodeFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["ElmTime", "compile-elm-program"],
            assembly: typeof(ElmInteractive).Assembly);

    private record EvaluateSubmissionResponseStructure
        (string? FailedToDecodeArguments = null,
        DecodedArgumentsStruct? DecodedArguments = null);

    private record DecodedArgumentsStruct(
        string? FailedToEvaluate = null,
        EvaluatedStruct? Evaluated = null);

    public record EvaluatedStruct(
        string DisplayText);

    public static readonly System.Text.Json.JsonSerializerOptions compilerInterfaceJsonSerializerOptions =
        new()
        {
            DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
            MaxDepth = 1000
        };
}
