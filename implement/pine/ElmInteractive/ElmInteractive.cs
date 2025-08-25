using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.Core.Elm;
using Pine.Elm;
using Pine.Elm019;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json.Serialization;
using static Pine.Core.PineValueComposition;

namespace ElmTime.ElmInteractive;

public class ElmInteractive
{
    public static long EstimatePineValueMemoryUsage(PineValue pineValue)
    {
        if (pineValue is PineValue.ListValue listValue)
        {
            return 100 + listValue.NodesCount * 100 + listValue.BlobsBytesCount;
        }

        if (pineValue is PineValue.BlobValue blobValue)
        {
            return blobValue.Bytes.Length + 100;
        }

        throw new NotImplementedException(
            "Unexpected value type: " + pineValue.GetType().FullName);
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

            var selfValue = PineValue.List([.. lastIncrementModulesBlobs.Select(PineValue.Blob)]);

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

    public static Result<string, EvaluatedStruct> SubmissionResponseFromResponsePineValue(
        PineValue response) =>
        ElmValueEncoding.PineValueAsElmValue(response, null, null)
        .MapError(err => "Failed to convert Pine value to Elm value: " + err)
        .Map(elmValue => new EvaluatedStruct(ElmValue.RenderAsElmExpression(elmValue).expressionString));

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
                if (0 < listComponent.Items.Length)
                {
                    if (StringEncoding.StringFromValue(pineValue) is Result<string, string>.Ok asString)
                        return new PineValueMappedForTransport(
                            ListAsString: asString.Value,
                            BlobAsInt: null,
                            List: null,
                            Origin: pineValue);
                }

                return new PineValueMappedForTransport(
                    ListAsString: null,
                    BlobAsInt: null,
                    List: [.. listComponent.Items.ToArray().Select(item => FromPineValue(item, cache))],
                    Origin: pineValue);
            }

            if (pineValue is PineValue.BlobValue blobValue)
            {
                if (1 < blobValue.Bytes.Length && blobValue.Bytes.Length < 3)
                {
                    if (IntegerEncoding.ParseSignedIntegerStrict(blobValue.Bytes.Span) is Result<string, System.Numerics.BigInteger>.Ok asInt)
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
                    if (list.Items.Length < 1)
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

        var keyIndex = 0;

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
            return StringEncoding.ValueFromString(listAsString);

        if (fromJson.BlobAsInt is { } asInt)
            return IntegerEncoding.EncodeSignedInteger(asInt);

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

    public static bool ShouldIgnoreSourceFile(IReadOnlyList<string> filePath, ReadOnlyMemory<byte> fileContent)
    {
        /*
         * Adapt to observation 2024-10-25:
         * Unhandled exception. System.Exception: Failed compilation: Failed to prepare the initial context: Failed to compile elm module 'Reporter': Failed to compile declaration: Failed to compile function 'main': Failed to compile Elm function syntax: Did not find module 'ElmTestRunner.Reporter'. There are 18 declarations in this scope: Basics, Bitwise, Bytes, Bytes.Decode, Bytes.Encode, Char, Dict, Elm.Kernel.Parser, Hex, Json.Decode, Json.Encode, Kernel.Json.Decode, Kernel.Json.Encode, List, Maybe, Result, String, Tuple
         * 
         * It turns out a tool had created a "port module Reporter" and "port module Runner"
         * in "elm-compiler\elm-stuff\tests-0.19.1\src\Reporter.elm"
         * */

        if (filePath.Contains("elm-stuff"))
            return true;

        return false;
    }

    public static IReadOnlyList<(IReadOnlyList<string> filePath, ReadOnlyMemory<byte> fileContent, string moduleText)>
        ModulesFilePathsAndTextsFromAppCodeTree(
        TreeNodeWithStringPath appCodeTree,
        bool skipLowering,
        IReadOnlySet<IReadOnlyList<string>>? entryPointsFilePaths,
        bool skipFilteringForSourceDirs,
        bool mergeKernelModules)
    {
        var loweredTree =
            skipLowering
            ?
            appCodeTree
            :
            CompileTree(appCodeTree);

        // After lowering we can remove all Elm modules not in the dependency tree of the entry points.

        var treeFiltered =
            entryPointsFilePaths is null || entryPointsFilePaths.Count < 1
            ?
            loweredTree
            :
            ElmCompiler.FilterTreeForCompilationRoots(
                loweredTree,
                entryPointsFilePaths,
                skipFilteringForSourceDirs: skipFilteringForSourceDirs);

        var treeWithKernelModules =
            mergeKernelModules
            ?
            InteractiveSessionPine.MergeDefaultElmCoreAndKernelModules(treeFiltered)
            :
            treeFiltered;

        return
            [.. TreeToFlatDictionaryWithPathComparer(treeWithKernelModules)
            .Where(sourceFile => sourceFile.Key.Last().EndsWith(".elm"))
            .Select(appCodeFile => (appCodeFile.Key, appCodeFile.Value, Encoding.UTF8.GetString(appCodeFile.Value.Span)))
            ];
    }

    private static TreeNodeWithStringPath CompileTree(TreeNodeWithStringPath sourceTree)
    {
        if (sourceTree.GetNodeAtPath(["elm.json"]) is not TreeNodeWithStringPath.BlobNode elmJsonFile)
            return sourceTree;

        var elmJsonFileParsed =
            System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(elmJsonFile.Bytes.Span);

        IReadOnlyList<IReadOnlyList<string>> elmJsonSourceDirectories =
            [..elmJsonFileParsed?.SourceDirectories
            .Select(flat => flat.Split('/', '\\'))
            ];

        bool filePathIsUnderElmJsonSourceDirectories(IReadOnlyList<string> filePath)
        {
            return
                elmJsonSourceDirectories
                .Any(sourceDir => filePath.Take(sourceDir.Count).SequenceEqual(sourceDir));
        }

        var sourceFiles = TreeToFlatDictionaryWithPathComparer(sourceTree);

        if (sourceFiles.Count is 0)
            return sourceTree;

        var compilationRootFilePath =
            sourceFiles
            .Where(c => c.Key[c.Key.Count - 1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .OrderBy(c => c.Key.Count)
            .OrderBy(c => filePathIsUnderElmJsonSourceDirectories(c.Key) ? 0 : 1)
            .FirstOrDefault()
            .Key;

        if (compilationRootFilePath.Count is 0)
            return sourceTree;

        var compilationResult = ElmAppCompilation.AsCompletelyLoweredElmApp(
            sourceFiles: TreeToFlatDictionaryWithPathComparer(sourceTree),
            workingDirectoryRelative: [],
            ElmAppInterfaceConfig.Default with { CompilationRootFilePath = compilationRootFilePath });

        return
            compilationResult
            .Unpack(
                fromErr: compilationError =>
                {
                    var errorMessage = "\n" + ElmAppCompilation.CompileCompilationErrorsDisplayText(compilationError) + "\n";

                    Console.WriteLine(errorMessage);

                    throw new Exception(errorMessage);
                },
                fromOk: compilationOk => SortedTreeFromSetOfBlobsWithStringPath(compilationOk.Result.CompiledFiles));
    }

    public record EvaluatedStruct(
        string DisplayText);

    public static readonly System.Text.Json.JsonSerializerOptions compilerInterfaceJsonSerializerOptions =
        new()
        {
            DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
            MaxDepth = 1000
        };
}
