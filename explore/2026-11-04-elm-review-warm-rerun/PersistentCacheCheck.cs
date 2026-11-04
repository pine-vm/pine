using Pine.Core.Caching;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.IO;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Text.Json;
using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace ElmReviewProfiling;

internal static class PersistentCacheCheck
{
    public static int Run(string directory, bool requireHits)
    {
        var source =
            ElmSyntaxParser.ParseModuleText(
                """
                module CacheProbe exposing (..)
                type alias Item = { value : Int }
                add : Int -> Int
                add n = n + 1
                root item = ( \n -> add n, item.value )
                """).Extract(error => throw new InvalidOperationException(error.ToString()));
        var canonical = Canonicalization.CanonicalizeOrThrow([source])
            .Extract(error => throw new InvalidOperationException(error));
        var declarations = ElmCompiler.FlattenModulesToDeclarationDictionary(
            [.. canonical.Values.Select(module =>
                ConvertFromConcrete.FromFile(module.Extract(error => throw new InvalidOperationException(error))))]);

        var caches = SourceCompilationCaches.CreateInMemory(new FileStoreFromSystemIOFile(directory));
        var operators = new Probe<Declaration>(caches.OperatorLowering, requireHits);
        var semantic = new Probe<LoweredSourceFunction>(caches.SemanticFunctions, requireHits);
        var watch = Stopwatch.StartNew();
        var lowered = BuiltinOperatorLowering.Apply(declarations, operators)
            .Extract(error => throw new InvalidOperationException(error));
        var actual = Model.FromCanonicalSyntax.Lower(lowered, semantic);
        var expected = Model.FromCanonicalSyntax.Lower(
            BuiltinOperatorLowering.Apply(declarations).Extract(error => throw new InvalidOperationException(error)));
        var options = Model.CompilerModelJson.CreateOptions();

        if (JsonSerializer.Serialize(actual.Program, options) != JsonSerializer.Serialize(expected.Program, options) ||
            actual.GenericAccessEvidence.Count != expected.GenericAccessEvidence.Count)
            throw new InvalidOperationException("Persistent transformation results differ from fresh conversion.");

        Console.WriteLine(JsonSerializer.Serialize(new
        {
            Directory = Path.GetFullPath(directory),
            RequireHits = requireHits,
            WallMs = watch.Elapsed.TotalMilliseconds,
            OperatorHits = operators.Hits,
            OperatorMisses = operators.Misses,
            SemanticHits = semantic.Hits,
            SemanticMisses = semantic.Misses,
            OperatorHashes = operators.Hashes,
            SemanticHashes = semantic.Hashes
        }));
        return 0;
    }

    private sealed class Probe<T>(ICache<SourceTransformationInput, T> cache, bool requireHits)
        : ICache<SourceTransformationInput, T>
    {
        public int Hits { get; private set; }
        public int Misses { get; private set; }
        public List<string> Hashes { get; } = [];

        public bool TryGetValue(SourceTransformationInput key, [MaybeNullWhen(false)] out T value)
        {
            Hashes.Add(Convert.ToHexStringLower(key.ComputeHash().Span));
            if (cache.TryGetValue(key, out value))
            {
                Hits++;
                return true;
            }

            Misses++;
            if (requireHits)
                throw new InvalidOperationException("Expected a persisted cache hit: " + key.Name.FullName);
            return false;
        }

        public void Set(SourceTransformationInput key, T value) => cache.Set(key, value);
    }
}
