using Pine.Core.Caching;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Elm.Testing;
using Pine.Core.CodeAnalysis;
using Pine.Core.IO;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Reflection;
using System.Text.Json;
using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace ElmReviewProfiling;

internal static class PersistentCacheCheck
{
    public static int Run(string directory, bool requireHits)
    {
        var declarationName =
            (typeof(Canonicalization).GetMethod("GetDeclarationName", BindingFlags.Static | BindingFlags.NonPublic)
            ?? throw new InvalidOperationException("Update cache-check declaration naming for this compiler version."))
            .CreateDelegate<Func<Pine.Core.Elm.Syntax.Concrete.Declaration, string>>();
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

        var store = new FileStoreFromSystemIOFile(directory);
        var caches = SourceCompilationCaches.CreateInMemory(store);
        var operators = new Probe<SourceTransformationInput, Declaration>(
            caches.OperatorLowering, requireHits, input => input.ComputeHash());
        var semantic = new Probe<SourceTransformationInput, LoweredSourceFunction>(
            caches.SemanticFunctions, requireHits, input => input.ComputeHash());
        var discoveryCaches = ElmTestDiscoveryCaches.CreateInMemory(store);
        var prepared = new Probe<PreparedDeclarationInput, PreparedSourceDeclaration>(
            discoveryCaches.PreparedDeclarations, requireHits, input => input.ComputeHash());
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

        foreach (var declaration in canonical.Values.Single().Extract(error => throw new InvalidOperationException(error)).Declarations)
        {
            var input = PreparedDeclarationInput.FromConcrete(
                DeclQualifiedName.Create(["CacheProbe"], declarationName(declaration.Value)),
                declaration.Value);
            if (!prepared.TryGetValue(input, out var value))
            {
                value = PreparedSourceDeclaration.Prepare(input.Syntax);
                prepared.Set(input, value);
            }

            var codec = PreparedSourceDeclaration.Codec;
            if (!codec.Encode(value).Span.SequenceEqual(
                codec.Encode(PreparedSourceDeclaration.Prepare(input.Syntax)).Span))
                throw new InvalidOperationException("Persistent preparation differs from fresh conversion.");
            _ = value.ExpressionPaths;
        }

        Console.WriteLine(JsonSerializer.Serialize(new
        {
            Directory = Path.GetFullPath(directory),
            RequireHits = requireHits,
            WallMs = watch.Elapsed.TotalMilliseconds,
            OperatorHits = operators.Hits,
            OperatorMisses = operators.Misses,
            SemanticHits = semantic.Hits,
            SemanticMisses = semantic.Misses,
            PreparedHits = prepared.Hits,
            PreparedMisses = prepared.Misses,
            OperatorHashes = operators.Hashes,
            SemanticHashes = semantic.Hashes,
            PreparedHashes = prepared.Hashes
        }));
        return 0;
    }

    private sealed class Probe<TKey, T>(
        ICache<TKey, T> cache, bool requireHits, Func<TKey, ReadOnlyMemory<byte>> hash)
        : ICache<TKey, T> where TKey : notnull
    {
        public int Hits { get; private set; }
        public int Misses { get; private set; }
        public List<string> Hashes { get; } = [];

        public bool TryGetValue(TKey key, [MaybeNullWhen(false)] out T value)
        {
            Hashes.Add(Convert.ToHexStringLower(hash(key).Span));
            if (cache.TryGetValue(key, out value))
            {
                Hits++;
                return true;
            }

            Misses++;
            if (requireHits)
                throw new InvalidOperationException("Expected a persisted cache hit: " + Hashes[^1]);
            return false;
        }

        public void Set(TKey key, T value) => cache.Set(key, value);
    }
}
