using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.IO;
using Pine.Core.Json;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Exact anonymous input to a source transformation. EnvironmentHash covers consulted type metadata;
/// Configuration includes the transformation's algorithm version and every lowering option.
/// It contains neither interpreter closures nor captured runtime values.
/// </summary>
public sealed record SourceTransformationInput(
    DeclQualifiedName Name, Declaration Syntax, string EnvironmentHash, string Configuration)
{
    private sealed record Digest(ReadOnlyMemory<byte> Bytes);

    private sealed class SyntaxDocument
    {
        private byte[]? bytes;

        public string Hash { get; }

        public Lazy<Declaration> Normalized { get; }

        public SyntaxDocument(Declaration syntax)
        {
            bytes = JsonSerializer.SerializeToUtf8Bytes(syntax, ElmSyntaxAbstractJson.JsonSerializerOptions);
            Hash = Convert.ToHexStringLower(SHA256.HashData(bytes));

            Normalized =
                new(
                    () =>
                    {
                        var normalized =
                            JsonSerializer.Deserialize<Declaration>(
                                bytes ?? throw new InvalidOperationException("Missing source syntax document."),
                                ElmSyntaxAbstractJson.JsonSerializerOptions) ??
                            throw new JsonException("Expected a lowered source declaration.");

                        bytes = null;
                        return normalized;
                    });
        }
    }

    private static readonly ConditionalWeakTable<Declaration, SyntaxDocument> s_syntaxDocuments = [];

    private static readonly ConditionalWeakTable<SourceTransformationInput, Digest> s_inputHashes = [];

    internal static string SyntaxHash(Declaration syntax) =>
        s_syntaxDocuments.GetValue(syntax, value => new(value)).Hash;

    internal static Declaration NormalizeSyntax(Declaration syntax)
    {
        var document = s_syntaxDocuments.GetValue(syntax, value => new(value));
        var normalized = document.Normalized.Value;
        s_syntaxDocuments.TryAdd(normalized, document);
        return normalized;
    }

    /// <summary>Memoizes syntax documents and complete content addresses with weak immutable ownership.</summary>
    public ReadOnlyMemory<byte> ComputeHash() =>
        s_inputHashes.GetValue(
            this,
            input => new(
                SHA256.HashData(
                    JsonSerializer.SerializeToUtf8Bytes(
                        new[]
                        {
                            "pine-source-transformation-v6", input.Configuration, input.Name.FullName, input.EnvironmentHash,
                            SyntaxHash(input.Syntax)
                        })))).Bytes;
}

/// <summary>One resolved function, including lexical spellings and identity-bound analysis evidence.</summary>
public sealed record LoweredSourceFunction(
    Model.Function Function,
    ImmutableDictionary<Model.BindingId, string> BindingNames,
    ImmutableDictionary<Model.Expression, GenericRecordAccessEvidence> Evidence);

/// <summary>
/// Independently configurable operator and semantic caches. Storage interfaces impose no lifetime policy.
/// Set the optional lifetime controls when the corresponding storage has a marker-managed memory layer.
/// </summary>
public sealed record SourceCompilationCaches(
    ICache<SourceTransformationInput, Declaration> OperatorLowering,
    ICache<SourceTransformationInput, LoweredSourceFunction> SemanticFunctions)
{
    /// <summary>Optional control of the operator cache's in-memory tier.</summary>
    public IMarkerCacheLifetime<long>? OperatorLifetime { get; init; }

    /// <summary>Optional control of the semantic cache's in-memory tier.</summary>
    public IMarkerCacheLifetime<long>? SemanticLifetime { get; init; }

    /// <summary>Creates independent thread-safe in-memory tiers, optionally backed by a file-store subtree.</summary>
    public static SourceCompilationCaches CreateInMemory(
        IFileStore? backingStore = null, IImmutableList<string>? directory = null)
    {
        var operators = new MemoryCache<SourceTransformationInput, Declaration, long>(0);
        var semantic = new MemoryCache<SourceTransformationInput, LoweredSourceFunction, long>(0);
        directory ??= ImmutableList<string>.Empty;

        return
            new(
                backingStore is null
                ?
                operators
                :
                new LayeredCache<SourceTransformationInput, Declaration>(
                    operators,
                    new FileStoreCache<SourceTransformationInput, Declaration>(
                        backingStore,
                        directory.Add("operator-lowering-v6"),
                        input => input.ComputeHash(),
                        OperatorCodec)),
                backingStore is null
                ?
                semantic
                :
                new LayeredCache<SourceTransformationInput, LoweredSourceFunction>(
                    semantic,
                    new FileStoreCache<SourceTransformationInput, LoweredSourceFunction>(
                        backingStore,
                        directory.Add("semantic-functions-v6"),
                        input => input.ComputeHash(),
                        SemanticCodec)))
            {
                OperatorLifetime = operators,
                SemanticLifetime = semantic
            };
    }

    /// <summary>Lossless codec for lowered abstract declarations.</summary>
    public static ICacheCodec<Declaration> OperatorCodec { get; } = new DeclarationCodec();

    /// <summary>Lossless codec for resolved functions and their binding/evidence identities.</summary>
    public static ICacheCodec<LoweredSourceFunction> SemanticCodec { get; } = new FunctionCodec();

    private static readonly JsonSerializerOptions s_options = CreateOptions();

    private static JsonSerializerOptions CreateOptions()
    {
        var options = Model.CompilerModelJson.CreateOptions();
        options.IncludeFields = true;
        options.Converters.Add(new JsonConverterForChoiceType<TypeInference.InferredType>());
        return options;
    }

    private sealed record EnvironmentEntry(DeclQualifiedName Name, Signature? Signature,
        int ParameterCount, Declaration? Metadata);

    private sealed record FunctionTypeEntry(
        DeclQualifiedName Name,
        TypeInference.InferredType ReturnType,
        Model.ValueList<TypeInference.InferredType> ParameterTypes);

    private sealed record EnvironmentShape(
        Model.ValueList<EnvironmentEntry> Entries,
        Model.ValueList<FunctionTypeEntry>? Types);

    /// <summary>Invocation-local fingerprints of anonymous metadata; source-bound lookup data is excluded.</summary>
    internal sealed class EnvironmentFingerprintCache
    {
        private readonly Dictionary<EnvironmentEntry, byte[]> entries = [];

        private readonly Dictionary<FunctionTypeEntry, byte[]> types = [];

        private readonly Dictionary<EnvironmentShape, string> environments = [];

        public string Compute(
            IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations,
            IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo>? functionTypes)
        {
            var shape =
                new EnvironmentShape(
                    new(
                        declarations.OrderBy(entry => entry.Key).Select(
                            entry =>
                            entry.Value is Declaration.FunctionDeclaration function
                            ?
                            new EnvironmentEntry(
                                entry.Key,
                                function.Function.Signature,
                                function.Function.Declaration.Arguments.Count,
                                null)
                            :
                            new EnvironmentEntry(entry.Key, null, 0, entry.Value))),
                    functionTypes is null
                    ?
                    null
                    :
                    new(
                        functionTypes.OrderBy(entry => entry.Key).Select(
                            entry =>
                            new FunctionTypeEntry(entry.Key, entry.Value.ReturnType, new(entry.Value.ParameterTypes)))));

            if (environments.TryGetValue(shape, out var previous))
                return previous;

            using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);
            hash.AppendData(Encoding.UTF8.GetBytes("pine-source-environment-v6"));
            AppendCount(shape.Entries.Count);

            foreach (var entry in shape.Entries)
            {
                if (!entries.TryGetValue(entry, out var digest))
                {
                    digest = SHA256.HashData(JsonSerializer.SerializeToUtf8Bytes(entry, s_options));
                    entries.Add(entry, digest);
                }

                hash.AppendData(digest);
            }

            AppendCount(shape.Types?.Count ?? -1);

            foreach (var entry in shape.Types ?? [])
            {
                if (!types.TryGetValue(entry, out var digest))
                {
                    digest = SHA256.HashData(JsonSerializer.SerializeToUtf8Bytes(entry, s_options));
                    types.Add(entry, digest);
                }

                hash.AppendData(digest);
            }

            var result = Convert.ToHexStringLower(hash.GetHashAndReset());
            environments.Add(shape, result);
            return result;

            void AppendCount(int count)
            {
                Span<byte> bytes = stackalloc byte[4];
                BinaryPrimitives.WriteInt32LittleEndian(bytes, count);
                hash.AppendData(bytes);
            }
        }
    }

    private static readonly Dictionary<string, WeakReference<SourceTransformationInput>> s_inputs = [];

    private static readonly ConditionalWeakTable<SourceTransformationInput, FunctionTypeInfo> s_inferredTypes = [];

    /// <summary>Weak declaration-local type memoization; neither keys nor values contain caller binding identities.</summary>
    internal static FunctionTypeInfo InferFunctionType(
        SourceTransformationInput input,
        IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations) =>
        s_inferredTypes.GetValue(
            input,
            normalized =>
            {
                var signatures = ImmutableDictionary.CreateBuilder<string, TypeInference.InferredType>();

                foreach (var (name, declaration) in declarations)
                {
                    TypeInference.CollectFunctionSignaturesFromDeclaration(
                        declaration,
                        string.Join(".", name.Namespaces),
                        signatures);
                }

                var implementation = ((Declaration.FunctionDeclaration)normalized.Syntax).Function.Declaration;

                var inferred =
                    TypeInference.InferFunctionDeclarationType(
                        implementation.Expression,
                        implementation.Arguments,
                        string.Join(".", normalized.Name.Namespaces),
                        new TypeInference.FunctionInferenceEnvironment(signatures.ToImmutable()));

                return
                    new(
                        inferred.returnType,
                        [
                            .. implementation.Arguments.Select(
                                argument =>
                                argument is Pattern.VarPattern variable &&
                                inferred.parameterTypes.TryGetValue(variable.Name, out var type)
                                ?
                                type
                                :
                                new TypeInference.InferredType.UnknownType())
                        ]);
            });

    /// <summary>Shares immutable normalized inputs without retaining source bindings or imposing cache lifetimes.</summary>
    internal static SourceTransformationInput Intern(SourceTransformationInput input)
    {
        var hash = Convert.ToHexStringLower(input.ComputeHash().Span);

        lock (s_inputs)
        {
            if (s_inputs.TryGetValue(hash, out var reference) && reference.TryGetTarget(out var previous))
                return previous;

            // Abstract syntax records precompute hashes; rebuilding discards hashes copied by name mapping.
            var normalized = NormalizeSyntax(input.Syntax);

            if (!ReferenceEquals(input.Syntax, normalized))
                input = input with { Syntax = normalized };

            if (s_inputs.Count >= 4096)
            {
                foreach (var dead in s_inputs.Where(entry => !entry.Value.TryGetTarget(out _)).Select(entry => entry.Key).ToArray())
                    s_inputs.Remove(dead);

                if (s_inputs.Count >= 4096)
                    s_inputs.Clear();
            }

            s_inputs[hash] = new(input);
            return input;
        }
    }

    internal static Declaration NormalizeSyntax(Declaration syntax) =>
        SourceTransformationInput.NormalizeSyntax(syntax);

    /// <summary>
    /// Fingerprints projected, anonymous cross-declaration inputs, without dependency function bodies.
    /// Semantic conversion additionally includes body-inferred function types. Declaration names, signatures,
    /// argument counts, aliases and constructor definitions are ordered independently of dictionary traversal.
    /// </summary>
    internal static string EnvironmentHash(
        IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations,
        IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo>? functionTypes = null,
        EnvironmentFingerprintCache? cache = null) =>
        (cache ?? new()).Compute(declarations, functionTypes);

    private sealed class DeclarationCodec : ICacheCodec<Declaration>
    {
        public ReadOnlyMemory<byte> Encode(Declaration value) =>
            JsonSerializer.SerializeToUtf8Bytes(value, ElmSyntaxAbstractJson.JsonSerializerOptions);

        public Declaration Decode(ReadOnlyMemory<byte> bytes) =>
            JsonSerializer.Deserialize<Declaration>(bytes.Span, ElmSyntaxAbstractJson.JsonSerializerOptions) ??
            throw new JsonException("Expected a lowered source declaration.");
    }

    private sealed record BindingEntry(Model.BindingId Id, string Name);

    private sealed record EvidenceEntry(int Index, GenericRecordAccessEvidence Evidence);

    private sealed record FunctionDocument(Model.Function Function, BindingEntry[] Bindings, EvidenceEntry[] Evidence);

    private sealed class FunctionCodec : ICacheCodec<LoweredSourceFunction>
    {
        public ReadOnlyMemory<byte> Encode(LoweredSourceFunction value)
        {
            var evidence = new List<EvidenceEntry>();

            Visit(
                value.Function.Body,
                (expression, index) =>
                {
                    if (value.Evidence.TryGetValue(expression, out var item))
                        evidence.Add(new(index, item));
                });

            return
                JsonSerializer.SerializeToUtf8Bytes(
                    new FunctionDocument(
                        value.Function,
                        [
                            .. value.BindingNames.OrderBy(entry => entry.Key, Model.IdentityComparer.Instance)
                            .Select(entry => new BindingEntry(entry.Key, entry.Value))
                        ],
                        [.. evidence]),
                    s_options);
        }

        public LoweredSourceFunction Decode(ReadOnlyMemory<byte> bytes)
        {
            var document =
                JsonSerializer.Deserialize<FunctionDocument>(bytes.Span, s_options) ??
                throw new JsonException("Expected a lowered source function.");

            var remaining = document.Evidence.ToDictionary(entry => entry.Index, entry => entry.Evidence);

            var evidence =
                ImmutableDictionary.CreateBuilder<Model.Expression, GenericRecordAccessEvidence>(
                    ReferenceEqualityComparer.Instance);

            Visit(
                document.Function.Body,
                (expression, index) =>
                {
                    if (remaining.Remove(index, out var item))
                        evidence.Add(expression, item);
                });

            if (remaining.Count != 0)
                throw new JsonException("Cached evidence refers to a nonexistent expression.");

            return
                new(
                    document.Function,
                    document.Bindings.ToImmutableDictionary(entry => entry.Id, entry => entry.Name),
                    evidence.ToImmutable());
        }

        private static void Visit(Model.Expression root, Action<Model.Expression, int> visitor)
        {
            var index = 0;
            Walk(root);

            void Walk(Model.Expression expression)
            {
                visitor(expression, index++);

                foreach (var child in Model.ExpressionTransform.Children(expression))
                    Walk(child);
            }
        }
    }
}
