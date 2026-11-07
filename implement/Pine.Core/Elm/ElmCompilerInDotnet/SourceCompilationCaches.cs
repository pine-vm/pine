using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.IO;
using Pine.Core.Json;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Text.Json;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Exact input to a source transformation. EnvironmentHash covers the type/name environment;
/// Configuration includes the transformation's algorithm version and every lowering option.
/// It contains neither interpreter closures nor captured runtime values.
/// </summary>
public sealed record SourceTransformationInput(
    DeclQualifiedName Name, Declaration Syntax, string EnvironmentHash, string Configuration)
{
    private sealed record Digest(ReadOnlyMemory<byte> Bytes);

    private static readonly ConditionalWeakTable<Declaration, Digest> s_syntaxHashes = [];

    /// <summary>
    /// Derives a domain-separated SHA-256 address. Syntax encoding is memoized by immutable object
    /// identity with weak ownership; shared environment data is hashed once per transformation pass.
    /// </summary>
    public ReadOnlyMemory<byte> ComputeHash()
    {
        var syntaxHash =
            s_syntaxHashes.GetValue(
                Syntax,
                syntax => new(
                    SHA256.HashData(
                        JsonSerializer.SerializeToUtf8Bytes(syntax, ElmSyntaxAbstractJson.JsonSerializerOptions))));

        return
            SHA256.HashData(
                JsonSerializer.SerializeToUtf8Bytes(
                    new[]
                    {
                        "pine-source-transformation-v1", Configuration, Name.FullName, EnvironmentHash,
                        Convert.ToHexStringLower(syntaxHash.Bytes.Span)
                    }));
    }
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
                        directory.Add("operator-lowering-v1"),
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
                        directory.Add("semantic-functions-v1"),
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

    private sealed record FunctionTypeEntry(DeclQualifiedName Name, FunctionTypeInfo Type);

    /// <summary>
    /// Fingerprints all cross-declaration inputs actually used by the transformations, without function bodies.
    /// Semantic conversion additionally includes body-inferred function types. Declaration names, signatures,
    /// argument counts, aliases and constructor definitions are ordered independently of dictionary traversal.
    /// </summary>
    internal static string EnvironmentHash(
        IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations,
        IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo>? functionTypes = null)
    {
        var entries =
            declarations.OrderBy(entry => entry.Key).Select(
                entry => entry.Value is Declaration.FunctionDeclaration function
                ?
                new EnvironmentEntry(
                    entry.Key,
                    function.Function.Signature,
                    function.Function.Declaration.Arguments.Count,
                    null)
                :
                new EnvironmentEntry(entry.Key, null, 0, entry.Value)).ToArray();

        var types =
            functionTypes?.OrderBy(entry => entry.Key)
            .Select(entry => new FunctionTypeEntry(entry.Key, entry.Value)).ToArray();

        return
            Convert.ToHexStringLower(
                SHA256.HashData(
                    JsonSerializer.SerializeToUtf8Bytes(new { Entries = entries, Types = types }, s_options)));
    }

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
