using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Text.Json;
using System.Text.Json.Serialization;

using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Elm.Syntax.Interpreter;

/// <summary>
/// Input to pure, unreduced declaration preparation. The owner separates expression identities used
/// by per-run observers, even when declarations in different modules have identical syntax.
/// Both in-memory and persistent identities exclude source ranges.
/// </summary>
public sealed record PreparedDeclarationInput(DeclQualifiedName Name, Abstract.Declaration Syntax)
{
    private sealed record Digest(ReadOnlyMemory<byte> Bytes);

    private static readonly ConditionalWeakTable<Abstract.Declaration, Digest> s_syntaxHashes = [];

    private static readonly ConditionalWeakTable<Concrete.Declaration, Abstract.Declaration> s_normalizedSyntax = [];

    /// <summary>
    /// Normalizes canonical CST once per immutable object, with weak ownership. Repeated canonical-cache
    /// hits avoid reconversion; range-only canonical misses can still hit range-free prepared storage.
    /// </summary>
    public static PreparedDeclarationInput FromConcrete(DeclQualifiedName name, Concrete.Declaration syntax) =>
        new(name, Normalize(syntax));

    internal static Abstract.Declaration Normalize(Concrete.Declaration syntax) =>
        s_normalizedSyntax.GetValue(syntax, Abstract.ConvertFromConcrete.FromDeclaration);

    /// <summary>
    /// Domain-separated SHA-256 over owner and normalized canonical syntax. Preparation has no
    /// environment/configuration inputs: operator lowering and closed-expression reduction are disabled.
    /// Bump the domain version when preparation semantics or the serialized model change.
    /// </summary>
    public ReadOnlyMemory<byte> ComputeHash()
    {
        var syntaxHash =
            s_syntaxHashes.GetValue(
                Syntax,
                syntax => new(
                    SHA256.HashData(
                        JsonSerializer.SerializeToUtf8Bytes(
                            syntax,
                            Abstract.ElmSyntaxAbstractJson.JsonSerializerOptions))));

        return
            SHA256.HashData(
                JsonSerializer.SerializeToUtf8Bytes(
                    new[]
                    {
                        "pine-unreduced-prepared-declaration-v1", Name.FullName,
                        Convert.ToHexStringLower(syntaxHash.Bytes.Span)
                    }));
    }
}

/// <summary>
/// Detached, immutable source preparation, shared by interpretation and later closure compilation.
/// Contains no resolver, runtime closure, observer, cancellation token or captured test value.
/// Expression paths are derived lazily from the exact retained prepared-expression identities.
/// </summary>
public sealed class PreparedSourceDeclaration(
    Abstract.Declaration syntax, Engine.PreparedDeclaration prepared)
{
    /// <summary>Range-free canonical syntax, also consumed by closure compilation.</summary>
    public Abstract.Declaration Syntax { get; } = syntax;

    /// <summary>Pure interpreter preparation preserving the source expression shape.</summary>
    public Engine.PreparedDeclaration Prepared { get; } = prepared;

    private readonly Lazy<IReadOnlyList<(Engine.PreparedExpression Expression, ElmSyntaxExpressionPath Path)>> _paths =
        new(
            () => prepared is Engine.PreparedDeclaration.FunctionDeclaration function
            ?
            Engine.EnumerateExpressionPaths(function.Function.Declaration.Expression).ToArray()
            :
            []);

    /// <summary>Range-free paths; each run projects these onto its own current CST.</summary>
    [JsonIgnore]
    public IReadOnlyList<(Engine.PreparedExpression Expression, ElmSyntaxExpressionPath Path)> ExpressionPaths =>
        _paths.Value;

    /// <summary>Converts and prepares once, without lowering or contextual closed-expression reduction.</summary>
    public static PreparedSourceDeclaration Prepare(Concrete.Declaration declaration) =>
        Prepare(PreparedDeclarationInput.Normalize(declaration));

    /// <summary>Prepares normalized canonical syntax, without lowering or reduction.</summary>
    public static PreparedSourceDeclaration Prepare(Abstract.Declaration syntax) =>
        new(syntax, Engine.PrepareDeclaration(syntax));

    /// <summary>Lossless codec reusing the interpreter's prepared/abstract syntax serialization.</summary>
    public static ICacheCodec<PreparedSourceDeclaration> Codec { get; } = new DeclarationCodec();

    private sealed class DeclarationCodec : ICacheCodec<PreparedSourceDeclaration>
    {
        public ReadOnlyMemory<byte> Encode(PreparedSourceDeclaration value) =>
            JsonSerializer.SerializeToUtf8Bytes(value, ElmSyntaxInterpreterPreparedJson.JsonSerializerOptions);

        public PreparedSourceDeclaration Decode(ReadOnlyMemory<byte> bytes) =>
            JsonSerializer.Deserialize<PreparedSourceDeclaration>(
                bytes.Span,
                ElmSyntaxInterpreterPreparedJson.JsonSerializerOptions) ??
            throw new JsonException("Expected a prepared source declaration.");
    }
}
