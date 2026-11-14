using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Text.Json.Serialization;

using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Elm.Syntax.Interpreter;

/// <summary>
/// Input to pure, unreduced declaration preparation. Scope and observer identities are rebound
/// outside the cache; only anonymous canonical syntax is retained here.
/// </summary>
public sealed record PreparedDeclarationInput(Abstract.Declaration Syntax)
{
    /// <summary>The lexical declaration name, without a source owner.</summary>
    [JsonIgnore]
    public DeclQualifiedName Name =>
        DeclQualifiedName.Create(
            [],
            ElmCompiler.GetDeclarationName(Syntax) ??
            throw new InvalidOperationException("Expected a named declaration."));

    private sealed record Digest(ReadOnlyMemory<byte> Bytes);

    private static readonly ConditionalWeakTable<Abstract.Declaration, Digest> s_hashes = [];

    /// <summary>
    /// Noncached compatibility projection from the caller's CST to anonymous preparation input.
    /// The source owner is not retained; preparation storage is keyed only by the returned syntax.
    /// </summary>
    public static PreparedDeclarationInput FromConcrete(DeclQualifiedName name, Concrete.Declaration syntax)
    {
        var slots = new Dictionary<string, int>(StringComparer.Ordinal);

        return
            new(
                Canonicalize.MapNames(
                    Normalize(syntax),
                    reference =>
                    {
                        var module = string.Join(".", reference.Namespaces);

                        if (module.Length is 0 || module is "Basics" or "Debug" or "Pine_kernel" or "Pine_builtin" ||
                            module is "List" && reference.DeclName is "List")
                        {
                            return reference;
                        }

                        if (!slots.TryGetValue(module, out var slot))
                        {
                            slot = slots.Count;
                            slots.Add(module, slot);
                        }

                        return Canonicalize.SlotName(slot, reference.DeclName);
                    }));
    }

    internal static Abstract.Declaration Normalize(Concrete.Declaration syntax) =>
        Abstract.ConvertFromConcrete.FromDeclaration(syntax);

    /// <summary>
    /// Domain-separated SHA-256 over anonymous canonical syntax. Preparation has no
    /// environment/configuration inputs: operator lowering and closed-expression reduction are disabled.
    /// Bump the domain version when preparation semantics or the serialized model change.
    /// </summary>
    public ReadOnlyMemory<byte> ComputeHash() =>
        s_hashes.GetValue(
            Syntax,
            static syntax =>
            {
                ReadOnlySpan<byte> domain = "pine-unreduced-prepared-declaration-v3-binary\0"u8;
                Span<byte> input = stackalloc byte[domain.Length + 32];
                domain.CopyTo(input);
                Abstract.ElmSyntaxAbstractBinary.ComputeHash(syntax).Span.CopyTo(input[domain.Length..]);
                return new(SHA256.HashData(input));
            }).Bytes;
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

    /// <summary>Attaches current binding identities and fresh observer sites without repeating preparation.</summary>
    public PreparedSourceDeclaration Rebind(Func<DeclQualifiedName, DeclQualifiedName> bindings) =>
        PreparedSourceRebinding.Apply(this, bindings);

    /// <summary>Lossless binary storage of the original syntax and retained preparation.</summary>
    public static ICacheCodec<PreparedSourceDeclaration> Codec { get; } =
        ElmSyntaxInterpreterPreparedBinary.SourceDeclarationCodec;
}
