using Pine.Core.CodeAnalysis;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Frontend;

/// <summary>Immutable preparation policy, including the legacy inlining cost gates.</summary>
public sealed record PreparationOptions(
    bool DisableReduction = false,
    bool DisableGenericApplicationChainConsolidation = false,
    int PathMaxLowExclusive = ExpressionCompilation.DefaultPathMaxLowExclusive,
    int PathMaxHighInclusive = ExpressionCompilation.DefaultPathMaxHighInclusive);

/// <summary>A value established at an owned environment path.</summary>
public sealed record SpecializationFact(EnvironmentPath Path, LiteralValue Value);

/// <summary>A data replacement for the legacy skip-inlining predicate.</summary>
public sealed record InlineExclusion(OwnedExpression Expression, SpecializationFacts? Specialization);

/// <summary>Owned specialization assumptions in deterministic path order.</summary>
public sealed record SpecializationFacts(ImmutableList<SpecializationFact> Items)
{
    /// <summary>Snapshots the legacy environment class.</summary>
    public static SpecializationFacts Capture(PineValueClass constraint) =>
        new([.. constraint.ParsedItems.OrderBy(item => item.Key, IntPathComparer.Instance)
            .Select(item => new SpecializationFact(
                new([.. item.Key]), OwnedExpression.CaptureValue(item.Value)))]);

    /// <summary>Creates detached inputs for legacy stateless analyses.</summary>
    public PineValueClass ToValueClass() =>
        PineValueClass.Create(Items.Select(item =>
            new System.Collections.Generic.KeyValuePair<System.Collections.Generic.IReadOnlyList<int>, PineValue>(
                item.Path.Indices, OwnedExpression.ToValue(item.Value))).ToArray());

    /// <inheritdoc/>
    public bool Equals(SpecializationFacts? other) => other is not null && Items.SequenceEqual(other.Items);

    /// <inheritdoc/>
    public override int GetHashCode() => ModelEquality.SequenceHash(Items);
}

/// <summary>All inputs affecting preparation are explicit data and participate in memo keys.</summary>
public sealed record CompilationRequest(
    FunctionId Id,
    OwnedExpression Root,
    PreparationOptions Options,
    SpecializationFacts? Specialization,
    ImmutableList<InlineExclusion> InlineExclusions)
{
    /// <summary>Captures a generic request without retaining the caller's AST.</summary>
    public static CompilationRequest Capture(
        Expression root, PreparationOptions? options = null, FunctionId id = default) =>
        new(id, OwnedExpression.Capture(root), options ?? new(), null, []);

    /// <inheritdoc/>
    public bool Equals(CompilationRequest? other) =>
        other is not null && Id == other.Id && Root == other.Root && Options == other.Options &&
        Specialization == other.Specialization && InlineExclusions.SequenceEqual(other.InlineExclusions);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(Id, Root, Options, Specialization, ModelEquality.SequenceHash(InlineExclusions));
}

/// <summary>Prepared language body, inferred projections and retained source/label provenance.</summary>
public sealed record PreparedFunction(
    CompilationRequest Request,
    OwnedExpression Body,
    FunctionSignature InferredSignature)
{
    /// <summary>The canonical source is retained even when preparation erased labels or inlined bodies.</summary>
    public OwnedExpression Source => Request.Root;

    /// <summary>The root identity used by the legacy transitive compiler, not an inline occurrence.</summary>
    public ImmutableList<OwnedExpression> AlternativeForms => [Request.Root];
}

/// <summary>A successful or unsuccessful parse, with no mutable language object retained.</summary>
public sealed record ParseMemoEntry(OwnedExpression? Expression, string? Error);

/// <summary>Configuration-specific reduction key.</summary>
public sealed record ReductionMemoKey(OwnedExpression Expression, ReductionConfig Config);

/// <summary>Explicit immutable preparation, parsing, reduction and graph memoization.</summary>
public sealed record CompilerMemo
{
    /// <summary>Successful and unsuccessful encoded expression parses.</summary>
    public ImmutableDictionary<LiteralValue, ParseMemoEntry> Parses { get; init; } =
        [];

    /// <summary>Reductions keyed by the complete reduction configuration.</summary>
    public ImmutableDictionary<ReductionMemoKey, OwnedExpression> Reductions { get; init; } =
        [];

    /// <summary>Preparations keyed by complete requests.</summary>
    public ImmutableDictionary<CompilationRequest, PreparedFunction> Preparations { get; init; } =
        [];

    /// <summary>Directly constructed semantic graphs.</summary>
    public ImmutableDictionary<PreparedFunction, FunctionGraph> Graphs { get; init; } =
        [];

    /// <summary>An empty persistent memo.</summary>
    public static CompilerMemo Empty { get; } = new();

    /// <inheritdoc/>
    public bool Equals(CompilerMemo? other) =>
        other is not null &&
        Equal(Parses, other.Parses) && Equal(Reductions, other.Reductions) &&
        Equal(Preparations, other.Preparations) && Equal(Graphs, other.Graphs);

    private static bool Equal<TKey, TValue>(
        ImmutableDictionary<TKey, TValue> left, ImmutableDictionary<TKey, TValue> right) where TKey : notnull =>
        left.Count == right.Count && left.All(item =>
            right.TryGetValue(item.Key, out var value) &&
            System.Collections.Generic.EqualityComparer<TValue>.Default.Equals(item.Value, value));

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(Hash(Parses), Hash(Reductions), Hash(Preparations), Hash(Graphs));

    private static int Hash<TKey, TValue>(ImmutableDictionary<TKey, TValue> dictionary) where TKey : notnull =>
        dictionary.Aggregate(0, (hash, item) => hash ^ HashCode.Combine(item.Key, item.Value));
}
