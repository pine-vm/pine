using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Semantic;

/// <summary>Identifies a function without embedding its body or a linked object cycle.</summary>
public readonly record struct FunctionId(int Value);

/// <summary>Identifies a call occurrence within a function.</summary>
public readonly record struct CallSiteId(int Value);

/// <summary>Semantic categories, not machine storage representations.</summary>
public enum ValueType
{
    /// <summary>A canonical Pine value.</summary>
    PineValue,
}

/// <summary>An owned path from the canonical environment. An empty path denotes the whole value.</summary>
public sealed record EnvironmentPath(ImmutableList<int> Indices)
{
    /// <inheritdoc/>
    public bool Equals(EnvironmentPath? other) =>
        other is not null && Indices.SequenceEqual(other.Indices);

    /// <inheritdoc/>
    public override int GetHashCode() => ModelEquality.SequenceHash(Indices);
}

/// <summary>A function parameter and its projection from the canonical environment.</summary>
public sealed record FunctionParameter(EnvironmentPath Path, ValueType Type = ValueType.PineValue);

/// <summary>
/// Ordered environment projections and return slots. A result vector does not imply a multi-return VM ABI.
/// </summary>
public sealed record FunctionSignature(
    ImmutableList<FunctionParameter> Parameters,
    ImmutableList<ValueType> Results)
{
    /// <summary>The opaque call convention: one whole environment in, one Pine value out.</summary>
    public static FunctionSignature Canonical { get; } =
        new([new(new([]))], [ValueType.PineValue]);

    /// <inheritdoc/>
    public bool Equals(FunctionSignature? other) =>
        other is not null &&
        Parameters.SequenceEqual(other.Parameters) &&
        Results.SequenceEqual(other.Results);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(ModelEquality.SequenceHash(Parameters), ModelEquality.SequenceHash(Results));
}

/// <summary>A block-local definition; all uses name its ID explicitly.</summary>
public sealed record ValueDefinition(PineVirtualValueId Id, ValueType Type = ValueType.PineValue);

/// <summary>
/// Deeply immutable literal data. Unlike PineValue's ReadOnlyMemory payloads, these collections
/// cannot retain mutable backing arrays. No mutable Expression or PineValue is embedded in a graph.
/// </summary>
public abstract record LiteralValue
{
    private LiteralValue() { }

    /// <summary>Exact bytes, including noncanonical language encodings.</summary>
    public sealed record Blob(ImmutableList<byte> Bytes) : LiteralValue
    {
        /// <inheritdoc/>
        public bool Equals(Blob? other) => other is not null && Bytes.SequenceEqual(other.Bytes);

        /// <inheritdoc/>
        public override int GetHashCode() => ModelEquality.SequenceHash(Bytes);
    }

    /// <summary>An ordered list of owned literals.</summary>
    public sealed record List(ImmutableList<LiteralValue> Items) : LiteralValue
    {
        /// <inheritdoc/>
        public bool Equals(List? other) => other is not null && Items.SequenceEqual(other.Items);

        /// <inheritdoc/>
        public override int GetHashCode() => ModelEquality.SequenceHash(Items);
    }
}

/// <summary>
/// Ordered, explicit semantic computations. Calls terminate blocks and are never hidden operations.
/// </summary>
public abstract record Operation
{
    private Operation() { }

    /// <summary>Defines a literal value.</summary>
    public sealed record Literal(ValueDefinition Result, LiteralValue Value) : Operation;

    /// <summary>Constructs a list from already evaluated items, in order.</summary>
    public sealed record MakeList(
        ValueDefinition Result,
        ImmutableList<PineVirtualValueId> Items) : Operation
    {
        /// <inheritdoc/>
        public bool Equals(MakeList? other) =>
            other is not null && Result == other.Result && Items.SequenceEqual(other.Items);

        /// <inheritdoc/>
        public override int GetHashCode() => HashCode.Combine(Result, ModelEquality.SequenceHash(Items));
    }

    /// <summary>Projects a path from an explicit source, using Pine list indexing semantics.</summary>
    public sealed record Project(
        ValueDefinition Result,
        PineVirtualValueId Source,
        EnvironmentPath Path) : Operation;

    /// <summary>Applies a named Pine builtin to its one canonical argument value.</summary>
    public sealed record Builtin(
        ValueDefinition Result,
        string Name,
        PineVirtualValueId Argument) : Operation;
}

/// <summary>
/// A distinct control-flow edge. Arguments bind target parameters simultaneously and by position.
/// </summary>
public sealed record Edge(PineBlockId Target, ImmutableList<PineVirtualValueId> Arguments)
{
    /// <inheritdoc/>
    public bool Equals(Edge? other) =>
        other is not null && Target == other.Target && Arguments.SequenceEqual(other.Arguments);

    /// <inheritdoc/>
    public override int GetHashCode() => HashCode.Combine(Target, ModelEquality.SequenceHash(Arguments));
}

/// <summary>Call targets are operands or identities, never mutable expression containers.</summary>
public abstract record CallTarget
{
    private CallTarget() { }

    /// <summary>An already evaluated encoded expression. Parsing may still fail at invocation.</summary>
    public sealed record Dynamic(PineVirtualValueId EncodedExpression) : CallTarget;

    /// <summary>A function identity resolved through a separate function table.</summary>
    public sealed record Known(FunctionId Function) : CallTarget;
}

/// <summary>
/// Explicit call operands and declared contract. Dynamic calls use the canonical signature;
/// a known target's projected signature must match its function table entry.
/// These obligations are checked by the subsequent graph-validation phase, not by this data model.
/// </summary>
public sealed record Call(
    CallSiteId Site,
    CallTarget Target,
    FunctionSignature Signature,
    ImmutableList<PineVirtualValueId> Arguments)
{
    /// <inheritdoc/>
    public bool Equals(Call? other) =>
        other is not null &&
        Site == other.Site && Target == other.Target && Signature == other.Signature &&
        Arguments.SequenceEqual(other.Arguments);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(Site, Target, Signature, ModelEquality.SequenceHash(Arguments));
}

/// <summary>A continuation argument is either preserved caller data or a successful return slot.</summary>
public abstract record ContinuationBinding
{
    private ContinuationBinding() { }

    /// <summary>A value defined in the invoking block.</summary>
    public sealed record CallerValue(PineVirtualValueId Value) : ContinuationBinding;

    /// <summary>A result slot that exists only after successful return.</summary>
    public sealed record ReturnedResult(int Index) : ContinuationBinding;
}

/// <summary>Positional bindings for the block entered after a successful non-tail call.</summary>
public sealed record InvokeContinuation(
    PineBlockId Target,
    ImmutableList<ContinuationBinding> Bindings)
{
    /// <inheritdoc/>
    public bool Equals(InvokeContinuation? other) =>
        other is not null && Target == other.Target && Bindings.SequenceEqual(other.Bindings);

    /// <inheritdoc/>
    public override int GetHashCode() => HashCode.Combine(Target, ModelEquality.SequenceHash(Bindings));
}

/// <summary>A switch case owns its edge, even when several cases have the same target.</summary>
public sealed record SwitchCase(LiteralValue Value, Edge Edge);

/// <summary>All control transfers and their operands, without layout or fall-through conventions.</summary>
public abstract record Terminator
{
    private Terminator() { }

    /// <summary>Returns the ordered result vector.</summary>
    public sealed record Return(ImmutableList<PineVirtualValueId> Values) : Terminator
    {
        /// <inheritdoc/>
        public bool Equals(Return? other) => other is not null && Values.SequenceEqual(other.Values);

        /// <inheritdoc/>
        public override int GetHashCode() => ModelEquality.SequenceHash(Values);
    }

    /// <summary>Transfers to one successor.</summary>
    public sealed record Jump(Edge Edge) : Terminator;

    /// <summary>Tests exact Pine equality, not host-language truthiness.</summary>
    public sealed record Branch(
        PineVirtualValueId TestedValue,
        LiteralValue Literal,
        Edge IfEqual,
        Edge IfNotEqual) : Terminator;

    /// <summary>
    /// Ordered exact-equality cases and an explicit default. Duplicate literal cases are a validation error.
    /// </summary>
    public sealed record Switch(
        PineVirtualValueId Selector,
        ImmutableList<SwitchCase> Cases,
        Edge Default) : Terminator
    {
        /// <inheritdoc/>
        public bool Equals(Switch? other) =>
            other is not null && Selector == other.Selector &&
            Cases.SequenceEqual(other.Cases) && Default == other.Default;

        /// <inheritdoc/>
        public override int GetHashCode() =>
            HashCode.Combine(Selector, ModelEquality.SequenceHash(Cases), Default);
    }

    /// <summary>Invokes a callee and then binds the continuation's parameters.</summary>
    public sealed record Invoke(Call Call, InvokeContinuation Continuation) : Terminator;

    /// <summary>Invokes a callee whose result vector becomes this function's return vector.</summary>
    public sealed record TailInvoke(Call Call) : Terminator;
}

/// <summary>A complete block shape, not a claim that definitions or transfers have been validated.</summary>
public sealed record BasicBlock(
    PineBlockId Id,
    ImmutableList<ValueDefinition> Parameters,
    ImmutableList<Operation> Operations,
    Terminator Terminator)
{
    /// <inheritdoc/>
    public bool Equals(BasicBlock? other) =>
        other is not null && Id == other.Id && Parameters.SequenceEqual(other.Parameters) &&
        Operations.SequenceEqual(other.Operations) && Terminator == other.Terminator;

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(
            Id, ModelEquality.SequenceHash(Parameters), ModelEquality.SequenceHash(Operations), Terminator);
}

/// <summary>
/// Semantic graph data, independent of any physical instruction representation.
/// Block identity is independent of dictionary enumeration and layout. Validation is a separate phase.
/// </summary>
public sealed record FunctionGraph
{
    /// <summary>The function's identity.</summary>
    public FunctionId Id { get; }

    /// <summary>The environment projections and result vector.</summary>
    public FunctionSignature Signature { get; }

    /// <summary>The entry block's identity.</summary>
    public PineBlockId Entry { get; }

    /// <summary>Blocks indexed by identity, with the default value comparer.</summary>
    public ImmutableDictionary<PineBlockId, BasicBlock> Blocks { get; }

    /// <summary>Constructs unvalidated graph data from immutable inputs.</summary>
    public FunctionGraph(
        FunctionId id,
        FunctionSignature signature,
        PineBlockId entry,
        ImmutableDictionary<PineBlockId, BasicBlock> blocks)
    {
        Id = id;
        Signature = signature;
        Entry = entry;
        Blocks = blocks.WithComparers(
            System.Collections.Generic.EqualityComparer<PineBlockId>.Default,
            System.Collections.Generic.EqualityComparer<BasicBlock>.Default);
    }

    /// <inheritdoc/>
    public bool Equals(FunctionGraph? other) =>
        other is not null && Id == other.Id && Signature == other.Signature &&
        Entry == other.Entry && ModelEquality.BlocksEqual(Blocks, other.Blocks);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(Id, Signature, Entry, ModelEquality.BlocksHash(Blocks));
}

internal static class ModelEquality
{
    public static int SequenceHash<T>(ImmutableList<T> items) =>
        items.Aggregate(0, (hash, item) => HashCode.Combine(hash, item));

    public static bool BlocksEqual(
        ImmutableDictionary<PineBlockId, BasicBlock> left,
        ImmutableDictionary<PineBlockId, BasicBlock> right) =>
        left.Count == right.Count &&
        left.All(pair => right.TryGetValue(pair.Key, out var block) && pair.Value == block);

    public static int BlocksHash(ImmutableDictionary<PineBlockId, BasicBlock> blocks) =>
        blocks.OrderBy(pair => pair.Key.Value)
        .Aggregate(0, (hash, pair) => HashCode.Combine(hash, pair.Key, pair.Value));
}
