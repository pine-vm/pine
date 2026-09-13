using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>A simultaneous physical-local assignment, not a sequential move.</summary>
public sealed record LocalCopy(int Source, int Destination);

/// <summary>
/// One edge's parallel assignment. Selection loads all sources before storing in reverse order.
/// This costs three instructions per binding and N stack slots, but no scratch locals, and handles
/// cycles, duplicate sources and self copies without liveness assumptions.
/// </summary>
public sealed record EdgeCopyPlan(PineBlockId Target, ImmutableList<LocalCopy> Copies)
{
    /// <inheritdoc/>
    public bool Equals(EdgeCopyPlan? other) =>
        other is not null && Target == other.Target && Copies.SequenceEqual(other.Copies);
    /// <inheritdoc/>
    public override int GetHashCode() => HashCode.Combine(Target, ModelEquality.SequenceHash(Copies));
}

/// <summary>An ordered exact literal test and its independently owned edge.</summary>
public sealed record SelectedCase(LiteralValue Literal, EdgeCopyPlan Edge);

/// <summary>Physical-local operands with explicit successors, before layout.</summary>
public abstract record SelectedTerminator
{
    private SelectedTerminator() { }
    /// <summary>The canonical single result.</summary>
    public sealed record Return(int Local) : SelectedTerminator;
    /// <summary>One simultaneous assignment and successor.</summary>
    public sealed record Jump(EdgeCopyPlan Edge) : SelectedTerminator;
    /// <summary>Ordered exact tests; a branch is a one-case match.</summary>
    public sealed record Match(
        int Local, ImmutableList<SelectedCase> Cases, EdgeCopyPlan Default) : SelectedTerminator
    {
        /// <inheritdoc/>
        public bool Equals(Match? other) =>
            other is not null && Local == other.Local && Cases.SequenceEqual(other.Cases) && Default == other.Default;
        /// <inheritdoc/>
        public override int GetHashCode() => HashCode.Combine(Local, ModelEquality.SequenceHash(Cases), Default);
    }
}

/// <summary>A selected semantic block, with empty stack at entry and after its operations.</summary>
public sealed record SelectedBlock(
    PineBlockId Id, ImmutableList<SelectedInstruction> Instructions, SelectedTerminator Terminator)
{
    /// <inheritdoc/>
    public bool Equals(SelectedBlock? other) =>
        other is not null && Id == other.Id && Instructions.SequenceEqual(other.Instructions) && Terminator == other.Terminator;
    /// <inheritdoc/>
    public override int GetHashCode() => HashCode.Combine(Id, ModelEquality.SequenceHash(Instructions), Terminator);
}

/// <summary>Separate namespaces prevent collisions with arbitrary semantic block IDs.</summary>
public enum LayoutLabelKind
{
    /// <summary>Canonical environment initialization, never an edge target.</summary>
    Prologue,
    /// <summary>A semantic block.</summary>
    Block,
    /// <summary>An edge-specific parallel-copy stub.</summary>
    Edge,
    /// <summary>An ordered switch test.</summary>
    Test,
}

/// <summary>A symbolic location; no numeric instruction address is assigned during selection.</summary>
public readonly record struct LayoutLabel(LayoutLabelKind Kind, PineBlockId Block, int Index = 0);

/// <summary>Explicit transfers between known fragments. Every outgoing stack is empty.</summary>
public abstract record LayoutTransfer
{
    private LayoutTransfer() { }
    /// <summary>Load and return one local.</summary>
    public sealed record Return(int Local) : LayoutTransfer;
    /// <summary>Unconditional transfer.</summary>
    public sealed record Jump(LayoutLabel Target) : LayoutTransfer;
    /// <summary>Load and consume a tested local, with two explicit targets.</summary>
    public sealed record Branch(int Local, LiteralValue Literal, LayoutLabel Equal, LayoutLabel NotEqual) : LayoutTransfer;
}

/// <summary>A known empty-stack fragment and its explicit transfer.</summary>
public sealed record LayoutFragment(
    LayoutLabel Label, ImmutableList<SelectedInstruction> Instructions, LayoutTransfer Transfer)
{
    /// <inheritdoc/>
    public bool Equals(LayoutFragment? other) =>
        other is not null && Label == other.Label && Instructions.SequenceEqual(other.Instructions) && Transfer == other.Transfer;
    /// <inheritdoc/>
    public override int GetHashCode() => HashCode.Combine(Label, ModelEquality.SequenceHash(Instructions), Transfer);
}

/// <summary>Immutable selection, copy plans, layout and precomputed resources; no legacy payloads.</summary>
public sealed record GraphFunction
{
    /// <summary>Function identity.</summary>
    public FunctionId Id { get; }
    /// <summary>Semantic signature; adaptation still uses the canonical environment and one result.</summary>
    public FunctionSignature Signature { get; }
    /// <summary>Unique locals in numeric block-ID order, then definition order. Zero is the environment.</summary>
    public ImmutableList<StorageBinding> Storage { get; }
    /// <summary>All selected blocks, including unreachable ones, in numeric ID order.</summary>
    public ImmutableList<SelectedBlock> Blocks { get; }
    /// <summary>Scheduled fragments, starting with the separate initialization prologue.</summary>
    public ImmutableList<LayoutFragment> Layout { get; }
    /// <summary>Exact maximum over all known fragments, including unreachable ones and edge copies.</summary>
    public FrameResourceUsage Resources { get; }

    internal GraphFunction(
        FunctionId id, FunctionSignature signature, ImmutableList<StorageBinding> storage,
        ImmutableList<SelectedBlock> blocks, ImmutableList<LayoutFragment> layout, FrameResourceUsage resources)
    {
        Id = id;
        Signature = signature;
        Storage = storage;
        Blocks = blocks;
        Layout = layout;
        Resources = resources;
    }

    /// <inheritdoc/>
    public bool Equals(GraphFunction? other) =>
        other is not null && Id == other.Id && Signature == other.Signature &&
        Storage.SequenceEqual(other.Storage) && Blocks.SequenceEqual(other.Blocks) &&
        Layout.SequenceEqual(other.Layout) && Resources == other.Resources;
    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(Id, Signature, ModelEquality.SequenceHash(Storage), ModelEquality.SequenceHash(Blocks),
            ModelEquality.SequenceHash(Layout), Resources);
}

/// <summary>Support/layout failures, separate from semantic validation failures.</summary>
public enum GraphBackendDiagnosticCode
{
    /// <summary>The VM boundary returns exactly one value.</summary>
    UnsupportedResultArity,
    /// <summary>Calls and their ABI/linking are deferred to increment 5b.</summary>
    UnsupportedCall,
    /// <summary>Layout must list every block exactly once.</summary>
    InvalidBlockOrder,
}

/// <summary>A deterministic backend refusal, without falling back to the old compiler.</summary>
public sealed record GraphBackendDiagnostic(GraphBackendDiagnosticCode Code, FunctionId Function, PineBlockId Block);
