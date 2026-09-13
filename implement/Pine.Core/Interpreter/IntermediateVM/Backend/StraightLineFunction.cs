using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>An owned instruction selection; never contains a legacy instruction or linked invocation.</summary>
public abstract record SelectedInstruction
{
    private SelectedInstruction() { }

    /// <summary>Pushes owned literal data.</summary>
    public sealed record Literal(LiteralValue Value) : SelectedInstruction;
    /// <summary>Reads an initialized physical local.</summary>
    public sealed record Load(int Local) : SelectedInstruction;
    /// <summary>Copies, without popping, the stack top into a physical local.</summary>
    public sealed record Store(int Local) : SelectedInstruction;
    /// <summary>Discards the stack top.</summary>
    public sealed record Pop : SelectedInstruction;
    /// <summary>Constructs a list in operand order.</summary>
    public sealed record MakeList(int Count) : SelectedInstruction;
    /// <summary>Projects one nonnegative list index.</summary>
    public sealed record Project(int Index) : SelectedInstruction;
    /// <summary>
    /// An existing Pine kernel with explicit stack effect. A constant supplies an existing opcode's
    /// literal operand; scalar locals permit guarded lowering of generic slicing semantics without
    /// constructing a canonical argument list. These are compiler-owned operands, not VM fields.
    /// </summary>
    public sealed record Builtin(
        StackInstructionKind Kind,
        int OperandCount = 1,
        LiteralValue? Constant = null,
        int? CountLocal = null,
        int? SourceLocal = null) : SelectedInstruction;
    /// <summary>Returns the stack top.</summary>
    public sealed record Return : SelectedInstruction;
}

/// <summary>A definition's unique local. Local zero is reserved for the canonical incoming environment.</summary>
public sealed record StorageBinding(PineVirtualValueId Value, int Local);

/// <summary>Exact graph-derived bounds, including the canonical environment local.</summary>
public sealed record FrameResourceUsage(int LocalsCount, int MaxStackUsage);

/// <summary>
/// Immutable straight-line artifact. Storage is ordered by entry parameters, then operation order;
/// sparse semantic IDs have no physical meaning. Only the compiler can publish an artifact.
/// </summary>
public sealed record StraightLineFunction
{
    /// <summary>Semantic function identity.</summary>
    public FunctionId Id { get; }
    /// <summary>Original semantic ABI; the VM adapter uses a canonical incoming environment.</summary>
    public FunctionSignature Signature { get; }
    /// <summary>One distinct local per definition, with no liveness reuse.</summary>
    public ImmutableList<StorageBinding> Storage { get; }
    /// <summary>Selected instructions in evaluation order.</summary>
    public ImmutableList<SelectedInstruction> Instructions { get; }
    /// <summary>Bounds computed during selection, not recovered from sequential code.</summary>
    public FrameResourceUsage Resources { get; }

    internal StraightLineFunction(
        FunctionId id,
        FunctionSignature signature,
        ImmutableList<StorageBinding> storage,
        ImmutableList<SelectedInstruction> instructions,
        FrameResourceUsage resources)
    {
        Id = id;
        Signature = signature;
        Storage = storage;
        Instructions = instructions;
        Resources = resources;
    }

    /// <inheritdoc/>
    public bool Equals(StraightLineFunction? other) =>
        other is not null && Id == other.Id && Signature == other.Signature &&
        Storage.SequenceEqual(other.Storage) && Instructions.SequenceEqual(other.Instructions) &&
        Resources == other.Resources;

    /// <inheritdoc/>
    public override int GetHashCode() =>
        HashCode.Combine(Id, Signature, ModelEquality.SequenceHash(Storage),
            ModelEquality.SequenceHash(Instructions), Resources);
}

/// <summary>Backend support is deliberately separate from semantic graph validity.</summary>
public enum StraightLineDiagnosticCode
{
    /// <summary>Even unreachable extra blocks are outside this bounded backend.</summary>
    MultipleBlocks,
    /// <summary>The VM ABI currently returns exactly one Pine value.</summary>
    UnsupportedResultArity,
    /// <summary>Only a return terminator is supported.</summary>
    UnsupportedTerminator,
}

/// <summary>A valid graph shape declined by this backend, not an invalid graph.</summary>
public sealed record StraightLineDiagnostic(
    StraightLineDiagnosticCode Code, FunctionId Function, PineBlockId Block);
