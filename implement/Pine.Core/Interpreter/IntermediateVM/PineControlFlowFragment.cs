using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Numerics;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Distinguishes the equality tests supported by <see cref="PineControlFlowNode.Switch"/> and
/// <see cref="PineControlFlowTerminator.Switch"/>.
/// </summary>
public enum PineSwitchKind
{
    /// <summary>
    /// Compares the top stack value against each case literal,
    /// see <see cref="StackInstructionKind.Switch_Jump_If_Equal_Const"/>.
    /// </summary>
    Equal,

    /// <summary>
    /// Compares a slice of a source value (source and skip count on the stack) against each case literal,
    /// see <see cref="StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const"/>.
    /// </summary>
    SliceSkipVarEqual,
}

/// <summary>
/// One case of a switch: the literal to compare with and the index of the branch to continue in.
/// </summary>
public readonly record struct PineSwitchFragmentCase(
    PineValue Literal,
    int BranchIndex);

/// <summary>
/// One node of structured control flow emitted by recursive expression compilation.
/// <para>
/// Nodes do not contain instruction offsets. Control transfers are modeled structurally,
/// and <see cref="PineControlFlowGraph.FromFragment"/> builds basic blocks from them before any
/// sequential instruction list is created.
/// </para>
/// </summary>
public abstract record PineControlFlowNode
{
    private PineControlFlowNode()
    {
    }

    /// <summary>
    /// An instruction that does not transfer control within the frame.
    /// Invocations are allowed here; building the graph turns them into
    /// <see cref="PineControlFlowTerminator.Invoke"/> terminators.
    /// </summary>
    public sealed record Operation : PineControlFlowNode
    {
        /// <summary>
        /// Creates an operation node, rejecting instructions that transfer control within the frame.
        /// </summary>
        public Operation(StackInstruction instruction)
        {
            if (IsControlTransfer(instruction.Kind))
            {
                throw new ArgumentException(
                    "Control transfers must be modeled structurally, not as operation: " + instruction.Kind,
                    nameof(instruction));
            }

            Instruction = instruction;
        }

        /// <summary>
        /// The instruction to execute.
        /// </summary>
        public StackInstruction Instruction { get; }
    }

    /// <summary>
    /// Pops a value and continues in <see cref="Branch"/> if it equals <see cref="Literal"/>,
    /// otherwise in <see cref="FallThrough"/>.
    /// Both branches continue after this node unless they end in a transfer.
    /// </summary>
    public sealed record Conditional(
        PineValue Literal,
        PineControlFlowFragment FallThrough,
        PineControlFlowFragment Branch)
        : PineControlFlowNode;

    /// <summary>
    /// Pops the scrutinee (one value for <see cref="PineSwitchKind.Equal"/>, source and skip count for
    /// <see cref="PineSwitchKind.SliceSkipVarEqual"/>) and continues in the branch of the first matching case,
    /// otherwise in <see cref="Default"/>.
    /// All branches continue after this node unless they end in a transfer.
    /// </summary>
    public sealed record Switch(
        PineSwitchKind Kind,
        ImmutableArray<PineSwitchFragmentCase> Cases,
        PineControlFlowFragment Default,
        ImmutableArray<PineControlFlowFragment> Branches,
        BigInteger SkipCountMultiplier)
        : PineControlFlowNode;

    /// <summary>
    /// Transfers control to the start of the frame (loop back-edge). The evaluation stack must be empty.
    /// No node may follow in the same fragment.
    /// </summary>
    public sealed record JumpToEntry : PineControlFlowNode;

    /// <summary>
    /// Whether the instruction kind transfers control within a frame and therefore needs a structural model.
    /// </summary>
    public static bool IsControlTransfer(StackInstructionKind kind) =>
        kind is
        StackInstructionKind.Jump_Const or
        StackInstructionKind.Jump_If_Equal_Const or
        StackInstructionKind.Switch_Jump_If_Equal_Const or
        StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const or
        StackInstructionKind.Return;
}

/// <summary>
/// Sequence of structured control-flow nodes, as produced by recursive expression compilation.
/// </summary>
public sealed record PineControlFlowFragment(
    ImmutableList<PineControlFlowNode> Nodes)
{
    /// <summary>
    /// A fragment without any nodes.
    /// </summary>
    public static readonly PineControlFlowFragment Empty = new([]);

    /// <summary>
    /// Creates a fragment containing the given straight-line operations.
    /// </summary>
    public static PineControlFlowFragment FromOperations(IEnumerable<StackInstruction> instructions) =>
        Empty.AppendOperations(instructions);

    /// <summary>
    /// Appends a single straight-line operation.
    /// </summary>
    public PineControlFlowFragment AppendOperation(StackInstruction instruction) =>
        Append(new PineControlFlowNode.Operation(instruction));

    /// <summary>
    /// Appends a sequence of straight-line operations.
    /// </summary>
    public PineControlFlowFragment AppendOperations(IEnumerable<StackInstruction> instructions)
    {
        var builder = Nodes.ToBuilder();

        foreach (var instruction in instructions)
        {
            builder.Add(new PineControlFlowNode.Operation(instruction));
        }

        return new PineControlFlowFragment(builder.ToImmutable());
    }

    /// <summary>
    /// Appends a single node.
    /// </summary>
    public PineControlFlowFragment Append(PineControlFlowNode node) =>
        new(Nodes.Add(node));

    /// <summary>
    /// Appends all nodes of another fragment.
    /// </summary>
    public PineControlFlowFragment Append(PineControlFlowFragment other) =>
        new(Nodes.AddRange(other.Nodes));

    /// <summary>
    /// The instruction of the last node if that node is a straight-line operation, otherwise null.
    /// Operations nested in control-flow nodes are never returned, since they do not execute last on every path.
    /// </summary>
    public StackInstruction? LastOperationOrNull =>
        Nodes.Count > 0 && Nodes[^1] is PineControlFlowNode.Operation operation
        ?
        operation.Instruction
        :
        null;

    /// <summary>
    /// Replaces the last node, which must be a straight-line operation, with the given operation.
    /// </summary>
    public PineControlFlowFragment ReplaceLastOperation(StackInstruction instruction)
    {
        if (LastOperationOrNull is null)
        {
            throw new InvalidOperationException("Fragment does not end with a straight-line operation.");
        }

        return new PineControlFlowFragment(Nodes.SetItem(Nodes.Count - 1, new PineControlFlowNode.Operation(instruction)));
    }
}
