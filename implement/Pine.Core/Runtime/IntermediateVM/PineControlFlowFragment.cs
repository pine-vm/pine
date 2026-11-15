using Pine.Core.PineLanguage;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Numerics;

namespace Pine.Core.Runtime.IntermediateVM;

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
        private static readonly FrozenDictionary<StackInstruction, Operation> s_shared = CreateShared();

        private static FrozenDictionary<StackInstruction, Operation> CreateShared()
        {
            StackInstruction[] instructions =
                [
                    StackInstruction.Pop, StackInstruction.Length, StackInstruction.Reverse,
                    StackInstruction.Head_Generic, StackInstruction.Concat_Generic, StackInstruction.Equal_Generic,
                    StackInstruction.Negate, StackInstruction.Int_Add_Binary, StackInstruction.Int_Add_Generic,
                    StackInstruction.Int_Mul_Binary, StackInstruction.Int_Mul_Generic, StackInstruction.Skip_Binary,
                    StackInstruction.Bit_Not, StackInstruction.Bit_And_Generic, StackInstruction.Skip_Head_Binary,
                    StackInstruction.Skip_Generic, StackInstruction.Take_Generic, StackInstruction.Take_Binary,
                    StackInstruction.Concat_Binary, StackInstruction.Equal_Binary, StackInstruction.Not_Equal_Binary,
                    StackInstruction.Is_List_Value, StackInstruction.Is_Blob_Value, StackInstruction.Int_Is_Sorted_Asc_Generic,
                    StackInstruction.Int_Less_Than_Binary, StackInstruction.Int_Less_Than_Or_Equal_Binary,
                    StackInstruction.Eval_Binary, StackInstruction.Bit_And_Binary, StackInstruction.Bit_Or_Generic,
                    StackInstruction.Bit_Or_Binary, StackInstruction.Bit_Xor_Generic, StackInstruction.Bit_Xor_Binary,
                    StackInstruction.Bit_Shift_Left_Binary, StackInstruction.Bit_Shift_Left_Generic,
                    StackInstruction.Bit_Shift_Right_Binary, StackInstruction.Bit_Shift_Right_Generic,
                    StackInstruction.Slice_Skip_Var_Take_Var, StackInstruction.Logical_And_Binary
                ];

            return
                instructions.ToFrozenDictionary<StackInstruction, StackInstruction, Operation>(
                    instruction => instruction,
                    instruction => new Operation(instruction),
                    ReferenceEqualityComparer.Instance);
        }

        internal static Operation FromInstruction(StackInstruction instruction) =>
            s_shared.TryGetValue(instruction, out var shared) ? shared : new(instruction);

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
    public sealed record JumpToEntry : PineControlFlowNode
    {
        /// <summary>A shared, stateless loop transfer.</summary>
        public static readonly JumpToEntry Instance = new();
    }

    /// <summary>
    /// Whether the instruction kind transfers control within a frame and therefore needs a structural model.
    /// </summary>
    public static bool IsControlTransfer(StackInstructionKind kind) =>
        kind is
        StackInstructionKind.Jump_Const or
        StackInstructionKind.Jump_If_Equal_Const or
        StackInstructionKind.Jump_If_Length_Equal_Const or
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
        Append(PineControlFlowNode.Operation.FromInstruction(instruction));

    /// <summary>
    /// Appends a sequence of straight-line operations.
    /// </summary>
    public PineControlFlowFragment AppendOperations(IEnumerable<StackInstruction> instructions)
    {
        ImmutableList<PineControlFlowNode>.Builder? builder = null;

        foreach (var instruction in instructions)
        {
            builder ??= Nodes.ToBuilder();
            builder.Add(PineControlFlowNode.Operation.FromInstruction(instruction));
        }

        return builder is null ? this : new PineControlFlowFragment(builder.ToImmutable());
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
        other.Nodes.IsEmpty ? this : Nodes.IsEmpty ? other : new(Nodes.AddRange(other.Nodes));

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
        if (LastOperationOrNull is not { } last)
        {
            throw new InvalidOperationException("Fragment does not end with a straight-line operation.");
        }

        return
            ReferenceEquals(last, instruction)
            ?
            this
            :
            new PineControlFlowFragment(
                Nodes.SetItem(Nodes.Count - 1, PineControlFlowNode.Operation.FromInstruction(instruction)));
    }
}
