using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Identifies a basic block independently of its eventual instruction offset.
/// </summary>
public readonly record struct PineBlockId(int Value);

/// <summary>
/// Identifies a value while validating and lowering control flow.
/// </summary>
public readonly record struct PineVirtualValueId(int Value);

/// <summary>
/// An ordinary operation and the virtual values it consumes and produces.
/// </summary>
public sealed record PineControlFlowOperation(
    StackInstruction Instruction,
    ImmutableArray<PineVirtualValueId> Inputs,
    ImmutableArray<PineVirtualValueId> Results);

/// <summary>
/// Ends a basic block and names all possible successor blocks explicitly.
/// </summary>
public abstract record PineControlFlowTerminator
{
    private PineControlFlowTerminator()
    {
    }

    /// <summary>
    /// Returns the value at the top of the evaluation stack.
    /// </summary>
    public sealed record Return(StackInstruction Instruction) : PineControlFlowTerminator;

    /// <summary>
    /// Transfers control and the block arguments to one successor.
    /// </summary>
    public sealed record Jump(
        PineBlockId Target,
        ImmutableArray<PineVirtualValueId> Arguments,
        StackInstruction? Instruction) : PineControlFlowTerminator;

    /// <summary>
    /// Consumes a condition and transfers control to one of two successors.
    /// </summary>
    public sealed record ConditionalJump(
        PineBlockId FallThrough,
        PineBlockId Branch,
        ImmutableArray<PineVirtualValueId> FallThroughArguments,
        ImmutableArray<PineVirtualValueId> BranchArguments,
        StackInstruction Instruction) : PineControlFlowTerminator;

    /// <summary>
    /// Invokes another frame and continues in a return block.
    /// </summary>
    public sealed record Invoke(
        PineBlockId Continuation,
        ImmutableArray<PineVirtualValueId> Arguments,
        StackInstruction Instruction) : PineControlFlowTerminator;

    /// <summary>
    /// Invokes another frame without a continuation in the current frame.
    /// </summary>
    public sealed record TailInvoke(
        StackInstruction InvokeInstruction,
        StackInstruction ReturnInstruction) : PineControlFlowTerminator;
}

/// <summary>
/// A sequential basic block with virtual block parameters and one terminator.
/// </summary>
public sealed record PineBasicBlock(
    PineBlockId Id,
    ImmutableArray<PineVirtualValueId> Parameters,
    ImmutableArray<PineControlFlowOperation> Operations,
    PineControlFlowTerminator Terminator);

/// <summary>
/// Validated control-flow representation used between recursive expression compilation and
/// physical stack-instruction layout.
/// </summary>
public sealed record PineControlFlowGraph(
    PineBlockId Entry,
    ImmutableArray<PineBasicBlock> Blocks)
{
    /// <summary>
    /// Builds a control-flow graph from symbolic instruction positions.
    /// </summary>
    public static PineControlFlowGraph FromInstructions(
        IReadOnlyList<StackInstruction> instructions)
    {
        if (instructions.Count is 0)
        {
            throw new InvalidOperationException("Cannot build control flow for an empty instruction list.");
        }

        var leaders = new SortedSet<int> { 0 };

        for (var instructionIndex = 0; instructionIndex < instructions.Count; instructionIndex++)
        {
            var instruction = instructions[instructionIndex];

            switch (instruction.Kind)
            {
                case StackInstructionKind.Jump_Const:
                case StackInstructionKind.Jump_If_Equal_Const:
                    var targetIndex =
                        instructionIndex +
                        (instruction.JumpOffset ??
                        throw new InvalidOperationException(
                            $"Jump without offset at instruction {instructionIndex}."));

                    if (targetIndex < 0 || targetIndex >= instructions.Count)
                    {
                        throw new InvalidOperationException(
                            $"Jump at instruction {instructionIndex} targets {targetIndex} outside the frame.");
                    }

                    leaders.Add(targetIndex);

                    if (instructionIndex + 1 < instructions.Count)
                    {
                        leaders.Add(instructionIndex + 1);
                    }

                    break;

                case StackInstructionKind.Parse_And_Eval_Binary:
                case StackInstructionKind.Return:
                    if (instructionIndex + 1 < instructions.Count)
                    {
                        leaders.Add(instructionIndex + 1);
                    }

                    break;

                default:
                    break;
            }
        }

        var leaderIndexes = leaders.ToArray();

        var blockFromInstructionIndex =
            leaderIndexes
            .Select((instructionIndex, blockIndex) => (instructionIndex, blockIndex))
            .ToDictionary(item => item.instructionIndex, item => new PineBlockId(item.blockIndex));

        var rawBlocks = new List<RawBlock>(leaderIndexes.Length);

        for (var blockIndex = 0; blockIndex < leaderIndexes.Length; blockIndex++)
        {
            var firstInstructionIndex = leaderIndexes[blockIndex];

            var endInstructionIndexExclusive =
                blockIndex + 1 < leaderIndexes.Length
                ?
                leaderIndexes[blockIndex + 1]
                :
                instructions.Count;

            rawBlocks.Add(
                BuildRawBlock(
                    new PineBlockId(blockIndex),
                    firstInstructionIndex,
                    endInstructionIndexExclusive,
                    instructions,
                    blockFromInstructionIndex));
        }

        var stackDepthAtEntry = ComputeStackDepths(rawBlocks);
        var nextVirtualValue = 0;
        var blocks = ImmutableArray.CreateBuilder<PineBasicBlock>(rawBlocks.Count);

        foreach (var rawBlock in rawBlocks)
        {
            var stack =
                new List<PineVirtualValueId>(
                    Enumerable.Range(0, stackDepthAtEntry[rawBlock.Id])
                    .Select(_ => new PineVirtualValueId(nextVirtualValue++)));

            var parameters = stack.ToImmutableArray();
            var operations = ImmutableArray.CreateBuilder<PineControlFlowOperation>();

            foreach (var instruction in rawBlock.Operations)
            {
                var details = StackInstruction.GetDetails(instruction);
                var inputs = PopVirtualValues(stack, details.PopCount, rawBlock.Id);
                var results = ImmutableArray.CreateBuilder<PineVirtualValueId>(details.PushCount);

                for (var resultIndex = 0; resultIndex < details.PushCount; resultIndex++)
                {
                    var result = new PineVirtualValueId(nextVirtualValue++);
                    stack.Add(result);
                    results.Add(result);
                }

                operations.Add(
                    new PineControlFlowOperation(
                        instruction,
                        inputs,
                        results.MoveToImmutable()));
            }

            ApplyTerminatorVirtualStack(rawBlock.Terminator, stack, rawBlock.Id, ref nextVirtualValue);

            blocks.Add(
                new PineBasicBlock(
                    rawBlock.Id,
                    parameters,
                    operations.ToImmutable(),
                    AddArguments(rawBlock.Terminator, stack)));
        }

        var graph = new PineControlFlowGraph(new PineBlockId(0), blocks.MoveToImmutable());
        graph.Validate();
        return graph;
    }

    /// <summary>
    /// Validates block termination, edge arity, virtual-value scope, and loop stack invariants.
    /// </summary>
    public void Validate()
    {
        if (Blocks.IsEmpty || Entry.Value < 0 || Entry.Value >= Blocks.Length)
        {
            throw new InvalidOperationException("Control-flow graph has no valid entry block.");
        }

        var blockById = Blocks.ToDictionary(block => block.Id);

        foreach (var block in Blocks)
        {
            var valuesInScope = block.Parameters.ToHashSet();

            foreach (var operation in block.Operations)
            {
                if (operation.Inputs.Any(input => !valuesInScope.Contains(input)))
                {
                    throw new InvalidOperationException(
                        $"Block {block.Id.Value} uses a virtual value before its definition.");
                }

                foreach (var result in operation.Results)
                {
                    if (!valuesInScope.Add(result))
                    {
                        throw new InvalidOperationException(
                            $"Virtual value {result.Value} is defined more than once.");
                    }
                }
            }

            foreach (var (target, arguments) in Successors(block.Terminator))
            {
                if (!blockById.TryGetValue(target, out var targetBlock))
                {
                    throw new InvalidOperationException(
                        $"Block {block.Id.Value} targets missing block {target.Value}.");
                }

                if (arguments.Length != targetBlock.Parameters.Length)
                {
                    throw new InvalidOperationException(
                        $"Edge {block.Id.Value} -> {target.Value} supplies {arguments.Length} arguments " +
                        $"for {targetBlock.Parameters.Length} parameters.");
                }

                if (target.Value <= block.Id.Value && arguments.Length is not 0)
                {
                    throw new InvalidOperationException(
                        $"Loop edge {block.Id.Value} -> {target.Value} carries a non-empty evaluation stack.");
                }
            }
        }
    }

    /// <summary>
    /// Assigns physical instruction offsets after the graph has been validated.
    /// </summary>
    public ImmutableArray<StackInstruction> LowerToStackInstructions()
    {
        Validate();

        var firstInstructionIndexByBlock = new Dictionary<PineBlockId, int>();
        var instructionCount = 0;

        foreach (var block in Blocks)
        {
            firstInstructionIndexByBlock.Add(block.Id, instructionCount);
            instructionCount += block.Operations.Length + TerminatorInstructionCount(block.Terminator);
        }

        var result = ImmutableArray.CreateBuilder<StackInstruction>(instructionCount);

        foreach (var block in Blocks)
        {
            foreach (var operation in block.Operations)
            {
                result.Add(operation.Instruction);
            }

            switch (block.Terminator)
            {
                case PineControlFlowTerminator.Return returnTerminator:
                    result.Add(returnTerminator.Instruction);
                    break;

                case PineControlFlowTerminator.Jump jump:
                    if (jump.Instruction is not null)
                    {
                        result.Add(
                            StackInstruction.Jump_Unconditional(
                                firstInstructionIndexByBlock[jump.Target] - result.Count));
                    }
                    else if (jump.Target.Value != block.Id.Value + 1)
                    {
                        throw new InvalidOperationException(
                            $"Implicit edge from block {block.Id.Value} is not a fall-through edge.");
                    }

                    break;

                case PineControlFlowTerminator.ConditionalJump conditional:
                    result.Add(
                        StackInstruction.Jump_If_Equal(
                            firstInstructionIndexByBlock[conditional.Branch] - result.Count,
                            conditional.Instruction.Literal ??
                            throw new InvalidOperationException(
                                $"Conditional block {block.Id.Value} has no comparison literal.")));

                    break;

                case PineControlFlowTerminator.Invoke invoke:
                    result.Add(invoke.Instruction);

                    if (invoke.Continuation.Value != block.Id.Value + 1)
                    {
                        throw new InvalidOperationException(
                            $"Invoke continuation from block {block.Id.Value} is not laid out next.");
                    }

                    break;

                case PineControlFlowTerminator.TailInvoke tailInvoke:
                    result.Add(tailInvoke.InvokeInstruction);
                    result.Add(tailInvoke.ReturnInstruction);
                    break;

                default:
                    throw new NotImplementedException(
                        "LowerToStackInstructions does not handle terminator variant: " +
                        block.Terminator.GetType().Name);
            }
        }

        return result.MoveToImmutable();
    }

    private static RawBlock BuildRawBlock(
        PineBlockId id,
        int firstInstructionIndex,
        int endInstructionIndexExclusive,
        IReadOnlyList<StackInstruction> instructions,
        IReadOnlyDictionary<int, PineBlockId> blockFromInstructionIndex)
    {
        var blockInstructions =
            instructions
            .Skip(firstInstructionIndex)
            .Take(endInstructionIndexExclusive - firstInstructionIndex)
            .ToArray();

        if (blockInstructions.Length is 0)
        {
            throw new InvalidOperationException($"Block {id.Value} contains no instructions.");
        }

        var last = blockInstructions[^1];
        var operations = blockInstructions.AsSpan(0, blockInstructions.Length - 1).ToArray();

        PineControlFlowTerminator terminator =
            last.Kind switch
            {
                StackInstructionKind.Return =>
                new PineControlFlowTerminator.Return(last),

                StackInstructionKind.Jump_Const =>
                new PineControlFlowTerminator.Jump(
                    Target: ResolveJumpTarget(
                        endInstructionIndexExclusive - 1,
                        last,
                        blockFromInstructionIndex),
                    Arguments: [],
                    Instruction: last),

                StackInstructionKind.Jump_If_Equal_Const =>
                new PineControlFlowTerminator.ConditionalJump(
                    FallThrough: ResolveBlock(
                        endInstructionIndexExclusive,
                        blockFromInstructionIndex),
                    Branch: ResolveJumpTarget(
                        endInstructionIndexExclusive - 1,
                        last,
                        blockFromInstructionIndex),
                    FallThroughArguments: [],
                    BranchArguments: [],
                    Instruction: last),

                StackInstructionKind.Parse_And_Eval_Binary =>
                new PineControlFlowTerminator.Invoke(
                    Continuation: ResolveBlock(
                        endInstructionIndexExclusive,
                        blockFromInstructionIndex),
                    Arguments: [],
                    Instruction: last),

                _ when endInstructionIndexExclusive < instructions.Count =>
                new PineControlFlowTerminator.Jump(
                    Target: ResolveBlock(
                        endInstructionIndexExclusive,
                        blockFromInstructionIndex),
                    Arguments: [],
                    Instruction: null),

                _ =>
                throw new InvalidOperationException(
                    $"Final block {id.Value} does not end in a return or transfer.")
            };

        if (terminator is PineControlFlowTerminator.Jump { Instruction: null })
        {
            operations = blockInstructions;
        }

        return new RawBlock(id, [.. operations], terminator);
    }

    private static IReadOnlyDictionary<PineBlockId, int> ComputeStackDepths(
        IReadOnlyList<RawBlock> blocks)
    {
        var depthAtEntry =
            new Dictionary<PineBlockId, int>
            {
                [new PineBlockId(0)] = 0
            };

        var worklist = new Stack<PineBlockId>();
        worklist.Push(new PineBlockId(0));

        while (worklist.TryPop(out var blockId))
        {
            var block = blocks[blockId.Value];
            var depth = depthAtEntry[blockId];

            foreach (var instruction in block.Operations)
            {
                depth = ApplyStackEffect(instruction, depth, blockId);
            }

            depth = ApplyTerminatorStackEffect(block.Terminator, depth, blockId);

            foreach (var (target, _) in Successors(block.Terminator))
            {
                if (depthAtEntry.TryGetValue(target, out var existingDepth))
                {
                    if (existingDepth != depth)
                    {
                        throw new InvalidOperationException(
                            $"Inconsistent stack depth at block {target.Value} ({existingDepth} vs {depth}).");
                    }

                    continue;
                }

                depthAtEntry.Add(target, depth);
                worklist.Push(target);
            }
        }

        foreach (var block in blocks)
        {
            depthAtEntry.TryAdd(block.Id, MinimumInputDepth(block));
        }

        return depthAtEntry;
    }

    private static int MinimumInputDepth(RawBlock block)
    {
        var relativeDepth = 0;
        var minimumRelativeDepth = 0;

        void Apply(StackInstruction instruction)
        {
            var details = StackInstruction.GetDetails(instruction);
            relativeDepth += -details.PopCount + details.PushCount;
            minimumRelativeDepth = Math.Min(minimumRelativeDepth, relativeDepth);
        }

        foreach (var instruction in block.Operations)
        {
            Apply(instruction);
        }

        switch (block.Terminator)
        {
            case PineControlFlowTerminator.Return returnTerminator:
                Apply(returnTerminator.Instruction);
                break;

            case PineControlFlowTerminator.Jump jump when jump.Instruction is not null:
                Apply(jump.Instruction);
                break;

            case PineControlFlowTerminator.Jump:
                break;

            case PineControlFlowTerminator.ConditionalJump conditional:
                Apply(conditional.Instruction);
                break;

            case PineControlFlowTerminator.Invoke invoke:
                Apply(invoke.Instruction);
                break;

            case PineControlFlowTerminator.TailInvoke tailInvoke:
                Apply(tailInvoke.InvokeInstruction);
                Apply(tailInvoke.ReturnInstruction);
                break;

            default:
                throw new NotImplementedException(
                    "MinimumInputDepth does not handle terminator variant: " +
                    block.Terminator.GetType().Name);
        }

        return -minimumRelativeDepth;
    }

    private static int ApplyTerminatorStackEffect(
        PineControlFlowTerminator terminator,
        int depth,
        PineBlockId blockId) =>
        terminator switch
        {
            PineControlFlowTerminator.Return returnTerminator =>
            ApplyStackEffect(returnTerminator.Instruction, depth, blockId),

            PineControlFlowTerminator.Jump jump =>
            jump.Instruction is null
            ?
            depth
            :
            ApplyStackEffect(jump.Instruction, depth, blockId),

            PineControlFlowTerminator.ConditionalJump conditional =>
            ApplyStackEffect(conditional.Instruction, depth, blockId),

            PineControlFlowTerminator.Invoke invoke =>
            ApplyStackEffect(invoke.Instruction, depth, blockId),

            PineControlFlowTerminator.TailInvoke tailInvoke =>
            ApplyStackEffect(
                tailInvoke.ReturnInstruction,
                ApplyStackEffect(tailInvoke.InvokeInstruction, depth, blockId),
                blockId),

            _ =>
            throw new NotImplementedException(
                "ApplyTerminatorStackEffect does not handle terminator variant: " +
                terminator.GetType().Name)
        };

    private static int ApplyStackEffect(
        StackInstruction instruction,
        int depth,
        PineBlockId blockId)
    {
        var details = StackInstruction.GetDetails(instruction);

        if (depth < details.PopCount)
        {
            throw new InvalidOperationException(
                $"Stack underflow in block {blockId.Value} while applying {instruction.Kind}.");
        }

        return depth - details.PopCount + details.PushCount;
    }

    private static PineControlFlowTerminator AddArguments(
        PineControlFlowTerminator terminator,
        IReadOnlyList<PineVirtualValueId> stack)
    {
        var arguments = stack.ToImmutableArray();

        return
            terminator switch
            {
                PineControlFlowTerminator.Return returnTerminator =>
                returnTerminator,

                PineControlFlowTerminator.Jump jump =>
                jump with { Arguments = arguments },

                PineControlFlowTerminator.ConditionalJump conditional =>
                conditional with
                {
                    FallThroughArguments = arguments,
                    BranchArguments = arguments
                },

                PineControlFlowTerminator.Invoke invoke =>
                invoke with { Arguments = arguments },

                PineControlFlowTerminator.TailInvoke tailInvoke =>
                tailInvoke,

                _ =>
                throw new NotImplementedException(
                    "AddArguments does not handle terminator variant: " +
                    terminator.GetType().Name)
            };
    }

    private static void ApplyTerminatorVirtualStack(
        PineControlFlowTerminator terminator,
        List<PineVirtualValueId> stack,
        PineBlockId blockId,
        ref int nextVirtualValue)
    {
        IEnumerable<StackInstruction> instructions =
            terminator switch
            {
                PineControlFlowTerminator.Return returnTerminator =>
                [returnTerminator.Instruction],

                PineControlFlowTerminator.Jump jump =>
                jump.Instruction is null ? [] : [jump.Instruction],

                PineControlFlowTerminator.ConditionalJump conditional =>
                [conditional.Instruction],

                PineControlFlowTerminator.Invoke invoke =>
                [invoke.Instruction],

                PineControlFlowTerminator.TailInvoke tailInvoke =>
                [tailInvoke.InvokeInstruction, tailInvoke.ReturnInstruction],

                _ =>
                throw new NotImplementedException(
                    "ApplyTerminatorVirtualStack does not handle terminator variant: " +
                    terminator.GetType().Name)
            };

        foreach (var instruction in instructions)
        {
            var details = StackInstruction.GetDetails(instruction);
            PopVirtualValues(stack, details.PopCount, blockId);

            for (var resultIndex = 0; resultIndex < details.PushCount; resultIndex++)
            {
                stack.Add(new PineVirtualValueId(nextVirtualValue++));
            }
        }
    }

    private static IEnumerable<(PineBlockId Target, ImmutableArray<PineVirtualValueId> Arguments)>
        Successors(PineControlFlowTerminator terminator)
    {
        switch (terminator)
        {
            case PineControlFlowTerminator.Return:
            case PineControlFlowTerminator.TailInvoke:
                yield break;

            case PineControlFlowTerminator.Jump jump:
                yield return (jump.Target, jump.Arguments);
                yield break;

            case PineControlFlowTerminator.ConditionalJump conditional:
                yield return (conditional.FallThrough, conditional.FallThroughArguments);
                yield return (conditional.Branch, conditional.BranchArguments);
                yield break;

            case PineControlFlowTerminator.Invoke invoke:
                yield return (invoke.Continuation, invoke.Arguments);
                yield break;

            default:
                throw new NotImplementedException(
                    "Successors does not handle terminator variant: " +
                    terminator.GetType().Name);
        }
    }

    private static ImmutableArray<PineVirtualValueId> PopVirtualValues(
        List<PineVirtualValueId> stack,
        int count,
        PineBlockId blockId)
    {
        if (stack.Count < count)
        {
            throw new InvalidOperationException(
                $"Virtual-value stack underflow in block {blockId.Value}.");
        }

        var first = stack.Count - count;
        var result = stack.Skip(first).ToImmutableArray();
        stack.RemoveRange(first, count);
        return result;
    }

    private static PineBlockId ResolveJumpTarget(
        int instructionIndex,
        StackInstruction instruction,
        IReadOnlyDictionary<int, PineBlockId> blockFromInstructionIndex) =>
        ResolveBlock(
            instructionIndex +
            (instruction.JumpOffset ??
            throw new InvalidOperationException($"Jump at {instructionIndex} has no offset.")),
            blockFromInstructionIndex);

    private static PineBlockId ResolveBlock(
        int instructionIndex,
        IReadOnlyDictionary<int, PineBlockId> blockFromInstructionIndex) =>
        blockFromInstructionIndex.TryGetValue(instructionIndex, out var block)
        ?
        block
        :
        throw new InvalidOperationException(
            $"Instruction {instructionIndex} is not the start of a basic block.");

    private static int TerminatorInstructionCount(PineControlFlowTerminator terminator) =>
        terminator switch
        {
            PineControlFlowTerminator.Return => 1,
            PineControlFlowTerminator.Jump jump => jump.Instruction is null ? 0 : 1,
            PineControlFlowTerminator.ConditionalJump => 1,
            PineControlFlowTerminator.Invoke => 1,
            PineControlFlowTerminator.TailInvoke => 2,

            _ =>
            throw new NotImplementedException(
                "TerminatorInstructionCount does not handle terminator variant: " +
                terminator.GetType().Name)
        };

    private sealed record RawBlock(
        PineBlockId Id,
        ImmutableArray<StackInstruction> Operations,
        PineControlFlowTerminator Terminator);
}
