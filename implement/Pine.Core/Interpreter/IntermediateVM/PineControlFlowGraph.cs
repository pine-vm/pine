using Pine.Core.Internal;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

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
/// One case of a <see cref="PineControlFlowTerminator.Switch"/>: the literal to compare with and the successor block.
/// </summary>
public readonly record struct PineSwitchCase(
    PineValue Literal,
    PineBlockId Target);

/// <summary>
/// Ends a basic block and names all possible successor blocks explicitly.
/// <para>
/// Terminators do not contain instruction offsets. Offsets are only assigned when lowering the graph
/// to sequential stack instructions in <see cref="PineControlFlowGraph.LowerToStackInstructions"/>.
/// </para>
/// </summary>
public abstract record PineControlFlowTerminator
{
    private PineControlFlowTerminator()
    {
    }

    /// <summary>
    /// Returns the value at the top of the evaluation stack.
    /// </summary>
    public sealed record Return : PineControlFlowTerminator;

    /// <summary>
    /// Transfers control and the block arguments to one successor.
    /// </summary>
    /// <param name="Target">The successor block.</param>
    /// <param name="Arguments">The values passed to the parameters of <paramref name="Target"/>.</param>
    /// <param name="IsFallThrough">
    /// Whether the edge is implicit: The target must be laid out directly after this block,
    /// and lowering emits no jump instruction.
    /// </param>
    public sealed record Jump(
        PineBlockId Target,
        ImmutableArray<PineVirtualValueId> Arguments,
        bool IsFallThrough) : PineControlFlowTerminator;

    /// <summary>
    /// Consumes a condition and transfers control to <paramref name="Branch"/> if it equals
    /// <paramref name="Literal"/>, otherwise to <paramref name="FallThrough"/>.
    /// </summary>
    public sealed record ConditionalJump(
        PineBlockId FallThrough,
        PineBlockId Branch,
        ImmutableArray<PineVirtualValueId> FallThroughArguments,
        ImmutableArray<PineVirtualValueId> BranchArguments,
        PineValue Literal) : PineControlFlowTerminator;

    /// <summary>
    /// Consumes the scrutinee and transfers control based on an ordered equality jump table.
    /// </summary>
    public sealed record Switch(
        PineSwitchKind Kind,
        PineBlockId FallThrough,
        ImmutableArray<PineSwitchCase> Cases,
        ImmutableArray<PineVirtualValueId> Arguments,
        BigInteger SkipCountMultiplier) : PineControlFlowTerminator
    {
        /// <summary>
        /// Number of values the switch consumes from the evaluation stack.
        /// </summary>
        public int PopCount =>
            Kind is PineSwitchKind.SliceSkipVarEqual ? 2 : 1;
    }

    /// <summary>
    /// Invokes another frame and continues in a return block.
    /// </summary>
    public sealed record Invoke(
        PineBlockId Continuation,
        ImmutableArray<PineVirtualValueId> Arguments,
        ImmutableArray<PineVirtualValueId> Inputs,
        StackInstruction Instruction) : PineControlFlowTerminator;

    /// <summary>
    /// Invokes another frame and returns its result without a continuation in the current frame.
    /// </summary>
    public sealed record TailInvoke(
        StackInstruction InvokeInstruction) : PineControlFlowTerminator;
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
/// <para>
/// Block IDs equal the index of the block in <see cref="Blocks"/>, which is also the layout order used
/// when lowering to stack instructions.
/// </para>
/// </summary>
public sealed partial record PineControlFlowGraph(
    PineBlockId Entry,
    ImmutableArray<PineBasicBlock> Blocks)
{
    /// <summary>
    /// Builds a control-flow graph from structured control flow emitted by expression compilation.
    /// The graph returns the value remaining on the evaluation stack at the end of <paramref name="fragment"/>.
    /// </summary>
    public static PineControlFlowGraph FromFragment(PineControlFlowFragment fragment)
    {
        var builder = new GraphBuilder();

        var entry = builder.StartBlock(parameterCount: 0);

        if (builder.Emit(fragment, entry) is { } end)
        {
            GraphBuilder.Pop(end, 1);
            end.Terminator = new PineControlFlowTerminator.Return();
        }

        var graph =
            new PineControlFlowGraph(entry.Id, builder.Build())
            .RemoveEmptyForwardingBlocks();

        graph.Validate();

        return graph;
    }

    private sealed class BlockUnderConstruction(
        PineBlockId id,
        ImmutableArray<PineVirtualValueId> parameters)
    {
        public PineBlockId Id { get; } = id;

        public ImmutableArray<PineVirtualValueId> Parameters { get; } = parameters;

        public List<PineVirtualValueId> Stack { get; } = [.. parameters];

        public ImmutableArray<PineControlFlowOperation>.Builder Operations { get; } =
            ImmutableArray.CreateBuilder<PineControlFlowOperation>();

        public PineControlFlowTerminator? Terminator { get; set; }
    }

    private sealed class GraphBuilder
    {
        private readonly List<BlockUnderConstruction> _blocks = [];

        private int _nextVirtualValue;

        public BlockUnderConstruction StartBlock(int parameterCount)
        {
            var parameters = ImmutableArray.CreateBuilder<PineVirtualValueId>(parameterCount);

            for (var i = 0; i < parameterCount; i++)
            {
                parameters.Add(NewValue());
            }

            var block =
                new BlockUnderConstruction(
                    new PineBlockId(_blocks.Count),
                    parameters.MoveToImmutable());

            _blocks.Add(block);

            return block;
        }

        private PineVirtualValueId NewValue() =>
            new(_nextVirtualValue++);

        /// <summary>
        /// Emits the fragment starting in <paramref name="current"/>.
        /// Returns the block in which control continues after the fragment,
        /// or null if the fragment ends in a transfer.
        /// </summary>
        public BlockUnderConstruction? Emit(
            PineControlFlowFragment fragment,
            BlockUnderConstruction current)
        {
            BlockUnderConstruction? open = current;

            foreach (var node in fragment.Nodes)
            {
                if (open is null)
                {
                    throw new InvalidOperationException(
                        "Fragment contains a node after a control transfer: " + node.GetType().Name);
                }

                open = EmitNode(node, open);
            }

            return open;
        }

        private BlockUnderConstruction? EmitNode(
            PineControlFlowNode node,
            BlockUnderConstruction current)
        {
            switch (node)
            {
                case PineControlFlowNode.Operation operation:
                    return EmitOperation(operation.Instruction, current);

                case PineControlFlowNode.Conditional conditional:
                    {
                        Pop(current, 1);

                        var arguments = current.Stack.ToImmutableArray();

                        var fallThroughStart = StartBlock(arguments.Length);
                        var fallThroughEnd = Emit(conditional.FallThrough, fallThroughStart);

                        var branchStart = StartBlock(arguments.Length);
                        var branchEnd = Emit(conditional.Branch, branchStart);

                        current.Terminator =
                            new PineControlFlowTerminator.ConditionalJump(
                                FallThrough: fallThroughStart.Id,
                                Branch: branchStart.Id,
                                FallThroughArguments: arguments,
                                BranchArguments: arguments,
                                Literal: conditional.Literal);

                        return Join(arguments.Length + 1, [fallThroughEnd, branchEnd]);
                    }

                case PineControlFlowNode.Switch switchNode:
                    {
                        var terminatorPopCount =
                            switchNode.Kind is PineSwitchKind.SliceSkipVarEqual ? 2 : 1;

                        Pop(current, terminatorPopCount);

                        var arguments = current.Stack.ToImmutableArray();

                        var defaultStart = StartBlock(arguments.Length);

                        var ends =
                            new List<BlockUnderConstruction?>
                            {
                                Emit(switchNode.Default, defaultStart)
                            };

                        var branchStarts = new PineBlockId[switchNode.Branches.Length];

                        for (var branchIndex = 0; branchIndex < switchNode.Branches.Length; branchIndex++)
                        {
                            var branchStart = StartBlock(arguments.Length);
                            branchStarts[branchIndex] = branchStart.Id;
                            ends.Add(Emit(switchNode.Branches[branchIndex], branchStart));
                        }

                        current.Terminator =
                            new PineControlFlowTerminator.Switch(
                                Kind: switchNode.Kind,
                                FallThrough: defaultStart.Id,
                                Cases:
                                [
                                .. switchNode.Cases.Select(
                                    switchCase =>
                                    new PineSwitchCase(switchCase.Literal, branchStarts[switchCase.BranchIndex]))
                                ],
                                Arguments: arguments,
                                SkipCountMultiplier: switchNode.SkipCountMultiplier);

                        return Join(arguments.Length + 1, ends);
                    }

                case PineControlFlowNode.JumpToEntry:
                    current.Terminator =
                        new PineControlFlowTerminator.Jump(
                            Target: _blocks[0].Id,
                            Arguments: current.Stack.ToImmutableArray(),
                            IsFallThrough: false);

                    return null;

                default:
                    throw new NotImplementedException(
                        "Unexpected control-flow node: " + node.GetType().Name);
            }
        }

        private BlockUnderConstruction EmitOperation(
            StackInstruction instruction,
            BlockUnderConstruction current)
        {
            var details = StackInstruction.GetDetails(instruction);
            var inputs = Pop(current, details.PopCount);
            var results = ImmutableArray.CreateBuilder<PineVirtualValueId>(details.PushCount);

            for (var resultIndex = 0; resultIndex < details.PushCount; resultIndex++)
            {
                var result = NewValue();
                current.Stack.Add(result);
                results.Add(result);
            }

            if (!IsInvocation(instruction.Kind))
            {
                current.Operations.Add(
                    new PineControlFlowOperation(
                        instruction,
                        inputs,
                        results.MoveToImmutable()));

                return current;
            }

            var arguments = current.Stack.ToImmutableArray();
            var continuation = StartBlock(arguments.Length);

            current.Terminator =
                new PineControlFlowTerminator.Invoke(
                    Continuation: continuation.Id,
                    Arguments: arguments,
                    Inputs: inputs,
                    Instruction: instruction);

            return continuation;
        }

        /// <summary>
        /// Creates the block where control continues after branches. The last branch falls through,
        /// since its end is the block laid out directly before the join.
        /// </summary>
        private BlockUnderConstruction Join(
            int depthIfUnreachable,
            IReadOnlyList<BlockUnderConstruction?> branchEnds)
        {
            int? depth = null;

            foreach (var branchEnd in branchEnds)
            {
                if (branchEnd is null)
                {
                    continue;
                }

                if (depth is { } previousDepth && previousDepth != branchEnd.Stack.Count)
                {
                    throw new InvalidOperationException(
                        $"Inconsistent stack depth at join ({previousDepth} vs {branchEnd.Stack.Count}).");
                }

                depth = branchEnd.Stack.Count;
            }

            var join = StartBlock(depth ?? depthIfUnreachable);

            for (var branchIndex = 0; branchIndex < branchEnds.Count; branchIndex++)
            {
                if (branchEnds[branchIndex] is not { } branchEnd)
                {
                    continue;
                }

                branchEnd.Terminator =
                    new PineControlFlowTerminator.Jump(
                        Target: join.Id,
                        Arguments: branchEnd.Stack.ToImmutableArray(),
                        IsFallThrough: branchIndex == branchEnds.Count - 1);
            }

            return join;
        }

        public static ImmutableArray<PineVirtualValueId> Pop(
            BlockUnderConstruction block,
            int count) =>
            PopVirtualValues(block.Stack, count, block.Id);

        public ImmutableArray<PineBasicBlock> Build() =>
            [
            .. _blocks.Select(
                block =>
                new PineBasicBlock(
                    block.Id,
                    block.Parameters,
                    block.Operations.ToImmutable(),
                    block.Terminator ??
                    throw new InvalidOperationException(
                        $"Block {block.Id.Value} has no terminator.")))
            ];
    }

    /// <summary>
    /// Whether the instruction invokes another frame, which ends a basic block with
    /// <see cref="PineControlFlowTerminator.Invoke"/>.
    /// </summary>
    public static bool IsInvocation(StackInstructionKind kind) =>
        kind is
        StackInstructionKind.Eval_Binary or
        StackInstructionKind.Eval_Multi or
        StackInstructionKind.Eval_Const or
        StackInstructionKind.Invoke_StackFrame_Const;

    /// <summary>
    /// Removes blocks without operations that only forward their parameters to the block laid out next,
    /// redirecting their predecessors to the forwarding target.
    /// </summary>
    private PineControlFlowGraph RemoveEmptyForwardingBlocks()
    {
        var forwardTargets = new Dictionary<PineBlockId, PineBlockId>();

        for (var blockIndex = Blocks.Length - 1; blockIndex >= 0; blockIndex--)
        {
            var block = Blocks[blockIndex];

            if (block.Id != Entry &&
                block.Operations.IsEmpty &&
                block.Terminator is PineControlFlowTerminator.Jump
                {
                    IsFallThrough: true,
                    Target: var target,
                    Arguments: var arguments
                } &&
                arguments.SequenceEqual(block.Parameters))
            {
                forwardTargets.Add(
                    block.Id,
                    forwardTargets.TryGetValue(target, out var transitiveTarget) ? transitiveTarget : target);
            }
        }

        if (forwardTargets.Count is 0)
        {
            return this;
        }

        var rewrittenBlocks =
            Blocks
            .Where(block => !forwardTargets.ContainsKey(block.Id))
            .Select(
                block =>
                block with
                {
                    Terminator =
                    RedirectTargets(
                        block.Terminator,
                        target => forwardTargets.TryGetValue(target, out var forwarded) ? forwarded : target)
                })
            .ToImmutableArray();

        return
            TryRemapBlockIds(rewrittenBlocks) ??
            throw new InvalidOperationException("Entry block was removed.");
    }

    /// <summary>
    /// Replaces explicit jumps to blocks that consist only of a return with a return.
    /// </summary>
    public PineControlFlowGraph ForwardJumpsToReturn()
    {
        var blocks = Blocks.ToArray();
        var changed = false;

        for (var blockIndex = blocks.Length - 1; blockIndex >= 0; blockIndex--)
        {
            if (blocks[blockIndex].Terminator is PineControlFlowTerminator.Jump
                {
                    IsFallThrough: false,
                    Target: var target
                } &&
                blocks[target.Value] is
                {
                    Operations.IsEmpty: true,
                    Terminator: PineControlFlowTerminator.Return
                })
            {
                blocks[blockIndex] =
                    blocks[blockIndex] with
                    {
                        Terminator = new PineControlFlowTerminator.Return()
                    };

                changed = true;
            }
        }

        if (!changed)
        {
            return this;
        }

        var graph = new PineControlFlowGraph(Entry, [.. blocks]);

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

        for (var blockIndex = 0; blockIndex < Blocks.Length; blockIndex++)
        {
            if (Blocks[blockIndex].Id.Value != blockIndex)
            {
                throw new InvalidOperationException(
                    $"Block at index {blockIndex} has ID {Blocks[blockIndex].Id.Value}.");
            }
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

            if (block.Terminator is PineControlFlowTerminator.Switch switchTerminator)
            {
                if (switchTerminator.Cases.IsDefault)
                {
                    throw new InvalidOperationException(
                        $"Switch in block {block.Id.Value} has no cases.");
                }

                var caseLiterals = new HashSet<PineValue>();

                foreach (var switchCase in switchTerminator.Cases)
                {
                    if (!caseLiterals.Add(switchCase.Literal))
                    {
                        throw new InvalidOperationException(
                            $"Switch in block {block.Id.Value} contains duplicate case literals.");
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

                if (target == Entry && target.Value <= block.Id.Value && arguments.Length is not 0)
                {
                    throw new InvalidOperationException(
                        $"Loop edge {block.Id.Value} -> {target.Value} carries a non-empty evaluation stack.");
                }
            }
        }
    }

    /// <summary>
    /// Forwards edges through blocks that materialize a constant Boolean solely for an
    /// immediately following Boolean conditional.
    /// </summary>
    public PineControlFlowGraph ForwardConstantBooleanBranches()
    {
        var graph = this;

        while (graph.TryForwardConstantBooleanBranch() is { } optimized)
        {
            graph = optimized;
        }

        return graph;
    }

    private PineControlFlowGraph? TryForwardConstantBooleanBranch()
    {
        var predecessors = BuildPredecessors();

        foreach (var conditionalBlock in Blocks)
        {
            if (conditionalBlock.Operations.Length is not 0 ||
                conditionalBlock.Terminator is not PineControlFlowTerminator.ConditionalJump conditional ||
                conditional.Literal is not { } comparedLiteral ||
                !comparedLiteral.Equals(PineKernelValues.TrueValue) &&
                !comparedLiteral.Equals(PineKernelValues.FalseValue) ||
                conditionalBlock.Parameters.Length is 0)
            {
                continue;
            }

            var forwardedArguments = conditionalBlock.Parameters[..^1];

            if (!conditional.FallThroughArguments.SequenceEqual(forwardedArguments) ||
                !conditional.BranchArguments.SequenceEqual(forwardedArguments) ||
                !predecessors.TryGetValue(conditionalBlock.Id, out var conditionalPredecessors) ||
                conditionalPredecessors.Count < 2)
            {
                continue;
            }

            var providerTargets = new Dictionary<PineBlockId, PineBlockId>();
            var valid = true;

            foreach (var providerId in conditionalPredecessors.Distinct())
            {
                var provider = Blocks[providerId.Value];

                if (provider.Id == Entry ||
                    provider.Operations is not
                    [
                    {
                        Instruction.Kind: StackInstructionKind.Push_Literal,
                        Instruction.Literal: { } booleanLiteral,
                        Inputs.Length: 0,
                        Results.Length: 1
                    } pushBoolean
                    ] ||
                    !PineValueInProcess.AreEqual(booleanLiteral, PineKernelValues.TrueValue) &&
                    !PineValueInProcess.AreEqual(booleanLiteral, PineKernelValues.FalseValue) ||
                    provider.Terminator is not PineControlFlowTerminator.Jump
                    {
                        Target: var jumpTarget,
                        Arguments: var jumpArguments
                    } ||
                    jumpTarget != conditionalBlock.Id ||
                    jumpArguments.Length != provider.Parameters.Length + 1 ||
                    !jumpArguments[..^1].SequenceEqual(provider.Parameters) ||
                    jumpArguments[^1] != pushBoolean.Results[0] ||
                    !predecessors.TryGetValue(provider.Id, out var providerPredecessors) ||
                    providerPredecessors.Count is 0)
                {
                    valid = false;
                    break;
                }

                var comparisonMatches =
                    PineValueInProcess.AreEqual(booleanLiteral, comparedLiteral);

                providerTargets.Add(
                    provider.Id,
                    comparisonMatches ? conditional.Branch : conditional.FallThrough);
            }

            if (!valid ||
                providerTargets.Count != conditionalPredecessors.Distinct().Count() ||
                providerTargets.Keys.Any(
                    providerId =>
                    predecessors[providerId].Any(
                        predecessorId =>
                        providerTargets.ContainsKey(predecessorId) ||
                        predecessorId == conditionalBlock.Id)) ||
                providerTargets.Values.Any(
                    target => target == conditionalBlock.Id || providerTargets.ContainsKey(target)))
            {
                continue;
            }

            var removedBlocks =
                providerTargets.Keys
                .Append(conditionalBlock.Id)
                .ToHashSet();

            var rewrittenBlocks =
                Blocks
                .Where(block => !removedBlocks.Contains(block.Id))
                .Select(
                    block =>
                    block with
                    {
                        Terminator =
                        RedirectTargets(
                            block.Terminator,
                            target =>
                            providerTargets.TryGetValue(target, out var forwardedTarget)
                            ?
                            forwardedTarget
                            :
                            target)
                    })
                .ToImmutableArray();

            if (TryOrderForFallthrough(rewrittenBlocks) is not { } orderedBlocks ||
                TryRemapBlockIds(orderedBlocks) is not { } candidate)
            {
                continue;
            }

            try
            {
                candidate.Validate();
            }
            catch (InvalidOperationException)
            {
                continue;
            }

            return candidate;
        }

        return null;
    }

    private ImmutableArray<PineBasicBlock>? TryOrderForFallthrough(
        ImmutableArray<PineBasicBlock> blocks)
    {
        var blockById = blocks.ToDictionary(block => block.Id);
        var requiredNextByBlock = new Dictionary<PineBlockId, PineBlockId>();
        var requiredPreviousByBlock = new Dictionary<PineBlockId, PineBlockId>();

        foreach (var block in blocks)
        {
            if (RequiredFallthroughTarget(block.Terminator) is not { } requiredNext)
            {
                continue;
            }

            if (!blockById.ContainsKey(requiredNext) ||
                requiredPreviousByBlock.TryGetValue(requiredNext, out var existingPrevious) &&
                existingPrevious != block.Id)
            {
                return null;
            }

            requiredNextByBlock.Add(block.Id, requiredNext);
            requiredPreviousByBlock[requiredNext] = block.Id;
        }

        if (requiredPreviousByBlock.ContainsKey(Entry))
        {
            return null;
        }

        var heads =
            blocks
            .Where(block => !requiredPreviousByBlock.ContainsKey(block.Id))
            .OrderBy(block => block.Id == Entry ? 0 : 1)
            .ThenBy(block => block.Id.Value)
            .ToArray();

        var ordered = ImmutableArray.CreateBuilder<PineBasicBlock>(blocks.Length);
        var visited = new HashSet<PineBlockId>();

        foreach (var head in heads)
        {
            var current = head;

            while (visited.Add(current.Id))
            {
                ordered.Add(current);

                if (!requiredNextByBlock.TryGetValue(current.Id, out var next))
                {
                    break;
                }

                current = blockById[next];
            }
        }

        return ordered.Count == blocks.Length ? ordered.MoveToImmutable() : null;
    }

    private Dictionary<PineBlockId, List<PineBlockId>> BuildPredecessors()
    {
        var predecessors = new Dictionary<PineBlockId, List<PineBlockId>>();

        foreach (var block in Blocks)
        {
            foreach (var (target, _) in Successors(block.Terminator))
            {
                if (!predecessors.TryGetValue(target, out var sources))
                {
                    sources = [];
                    predecessors.Add(target, sources);
                }

                sources.Add(block.Id);
            }
        }

        return predecessors;
    }

    private PineControlFlowGraph? TryRemapBlockIds(ImmutableArray<PineBasicBlock> blocks)
    {
        var newIdByOldId =
            blocks
            .Select((block, index) => (block.Id, NewId: new PineBlockId(index)))
            .ToDictionary(item => item.Id, item => item.NewId);

        if (!newIdByOldId.TryGetValue(Entry, out var newEntry))
        {
            return null;
        }

        PineBlockId Remap(PineBlockId oldId) =>
            newIdByOldId.TryGetValue(oldId, out var newId)
            ?
            newId
            :
            throw new InvalidOperationException($"Removed block {oldId.Value} is still referenced.");

        return
            new PineControlFlowGraph(
                newEntry,
                blocks
                .Select(
                    block =>
                    block with
                    {
                        Id = Remap(block.Id),
                        Terminator = RedirectTargets(block.Terminator, Remap)
                    })
                .ToImmutableArray());
    }

    private static PineBlockId? RequiredFallthroughTarget(
        PineControlFlowTerminator terminator) =>
        terminator switch
        {
            PineControlFlowTerminator.Jump { IsFallThrough: true } jump =>
            jump.Target,

            PineControlFlowTerminator.ConditionalJump conditional =>
            conditional.FallThrough,

            PineControlFlowTerminator.Switch switchTerminator =>
            switchTerminator.FallThrough,

            PineControlFlowTerminator.Invoke invoke =>
            invoke.Continuation,

            _ =>
            null
        };

    private static PineControlFlowTerminator RedirectTargets(
        PineControlFlowTerminator terminator,
        Func<PineBlockId, PineBlockId> redirect) =>
        terminator switch
        {
            PineControlFlowTerminator.Return returnTerminator =>
            returnTerminator,

            PineControlFlowTerminator.Jump jump =>
            jump with { Target = redirect(jump.Target) },

            PineControlFlowTerminator.ConditionalJump conditional =>
            conditional with
            {
                FallThrough = redirect(conditional.FallThrough),
                Branch = redirect(conditional.Branch)
            },

            PineControlFlowTerminator.Switch switchTerminator =>
            switchTerminator with
            {
                FallThrough = redirect(switchTerminator.FallThrough),
                Cases =
                [
                .. switchTerminator.Cases.Select(
                    switchCase => switchCase with { Target = redirect(switchCase.Target) })
                ]
            },

            PineControlFlowTerminator.Invoke invoke =>
            invoke with { Continuation = redirect(invoke.Continuation) },

            PineControlFlowTerminator.TailInvoke tailInvoke =>
            tailInvoke,

            _ =>
            throw new NotImplementedException(
                "RedirectTargets does not handle terminator variant: " +
                terminator.GetType().Name)
        };

    /// <summary>
    /// Assigns physical instruction offsets after the graph has been validated.
    /// This is the only place where sequential stack instructions are created from the graph.
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
                case PineControlFlowTerminator.Return:
                    result.Add(StackInstruction.Return);
                    break;

                case PineControlFlowTerminator.Jump jump:
                    if (!jump.IsFallThrough)
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
                            offset: firstInstructionIndexByBlock[conditional.Branch] - result.Count,
                            literal: conditional.Literal));

                    if (conditional.FallThrough.Value != block.Id.Value + 1)
                    {
                        throw new InvalidOperationException(
                            $"Conditional fall-through from block {block.Id.Value} is not laid out next.");
                    }

                    break;

                case PineControlFlowTerminator.Switch switchTerminator:
                    if (switchTerminator.Kind is PineSwitchKind.SliceSkipVarEqual)
                    {
                        result.Add(
                            StackInstruction.Switch_Jump_If_Slice_Skip_Var_Equal_Const(
                                [
                                .. switchTerminator.Cases.Select(
                                    switchCase =>
                                    new SliceSwitchCase(
                                        switchCase.Literal,
                                        firstInstructionIndexByBlock[switchCase.Target] - result.Count))
                                ],
                                skipCountMultiplier: switchTerminator.SkipCountMultiplier));
                    }
                    else
                    {
                        result.Add(
                            new StackInstruction(
                                StackInstructionKind.Switch_Jump_If_Equal_Const,
                                SwitchJumpTable:
                                switchTerminator.Cases
                                .ToImmutableDictionary(
                                    switchCase => switchCase.Literal,
                                    switchCase => firstInstructionIndexByBlock[switchCase.Target] - result.Count)));
                    }

                    if (switchTerminator.FallThrough.Value != block.Id.Value + 1)
                    {
                        throw new InvalidOperationException(
                            $"Switch fall-through from block {block.Id.Value} is not laid out next.");
                    }

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
                    result.Add(StackInstruction.Return);
                    break;

                default:
                    throw new NotImplementedException(
                        "LowerToStackInstructions does not handle terminator variant: " +
                        block.Terminator.GetType().Name);
            }
        }

        return result.MoveToImmutable();
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

            case PineControlFlowTerminator.Switch switchTerminator:
                yield return (switchTerminator.FallThrough, switchTerminator.Arguments);

                foreach (var switchCase in switchTerminator.Cases)
                {
                    yield return (switchCase.Target, switchTerminator.Arguments);
                }

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

    private static int TerminatorInstructionCount(PineControlFlowTerminator terminator) =>
        terminator switch
        {
            PineControlFlowTerminator.Return => 1,
            PineControlFlowTerminator.Jump jump => jump.IsFallThrough ? 0 : 1,
            PineControlFlowTerminator.ConditionalJump => 1,
            PineControlFlowTerminator.Switch => 1,
            PineControlFlowTerminator.Invoke => 1,
            PineControlFlowTerminator.TailInvoke => 2,

            _ =>
            throw new NotImplementedException(
                "TerminatorInstructionCount does not handle terminator variant: " +
                terminator.GetType().Name)
        };
}
