using System;
using System.Collections.Generic;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    private void Validate(OptimizationContext? context)
    {
        if (context is null)
            Validate();

        else
            context.Validate(this);
    }

    internal PineControlFlowGraph OptimizeForExecution(int parameterCount) =>
        new OptimizationContext().Optimize(this, parameterCount);

    [Flags]
    private enum OperationFlags
    {
        None = 0,
        LocalRead = 1,
        LocalStore = 2,
        LiteralStore = 4,
        Pop = 8,
        ListBuild = 16,
        Projection = 32,
        IntegerAdd = 64,
        Literal = 128,
        EqualityBranch = 256,
        ValueMap = 512,
        PotentialCycle = 1024
    }

    private sealed record BlockSummary(
        PineBasicBlock Block, LocalSet Written, LocalSet Uses, OperationFlags Flags);

    private sealed record GraphSummary(BlockSummary[] Blocks, OperationFlags Flags);

    private enum OptimizationPass
    {
        EmptyBlocks, JumpsToReturn, BooleanBranches, Unreachable,
        ScalarReplacement, LocalCopies, RedundantWrites, EquivalentReads,
        ListProjections, EqualityBranches, DeadStores, DiscardedValues,
        ForwardJumps, DescendingAdditions, LocalAdditions, LiteralValues, ValueMaps, LiteralStores, LoopInvariants
    }

    // This context owns only one compilation's snapshots. Nothing is attached to graph record equality.
    private sealed class OptimizationContext
    {
        private PineControlFlowGraph? _validated;

        private PineControlFlowGraph? _summaryGraph;

        private GraphSummary? _summary;

        private PineControlFlowGraph? _predecessorGraph;

        private Dictionary<PineBlockId, List<PineBlockId>>? _predecessors;

        private PineControlFlowGraph? _initializedGraph;

        private GraphSummary? _initializedSummary;

        private int _parameterCount;

        private LocalSet?[]? _initialized;

        private PineControlFlowGraph? _livenessGraph;

        private GraphSummary? _livenessSummary;

        private LocalSet[]? _liveOut;

        private PineControlFlowGraph? _valueGraph;

        private ValueFlowState?[]? _valueFlow;

        private PineControlFlowGraph? _loopGraph;

        private PineControlFlowLoopAnalysis? _loops;

        private readonly PineControlFlowGraph?[] _noChange =
            new PineControlFlowGraph?[(int)OptimizationPass.LoopInvariants + 1];

        private readonly int[] _noChangeParameters = new int[(int)OptimizationPass.LoopInvariants + 1];

        public int ValidationRuns { get; private set; }

        public int ValidationHits { get; private set; }

        public int InitializationRuns { get; private set; }

        public int InitializationHits { get; private set; }

        public int LivenessRuns { get; private set; }

        public int LivenessHits { get; private set; }

        public int PredecessorHits { get; private set; }

        public int ValueFlowHits { get; private set; }

        public int ExecutedPasses { get; private set; }

        public int SkippedPasses { get; private set; }

        public void Validate(PineControlFlowGraph graph)
        {
            if (ReferenceEquals(_validated, graph))
            {
                ValidationHits++;
                return;
            }

            graph.Validate();
            _validated = graph;
            ValidationRuns++;
        }

        public GraphSummary GetSummary(PineControlFlowGraph graph)
        {
            if (ReferenceEquals(_summaryGraph, graph))
                return _summary!;

            var blocks = new BlockSummary[graph.Blocks.Length];
            var flags = OperationFlags.None;

            for (var index = 0; index < blocks.Length; index++)
            {
                var block = graph.Blocks[index];

                if (_summary is { } previous && index < previous.Blocks.Length &&
                    ReferenceEquals(block, previous.Blocks[index].Block))
                {
                    blocks[index] = previous.Blocks[index];
                    flags |= blocks[index].Flags;
                    continue;
                }

                var written = new LocalSet();
                var uses = new LocalSet();
                var blockFlags = OperationFlags.None;

                foreach (var operation in block.Operations)
                {
                    var instruction = operation.Instruction;

                    foreach (var local in ReadLocals(instruction))
                    {
                        if (!written.Contains(local))
                            uses.Add(local);
                    }

                    AddWrittenLocals(instruction, ref written);

                    blockFlags |=
                        instruction.Kind switch
                        {
                            StackInstructionKind.Local_Get => OperationFlags.LocalRead,

                            StackInstructionKind.Local_Get_Skip_Head_Const =>
                            OperationFlags.LocalRead | OperationFlags.Projection,

                            StackInstructionKind.Local_Set => OperationFlags.LocalStore,
                            StackInstructionKind.Local_Set_Literal => OperationFlags.LiteralStore,
                            StackInstructionKind.Pop when instruction.PopCount is > 0 => OperationFlags.Pop,

                            StackInstructionKind.Build_List or StackInstructionKind.Build_List_With_Prefix =>
                            OperationFlags.ListBuild,

                            StackInstructionKind.Head_Generic or StackInstructionKind.Skip_Head_Const =>
                            OperationFlags.Projection,

                            StackInstructionKind.Int_Add_Const => OperationFlags.IntegerAdd,
                            StackInstructionKind.Push_Literal => OperationFlags.Literal,
                            StackInstructionKind.Select_Value_Const => OperationFlags.ValueMap,

                            _ =>
                            OperationFlags.None
                        };
                }

                switch (block.Terminator)
                {
                    case PineControlFlowTerminator.ConditionalJump:
                    case PineControlFlowTerminator.Switch:
                        blockFlags |= OperationFlags.EqualityBranch;
                        break;

                    case PineControlFlowTerminator.Return:
                    case PineControlFlowTerminator.Jump:
                    case PineControlFlowTerminator.ValueMap:
                    case PineControlFlowTerminator.Invoke:
                    case PineControlFlowTerminator.TailInvoke:
                        break;

                    default:
                        throw new NotImplementedException(
                            "GetSummary does not handle terminator variant: " + block.Terminator.GetType().Name);
                }

                foreach (var (target, _) in Successors(block.Terminator))
                {
                    if (target.Value <= block.Id.Value)
                        blockFlags |= OperationFlags.PotentialCycle;
                }

                blocks[index] = new(block, written, uses, blockFlags);
                flags |= blockFlags;
            }

            _summaryGraph = graph;
            return _summary = new(blocks, flags);
        }

        private static bool SameTopology(PineControlFlowGraph? before, PineControlFlowGraph after)
        {
            if (before is null || before.Entry != after.Entry || before.Blocks.Length != after.Blocks.Length)
                return false;

            for (var index = 0; index < before.Blocks.Length; index++)
            {
                if (before.Blocks[index].Id != after.Blocks[index].Id ||
                    before.Blocks[index].Parameters != after.Blocks[index].Parameters ||
                    !ReferenceEquals(before.Blocks[index].Terminator, after.Blocks[index].Terminator))
                {
                    return false;
                }
            }

            return true;
        }

        public Dictionary<PineBlockId, List<PineBlockId>> GetPredecessors(PineControlFlowGraph graph)
        {
            if (_predecessors is not null && SameTopology(_predecessorGraph, graph))
            {
                _predecessorGraph = graph;
                PredecessorHits++;
                return _predecessors;
            }

            _predecessorGraph = graph;
            return _predecessors = graph.BuildPredecessors();
        }

        public LocalSet?[] GetInitializedLocals(PineControlFlowGraph graph, int parameterCount)
        {
            var summary = GetSummary(graph);

            var reusable =
                _initialized is not null && _parameterCount == parameterCount &&
                SameTopology(_initializedGraph, graph);

            if (reusable)
            {
                for (var index = 0; index < summary.Blocks.Length; index++)
                    reusable &= summary.Blocks[index].Written.SetEquals(_initializedSummary!.Blocks[index].Written);
            }

            if (reusable)
                InitializationHits++;

            else
            {
                _initialized = graph.ComputeDefinitelyInitializedLocals(parameterCount, summary);
                InitializationRuns++;
            }

            _parameterCount = parameterCount;
            _initializedGraph = graph;
            _initializedSummary = summary;
            return _initialized!;
        }

        public LocalSet[] GetLiveness(PineControlFlowGraph graph)
        {
            var summary = GetSummary(graph);
            var reusable = _liveOut is not null && SameTopology(_livenessGraph, graph);

            if (reusable)
            {
                for (var index = 0; index < summary.Blocks.Length; index++)
                {
                    reusable &=
                        summary.Blocks[index].Written.SetEquals(_livenessSummary!.Blocks[index].Written) &&
                        summary.Blocks[index].Uses.SetEquals(_livenessSummary.Blocks[index].Uses);
                }
            }

            if (reusable)
                LivenessHits++;

            else
            {
                _liveOut = graph.AnalyzeLocalLiveness(this);
                LivenessRuns++;
            }

            _livenessGraph = graph;
            _livenessSummary = summary;
            return _liveOut!;
        }

        public ValueFlowState?[] GetValueFlow(PineControlFlowGraph graph)
        {
            if (ReferenceEquals(_valueGraph, graph))
            {
                ValueFlowHits++;
                return _valueFlow!;
            }

            _valueGraph = graph;
            return _valueFlow = graph.AnalyzeValueFlow();
        }

        public PineControlFlowLoopAnalysis GetLoops(PineControlFlowGraph graph)
        {
            Validate(graph);

            if (_loops is null || !SameTopology(_loopGraph, graph))
                _loops = graph.AnalyzeLoops(GetPredecessors(graph));

            _loopGraph = graph;
            return _loops;
        }

        private static bool Eligible(OptimizationPass pass, OperationFlags flags)
        {
            bool Has(OperationFlags required) => (flags & required) == required;

            return pass switch
            {
                OptimizationPass.ScalarReplacement => Has(OperationFlags.ListBuild),

                OptimizationPass.LocalCopies or OptimizationPass.RedundantWrites =>
                Has(OperationFlags.LocalStore | OperationFlags.Pop) &&
                (pass == OptimizationPass.RedundantWrites || Has(OperationFlags.LocalRead)),

                OptimizationPass.EquivalentReads => Has(OperationFlags.LocalRead),
                OptimizationPass.ListProjections => Has(OperationFlags.LocalRead | OperationFlags.Projection),
                OptimizationPass.ValueMaps => Has(OperationFlags.ValueMap),
                OptimizationPass.EqualityBranches => Has(OperationFlags.EqualityBranch),
                OptimizationPass.DeadStores => (flags & (OperationFlags.LocalStore | OperationFlags.LiteralStore)) != 0,
                OptimizationPass.DiscardedValues => Has(OperationFlags.Pop),

                OptimizationPass.DescendingAdditions or OptimizationPass.LocalAdditions =>
                Has(OperationFlags.IntegerAdd | OperationFlags.LocalRead | OperationFlags.LocalStore | OperationFlags.Pop),

                OptimizationPass.LiteralValues =>
                (flags & (OperationFlags.LocalRead | OperationFlags.Projection | OperationFlags.ValueMap)) != 0,

                OptimizationPass.LiteralStores =>
                Has(OperationFlags.Literal | OperationFlags.LocalStore | OperationFlags.Pop),

                OptimizationPass.LoopInvariants =>
                Has(OperationFlags.PotentialCycle) &&
                (flags & (OperationFlags.LocalStore | OperationFlags.LiteralStore)) != 0,

                _ =>
                true
            };
        }

        private PineControlFlowGraph Apply(PineControlFlowGraph graph, OptimizationPass pass, int parameterCount)
        {
            Validate(graph);

            if (ReferenceEquals(_noChange[(int)pass], graph) && _noChangeParameters[(int)pass] == parameterCount ||
                !Eligible(pass, GetSummary(graph).Flags))
            {
                SkippedPasses++;
                return graph;
            }

            ExecutedPasses++;

            var result =
                pass switch
                {
                    OptimizationPass.EmptyBlocks => graph.RemoveEmptyForwardingBlocks(this),
                    OptimizationPass.JumpsToReturn => graph.ForwardJumpsToReturn(this),
                    OptimizationPass.BooleanBranches => graph.ForwardConstantBooleanBranches(this),
                    OptimizationPass.Unreachable => graph.RemoveUnreachableBlocks(this),
                    OptimizationPass.ScalarReplacement => graph.ReplaceNonEscapingLists(parameterCount, this),
                    OptimizationPass.LocalCopies => graph.EliminateLocalCopies(this),
                    OptimizationPass.RedundantWrites => graph.EliminateRedundantLocalWrites(parameterCount, this),
                    OptimizationPass.EquivalentReads => graph.ForwardEquivalentLocalReads(this),
                    OptimizationPass.ListProjections => graph.FuseLocalListProjections(this),
                    OptimizationPass.EqualityBranches => graph.ForwardProvenEqualityBranches(this),
                    OptimizationPass.DeadStores => graph.EliminateDeadLocalStores(this),
                    OptimizationPass.DiscardedValues => graph.EliminateDiscardedStackValues(parameterCount, this),
                    OptimizationPass.ForwardJumps => graph.RemoveRedundantForwardJumps(this),

                    OptimizationPass.DescendingAdditions =>
                    graph.FuseDescendingLocalIntegerAdditions(parameterCount, this),

                    OptimizationPass.LocalAdditions => graph.FuseLocalIntegerAdditions(this),
                    OptimizationPass.LiteralValues => graph.FoldKnownLiteralValues(parameterCount, this),
                    OptimizationPass.ValueMaps => graph.ComposeValueMaps(this),
                    OptimizationPass.LiteralStores => graph.FuseLiteralLocalStores(this),
                    OptimizationPass.LoopInvariants => graph.HoistLoopInvariantStores(parameterCount, this),

                    _ =>
                    throw new NotImplementedException("Apply does not handle optimization pass: " + pass)
                };

            Validate(result);

            if (ReferenceEquals(result, graph))
            {
                _noChange[(int)pass] = graph;
                _noChangeParameters[(int)pass] = parameterCount;
            }

            return result;
        }

        public PineControlFlowGraph Optimize(PineControlFlowGraph graph, int parameterCount)
        {
            ArgumentOutOfRangeException.ThrowIfNegative(parameterCount);

            ReadOnlySpan<OptimizationPass> order =
                [
                    OptimizationPass.EmptyBlocks, OptimizationPass.JumpsToReturn,
                    OptimizationPass.BooleanBranches, OptimizationPass.Unreachable,
                    OptimizationPass.ScalarReplacement, OptimizationPass.LocalCopies,
                    OptimizationPass.ValueMaps,
                    OptimizationPass.RedundantWrites, OptimizationPass.EquivalentReads,
                    OptimizationPass.ListProjections, OptimizationPass.EqualityBranches,
                    OptimizationPass.DeadStores, OptimizationPass.RedundantWrites,
                    OptimizationPass.ValueMaps,
                    OptimizationPass.DiscardedValues, OptimizationPass.DeadStores,
                    OptimizationPass.RedundantWrites, OptimizationPass.DiscardedValues,
                    OptimizationPass.ForwardJumps, OptimizationPass.Unreachable,
                    OptimizationPass.DescendingAdditions, OptimizationPass.LocalAdditions,
                    OptimizationPass.LiteralValues, OptimizationPass.LiteralStores,
                    OptimizationPass.DeadStores, OptimizationPass.RedundantWrites,
                    OptimizationPass.DiscardedValues, OptimizationPass.LoopInvariants
                ];

            foreach (var pass in order)
                graph = Apply(graph, pass, parameterCount);

            // Removing an outer envelope can expose a nested value as non-escaping.
            while (true)
            {
                var scalarized = Apply(graph, OptimizationPass.ScalarReplacement, parameterCount);
                var invariant = Apply(scalarized, OptimizationPass.LoopInvariants, parameterCount);

                if (ReferenceEquals(invariant, graph))
                    break;

                graph = invariant;

                foreach (var pass in order)
                    graph = Apply(graph, pass, parameterCount);
            }

            return graph;
        }
    }
}
