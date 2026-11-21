using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>A control-flow edge, independent of sequential instruction layout.</summary>
public readonly record struct PineControlFlowEdge(PineBlockId Source, PineBlockId Target);

/// <summary>
/// A natural loop: its header dominates every member. Multiple latches and exits are retained,
/// without forcing a counted-loop shape or modifying the underlying control flow.
/// </summary>
public sealed record PineControlFlowLoop(
    PineBlockId Header,
    PineBlockId? ParentHeader,
    PineBlockId? Preheader,
    ImmutableArray<PineBlockId> Blocks,
    ImmutableArray<PineControlFlowEdge> Backedges,
    ImmutableArray<PineControlFlowEdge> Exits);

/// <summary>A cyclic region remaining after removing dominance-proven natural backedges.</summary>
public sealed record PineControlFlowIrreducibleCycle(
    ImmutableArray<PineBlockId> Blocks,
    ImmutableArray<PineBlockId> Entries);

/// <summary>
/// Immutable loop and dominance information for one CFG topology. Operation-only edits preserve
/// this information; changes to entries or edges require fresh analysis.
/// </summary>
public sealed class PineControlFlowLoopAnalysis
{
    private readonly int[] _enter;

    private readonly int[] _exit;

    /// <summary>Natural loops, in deterministic header order, with their containment forest.</summary>
    public ImmutableArray<PineControlFlowLoop> Loops { get; }

    /// <summary>Irreducible cycles, including ones nested within natural loops.</summary>
    public ImmutableArray<PineControlFlowIrreducibleCycle> IrreducibleCycles { get; }

    /// <summary>Whether reachable control flow contains any cycle, reducible or irreducible.</summary>
    public bool HasCycles => !Loops.IsEmpty || !IrreducibleCycles.IsEmpty;

    internal PineControlFlowLoopAnalysis(
        ImmutableArray<PineControlFlowLoop> loops,
        ImmutableArray<PineControlFlowIrreducibleCycle> irreducibleCycles,
        ImmutableArray<int> dominators)
    {
        Loops = loops;
        IrreducibleCycles = irreducibleCycles;
        _enter = new int[dominators.Length];
        _exit = new int[dominators.Length];
        Array.Fill(_enter, -1);
        var children = new int[dominators.Length];
        var siblings = new int[dominators.Length];
        Array.Fill(children, -1);
        var pending = new Stack<(int Block, bool Exiting)>();

        for (var block = 0; block < dominators.Length; block++)
        {
            if (dominators[block] == block)
                pending.Push((block, false));

            else if (dominators[block] >= 0)
            {
                siblings[block] = children[dominators[block]];
                children[dominators[block]] = block;
            }
        }

        var clock = 0;

        while (pending.TryPop(out var item))
        {
            if (item.Exiting)
                _exit[item.Block] = clock;

            else
            {
                _enter[item.Block] = clock++;
                pending.Push((item.Block, true));

                for (var child = children[item.Block]; child >= 0; child = siblings[child])
                    pending.Push((child, false));
            }
        }
    }

    /// <summary>Unreachable blocks do not dominate any block, including themselves.</summary>
    public bool Dominates(PineBlockId dominator, PineBlockId block)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(dominator.Value);
        ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(dominator.Value, _enter.Length);
        ArgumentOutOfRangeException.ThrowIfNegative(block.Value);
        ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(block.Value, _enter.Length);

        return
            _enter[dominator.Value] >= 0 && _enter[block.Value] >= 0 &&
            _enter[dominator.Value] <= _enter[block.Value] && _exit[block.Value] <= _exit[dominator.Value];
    }
}

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Derives natural loops, their nesting and exits, and irreducible regions from reachable
    /// basic blocks. No instruction offsets, source-loop syntax, or termination assumptions are used.
    /// </summary>
    public PineControlFlowLoopAnalysis AnalyzeLoops()
    {
        Validate();
        return AnalyzeLoops(BuildPredecessors());
    }

    private PineControlFlowLoopAnalysis AnalyzeLoops(Dictionary<PineBlockId, List<PineBlockId>> predecessors)
    {
        var successors = new int[Blocks.Length][];

        foreach (var block in Blocks)
        {
            var targets = new List<int>();

            foreach (var (target, _) in Successors(block.Terminator))
            {
                if (!targets.Contains(target.Value))
                    targets.Add(target.Value);
            }

            successors[block.Id.Value] = [.. targets];
        }

        var postorder = Postorder([Entry.Value], successors);
        var ranks = new int[Blocks.Length];
        Array.Fill(ranks, -1);
        var reversePostorder = postorder.AsEnumerable().Reverse().ToArray();

        for (var index = 0; index < reversePostorder.Length; index++)
            ranks[reversePostorder[index]] = index;

        var dominators = new int[Blocks.Length];
        Array.Fill(dominators, -1);
        dominators[Entry.Value] = Entry.Value;

        int Intersect(int left, int right)
        {
            while (left != right)
            {
                while (ranks[left] > ranks[right])
                    left = dominators[left];

                while (ranks[right] > ranks[left])
                    right = dominators[right];
            }

            return left;
        }

        bool changed;

        do
        {
            changed = false;

            foreach (var block in reversePostorder.Skip(1))
            {
                var immediate = -1;

                foreach (var predecessor in predecessors.GetValueOrDefault(new(block)) ?? [])
                {
                    if (dominators[predecessor.Value] < 0)
                        continue;

                    immediate = immediate < 0 ? predecessor.Value : Intersect(immediate, predecessor.Value);
                }

                if (dominators[block] != immediate)
                {
                    dominators[block] = immediate;
                    changed = true;
                }
            }
        }
        while (changed);

        var dominance = new PineControlFlowLoopAnalysis([], [], [.. dominators]);
        var backedges = new Dictionary<int, List<PineControlFlowEdge>>();
        var withoutBackedges = new int[Blocks.Length][];

        foreach (var block in Blocks)
        {
            var remaining = new List<int>();

            foreach (var target in successors[block.Id.Value])
            {
                if (dominance.Dominates(new(target), block.Id))
                {
                    if (!backedges.TryGetValue(target, out var latches))
                        backedges.Add(target, latches = []);

                    latches.Add(new(block.Id, new(target)));
                }
                else
                    remaining.Add(target);
            }

            withoutBackedges[block.Id.Value] = [.. remaining];
        }

        var members = new Dictionary<int, HashSet<int>>();

        foreach (var (header, latches) in backedges)
        {
            var body = new HashSet<int> { header };
            var pending = new Stack<int>();

            foreach (var edge in latches)
            {
                if (body.Add(edge.Source.Value))
                    pending.Push(edge.Source.Value);
            }

            while (pending.TryPop(out var block))
            {
                foreach (var predecessor in predecessors.GetValueOrDefault(new(block)) ?? [])
                {
                    if (ranks[predecessor.Value] >= 0 && body.Add(predecessor.Value))
                        pending.Push(predecessor.Value);
                }
            }

            members.Add(header, body);
        }

        var loops = ImmutableArray.CreateBuilder<PineControlFlowLoop>(members.Count);

        foreach (var (header, body) in members.OrderBy(item => item.Key))
        {
            var parent =
                members.Where(item => item.Key != header && item.Value.Contains(header))
                .OrderBy(item => item.Value.Count).Select(item => (PineBlockId?)new(item.Key)).FirstOrDefault();

            var entering =
                (predecessors.GetValueOrDefault(new(header)) ?? [])
                .Where(block => ranks[block.Value] >= 0 && !body.Contains(block.Value)).Distinct().ToArray();

            PineBlockId? preheader =
                entering.Length == 1 && successors[entering[0].Value].Length == 1 ? entering[0] : null;

            var exits = new List<PineControlFlowEdge>();

            foreach (var block in body.Order())
            {
                foreach (var target in successors[block])
                {
                    if (!body.Contains(target))
                        exits.Add(new(new(block), new(target)));
                }
            }

            loops.Add(
                new(
                    new(header),
                    parent,
                    preheader,
                    [.. body.Order().Select(block => new PineBlockId(block))],
                    [.. backedges[header].OrderBy(edge => edge.Source.Value)],
                    [.. exits]));
        }

        var transpose = new List<int>[Blocks.Length];

        for (var index = 0; index < transpose.Length; index++)
            transpose[index] = [];

        foreach (var block in reversePostorder)
        {
            foreach (var target in withoutBackedges[block])
                transpose[target].Add(block);
        }

        var visited = new bool[Blocks.Length];
        var irreducible = new List<PineControlFlowIrreducibleCycle>();

        foreach (var root in Postorder(reversePostorder, withoutBackedges).AsEnumerable().Reverse())
        {
            if (visited[root])
                continue;

            var body = new HashSet<int>();
            var pending = new Stack<int>();
            pending.Push(root);
            visited[root] = true;

            while (pending.TryPop(out var block))
            {
                body.Add(block);

                foreach (var predecessor in transpose[block])
                {
                    if (!visited[predecessor])
                    {
                        visited[predecessor] = true;
                        pending.Push(predecessor);
                    }
                }
            }

            if (body.Count <= 1)
                continue;

            var entries =
                body.Where(
                    block =>
                    block == Entry.Value ||
                    (predecessors.GetValueOrDefault(new(block)) ?? [])
                    .Any(predecessor => ranks[predecessor.Value] >= 0 && !body.Contains(predecessor.Value)));

            irreducible.Add(
                new(
                    [.. body.Order().Select(block => new PineBlockId(block))],
                    [.. entries.Order().Select(block => new PineBlockId(block))]));
        }

        return
            new(
                loops.MoveToImmutable(),
                [.. irreducible.OrderBy(cycle => cycle.Blocks[0].Value)],
                [.. dominators]);
    }

    private static List<int> Postorder(IEnumerable<int> roots, int[][] successors)
    {
        var visited = new bool[successors.Length];
        var result = new List<int>();
        var pending = new Stack<(int Block, int Next)>();

        foreach (var root in roots)
        {
            if (visited[root])
                continue;

            visited[root] = true;
            pending.Push((root, 0));

            while (pending.TryPop(out var current))
            {
                if (current.Next == successors[current.Block].Length)
                {
                    result.Add(current.Block);
                    continue;
                }

                pending.Push((current.Block, current.Next + 1));
                var next = successors[current.Block][current.Next];

                if (!visited[next])
                {
                    visited[next] = true;
                    pending.Push((next, 0));
                }
            }
        }

        return result;
    }
}
