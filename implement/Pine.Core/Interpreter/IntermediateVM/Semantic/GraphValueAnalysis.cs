using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Interpreter.IntermediateVM.Semantic;

/// <summary>Owned abstract values; unknown slots do not erase a constructed list's shape.</summary>
public abstract record GraphValueFact
{
    private GraphValueFact() { }

    /// <summary>Any Pine value, not an unreachable value.</summary>
    public sealed record Unknown : GraphValueFact;

    /// <summary>An exact, deeply owned Pine literal.</summary>
    public sealed record Exact(LiteralValue Value) : GraphValueFact;

    /// <summary>A known list shape whose items may be unknown.</summary>
    public sealed record List(ImmutableList<GraphValueFact> Items) : GraphValueFact
    {
        /// <inheritdoc/>
        public bool Equals(List? other) => other is not null && Items.SequenceEqual(other.Items);

        /// <inheritdoc/>
        public override int GetHashCode() => ModelEquality.SequenceHash(Items);
    }

    /// <summary>The shared immutable unknown fact.</summary>
    public static GraphValueFact Any { get; } = new Unknown();
}

/// <summary>Deterministic bounds on traversal, comparison and constructed shape.</summary>
public sealed record GraphValueAnalysisOptions(long MaxWorkUnits = 100_000, int MaxDepth = 32, int MaxListItems = 256);

/// <summary>No partial or optimistic facts are published when the work budget is exhausted.</summary>
public sealed record GraphValueAnalysisResult(
    ImmutableDictionary<PineVirtualValueId, GraphValueFact> Facts, long WorkUnits, bool BudgetExhausted)
{
    /// <summary>Absent definitions, including unreachable components, are conservatively unknown.</summary>
    public GraphValueFact ValueOf(PineVirtualValueId value) => Facts.GetValueOrDefault(value, GraphValueFact.Any);
}

/// <summary>
/// Intraprocedural, descending refinement from unknown, never from an unvisited backedge.
/// Every incoming edge participates; entry always also receives an unknown external environment.
/// This deliberately cannot discover invariants requiring an optimistic identity-backedge seed.
/// Facts describe successful evaluation only and must never be used to remove operand evaluation.
/// </summary>
public static class GraphValueAnalysis
{
    private sealed class WorkLimitException : Exception;

    /// <summary>Analyzes a validated semantic graph without inspecting runtime environments or call bodies.</summary>
    public static GraphValueAnalysisResult Analyze(ValidatedFunctionGraph input, GraphValueAnalysisOptions options)
    {
        return Run();

        GraphValueAnalysisResult Run()
        {
            long work = 0;
            try
            {
                if (options.MaxDepth <= 0 || options.MaxListItems <= 0 || options.MaxWorkUnits <= 0)
                    throw new WorkLimitException();
                Spend(input.Graph.Blocks.Count);
                var blocks = input.Graph.Blocks.Values.OrderBy(block => block.Id.Value).ToImmutableList();
                var facts = EmptyFacts();
                while (true)
                {
                    var reachable = ImmutableHashSet<PineBlockId>.Empty;
                    var pending = ImmutableQueue<PineBlockId>.Empty.Enqueue(input.Graph.Entry);
                    while (!pending.IsEmpty)
                    {
                        Spend();
                        pending = pending.Dequeue(out var id);
                        if (reachable.Contains(id))
                            continue;
                        reachable = reachable.Add(id);
                        foreach (var edge in Transfers(input.Graph.Blocks[id], facts))
                            pending = pending.Enqueue(edge.Target);
                    }

                    var next = EmptyFacts();
                    foreach (var block in blocks)
                    {
                        Spend();
                        if (!reachable.Contains(block.Id))
                            continue;
                        foreach (var parameter in block.Parameters)
                        {
                            Spend();
                            next = next.SetItem(parameter.Id, Get(facts, parameter.Id));
                        }
                        foreach (var operation in block.Operations)
                        {
                            Spend();
                            var result = operation switch
                            {
                                Operation.Literal literal => (literal.Result.Id, (GraphValueFact)new GraphValueFact.Exact(literal.Value)),
                                Operation.MakeList list => (list.Result.Id, MakeList([.. list.Items.Select(id => Get(next, id))])),
                                Operation.Project project => (project.Result.Id, Project(Get(next, project.Source), project.Path)),
                                Operation.Builtin builtin => (builtin.Result.Id, Builtin(builtin.Name, Get(next, builtin.Argument))),
                                _ => throw new NotImplementedException("Analyze does not handle operation variant: " + operation.GetType().Name),
                            };
                            next = next.SetItem(result.Id, result.Item2);
                        }
                    }

                    var incoming = EmptyFacts();
                    foreach (var parameter in input.Graph.Blocks[input.Graph.Entry].Parameters)
                        incoming = incoming.SetItem(parameter.Id, GraphValueFact.Any);
                    foreach (var block in blocks)
                    {
                        Spend();
                        if (!reachable.Contains(block.Id))
                            continue;
                        foreach (var transfer in Transfers(block, facts))
                        {
                            var parameters = input.Graph.Blocks[transfer.Target].Parameters;
                            for (var index = 0; index < parameters.Count; ++index)
                            {
                                Spend();
                                var value = transfer.Values[index] is { } id ? Get(next, id) : GraphValueFact.Any;
                                var target = parameters[index].Id;
                                incoming = incoming.SetItem(target,
                                    incoming.TryGetValue(target, out var previous) ? Meet(previous, value, 0) : value);
                            }
                        }
                    }
                    foreach (var block in blocks)
                    {
                        Spend();
                        if (!reachable.Contains(block.Id))
                            continue;
                        foreach (var parameter in block.Parameters)
                        {
                            Spend();
                            next = next.SetItem(parameter.Id, Get(incoming, parameter.Id));
                        }
                    }
                    var same = next.Count == facts.Count;
                    foreach (var pair in next)
                    {
                        Spend();
                        same &= facts.TryGetValue(pair.Key, out var old) && Same(pair.Value, old, 0);
                    }
                    if (same)
                        return new(next, work, false);
                    facts = next;
                }
            }
            catch (WorkLimitException)
            {
                return new(ImmutableDictionary<PineVirtualValueId, GraphValueFact>.Empty, work, true);
            }

            void Spend(long count = 1)
            {
                if (count > Math.Max(0, options.MaxWorkUnits - work))
                {
                    work = Math.Max(0, options.MaxWorkUnits);
                    throw new WorkLimitException();
                }
                work += count;
            }

            ImmutableDictionary<PineVirtualValueId, GraphValueFact> EmptyFacts() =>
                ImmutableDictionary<PineVirtualValueId, GraphValueFact>.Empty.WithComparers(
                    EqualityComparer<PineVirtualValueId>.Default, ReferenceEqualityComparer.Instance);

            GraphValueFact Get(ImmutableDictionary<PineVirtualValueId, GraphValueFact> values, PineVirtualValueId id)
            {
                Spend();
                return values.GetValueOrDefault(id, GraphValueFact.Any);
            }

            ImmutableList<(PineBlockId Target, ImmutableList<PineVirtualValueId?> Values)> Transfers(
                BasicBlock block, ImmutableDictionary<PineVirtualValueId, GraphValueFact> values)
            {
                Spend();
                return block.Terminator switch
                {
                    Terminator.Return or Terminator.TailInvoke => [],
                    Terminator.Jump jump => [Edge(jump.Edge)],
                    Terminator.Branch branch => SelectBranch(branch),
                    Terminator.Switch selection => SelectSwitch(selection),
                    Terminator.Invoke invoke => [(invoke.Continuation.Target, invoke.Continuation.Bindings.Select(Binding).ToImmutableList())],
                    _ => throw new NotImplementedException("Transfers does not handle terminator variant: " + block.Terminator.GetType().Name),
                };

                (PineBlockId, ImmutableList<PineVirtualValueId?>) Edge(Edge edge)
                {
                    Spend(edge.Arguments.Count);
                    return (edge.Target, edge.Arguments.Select(value => (PineVirtualValueId?)value).ToImmutableList());
                }

                PineVirtualValueId? Binding(ContinuationBinding binding)
                {
                    Spend();
                    return binding switch
                    {
                        ContinuationBinding.CallerValue caller => caller.Value,
                        ContinuationBinding.ReturnedResult => null,
                        _ => throw new NotImplementedException("Binding does not handle continuation variant: " + binding.GetType().Name),
                    };
                }

                ImmutableList<(PineBlockId, ImmutableList<PineVirtualValueId?>)> SelectBranch(Terminator.Branch branch) =>
                    Equal(Get(values, branch.TestedValue), new GraphValueFact.Exact(branch.Literal), 0) switch
                    {
                        true => [Edge(branch.IfEqual)],
                        false => [Edge(branch.IfNotEqual)],
                        null => [Edge(branch.IfEqual), Edge(branch.IfNotEqual)],
                    };

                ImmutableList<(PineBlockId, ImmutableList<PineVirtualValueId?>)> SelectSwitch(Terminator.Switch selection)
                {
                    var edges = ImmutableList<(PineBlockId, ImmutableList<PineVirtualValueId?>)>.Empty;
                    foreach (var item in selection.Cases)
                    {
                        Spend();
                        var equal = Equal(Get(values, selection.Selector), new GraphValueFact.Exact(item.Value), 0);
                        if (equal is not false)
                            edges = edges.Add(Edge(item.Edge));
                        if (equal is true)
                            return edges;
                    }
                    return edges.Add(Edge(selection.Default));
                }
            }

            GraphValueFact MakeList(ImmutableList<GraphValueFact> items, int depth = 0)
            {
                Spend(items.Count + 1L);
                if (items.Count > options.MaxListItems || depth >= options.MaxDepth)
                    return GraphValueFact.Any;
                var bounded = items.Select(item => Bound(item, depth + 1)).ToImmutableList();
                return bounded.All(item => item is GraphValueFact.Exact)
                    ? new GraphValueFact.Exact(new LiteralValue.List(bounded.Cast<GraphValueFact.Exact>().Select(item => item.Value).ToImmutableList()))
                    : new GraphValueFact.List(bounded);
            }

            GraphValueFact Bound(GraphValueFact value, int depth)
            {
                Spend();
                return value switch
                {
                    GraphValueFact.Unknown or GraphValueFact.Exact => value,
                    GraphValueFact.List list => depth >= options.MaxDepth ? GraphValueFact.Any : MakeList(list.Items, depth),
                    _ => throw new NotImplementedException("Bound does not handle fact variant: " + value.GetType().Name),
                };
            }

            ImmutableList<GraphValueFact>? Items(GraphValueFact value)
            {
                Spend();
                return value switch
                {
                    GraphValueFact.Unknown => null,
                    GraphValueFact.List list => list.Items,
                    GraphValueFact.Exact exact => exact.Value switch
                    {
                        LiteralValue.Blob => null,
                        LiteralValue.List list => LiteralItems(list),
                        _ => throw new NotImplementedException("Items does not handle literal variant: " + exact.Value.GetType().Name),
                    },
                    _ => throw new NotImplementedException("Items does not handle fact variant: " + value.GetType().Name),
                };

                ImmutableList<GraphValueFact>? LiteralItems(LiteralValue.List list)
                {
                    Spend(list.Items.Count);
                    return list.Items.Count > options.MaxListItems ? null :
                        [.. list.Items.Select(item => (GraphValueFact)new GraphValueFact.Exact(item))];
                }
            }

            GraphValueFact Project(GraphValueFact source, EnvironmentPath path)
            {
                var value = source;
                foreach (var index in path.Indices)
                {
                    Spend();
                    var items = Items(value);
                    value = items is not null
                        ? index >= 0 && index < items.Count ? items[index] : Empty()
                        : value is GraphValueFact.Exact { Value: LiteralValue.Blob } ? Empty() : GraphValueFact.Any;
                }
                return value;
            }

            GraphValueFact Empty() => new GraphValueFact.Exact(new LiteralValue.List([]));

            bool Same(GraphValueFact left, GraphValueFact right, int depth)
            {
                Spend();
                if (ReferenceEquals(left, right))
                    return true;
                return left switch
                {
                    GraphValueFact.Unknown => right is GraphValueFact.Unknown,
                    GraphValueFact.Exact => Equal(left, right, depth) is true,
                    GraphValueFact.List a => right is GraphValueFact.List b &&
                        a.Items.Count == b.Items.Count && a.Items.Zip(b.Items).All(pair => Same(pair.First, pair.Second, depth + 1)),
                    _ => throw new NotImplementedException("Same does not handle fact variant: " + left.GetType().Name),
                };
            }

            bool? Equal(GraphValueFact left, GraphValueFact right, int depth)
            {
                Spend();
                if (depth > options.MaxDepth)
                    return null;
                if (left is GraphValueFact.Unknown || right is GraphValueFact.Unknown)
                    return null;
                if (left is GraphValueFact.Exact a && right is GraphValueFact.Exact b)
                    return LiteralEqual(a.Value, b.Value, depth);
                var leftItems = Items(left);
                var rightItems = Items(right);
                if (leftItems is null || rightItems is null)
                    return left is GraphValueFact.Exact { Value: LiteralValue.Blob } ||
                        right is GraphValueFact.Exact { Value: LiteralValue.Blob } ? false : null;
                if (leftItems.Count != rightItems.Count)
                    return false;
                var answer = (bool?)true;
                for (var index = 0; index < leftItems.Count; ++index)
                {
                    var equal = Equal(leftItems[index], rightItems[index], depth + 1);
                    if (equal is false)
                        return false;
                    if (equal is null)
                        answer = null;
                }
                return answer;
            }

            bool? LiteralEqual(LiteralValue left, LiteralValue right, int depth)
            {
                Spend();
                if (ReferenceEquals(left, right))
                    return true;
                if (depth > options.MaxDepth)
                    return null;
                switch (left)
                {
                    case LiteralValue.Blob a:
                        if (right is not LiteralValue.Blob b)
                            return false;
                        Spend(Math.Min(a.Bytes.Count, b.Bytes.Count));
                        return a.Bytes.SequenceEqual(b.Bytes);
                    case LiteralValue.List a:
                        if (right is not LiteralValue.List listRight)
                            return false;
                        if (a.Items.Count != listRight.Items.Count)
                            return false;
                        var answer = (bool?)true;
                        for (var index = 0; index < a.Items.Count; ++index)
                        {
                            var equal = LiteralEqual(a.Items[index], listRight.Items[index], depth + 1);
                            if (equal is false)
                                return false;
                            if (equal is null)
                                answer = null;
                        }
                        return answer;
                    default:
                        throw new NotImplementedException("LiteralEqual does not handle literal variant: " + left.GetType().Name);
                }
            }

            GraphValueFact Meet(GraphValueFact left, GraphValueFact right, int depth)
            {
                Spend();
                if (depth >= options.MaxDepth)
                    return GraphValueFact.Any;
                if (Same(left, right, depth))
                    return left;
                var a = Items(left);
                var b = Items(right);
                return a is not null && b is not null && a.Count == b.Count
                    ? MakeList([.. a.Zip(b).Select(pair => Meet(pair.First, pair.Second, depth + 1))], depth)
                    : GraphValueFact.Any;
            }

            BigInteger? Integer(GraphValueFact value)
            {
                if (value is not GraphValueFact.Exact { Value: LiteralValue.Blob blob } ||
                    blob.Bytes.Count < 2 || blob.Bytes.Count > 32 || blob.Bytes[0] is not (2 or 4))
                    return null;
                Spend(blob.Bytes.Count);
                var magnitude = new BigInteger(blob.Bytes.Skip(1).ToArray(), isUnsigned: true, isBigEndian: true);
                return blob.Bytes[0] == 2 ? -magnitude : magnitude;
            }

            GraphValueFact Builtin(string name, GraphValueFact argument)
            {
                Spend();
                switch (name)
                {
                    case "head":
                        if (argument is GraphValueFact.Exact { Value: LiteralValue.Blob blob })
                            return new GraphValueFact.Exact(new LiteralValue.Blob([.. blob.Bytes.Take(1)]));
                        var headItems = Items(argument);
                        return headItems is null ? GraphValueFact.Any : headItems.Count == 0 ? Empty() : headItems[0];
                    case "skip":
                        var skipItems = Items(argument);
                        if (skipItems is not { Count: 2 } || Integer(skipItems[0]) is not { } count)
                            return GraphValueFact.Any;
                        if (count <= 0)
                            return skipItems[1];
                        // The current primitive casts positive counts to int and can fail.
                        if (count > int.MaxValue)
                            return GraphValueFact.Any;
                        if (skipItems[1] is GraphValueFact.Exact { Value: LiteralValue.Blob skippedBlob })
                        {
                            Spend(skippedBlob.Bytes.Count);
                            return new GraphValueFact.Exact(new LiteralValue.Blob([.. skippedBlob.Bytes.Skip((int)count)]));
                        }
                        var sourceItems = Items(skipItems[1]);
                        return sourceItems is null ? GraphValueFact.Any : MakeList([.. sourceItems.Skip((int)count)]);
                    case "concat":
                        return Concat(argument);
                    case "int_add":
                        var addItems = Items(argument);
                        if (addItems is null)
                            return GraphValueFact.Any;
                        var sum = BigInteger.Zero;
                        foreach (var item in addItems)
                        {
                            Spend(32);
                            if (Integer(item) is not { } integer)
                                return GraphValueFact.Any;
                            sum += integer;
                        }
                        var bytes = BigInteger.Abs(sum).ToByteArray(isUnsigned: true, isBigEndian: true);
                        Spend(bytes.Length);
                        return new GraphValueFact.Exact(new LiteralValue.Blob(
                            [(byte)(sum.Sign < 0 ? 2 : 4), .. bytes]));
                    case "equal":
                        if (argument is GraphValueFact.Exact { Value: LiteralValue.Blob equalBlob })
                        {
                            Spend(equalBlob.Bytes.Count);
                            return Boolean(equalBlob.Bytes.Count < 2 || equalBlob.Bytes.All(value => value == equalBlob.Bytes[0]));
                        }
                        var equalItems = Items(argument);
                        if (equalItems is null)
                            return GraphValueFact.Any;
                        var allEqual = (bool?)true;
                        foreach (var item in equalItems.Skip(1))
                        {
                            var equal = Equal(equalItems[0], item, 0);
                            if (equal is false)
                                return Boolean(false);
                            if (equal is null)
                                allEqual = null;
                        }
                        return allEqual is true ? Boolean(true) : GraphValueFact.Any;
                    default:
                        return GraphValueFact.Any;
                }
            }

            GraphValueFact Boolean(bool value) => new GraphValueFact.Exact(new LiteralValue.Blob([(byte)(value ? 4 : 2)]));

            GraphValueFact Concat(GraphValueFact argument)
            {
                var inputs = Items(argument);
                if (inputs is null)
                    return GraphValueFact.Any;
                var lists = ImmutableList<GraphValueFact>.Empty;
                var bytes = ImmutableList<byte>.Empty;
                var kind = 0;
                foreach (var item in inputs)
                {
                    Spend();
                    var items = Items(item);
                    if (items is not null)
                    {
                        if (items.Count == 0)
                            continue;
                        if (kind == 2)
                            return Empty();
                        kind = 1;
                        Spend(items.Count);
                        if (items.Count > options.MaxListItems - lists.Count)
                            return GraphValueFact.Any;
                        lists = lists.AddRange(items);
                    }
                    else if (item is GraphValueFact.Exact { Value: LiteralValue.Blob blob })
                    {
                        if (kind == 1)
                            return Empty();
                        kind = 2;
                        Spend(blob.Bytes.Count);
                        bytes = bytes.AddRange(blob.Bytes);
                    }
                    else
                        return GraphValueFact.Any;
                }
                return kind == 2 ? new GraphValueFact.Exact(new LiteralValue.Blob(bytes)) : MakeList(lists);
            }
        }
    }
}
