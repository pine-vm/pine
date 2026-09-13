using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Semantic;

/// <summary>
/// Demand-driven scalar replacement. A projected block parameter becomes an additional scalar
/// parameter; predecessor projections are simplified against constructors. Opaque uses retain
/// the original value. Edge bindings remain simultaneous, including loop and capture bindings.
/// </summary>
public static class GraphScalarReplacement
{
    private sealed class WorkLimitException : Exception;

    /// <summary>Runs bounded immutable graph rewrites; partial progress is always semantically valid.</summary>
    public static ValidatedFunctionGraph Rewrite(
        ValidatedFunctionGraph input, int maxRounds = 32, long maxWorkUnits = 200_000, long maxExpansionUnits = 20_000)
    {
        return Run();

        ValidatedFunctionGraph Run()
        {
            var currentGraph = input.Graph;
            if (maxRounds <= 0 || maxWorkUnits <= 0 || maxExpansionUnits <= 0)
                return input;
            var initialSize = Size(currentGraph);
            long work = 0;
            try
            {
                for (var round = 0; round < maxRounds; ++round)
                {
                    var next = RemoveDead(CoalesceParameters(Fold(currentGraph)));
                    next = RemoveDead(CoalesceParameters(Fold(ExpandProjections(next))));
                    if (Size(next) - initialSize > maxExpansionUnits)
                        break;
                    if (next == currentGraph)
                        break;
                    currentGraph = next;
                }
            }
            catch (WorkLimitException)
            {
                // Publish only the last fully completed round, never a partially rewritten graph.
            }
            catch (OverflowException)
            {
                // Exhausted virtual identity space is an optimization refusal.
            }
            if (currentGraph == input.Graph)
                return input;
            return ValidatedFunctionGraph.ValidateGraph(currentGraph, input.KnownFunctionSignatures)
                .Extract(errors => throw new InvalidOperationException(string.Join(", ", errors)));

            void Spend(long count = 1)
            {
                if (count > Math.Max(0, maxWorkUnits - work))
                    throw new WorkLimitException();
                work += count;
            }

            Terminator MapTerminator(
                Terminator terminator, Func<PineVirtualValueId, PineVirtualValueId> value, Func<Edge, Edge> edge,
                Func<InvokeContinuation, InvokeContinuation>? continuation = null)
            {
                return terminator switch
                {
                    Terminator.Return ret => ret with { Values = [.. ret.Values.Select(value)] },
                    Terminator.Jump jump => new Terminator.Jump(MapEdge(jump.Edge)),
                    Terminator.Branch branch => branch with
                    {
                        TestedValue = value(branch.TestedValue),
                        IfEqual = MapEdge(branch.IfEqual),
                        IfNotEqual = MapEdge(branch.IfNotEqual),
                    },
                    Terminator.Switch selection => selection with
                    {
                        Selector = value(selection.Selector),
                        Cases = [.. selection.Cases.Select(item => item with { Edge = MapEdge(item.Edge) })],
                        Default = MapEdge(selection.Default),
                    },
                    Terminator.Invoke invoke => invoke with
                    {
                        Call = MapCall(invoke.Call),
                        Continuation = MapContinuation(invoke.Continuation),
                    },
                    Terminator.TailInvoke tail => tail with { Call = MapCall(tail.Call) },
                    _ => throw new NotImplementedException("MapTerminator does not handle terminator variant: " + terminator.GetType().Name),
                };

                Edge MapEdge(Edge original) => edge(original with { Arguments = [.. original.Arguments.Select(value)] });
                Call MapCall(Call call) => call with
                {
                    Arguments = [.. call.Arguments.Select(value)],
                    Target = call.Target switch
                    {
                        CallTarget.Dynamic target => target with { EncodedExpression = value(target.EncodedExpression) },
                        CallTarget.Known known => known,
                        _ => throw new NotImplementedException("MapCall does not handle target variant: " + call.Target.GetType().Name),
                    },
                };
                InvokeContinuation MapContinuation(InvokeContinuation original)
                {
                    var mapped = original with
                    {
                        Bindings = [.. original.Bindings.Select(binding => binding switch
                        {
                            ContinuationBinding.CallerValue caller => (ContinuationBinding)(caller with { Value = value(caller.Value) }),
                            ContinuationBinding.ReturnedResult returned => returned,
                            _ => throw new NotImplementedException("MapContinuation does not handle binding variant: " + binding.GetType().Name),
                        })],
                    };
                    return continuation is null ? mapped : continuation(mapped);
                }
            }
            FunctionGraph Fold(FunctionGraph graph)
            {
                return FoldBlocks();

                FunctionGraph FoldBlocks()
                {
                    var incoming = Incoming(graph);
                    var definitions = graph.Blocks.Values.SelectMany(block => block.Operations)
                        .ToImmutableDictionary(Result);
                    var parameterBlocks = graph.Blocks.Values.SelectMany(block =>
                        block.Parameters.Select((parameter, index) => (parameter.Id, block.Id, Index: index)))
                        .ToImmutableDictionary(pair => pair.Item1, pair => (Block: pair.Item2, pair.Index));
                    var constants = new Dictionary<PineVirtualValueId, LiteralValue?>();
                    var blocks = graph.Blocks;
                    foreach (var block in graph.Blocks.Values.OrderBy(block => block.Id.Value))
                    {
                        Spend(block.Parameters.Count + block.Operations.Count + 1L);
                        var aliases = new Dictionary<PineVirtualValueId, PineVirtualValueId>();
                        var local = new Dictionary<PineVirtualValueId, Operation>();
                        var common = new Dictionary<Operation, PineVirtualValueId>();
                        var operations = ImmutableList<Operation>.Empty;
                        foreach (var parameter in block.Parameters)
                            if (Constant(parameter.Id, []) is { } value)
                                local[parameter.Id] = new Operation.Literal(parameter, value);
                        foreach (var original in block.Operations)
                        {
                            var operation = MapOperation(original, Resolve);
                            if (operation is Operation.Builtin { Name: "head" } head && IsList(head.Argument, []))
                                operation = new Operation.Project(head.Result, head.Argument, new([0]));
                            switch (operation)
                            {
                                case Operation.Project project:
                                    var source = project.Source;
                                    var path = project.Path.Indices;
                                    while (path.Count > 0 && local.TryGetValue(source, out var sourceOperation))
                                    {
                                        if (sourceOperation is Operation.MakeList list)
                                        {
                                            var index = path[0];
                                            if (index < 0 || index >= list.Items.Count)
                                            {
                                                operation = new Operation.Literal(project.Result, new LiteralValue.List([]));
                                                break;
                                            }
                                            source = Resolve(list.Items[index]);
                                            path = path.RemoveAt(0);
                                            continue;
                                        }
                                        if (sourceOperation is Operation.Literal literal)
                                        {
                                            operation = new Operation.Literal(project.Result, Project(literal.Value, path));
                                            break;
                                        }
                                        if (sourceOperation is Operation.Project parent)
                                        {
                                            source = parent.Source;
                                            path = parent.Path.Indices.AddRange(path);
                                            continue;
                                        }
                                        break;
                                    }
                                    if (operation is Operation.Project)
                                    {
                                        if (path.Count == 0)
                                        {
                                            aliases[project.Result.Id] = source;
                                            continue;
                                        }
                                        operation = project with { Source = source, Path = new(path) };
                                    }
                                    break;
                                case Operation.Literal:
                                case Operation.MakeList:
                                case Operation.Builtin:
                                    break;
                                default:
                                    throw new NotImplementedException("Fold does not handle operation variant: " + operation.GetType().Name);
                            }
                            var key = operation switch
                            {
                                Operation.Literal literal => (Operation)(literal with { Result = new(new(-1)) }),
                                Operation.MakeList list => list with { Result = new(new(-1)) },
                                Operation.Project project => project with { Result = new(new(-1)) },
                                Operation.Builtin builtin => builtin,
                                _ => throw new NotImplementedException("Fold does not handle operation variant: " + operation.GetType().Name),
                            };
                            if (common.TryGetValue(key, out var existing))
                            {
                                aliases[Result(operation)] = existing;
                                continue;
                            }
                            common[key] = Result(operation);
                            local[Result(operation)] = operation;
                            operations = operations.Add(operation);
                        }
                        // Constants crossing cycles are rematerialized locally, never captured from another block.
                        var used = operations.SelectMany(Operands).Concat(TerminatorOperands(MapTerminator(block.Terminator, Resolve, edge => edge)))
                            .ToImmutableHashSet();
                        operations = block.Parameters.Where(parameter => used.Contains(parameter.Id) &&
                            local.TryGetValue(parameter.Id, out var value) && value is Operation.Literal)
                            .Select(parameter => local[parameter.Id]).ToImmutableList().AddRange(operations);
                        var replacedParameters = operations.OfType<Operation.Literal>().Select(operation => operation.Result.Id).ToImmutableHashSet();
                        // Parameter IDs cannot also define operations. Give local constants fresh IDs.
                        long nextId = (long)MaxValue(graph) + 1;
                        var remapped = new Dictionary<PineVirtualValueId, PineVirtualValueId>();
                        foreach (var parameter in block.Parameters.Where(parameter => replacedParameters.Contains(parameter.Id)))
                            remapped[parameter.Id] = new(checked((int)nextId++));
                        // Reserve across blocks as the immutable accumulator grows.
                        if (remapped.Count > 0)
                        {
                            var max = Math.Max(MaxValue(graph), blocks.Values.SelectMany(b => b.Operations).Select(Result).Select(v => v.Value).DefaultIfEmpty(-1).Max());
                            nextId = (long)max + 1;
                            foreach (var id in remapped.Keys.ToArray())
                                remapped[id] = new(checked((int)nextId++));
                        }
                        PineVirtualValueId Final(PineVirtualValueId id) => remapped.GetValueOrDefault(Resolve(id), Resolve(id));
                        operations = [.. operations.Select(operation => operation is Operation.Literal literal && remapped.TryGetValue(literal.Result.Id, out var id)
                            ? new Operation.Literal(new(id), literal.Value)
                            : MapOperation(operation, Final))];
                        var terminator = MapTerminator(block.Terminator, Final, edge => edge);
                        var localConstants = operations.OfType<Operation.Literal>().ToImmutableDictionary(literal => literal.Result.Id, literal => literal.Value);
                        if (terminator is Terminator.Branch branch && localConstants.TryGetValue(branch.TestedValue, out var tested))
                            terminator = new Terminator.Jump(tested == branch.Literal ? branch.IfEqual : branch.IfNotEqual);
                        if (terminator is Terminator.Switch selection && localConstants.TryGetValue(selection.Selector, out var selected))
                            terminator = new Terminator.Jump(selection.Cases.FirstOrDefault(item => item.Value == selected)?.Edge ?? selection.Default);
                        blocks = blocks.SetItem(block.Id, block with { Operations = operations, Terminator = terminator });

                        PineVirtualValueId Resolve(PineVirtualValueId id)
                        {
                            while (aliases.TryGetValue(id, out var alias))
                                id = alias;
                            return id;
                        }
                    }
                    return NewGraph(graph, blocks);

                    LiteralValue? Constant(PineVirtualValueId id, ImmutableHashSet<PineVirtualValueId> visiting)
                    {
                        if (constants.TryGetValue(id, out var cached))
                            return cached;
                        var leaves = Leaves(id, visiting);
                        var value = leaves is { Count: > 0 } && leaves.All(item => item == leaves[0]) ? leaves[0] : null;
                        constants[id] = value;
                        return value;
                    }

                    bool IsList(PineVirtualValueId id, ImmutableHashSet<PineVirtualValueId> visiting)
                    {
                        Spend(1);
                        if (visiting.Count >= 256)
                            throw new WorkLimitException();
                        if (visiting.Contains(id))
                            return true;
                        if (definitions.TryGetValue(id, out var definition))
                            return definition switch
                            {
                                Operation.MakeList => true,
                                Operation.Literal literal => literal.Value is LiteralValue.List,
                                Operation.Project or Operation.Builtin => false,
                                _ => throw new NotImplementedException("IsList does not handle operation variant: " + definition.GetType().Name),
                            };
                        if (!parameterBlocks.TryGetValue(id, out var parameter) || parameter.Block == graph.Entry)
                            return false;
                        var transfers = incoming.GetValueOrDefault(parameter.Block, []);
                        return transfers.Count > 0 && transfers.All(transfer =>
                            transfer[parameter.Index] is { } source && IsList(source, visiting.Add(id)));
                    }

                    ImmutableList<LiteralValue>? Leaves(PineVirtualValueId id, ImmutableHashSet<PineVirtualValueId> visiting)
                    {
                        Spend(1);
                        if (visiting.Count >= 256)
                            throw new WorkLimitException();
                        if (visiting.Contains(id))
                            return [];
                        if (definitions.TryGetValue(id, out var definition))
                            return definition is Operation.Literal literal ? [literal.Value] : null;
                        if (!parameterBlocks.TryGetValue(id, out var parameter) || parameter.Block == graph.Entry)
                            return null;
                        var leaves = ImmutableList<LiteralValue>.Empty;
                        foreach (var transfer in incoming.GetValueOrDefault(parameter.Block, []))
                        {
                            if (transfer[parameter.Index] is not { } source)
                                return null;
                            var branch = Leaves(source, visiting.Add(id));
                            if (branch is null)
                                return null;
                            leaves = leaves.AddRange(branch);
                            if (leaves.Count > 0 && leaves.Any(item => item != leaves[0]))
                                return null;
                        }
                        return leaves;
                    }
                }
            }

            FunctionGraph CoalesceParameters(FunctionGraph graph)
            {
                return Coalesce();

                FunctionGraph Coalesce()
                {
                    var incoming = Incoming(graph);
                    var parameters = graph.Blocks.Values.SelectMany(block => block.Parameters.Select(parameter => (block, parameter))).ToImmutableList();
                    var colors = parameters.ToDictionary(pair => pair.parameter.Id,
                        pair => pair.block.Id == graph.Entry ? pair.parameter.Id : pair.block.Parameters[0].Id);
                    var changed = true;
                    while (changed)
                    {
                        changed = false;
                        var next = new Dictionary<PineVirtualValueId, PineVirtualValueId>();
                        foreach (var block in graph.Blocks.Values)
                        {
                            Spend(1L + block.Parameters.Count * (1L + incoming.GetValueOrDefault(block.Id, []).Count));
                            var groups = new Dictionary<string, PineVirtualValueId>();
                            for (var index = 0; index < block.Parameters.Count; ++index)
                            {
                                var id = block.Parameters[index].Id;
                                var key = colors[id].Value + ":" + string.Join(",",
                                    incoming.GetValueOrDefault(block.Id, []).Select(transfer =>
                                        transfer[index] is { } source ? colors.GetValueOrDefault(source, source).Value.ToString() : "return" + index));
                                if (!groups.TryGetValue(key, out var representative))
                                    groups[key] = representative = id;
                                next[id] = representative;
                                changed |= representative != colors[id];
                            }
                        }
                        colors = next;
                    }
                    return NewGraph(graph, graph.Blocks.Values.ToImmutableDictionary(block => block.Id, block => block with
                    {
                        Operations = [.. block.Operations.Select(operation => MapOperation(operation, Alias))],
                        Terminator = MapTerminator(block.Terminator, Alias, edge => edge),
                    }));

                    PineVirtualValueId Alias(PineVirtualValueId id) => colors.GetValueOrDefault(id, id);
                }
            }

            FunctionGraph ExpandProjections(FunctionGraph graph)
            {
                return Expand();

                FunctionGraph Expand()
                {
                    var incoming = Incoming(graph);
                    var demands = graph.Blocks.Values.ToDictionary(block => block.Id, block =>
                        block.Operations.OfType<Operation.Project>()
                            .Where(project => project.Path.Indices.Count > 0 && block.Parameters.Any(parameter => parameter.Id == project.Source))
                            .Select(project => (Parameter: block.Parameters.FindIndex(parameter => parameter.Id == project.Source), Index: project.Path.Indices[0]))
                            .Distinct().OrderBy(item => item.Parameter).ThenBy(item => item.Index)
                            // Returned values are unavailable before a call. Keep such parameters opaque.
                            .Where(item => block.Id != graph.Entry && incoming.GetValueOrDefault(block.Id, [])
                                .All(transfer => transfer[item.Parameter] is not null)).ToImmutableList());
                    var owners = graph.Blocks.Values.SelectMany(block =>
                        block.Parameters.Select((parameter, index) => (parameter.Id, Block: block.Id, Index: index)))
                        .ToImmutableDictionary(item => item.Id);
                    var pending = new Queue<(PineBlockId Block, int Parameter, int Index)>(
                        demands.SelectMany(pair => pair.Value.Select(demand => (pair.Key, demand.Parameter, demand.Index))));
                    while (pending.TryDequeue(out var item))
                    {
                        Spend(1L + incoming.GetValueOrDefault(item.Block, []).Count);
                        foreach (var transfer in incoming.GetValueOrDefault(item.Block, []))
                            if (transfer[item.Parameter] is { } source && owners.TryGetValue(source, out var owner) &&
                                owner.Block != graph.Entry &&
                                incoming.GetValueOrDefault(owner.Block, []).All(edge => edge[owner.Index] is not null) &&
                                !demands[owner.Block].Contains((owner.Index, item.Index)))
                            {
                                demands[owner.Block] = demands[owner.Block].Add((owner.Index, item.Index));
                                pending.Enqueue((owner.Block, owner.Index, item.Index));
                            }
                    }
                    long nextValue = (long)MaxValue(graph) + 1;
                    var added = graph.Blocks.Values.OrderBy(block => block.Id.Value).ToImmutableDictionary(block => block.Id,
                        block => demands[block.Id].Select(_ => new ValueDefinition(new(checked((int)nextValue++)))).ToImmutableList());
                    var blocks = graph.Blocks.Values.OrderBy(block => block.Id.Value).ToImmutableDictionary(block => block.Id, RewriteBlock);
                    return NewGraph(graph, blocks);

                    BasicBlock RewriteBlock(BasicBlock block)
                    {
                        Spend(1L + block.Parameters.Count + block.Operations.Count);
                        var operations = block.Operations.Select(operation =>
                        {
                            if (operation is not Operation.Project project || project.Path.Indices.Count == 0)
                                return operation;
                            var position = demands[block.Id].FindIndex(demand =>
                                block.Parameters[demand.Parameter].Id == project.Source && demand.Index == project.Path.Indices[0]);
                            return position < 0 ? operation : project with
                            {
                                Source = added[block.Id][position].Id,
                                Path = new(project.Path.Indices.RemoveAt(0)),
                            };
                        }).ToImmutableList();
                        var terminator = MapTerminator(block.Terminator, id => id, ExpandEdge, ExpandContinuation);
                        return block with
                        {
                            Parameters = block.Parameters.AddRange(added[block.Id]),
                            Operations = operations,
                            Terminator = terminator,
                        };

                        Edge ExpandEdge(Edge edge)
                        {
                            var arguments = edge.Arguments;
                            foreach (var demand in demands[edge.Target])
                            {
                                arguments = arguments.Add(Projection(arguments[demand.Parameter], demand.Index));
                            }
                            return edge with { Arguments = arguments };
                        }

                        InvokeContinuation ExpandContinuation(InvokeContinuation continuation)
                        {
                            var bindings = continuation.Bindings;
                            foreach (var demand in demands[continuation.Target])
                            {
                                var caller = (ContinuationBinding.CallerValue)bindings[demand.Parameter];
                                bindings = bindings.Add(new ContinuationBinding.CallerValue(Projection(caller.Value, demand.Index)));
                            }
                            return continuation with { Bindings = bindings };
                        }

                        PineVirtualValueId Projection(PineVirtualValueId source, int index)
                        {
                            Spend(1);
                            var position = demands[block.Id].FindIndex(demand =>
                                block.Parameters[demand.Parameter].Id == source && demand.Index == index);
                            if (position >= 0)
                                return added[block.Id][position].Id;
                            var value = new ValueDefinition(new(checked((int)nextValue++)));
                            operations = operations.Add(new Operation.Project(value, source, new([index])));
                            return value.Id;
                        }
                    }
                }
            }

            FunctionGraph RemoveDead(FunctionGraph graph)
            {
                return Prune();

                FunctionGraph Prune()
                {
                    var reachable = new HashSet<PineBlockId>();
                    var pending = new Queue<PineBlockId>();
                    pending.Enqueue(graph.Entry);
                    while (pending.TryDequeue(out var id))
                    {
                        Spend(1);
                        if (reachable.Add(id))
                            foreach (var edge in Transfers(graph.Blocks[id].Terminator))
                                pending.Enqueue(edge.Target);
                    }
                    var blocks = graph.Blocks.Values.Where(block => reachable.Contains(block.Id)).ToImmutableList();
                    var live = blocks.SelectMany(block => DirectTerminatorOperands(block.Terminator)
                        .Concat(block.Operations.OfType<Operation.Builtin>().Select(builtin => builtin.Argument))).ToHashSet();
                    var changed = true;
                    while (changed)
                    {
                        changed = false;
                        foreach (var block in blocks)
                        {
                            Spend(1L + block.Operations.Count);
                            foreach (var operation in block.Operations)
                                if (live.Contains(Result(operation)))
                                    foreach (var operand in Operands(operation))
                                        changed |= live.Add(operand);
                            foreach (var transfer in Transfers(block.Terminator))
                            {
                                Spend(transfer.Values.Count);
                                var target = graph.Blocks[transfer.Target];
                                for (var index = 0; index < target.Parameters.Count; ++index)
                                    if (live.Contains(target.Parameters[index].Id) && transfer.Values[index] is { } source)
                                        changed |= live.Add(source);
                            }
                        }
                    }
                    var kept = blocks.ToImmutableDictionary(block => block.Id, block =>
                        block.Parameters.Select((parameter, index) => (parameter, index))
                            .Where(pair => block.Id == graph.Entry || live.Contains(pair.parameter.Id)).Select(pair => pair.index).ToImmutableList());
                    return NewGraph(graph, blocks.ToImmutableDictionary(block => block.Id, block => block with
                    {
                        Parameters = [.. kept[block.Id].Select(index => block.Parameters[index])],
                        Operations = [.. block.Operations.Where(operation => operation is Operation.Builtin || live.Contains(Result(operation)))],
                        Terminator = MapTerminator(block.Terminator, id => id,
                            edge => edge with { Arguments = [.. kept[edge.Target].Select(index => edge.Arguments[index])] },
                            continuation => continuation with
                            {
                                Bindings = [.. kept[continuation.Target].Select(index => continuation.Bindings[index])],
                            }),
                    }));
                }
            }

        }
    }

    private static LiteralValue Project(LiteralValue source, ImmutableList<int> path) =>
        path.Aggregate(source, (value, index) => value switch
        {
            LiteralValue.List list => index >= 0 && index < list.Items.Count ? list.Items[index] : new LiteralValue.List([]),
            LiteralValue.Blob => new LiteralValue.List([]),
            _ => throw new NotImplementedException("Project does not handle literal variant: " + value.GetType().Name),
        });

    private static FunctionGraph NewGraph(FunctionGraph graph, ImmutableDictionary<PineBlockId, BasicBlock> blocks) =>
        new(graph.Id, graph.Signature, graph.Entry, blocks);

    private static long Size(FunctionGraph graph) => graph.Blocks.Values.Sum(block =>
        1L + block.Parameters.Count + block.Operations.Sum(operation =>
            1L + Operands(operation).Count + (operation is Operation.Project project ? project.Path.Indices.Count : 0)) +
        TerminatorOperands(block.Terminator).Count + Transfers(block.Terminator).Count);

    private static int MaxValue(FunctionGraph graph) => graph.Blocks.Values.SelectMany(block =>
        block.Parameters.Select(parameter => parameter.Id).Concat(block.Operations.Select(Result))).Select(id => id.Value).DefaultIfEmpty(-1).Max();

    private static PineVirtualValueId Result(Operation operation) => operation switch
    {
        Operation.Literal literal => literal.Result.Id,
        Operation.MakeList list => list.Result.Id,
        Operation.Project project => project.Result.Id,
        Operation.Builtin builtin => builtin.Result.Id,
        _ => throw new NotImplementedException("Result does not handle operation variant: " + operation.GetType().Name),
    };

    private static ImmutableList<PineVirtualValueId> Operands(Operation operation) => operation switch
    {
        Operation.Literal => [],
        Operation.MakeList list => list.Items,
        Operation.Project project => [project.Source],
        Operation.Builtin builtin => [builtin.Argument],
        _ => throw new NotImplementedException("Operands does not handle operation variant: " + operation.GetType().Name),
    };

    private static Operation MapOperation(Operation operation, Func<PineVirtualValueId, PineVirtualValueId> value) => operation switch
    {
        Operation.Literal literal => literal,
        Operation.MakeList list => list with { Items = [.. list.Items.Select(value)] },
        Operation.Project project => project with { Source = value(project.Source) },
        Operation.Builtin builtin => builtin with { Argument = value(builtin.Argument) },
        _ => throw new NotImplementedException("MapOperation does not handle operation variant: " + operation.GetType().Name),
    };

    private static ImmutableDictionary<PineBlockId, ImmutableList<ImmutableList<PineVirtualValueId?>>> Incoming(FunctionGraph graph) =>
        graph.Blocks.Values.SelectMany(block => Transfers(block.Terminator)).GroupBy(transfer => transfer.Target)
            .ToImmutableDictionary(group => group.Key, group => group.Select(transfer => transfer.Values).ToImmutableList());

    private static ImmutableList<(PineBlockId Target, ImmutableList<PineVirtualValueId?> Values)> Transfers(Terminator terminator) => terminator switch
    {
        Terminator.Return or Terminator.TailInvoke => [],
        Terminator.Jump jump => [(jump.Edge.Target, jump.Edge.Arguments.Select(value => (PineVirtualValueId?)value).ToImmutableList())],
        Terminator.Branch branch => Transfers(new Terminator.Jump(branch.IfEqual)).AddRange(Transfers(new Terminator.Jump(branch.IfNotEqual))),
        Terminator.Switch selection => selection.Cases.SelectMany(item => Transfers(new Terminator.Jump(item.Edge))).ToImmutableList()
            .AddRange(Transfers(new Terminator.Jump(selection.Default))),
        Terminator.Invoke invoke => [(invoke.Continuation.Target, invoke.Continuation.Bindings.Select(binding => binding switch
        {
            ContinuationBinding.CallerValue caller => (PineVirtualValueId?)caller.Value,
            ContinuationBinding.ReturnedResult => null,
            _ => throw new NotImplementedException("Transfers does not handle binding variant: " + binding.GetType().Name),
        }).ToImmutableList())],
        _ => throw new NotImplementedException("Transfers does not handle terminator variant: " + terminator.GetType().Name),
    };

    private static ImmutableList<PineVirtualValueId> CallOperands(Call call) => call.Arguments.AddRange(call.Target switch
    {
        CallTarget.Dynamic target => [target.EncodedExpression],
        CallTarget.Known => ImmutableList<PineVirtualValueId>.Empty,
        _ => throw new NotImplementedException("CallOperands does not handle target variant: " + call.Target.GetType().Name),
    });

    private static ImmutableList<PineVirtualValueId> DirectTerminatorOperands(Terminator terminator) => terminator switch
    {
        Terminator.Return ret => ret.Values,
        Terminator.Branch branch => [branch.TestedValue],
        Terminator.Switch selection => [selection.Selector],
        Terminator.Invoke invoke => CallOperands(invoke.Call),
        Terminator.TailInvoke tail => CallOperands(tail.Call),
        Terminator.Jump => [],
        _ => throw new NotImplementedException("DirectTerminatorOperands does not handle terminator variant: " + terminator.GetType().Name),
    };

    private static ImmutableList<PineVirtualValueId> TerminatorOperands(Terminator terminator) =>
        DirectTerminatorOperands(terminator).AddRange(Transfers(terminator).SelectMany(transfer => transfer.Values.OfType<PineVirtualValueId>()));

}
