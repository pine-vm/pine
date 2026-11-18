using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    private const int MaxComposedValueMapCases = 128;

    private sealed record FiniteMapState(
        ImmutableArray<ImmutableHashSet<PineValue>?> Parameters,
        Dictionary<int, ImmutableHashSet<PineValue>> Locals);

    private sealed record ReloadableMap(
        StackInstruction Key,
        ImmutableDictionary<PineValue, PineValue> Map,
        PineValue Fallback,
        ImmutableHashSet<PineValue>? Domain);

    /// <summary>
    /// Composes finite selections with direct projections, selections, and literal comparisons.
    /// Missing-key behavior participates in every composition; identities require a proven finite domain.
    /// </summary>
    public PineControlFlowGraph ComposeValueMaps() => ComposeValueMaps(null);

    private PineControlFlowGraph ComposeValueMaps(OptimizationContext? context)
    {
        Validate(context);
        var states = AnalyzeFiniteMapValues();

        var nextValue =
            Blocks.SelectMany(
                block =>
                block.Parameters
                .Concat(block.Operations.SelectMany(operation => operation.Results)))
            .Select(value => value.Value).DefaultIfEmpty(-1).Max() + 1;

        foreach (var block in Blocks)
        {
            foreach (var (_, arguments) in Successors(block.Terminator))
            {
                foreach (var value in arguments)
                    nextValue = Math.Max(nextValue, value.Value + 1);
            }
        }

        var blocks = RewriteBlocks(Rewrite);
        var result = blocks == Blocks ? this : this with { Blocks = blocks };
        result.Validate(context);
        return result;

        PineBasicBlock Rewrite(PineBasicBlock block)
        {
            if (states[block.Id.Value] is not { } state)
                return block;

            var (domains, _) = TraceFiniteMapValues(block, state);
            var reloads = new Dictionary<PineVirtualValueId, StackInstruction>();
            var maps = new Dictionary<PineVirtualValueId, ReloadableMap>();
            var locals = new Dictionary<int, ReloadableMap>();
            var stack = block.Parameters.ToList();
            var replacements = new Dictionary<PineVirtualValueId, PineVirtualValueId>();
            var operations = new OperationBuilder(block.Operations);
            var terminator = block.Terminator;

            PineVirtualValueId Resolve(PineVirtualValueId value)
            {
                while (replacements.TryGetValue(value, out var replacement))
                    value = replacement;

                return value;
            }

            for (var index = 0; index < block.Operations.Length; index++)
            {
                var operation = block.Operations[index];
                var instruction = operation.Instruction;

                if (IsLiteralValueMap(instruction) &&
                    index + 1 < block.Operations.Length &&
                    block.Operations[index + 1] is { } discard &&
                    discard.Instruction.Kind is StackInstructionKind.Pop &&
                    discard.Inputs.Length > 0 && operation.Results is [var discarded] &&
                    discard.Inputs[^1] == discarded)
                {
                    operations.Add(
                        discard with
                        {
                            Inputs =
                            [
                            .. discard.Inputs.Select(
                                value =>
                                Resolve(value == discarded ? operation.Inputs[0] : value))
                            ]
                        });

                    Advance(index, index + 1);
                    index++;
                    continue;
                }

                if (instruction.Kind is StackInstructionKind.Local_Set)
                {
                    var stored =
                        instruction.LocalIndices
                        .Select((local, depth) => (local, Map: maps.GetValueOrDefault(stack[^(depth + 1)])))
                        .ToArray();

                    Invalidate(instruction.LocalIndices);

                    foreach (var (local, storedMap) in stored)
                    {
                        if (storedMap is not null && !DependsOnAny(storedMap.Key, instruction.LocalIndices))
                            locals[local] = storedMap;

                        else
                            locals.Remove(local);
                    }
                }
                else
                {
                    Invalidate(WrittenLocals(instruction));
                }

                var lastIndex = index;
                var fromLocal = false;
                ReloadableMap? source = null;
                ImmutableDictionary<PineValue, PineValue>? map = null;
                PineValue? fallback = null;
                var input = default(PineVirtualValueId);
                var output = operation.Results.Length is 1 ? operation.Results[0] : default;
                ImmutableHashSet<PineValue>? domain = null;

                if (IsLiteralValueMap(instruction) && operation.Inputs.Length is 1 && operation.Results.Length is 1)
                {
                    map = instruction.ValueMap!;
                    fallback = instruction.Literal!.Evaluate();
                    input = operation.Inputs[0];
                    domain = domains.GetValueOrDefault(input);

                    if (reloads.TryGetValue(input, out var key))
                        source = new(key, map, fallback, domain);
                }
                else if (instruction.Kind is StackInstructionKind.Local_Get or StackInstructionKind.Local_Get_Skip_Head_Const &&
                    instruction.LocalIndices is [var local] && operation.Results.Length is 1 &&
                    locals.TryGetValue(local, out source))
                {
                    map = source.Map;
                    fallback = source.Fallback;
                    input = output;
                    domain = source.Domain;
                    fromLocal = true;

                    if (instruction.Kind is StackInstructionKind.Local_Get_Skip_Head_Const)
                    {
                        var projection = StackInstruction.Skip_Head_Const(instruction.SkipCount ?? 0);

                        map =
                            map.ToImmutableDictionary(
                                item => item.Key,
                                item => TransformMapValue(projection, item.Value));

                        fallback = TransformMapValue(projection, fallback);
                    }
                }

                if (map is { Count: <= MaxComposedValueMapCases } && fallback is not null)
                {
                    while (lastIndex + 1 < block.Operations.Length &&
                        block.Operations[lastIndex + 1] is { Inputs.Length: 1, Results.Length: 1 } next &&
                        next.Inputs[0] == output && CanTransformMapValue(next.Instruction))
                    {
                        map =
                            map.ToImmutableDictionary(
                                item => item.Key,
                                item => TransformMapValue(next.Instruction, item.Value));

                        fallback = TransformMapValue(next.Instruction, fallback);
                        output = next.Results[0];
                        lastIndex++;
                    }

                    var conditional =
                        lastIndex == block.Operations.Length - 1
                        ?
                        terminator as PineControlFlowTerminator.ConditionalJump
                        :
                        null;

                    var changed =
                        lastIndex > index ||
                        fromLocal && instruction.Kind is StackInstructionKind.Local_Get_Skip_Head_Const ||
                        conditional is not null;

                    if (!fromLocal || changed)
                    {
                        if (fromLocal)
                        {
                            input = new PineVirtualValueId(nextValue++);
                            operations.Add(operation with { Instruction = source!.Key, Inputs = [], Results = [input] });
                        }

                        if (conditional is not null)
                        {
                            map =
                                map.ToImmutableDictionary(
                                    item => item.Key,
                                    item => Boolean(item.Value.Equals(conditional.Literal)));

                            fallback = Boolean(fallback.Equals(conditional.Literal));
                            terminator = conditional with { Literal = Boolean(true) };
                        }

                        map = map.RemoveRange(map.Where(item => item.Value.Equals(fallback)).Select(item => item.Key));
                        var resolvedInput = Resolve(input);

                        if (conditional is not null && fallback.Equals(Boolean(false)) &&
                            map is { Count: 1 } && map.Values.Single().Equals(Boolean(true)))
                        {
                            replacements[output] = resolvedInput;
                            terminator = conditional with { Literal = map.Keys.Single() };
                        }
                        else if (domain is not null &&
                            domain.All(key => map.GetValueOrDefault(key, fallback).Equals(key)))
                        {
                            replacements[output] = resolvedInput;
                        }
                        else
                        {
                            if (map.Count is 0)
                            {
                                operations.Add(
                                    new PineControlFlowOperation(StackInstruction.Pop, [resolvedInput], []));

                                operations.Add(
                                    operation with
                                    {
                                        Instruction = StackInstruction.Push_Literal(fallback),
                                        Inputs = [],
                                        Results = [output]
                                    });
                            }
                            else
                            {
                                var composed =
                                    map is { Count: 1 } &&
                                    (fallback.Equals(Boolean(false)) && map.Values.Single().Equals(Boolean(true)) ||
                                    fallback.Equals(Boolean(true)) && map.Values.Single().Equals(Boolean(false)))
                                    ?
                                    fallback.Equals(Boolean(false))
                                    ?
                                    StackInstruction.Equal_Binary_Const(map.Keys.Single())
                                    :
                                    StackInstruction.Not_Equal_Binary_Const(map.Keys.Single())
                                    :
                                    StackInstruction.Select_Value_Const(map, fallback);

                                var composedOperation =
                                    operation with
                                    {
                                        Instruction = composed,
                                        Inputs = [resolvedInput],
                                        Results = [output]
                                    };

                                operations.Add(
                                    composed.Equals(operation.Instruction) &&
                                    composedOperation.Inputs.AsSpan().SequenceEqual(operation.Inputs.AsSpan()) &&
                                    composedOperation.Results.AsSpan().SequenceEqual(operation.Results.AsSpan())
                                    ?
                                    operation
                                    :
                                    composedOperation);
                            }
                        }

                        if (source is not null && conditional is null)
                            maps[output] = source with { Map = map, Fallback = fallback };

                        Advance(index, lastIndex);
                        index = lastIndex;
                        continue;
                    }
                }

                if (operation.Results.Length is 1 &&
                    instruction.Kind is StackInstructionKind.Local_Get or StackInstructionKind.Push_Literal or
                        StackInstructionKind.Local_Get_Skip_Head_Const &&
                    (instruction.Kind is StackInstructionKind.Push_Literal || instruction.LocalIndices.Length is 1))
                {
                    reloads[operation.Results[0]] = instruction;
                }

                operations.Add(
                    operation.Inputs.Any(value => Resolve(value) != value)
                    ?
                    operation with { Inputs = [.. operation.Inputs.Select(Resolve)] }
                    :
                    operation);

                Advance(index, index);
            }

            var rewritten = operations.Build(block);

            var remapped =
                replacements.Count is 0 ? terminator : RemapMapTerminator(terminator, Resolve);

            return Equals(remapped, block.Terminator) ? rewritten : rewritten with { Terminator = remapped };

            void Advance(int first, int last)
            {
                for (var current = first; current <= last; current++)
                {
                    var original = block.Operations[current];
                    var pop = StackInstruction.GetDetails(original.Instruction).PopCount;
                    stack.RemoveRange(stack.Count - pop, pop);
                    stack.AddRange(original.Results);
                }
            }

            void Invalidate(IEnumerable<int> written)
            {
                var changedLocals = written.ToArray();

                if (changedLocals.Length is 0)
                    return;

                foreach (var local in locals.Keys.ToArray())
                {
                    if (changedLocals.Contains(local) || DependsOnAny(locals[local].Key, changedLocals))
                        locals.Remove(local);
                }

                foreach (var value in reloads.Keys.ToArray())
                {
                    if (DependsOnAny(reloads[value], changedLocals))
                        reloads.Remove(value);
                }

                foreach (var value in maps.Keys.ToArray())
                {
                    if (DependsOnAny(maps[value].Key, changedLocals))
                        maps.Remove(value);
                }
            }
        }
    }

    private static bool DependsOnAny(StackInstruction instruction, IEnumerable<int> locals) =>
        !instruction.LocalIndices.IsDefaultOrEmpty && instruction.LocalIndices.Intersect(locals).Any();

    private static bool IsLiteralValueMap(StackInstruction instruction) =>
        instruction.Kind is StackInstructionKind.Select_Value_Const &&
        instruction.ValueMap is { Count: <= MaxComposedValueMapCases } &&
        instruction.Literal is not null && instruction.JumpOffset is null;

    private static bool CanTransformMapValue(StackInstruction instruction) =>
        IsLiteralValueMap(instruction) ||
        instruction.Kind is StackInstructionKind.Head_Generic ||
        instruction.Kind is StackInstructionKind.Skip_Head_Const && instruction.SkipCount is not null ||
        instruction.Kind is StackInstructionKind.Equal_Binary_Const or StackInstructionKind.Not_Equal_Binary_Const &&
        instruction.Literal is not null;

    private static PineValue Boolean(bool value) => PineValueInProcess.CreateBool(value).Evaluate();

    private static PineValue TransformMapValue(StackInstruction instruction, PineValue value) =>
        instruction.Kind switch
        {
            StackInstructionKind.Select_Value_Const =>
            instruction.ValueMap!.GetValueOrDefault(value, instruction.Literal!.Evaluate()),

            StackInstructionKind.Head_Generic or StackInstructionKind.Skip_Head_Const =>
            PineValueInProcess.Create(value).GetElementAt(
                instruction.Kind is StackInstructionKind.Head_Generic ? 0 : Math.Max(0, instruction.SkipCount ?? 0)).Evaluate(),

            StackInstructionKind.Equal_Binary_Const =>
            Boolean(value.Equals(instruction.Literal!.Evaluate())),

            StackInstructionKind.Not_Equal_Binary_Const =>
            Boolean(!value.Equals(instruction.Literal!.Evaluate())),

            _ =>
            throw new NotImplementedException("TransformMapValue does not handle instruction kind: " + instruction.Kind)
        };

    private FiniteMapState?[] AnalyzeFiniteMapValues()
    {
        var states = new FiniteMapState?[Blocks.Length];
        states[Entry.Value] = new([], []);
        var pending = new Queue<PineBlockId>();
        var queued = new HashSet<PineBlockId> { Entry };
        pending.Enqueue(Entry);

        while (pending.TryDequeue(out var id))
        {
            queued.Remove(id);
            var block = Blocks[id.Value];
            var (values, locals) = TraceFiniteMapValues(block, states[id.Value]!);

            if (block.Terminator is PineControlFlowTerminator.ValueMap { Map.Count: <= MaxComposedValueMapCases } valueMap &&
                FiniteSet(valueMap.Map.Values) is { } mapped)
            {
                values[valueMap.Result] = mapped;
            }

            foreach (var (target, arguments) in Successors(block.Terminator))
            {
                var parameters = arguments.Select(value => values.GetValueOrDefault(value)).ToImmutableArray();
                var incoming = new FiniteMapState(parameters, locals);

                if (states[target.Value] is { } previous)
                {
                    incoming =
                        new(
                            [.. previous.Parameters.Zip(parameters, MergeFiniteSets)],
                            previous.Locals.Keys.Intersect(locals.Keys)
                            .Select(local => (local, Values: MergeFiniteSets(previous.Locals[local], locals[local])))
                            .Where(item => item.Values is not null)
                            .ToDictionary(item => item.local, item => item.Values!));

                    if (previous.Parameters.Zip(incoming.Parameters)
                        .All(pair => SameFiniteSet(pair.First, pair.Second)) &&
                        previous.Locals.Count == incoming.Locals.Count &&
                        previous.Locals.All(
                            item =>
                            incoming.Locals.TryGetValue(item.Key, out var set) && item.Value.SetEquals(set)))
                    {
                        continue;
                    }
                }

                states[target.Value] = incoming;

                if (queued.Add(target))
                    pending.Enqueue(target);
            }
        }

        return states;
    }

    private static (
        Dictionary<PineVirtualValueId, ImmutableHashSet<PineValue>?> Values,
        Dictionary<int, ImmutableHashSet<PineValue>> Locals)
        TraceFiniteMapValues(PineBasicBlock block, FiniteMapState state)
    {
        var values = new Dictionary<PineVirtualValueId, ImmutableHashSet<PineValue>?>();
        var locals = new Dictionary<int, ImmutableHashSet<PineValue>>(state.Locals);
        var stack = block.Parameters.ToList();

        for (var index = 0; index < stack.Count; index++)
            values[stack[index]] = state.Parameters[index];

        foreach (var operation in block.Operations)
        {
            var instruction = operation.Instruction;

            if (instruction.Kind is StackInstructionKind.Local_Set)
            {
                for (var depth = 0; depth < instruction.LocalIndices.Length; depth++)
                {
                    var local = instruction.LocalIndices[depth];

                    if (values.GetValueOrDefault(stack[^(depth + 1)]) is { } finite)
                        locals[local] = finite;

                    else
                        locals.Remove(local);
                }
            }
            else
            {
                foreach (var local in WrittenLocals(instruction))
                    locals.Remove(local);

                if (instruction.Kind is StackInstructionKind.Local_Set_Literal && instruction.Literal is { } literal)
                    locals[instruction.SingleLocalIndex] = [literal.Evaluate()];
            }

            ImmutableHashSet<PineValue>? domain = null;

            if (instruction.Kind is StackInstructionKind.Push_Literal && instruction.Literal is { } pushed)
                domain = [pushed.Evaluate()];

            else if (IsLiteralValueMap(instruction))
            {
                domain =
                    values.GetValueOrDefault(operation.Inputs[0]) is { } inputs
                    ?
                    FiniteSet(inputs.Select(value => TransformMapValue(instruction, value)))
                    :
                    FiniteSet(instruction.ValueMap!.Values.Append(instruction.Literal!.Evaluate()));
            }
            else if (operation.Inputs is [var input] && CanTransformMapValue(instruction) &&
                values.GetValueOrDefault(input) is { } known)
            {
                domain = FiniteSet(known.Select(value => TransformMapValue(instruction, value)));
            }

            for (var index = 0; index < operation.Results.Length; index++)
            {
                var resultDomain = domain;

                if (instruction.Kind is StackInstructionKind.Local_Get)
                    resultDomain = locals.GetValueOrDefault(instruction.LocalIndices[index]);

                else if (instruction.Kind is StackInstructionKind.Local_Get_Skip_Head_Const)
                {
                    resultDomain =
                        locals.GetValueOrDefault(instruction.LocalIndices[index]) is { } source
                        ?
                        FiniteSet(
                            source.Select(
                                value => TransformMapValue(
                                    StackInstruction.Skip_Head_Const(instruction.SkipCount ?? 0),
                                    value)))
                        :
                        null;
                }

                values[operation.Results[index]] = resultDomain;
            }

            var pop = StackInstruction.GetDetails(instruction).PopCount;
            stack.RemoveRange(stack.Count - pop, pop);
            stack.AddRange(operation.Results);
        }

        return (values, locals);
    }

    private static ImmutableHashSet<PineValue>? FiniteSet(IEnumerable<PineValue> values)
    {
        var set = ImmutableHashSet.CreateBuilder<PineValue>();

        foreach (var value in values)
        {
            set.Add(value);

            if (set.Count > MaxComposedValueMapCases)
                return null;
        }

        return set.ToImmutable();
    }

    private static ImmutableHashSet<PineValue>? MergeFiniteSets(
        ImmutableHashSet<PineValue>? left,
        ImmutableHashSet<PineValue>? right) =>
        left is null || right is null ? null : FiniteSet(left.Concat(right));

    private static bool SameFiniteSet(ImmutableHashSet<PineValue>? left, ImmutableHashSet<PineValue>? right) =>
        left is null ? right is null : right is not null && left.SetEquals(right);

    private static PineControlFlowTerminator RemapMapTerminator(
        PineControlFlowTerminator terminator,
        Func<PineVirtualValueId, PineVirtualValueId> resolve) =>
        terminator switch
        {
            PineControlFlowTerminator.Return or PineControlFlowTerminator.TailInvoke => terminator,
            PineControlFlowTerminator.Jump jump => jump with { Arguments = [.. jump.Arguments.Select(resolve)] },

            PineControlFlowTerminator.ConditionalJump conditional =>
            conditional with
            {
                FallThroughArguments = [.. conditional.FallThroughArguments.Select(resolve)],
                BranchArguments = [.. conditional.BranchArguments.Select(resolve)]
            },

            PineControlFlowTerminator.Switch switchTerminator =>
            switchTerminator with
            {
                Arguments = [.. switchTerminator.Arguments.Select(resolve)]
            },

            PineControlFlowTerminator.Invoke invoke =>
            invoke with
            {
                Arguments = [.. invoke.Arguments.Select(resolve)],
                Inputs = [.. invoke.Inputs.Select(resolve)]
            },

            PineControlFlowTerminator.ValueMap valueMap =>
            valueMap with
            {
                Arguments = [.. valueMap.Arguments.Select(resolve)],
                Input = resolve(valueMap.Input)
            },

            _ =>
            throw new NotImplementedException(
                "RemapMapTerminator does not handle terminator variant: " + terminator.GetType().Name)
        };
}
