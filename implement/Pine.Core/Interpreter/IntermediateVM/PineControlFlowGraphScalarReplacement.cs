using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    private sealed record ListCandidate(
        PineBlockId Block,
        ImmutableArray<PineValue> Prefix,
        int ItemCount);

    private readonly record struct ListOrigin(
        ImmutableHashSet<int> Candidates,
        bool MayBeOther)
    {
        public static ListOrigin Other => new([], true);

        public static ListOrigin FromCandidate(int index) =>
            new([index], false);

        public ListOrigin Union(ListOrigin other) =>
            new(Candidates.Union(other.Candidates), MayBeOther || other.MayBeOther);

        public int? SoleCandidate =>
            !MayBeOther && Candidates.Count is 1 ? Candidates.Single() : null;
    }

    private sealed record ListFlowState(
        ImmutableArray<ListOrigin> Parameters,
        Dictionary<int, ListOrigin> Locals);

    /// <summary>
    /// Replaces lists used only for element or length projections with independent local slots.
    /// An unobserved empty-list placeholder keeps the evaluation stack and block edges unchanged.
    /// </summary>
    public PineControlFlowGraph ReplaceNonEscapingLists(int parameterCount)
    {
        Validate();

        var candidates = new List<ListCandidate>();
        var candidateByResult = new Dictionary<PineVirtualValueId, int>();

        foreach (var block in Blocks)
        {
            for (var index = 0; index < block.Operations.Length; index++)
            {
                var operation = block.Operations[index];
                var instruction = operation.Instruction;

                if (instruction.Kind is not
                    (StackInstructionKind.Build_List or StackInstructionKind.Build_List_With_Prefix) ||
                    instruction.TakeCount is not { } count ||
                    count < 0 ||
                    operation.Results.Length is not 1)
                {
                    continue;
                }

                ImmutableArray<PineValue> prefix = [];

                if (instruction.Kind is StackInstructionKind.Build_List_With_Prefix)
                {
                    if (instruction.Literal?.Evaluate() is not PineValue.ListValue literal)
                    {
                        continue;
                    }

                    prefix = [.. literal.Items.ToArray()];
                }

                candidateByResult.Add(operation.Results[0], candidates.Count);
                candidates.Add(new ListCandidate(block.Id, prefix, count));
            }
        }

        if (candidates.Count is 0)
        {
            return this;
        }

        var states = new ListFlowState?[Blocks.Length];
        states[Entry.Value] = new ListFlowState([], []);
        var pending = new Queue<PineBlockId>();
        pending.Enqueue(Entry);

        void MergeInto(PineBlockId target, ImmutableArray<ListOrigin> arguments, Dictionary<int, ListOrigin> locals)
        {
            if (states[target.Value] is not { } previous)
            {
                states[target.Value] = new ListFlowState(arguments, new Dictionary<int, ListOrigin>(locals));
                pending.Enqueue(target);
                return;
            }

            var mergedParameters =
                previous.Parameters.Zip(arguments, (left, right) => left.Union(right)).ToImmutableArray();

            var mergedLocals = new Dictionary<int, ListOrigin>();

            foreach (var key in previous.Locals.Keys.Union(locals.Keys))
            {
                var left = previous.Locals.GetValueOrDefault(key, ListOrigin.Other);
                var right = locals.GetValueOrDefault(key, ListOrigin.Other);
                var merged = left.Union(right);

                if (merged.Candidates.Count is not 0)
                {
                    mergedLocals.Add(key, merged);
                }
            }

            if (!previous.Parameters.Zip(mergedParameters, SameOrigin).All(equal => equal) ||
                previous.Locals.Count != mergedLocals.Count ||
                mergedLocals.Any(
                    pair =>
                    !previous.Locals.TryGetValue(pair.Key, out var old) || !SameOrigin(old, pair.Value)))
            {
                states[target.Value] = new ListFlowState(mergedParameters, mergedLocals);
                pending.Enqueue(target);
            }
        }

        while (pending.TryDequeue(out var blockId))
        {
            var block = Blocks[blockId.Value];
            var state = states[blockId.Value]!;

            var (values, locals) =
                TraceListOrigins(
                    block,
                    state,
                    candidateByResult,
                    onOperation: null);

            foreach (var (target, arguments) in Successors(block.Terminator))
            {
                MergeInto(
                    target,
                    [.. arguments.Select(argument => values.GetValueOrDefault(argument, ListOrigin.Other))],
                    locals);
            }
        }

        var escapes = new bool[candidates.Count];
        var projections = new Dictionary<(PineBlockId, int), int>();

        void Escape(ListOrigin origin)
        {
            foreach (var candidate in origin.Candidates)
            {
                escapes[candidate] = true;
            }
        }

        for (var blockIndex = 0; blockIndex < Blocks.Length; blockIndex++)
        {
            if (states[blockIndex] is not { } state)
            {
                continue;
            }

            var block = Blocks[blockIndex];

            var (values, _) =
                TraceListOrigins(
                    block,
                    state,
                    candidateByResult,
                    (index, operation, inputs, locals) =>
                    {
                        var kind = operation.Instruction.Kind;

                        var isProjection =
                            kind is
                            StackInstructionKind.Skip_Head_Const or
                            StackInstructionKind.Head_Generic or
                            StackInstructionKind.Length or
                            StackInstructionKind.Length_Equal_Const;

                        if (kind is StackInstructionKind.Local_Get_Skip_Head_Const &&
                            operation.Instruction.LocalIndex is { } localIndex)
                        {
                            var origin = locals.GetValueOrDefault(localIndex, ListOrigin.Other);

                            if (origin.SoleCandidate is { } candidate)
                            {
                                projections.Add((block.Id, index), candidate);
                            }
                            else
                            {
                                Escape(origin);
                            }
                        }

                        if (isProjection && inputs.Length is 1)
                        {
                            if (inputs[0].SoleCandidate is { } candidate)
                            {
                                projections.Add((block.Id, index), candidate);
                            }
                            else
                            {
                                Escape(inputs[0]);
                            }

                            return;
                        }

                        if (kind is StackInstructionKind.Local_Set or
                        StackInstructionKind.Local_Set_Descending or
                        StackInstructionKind.Local_Get or
                        StackInstructionKind.Local_Get_Skip_Head_Const or
                        StackInstructionKind.Pop)
                        {
                            return;
                        }

                        foreach (var input in inputs)
                        {
                            Escape(input);
                        }
                    });

            switch (block.Terminator)
            {
                case PineControlFlowTerminator.Return:
                    Escape(TerminatorInput(block, values, 1));
                    break;

                case PineControlFlowTerminator.ConditionalJump:
                    Escape(TerminatorInput(block, values, 1));
                    break;

                case PineControlFlowTerminator.Switch switchTerminator:
                    for (var depth = 0; depth < switchTerminator.PopCount; depth++)
                    {
                        Escape(TerminatorInput(block, values, depth + 1));
                    }

                    break;

                case PineControlFlowTerminator.Invoke invoke:
                    foreach (var input in invoke.Inputs)
                    {
                        Escape(values.GetValueOrDefault(input, ListOrigin.Other));
                    }

                    break;

                case PineControlFlowTerminator.TailInvoke tailInvoke:
                    if (!IsInvocation(tailInvoke.InvokeInstruction.Kind))
                    {
                        Array.Fill(escapes, true);
                        break;
                    }

                    var operandCount = StackInstruction.GetDetails(tailInvoke.InvokeInstruction).PopCount;

                    for (var depth = 1; depth <= operandCount; depth++)
                    {
                        Escape(TerminatorInput(block, values, depth));
                    }

                    break;

                case PineControlFlowTerminator.Jump:
                    break;

                default:
                    throw new NotImplementedException(
                        "ReplaceNonEscapingLists does not handle terminator variant: " +
                        block.Terminator.GetType().Name);
            }
        }

        // Repeated execution of one builder could overwrite slots still referenced by an older alias.
        foreach (var (candidate, index) in candidates.Select((item, index) => (item, index)))
        {
            var visited = new HashSet<PineBlockId>();

            var todo =
                new Stack<PineBlockId>(
                    Successors(Blocks[candidate.Block.Value].Terminator).Select(edge => edge.Target));

            while (todo.TryPop(out var next))
            {
                if (next == candidate.Block)
                {
                    escapes[index] = true;
                    break;
                }

                if (visited.Add(next))
                {
                    foreach (var (target, _) in Successors(Blocks[next.Value].Terminator))
                    {
                        todo.Push(target);
                    }
                }
            }
        }

        if (escapes.All(escape => escape))
        {
            return this;
        }

        var nextLocal =
            Math.Max(
                parameterCount,
                Blocks.SelectMany(block => block.Operations)
                .Where(operation => IsLocalInstruction(operation.Instruction.Kind))
                .Select(operation => operation.Instruction.LocalIndex!.Value + 1)
                .DefaultIfEmpty(0).Max());

        var itemLocals = new int[candidates.Count];

        for (var index = 0; index < candidates.Count; index++)
        {
            if (!escapes[index])
            {
                itemLocals[index] = nextLocal;
                nextLocal += candidates[index].ItemCount;
            }
        }

        var rewritten =
            Blocks.Select(
                block =>
                {
                    var operations = ImmutableArray.CreateBuilder<PineControlFlowOperation>();

                    for (var index = 0; index < block.Operations.Length; index++)
                    {
                        var operation = block.Operations[index];

                        if (operation.Results.Length is 1 &&
                            candidateByResult.TryGetValue(operation.Results[0], out var buildIndex) &&
                            !escapes[buildIndex])
                        {
                            var count = candidates[buildIndex].ItemCount;

                            if (count > 0)
                            {
                                operations.Add(
                                    new(
                                        StackInstruction.Local_Set_Descending(itemLocals[buildIndex] + count - 1, count),
                                        [],
                                        []));

                                operations.Add(new(StackInstruction.PopMultiple(count), operation.Inputs, []));
                            }

                            operations.Add(
                                new(
                                    StackInstruction.Push_Literal(PineValue.EmptyList),
                                    [],
                                    operation.Results));

                            continue;
                        }

                        if (projections.TryGetValue((block.Id, index), out var projected) &&
                            !escapes[projected])
                        {
                            var instruction = operation.Instruction;

                            if (operation.Inputs.Length is 1)
                            {
                                operations.Add(new(StackInstruction.Pop, operation.Inputs, []));
                            }

                            var projection =
                                instruction.Kind switch
                                {
                                    StackInstructionKind.Skip_Head_Const or
                                    StackInstructionKind.Local_Get_Skip_Head_Const =>
                                    ProjectElement(
                                        candidates[projected],
                                        itemLocals[projected],
                                        instruction.SkipCount!.Value),

                                    StackInstructionKind.Head_Generic =>
                                    ProjectElement(candidates[projected], itemLocals[projected], 0),

                                    StackInstructionKind.Length =>
                                    StackInstruction.Push_Literal(
                                        PineValueInProcess.CreateInteger(
                                            candidates[projected].Prefix.Length +
                                            candidates[projected].ItemCount).Evaluate()),

                                    StackInstructionKind.Length_Equal_Const =>
                                    StackInstruction.Push_Literal(
                                        PineValueInProcess.CreateBool(
                                            candidates[projected].Prefix.Length +
                                            candidates[projected].ItemCount ==
                                            instruction.IntegerLiteral).Evaluate()),

                                    _ =>
                                    throw new InvalidOperationException("Unexpected list projection.")
                                };

                            operations.Add(new(projection, [], operation.Results));
                            continue;
                        }

                        operations.Add(operation);
                    }

                    return block with { Operations = operations.ToImmutable() };
                })
            .ToImmutableArray();

        var result = new PineControlFlowGraph(Entry, rewritten);
        result.Validate();
        return result;
    }

    private static StackInstruction ProjectElement(ListCandidate candidate, int firstLocal, int index) =>
        ProjectElementClamped(candidate, firstLocal, Math.Max(0, index));

    private static StackInstruction ProjectElementClamped(ListCandidate candidate, int firstLocal, int index) =>
        index < candidate.Prefix.Length
        ?
        StackInstruction.Push_Literal(candidate.Prefix[index])
        :
        index - candidate.Prefix.Length < candidate.ItemCount
        ?
        StackInstruction.Local_Get(firstLocal + index - candidate.Prefix.Length)
        :
        StackInstruction.Push_Literal(PineValue.EmptyList);

    private static bool SameOrigin(ListOrigin left, ListOrigin right) =>
        left.MayBeOther == right.MayBeOther &&
        left.Candidates.SetEquals(right.Candidates);

    private static ListOrigin TerminatorInput(
        PineBasicBlock block,
        Dictionary<PineVirtualValueId, ListOrigin> values,
        int depth)
    {
        var stack = block.Parameters.ToList();

        foreach (var operation in block.Operations)
        {
            var count = StackInstruction.GetDetails(operation.Instruction).PopCount;
            stack.RemoveRange(stack.Count - count, count);
            stack.AddRange(operation.Results);
        }

        return values.GetValueOrDefault(stack[^depth], ListOrigin.Other);
    }

    private static (Dictionary<PineVirtualValueId, ListOrigin> Values, Dictionary<int, ListOrigin> Locals)
        TraceListOrigins(
        PineBasicBlock block,
        ListFlowState state,
        Dictionary<PineVirtualValueId, int> candidateByResult,
        Action<int, PineControlFlowOperation, ImmutableArray<ListOrigin>, Dictionary<int, ListOrigin>>? onOperation)
    {
        var values = new Dictionary<PineVirtualValueId, ListOrigin>();
        var locals = new Dictionary<int, ListOrigin>(state.Locals);
        var stack = block.Parameters.ToList();

        for (var index = 0; index < stack.Count; index++)
        {
            values.Add(stack[index], state.Parameters[index]);
        }

        for (var index = 0; index < block.Operations.Length; index++)
        {
            var operation = block.Operations[index];
            var instruction = operation.Instruction;

            var inputs =
                operation.Inputs
                .Select(input => values.GetValueOrDefault(input, ListOrigin.Other))
                .ToImmutableArray();

            onOperation?.Invoke(index, operation, inputs, locals);

            if (instruction.Kind is StackInstructionKind.Local_Set && instruction.LocalIndex is { } local)
            {
                locals[local] = values.GetValueOrDefault(stack[^1], ListOrigin.Other);
            }
            else if (instruction.Kind is StackInstructionKind.Local_Set_Descending &&
                instruction.LocalIndex is { } highest &&
                instruction.TakeCount is { } count)
            {
                for (var depth = 0; depth < count; depth++)
                {
                    locals[highest - depth] = values.GetValueOrDefault(stack[^(depth + 1)], ListOrigin.Other);
                }
            }

            var origin =
                operation.Results.Length is 1 &&
                candidateByResult.TryGetValue(operation.Results[0], out var candidate)
                ?
                ListOrigin.FromCandidate(candidate)
                :
                instruction.Kind is StackInstructionKind.Local_Get &&
                instruction.LocalIndex is { } sourceLocal
                ?
                locals.GetValueOrDefault(sourceLocal, ListOrigin.Other)
                :
                ListOrigin.Other;

            foreach (var result in operation.Results)
            {
                values.Add(result, origin);
            }

            var popCount = StackInstruction.GetDetails(instruction).PopCount;
            stack.RemoveRange(stack.Count - popCount, popCount);
            stack.AddRange(operation.Results);
        }

        return (values, locals);
    }
}
