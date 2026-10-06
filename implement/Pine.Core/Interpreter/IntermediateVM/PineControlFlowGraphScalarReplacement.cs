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
        int OperationIndex,
        ImmutableArray<PineValue> Prefix,
        int ItemCount,
        bool IsBuilder)
    {
        public int Length => Prefix.Length + ItemCount;
    }

    private sealed record ListSlotLayout(
        ImmutableArray<PineValue> CommonPrefix,
        int FirstLocal,
        int MaxLength,
        int? LengthLocal);

    private readonly record struct ListOrigin(
        ImmutableHashSet<int> Candidates,
        bool MayBeOther)
    {
        public static ListOrigin Other => new([], true);

        public static ListOrigin FromCandidate(int index) =>
            new([index], false);

        public ListOrigin Union(ListOrigin other) =>
            new(Candidates.Union(other.Candidates), MayBeOther || other.MayBeOther);

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

                if (operation.Results.Length is not 1)
                {
                    continue;
                }

                ImmutableArray<PineValue> prefix = [];

                var isBuilder =
                    instruction.Kind is
                    StackInstructionKind.Build_List or StackInstructionKind.Build_List_With_Prefix;

                var count = 0;

                if (isBuilder)
                {
                    if (instruction.TakeCount is not { } takeCount || takeCount < 0)
                    {
                        continue;
                    }

                    count = takeCount;
                }

                if (instruction.Kind is
                    StackInstructionKind.Build_List_With_Prefix or StackInstructionKind.Push_Literal)
                {
                    if (instruction.Literal?.Evaluate() is not PineValue.ListValue literal)
                    {
                        continue;
                    }

                    prefix = [.. literal.Items.ToArray()];
                }
                else if (!isBuilder)
                {
                    continue;
                }

                candidateByResult.Add(operation.Results[0], candidates.Count);
                candidates.Add(new ListCandidate(block.Id, index, prefix, count, isBuilder));
            }
        }

        if (!candidates.Any(candidate => candidate.IsBuilder))
        {
            return this;
        }

        var states = new ListFlowState?[Blocks.Length];

        states[Entry.Value] =
            new ListFlowState(
                [.. Enumerable.Repeat(ListOrigin.Other, Blocks[Entry.Value].Parameters.Length)],
                []);

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
        var projections = new Dictionary<(PineBlockId, int), ListOrigin>();

        for (var index = 0; index < candidates.Count; index++)
        {
            if (states[candidates[index].Block.Value] is null)
            {
                escapes[index] = true;
            }
        }

        void RecordProjection(PineBlockId block, int index, ListOrigin origin)
        {
            if (origin.MayBeOther)
            {
                Escape(origin);
            }
            else if (origin.Candidates.Count > 0)
            {
                projections.Add((block, index), origin);
            }
        }

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
                            operation.Instruction.LocalIndices.Length is 1 &&
                            operation.Instruction.SingleLocalIndex is var localIndex)
                        {
                            var origin = locals.GetValueOrDefault(localIndex, ListOrigin.Other);

                            RecordProjection(block.Id, index, origin);
                        }

                        if (isProjection && inputs.Length is 1)
                        {
                            RecordProjection(block.Id, index, inputs[0]);

                            return;
                        }

                        if (kind is StackInstructionKind.Local_Set or
                        StackInstructionKind.Local_Set_Literal or
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

        // Alternatives can share a layout only when they cannot coexist before the
        // next loop backedge, and no old alias remains live when the layout is overwritten.
        var groupByCandidate = Enumerable.Range(0, candidates.Count).ToArray();

        foreach (var origin in projections.Values)
        {
            var roots = origin.Candidates.Select(index => groupByCandidate[index]).ToHashSet();
            var first = groupByCandidate[origin.Candidates.First()];

            for (var index = 0; index < groupByCandidate.Length; index++)
            {
                if (roots.Contains(groupByCandidate[index]))
                {
                    groupByCandidate[index] = first;
                }
            }
        }

        bool CanReach(PineBlockId source, PineBlockId target, PineBlockId? excluded = null)
        {
            var visited = new HashSet<PineBlockId>();
            var pendingBlocks = new Stack<PineBlockId>();
            pendingBlocks.Push(source);

            while (pendingBlocks.TryPop(out var block))
            {
                if (block == excluded)
                {
                    continue;
                }

                if (block == target)
                {
                    return true;
                }

                if (visited.Add(block))
                {
                    foreach (var (successor, _) in Successors(Blocks[block.Value].Terminator))
                    {
                        pendingBlocks.Push(successor);
                    }
                }
            }

            return false;
        }

        var backEdges = new Dictionary<(PineBlockId, PineBlockId), bool>();

        bool IsBackEdge(PineBlockId source, PineBlockId target)
        {
            if (!backEdges.TryGetValue((source, target), out var backEdge))
            {
                backEdge =
                    source == target ||
                    target == Entry ||
                    !CanReach(Entry, source, excluded: target);

                backEdges.Add((source, target), backEdge);
            }

            return backEdge;
        }

        bool CanReachInOneIteration(PineBlockId source, PineBlockId target)
        {
            var visited = new HashSet<PineBlockId>();
            var pendingBlocks = new Stack<PineBlockId>();
            pendingBlocks.Push(source);

            while (pendingBlocks.TryPop(out var block))
            {
                if (block == target)
                {
                    return true;
                }

                if (visited.Add(block))
                {
                    foreach (var (successor, _) in Successors(Blocks[block.Value].Terminator))
                    {
                        if (!IsBackEdge(block, successor))
                        {
                            pendingBlocks.Push(successor);
                        }
                    }
                }
            }

            return false;
        }

        bool CanReachFromSuccessors(PineBlockId source, PineBlockId target) =>
            Successors(Blocks[source.Value].Terminator)
            .Any(edge => CanReach(edge.Target, target));

        bool LocalMayBeReadAfter(PineBlockId block, int operationIndex, int local)
        {
            var visited = new HashSet<(PineBlockId, int)>();
            var pendingBlocks = new Stack<(PineBlockId Block, int Start)>();
            pendingBlocks.Push((block, operationIndex + 1));

            while (pendingBlocks.TryPop(out var pendingBlock))
            {
                if (!visited.Add(pendingBlock))
                {
                    continue;
                }

                var current = Blocks[pendingBlock.Block.Value];
                var overwritten = false;

                for (var index = pendingBlock.Start; index < current.Operations.Length; index++)
                {
                    var instruction = current.Operations[index].Instruction;

                    if (ReadLocals(instruction).Contains(local))
                    {
                        return true;
                    }

                    var writes = new HashSet<int>();
                    AddWrittenLocals(instruction, writes);

                    if (writes.Contains(local))
                    {
                        overwritten = true;
                        break;
                    }
                }

                if (!overwritten)
                {
                    foreach (var (successor, _) in Successors(current.Terminator))
                    {
                        pendingBlocks.Push((successor, 0));
                    }
                }
            }

            return false;
        }

        var groups =
            Enumerable.Range(0, candidates.Count)
            .GroupBy(index => groupByCandidate[index])
            .Select(group => group.ToArray())
            .ToArray();

        foreach (var group in groups)
        {
            var groupMembers = group.ToHashSet();

            bool OldAliasMaySurviveReplacement(ListCandidate candidate)
            {
                var block = Blocks[candidate.Block.Value];
                var state = states[candidate.Block.Value]!;
                var oldLocals = new HashSet<int>();

                var (values, _) =
                    TraceListOrigins(
                        block,
                        state,
                        candidateByResult,
                        (index, _, _, locals) =>
                        {
                            if (index == candidate.OperationIndex)
                            {
                                foreach (var (local, origin) in locals)
                                {
                                    if (origin.Candidates.Overlaps(groupMembers))
                                    {
                                        oldLocals.Add(local);
                                    }
                                }
                            }
                        });

                var stack = block.Parameters.ToList();

                for (var index = 0; index < candidate.OperationIndex; index++)
                {
                    var operation = block.Operations[index];
                    var popCount = StackInstruction.GetDetails(operation.Instruction).PopCount;
                    stack.RemoveRange(stack.Count - popCount, popCount);
                    stack.AddRange(operation.Results);
                }

                if (stack.Any(
                    value =>
                    values.GetValueOrDefault(value, ListOrigin.Other).Candidates.Overlaps(groupMembers)))
                {
                    return true;
                }

                return
                    oldLocals.Any(
                        local =>
                        LocalMayBeReadAfter(candidate.Block, candidate.OperationIndex, local));
            }

            var canShare =
                !group.Any(index => escapes[index]) &&
                !group.Where((left, leftIndex) =>
                    group.Skip(leftIndex + 1).Any(right =>
                        CanReachInOneIteration(candidates[left].Block, candidates[right].Block) ||
                        CanReachInOneIteration(candidates[right].Block, candidates[left].Block))).Any() &&
                !group.Any(index =>
                    CanReachFromSuccessors(candidates[index].Block, candidates[index].Block) &&
                    OldAliasMaySurviveReplacement(candidates[index]));

            if (!canShare)
            {
                foreach (var index in group)
                {
                    escapes[index] = true;
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
                .SelectMany(operation => operation.Instruction.LocalIndices)
                .Select(index => index + 1)
                .DefaultIfEmpty(0).Max());

        var layouts = new ListSlotLayout?[candidates.Count];

        foreach (var group in groups)
        {
            if (escapes[group[0]] || !group.Any(index => candidates[index].IsBuilder))
            {
                continue;
            }

            var firstPrefix = candidates[group[0]].Prefix;
            var commonLength = firstPrefix.Length;

            foreach (var index in group.Skip(1))
            {
                var prefix = candidates[index].Prefix;
                commonLength = Math.Min(commonLength, prefix.Length);

                for (var item = 0; item < commonLength; item++)
                {
                    if (!firstPrefix[item].Equals(prefix[item]))
                    {
                        commonLength = item;
                        break;
                    }
                }
            }

            var maxLength = group.Max(index => candidates[index].Length);
            var hasDifferentLengths = group.Any(index => candidates[index].Length != candidates[group[0]].Length);

            var needsLength =
                hasDifferentLengths &&
                projections.Any(
                    projection =>
                    projection.Value.Candidates.Any(index => groupByCandidate[index] == groupByCandidate[group[0]]) &&
                    Blocks[projection.Key.Item1.Value].Operations[projection.Key.Item2].Instruction.Kind is
                    (StackInstructionKind.Length or StackInstructionKind.Length_Equal_Const));

            var layout =
                new ListSlotLayout(
                    [.. firstPrefix.Take(commonLength)],
                    nextLocal,
                    maxLength,
                    needsLength ? nextLocal + maxLength - commonLength : null);

            nextLocal += maxLength - commonLength + (needsLength ? 1 : 0);

            foreach (var index in group)
            {
                layouts[index] = layout;
            }
        }

        var nextValue =
            Blocks.SelectMany(
                block =>
                block.Parameters.Concat(block.Operations.SelectMany(operation => operation.Results)))
            .Select(value => value.Value + 1)
            .DefaultIfEmpty(0)
            .Max();

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
                            layouts[buildIndex] is { } buildLayout)
                        {
                            var candidate = candidates[buildIndex];
                            var count = candidate.ItemCount;

                            var firstItemLocal =
                                buildLayout.FirstLocal + candidate.Prefix.Length - buildLayout.CommonPrefix.Length;

                            if (count > 0)
                            {
                                operations.Add(
                                    new(
                                        StackInstruction.Local_Set(
                                            [.. Enumerable.Range(firstItemLocal, count).Reverse()]),
                                        [],
                                        []));

                                operations.Add(new(StackInstruction.PopMultiple(count), operation.Inputs, []));
                            }

                            void StoreLiteral(int local, PineValue value)
                            {
                                var temporary = new PineVirtualValueId(nextValue++);
                                operations.Add(new(StackInstruction.Push_Literal(value), [], [temporary]));
                                operations.Add(new(StackInstruction.Local_Set([local]), [], []));
                                operations.Add(new(StackInstruction.Pop, [temporary], []));
                            }

                            for (var item = buildLayout.CommonPrefix.Length; item < candidate.Prefix.Length; item++)
                            {
                                StoreLiteral(
                                    buildLayout.FirstLocal + item - buildLayout.CommonPrefix.Length,
                                    candidate.Prefix[item]);
                            }

                            for (var item = candidate.Length; item < buildLayout.MaxLength; item++)
                            {
                                StoreLiteral(
                                    buildLayout.FirstLocal + item - buildLayout.CommonPrefix.Length,
                                    PineValue.EmptyList);
                            }

                            if (buildLayout.LengthLocal is { } lengthLocal)
                            {
                                StoreLiteral(
                                    lengthLocal,
                                    PineValueInProcess.CreateInteger(candidate.Length).Evaluate());
                            }

                            operations.Add(
                                new(
                                    StackInstruction.Push_Literal(PineValue.EmptyList),
                                    [],
                                    operation.Results));

                            continue;
                        }

                        if (projections.TryGetValue((block.Id, index), out var projected) &&
                            layouts[projected.Candidates.First()] is { } projectionLayout)
                        {
                            var instruction = operation.Instruction;

                            if (operation.Inputs.Length is 1)
                            {
                                operations.Add(new(StackInstruction.Pop, operation.Inputs, []));
                            }

                            var projectedLength = candidates[projected.Candidates.First()].Length;

                            if (instruction.Kind is StackInstructionKind.Length_Equal_Const &&
                                projectionLayout.LengthLocal is { } lengthLocal)
                            {
                                var lengthValue = new PineVirtualValueId(nextValue++);
                                operations.Add(new(StackInstruction.Local_Get(lengthLocal), [], [lengthValue]));

                                operations.Add(
                                    new(
                                        StackInstruction.Equal_Binary_Const(
                                            PineValueInProcess.CreateInteger(instruction.IntegerLiteral!.Value)
                                            .Evaluate()),
                                        [lengthValue],
                                        operation.Results));

                                continue;
                            }

                            var projection =
                                instruction.Kind switch
                                {
                                    StackInstructionKind.Skip_Head_Const or
                                    StackInstructionKind.Local_Get_Skip_Head_Const =>
                                    ProjectElement(projectionLayout, instruction.SkipCount!.Value),

                                    StackInstructionKind.Head_Generic =>
                                    ProjectElement(projectionLayout, 0),

                                    StackInstructionKind.Length when projectionLayout.LengthLocal is { } projectedLengthLocal =>
                                    StackInstruction.Local_Get(projectedLengthLocal),

                                    StackInstructionKind.Length =>
                                    StackInstruction.Push_Literal(
                                        PineValueInProcess.CreateInteger(projectedLength).Evaluate()),

                                    StackInstructionKind.Length_Equal_Const =>
                                    StackInstruction.Push_Literal(
                                        PineValueInProcess.CreateBool(projectedLength == instruction.IntegerLiteral)
                                        .Evaluate()),

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

    private static StackInstruction ProjectElement(ListSlotLayout layout, int index) =>
        ProjectElementClamped(layout, Math.Max(0, index));

    private static StackInstruction ProjectElementClamped(ListSlotLayout layout, int index) =>
        index < layout.CommonPrefix.Length
        ?
        StackInstruction.Push_Literal(layout.CommonPrefix[index])
        :
        index < layout.MaxLength
        ?
        StackInstruction.Local_Get(layout.FirstLocal + index - layout.CommonPrefix.Length)
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

            if (instruction.Kind is StackInstructionKind.Local_Set)
            {
                for (var depth = 0; depth < instruction.LocalIndices.Length; depth++)
                    locals[instruction.LocalIndices[depth]] =
                        values.GetValueOrDefault(stack[^(depth + 1)], ListOrigin.Other);
            }
            else if (instruction.Kind is StackInstructionKind.Local_Set_Literal &&
                instruction.SingleLocalIndex is { } literalLocal)
            {
                locals[literalLocal] = ListOrigin.Other;
            }
            else if (instruction.Kind is StackInstructionKind.Local_Int_Add_Const)
            {
                foreach (var local in instruction.LocalIndices)
                    locals[local] = ListOrigin.Other;
            }

            var origin =
                operation.Results.Length is 1 &&
                candidateByResult.TryGetValue(operation.Results[0], out var candidate)
                ?
                ListOrigin.FromCandidate(candidate)
                :
                instruction.Kind is StackInstructionKind.Local_Get &&
                instruction.LocalIndices.Length is 1 &&
                instruction.SingleLocalIndex is { } sourceLocal
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
