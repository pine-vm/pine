using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Moves total, loop-invariant header computations and their existing local bindings into
    /// a preheader. Initial execution performs the same work; later iterations reuse immutable
    /// values without extra transport slots, speculative body evaluation, or runtime guards.
    /// </summary>
    public PineControlFlowGraph HoistLoopInvariantStores(int parameterCount) =>
        HoistLoopInvariantStores(parameterCount, null);

    private PineControlFlowGraph HoistLoopInvariantStores(int parameterCount, OptimizationContext? context)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(parameterCount);
        Validate(context);
        var loops = context?.GetLoops(this) ?? AnalyzeLoops();

        if (loops.Loops.IsEmpty)
            return this;

        var initialized = DefinitelyInitializedLocals(parameterCount, context);

        foreach (var loop in loops.Loops.OrderBy(loop => loop.Blocks.Length).ThenBy(loop => loop.Header.Value))
        {
            var header = Blocks[loop.Header.Value];

            if (!header.Parameters.IsEmpty)
                continue;

            var body = loop.Blocks.ToHashSet();

            if (body.Any(block => RequiredFallthroughTarget(Blocks[block.Value].Terminator) == loop.Header))
                continue;

            var writeCounts = new Dictionary<int, int>();

            foreach (var block in loop.Blocks)
                CountChangingLocalWrites(Blocks[block.Value], writeCounts);

            var invariant = new HashSet<int>();

            foreach (var operation in header.Operations)
            {
                foreach (var local in ReadLocals(operation.Instruction))
                {
                    if (!writeCounts.ContainsKey(local) && initialized[header.Id.Value] is { } ready &&
                        ready.Contains(local))
                    {
                        invariant.Add(local);
                    }
                }
            }

            if (HoistBoundHeaderPrefix(loop, writeCounts, invariant) is { } boundPrefix)
            {
                boundPrefix.Validate(context);
                return boundPrefix;
            }

            var move = new bool[header.Operations.Length];
            var readsBefore = new HashSet<int>();
            var depth = 0;
            var first = 0;

            for (var index = 0; index < header.Operations.Length; index++)
            {
                var operation = header.Operations[index];
                depth += operation.Results.Length - operation.Inputs.Length;

                if (depth != 0)
                    continue;

                var group = header.Operations[first..(index + 1)];

                if (TryHoist(group, writeCounts, invariant, readsBefore))
                    Array.Fill(move, true, first, index - first + 1);

                foreach (var item in group)
                    readsBefore.UnionWith(ReadLocals(item.Instruction));

                first = index + 1;
            }

            if (!move.Any(value => value))
                continue;

            var hoisted = header.Operations.Where((_, index) => move[index]).ToImmutableArray();
            var remaining = header.Operations.Where((_, index) => !move[index]).ToImmutableArray();
            var blocks = Blocks.SetItem(header.Id.Value, header with { Operations = remaining });
            var result = WithLoopPreheader(loop, hoisted, blocks, []);
            result.Validate(context);
            return result;
        }

        return this;
    }

    private PineControlFlowGraph WithLoopPreheader(
        PineControlFlowLoop loop,
        ImmutableArray<PineControlFlowOperation> hoisted,
        ImmutableArray<PineBasicBlock> blocks,
        ImmutableArray<PineVirtualValueId> arguments)
    {
        var defined = hoisted.SelectMany(operation => operation.Results).ToHashSet();

        if (loop.Preheader is { } preheader &&
            Blocks[preheader.Value].Terminator is PineControlFlowTerminator.Jump jump &&
            !blocks[preheader.Value].Parameters.Concat(blocks[preheader.Value].Operations.SelectMany(operation => operation.Results))
                .Any(defined.Contains))
        {
            var entering = blocks[preheader.Value];

            return
                this with
                {
                    Blocks =
                    blocks.SetItem(
                        preheader.Value,
                        entering with
                        {
                            Operations = entering.Operations.AddRange(hoisted),
                            Terminator = jump with { Arguments = arguments }
                        })
                };
        }

        var body = loop.Blocks.ToHashSet();
        var preheaderId = new PineBlockId(Blocks.Length);
        var rewritten = ImmutableArray.CreateBuilder<PineBasicBlock>(Blocks.Length + 1);

        foreach (var block in blocks)
        {
            if (block.Id == loop.Header)
            {
                rewritten.Add(
                    new(
                        preheaderId,
                        [],
                        hoisted,
                        new PineControlFlowTerminator.Jump(loop.Header, arguments, IsFallThrough: true)));
            }

            rewritten.Add(
                block with
                {
                    Terminator =
                    body.Contains(block.Id)
                    ?
                    block.Terminator
                    :
                    RedirectTargets(block.Terminator, target => target == loop.Header ? preheaderId : target)
                });
        }

        return
            (this with { Entry = Entry == loop.Header ? preheaderId : Entry })
            .TryRemapBlockIds(rewritten.MoveToImmutable()) ??
            throw new InvalidOperationException("Loop preheader insertion removed the entry.");
    }

    private PineControlFlowGraph? HoistBoundHeaderPrefix(
        PineControlFlowLoop loop, Dictionary<int, int> writes, HashSet<int> invariant)
    {
        if (loop.Backedges.Any(
            edge => Blocks[edge.Source.Value].Terminator is not PineControlFlowTerminator.Jump
            { Arguments.IsEmpty: true }))
        {
            return null;
        }

        var header = Blocks[loop.Header.Value];
        var stack = new List<PineVirtualValueId>();

        for (var index = 0; index < header.Operations.Length; index++)
        {
            var operation = header.Operations[index];
            stack.RemoveRange(stack.Count - operation.Inputs.Length, operation.Inputs.Length);
            stack.AddRange(operation.Results);

            if (operation.Instruction.Kind != StackInstructionKind.Local_Set ||
                operation.Instruction.PopCount is > 0 || stack.Count == 0 ||
                operation.Instruction.LocalIndices.Length != stack.Count ||
                operation.Instruction.LocalIndices.Distinct().Count() != stack.Count)
            {
                continue;
            }

            var prefix = header.Operations[..(index + 1)];

            if (!TryHoist(prefix, writes, [.. invariant], []))
                continue;

            var values = stack.ToImmutableArray();
            var defined = prefix.SelectMany(item => item.Results).ToHashSet();

            if (header.Operations[(index + 1)..].SelectMany(item => item.Inputs)
                .Any(value => defined.Contains(value) && !values.Contains(value)))
            {
                continue;
            }

            var locals = operation.Instruction.LocalIndices.Reverse().ToImmutableArray();
            var next = checked(MaximumVirtualValueId() + 1);
            var parameters = values.Select(_ => new PineVirtualValueId(checked(next++))).ToImmutableArray();
            var substitutions = values.Zip(parameters).ToDictionary(pair => pair.First, pair => pair.Second);
            PineVirtualValueId Rewrite(PineVirtualValueId value) => substitutions.GetValueOrDefault(value, value);

            var blocks =
                Blocks.SetItem(
                    header.Id.Value,
                    header with
                    {
                        Parameters = parameters,
                        Operations =
                        [
                        .. header.Operations[(index + 1)..].Select(
                            item => item with
                            {
                                Inputs = [.. item.Inputs.Select(Rewrite)]
                            })
                        ],
                        Terminator = RewriteTerminatorInputs(header.Terminator, Rewrite)
                    });

            foreach (var edge in loop.Backedges)
            {
                var block = blocks[edge.Source.Value];
                var results = locals.Select(_ => new PineVirtualValueId(checked(next++))).ToImmutableArray();
                var reload = new PineControlFlowOperation(StackInstruction.Local_Get(locals), [], results);

                blocks =
                    blocks.SetItem(
                        block.Id.Value,
                        block with
                        {
                            Operations = block.Operations.Add(reload),
                            Terminator = ((PineControlFlowTerminator.Jump)block.Terminator) with { Arguments = results }
                        });
            }

            return WithLoopPreheader(loop, prefix, blocks, values);
        }

        return null;
    }

    private static bool TryHoist(
        ImmutableArray<PineControlFlowOperation> group,
        Dictionary<int, int> loopWrites,
        HashSet<int> invariant,
        HashSet<int> readsBefore)
    {
        var writes = new Dictionary<int, int>();

        foreach (var operation in group)
        {
            if (!IsTotalInvariantOperation(operation.Instruction.Kind))
                return false;

            foreach (var local in WrittenLocals(operation.Instruction))
                writes[local] = writes.GetValueOrDefault(local) + 1;
        }

        if (writes.Count == 0 ||
            writes.Any(item => loopWrites.GetValueOrDefault(item.Key) != item.Value || readsBefore.Contains(item.Key)))
        {
            return false;
        }

        var available = new HashSet<int>(invariant);

        foreach (var operation in group)
        {
            if (ReadLocals(operation.Instruction).Any(local => !available.Contains(local)))
                return false;

            foreach (var local in WrittenLocals(operation.Instruction))
                available.Add(local);
        }

        invariant.UnionWith(writes.Keys);
        return true;
    }

    private static void CountChangingLocalWrites(PineBasicBlock block, Dictionary<int, int> writes)
    {
        var stack = block.Parameters.ToList();
        var origins = new Dictionary<PineVirtualValueId, int>();

        foreach (var operation in block.Operations)
        {
            var instruction = operation.Instruction;

            if (instruction.Kind == StackInstructionKind.Local_Get)
            {
                for (var index = 0; index < operation.Results.Length; index++)
                    origins[operation.Results[index]] = instruction.LocalIndices[index];
            }

            var changing = WrittenLocals(instruction).ToHashSet();

            if (instruction.Kind == StackInstructionKind.Local_Set && instruction.LocalIndices.Length <= stack.Count)
            {
                var assignments = new Dictionary<int, PineVirtualValueId>();

                for (var depth = 0; depth < instruction.LocalIndices.Length; depth++)
                    assignments[instruction.LocalIndices[depth]] = stack[stack.Count - depth - 1];

                foreach (var (local, value) in assignments)
                {
                    if (origins.TryGetValue(value, out var source) && source == local)
                        changing.Remove(local);
                }
            }

            foreach (var local in WrittenLocals(instruction))
            {
                if (changing.Contains(local))
                    writes[local] = writes.GetValueOrDefault(local) + 1;
            }

            if (changing.Count > 0)
            {
                foreach (var value in origins.Where(item => changing.Contains(item.Value)).Select(item => item.Key).ToArray())
                    origins.Remove(value);
            }

            stack.RemoveRange(stack.Count - operation.Inputs.Length, operation.Inputs.Length);
            stack.AddRange(operation.Results);
        }
    }

    private static bool IsTotalInvariantOperation(StackInstructionKind kind) =>
        kind is
        StackInstructionKind.Push_Literal or
        StackInstructionKind.Local_Get or StackInstructionKind.Local_Get_Skip_Head_Const or
        StackInstructionKind.Local_Set or StackInstructionKind.Local_Set_Literal or StackInstructionKind.Pop or
        StackInstructionKind.Length or StackInstructionKind.Length_Equal_Const or
        StackInstructionKind.Build_List or StackInstructionKind.Build_List_With_Prefix or
        StackInstructionKind.Head_Generic or StackInstructionKind.Skip_Head_Const or
        StackInstructionKind.Skip_Const or StackInstructionKind.Take_Const or StackInstructionKind.Take_Last_Const or
        StackInstructionKind.Concat_Binary or StackInstructionKind.Reverse or
        StackInstructionKind.Equal_Binary or StackInstructionKind.Not_Equal_Binary or
        StackInstructionKind.Equal_Binary_Const or StackInstructionKind.Not_Equal_Binary_Const or
        StackInstructionKind.Is_List_Value or StackInstructionKind.Is_Blob_Value or
        StackInstructionKind.Int_Add_Binary or StackInstructionKind.Int_Add_Const or
        StackInstructionKind.Int_Mul_Binary or StackInstructionKind.Int_Mul_Const or
        StackInstructionKind.Int_Mul_Const_Add_Binary or
        StackInstructionKind.Int_Less_Than_Binary or StackInstructionKind.Int_Less_Than_Or_Equal_Binary or
        StackInstructionKind.Int_Less_Than_Const or StackInstructionKind.Int_Less_Than_Or_Equal_Const or
        StackInstructionKind.Int_Greater_Than_Or_Equal_Const;
}
