using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    /// <summary>
    /// Splices a compiled callee into one invocation, giving its locals and virtual values
    /// independent namespaces. The caller's stack below the arguments stays live throughout
    /// the callee, including across its local loop back-edges.
    /// </summary>
    public PineControlFlowGraph InlineInvocation(
        PineBlockId invocationBlock,
        PineControlFlowGraph callee,
        int callerParameterCount,
        int calleeParameterCount)
    {
        Validate();
        callee.Validate();

        var callBlock = Blocks[invocationBlock.Value];

        if (callBlock.Terminator is not PineControlFlowTerminator.Invoke call)
        {
            throw new ArgumentException("Block does not end in an invocation.", nameof(invocationBlock));
        }

        if (callee.Entry != callee.Blocks[0].Id ||
            callee.Blocks[0].Parameters.Length is not 0 ||
            call.Inputs.Length != calleeParameterCount ||
            callee.Blocks.Any(block => block.Terminator is PineControlFlowTerminator.TailInvoke))
        {
            throw new ArgumentException("Callee cannot be spliced into this invocation.", nameof(callee));
        }

        var baseStack = call.Arguments[..^1];
        var baseDepth = baseStack.Length;

        var nextValue =
            Blocks
            .SelectMany(
                block =>
                block.Parameters
                .Concat(block.Operations.SelectMany(operation => operation.Inputs.Concat(operation.Results)))
                .Concat(Successors(block.Terminator).SelectMany(edge => edge.Arguments))
                .Concat(block.Terminator is PineControlFlowTerminator.Invoke invoke ? invoke.Inputs : []))
            .Select(value => value.Value)
            .DefaultIfEmpty(-1)
            .Max() + 1;

        var localOffset =
            Blocks
            .SelectMany(block => block.Operations.Select(operation => operation.Instruction))
            .Where(instruction => IsLocalInstruction(instruction.Kind))
            .SelectMany(instruction => instruction.LocalIndices)
            .DefaultIfEmpty(callerParameterCount - 1)
            .Max() + 1;

        var valueOffset = nextValue;

        nextValue +=
            callee.Blocks
            .SelectMany(
                block =>
                block.Parameters
                .Concat(block.Operations.SelectMany(operation => operation.Inputs.Concat(operation.Results)))
                .Concat(Successors(block.Terminator).SelectMany(edge => edge.Arguments))
                .Concat(block.Terminator is PineControlFlowTerminator.Invoke invoke ? invoke.Inputs : []))
            .Select(value => value.Value)
            .DefaultIfEmpty(-1)
            .Max() + 1;

        var localBase = Math.Max(localOffset, callerParameterCount);
        var oldToNew = new Dictionary<PineBlockId, PineBlockId>();
        var inserted = ImmutableArray.CreateBuilder<PineBasicBlock>();

        for (var index = 0; index < callee.Blocks.Length; index++)
        {
            oldToNew.Add(callee.Blocks[index].Id, new PineBlockId(Blocks.Length + index));
        }

        PineVirtualValueId RemapValue(PineVirtualValueId value) => new(value.Value + valueOffset);

        ImmutableArray<PineVirtualValueId> RemapValues(ImmutableArray<PineVirtualValueId> values) =>
            [.. values.Select(RemapValue)];

        foreach (var block in callee.Blocks)
        {
            var prefix =
                Enumerable.Range(0, baseDepth)
                .Select(_ => new PineVirtualValueId(nextValue++))
                .ToImmutableArray();

            ImmutableArray<PineVirtualValueId> EdgeArguments(ImmutableArray<PineVirtualValueId> arguments) =>
                [.. prefix, .. RemapValues(arguments)];

            var operations =
                block.Operations
                .Select(
                    operation =>
                    operation with
                    {
                        Instruction = ShiftLocal(operation.Instruction, localBase),
                        Inputs = RemapValues(operation.Inputs),
                        Results = RemapValues(operation.Results)
                    })
                .ToImmutableArray();

            PineControlFlowTerminator terminator;

            switch (block.Terminator)
            {
                case PineControlFlowTerminator.Return:
                    var stack = block.Parameters.ToList();

                    foreach (var operation in block.Operations)
                    {
                        var popCount = StackInstruction.GetDetails(operation.Instruction).PopCount;
                        stack.RemoveRange(stack.Count - popCount, popCount);
                        stack.AddRange(operation.Results);
                    }

                    if (stack.Count is not 1)
                    {
                        throw new InvalidOperationException("Inlined callee must return one value.");
                    }

                    terminator =
                        new PineControlFlowTerminator.Jump(
                            call.Continuation,
                            [.. prefix, RemapValue(stack[0])],
                            IsFallThrough: false);

                    break;

                case PineControlFlowTerminator.Jump jump:
                    terminator =
                        jump with
                        {
                            Target = oldToNew[jump.Target],
                            Arguments = EdgeArguments(jump.Arguments)
                        };

                    break;

                case PineControlFlowTerminator.ConditionalJump conditional:
                    terminator =
                        conditional with
                        {
                            FallThrough = oldToNew[conditional.FallThrough],
                            Branch = oldToNew[conditional.Branch],
                            FallThroughArguments = EdgeArguments(conditional.FallThroughArguments),
                            BranchArguments = EdgeArguments(conditional.BranchArguments)
                        };

                    break;

                case PineControlFlowTerminator.Switch switchTerminator:
                    terminator =
                        switchTerminator with
                        {
                            FallThrough = oldToNew[switchTerminator.FallThrough],
                            Cases =
                            [
                            .. switchTerminator.Cases.Select(
                                switchCase => switchCase with { Target = oldToNew[switchCase.Target] })
                            ],
                            Arguments = EdgeArguments(switchTerminator.Arguments)
                        };

                    break;

                case PineControlFlowTerminator.Invoke invoke:
                    terminator =
                        invoke with
                        {
                            Continuation = oldToNew[invoke.Continuation],
                            Arguments = EdgeArguments(invoke.Arguments),
                            Inputs = RemapValues(invoke.Inputs)
                        };

                    break;

                case PineControlFlowTerminator.TailInvoke:
                    throw new InvalidOperationException("A tail invocation cannot be inlined.");

                default:
                    throw new NotImplementedException(
                        "InlineInvocation does not handle terminator variant: " + block.Terminator.GetType().Name);
            }

            inserted.Add(
                new PineBasicBlock(
                    oldToNew[block.Id],
                    [.. prefix, .. RemapValues(block.Parameters)],
                    operations,
                    terminator));
        }

        var setup = callBlock.Operations.ToBuilder();

        if (calleeParameterCount > 0)
        {
            setup.Add(
                new PineControlFlowOperation(
                    StackInstruction.Local_Set(
                        [.. Enumerable.Range(localBase, calleeParameterCount).Reverse()]),
                    [],
                    []));

            setup.Add(
                new PineControlFlowOperation(
                    StackInstruction.PopMultiple(calleeParameterCount),
                    call.Inputs,
                    []));
        }

        var rewrittenCall =
            callBlock with
            {
                Operations = setup.ToImmutable(),
                Terminator =
                new PineControlFlowTerminator.Jump(
                    oldToNew[callee.Entry],
                    baseStack,
                    IsFallThrough: true)
            };

        var ordered = ImmutableArray.CreateBuilder<PineBasicBlock>(Blocks.Length + inserted.Count);

        foreach (var block in Blocks)
        {
            ordered.Add(block.Id == invocationBlock ? rewrittenCall : block);

            if (block.Id == invocationBlock)
            {
                ordered.AddRange(inserted);
            }
        }

        var result =
            TryRemapBlockIds(ordered.ToImmutable()) ??
            throw new InvalidOperationException("Inlining removed the entry block.");

        result.Validate();
        return result;
    }

    private static bool IsLocalInstruction(StackInstructionKind kind) =>
        kind is
        StackInstructionKind.Local_Get or
        StackInstructionKind.Local_Get_Skip_Head_Const or
        StackInstructionKind.Local_Set or
        StackInstructionKind.Local_Set_Literal or
        StackInstructionKind.Local_Int_Add_Const;

    private static StackInstruction ShiftLocal(StackInstruction instruction, int offset) =>
        IsLocalInstruction(instruction.Kind)
        ?
        instruction with { LocalIndices = [.. instruction.LocalIndices.Select(index => index + offset)] }
        :
        instruction;
}
