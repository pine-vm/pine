using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;

using Function = Pine.Core.DotNet.PineCSharpCompiler.Function;

namespace Pine.Core.DotNet;

/// <summary>Final native reachability and scalar-view bindings, after native tail-call selection.</summary>
internal sealed class PineCSharpFunctionPlan
{
    public IReadOnlyList<PineBasicBlock> Blocks { get; }

    public IReadOnlyDictionary<PineBlockId, Function> Calls { get; }

    public HashSet<string> ScalarVariables { get; } = [];

    public bool IsNoOp { get; }

    public bool HasCycles { get; }

    public PineCSharpFunctionPlan(
        Function function,
        IReadOnlySet<PineBlockId> tailCalls,
        Func<int, string> localName,
        Func<PineVirtualValueId, string> valueName)
    {
        var reachable = new HashSet<PineBlockId>();
        var pending = new Stack<PineBlockId>();
        pending.Push(function.Graph.Entry);
        var calls = new Dictionary<PineBlockId, Function>();
        var blocks = new Dictionary<PineBlockId, PineBasicBlock>();

        while (pending.TryPop(out var id))
        {
            if (!reachable.Add(id))
                continue;

            var block = function.Graph.Blocks[id.Value];

            var error =
                Array.FindIndex(
                    block.Operations.ToArray(),
                    operation => operation.Instruction.Kind == StackInstructionKind.Parse_Error_Const);

            if (error >= 0)
            {
                blocks.Add(id, block with { Operations = block.Operations[..(error + 1)] });
                continue;
            }

            blocks.Add(id, FuseLengthPredicates(block));

            if (function.Calls.TryGetValue(id, out var callee))
                calls.Add(id, callee);

            if (tailCalls.Contains(id))
                continue;

            foreach (var target in Successors(block.Terminator))
                pending.Push(target);
        }

        Blocks = blocks.Values.OrderBy(block => block.Id.Value).ToArray();
        Calls = calls;

        var edges =
            Blocks.ToDictionary(
                block => block.Id,
                block =>
                tailCalls.Contains(block.Id) ||
                block.Operations.Any(
                    operation =>
                    operation.Instruction.Kind == StackInstructionKind.Parse_Error_Const)
                ?
                Array.Empty<PineBlockId>()
                :
                Successors(block.Terminator).ToArray());

        var incoming = Blocks.ToDictionary(block => block.Id, _ => 0);

        foreach (var target in edges.Values.SelectMany(targets => targets))
            ++incoming[target];

        var ready = new Queue<PineBlockId>(incoming.Where(item => item.Value == 0).Select(item => item.Key));
        var processed = 0;

        while (ready.TryDequeue(out var id))
        {
            ++processed;

            foreach (var target in edges[id])
            {
                if (--incoming[target] == 0)
                    ready.Enqueue(target);
            }
        }

        HasCycles = processed != Blocks.Count;

        IsNoOp =
            Blocks.Count == 1 && Blocks[0].Operations.IsEmpty &&
            Blocks[0].Terminator is PineControlFlowTerminator.Return { ValueCount: 0 };

        var aliases = new List<(string Left, string Right)>();
        var pineConsumers = new HashSet<string>();

        foreach (var block in Blocks)
        {
            var stack = block.Parameters.ToList();

            foreach (var operation in block.Operations)
            {
                var instruction = operation.Instruction;

                if (IsScalarProducer(instruction.Kind))
                    ScalarVariables.UnionWith(operation.Results.Select(valueName));

                if (instruction.Kind == StackInstructionKind.Local_Int_Add_Const)
                    ScalarVariables.UnionWith(instruction.LocalIndices.Select(localName));

                for (var index = 0; index < operation.Inputs.Length; index++)
                {
                    if (NeedsPineInput(instruction.Kind, index))
                        pineConsumers.Add(valueName(operation.Inputs[index]));
                }

                if (instruction.Kind == StackInstructionKind.Local_Get)
                {
                    aliases.AddRange(
                        operation.Results.Select(
                            (result, index) =>
                            (valueName(result), localName(instruction.LocalIndices[index]))));
                }

                if (instruction.Kind == StackInstructionKind.Local_Set)
                {
                    aliases.AddRange(
                        instruction.LocalIndices.Select(
                            (index, depth) =>
                            (localName(index), valueName(stack[stack.Count - depth - 1]))));
                }

                stack.RemoveRange(stack.Count - operation.Inputs.Length, operation.Inputs.Length);
                stack.AddRange(operation.Results);
            }

            foreach (var (target, arguments) in Edges(block.Terminator))
            {
                aliases.AddRange(
                    function.Graph.Blocks[target.Value].Parameters.Zip(arguments)
                    .Select(pair => (valueName(pair.First), valueName(pair.Second))));
            }

            if (block.Terminator is PineControlFlowTerminator.Return returned)
            {
                foreach (var (value, index) in stack.TakeLast(returned.ValueCount).Select((value, index) => (value, index)))
                {
                    if (function.Prepared.Returns.Slots[index].Values.Length > 0)
                        ScalarVariables.Add(valueName(value));
                }
            }

            if (block.Terminator is PineControlFlowTerminator.Invoke invoke && !tailCalls.Contains(block.Id))
                pineConsumers.UnionWith(invoke.Inputs.Select(valueName));

            if (block.Terminator is PineControlFlowTerminator.ValueMap map)
                pineConsumers.Add(valueName(map.Input));
        }

        var adjacent = new Dictionary<string, List<string>>();

        foreach (var (left, right) in aliases)
        {
            if (!adjacent.TryGetValue(left, out var leftEdges))
                adjacent[left] = leftEdges = [];

            if (!adjacent.TryGetValue(right, out var rightEdges))
                adjacent[right] = rightEdges = [];

            leftEdges.Add(right);
            rightEdges.Add(left);
        }

        Propagate(ScalarVariables);
        Propagate(pineConsumers);
        // Escaping aliases need one shared Pine object, not independently materialized struct copies.
        ScalarVariables.ExceptWith(pineConsumers);

        void Propagate(HashSet<string> values)
        {
            var queue = new Queue<string>(values);

            while (queue.TryDequeue(out var value))
            {
                foreach (var next in adjacent.GetValueOrDefault(value) ?? [])
                {
                    if (values.Add(next))
                        queue.Enqueue(next);
                }
            }
        }
    }

    private static PineBasicBlock FuseLengthPredicates(PineBasicBlock block)
    {
        var operations = new List<PineControlFlowOperation>();

        for (var index = 0; index < block.Operations.Length; index++)
        {
            var operation = block.Operations[index];

            if (operation.Instruction.Kind == StackInstructionKind.Length && index + 1 < block.Operations.Length &&
                block.Operations[index + 1] is { } comparison &&
                comparison.Instruction.Kind is StackInstructionKind.Equal_Binary_Const or StackInstructionKind.Not_Equal_Binary_Const &&
                comparison.Inputs.SequenceEqual(operation.Results) &&
                IntegerEncoding.ParseSignedIntegerStrict(comparison.Instruction.Literal!.Evaluate()).IsOkOrNullable() is { } length)
            {
                var negate = comparison.Instruction.Kind == StackInstructionKind.Not_Equal_Binary_Const;

                operations.Add(
                    operation with
                    {
                        Instruction = StackInstruction.Length_Equal_Const(length),
                        Results = negate ? operation.Results : comparison.Results
                    });

                if (negate)
                    operations.Add(comparison with { Instruction = StackInstruction.Negate });

                ++index;
            }
            else
                operations.Add(operation);
        }

        var terminator = block.Terminator;

        if (operations.LastOrDefault() is { Instruction.Kind: StackInstructionKind.Length } last)
        {
            var literal =
                terminator switch
                {
                    PineControlFlowTerminator.ConditionalJump branch => branch.Literal,

                    PineControlFlowTerminator.Switch { Kind: PineSwitchKind.Equal, Cases.Length: 1 } choice =>
                    choice.Cases[0].Literal,

                    PineControlFlowTerminator.Return or PineControlFlowTerminator.Jump or
                    PineControlFlowTerminator.Switch or PineControlFlowTerminator.Invoke or
                    PineControlFlowTerminator.ValueMap or PineControlFlowTerminator.TailInvoke =>
                    null,

                    _ =>
                    throw new NotImplementedException(
                        nameof(FuseLengthPredicates) + " does not handle terminator variant: " +
                        terminator.GetType().Name)
                };

            if (literal is not null &&
                IntegerEncoding.ParseSignedIntegerStrict(literal).IsOkOrNullable() is { } length)
            {
                operations[^1] = last with { Instruction = StackInstruction.Length_Equal_Const(length) };

                terminator =
                    terminator switch
                    {
                        PineControlFlowTerminator.ConditionalJump branch =>
                        branch with { Literal = PineKernelValues.TrueValue },

                        PineControlFlowTerminator.Switch choice =>
                        new PineControlFlowTerminator.ConditionalJump(
                            choice.FallThrough,
                            choice.Cases[0].Target,
                            choice.Arguments,
                            choice.Arguments,
                            PineKernelValues.TrueValue),

                        PineControlFlowTerminator.Return or PineControlFlowTerminator.Jump or
                        PineControlFlowTerminator.Invoke or PineControlFlowTerminator.ValueMap or
                        PineControlFlowTerminator.TailInvoke =>
                        throw new InvalidOperationException("Expected a length comparison."),

                        _ =>
                        throw new NotImplementedException(
                            nameof(FuseLengthPredicates) + " does not handle terminator variant: " +
                            terminator.GetType().Name)
                    };
            }
        }

        return block with { Operations = [.. operations], Terminator = terminator };
    }

    private static bool NeedsPineInput(StackInstructionKind kind, int index) =>
        kind switch
        {
            StackInstructionKind.Local_Set or StackInstructionKind.Pop => false,

            StackInstructionKind.Int_Add_Binary or StackInstructionKind.Int_Add_Const or
            StackInstructionKind.Int_Mul_Binary or StackInstructionKind.Int_Mul_Const or
            StackInstructionKind.Int_Mul_Const_Add_Binary or
            StackInstructionKind.Int_Less_Than_Binary or StackInstructionKind.Int_Less_Than_Or_Equal_Binary or
            StackInstructionKind.Int_Less_Than_Const or StackInstructionKind.Int_Less_Than_Or_Equal_Const or
            StackInstructionKind.Int_Greater_Than_Or_Equal_Const =>
            false,

            StackInstructionKind.Skip_Binary or StackInstructionKind.Take_Binary or StackInstructionKind.Skip_Head_Binary or
            StackInstructionKind.Slice_Skip_Var_Take_Const or StackInstructionKind.Slice_Skip_Var_Take_Var or
            StackInstructionKind.Slice_Skip_Var_Equal_Const =>
            index == 0,

            _ =>
            true
        };

    private static bool IsScalarProducer(StackInstructionKind kind) =>
        kind is
        StackInstructionKind.Length or StackInstructionKind.Length_Equal_Const or
        StackInstructionKind.Int_Add_Binary or StackInstructionKind.Int_Add_Const or
        StackInstructionKind.Int_Mul_Binary or StackInstructionKind.Int_Mul_Const or
        StackInstructionKind.Int_Mul_Const_Add_Binary or
        StackInstructionKind.Int_Less_Than_Binary or StackInstructionKind.Int_Less_Than_Or_Equal_Binary or
        StackInstructionKind.Int_Less_Than_Const or StackInstructionKind.Int_Less_Than_Or_Equal_Const or
        StackInstructionKind.Int_Greater_Than_Or_Equal_Const or
        StackInstructionKind.Equal_Binary or StackInstructionKind.Not_Equal_Binary or
        StackInstructionKind.Equal_Binary_Const or StackInstructionKind.Not_Equal_Binary_Const or
        StackInstructionKind.Is_List_Value or StackInstructionKind.Is_Blob_Value or
        StackInstructionKind.Logical_And_Binary;

    public static IEnumerable<PineBlockId> Successors(PineControlFlowTerminator terminator) =>
        Edges(terminator).Select(edge => edge.Target).Distinct();

    private static IEnumerable<(PineBlockId Target, IReadOnlyList<PineVirtualValueId> Arguments)> Edges(
        PineControlFlowTerminator terminator)
    {
        switch (terminator)
        {
            case PineControlFlowTerminator.Jump jump:
                yield return (jump.Target, jump.Arguments);
                break;

            case PineControlFlowTerminator.ConditionalJump branch:
                yield return (branch.FallThrough, branch.FallThroughArguments);
                yield return (branch.Branch, branch.BranchArguments);
                break;

            case PineControlFlowTerminator.Switch choice:
                yield return (choice.FallThrough, choice.Arguments);

                foreach (var item in choice.Cases)
                    yield return (item.Target, choice.Arguments);

                break;

            case PineControlFlowTerminator.Invoke invoke:
                yield return (invoke.Continuation, invoke.Arguments);
                break;

            case PineControlFlowTerminator.ValueMap map:
                yield return (map.FallThrough, [.. map.Arguments, map.Result]);
                yield return (map.Fallback, map.Arguments);
                break;

            case PineControlFlowTerminator.Return:
            case PineControlFlowTerminator.TailInvoke:
                break;

            default:
                throw new NotImplementedException("Edges does not handle: " + terminator.GetType().Name);
        }
    }
}
