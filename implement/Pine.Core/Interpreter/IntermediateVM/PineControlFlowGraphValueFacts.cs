using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    private abstract record ValueConstraint
    {
        public sealed record Exact(PineValue Value) : ValueConstraint;

        public sealed record ListLength(int Length) : ValueConstraint;

        public sealed record ListElement(int Index, PineValue Value) : ValueConstraint;
    }

    private sealed record ValueFacts(ImmutableHashSet<ValueConstraint> Constraints)
    {
        public static ValueFacts Unknown { get; } = new([]);

        public static ValueFacts ForLiteral(PineValue value) =>
            new(ImmutableHashSet<ValueConstraint>.Empty.Add(new ValueConstraint.Exact(value)));

        public static ValueFacts ForConstructedList(
            PineValue.ListValue prefix,
            ImmutableArray<PineVirtualValueId> inputs,
            IReadOnlyDictionary<PineVirtualValueId, ValueFacts> values)
        {
            var constraints =
                ImmutableHashSet<ValueConstraint>.Empty
                .Add(new ValueConstraint.ListLength(prefix.Items.Length + inputs.Length));

            for (var index = 0; index < prefix.Items.Length; index++)
            {
                constraints = constraints.Add(new ValueConstraint.ListElement(index, prefix.Items.Span[index]));
            }

            for (var index = 0; index < inputs.Length; index++)
            {
                if (values.GetValueOrDefault(inputs[index], Unknown).ExactValue is { } item)
                {
                    constraints =
                        constraints.Add(new ValueConstraint.ListElement(prefix.Items.Length + index, item));
                }
            }

            return new(constraints);
        }

        public PineValue? ExactValue =>
            Constraints.OfType<ValueConstraint.Exact>().FirstOrDefault()?.Value;

        public int? KnownListLength =>
            ExactValue is PineValue.ListValue list
            ?
            list.Items.Length
            :
            Constraints.OfType<ValueConstraint.ListLength>().FirstOrDefault()?.Length;

        public PineValue? KnownListElement(int index)
        {
            if (ExactValue is { } exact)
            {
                return PineValueInProcess.Create(exact).GetElementAt(index).Evaluate();
            }

            if (KnownListLength is { } length && index >= length)
            {
                return PineValue.EmptyList;
            }

            return
                Constraints.OfType<ValueConstraint.ListElement>()
                .FirstOrDefault(element => element.Index == index)?.Value;
        }

        public bool? EqualsLiteral(PineValue literal)
        {
            if (ExactValue is { } exact)
            {
                return
                    PineValueInProcess.AreEqual(
                        PineValueInProcess.Create(exact),
                        PineValueInProcess.Create(literal));
            }

            if (KnownListLength is not { } length)
            {
                return null;
            }

            if (literal is not PineValue.ListValue list || list.Items.Length != length)
            {
                return false;
            }

            foreach (var element in Constraints.OfType<ValueConstraint.ListElement>())
            {
                if (!element.Value.Equals(list.Items.Span[element.Index]))
                {
                    return false;
                }
            }

            return
                length is 0 ||
                Constraints.OfType<ValueConstraint.ListElement>().Count() == length
                ?
                true
                :
                null;
        }

        public ValueFacts Meet(ValueFacts other)
        {
            var common = Constraints.Intersect(other.Constraints);

            if (KnownListLength is not { } length || other.KnownListLength != length)
            {
                return new(common);
            }

            common = common.Add(new ValueConstraint.ListLength(length));

            if (ExactValue is PineValue.ListValue list)
            {
                foreach (var element in other.Constraints.OfType<ValueConstraint.ListElement>())
                {
                    if (list.Items.Span[element.Index].Equals(element.Value))
                    {
                        common = common.Add(element);
                    }
                }
            }

            if (other.ExactValue is PineValue.ListValue otherList)
            {
                foreach (var element in Constraints.OfType<ValueConstraint.ListElement>())
                {
                    if (otherList.Items.Span[element.Index].Equals(element.Value))
                    {
                        common = common.Add(element);
                    }
                }
            }

            return new(common);
        }

        public bool SameFacts(ValueFacts other) =>
            Constraints.SetEquals(other.Constraints);
    }

    private sealed record ValueFlowState(
        ImmutableArray<ValueFacts> Parameters,
        Dictionary<int, ValueFacts> Locals);

    /// <summary>
    /// Propagates proven values through stack parameters and local slots, including loop edges,
    /// then removes equality branches whose outcome is known on every incoming path.
    /// </summary>
    public PineControlFlowGraph ForwardProvenEqualityBranches()
    {
        Validate();

        var states = new ValueFlowState?[Blocks.Length];
        states[Entry.Value] = new ValueFlowState([], []);

        var pending = new Queue<PineBlockId>();
        var queued = new HashSet<PineBlockId> { Entry };
        pending.Enqueue(Entry);

        while (pending.TryDequeue(out var id))
        {
            queued.Remove(id);
            var block = Blocks[id.Value];
            var (values, locals, _) = TraceValueFacts(block, states[id.Value]!);

            foreach (var (target, arguments) in Successors(block.Terminator))
            {
                var incoming =
                    arguments.Select(argument => values.GetValueOrDefault(argument, ValueFacts.Unknown))
                    .ToImmutableArray();

                if (states[target.Value] is not { } previous)
                {
                    states[target.Value] = new ValueFlowState(incoming, new Dictionary<int, ValueFacts>(locals));

                    if (queued.Add(target))
                    {
                        pending.Enqueue(target);
                    }

                    continue;
                }

                var mergedParameters =
                    previous.Parameters.Zip(incoming, (left, right) => left.Meet(right)).ToImmutableArray();

                var mergedLocals = new Dictionary<int, ValueFacts>();

                foreach (var local in previous.Locals.Keys.Union(locals.Keys))
                {
                    var facts =
                        previous.Locals.GetValueOrDefault(local, ValueFacts.Unknown)
                        .Meet(locals.GetValueOrDefault(local, ValueFacts.Unknown));

                    if (!facts.Constraints.IsEmpty)
                    {
                        mergedLocals.Add(local, facts);
                    }
                }

                if (previous.Parameters.Zip(mergedParameters).All(pair => pair.First.SameFacts(pair.Second)) &&
                    previous.Locals.Count == mergedLocals.Count &&
                    mergedLocals.All(
                        pair => previous.Locals.TryGetValue(pair.Key, out var old) && old.SameFacts(pair.Value)))
                {
                    continue;
                }

                states[target.Value] = new ValueFlowState(mergedParameters, mergedLocals);

                if (queued.Add(target))
                {
                    pending.Enqueue(target);
                }
            }
        }

        var changed = false;

        var blocks =
            Blocks.Select(
                block =>
                {
                    if (states[block.Id.Value] is not { } state)
                    {
                        return block;
                    }

                    var (values, _, stack) = TraceValueFacts(block, state);

                    PineBlockId target;
                    ImmutableArray<PineVirtualValueId> arguments;
                    bool fallThrough;
                    int popCount;

                    switch (block.Terminator)
                    {
                        case PineControlFlowTerminator.ConditionalJump conditional
                        when values.GetValueOrDefault(stack[^1], ValueFacts.Unknown)
                                .EqualsLiteral(conditional.Literal) is { } equal:
                            fallThrough = !equal;
                            target = fallThrough ? conditional.FallThrough : conditional.Branch;

                            arguments =
                                fallThrough ? conditional.FallThroughArguments : conditional.BranchArguments;

                            popCount = 1;
                            break;

                        case PineControlFlowTerminator.Switch { Kind: PineSwitchKind.Equal } switchTerminator
                        when values.GetValueOrDefault(stack[^1], ValueFacts.Unknown).ExactValue is { } selector:
                            var matchingCase =
                                switchTerminator.Cases.FirstOrDefault(
                                    switchCase => selector.Equals(switchCase.Literal));

                            target = matchingCase == default ? switchTerminator.FallThrough : matchingCase.Target;
                            arguments = switchTerminator.Arguments;
                            fallThrough = target == switchTerminator.FallThrough;
                            popCount = 1;
                            break;

                        case PineControlFlowTerminator.Switch { Kind: PineSwitchKind.Equal } switchTerminator
                        when switchTerminator.Cases.Any(
                                switchCase =>
                                    values.GetValueOrDefault(stack[^1], ValueFacts.Unknown)
                                    .EqualsLiteral(switchCase.Literal) is true):
                            var provenCase =
                                switchTerminator.Cases.First(
                                    switchCase =>
                                    values.GetValueOrDefault(stack[^1], ValueFacts.Unknown)
                                        .EqualsLiteral(switchCase.Literal) is true);

                            target = provenCase.Target;
                            arguments = switchTerminator.Arguments;
                            fallThrough = false;
                            popCount = 1;
                            break;

                        case PineControlFlowTerminator.Switch { Kind: PineSwitchKind.Equal } switchTerminator
                        when switchTerminator.Cases.All(
                                switchCase =>
                                    values.GetValueOrDefault(stack[^1], ValueFacts.Unknown)
                                    .EqualsLiteral(switchCase.Literal) is false):
                            target = switchTerminator.FallThrough;
                            arguments = switchTerminator.Arguments;
                            fallThrough = true;
                            popCount = 1;
                            break;

                        case PineControlFlowTerminator.Return:
                        case PineControlFlowTerminator.Jump:
                        case PineControlFlowTerminator.ConditionalJump:
                        case PineControlFlowTerminator.Switch:
                        case PineControlFlowTerminator.Invoke:
                        case PineControlFlowTerminator.TailInvoke:
                            return block;

                        default:
                            throw new NotImplementedException(
                                "ForwardProvenEqualityBranches does not handle terminator variant: " +
                                block.Terminator.GetType().Name);
                    }

                    var operations = block.Operations.ToBuilder();

                    var last =
                        operations.Count > 0 &&
                        operations[^1].Results is { Length: 1 } results &&
                        results[0] == stack[^1]
                        ?
                        operations[^1]
                        :
                        null;

                    var dropLast =
                        last?.Instruction.Kind is
                            StackInstructionKind.Push_Literal or
                            StackInstructionKind.Local_Get or
                            StackInstructionKind.Local_Get_Skip_Head_Const;

                    var replaceLastWithPop =
                        last?.Instruction.Kind is
                            StackInstructionKind.Head_Generic or
                            StackInstructionKind.Skip_Head_Const or
                            StackInstructionKind.Length or
                            StackInstructionKind.Length_Equal_Const;

                    changed = true;

                    // An immediately preceding, pure stack push can be dropped together with
                    // the branch input; otherwise consume the input as the terminator did.
                    if (dropLast)
                    {
                        operations.RemoveAt(operations.Count - 1);
                    }
                    else if (replaceLastWithPop)
                    {
                        operations[^1] =
                            last! with
                            {
                                Instruction = StackInstruction.Pop,
                                Results = []
                            };
                    }
                    else
                    {
                        operations.Add(
                            new PineControlFlowOperation(
                                StackInstruction.PopMultiple(popCount),
                                [.. stack.TakeLast(popCount)],
                                []));
                    }

                    return
                        block with
                        {
                            Operations = operations.ToImmutable(),
                            Terminator = new PineControlFlowTerminator.Jump(target, arguments, fallThrough)
                        };
                })
            .ToImmutableArray();

        if (!changed)
        {
            return this;
        }

        var result = this with { Blocks = blocks };
        result.Validate();
        return result.RemoveUnreachableBlocks();
    }

    private static (
        Dictionary<PineVirtualValueId, ValueFacts> Values,
        Dictionary<int, ValueFacts> Locals,
        List<PineVirtualValueId> Stack)
        TraceValueFacts(PineBasicBlock block, ValueFlowState state)
    {
        var values = new Dictionary<PineVirtualValueId, ValueFacts>();
        var locals = new Dictionary<int, ValueFacts>(state.Locals);
        var stack = block.Parameters.ToList();

        for (var index = 0; index < stack.Count; index++)
        {
            values.Add(stack[index], state.Parameters[index]);
        }

        foreach (var operation in block.Operations)
        {
            var instruction = operation.Instruction;

            if (instruction.Kind is StackInstructionKind.Local_Set &&
                instruction.LocalIndex is { } local)
            {
                locals[local] = values.GetValueOrDefault(stack[^1], ValueFacts.Unknown);
            }
            else if (instruction.Kind is StackInstructionKind.Local_Int_Add_Const &&
                instruction.LocalIndex is { } incrementedLocal)
            {
                locals.Remove(incrementedLocal);
            }
            else if (instruction.Kind is StackInstructionKind.Local_Set_Descending &&
                instruction.LocalIndex is { } highest &&
                instruction.TakeCount is { } count)
            {
                for (var depth = 0; depth < count; depth++)
                {
                    locals[highest - depth] = values.GetValueOrDefault(stack[^(depth + 1)], ValueFacts.Unknown);
                }
            }

            var facts = ValueFacts.Unknown;

            switch (instruction.Kind)
            {
                case StackInstructionKind.Push_Literal when instruction.Literal is { } literal:
                    facts = ValueFacts.ForLiteral(literal.Evaluate());
                    break;

                case StackInstructionKind.Local_Get when instruction.LocalIndex is { } index:
                    facts = locals.GetValueOrDefault(index, ValueFacts.Unknown);
                    break;

                case StackInstructionKind.Build_List when instruction.TakeCount is >= 0:
                    facts =
                        ValueFacts.ForConstructedList(PineValue.EmptyList, operation.Inputs, values);

                    break;

                case StackInstructionKind.Build_List_With_Prefix
                when instruction.TakeCount is >= 0 &&
                    instruction.Literal?.Evaluate() is PineValue.ListValue prefix:
                    facts = ValueFacts.ForConstructedList(prefix, operation.Inputs, values);
                    break;

                case StackInstructionKind.Head_Generic or StackInstructionKind.Skip_Head_Const or
                    StackInstructionKind.Local_Get_Skip_Head_Const:
                    var source =
                        instruction.Kind is StackInstructionKind.Local_Get_Skip_Head_Const
                        ?
                        locals.GetValueOrDefault(instruction.LocalIndex!.Value, ValueFacts.Unknown)
                        :
                        values.GetValueOrDefault(operation.Inputs[0], ValueFacts.Unknown);

                    if (source.KnownListElement(Math.Max(0, instruction.SkipCount ?? 0)) is { } known)
                    {
                        facts = ValueFacts.ForLiteral(known);
                    }

                    break;

                case StackInstructionKind.Length or StackInstructionKind.Length_Equal_Const:
                    var listFacts = values.GetValueOrDefault(operation.Inputs[0], ValueFacts.Unknown);

                    if (listFacts.KnownListLength is { } length)
                    {
                        facts =
                            ValueFacts.ForLiteral(
                                instruction.Kind is StackInstructionKind.Length
                                ?
                                PineValueInProcess.CreateInteger(length).Evaluate()
                                :
                                PineValueInProcess.CreateBool(length == instruction.IntegerLiteral).Evaluate());
                    }

                    break;
            }

            foreach (var result in operation.Results)
            {
                values.Add(result, facts);
            }

            var popCount = StackInstruction.GetDetails(instruction).PopCount;
            stack.RemoveRange(stack.Count - popCount, popCount);
            stack.AddRange(operation.Results);
        }

        return (values, locals, stack);
    }
}
