using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.IntermediateVM;

public sealed partial record PineControlFlowGraph
{
    private readonly record struct ValueFacts(
        PineValue? ExactValue,
        int? ExplicitLength,
        ImmutableArray<KeyValuePair<int, PineValue>> Elements)
    {
        public static ValueFacts Unknown => new(null, null, []);

        public bool IsUnknown => ExactValue is null && ExplicitLength is null && Elements.IsDefaultOrEmpty;

        public static ValueFacts ForLiteral(PineValue value) =>
            new(value, null, []);

        public static ValueFacts ForConstructedList(
            PineValue.ListValue prefix,
            ImmutableArray<PineVirtualValueId> inputs,
            IReadOnlyDictionary<PineVirtualValueId, ValueFacts> values)
        {
            var elements = ImmutableArray.CreateBuilder<KeyValuePair<int, PineValue>>();

            for (var index = 0; index < prefix.Items.Length; index++)
            {
                elements.Add(new(index, prefix.Items.Span[index]));
            }

            for (var index = 0; index < inputs.Length; index++)
            {
                if (values.GetValueOrDefault(inputs[index], Unknown).ExactValue is { } item)
                {
                    elements.Add(new(prefix.Items.Length + index, item));
                }
            }

            return new(null, prefix.Items.Length + inputs.Length, elements.ToImmutable());
        }

        public int? KnownListLength =>
            ExactValue is PineValue.ListValue list
            ?
            list.Items.Length
            :
            ExplicitLength;

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

            var low = 0;
            var high = Elements.Length - 1;

            while (low <= high)
            {
                var middle = low + (high - low) / 2;
                var element = Elements[middle];

                if (element.Key == index)
                    return element.Value;

                if (element.Key < index)
                    low = middle + 1;

                else
                    high = middle - 1;
            }

            return null;
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

            foreach (var element in Elements)
            {
                if (!element.Value.Equals(list.Items.Span[element.Key]))
                {
                    return false;
                }
            }

            return
                length is 0 ||
                Elements.Length == length
                ?
                true
                :
                null;
        }

        public ValueFacts Meet(ValueFacts other)
        {
            if (IsUnknown || other.IsUnknown)
                return Unknown;

            var exact = ExactValue is { } value && value.Equals(other.ExactValue) ? value : null;

            var length =
                KnownListLength is { } knownLength && other.KnownListLength == knownLength
                ?
                knownLength
                :
                ExplicitLength is { } explicitLength && other.ExplicitLength == explicitLength
                ?
                (int?)explicitLength
                :
                null;

            ImmutableArray<KeyValuePair<int, PineValue>>.Builder? common = null;
            var left = 0;
            var right = 0;

            // Only explicitly retained elements participate, including their agreement with an exact list.
            // Expanding both exact lists here would introduce proofs the previous meet did not retain.
            while (left < Elements.Length || right < other.Elements.Length)
            {
                var takeLeft =
                    right == other.Elements.Length ||
                    left < Elements.Length && Elements[left].Key <= other.Elements[right].Key;

                var element = takeLeft ? Elements[left++] : other.Elements[right++];
                var sameIndex = takeLeft && right < other.Elements.Length && element.Key == other.Elements[right].Key;

                var matches =
                    sameIndex && element.Value.Equals(other.Elements[right].Value) ||
                    length is not null &&
                    (takeLeft
                    ?
                    other.ExactValue is PineValue.ListValue otherList &&
                    otherList.Items.Span[element.Key].Equals(element.Value)
                    :
                    ExactValue is PineValue.ListValue list &&
                    list.Items.Span[element.Key].Equals(element.Value));

                if (sameIndex)
                {
                    if (length is not null && ExactValue is PineValue.ListValue exactList &&
                        exactList.Items.Span[element.Key].Equals(other.Elements[right].Value))
                    {
                        matches = true;
                        element = other.Elements[right];
                    }

                    right++;
                }

                if (matches)
                    (common ??= ImmutableArray.CreateBuilder<KeyValuePair<int, PineValue>>()).Add(element);
            }

            return new(exact, length, common?.ToImmutable() ?? []);
        }

        public bool SameFacts(ValueFacts other)
        {
            if (!Equals(ExactValue, other.ExactValue) || ExplicitLength != other.ExplicitLength ||
                Elements.Length != other.Elements.Length)
            {
                return false;
            }

            for (var index = 0; index < Elements.Length; index++)
            {
                if (Elements[index].Key != other.Elements[index].Key ||
                    !Elements[index].Value.Equals(other.Elements[index].Value))
                {
                    return false;
                }
            }

            return true;
        }
    }

    private sealed record ValueFlowState(
        ImmutableArray<ValueFacts> Parameters,
        Dictionary<int, ValueFacts> Locals);

    /// <summary>
    /// Propagates proven values through stack parameters and local slots, including loop edges,
    /// then removes equality branches whose outcome is known on every incoming path.
    /// </summary>
    public PineControlFlowGraph ForwardProvenEqualityBranches()
        => ForwardProvenEqualityBranches(null);

    private PineControlFlowGraph ForwardProvenEqualityBranches(OptimizationContext? context)
    {
        Validate(context);

        return ForwardProvenEqualityBranches(context?.GetValueFlow(this) ?? AnalyzeValueFlow(), context);
    }

    private ValueFlowState?[] AnalyzeValueFlow()
    {
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

                    if (!facts.IsUnknown)
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

        return states;
    }

    /// <summary>
    /// Uses the same stack and local value facts as branch forwarding to evaluate projections
    /// of known values and replace initialized local reads with their proven literals.
    /// </summary>
    public PineControlFlowGraph FoldKnownLiteralValues(int parameterCount)
        => FoldKnownLiteralValues(parameterCount, null);

    private PineControlFlowGraph FoldKnownLiteralValues(int parameterCount, OptimizationContext? context)
    {
        Validate(context);

        var states = context?.GetValueFlow(this) ?? AnalyzeValueFlow();
        var initializedAtEntry = DefinitelyInitializedLocals(parameterCount, context);
        var changed = false;

        var blocks =
            RewriteBlocks(
                block =>
                {
                    if (states[block.Id.Value] is not { } state)
                    {
                        return block;
                    }

                    var initialized = initializedAtEntry[block.Id.Value]?.Clone() ?? default;
                    var operations = new OperationBuilder(block.Operations);

                    TraceValueFacts(
                        block,
                        state,
                        (operation, values, locals) =>
                        {
                            var instruction = operation.Instruction;
                            PineValue? literal = null;

                            if (instruction.Kind is StackInstructionKind.Local_Get &&
                                instruction.LocalIndices.Length is 1 &&
                                instruction.SingleLocalIndex is { } local &&
                                initialized.Contains(local))
                            {
                                literal = locals.GetValueOrDefault(local, ValueFacts.Unknown).ExactValue;
                            }
                            else if (instruction.Kind is StackInstructionKind.Local_Get_Skip_Head_Const &&
                                instruction.LocalIndices.Length is 1 &&
                                instruction.SingleLocalIndex is { } projectedLocal &&
                                initialized.Contains(projectedLocal) &&
                                instruction.SkipCount is { } skip)
                            {
                                literal =
                                    locals.GetValueOrDefault(projectedLocal, ValueFacts.Unknown)
                                    .KnownListElement(Math.Max(0, skip));
                            }
                            else if (instruction.Kind is StackInstructionKind.Select_Value_Const &&
                                operation.Inputs is [var mapInput] &&
                                KnownValueMapResult(
                                    instruction,
                                    values.GetValueOrDefault(mapInput, ValueFacts.Unknown).ExactValue) is { } selected)
                            {
                                literal = selected;

                                if (operations.Count > 0 &&
                                    operations[^1] is { } previous &&
                                    previous.Instruction.Kind is StackInstructionKind.Push_Literal &&
                                    previous.Results is [var pushed] && pushed == mapInput)
                                {
                                    operations.RemoveAt(operations.Count - 1);
                                }
                                else
                                {
                                    operations.Add(
                                        new PineControlFlowOperation(StackInstruction.Pop, [mapInput], []));
                                }
                            }
                            else if (instruction.Kind is StackInstructionKind.Head_Generic ||
                                instruction.Kind is StackInstructionKind.Skip_Head_Const &&
                                instruction.SkipCount is not null)
                            {
                                if (operation.Inputs.Length is 1 &&
                                    operations.Count > 0 &&
                                    operations[^1] is { } previous &&
                                    previous.Instruction.Kind is StackInstructionKind.Push_Literal &&
                                    previous.Results.Length is 1 &&
                                    previous.Results[0] == operation.Inputs[0])
                                {
                                    literal =
                                        values.GetValueOrDefault(operation.Inputs[0], ValueFacts.Unknown)
                                        .KnownListElement(Math.Max(0, instruction.SkipCount ?? 0));

                                    if (literal is not null)
                                    {
                                        operations.RemoveAt(operations.Count - 1);
                                    }
                                }
                            }

                            if (literal is not null)
                            {
                                operations.Add(
                                    operation with
                                    {
                                        Instruction = StackInstruction.Push_Literal(literal),
                                        Inputs = []
                                    });

                                changed = true;
                            }
                            else
                            {
                                operations.Add(operation);
                            }

                            AddWrittenLocals(instruction, ref initialized);
                        });

                    return operations.Build(block);
                });

        if (!changed)
        {
            return this;
        }

        var result = this with { Blocks = blocks };
        result.Validate(context);
        return result;
    }

    private PineControlFlowGraph ForwardProvenEqualityBranches(ValueFlowState?[] states, OptimizationContext? context)
    {
        var changed = false;

        var blocks =
            RewriteBlocks(
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
                        case PineControlFlowTerminator.ValueMap:
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
                });

        if (!changed)
        {
            return this;
        }

        var result = this with { Blocks = blocks };
        result.Validate(context);
        return result.RemoveUnreachableBlocks(context);
    }

    private static PineValue? KnownValueMapResult(StackInstruction instruction, PineValue? key) =>
        instruction.Kind is StackInstructionKind.Select_Value_Const &&
        instruction.ValueMap is { } map && instruction.Literal is { } fallback &&
        instruction.JumpOffset is null && key is not null
        ?
        map.TryGetValue(key, out var selected) ? selected : fallback.Evaluate()
        :
        null;

    private static (
        Dictionary<PineVirtualValueId, ValueFacts> Values,
        Dictionary<int, ValueFacts> Locals,
        List<PineVirtualValueId> Stack)
        TraceValueFacts(
        PineBasicBlock block,
        ValueFlowState state,
        Action<PineControlFlowOperation, IReadOnlyDictionary<PineVirtualValueId, ValueFacts>,
                IReadOnlyDictionary<int, ValueFacts>>? beforeOperation = null)
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

            beforeOperation?.Invoke(operation, values, locals);

            if (instruction.Kind is StackInstructionKind.Local_Set)
            {
                for (var depth = 0; depth < instruction.LocalIndices.Length; depth++)
                {
                    locals[instruction.LocalIndices[depth]] =
                        values.GetValueOrDefault(stack[^(depth + 1)], ValueFacts.Unknown);
                }
            }
            else if (instruction.Kind is StackInstructionKind.Local_Set_Literal &&
                instruction.SingleLocalIndex is { } literalLocal &&
                instruction.Literal is { } localLiteral)
            {
                locals[literalLocal] = ValueFacts.ForLiteral(localLiteral.Evaluate());
            }
            else if (instruction.Kind is StackInstructionKind.Local_Int_Add_Const)
            {
                foreach (var local in instruction.LocalIndices)
                    locals.Remove(local);
            }

            var facts = ValueFacts.Unknown;

            switch (instruction.Kind)
            {
                case StackInstructionKind.Push_Literal when instruction.Literal is { } literal:
                    facts = ValueFacts.ForLiteral(literal.Evaluate());
                    break;

                case StackInstructionKind.Local_Get when instruction.LocalIndices.Length is 1:
                    var index = instruction.SingleLocalIndex;
                    facts = locals.GetValueOrDefault(index, ValueFacts.Unknown);
                    break;

                case StackInstructionKind.Select_Value_Const when operation.Inputs is [var mapInput]:
                    if (KnownValueMapResult(
                        instruction,
                        values.GetValueOrDefault(mapInput, ValueFacts.Unknown).ExactValue) is { } selected)
                    {
                        facts = ValueFacts.ForLiteral(selected);
                    }

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
                    StackInstructionKind.Local_Get_Skip_Head_Const
                when instruction.Kind is not StackInstructionKind.Local_Get_Skip_Head_Const ||
                        instruction.LocalIndices.Length is 1:
                    var source =
                        instruction.Kind is StackInstructionKind.Local_Get_Skip_Head_Const
                        ?
                        locals.GetValueOrDefault(instruction.SingleLocalIndex, ValueFacts.Unknown)
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

            var popCount = StackInstruction.GetStackEffect(instruction).PopCount;
            stack.RemoveRange(stack.Count - popCount, popCount);
            stack.AddRange(operation.Results);
        }

        return (values, locals, stack);
    }
}
