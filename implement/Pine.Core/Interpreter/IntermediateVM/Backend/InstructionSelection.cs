using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Shared operation selection and stack effects for already-known straight-line fragments.</summary>
internal static class InstructionSelection
{
    internal sealed record SliceSwitch(
        Operation.Builtin Skip, Operation.Builtin Take, PineVirtualValueId Source, PineVirtualValueId Count);

    internal static SliceSwitch? SelectSliceSwitch(BasicBlock block)
    {
        if (block.Terminator is not Terminator.Switch selection || selection.Cases.Count == 0)
            return null;
        var definitions = block.Operations.ToImmutableDictionary(operation => Result(operation).Id);
        var uses = block.Operations.SelectMany(Operands).Concat(TerminatorOperands(block.Terminator))
            .GroupBy(value => value).ToImmutableDictionary(group => group.Key, group => group.Count());
        if (!definitions.TryGetValue(selection.Selector, out var selector) || selector is not Operation.Builtin { Name: "take" } take ||
            uses[take.Result.Id] != 1 ||
            !definitions.TryGetValue(take.Argument, out var takeInput) || takeInput is not Operation.MakeList { Items.Count: 2 } takeArgs ||
            uses[take.Argument] != 1 ||
            !definitions.TryGetValue(takeArgs.Items[0], out var countInput) || countInput is not Operation.Literal count ||
            !definitions.TryGetValue(takeArgs.Items[1], out var skipInput) || skipInput is not Operation.Builtin { Name: "skip" } skip ||
            uses[skip.Result.Id] != 1 ||
            !definitions.TryGetValue(skip.Argument, out var arguments) || arguments is not Operation.MakeList { Items.Count: 2 } skipArgs)
            return null;
        var length = BuiltinFunction.SignedIntegerFromValueRelaxed(OwnedValue(count.Value));
        return length > 0 && selection.Cases.All(@case => @case.Value switch
        {
            LiteralValue.Blob blob => blob.Bytes.Count == length,
            LiteralValue.List list => list.Items.Count == length,
            _ => throw new NotImplementedException("SelectSliceSwitch does not handle literal variant: " + @case.Value.GetType().Name),
        }) ? new(skip, take, skipArgs.Items[1], skipArgs.Items[0]) : null;

        static PineValue OwnedValue(LiteralValue value) => StraightLineVMAdapter.ToPineValue(value);
    }

    private sealed record SliceComparison(
        Operation.Builtin Skip, Operation.Builtin Take, Operation.Builtin Equal,
        PineVirtualValueId Source, PineVirtualValueId Count, LiteralValue Literal);
    internal static ValueDefinition Result(Operation operation) =>
        operation switch
        {
            Operation.Literal literal => literal.Result,
            Operation.MakeList list => list.Result,
            Operation.Project project => project.Result,
            Operation.Builtin builtin => builtin.Result,
            _ => throw new NotImplementedException(
                "Result does not handle operation variant: " + operation.GetType().Name),
        };

    internal static ImmutableList<SelectedInstruction> Project(EnvironmentPath path) =>
        [.. path.Indices.Select(index => (SelectedInstruction)new SelectedInstruction.Project(index))];

    internal static ImmutableList<SelectedInstruction> Store(int local) =>
        [new SelectedInstruction.Store(local), new SelectedInstruction.Pop()];

    internal static ImmutableList<SelectedInstruction> Select(
        Operation operation, ImmutableDictionary<PineVirtualValueId, int> locals) =>
        Compute(operation, locals).AddRange(Store(locals[Result(operation).Id]));

    internal static ImmutableList<SelectedInstruction> SelectBlock(
        BasicBlock block, ImmutableDictionary<PineVirtualValueId, int> locals, bool compact = false)
    {
        return Run();

        ImmutableList<SelectedInstruction> Run()
        {
            var lists = block.Operations.OfType<Operation.MakeList>().ToImmutableDictionary(list => list.Result.Id);
            var definitions = block.Operations.ToImmutableDictionary(operation => Result(operation).Id);
            var uses = block.Operations.SelectMany(Operands).Concat(TerminatorOperands(block.Terminator))
                .GroupBy(value => value).ToImmutableDictionary(group => group.Key, group => group.Count());
            var comparisons = block.Operations.OfType<Operation.Builtin>().Select(Comparison).OfType<SliceComparison>()
                .ToImmutableDictionary(comparison => comparison.Skip.Result.Id);
            var sliceSwitch = compact ? SelectSliceSwitch(block) : null;
            var removed = comparisons.Values.SelectMany(comparison =>
                ImmutableList.Create(comparison.Take.Result.Id, comparison.Equal.Result.Id)).ToImmutableHashSet();
            if (sliceSwitch is not null)
                removed = removed.Add(sliceSwitch.Skip.Result.Id).Add(sliceSwitch.Take.Result.Id);
            var fused = block.Operations.OfType<Operation.Builtin>().Where(builtin =>
                builtin.Name is "equal" or "int_add" or "int_mul" or "skip" or "take" &&
                lists.TryGetValue(builtin.Argument, out var list) && list.Items.Count == 2).ToImmutableHashSet();
            var required = block.Operations.OfType<Operation.Builtin>().Where(builtin => !removed.Contains(builtin.Result.Id))
                .SelectMany(builtin => comparisons.TryGetValue(builtin.Result.Id, out var comparison)
                    ? ImmutableList.Create(comparison.Source, comparison.Count)
                    : fused.Contains(builtin) ? lists[builtin.Argument].Items : [builtin.Argument])
                .Concat(sliceSwitch is null ? TerminatorOperands(block.Terminator) : [sliceSwitch.Source, sliceSwitch.Count]).ToHashSet();
            var pending = new Queue<PineVirtualValueId>(required);
            while (pending.TryDequeue(out var id))
                if (definitions.TryGetValue(id, out var definition) && definition is not Operation.Builtin)
                    foreach (var operand in Operands(definition))
                        if (required.Add(operand))
                            pending.Enqueue(operand);
            var selected = block.Operations.SelectMany(operation =>
                comparisons.TryGetValue(Result(operation).Id, out var comparison) ? CompareSlice(comparison)
                    : removed.Contains(Result(operation).Id) || operation is not Operation.Builtin && !required.Contains(Result(operation).Id)
                    ? [] : operation is Operation.Builtin builtin && fused.Contains(builtin)
                    ? Binary(builtin, lists[builtin.Argument]) : Select(operation, locals)).ToImmutableList();
            return compact ? CompactSelection(selected, block, locals) : selected;

            ImmutableList<SelectedInstruction> CompareSlice(SliceComparison comparison) =>
                ImmutableList.Create<SelectedInstruction>(
                    new SelectedInstruction.Load(locals[comparison.Source]),
                    new SelectedInstruction.Load(locals[comparison.Count]),
                    new SelectedInstruction.Builtin(StackInstructionKind.Slice_Skip_Var_Equal_Const, 2, comparison.Literal))
                    .AddRange(Store(locals[comparison.Equal.Result.Id]));

            SliceComparison? Comparison(Operation.Builtin equal)
            {
                if (equal.Name != "equal" || !lists.TryGetValue(equal.Argument, out var equalArgs) || equalArgs.Items.Count != 2 ||
                    uses[equal.Argument] != 1)
                    return null;
                foreach (var index in new[] { 0, 1 })
                {
                    if (!definitions.TryGetValue(equalArgs.Items[index], out var literalOp) || literalOp is not Operation.Literal literal ||
                        !definitions.TryGetValue(equalArgs.Items[1 - index], out var takeOp) || takeOp is not Operation.Builtin { Name: "take" } take ||
                        uses[take.Result.Id] != 1 || !lists.TryGetValue(take.Argument, out var takeArgs) || takeArgs.Items.Count != 2 ||
                        uses[take.Argument] != 1 ||
                        !definitions.TryGetValue(takeArgs.Items[0], out var countOp) || countOp is not Operation.Literal { Value: LiteralValue.Blob count } ||
                        !definitions.TryGetValue(takeArgs.Items[1], out var skipOp) || skipOp is not Operation.Builtin { Name: "skip" } skip ||
                        uses[skip.Result.Id] != 1 || !lists.TryGetValue(skip.Argument, out var skipArgs) || skipArgs.Items.Count != 2)
                        continue;
                    var length = literal.Value switch
                    {
                        LiteralValue.Blob blob => blob.Bytes.Count,
                        LiteralValue.List list => list.Items.Count,
                        _ => throw new NotImplementedException("Comparison does not handle literal variant: " + literal.Value.GetType().Name),
                    };
                    if (length > 0 && BuiltinFunction.SignedIntegerFromValueRelaxed(new PineValue.BlobValue(count.Bytes.ToArray())) == length)
                        return new(skip, take, equal, skipArgs.Items[1], skipArgs.Items[0], literal.Value);
                }

                return null;
            }

            ImmutableList<SelectedInstruction> Binary(Operation.Builtin builtin, Operation.MakeList list)
            {
                if (compact && builtin.Name is "equal" or "int_add" or "int_mul")
                {
                    foreach (var index in new[] { 0, 1 })
                        if (definitions.TryGetValue(list.Items[index], out var operand) && operand is Operation.Literal literal &&
                            (builtin.Name == "equal" || BuiltinFunction.SignedIntegerFromValueRelaxed(StraightLineVMAdapter.ToPineValue(literal.Value)) is not null))
                            return ImmutableList.Create<SelectedInstruction>(
                                new SelectedInstruction.Load(locals[list.Items[1 - index]]),
                                new SelectedInstruction.Builtin(builtin.Name switch
                                {
                                    "equal" => StackInstructionKind.Equal_Binary_Const,
                                    "int_add" => StackInstructionKind.Int_Add_Const,
                                    "int_mul" => StackInstructionKind.Int_Mul_Const,
                                    _ => throw new InvalidOperationException("Binary does not handle builtin: " + builtin.Name),
                                }, 1, literal.Value)).AddRange(Store(locals[builtin.Result.Id]));
                }
                var computation = builtin.Name is "skip" or "take"
                    ? ImmutableList.Create<SelectedInstruction>(new SelectedInstruction.Builtin(
                        builtin.Name == "take" ? StackInstructionKind.Take_Generic : StackInstructionKind.Skip_Generic,
                        0, CountLocal: locals[list.Items[0]], SourceLocal: locals[list.Items[1]]))
                    : ImmutableList.Create<SelectedInstruction>(
                        new SelectedInstruction.Load(locals[list.Items[0]]),
                        new SelectedInstruction.Load(locals[list.Items[1]]),
                        new SelectedInstruction.Builtin(builtin.Name switch
                        {
                            "equal" => StackInstructionKind.Equal_Binary,
                            "int_add" => StackInstructionKind.Int_Add_Binary,
                            "int_mul" => StackInstructionKind.Int_Mul_Binary,
                            _ => throw new InvalidOperationException("Binary does not handle builtin: " + builtin.Name),
                        }, 2));
                return computation.AddRange(Store(locals[builtin.Result.Id]));
            }
        }
    }

    private static ImmutableList<SelectedInstruction> CompactSelection(
        ImmutableList<SelectedInstruction> selected, BasicBlock block, ImmutableDictionary<PineVirtualValueId, int> locals)
    {
        return Run();

        ImmutableList<SelectedInstruction> Run()
        {
            var literals = block.Operations.OfType<Operation.Literal>()
                .ToImmutableDictionary(literal => locals[literal.Result.Id], literal => literal.Value);
            var retained = TerminatorOperands(block.Terminator).Select(id => locals[id]).ToImmutableHashSet()
                .Union(selected.OfType<SelectedInstruction.Builtin>().SelectMany(builtin =>
                    ImmutableList<int?>.Empty.Add(builtin.CountLocal).Add(builtin.SourceLocal)).OfType<int>());
            if (SelectSliceSwitch(block) is { } slice)
                retained = retained.Add(locals[slice.Source]).Add(locals[slice.Count]);
            var output = ImmutableList.CreateBuilder<SelectedInstruction>();
            for (var index = 0; index < selected.Count; ++index)
            {
                if (index + 2 < selected.Count && selected[index] is SelectedInstruction.Literal &&
                    selected[index + 1] is SelectedInstruction.Store store &&
                    literals.ContainsKey(store.Local) && !retained.Contains(store.Local) &&
                    selected[index + 2] is SelectedInstruction.Pop)
                {
                    index += 2;
                    continue;
                }
                var instruction = selected[index] is SelectedInstruction.Load load && literals.TryGetValue(load.Local, out var literal)
                    ? new SelectedInstruction.Literal(literal) : selected[index];
                if (instruction is SelectedInstruction.Load next && output.Count >= 2 &&
                    output[^1] is SelectedInstruction.Pop && output[^2] is SelectedInstruction.Store previous && previous.Local == next.Local)
                {
                    output.RemoveAt(output.Count - 1);
                    continue;
                }
                output.Add(instruction);
            }
            return output.ToImmutable();
        }
    }

    private static ImmutableList<PineVirtualValueId> Operands(Operation operation) => operation switch
    {
        Operation.Literal => [],
        Operation.MakeList list => list.Items,
        Operation.Project project => [project.Source],
        Operation.Builtin builtin => [builtin.Argument],
        _ => throw new NotImplementedException("Operands does not handle operation variant: " + operation.GetType().Name),
    };
    private static ImmutableList<PineVirtualValueId> TerminatorOperands(Terminator terminator) => terminator switch
    {
        Terminator.Return ret => ret.Values,
        Terminator.Jump jump => jump.Edge.Arguments,
        Terminator.Branch branch => [branch.TestedValue, .. branch.IfEqual.Arguments, .. branch.IfNotEqual.Arguments],
        Terminator.Switch selection => [selection.Selector, .. selection.Cases.SelectMany(item => item.Edge.Arguments), .. selection.Default.Arguments],
        Terminator.Invoke invoke => [.. CallOperands(invoke.Call), .. invoke.Continuation.Bindings.SelectMany(binding => binding switch
        {
            ContinuationBinding.CallerValue caller => ImmutableList.Create(caller.Value),
            ContinuationBinding.ReturnedResult => [],
            _ => throw new NotImplementedException("TerminatorOperands does not handle binding variant: " + binding.GetType().Name),
        })],
        Terminator.TailInvoke tail => CallOperands(tail.Call),
        _ => throw new NotImplementedException("TerminatorOperands does not handle terminator variant: " + terminator.GetType().Name),
    };

    private static ImmutableList<PineVirtualValueId> CallOperands(Call call) => call.Arguments.AddRange(call.Target switch
    {
        CallTarget.Dynamic target => ImmutableList.Create(target.EncodedExpression),
        CallTarget.Known => [],
        _ => throw new NotImplementedException("CallOperands does not handle target variant: " + call.Target.GetType().Name),
    });

    private static ImmutableList<SelectedInstruction> Compute(
        Operation operation, ImmutableDictionary<PineVirtualValueId, int> locals) =>
        operation switch
        {
            Operation.Literal literal => [new SelectedInstruction.Literal(literal.Value)],
            Operation.MakeList list =>
                list.Items.Select(value => (SelectedInstruction)new SelectedInstruction.Load(locals[value]))
                .ToImmutableList().Add(new SelectedInstruction.MakeList(list.Items.Count)),
            Operation.Project project =>
                [new SelectedInstruction.Load(locals[project.Source]), .. Project(project.Path)],
            Operation.Builtin builtin =>
                [new SelectedInstruction.Load(locals[builtin.Argument]),
                new SelectedInstruction.Builtin(StraightLineCompiler.SelectBuiltin(builtin.Name))],
            _ => throw new NotImplementedException(
                "Compute does not handle operation variant: " + operation.GetType().Name),
        };

    internal static int MaximumStack(ImmutableList<SelectedInstruction> instructions, int entryDepth = 0)
    {
        return Fold();

        int Fold()
        {
            var depth = entryDepth;
            var maximum = entryDepth;
            foreach (var instruction in instructions)
            {
                var (read, produced) = instruction switch
                {
                    SelectedInstruction.Literal => (0, 1),
                    SelectedInstruction.Load => (0, 1),
                    SelectedInstruction.Store => (1, 1),
                    SelectedInstruction.Pop => (1, 0),
                    SelectedInstruction.MakeList list => (list.Count, 1),
                    SelectedInstruction.Project => (1, 1),
                    SelectedInstruction.Builtin builtin => (builtin.OperandCount, 1),
                    SelectedInstruction.Return => (1, 0),
                    _ => throw new NotImplementedException(
                        "MaximumStack does not handle selected instruction variant: " + instruction.GetType().Name),
                };
                if (depth < read)
                    throw new InvalidOperationException("Selection produced stack underflow.");
                if (instruction is SelectedInstruction.Builtin { CountLocal: not null, SourceLocal: not null })
                    maximum = Math.Max(maximum, checked(depth + 2));
                depth = checked(depth - read + produced);
                maximum = Math.Max(maximum, depth);
            }
            if (depth != 0)
                throw new InvalidOperationException("A selected fragment left values on the stack.");
            return maximum;
        }
    }
}
