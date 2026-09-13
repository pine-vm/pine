using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Graph lowering with explicit calls and continuations; production selection remains opt-in.</summary>
public static class GraphCompiler
{
    /// <summary>
    /// Selects all blocks, then schedules an optional immutable permutation. Entry initialization
    /// lives only in the prologue, so backedges to entry bind parameters without reloading input.
    /// </summary>
    public static Result<GraphBackendDiagnostic, GraphFunction> Compile(
        ValidatedFunctionGraph validated, ImmutableList<PineBlockId>? blockOrder = null, bool fuseScalarBuiltins = false)
    {
        var graph = validated.Graph;
        var orderedBlocks = graph.Blocks.Values.OrderBy(block => block.Id.Value).ToImmutableList();
        var order = blockOrder ?? orderedBlocks.Select(block => block.Id).ToImmutableList();

        if (graph.Signature.Results.Count != 1)
            return Decline(GraphBackendDiagnosticCode.UnsupportedResultArity, graph.Entry);
        if (order.Count != graph.Blocks.Count || order.Distinct().Count() != order.Count ||
            order.Any(id => !graph.Blocks.ContainsKey(id)))
            return Decline(GraphBackendDiagnosticCode.InvalidBlockOrder, graph.Entry);
        var unsupported = orderedBlocks.FirstOrDefault(block => CallResults(block.Terminator) is { } count && count != 1);
        if (unsupported is not null)
            return Decline(GraphBackendDiagnosticCode.UnsupportedCall, unsupported.Id);

        var storage = orderedBlocks.SelectMany(block =>
            block.Parameters.Concat(block.Operations.Select(InstructionSelection.Result)))
            .Select((definition, index) => new StorageBinding(definition.Id, checked(index + 1))).ToImmutableList();
        var resultLocal = checked(1 + storage.Count);
        var hasInvoke = orderedBlocks.Any(block => block.Terminator is Terminator.Invoke);
        var locals = storage.ToImmutableDictionary(binding => binding.Value, binding => binding.Local);
        var blocks = orderedBlocks.Select(block => new SelectedBlock(
            block.Id,
            fuseScalarBuiltins ? InstructionSelection.SelectBlock(block, locals) :
                block.Operations.SelectMany(operation => InstructionSelection.Select(operation, locals)).ToImmutableList(),
            SelectTerminator(block.Terminator))).ToImmutableList();
        var prologue = graph.Blocks[graph.Entry].Parameters.SelectMany((parameter, index) =>
            ImmutableList.Create<SelectedInstruction>(new SelectedInstruction.Load(0))
            .AddRange(InstructionSelection.Project(graph.Signature.Parameters[index].Path))
            .AddRange(InstructionSelection.Store(locals[parameter.Id]))).ToImmutableList();
        var layout = GraphLayout.Schedule(graph.Entry, prologue, blocks, order);
        var maximum = layout.Max(fragment =>
            Math.Max(InstructionSelection.MaximumStack(fragment.Instructions,
                fragment.Label.Kind == LayoutLabelKind.Return ? 1 : 0),
                fragment.Transfer switch
                {
                    LayoutTransfer.Return => 1,
                    LayoutTransfer.Jump => 0,
                    LayoutTransfer.Branch => 1,
                    LayoutTransfer.Invoke invoke => CallStack(invoke.Call),
                    LayoutTransfer.TailInvoke invoke => CallStack(invoke.Call),
                    _ => throw new NotImplementedException(
                        "Compile does not handle layout transfer variant: " + fragment.Transfer.GetType().Name),
                }));
        return Result<GraphBackendDiagnostic, GraphFunction>.ok(
            new(graph.Id, graph.Signature, storage, blocks, layout, new(checked(resultLocal + (hasInvoke ? 1 : 0)), maximum)));

        Result<GraphBackendDiagnostic, GraphFunction> Decline(GraphBackendDiagnosticCode code, PineBlockId block) =>
            Result<GraphBackendDiagnostic, GraphFunction>.err(new(code, graph.Id, block));

        EdgeCopyPlan SelectEdge(Edge edge) =>
            new(edge.Target, edge.Arguments.Select((argument, index) =>
                new LocalCopy(locals[argument], locals[graph.Blocks[edge.Target].Parameters[index].Id])).ToImmutableList());

        SelectedTerminator SelectTerminator(Terminator terminator) =>
            terminator switch
            {
                Terminator.Return ret => new SelectedTerminator.Return(locals[ret.Values[0]]),
                Terminator.Jump jump => new SelectedTerminator.Jump(SelectEdge(jump.Edge)),
                Terminator.Branch branch => new SelectedTerminator.Match(
                    locals[branch.TestedValue], [new(branch.Literal, SelectEdge(branch.IfEqual))], SelectEdge(branch.IfNotEqual)),
                Terminator.Switch selection => new SelectedTerminator.Match(
                    locals[selection.Selector],
                    selection.Cases.Select(@case => new SelectedCase(@case.Value, SelectEdge(@case.Edge))).ToImmutableList(),
                    SelectEdge(selection.Default)),
                Terminator.Invoke invoke => new SelectedTerminator.Invoke(SelectCall(invoke.Call), resultLocal,
                    new(invoke.Continuation.Target, invoke.Continuation.Bindings.Select((binding, index) =>
                        new LocalCopy(binding switch
                        {
                            ContinuationBinding.CallerValue caller => locals[caller.Value],
                            ContinuationBinding.ReturnedResult => resultLocal,
                            _ => throw new NotImplementedException(
                                "SelectTerminator does not handle binding variant: " + binding.GetType().Name),
                        }, locals[graph.Blocks[invoke.Continuation.Target].Parameters[index].Id])).ToImmutableList())),
                Terminator.TailInvoke invoke => new SelectedTerminator.TailInvoke(SelectCall(invoke.Call)),
                _ => throw new NotImplementedException(
                    "SelectTerminator does not handle terminator variant: " + terminator.GetType().Name),
            };

        SelectedCall SelectCall(Call call) =>
            new(call.Target switch
            {
                CallTarget.Dynamic dynamic => new SelectedCallTarget.Dynamic(locals[dynamic.EncodedExpression]),
                CallTarget.Known known => new SelectedCallTarget.Known(known.Function),
                _ => throw new NotImplementedException("SelectCall does not handle target variant: " + call.Target.GetType().Name),
            }, call.Signature, call.Arguments.Select(argument => locals[argument]).ToImmutableList());
    }

    private static int CallStack(SelectedCall call) =>
        Math.Max(1, call.Arguments.Count + (call.Target switch
        {
            SelectedCallTarget.Dynamic => 1,
            SelectedCallTarget.Known => 0,
            _ => throw new NotImplementedException("CallStack does not handle target variant: " + call.Target.GetType().Name),
        }));

    private static int? CallResults(Terminator terminator) =>
        terminator switch
        {
            Terminator.Return => null,
            Terminator.Jump => null,
            Terminator.Branch => null,
            Terminator.Switch => null,
            Terminator.Invoke invoke => invoke.Call.Signature.Results.Count,
            Terminator.TailInvoke invoke => invoke.Call.Signature.Results.Count,
            _ => throw new NotImplementedException(
                "CallResults does not handle terminator variant: " + terminator.GetType().Name),
        };
}
