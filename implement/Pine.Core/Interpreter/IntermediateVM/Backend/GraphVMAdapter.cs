using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Final numeric emission and fresh legacy payload conversion; never reconstructs a graph.</summary>
public static class GraphVMAdapter
{
    private sealed record EmissionFragment(LayoutFragment Fragment, bool StackForwarded, bool Fallthrough, bool SliceForwarded);
    /// <summary>
    /// Uses graph-derived resources with the precomputed frame constructor. Every edge has its own
    /// nonempty stub (at least a jump), so even an empty self-cycle has a negative, nonzero backjump.
    /// Legacy quotas/cancellation remain active; loop counts still depend on layout until increment 9.
    /// </summary>
    public static StackFrameInstructions ToStackFrame(
        GraphFunction function, GraphProgram? program = null, bool projectedArguments = false)
    {
        if (projectedArguments && function.LegacyParameterLocals)
            throw new ArgumentException("ToStackFrame cannot use graph-call entry bindings for legacy parameter locals.");
        var layout = projectedArguments
            ? function.Layout.SetItem(0, function.Layout[0] with { Instructions = [] })
            : function.Layout;
        var emission = layout.Select((fragment, index) => Plan(fragment, index)).ToImmutableList();
        return Emit();

        EmissionFragment Plan(LayoutFragment fragment, int index)
        {
            var terminalLocal = fragment.Transfer switch
            {
                LayoutTransfer.Return ret => (int?)ret.Local,
                LayoutTransfer.Branch branch => branch.Local,
                LayoutTransfer.Jump or LayoutTransfer.Invoke or LayoutTransfer.TailInvoke or LayoutTransfer.Match => null,
                _ => throw new NotImplementedException("Plan does not handle transfer variant: " + fragment.Transfer.GetType().Name),
            };
            var stackForwarded = function.Compact && fragment.Instructions.Count >= 2 &&
                fragment.Instructions[^1] is SelectedInstruction.Pop &&
                fragment.Instructions[^2] is SelectedInstruction.Store store && store.Local == terminalLocal;
            var sliceForwarded = function.Compact &&
                fragment.Transfer is LayoutTransfer.Match { SliceSourceLocal: not null } slice &&
                slice.SliceSourceLocal != slice.Local && fragment.Instructions.Count >= 4 &&
                fragment.Instructions[^4] is SelectedInstruction.Load &&
                fragment.Instructions[^3] is SelectedInstruction.Builtin { OperandCount: 1, CountLocal: null, SourceLocal: null } &&
                fragment.Instructions[^2] is SelectedInstruction.Store count && count.Local == slice.Local &&
                fragment.Instructions[^1] is SelectedInstruction.Pop;
            var successor = fragment.Transfer switch
            {
                LayoutTransfer.Jump jump => (LayoutLabel?)jump.Target,
                LayoutTransfer.Branch branch => branch.NotEqual,
                LayoutTransfer.Invoke invoke => invoke.Success,
                LayoutTransfer.Match match => match.Default,
                LayoutTransfer.Return or LayoutTransfer.TailInvoke => null,
                _ => throw new NotImplementedException("Plan does not handle transfer variant: " + fragment.Transfer.GetType().Name),
            };
            var fallthrough = function.Compact && index + 1 < layout.Count && successor == layout[index + 1].Label &&
                (fragment.Transfer is not LayoutTransfer.Jump || fragment.Instructions.Count > 0 ||
                    fragment.Label.Kind == LayoutLabelKind.Prologue && layout[index + 1].Instructions.Count > 0);
            var selectedInstructions = sliceForwarded
                ? fragment.Instructions.RemoveAt(fragment.Instructions.Count - 1)
                    .Insert(fragment.Instructions.Count - 4, new SelectedInstruction.Load(((LayoutTransfer.Match)fragment.Transfer).SliceSourceLocal!.Value))
                : stackForwarded
                    ? fragment.Instructions.RemoveRange(fragment.Instructions.Count - (fragment.Transfer is LayoutTransfer.Return ? 2 : 1),
                        fragment.Transfer is LayoutTransfer.Return ? 2 : 1)
                    : fragment.Instructions;
            if (sliceForwarded && GraphLayoutLiveness.IsDeadAfterTransfer(layout, fragment, ((LayoutTransfer.Match)fragment.Transfer).Local))
                selectedInstructions = selectedInstructions.RemoveAt(selectedInstructions.Count - 1);
            return new(fragment with
            {
                Instructions = selectedInstructions,
            }, stackForwarded, fallthrough, sliceForwarded);
        }

        StackFrameInstructions Emit()
        {
            var offsets = ImmutableDictionary.CreateBuilder<LayoutLabel, int>();
            var position = 0;
            foreach (var plan in emission)
            {
                var fragment = plan.Fragment;
                offsets.Add(fragment.Label, position);
                position = checked(position + fragment.Instructions.Sum(StraightLineVMAdapter.InstructionCount) +
                    TransferSize(fragment.Transfer) - (plan.SliceForwarded ? 2 : plan.StackForwarded ? 1 : 0) - (plan.Fallthrough ? 1 : 0));
            }

            var instructions = ImmutableList.CreateBuilder<StackInstruction>();
            foreach (var plan in emission)
            {
                var fragment = plan.Fragment;
                instructions.AddRange(fragment.Instructions.SelectMany(StraightLineVMAdapter.ToInstructions));
                switch (fragment.Transfer)
                {
                    case LayoutTransfer.Return ret:
                        if (!plan.StackForwarded)
                            instructions.Add(StackInstruction.Local_Get(ret.Local));
                        instructions.Add(new(StackInstructionKind.Return));
                        break;
                    case LayoutTransfer.Jump jump:
                        if (!plan.Fallthrough)
                            Jump(jump.Target);
                        break;
                    case LayoutTransfer.Branch branch:
                        if (!plan.StackForwarded)
                            instructions.Add(StackInstruction.Local_Get(branch.Local));
                        // The current VM pops this operand on BOTH arms; no extra Pop is needed.
                        instructions.Add(StackInstruction.Jump_If_Equal(
                            checked(offsets[branch.Equal] - instructions.Count),
                            StraightLineVMAdapter.ToPineValue(branch.Literal)));
                        if (!plan.Fallthrough)
                            Jump(branch.NotEqual);
                        break;
                    case LayoutTransfer.Match match:
                        if (!plan.SliceForwarded && match.SliceSourceLocal is { } source)
                            instructions.Add(StackInstruction.Local_Get(source));
                        if (!plan.SliceForwarded)
                            instructions.Add(StackInstruction.Local_Get(match.Local));
                        var switchOffset = instructions.Count;
                        instructions.Add(new(match.SliceSourceLocal is null
                            ? StackInstructionKind.Switch_Jump_If_Equal_Const : StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const,
                            SwitchJumpTable: match.Cases.ToImmutableDictionary(
                                @case => StraightLineVMAdapter.ToPineValue(@case.Literal),
                                @case => checked(offsets[@case.Target] - switchOffset))));
                        if (!plan.Fallthrough)
                            Jump(match.Default);
                        break;
                    case LayoutTransfer.Invoke invoke:
                        Call(invoke.Call, false);
                        if (!plan.Fallthrough)
                            Jump(invoke.Success);
                        break;
                    case LayoutTransfer.TailInvoke invoke:
                        Call(invoke.Call, true);
                        instructions.Add(StackInstruction.Return);
                        break;
                    default:
                        throw new NotImplementedException(
                            "ToStackFrame does not handle layout transfer variant: " + fragment.Transfer.GetType().Name);
                }
            }
            return new(projectedArguments || function.LegacyParameterLocals
                ? StaticFunctionInterface.FromPathsInOrder([.. function.Signature.Parameters.Select(parameter => parameter.Path.Indices)])
                : StaticFunctionInterface.FromPathsSorted([[]]), instructions.ToArray(),
                function.Resources.LocalsCount, function.Resources.MaxStackUsage)
            {
                GraphProgram = program,
                GraphFunctionId = projectedArguments ? function.Id : null,
                GraphParameterLocals = projectedArguments
                    ? [.. function.Layout[0].Instructions.OfType<SelectedInstruction.Store>().Select(store => store.Local)]
                    : null,
            };

            void Call(SelectedCall call, bool tail)
            {
                instructions.AddRange(call.Arguments.Select(StackInstruction.Local_Get));
                switch (call.Target)
                {
                    case SelectedCallTarget.Dynamic dynamic:
                        instructions.Add(StackInstruction.Local_Get(dynamic.Local));
                        instructions.Add(StackInstruction.Eval_Binary);
                        break;
                    case SelectedCallTarget.Known known:
                        if (program is null || !program.Functions.ContainsKey(known.Function) ||
                            program.Functions[known.Function].Signature != call.Signature)
                            throw new ArgumentException("Known call requires a program containing its declared target.");
                        instructions.Add(new(StackInstructionKind.Invoke_GraphFunction)
                        {
                            GraphInvocation = new(known.Function, call.Arguments.Count, tail),
                        });
                        break;
                    default:
                        throw new NotImplementedException("ToStackFrame does not handle call target variant: " + call.Target.GetType().Name);
                }
            }

            void Jump(LayoutLabel target)
            {
                var offset = checked(offsets[target] - instructions.Count);
                if (offset == 0)
                    throw new InvalidOperationException("Graph emission must not produce a zero-offset unconditional jump.");
                instructions.Add(StackInstruction.Jump_Unconditional(offset));
            }
        }
    }

    private static int TransferSize(LayoutTransfer transfer) =>
        transfer switch
        {
            LayoutTransfer.Return => 2,
            LayoutTransfer.Jump => 1,
            LayoutTransfer.Branch => 3,
            LayoutTransfer.Match match => match.SliceSourceLocal is null ? 3 : 4,
            LayoutTransfer.Invoke invoke => CallSize(invoke.Call) + 1,
            LayoutTransfer.TailInvoke invoke => CallSize(invoke.Call) + 1,
            _ => throw new NotImplementedException(
                "TransferSize does not handle layout transfer variant: " + transfer.GetType().Name),
        };

    private static int CallSize(SelectedCall call) =>
        call.Arguments.Count + (call.Target switch
        {
            SelectedCallTarget.Dynamic => 2,
            SelectedCallTarget.Known => 1,
            _ => throw new NotImplementedException("CallSize does not handle target variant: " + call.Target.GetType().Name),
        });
}
