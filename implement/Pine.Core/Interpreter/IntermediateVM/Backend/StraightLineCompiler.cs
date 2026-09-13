using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>
/// Conservative lowering for exactly one block ending in a single-value return. Extra blocks,
/// including unreachable ones, are declined rather than silently discarded. No production cutover.
/// </summary>
public static class StraightLineCompiler
{
    /// <summary>
    /// Preserves every operation, including unused potentially failing computations. Each definition
    /// receives a distinct local. The equivalent immutable fold accumulates storage, selected code,
    /// current stack height and its maximum; imperative working state exists only in the local fold.
    /// </summary>
    public static Result<StraightLineDiagnostic, StraightLineFunction> Compile(ValidatedFunctionGraph validated)
    {
        var graph = validated.Graph;

        if (graph.Blocks.Count != 1)
            return Result<StraightLineDiagnostic, StraightLineFunction>.err(
                new(StraightLineDiagnosticCode.MultipleBlocks, graph.Id, graph.Entry));

        if (graph.Signature.Results.Count != 1)
            return Result<StraightLineDiagnostic, StraightLineFunction>.err(
                new(StraightLineDiagnosticCode.UnsupportedResultArity, graph.Id, graph.Entry));

        var block = graph.Blocks[graph.Entry];

        switch (block.Terminator)
        {
            case Terminator.Return ret:
                return Select(ret);
            case Terminator.Jump:
            case Terminator.Branch:
            case Terminator.Switch:
            case Terminator.Invoke:
            case Terminator.TailInvoke:
                return Result<StraightLineDiagnostic, StraightLineFunction>.err(
                    new(StraightLineDiagnosticCode.UnsupportedTerminator, graph.Id, block.Id));
            default:
                throw new NotImplementedException(
                    "Compile does not handle terminator variant: " + block.Terminator.GetType().Name);
        }

        StraightLineFunction Select(Terminator.Return ret)
        {
            var storage = block.Parameters.Concat(block.Operations.Select(InstructionSelection.Result))
                .Select((definition, index) => new StorageBinding(definition.Id, checked(index + 1)))
                .ToImmutableList();
            var locals = storage.ToImmutableDictionary(binding => binding.Value, binding => binding.Local);
            var instructions = block.Parameters.SelectMany((parameter, index) =>
                ImmutableList.Create<SelectedInstruction>(new SelectedInstruction.Load(0))
                .AddRange(InstructionSelection.Project(graph.Signature.Parameters[index].Path))
                .AddRange(InstructionSelection.Store(locals[parameter.Id])))
                .Concat(block.Operations.SelectMany(operation => InstructionSelection.Select(operation, locals)))
                .ToImmutableList()
                .Add(new SelectedInstruction.Load(locals[ret.Values[0]]))
                .Add(new SelectedInstruction.Return());
            return new(graph.Id, graph.Signature, storage, instructions,
                new(checked(storage.Count + 1), InstructionSelection.MaximumStack(instructions)));
        }
    }

    internal static StackInstructionKind SelectBuiltin(string name) =>
        name switch
        {
            nameof(BuiltinFunction.equal) => StackInstructionKind.Equal_Generic,
            nameof(BuiltinFunction.length) => StackInstructionKind.Length,
            nameof(BuiltinFunction.head) => StackInstructionKind.Head_Generic,
            nameof(BuiltinFunction.skip) => StackInstructionKind.Skip_Generic,
            nameof(BuiltinFunction.take) => StackInstructionKind.Take_Generic,
            nameof(BuiltinFunction.concat) => StackInstructionKind.Concat_Generic,
            nameof(BuiltinFunction.reverse) => StackInstructionKind.Reverse,
            nameof(BuiltinFunction.negate) => StackInstructionKind.Negate,
            nameof(BuiltinFunction.int_add) => StackInstructionKind.Int_Add_Generic,
            nameof(BuiltinFunction.int_mul) => StackInstructionKind.Int_Mul_Generic,
            nameof(BuiltinFunction.int_is_sorted_asc) => StackInstructionKind.Int_Is_Sorted_Asc_Generic,
            nameof(BuiltinFunction.bit_and) => StackInstructionKind.Bit_And_Generic,
            nameof(BuiltinFunction.bit_or) => StackInstructionKind.Bit_Or_Generic,
            nameof(BuiltinFunction.bit_xor) => StackInstructionKind.Bit_Xor_Generic,
            nameof(BuiltinFunction.bit_not) => StackInstructionKind.Bit_Not,
            nameof(BuiltinFunction.bit_shift_left) => StackInstructionKind.Bit_Shift_Left_Generic,
            nameof(BuiltinFunction.bit_shift_right) => StackInstructionKind.Bit_Shift_Right_Generic,
            _ => throw new NotImplementedException("SelectBuiltin does not handle validated builtin: " + name),
        };
}
