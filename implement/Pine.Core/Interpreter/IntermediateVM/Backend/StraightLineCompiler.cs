using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;

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
            var storage = ImmutableList.CreateBuilder<StorageBinding>();
            var locals = ImmutableDictionary.CreateBuilder<PineVirtualValueId, int>();
            var instructions = ImmutableList.CreateBuilder<SelectedInstruction>();
            var depth = 0;
            var maximum = 0;

            void Emit(SelectedInstruction instruction, int consumed, int produced)
            {
                if (depth < consumed)
                    throw new InvalidOperationException("Selection produced stack underflow.");

                depth = checked(depth - consumed + produced);
                maximum = Math.Max(maximum, depth);
                instructions.Add(instruction);
            }

            void Store(ValueDefinition definition)
            {
                var local = checked(storage.Count + 1);
                storage.Add(new(definition.Id, local));
                locals.Add(definition.Id, local);
                // Local_Set reads but does not pop.
                Emit(new SelectedInstruction.Store(local), 1, 1);
                Emit(new SelectedInstruction.Pop(), 1, 0);

                if (depth != 0)
                    throw new InvalidOperationException("An operation left values on the stack.");
            }

            void Load(PineVirtualValueId value) =>
                Emit(new SelectedInstruction.Load(locals[value]), 0, 1);

            void Project(EnvironmentPath path)
            {
                foreach (var index in path.Indices)
                    Emit(new SelectedInstruction.Project(index), 1, 1);
            }

            for (var index = 0; index < block.Parameters.Count; ++index)
            {
                Emit(new SelectedInstruction.Load(0), 0, 1);
                Project(graph.Signature.Parameters[index].Path);
                Store(block.Parameters[index]);
            }

            foreach (var operation in block.Operations)
            {
                switch (operation)
                {
                    case Operation.Literal literal:
                        Emit(new SelectedInstruction.Literal(literal.Value), 0, 1);
                        Store(literal.Result);
                        break;
                    case Operation.MakeList list:
                        foreach (var item in list.Items)
                            Load(item);
                        Emit(new SelectedInstruction.MakeList(list.Items.Count), list.Items.Count, 1);
                        Store(list.Result);
                        break;
                    case Operation.Project project:
                        Load(project.Source);
                        Project(project.Path);
                        Store(project.Result);
                        break;
                    case Operation.Builtin builtin:
                        Load(builtin.Argument);
                        Emit(new SelectedInstruction.Builtin(SelectBuiltin(builtin.Name)), 1, 1);
                        Store(builtin.Result);
                        break;
                    default:
                        throw new NotImplementedException(
                            "Compile does not handle operation variant: " + operation.GetType().Name);
                }
            }

            Load(ret.Values[0]);
            // The VM's Return requires a value, despite legacy details reporting PopCount = 0.
            Emit(new SelectedInstruction.Return(), 1, 0);
            return new(graph.Id, graph.Signature, storage.ToImmutable(), instructions.ToImmutable(),
                new(checked(storage.Count + 1), maximum));
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
