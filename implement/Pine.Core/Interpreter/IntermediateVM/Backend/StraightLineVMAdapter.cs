using Pine.Core.CodeAnalysis;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Backend;

/// <summary>Explicit legacy VM boundary. No legacy mutable payload is retained in compiler artifacts.</summary>
public static class StraightLineVMAdapter
{
    /// <summary>
    /// Creates a fresh frame payload using precomputed bounds, bypassing legacy topology analysis.
    /// The canonical incoming environment avoids sorting or deduplicating semantic parameters.
    /// </summary>
    public static StackFrameInstructions ToStackFrame(StraightLineFunction function) =>
        new(
            StaticFunctionInterface.FromPathsSorted([[]]),
            function.Instructions.SelectMany(ToInstructions).ToArray(),
            function.Resources.LocalsCount,
            function.Resources.MaxStackUsage);

    /// <summary>
    /// Owns every array and deliberately bypasses PineValue's global interning factories. Adapting
    /// twice cannot share mutable literal payloads, even for small blobs or empty lists.
    /// </summary>
    public static PineValue ToPineValue(LiteralValue value) =>
        value switch
        {
            LiteralValue.Blob blob => new PineValue.BlobValue(blob.Bytes.ToArray()),
            LiteralValue.List list => new PineValue.ListValue(list.Items.Select(ToPineValue).ToArray()),
            _ => throw new NotImplementedException(
                "ToPineValue does not handle literal variant: " + value.GetType().Name),
        };

    internal static ImmutableList<StackInstruction> ToInstructions(SelectedInstruction instruction) =>
        instruction is SelectedInstruction.Builtin { CountLocal: not null, SourceLocal: not null } slice
            ? SliceInstructions(slice) : [ToInstruction(instruction)];

    internal static int InstructionCount(SelectedInstruction instruction) =>
        instruction is SelectedInstruction.Builtin { CountLocal: not null, SourceLocal: not null } slice
            ? slice.Kind == StackInstructionKind.Take_Generic ? 14 : 19 : 1;

    private static ImmutableList<StackInstruction> SliceInstructions(SelectedInstruction.Builtin slice) =>
        slice.OperandCount != 0 || slice.Kind is not (StackInstructionKind.Take_Generic or StackInstructionKind.Skip_Generic) ||
        slice.Constant is not null
        ? throw new InvalidOperationException("SliceInstructions does not handle selection: " + slice)
        : slice.Kind == StackInstructionKind.Take_Generic
        ? [
            StackInstruction.Local_Get(slice.CountLocal!.Value),
            new(StackInstructionKind.Int_Less_Than_Or_Equal_Const, IntegerLiteral: 0),
            StackInstruction.Jump_If_True(10),
            StackInstruction.Local_Get(slice.CountLocal!.Value),
            new(StackInstructionKind.Int_Greater_Than_Or_Equal_Const, IntegerLiteral: int.MaxValue),
            StackInstruction.Jump_If_True(5),
            StackInstruction.Local_Get(slice.SourceLocal!.Value),
            StackInstruction.Local_Get(slice.CountLocal!.Value),
            new(StackInstructionKind.Take_Binary),
            StackInstruction.Jump_Unconditional(5),
            StackInstruction.Local_Get(slice.SourceLocal!.Value),
            StackInstruction.Jump_Unconditional(3),
            StackInstruction.Local_Get(slice.SourceLocal!.Value),
            new(StackInstructionKind.Take_Const, TakeCount: 0),
        ]
        : [
            StackInstruction.Local_Get(slice.CountLocal!.Value),
            new(StackInstructionKind.Int_Less_Than_Or_Equal_Const, IntegerLiteral: 0),
            StackInstruction.Jump_If_True(13),
            StackInstruction.Local_Get(slice.CountLocal!.Value),
            new(StackInstructionKind.Int_Greater_Than_Or_Equal_Const, IntegerLiteral: (long)int.MaxValue + 1),
            StackInstruction.Jump_If_True(6),
            StackInstruction.Local_Get(slice.SourceLocal!.Value),
            new(StackInstructionKind.Length),
            StackInstruction.Local_Get(slice.CountLocal!.Value),
            new(StackInstructionKind.Int_Less_Than_Or_Equal_Binary),
            StackInstruction.Jump_If_True(7),
            StackInstruction.Local_Get(slice.SourceLocal!.Value),
            StackInstruction.Local_Get(slice.CountLocal!.Value),
            new(StackInstructionKind.Skip_Binary),
            StackInstruction.Jump_Unconditional(5),
            StackInstruction.Local_Get(slice.SourceLocal!.Value),
            StackInstruction.Jump_Unconditional(3),
            StackInstruction.Local_Get(slice.SourceLocal!.Value),
            new(StackInstructionKind.Take_Const, TakeCount: 0),
        ];

    internal static StackInstruction ToInstruction(SelectedInstruction instruction) =>
        instruction switch
        {
            SelectedInstruction.Literal literal => StackInstruction.Push_Literal(ToPineValue(literal.Value)),
            SelectedInstruction.Load load => StackInstruction.Local_Get(load.Local),
            SelectedInstruction.Store store => StackInstruction.Local_Set(store.Local),
            SelectedInstruction.Pop => StackInstruction.PopMultiple(1),
            SelectedInstruction.MakeList list => StackInstruction.Build_List(list.Count),
            SelectedInstruction.Project project =>
                new StackInstruction(StackInstructionKind.List_Project_Const, SkipCount: project.Index),
            SelectedInstruction.Builtin builtin => new StackInstruction(RequireBuiltin(builtin),
                Literal: builtin.Kind is not (StackInstructionKind.Int_Add_Const or StackInstructionKind.Int_Mul_Const) &&
                    builtin.Constant is { } constant ? ToPineValue(constant) : null,
                IntegerLiteral: builtin.Kind is StackInstructionKind.Int_Add_Const or StackInstructionKind.Int_Mul_Const
                    ? BuiltinFunction.SignedIntegerFromValueRelaxed(ToPineValue(builtin.Constant!)) : null),
            SelectedInstruction.Return => new StackInstruction(StackInstructionKind.Return),
            _ => throw new NotImplementedException(
                "ToInstruction does not handle selected instruction variant: " + instruction.GetType().Name),
        };

    private static StackInstructionKind RequireBuiltin(SelectedInstruction.Builtin builtin) =>
        builtin.CountLocal is not null || builtin.SourceLocal is not null
        ? throw new InvalidOperationException("RequireBuiltin requires ToInstructions for scalar-local operands.")
        : (builtin.Kind, builtin.OperandCount, builtin.Constant) switch
        {
            (StackInstructionKind.Slice_Skip_Var_Equal_Const, 2, not null) => builtin.Kind,
            (StackInstructionKind.Equal_Binary_Const, 1, not null) => builtin.Kind,
            (StackInstructionKind.Int_Add_Const or StackInstructionKind.Int_Mul_Const, 1, not null)
                when BuiltinFunction.SignedIntegerFromValueRelaxed(ToPineValue(builtin.Constant)) is not null => builtin.Kind,
            (StackInstructionKind.Equal_Binary or StackInstructionKind.Int_Add_Binary or StackInstructionKind.Int_Mul_Binary, 2, null) => builtin.Kind,
            (StackInstructionKind.Equal_Generic or StackInstructionKind.Length or
            StackInstructionKind.Head_Generic or StackInstructionKind.Skip_Generic or
            StackInstructionKind.Take_Generic or StackInstructionKind.Concat_Generic or
            StackInstructionKind.Reverse or StackInstructionKind.Negate or
            StackInstructionKind.Int_Add_Generic or StackInstructionKind.Int_Mul_Generic or
            StackInstructionKind.Int_Is_Sorted_Asc_Generic or StackInstructionKind.Bit_And_Generic or
            StackInstructionKind.Bit_Or_Generic or StackInstructionKind.Bit_Xor_Generic or
            StackInstructionKind.Bit_Not or StackInstructionKind.Bit_Shift_Left_Generic or
            StackInstructionKind.Bit_Shift_Right_Generic, 1, null) => builtin.Kind,
            _ => throw new InvalidOperationException("Not a selected builtin: " + builtin),
        };
}
