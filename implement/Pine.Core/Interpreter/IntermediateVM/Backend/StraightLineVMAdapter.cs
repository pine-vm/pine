using Pine.Core.CodeAnalysis;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using System;
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
            function.Instructions.Select(ToInstruction).ToArray(),
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
            SelectedInstruction.Builtin builtin => new StackInstruction(RequireGenericBuiltin(builtin.Kind)),
            SelectedInstruction.Return => new StackInstruction(StackInstructionKind.Return),
            _ => throw new NotImplementedException(
                "ToInstruction does not handle selected instruction variant: " + instruction.GetType().Name),
        };

    private static StackInstructionKind RequireGenericBuiltin(StackInstructionKind kind) =>
        kind switch
        {
            StackInstructionKind.Equal_Generic or StackInstructionKind.Length or
            StackInstructionKind.Head_Generic or StackInstructionKind.Skip_Generic or
            StackInstructionKind.Take_Generic or StackInstructionKind.Concat_Generic or
            StackInstructionKind.Reverse or StackInstructionKind.Negate or
            StackInstructionKind.Int_Add_Generic or StackInstructionKind.Int_Mul_Generic or
            StackInstructionKind.Int_Is_Sorted_Asc_Generic or StackInstructionKind.Bit_And_Generic or
            StackInstructionKind.Bit_Or_Generic or StackInstructionKind.Bit_Xor_Generic or
            StackInstructionKind.Bit_Not or StackInstructionKind.Bit_Shift_Left_Generic or
            StackInstructionKind.Bit_Shift_Right_Generic => kind,
            _ => throw new InvalidOperationException("Not a selected generic builtin: " + kind),
        };
}
