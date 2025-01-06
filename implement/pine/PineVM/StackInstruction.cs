using Pine.Core;
using System.Numerics;

namespace Pine.PineVM;

/*
 * As in the Pine language, the value representing True for conditional jumps is (Blob [4]).
 * 
 * As in the Pine language, operations expecting integers will return an empty list if one
 * of the operand values is not a valid encoding of an integer.
 * */

/// <summary>
/// On execution of an instruction, the arguments are popped from the stack and the result is pushed to the stack.
/// 
/// For more information on how these instructions are interpreted, see <see cref="StackInstruction"/>
/// </summary>
public enum StackInstructionKind
{
    /// <summary>
    /// Push the literal from <see cref="StackInstruction.Literal"/> to the stack.
    /// </summary>
    Push_Literal,

    Push_Environment,

    /// <summary>
    /// Copy the top value from the stack into the local at index <see cref="StackInstruction.LocalIndex"/>.
    /// The value is not popped from the stack.
    /// </summary>
    Local_Set,

    /// <summary>
    /// Load the local at index <see cref="StackInstruction.LocalIndex"/> and push it to the stack.
    /// </summary>
    Local_Get,

    /// <summary>
    /// Drop the top value from the stack.
    /// </summary>
    Pop,

    /// <summary>
    /// Gets the length of the top value from the stack as integer.
    /// </summary>
    Length,

    /// <summary>
    /// Concatenates the items in the list from the top value on the stack.
    /// </summary>
    Concat_Generic,

    /// <summary>
    /// Concatenates the top value from the stack and the second value from the stack.
    /// 
    /// If the two arguments are not both lists or both blobs, returns only the top value.
    /// </summary>
    Concat_Binary,

    /// <summary>
    /// Slice the third value from the stack,
    /// using the second value as the start index and the top value as the length.
    /// </summary>
    Slice_Skip_Var_Take_Var,

    /// <summary>
    /// Slice the second value from the stack,
    /// using the top value as the start index and the <see cref="StackInstruction.TakeCount"/> as the length.
    /// </summary>
    Slice_Skip_Var_Take_Const,

    Skip_Generic,

    Skip_Binary,

    Take_Generic,

    Take_Binary,

    /// <summary>
    /// From the second value on the stack, get the element at the index of the top value on the stack.
    /// </summary>
    Skip_Head_Var,

    /// <summary>
    /// From the top value on the stack, get the element at the index of <see cref="StackInstruction.SkipCount"/>.
    /// </summary>
    Skip_Head_Const,

    Head_Generic,

    Reverse,

    /// <summary>
    /// Builds a list from the top <see cref="StackInstruction.TakeCount"/> values on the stack.
    /// </summary>
    BuildList,

    /// <summary>
    /// Check if the top two values on the stack are equal.
    /// </summary>
    Equal_Binary_Var,

    Not_Equal_Binary_Var,

    /// <summary>
    /// Check if the top value on the stack is equal to the literal from <see cref="StackInstruction.Literal"/>.
    /// </summary>
    Equal_Binary_Const,

    Not_Equal_Binary_Const,

    /// <summary>
    /// Check if all items in the list from the top value on the stack are equal.
    /// </summary>
    Equal_Generic,

    /// <summary>
    /// Negate the top value on the stack, works for both integers and booleans.
    /// </summary>
    Negate,

    /// <summary>
    /// Corresponding to the kernel function 'int_is_sorted_asc' of the Pine language,
    /// where we cannot proof a more specific representation is possible at compile time.
    /// </summary>
    Int_Is_Sorted_Asc_Generic,

    Int_Less_Than_Binary,

    Int_Less_Than_Or_Equal_Binary,

    Int_Less_Than_Const,

    Int_Less_Than_Or_Equal_Const,

    /// <summary>
    /// Jump to the offset from <see cref="StackInstruction.JumpOffset"/> if the top value on the stack is true.
    /// </summary>
    Jump_If_True_Const,

    /// <summary>
    /// Unconditional jump to the offset from <see cref="StackInstruction.JumpOffset"/>.
    /// </summary>
    Jump_Const,

    /// <summary>
    /// The top value on the stack becomes the result of the function.
    /// </summary>
    Return,

    /// <summary>
    /// Tries to parse the top value from the stack as a Pine expression and evaluates it using
    /// the second value on the stack as the environment.
    /// </summary>
    Parse_And_Eval,

    /// <summary>
    /// Add the top two values on the stack.
    /// </summary>
    Int_Add_Binary,

    /// <summary>
    /// Add the integer literal from <see cref="StackInstruction.IntegerLiteral"/>.
    /// </summary>
    Int_Add_Const,

    /// <summary>
    /// Add all items in the list from the top value from the stack.
    /// </summary>
    Int_Add_Generic,

    /// <summary>
    /// Subtract the top value from the second value on the stack.
    /// </summary>
    Int_Sub_Binary,

    /// <summary>
    /// Multiply the top two values on the stack.
    /// </summary>
    Int_Mul_Binary,

    /// <summary>
    /// Multiply the top value on the stack with the integer literal from <see cref="StackInstruction.IntegerLiteral"/>.
    /// </summary>
    Int_Mul_Const,

    /// <summary>
    /// Multiply all items in the list from the top value from the stack.
    /// </summary>
    Int_Mul_Generic,

    Bit_And_Generic,

    Bit_And_Binary,

    Bit_And_Const,

    Bit_Or_Generic,

    Bit_Or_Binary,

    Bit_Or_Const,

    Bit_Xor_Generic,

    Bit_Xor_Binary,

    Bit_Not,

    Bit_Shift_Left_Var,

    Bit_Shift_Left_Const,

    Bit_Shift_Left_Generic,

    Bit_Shift_Right_Var,

    Bit_Shift_Right_Const,

    Bit_Shift_Right_Generic,
}

/// <summary>
/// Represents a single instruction for the Pine stack-based virtual machine. 
/// Depending on the <see cref="Kind"/>, this instruction may consume zero or more values 
/// from the evaluation stack, optionally produce a new value to push onto the stack, 
/// or manipulate local variables and control flow.
/// </summary>
/// <remarks>
/// The fields <see cref="Literal"/>, <see cref="LocalIndex"/>, <see cref="SkipCount"/>, 
/// <see cref="TakeCount"/>, <see cref="JumpOffset"/>, and <see cref="ShiftCount"/> may or may not 
/// be relevant depending on the specific <see cref="Kind"/>. For instance, 
/// <see cref="StackInstructionKind.Push_Literal"/> uses <see cref="Literal"/>, 
/// <see cref="StackInstructionKind.Local_Set"/> uses <see cref="LocalIndex"/>, 
/// and jump instructions use <see cref="JumpOffset"/>. 
/// Refer to the documentation on each <see cref="StackInstructionKind"/> value for more details.
/// </remarks>
/// <param name="Kind">The enum value specifying which operation to perform.</param>
/// <param name="Literal">An optional literal value (used by push or parse instructions).</param>
/// <param name="LocalIndex">An optional index for reading/writing to a local variable.</param>
/// <param name="SkipCount">An optional skip count used in slice/skip operations.</param>
/// <param name="TakeCount">An optional take count used in slice/build operations.</param>
/// <param name="JumpOffset">An optional offset for conditional or unconditional jumps.</param>
/// <param name="ShiftCount">An optional amount to shift bits, used by bitwise shift instructions.</param>
public record StackInstruction(
    StackInstructionKind Kind,
    PineValue? Literal = null,
    BigInteger? IntegerLiteral = null,
    int? LocalIndex = null,
    int? SkipCount = null,
    int? TakeCount = null,
    int? JumpOffset = null,
    int? ShiftCount = null)
{
    public static readonly StackInstruction PushEnvironment =
        new(StackInstructionKind.Push_Environment);

    public static readonly StackInstruction Return =
        new(StackInstructionKind.Return);

    public static StackInstruction Jump_Unconditional(int offset) =>
        new(StackInstructionKind.Jump_Const, JumpOffset: offset);

    /// <summary>
    /// Creates a new instruction to jump to the specified offset if the top value on the stack is true.
    /// </summary>
    public static StackInstruction Jump_If_True(int offset) =>
        new(StackInstructionKind.Jump_If_True_Const, JumpOffset: offset);

    public static (int popCount, int pushCount) GetPopCountAndPushCount(StackInstruction instruction) =>
        instruction.Kind switch
        {
            StackInstructionKind.Push_Literal => (0, 1),
            StackInstructionKind.Push_Environment => (0, 1),

            StackInstructionKind.Local_Set => (0, 0),
            StackInstructionKind.Local_Get => (0, 1),
            
            StackInstructionKind.Pop => (1, 0),

            StackInstructionKind.Length => (1, 1),
            
            StackInstructionKind.Concat_Generic => (1, 1),
            StackInstructionKind.Concat_Binary => (2, 1),
            
            StackInstructionKind.Slice_Skip_Var_Take_Var => (3, 1),
            StackInstructionKind.Slice_Skip_Var_Take_Const => (2, 1),
            
            StackInstructionKind.Skip_Generic => (1, 1),
            StackInstructionKind.Skip_Binary => (2, 1),
            
            StackInstructionKind.Take_Generic => (1, 1),
            StackInstructionKind.Take_Binary => (2, 1),
            
            StackInstructionKind.Skip_Head_Var => (2, 1),
            StackInstructionKind.Skip_Head_Const => (1, 1),
            
            StackInstructionKind.Head_Generic => (1, 1),

            StackInstructionKind.Reverse => (1, 1),
            
            StackInstructionKind.BuildList =>
            (instruction.TakeCount
            ?? throw new System.Exception(
                "Missing TakeCount for BuildList instruction"),
            1),

            StackInstructionKind.Equal_Binary_Var => (2, 1),
            StackInstructionKind.Not_Equal_Binary_Var => (2, 1),
            StackInstructionKind.Equal_Binary_Const => (1, 1),
            StackInstructionKind.Not_Equal_Binary_Const => (1, 1),
            StackInstructionKind.Equal_Generic => (1, 1),
            StackInstructionKind.Negate => (1, 1),

            StackInstructionKind.Int_Is_Sorted_Asc_Generic => (1, 1),
            StackInstructionKind.Int_Less_Than_Binary => (2, 1),
            StackInstructionKind.Int_Less_Than_Or_Equal_Binary => (2, 1),
            StackInstructionKind.Int_Less_Than_Const => (1, 1),
            StackInstructionKind.Int_Less_Than_Or_Equal_Const => (1, 1),

            StackInstructionKind.Jump_If_True_Const => (1, 0),
            StackInstructionKind.Jump_Const => (0, 0),
            StackInstructionKind.Return => (1, 0),
            StackInstructionKind.Parse_And_Eval => (2, 1),

            StackInstructionKind.Int_Add_Binary => (2, 1),
            StackInstructionKind.Int_Add_Const => (1, 1),
            StackInstructionKind.Int_Add_Generic => (1, 1),
            StackInstructionKind.Int_Sub_Binary => (2, 1),
            StackInstructionKind.Int_Mul_Binary => (2, 1),
            StackInstructionKind.Int_Mul_Const => (1, 1),
            StackInstructionKind.Int_Mul_Generic => (1, 1),

            StackInstructionKind.Bit_And_Generic => (1, 1),
            StackInstructionKind.Bit_And_Binary => (2, 1),
            StackInstructionKind.Bit_And_Const => (1, 1),

            StackInstructionKind.Bit_Or_Generic => (1, 1),
            StackInstructionKind.Bit_Or_Binary => (2, 1),
            StackInstructionKind.Bit_Or_Const => (1, 1),
            
            StackInstructionKind.Bit_Xor_Generic => (1, 1),
            StackInstructionKind.Bit_Xor_Binary => (2, 1),
            
            StackInstructionKind.Bit_Not => (1, 1),
            
            StackInstructionKind.Bit_Shift_Left_Var => (2, 1),
            StackInstructionKind.Bit_Shift_Left_Const => (1, 1),
            StackInstructionKind.Bit_Shift_Left_Generic => (1, 1),
            
            StackInstructionKind.Bit_Shift_Right_Var => (2, 1),
            StackInstructionKind.Bit_Shift_Right_Const => (1, 1),
            StackInstructionKind.Bit_Shift_Right_Generic => (1, 1),

            var otherKind =>
            throw new System.NotImplementedException(
                "Unknown StackInstructionKind: " + otherKind)
        };
}


