using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Numerics;

namespace Pine.Core.Interpreter.IntermediateVM;

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
    /// <para />
    /// This corresponds to the kernel function 'length' of the Pine language.
    /// </summary>
    Length,

    /// <summary>
    /// Gets the length of the top value from the stack as integer and
    /// checks if it is equal to the constant from <see cref="StackInstruction.IntegerLiteral"/>.
    /// </summary>
    Length_Equal_Const,

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
    /// Prepends the top value from the stack to the list from the second value on the stack.
    /// </summary>
    Prepend_List_Item_Binary,

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

    Skip_Const,

    Take_Generic,

    Take_Binary,

    Take_Const,

    Take_Last_Const,

    /// <summary>
    /// From the second value on the stack, get the element at the index of the top value on the stack.
    /// </summary>
    Skip_Head_Binary,

    /// <summary>
    /// From the top value on the stack, get the element at the index of <see cref="StackInstruction.SkipCount"/>.
    /// </summary>
    Skip_Head_Const,

    Head_Generic,

    Reverse,

    /// <summary>
    /// Builds a list from the top <see cref="StackInstruction.TakeCount"/> values on the stack.
    /// </summary>
    Build_List,

    Build_List_Tagged_Const,

    /// <summary>
    /// Check if the top two values on the stack are equal.
    /// </summary>
    Equal_Binary,

    Not_Equal_Binary,

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
    /// An application of <see cref="KernelFunction.equal(PineValue)"/> which checks if the top value is a list.
    /// One way compilers form such a test is to <see cref="KernelFunction.take(PineValue)"/> zero items from the value and
    /// then compare that to an empty list.
    /// </summary>
    Is_List_Value,

    /// <summary>
    /// An application of <see cref="KernelFunction.equal(PineValue)"/> which checks if the top value is a blob.
    /// One way compilers form such a test is to <see cref="KernelFunction.take(PineValue)"/> zero items from the value and
    /// then compare that to an empty blob.
    /// </summary>
    Is_Blob_Value,

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

    Int_Greater_Than_Or_Equal_Const,

    Int_Unsigned_Less_Than_Or_Equal_Const,

    Int_Unsigned_Greater_Than_Or_Equal_Const,

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
    Parse_And_Eval_Binary,

    /// <summary>
    /// Add the top two values on the stack.
    /// </summary>
    Int_Add_Binary,

    /// <summary>
    /// Add the integer literal from <see cref="StackInstruction.IntegerLiteral"/>.
    /// </summary>
    Int_Add_Const,

    /// <summary>
    /// Interpret the top value on the stack as unsigned integer and add the integer literal from <see cref="StackInstruction.IntegerLiteral"/>.
    /// </summary>
    Int_Unsigned_Add_Const,

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

    Bit_Shift_Left_Binary,

    Bit_Shift_Left_Const,

    Bit_Shift_Left_Generic,

    Bit_Shift_Right_Binary,

    Bit_Shift_Right_Const,

    Bit_Shift_Right_Generic,

    Logical_And_Binary,

    Blob_Trim_Leading_Zeros,

    Starts_With_Const_At_Offset_Var,
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
    public static readonly StackInstruction Return =
        new(StackInstructionKind.Return);

    public static StackInstruction Jump_Unconditional(int offset) =>
        new(StackInstructionKind.Jump_Const, JumpOffset: offset);

    /// <summary>
    /// Creates a new instruction to jump to the specified offset if the top value on the stack is true.
    /// </summary>
    public static StackInstruction Jump_If_True(int offset) =>
        new(StackInstructionKind.Jump_If_True_Const, JumpOffset: offset);

    public static StackInstruction Push_Literal(PineValue literal) =>
        new(StackInstructionKind.Push_Literal, Literal: literal);

    public static StackInstruction Local_Set(int index) =>
        new(StackInstructionKind.Local_Set, LocalIndex: index);

    public static StackInstruction Local_Get(int index) =>
        new(StackInstructionKind.Local_Get, LocalIndex: index);

    public static StackInstruction Build_List(int takeCount) =>
        new(StackInstructionKind.Build_List, TakeCount: takeCount);

    public static StackInstruction Build_List_Tagged_Const(PineValue tag, int takeCount) =>
        new(StackInstructionKind.Build_List_Tagged_Const, Literal: tag, TakeCount: takeCount);

    public static readonly StackInstruction Pop =
        new(StackInstructionKind.Pop);

    public static readonly StackInstruction Length =
        new(StackInstructionKind.Length);

    public static StackInstruction Length_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Length_Equal_Const, IntegerLiteral: integerLiteral);

    public static readonly StackInstruction Reverse =
        new(StackInstructionKind.Reverse);

    public static readonly StackInstruction Head_Generic =
        new(StackInstructionKind.Head_Generic);

    public static readonly StackInstruction Concat_Generic =
        new(StackInstructionKind.Concat_Generic);

    public static readonly StackInstruction Equal_Generic =
        new(StackInstructionKind.Equal_Generic);

    public static readonly StackInstruction Negate =
        new(StackInstructionKind.Negate);

    public static StackInstruction Int_Less_Than_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Less_Than_Const, IntegerLiteral: integerLiteral);

    public static StackInstruction Int_Add_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Add_Const, IntegerLiteral: integerLiteral);

    public static StackInstruction Int_Unsigned_Add_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Unsigned_Add_Const, IntegerLiteral: integerLiteral);

    public static readonly StackInstruction Int_Add_Binary =
        new(StackInstructionKind.Int_Add_Binary);

    public static readonly StackInstruction Int_Add_Generic =
        new(StackInstructionKind.Int_Add_Generic);

    public static readonly StackInstruction Int_Sub_Binary =
        new(StackInstructionKind.Int_Sub_Binary);

    public static StackInstruction Int_Mul_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Mul_Const, IntegerLiteral: integerLiteral);

    public static readonly StackInstruction Int_Mul_Binary =
        new(StackInstructionKind.Int_Mul_Binary);

    public static readonly StackInstruction Int_Mul_Generic =
        new(StackInstructionKind.Int_Mul_Generic);

    public static readonly StackInstruction Skip_Binary =
        new(StackInstructionKind.Skip_Binary);

    public static StackInstruction Skip_Const(int skipCount) =>
        new(StackInstructionKind.Skip_Const, SkipCount: skipCount);

    public static readonly StackInstruction Bit_Not =
        new(StackInstructionKind.Bit_Not);

    public static readonly StackInstruction Bit_And_Generic =
        new(StackInstructionKind.Bit_And_Generic);

    public static readonly StackInstruction Skip_Head_Binary =
        new(StackInstructionKind.Skip_Head_Binary);

    public static StackInstruction Slice_Skip_Var_Take_Const(
        int takeCount) =>
        new(StackInstructionKind.Slice_Skip_Var_Take_Const, TakeCount: takeCount);

    public static StackInstruction Skip_Head_Const(
        int skipCount) =>
        new(StackInstructionKind.Skip_Head_Const, SkipCount: skipCount);

    public static readonly StackInstruction Skip_Generic =
        new(StackInstructionKind.Skip_Generic);

    public static readonly StackInstruction Take_Generic =
        new(StackInstructionKind.Take_Generic);

    public static readonly StackInstruction Take_Binary =
        new(StackInstructionKind.Take_Binary);

    public static StackInstruction Take_Const(int takeCount) =>
        new(StackInstructionKind.Take_Const, TakeCount: takeCount);

    public static StackInstruction Take_Last_Const(int takeCount) =>
        new(StackInstructionKind.Take_Last_Const, TakeCount: takeCount);

    public static readonly StackInstruction Concat_Binary =
        new(StackInstructionKind.Concat_Binary);

    public static readonly StackInstruction Prepend_List_Item_Binary =
        new(StackInstructionKind.Prepend_List_Item_Binary);

    public static readonly StackInstruction Equal_Binary =
        new(StackInstructionKind.Equal_Binary);

    public static readonly StackInstruction Not_Equal_Binary =
        new(StackInstructionKind.Not_Equal_Binary);

    public static StackInstruction Not_Equal_Binary_Const(PineValue literal) =>
        new(StackInstructionKind.Not_Equal_Binary_Const, Literal: literal);

    public static StackInstruction Equal_Binary_Const(PineValue literal) =>
        new(StackInstructionKind.Equal_Binary_Const, Literal: literal);

    public static readonly StackInstruction Is_List_Value =
        new(StackInstructionKind.Is_List_Value);

    public static readonly StackInstruction Is_Blob_Value =
        new(StackInstructionKind.Is_Blob_Value);

    public static readonly StackInstruction Int_Is_Sorted_Asc_Generic =
        new(StackInstructionKind.Int_Is_Sorted_Asc_Generic);

    public static readonly StackInstruction Int_Less_Than_Binary =
        new(StackInstructionKind.Int_Less_Than_Binary);

    public static readonly StackInstruction Int_Less_Than_Or_Equal_Binary =
        new(StackInstructionKind.Int_Less_Than_Or_Equal_Binary);

    public static readonly StackInstruction Parse_And_Eval_Binary =
        new(StackInstructionKind.Parse_And_Eval_Binary);

    public static StackInstruction Bit_And_Const(PineValue blobValue) =>
        new(StackInstructionKind.Bit_And_Const, Literal: blobValue);

    public static readonly StackInstruction Bit_And_Binary =
        new(StackInstructionKind.Bit_And_Binary);

    public static readonly StackInstruction Bit_Or_Generic =
        new(StackInstructionKind.Bit_Or_Generic);

    public static StackInstruction Bit_Or_Const(PineValue blobValue) =>
        new(StackInstructionKind.Bit_Or_Const, Literal: blobValue);

    public static readonly StackInstruction Bit_Or_Binary =
        new(StackInstructionKind.Bit_Or_Binary);

    public static readonly StackInstruction Bit_Xor_Generic =
        new(StackInstructionKind.Bit_Xor_Generic);

    public static readonly StackInstruction Bit_Xor_Binary =
        new(StackInstructionKind.Bit_Xor_Binary);

    public static StackInstruction Bit_Shift_Left_Const(int shiftCount) =>
        new(StackInstructionKind.Bit_Shift_Left_Const, ShiftCount: shiftCount);

    public static readonly StackInstruction Bit_Shift_Left_Binary =
        new(StackInstructionKind.Bit_Shift_Left_Binary);

    public static readonly StackInstruction Bit_Shift_Left_Generic =
        new(StackInstructionKind.Bit_Shift_Left_Generic);

    public static StackInstruction Bit_Shift_Right_Const(int shiftCount) =>
        new(StackInstructionKind.Bit_Shift_Right_Const, ShiftCount: shiftCount);

    public static readonly StackInstruction Bit_Shift_Right_Binary =
        new(StackInstructionKind.Bit_Shift_Right_Binary);

    public static readonly StackInstruction Bit_Shift_Right_Generic =
        new(StackInstructionKind.Bit_Shift_Right_Generic);

    public static StackInstruction Int_Less_Than_Or_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Less_Than_Or_Equal_Const, IntegerLiteral: integerLiteral);

    public static StackInstruction Int_Greater_Than_Or_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Greater_Than_Or_Equal_Const, IntegerLiteral: integerLiteral);

    public static StackInstruction Int_Unsigned_Less_Than_Or_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Unsigned_Less_Than_Or_Equal_Const, IntegerLiteral: integerLiteral);

    public static StackInstruction Int_Unsigned_Greater_Than_Or_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Unsigned_Greater_Than_Or_Equal_Const, IntegerLiteral: integerLiteral);

    public static readonly StackInstruction Slice_Skip_Var_Take_Var =
        new(StackInstructionKind.Slice_Skip_Var_Take_Var);

    public static readonly StackInstruction Logical_And_Binary =
        new(StackInstructionKind.Logical_And_Binary);

    public static StackInstruction Blob_Trim_Leading_Zeros(int minRemainingCount) =>
        new(StackInstructionKind.Blob_Trim_Leading_Zeros, TakeCount: minRemainingCount);

    public static StackInstruction Starts_With_Const_At_Offset_Var(PineValue startValue) =>
        new(StackInstructionKind.Starts_With_Const_At_Offset_Var, Literal: startValue);


    public override string ToString()
    {
        var details = GetDetails(this);

        var detailsText =
            details.Arguments.Count is 0
            ?
            ""
            :
            " (" +
            string.Join(" , ", details.Arguments)
            + ")";

        return Kind.ToString() + detailsText;
    }

    public static string LiteralDisplayStringDefault(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blob =>
            "Blob [" +
            CommandLineInterface.FormatIntegerForDisplay(blob.Bytes.Length) +
            "] ("
            +
            (IntegerEncoding.ParseSignedIntegerRelaxed(blob.Bytes.Span).IsOkOrNullable() is { } asInt ?
            "int " + asInt
            :
            "0x" + Convert.ToHexStringLower(blob.Bytes.Span))
            +
            ")",

            PineValue.ListValue list =>
            "List [" +
            CommandLineInterface.FormatIntegerForDisplay(list.Items.Length) +
            "] (" +
            CommandLineInterface.FormatIntegerForDisplay(list.NodesCount) +
            ")",

            _ => throw new NotImplementedException(
                "Unknown PineValue: " + value)
        };


    public record struct InstructionDetails(
        int PopCount,
        int PushCount,
        IReadOnlyList<string> Arguments);

    public static InstructionDetails GetDetails(StackInstruction instruction) =>
        GetDetails(
            instruction,
            LiteralDisplayStringDefault);

    public static InstructionDetails GetDetails(
        StackInstruction instruction,
        Func<PineValue, string> literalDisplayString) =>
        instruction.Kind switch
        {
            StackInstructionKind.Push_Literal =>
                new InstructionDetails(
                    PopCount: 0,
                    PushCount: 1,
                    [literalDisplayString(instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for PushLiteral instruction")
                    )]),

            StackInstructionKind.Local_Set =>
                new InstructionDetails(
                    PopCount: 0,
                    PushCount: 0,
                    [instruction.LocalIndex?.ToString()
                    ?? throw new Exception(
                        "Missing LocalIndex for LocalSet instruction")]),

            StackInstructionKind.Local_Get =>
                new InstructionDetails(
                    PopCount: 0,
                    PushCount: 1,
                    [instruction.LocalIndex?.ToString()
                    ?? throw new Exception(
                        "Missing LocalIndex for LocalGet instruction")]),

            StackInstructionKind.Pop =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 0,
                    []),

            StackInstructionKind.Length =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Length_Equal_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.IntegerLiteral?.ToString()
                    ?? throw new Exception(
                        "Missing IntegerLiteral for LengthEqualConst instruction")]),

            StackInstructionKind.Concat_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Concat_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Prepend_List_Item_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Slice_Skip_Var_Take_Var =>
                new InstructionDetails(
                    PopCount: 3,
                    PushCount: 1,
                    []),

            StackInstructionKind.Slice_Skip_Var_Take_Const =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    [instruction.TakeCount?.ToString()
                    ??
                    throw new Exception(
                        "Missing TakeCount for SliceSkipVarTakeConst instruction")]),

            StackInstructionKind.Skip_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Skip_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Skip_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.SkipCount?.ToString()
                    ??
                    throw new Exception(
                        "Missing SkipCount for SkipConst instruction")]),

            StackInstructionKind.Take_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Take_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Take_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.TakeCount?.ToString()
                    ??
                    throw new Exception(
                        "Missing TakeCount for TakeConst instruction")]),

            StackInstructionKind.Take_Last_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [instruction.TakeCount?.ToString()
                    ??
                    throw new Exception(
                        "Missing TakeCount for TakeLastConst instruction")]),

            StackInstructionKind.Skip_Head_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Skip_Head_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.SkipCount?.ToString()
                    ??
                    throw new Exception(
                        "Missing SkipCount for SkipHeadConst instruction")]),

            StackInstructionKind.Head_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Reverse =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Build_List =>
                new InstructionDetails(
                    PopCount:
                    instruction.TakeCount
                        ?? throw new Exception(
                            "Missing TakeCount for BuildList instruction"),
                    PushCount: 1,
                    [instruction.TakeCount?.ToString()
                        ?? throw new Exception(
                            "Missing TakeCount for BuildList instruction")]),

            StackInstructionKind.Build_List_Tagged_Const =>
                new InstructionDetails(
                    PopCount:
                    instruction.TakeCount
                    ?? throw new Exception(
                            "Missing TakeCount for BuildList instruction"),
                    PushCount: 1,
                    [literalDisplayString(instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for EqualBinaryConst instruction")),
                    instruction.TakeCount?.ToString()
                        ?? throw new Exception(
                            "Missing TakeCount for BuildList instruction")
                    ]),

            StackInstructionKind.Equal_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Not_Equal_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Equal_Binary_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [literalDisplayString(instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for EqualBinaryConst instruction")
                    )]),

            StackInstructionKind.Not_Equal_Binary_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [literalDisplayString(instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for NotEqualBinaryConst instruction")
                    )]),

            StackInstructionKind.Equal_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Is_List_Value =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Is_Blob_Value =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Negate =>

                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Int_Is_Sorted_Asc_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Int_Less_Than_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Int_Less_Than_Or_Equal_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Int_Less_Than_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.IntegerLiteral?.ToString()
                    ?? throw new Exception(
                        "Missing IntegerLiteral for IntLessThanConst instruction")]),

            StackInstructionKind.Int_Less_Than_Or_Equal_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.IntegerLiteral?.ToString()
                    ?? throw new Exception(
                        "Missing IntegerLiteral for IntLessThanOrEqualConst instruction")]),

            StackInstructionKind.Int_Unsigned_Less_Than_Or_Equal_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for Int_Unsigned_Less_Than_Or_Equal_Const instruction")]),

            StackInstructionKind.Int_Greater_Than_Or_Equal_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.IntegerLiteral?.ToString()
                    ?? throw new Exception(
                        "Missing IntegerLiteral for IntGreaterThanOrEqualConst instruction")]),

            StackInstructionKind.Int_Unsigned_Greater_Than_Or_Equal_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for Int_Unsigned_Greater_Than_Or_Equal_Const instruction")]),

            StackInstructionKind.Jump_If_True_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 0,
                    [instruction.JumpOffset?.ToString()
                    ?? throw new Exception(
                        "Missing JumpOffset for JumpIfTrueConst instruction")]),

            StackInstructionKind.Jump_Const =>
                new InstructionDetails(
                    PopCount: 0,
                    PushCount: 0,
                    [instruction.JumpOffset?.ToString()
                    ?? throw new Exception(
                        "Missing JumpOffset for JumpConst instruction")]),


            StackInstructionKind.Return =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 0,
                    []),

            StackInstructionKind.Parse_And_Eval_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Int_Add_Binary =>

                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Int_Add_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.IntegerLiteral?.ToString()
                    ?? throw new Exception(
                        "Missing IntegerLiteral for IntAddConst instruction")]),

            StackInstructionKind.Int_Unsigned_Add_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.IntegerLiteral?.ToString()
                    ?? throw new Exception(
                        "Missing IntegerLiteral for IntUnsignedAddConst instruction")]),

            StackInstructionKind.Int_Add_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Int_Sub_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Int_Mul_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Int_Mul_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.IntegerLiteral?.ToString()
                    ?? throw new Exception(
                        "Missing IntegerLiteral for IntMulConst instruction")]),

            StackInstructionKind.Int_Mul_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_And_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_And_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_And_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [literalDisplayString(instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for BitAndConst instruction"))]),

            StackInstructionKind.Bit_Or_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_Or_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_Or_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [literalDisplayString(instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for BitOrConst instruction"))]),

            StackInstructionKind.Bit_Xor_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_Xor_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_Not =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_Shift_Left_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_Shift_Left_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.ShiftCount?.ToString()
                    ?? throw new Exception(
                        "Missing ShiftCount for BitShiftLeftConst instruction")]),

            StackInstructionKind.Bit_Shift_Left_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_Shift_Right_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Bit_Shift_Right_Const =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    [instruction.ShiftCount?.ToString()
                    ?? throw new Exception(
                        "Missing ShiftCount for BitShiftRightConst instruction")]),

            StackInstructionKind.Bit_Shift_Right_Generic =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Logical_And_Binary =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    []),

            StackInstructionKind.Blob_Trim_Leading_Zeros =>
                new InstructionDetails(
                    PopCount: 1,
                    PushCount: 1,
                    []),

            StackInstructionKind.Starts_With_Const_At_Offset_Var =>
                new InstructionDetails(
                    PopCount: 2,
                    PushCount: 1,
                    [literalDisplayString(instruction.Literal
                        ??
                        throw new Exception("Missing Literal for Starts_With_Const_At_Offset_Var instruction"))
                    ]),

            var otherKind =>
            throw new NotImplementedException(
                "Unknown StackInstructionKind: " + otherKind)
        };
}


