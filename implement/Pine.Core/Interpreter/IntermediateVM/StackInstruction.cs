using Pine.Core.CommonEncodings;
using Pine.Core.PineVM;
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
    /// This corresponds to the built-in function 'length' of the Pine language.
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
    /// Prepends multiple items from the stack to the list on the stack.
    /// Pops <see cref="StackInstruction.TakeCount"/> items from the stack,
    /// then pops the target list, and pushes the result of prepending those items to the list.
    /// </summary>
    Prepend_List_Items,

    /// <summary>
    /// Appends multiple items from the stack to the list on the stack.
    /// Pops the target list first, then pops <see cref="StackInstruction.TakeCount"/> items from the stack,
    /// and pushes the result of appending those items to the list.
    /// </summary>
    Append_List_Items,

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

    /// <summary>
    /// Pops the top value from the stack, passes it to the generic Pine built-in function 'skip',
    /// and pushes the result.
    /// </summary>
    Skip_Generic,

    /// <summary>
    /// Pops the skip count (top) and the source value (second) from the stack.
    /// Skips that many elements from the source value and pushes the result.
    /// If the skip count is not a valid integer, pushes an empty list.
    /// </summary>
    Skip_Binary,

    /// <summary>
    /// Skips a constant number of elements from the top value on the stack,
    /// using the count from <see cref="StackInstruction.SkipCount"/>.
    /// </summary>
    Skip_Const,

    /// <summary>
    /// Pops the top value from the stack, passes it to the generic Pine built-in function 'take',
    /// and pushes the result.
    /// </summary>
    Take_Generic,

    /// <summary>
    /// Pops the take count (top) and the source value (second) from the stack.
    /// Takes that many elements from the beginning of the source value and pushes the result.
    /// If the take count is not a valid integer, pushes an empty list.
    /// </summary>
    Take_Binary,

    /// <summary>
    /// Takes a constant number of elements from the beginning of the top value on the stack,
    /// using the count from <see cref="StackInstruction.TakeCount"/>.
    /// </summary>
    Take_Const,

    /// <summary>
    /// Takes a constant number of elements from the end of the top value on the stack,
    /// using the count from <see cref="StackInstruction.TakeCount"/>.
    /// </summary>
    Take_Last_Const,

    /// <summary>
    /// From the second value on the stack, get the element at the index of the top value on the stack.
    /// </summary>
    Skip_Head_Binary,

    /// <summary>
    /// From the top value on the stack, get the element at the index of <see cref="StackInstruction.SkipCount"/>.
    /// </summary>
    Skip_Head_Const,

    /// <summary>
    /// Pops the top value from the stack and pushes the first element (element at index 0).
    /// </summary>
    Head_Generic,

    /// <summary>
    /// Pops the top value from the stack, reverses it using the Pine built-in function 'reverse',
    /// and pushes the result.
    /// </summary>
    Reverse,

    /// <summary>
    /// Builds a list from the top <see cref="StackInstruction.TakeCount"/> values on the stack.
    /// </summary>
    Build_List,

    /// <summary>
    /// Builds a tagged list by popping <see cref="StackInstruction.TakeCount"/> values from the stack,
    /// prepending the tag from <see cref="StackInstruction.Literal"/> as the first element,
    /// and pushing the resulting list.
    /// </summary>
    Build_List_Tagged_Const,

    /// <summary>
    /// Check if the top two values on the stack are equal.
    /// </summary>
    Equal_Binary,

    /// <summary>
    /// Checks if the top two values on the stack are not equal. Pushes the negated boolean result.
    /// </summary>
    Not_Equal_Binary,

    /// <summary>
    /// Checks if the top value on the stack is equal to the literal from <see cref="StackInstruction.Literal"/>.
    /// </summary>
    Equal_Binary_Const,

    /// <summary>
    /// Checks if the top value on the stack is not equal to the literal from <see cref="StackInstruction.Literal"/>.
    /// Pushes the negated boolean result.
    /// </summary>
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
    /// Corresponding to the built-in function 'int_is_sorted_asc' of the Pine language,
    /// where we cannot proof a more specific representation is possible at compile time.
    /// </summary>
    Int_Is_Sorted_Asc_Generic,

    /// <summary>
    /// Pops the top two values as integers and checks if the second value is less than the top value.
    /// Pushes a boolean result. If either value is not a valid integer, pushes an empty list.
    /// </summary>
    Int_Less_Than_Binary,

    /// <summary>
    /// Pops the top two values as integers and checks if the second value is less than or equal to the top value.
    /// Pushes a boolean result. If either value is not a valid integer, pushes an empty list.
    /// </summary>
    Int_Less_Than_Or_Equal_Binary,

    /// <summary>
    /// Pops the top value as an integer and checks if it is less than the constant
    /// from <see cref="StackInstruction.IntegerLiteral"/>.
    /// Pushes a boolean result. If the value is not a valid integer, pushes an empty list.
    /// </summary>
    Int_Less_Than_Const,

    /// <summary>
    /// Pops the top value as an integer and checks if it is less than or equal to the constant
    /// from <see cref="StackInstruction.IntegerLiteral"/>.
    /// Pushes a boolean result. If the value is not a valid integer, pushes an empty list.
    /// </summary>
    Int_Less_Than_Or_Equal_Const,

    /// <summary>
    /// Pops the top value as an integer and checks if it is greater than or equal to the constant
    /// from <see cref="StackInstruction.IntegerLiteral"/>.
    /// Pushes a boolean result. If the value is not a valid integer, pushes an empty list.
    /// </summary>
    Int_Greater_Than_Or_Equal_Const,

    /// <summary>
    /// Pops the top value, interprets it as an unsigned integer, and checks if it is less than or equal to
    /// the constant from <see cref="StackInstruction.IntegerLiteral"/>.
    /// Pushes a boolean result. If conversion fails, pushes an empty list.
    /// </summary>
    Int_Unsigned_Less_Than_Or_Equal_Const,

    /// <summary>
    /// Pops the top value, interprets it as an unsigned integer, and checks if it is greater than or equal to
    /// the constant from <see cref="StackInstruction.IntegerLiteral"/>.
    /// Pushes a boolean result. If conversion fails, pushes an empty list.
    /// </summary>
    Int_Unsigned_Greater_Than_Or_Equal_Const,

    /// <summary>
    /// Jump to the offset from <see cref="StackInstruction.JumpOffset"/> if the top value on the stack
    /// is equal to the value from <see cref="StackInstruction.Literal"/>.
    /// </summary>
    Jump_If_Equal_Const,

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
    /// Invokes a specific stack-frame representation using values forwarded directly from the stack.
    /// </summary>
    Invoke_StackFrame_Const,

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

    /// <summary>
    /// Bitwise AND all items in the list from the top value on the stack.
    /// Not yet implemented in the VM; will throw at runtime.
    /// </summary>
    Bit_And_Generic,

    /// <summary>
    /// Pops the top two values from the stack and computes their bitwise AND.
    /// </summary>
    Bit_And_Binary,

    /// <summary>
    /// Pops the top value from the stack and computes its bitwise AND
    /// with the constant from <see cref="StackInstruction.Literal"/>.
    /// </summary>
    Bit_And_Const,

    /// <summary>
    /// Bitwise OR all items in the list from the top value on the stack.
    /// Not yet implemented in the VM; will throw at runtime.
    /// </summary>
    Bit_Or_Generic,

    /// <summary>
    /// Pops the top two values from the stack and computes their bitwise OR.
    /// </summary>
    Bit_Or_Binary,

    /// <summary>
    /// Pops the top value from the stack and computes its bitwise OR
    /// with the constant from <see cref="StackInstruction.Literal"/>.
    /// </summary>
    Bit_Or_Const,

    /// <summary>
    /// Bitwise XOR all items in the list from the top value on the stack.
    /// Not yet implemented in the VM; will throw at runtime.
    /// </summary>
    Bit_Xor_Generic,

    /// <summary>
    /// Pops the top two values from the stack and computes their bitwise XOR.
    /// </summary>
    Bit_Xor_Binary,

    /// <summary>
    /// Pops the top value from the stack and computes its bitwise NOT.
    /// </summary>
    Bit_Not,

    /// <summary>
    /// Pops the shift count (top) and the value (second) from the stack,
    /// shifts the value left by the shift count, and pushes the result.
    /// If the shift count is not a valid integer, pushes an empty list.
    /// </summary>
    Bit_Shift_Left_Binary,

    /// <summary>
    /// Pops the top value from the stack and shifts it left by the constant
    /// from <see cref="StackInstruction.ShiftCount"/>.
    /// </summary>
    Bit_Shift_Left_Const,

    /// <summary>
    /// Shift left all items in the list from the top value on the stack.
    /// Not yet implemented in the VM; will throw at runtime.
    /// </summary>
    Bit_Shift_Left_Generic,

    /// <summary>
    /// Pops the shift count (top) and the value (second) from the stack,
    /// shifts the value right by the shift count, and pushes the result.
    /// If the shift count is not a valid integer, pushes an empty list.
    /// </summary>
    Bit_Shift_Right_Binary,

    /// <summary>
    /// Pops the top value from the stack and shifts it right by the constant
    /// from <see cref="StackInstruction.ShiftCount"/>.
    /// </summary>
    Bit_Shift_Right_Const,

    /// <summary>
    /// Shift right all items in the list from the top value on the stack.
    /// Not yet implemented in the VM; will throw at runtime.
    /// </summary>
    Bit_Shift_Right_Generic,

    /// <summary>
    /// Pops the top two values from the stack and computes their logical AND.
    /// Pushes true only if both values are true.
    /// </summary>
    Logical_And_Binary,

    /// <summary>
    /// Pops a blob from the stack and trims leading zero bytes,
    /// keeping at least <see cref="StackInstruction.TakeCount"/> bytes.
    /// </summary>
    Blob_Trim_Leading_Zeros,

    /// <summary>
    /// Pops the offset (top) and the source value (second) from the stack.
    /// Checks whether the source value starting at the offset from the stack matches
    /// the prefix from <see cref="StackInstruction.Literal"/>.
    /// Pushes a boolean result.
    /// </summary>
    Starts_With_Const_At_Offset_Var,
}

/// <summary>
/// Holds information to identify the call target for a direct invocation,
/// and after linking, also a reference to the target stack frame instructions.
/// 
/// <para>
/// In the future, we will probably expand this record with a description of the interface of the callee,
/// describing both argument layout as well as how a list of returned values is to be
/// mapped to compose the canonical single return value.
/// </para>
/// </summary>
public record DirectInvocation(
    Expression Expression,
    PineValue ExpressionEncoded)
{
    StackFrameInstructions? _linkedStackFrameInstructions;

    /// <summary>
    /// The linked target stack-frame instructions, resolved after compile-time linking.
    /// </summary>
    public StackFrameInstructions? LinkedStackFrameInstructions =>
        _linkedStackFrameInstructions;

    /// <summary>
    /// Links the target stack-frame instructions for this direct invocation.
    /// </summary>
    public void SetLinkedStackFrameInstructions(
        StackFrameInstructions linkedStackFrameInstructions)
    {
        _linkedStackFrameInstructions = linkedStackFrameInstructions;
    }
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
/// <param name="TakeCount">An optional take count used in slice/build/direct invocation operations.</param>
/// <param name="JumpOffset">An optional offset for conditional or unconditional jumps.</param>
/// <param name="ShiftCount">An optional amount to shift bits, used by bitwise shift instructions.</param>
/// <param name="IntegerLiteral">An optional integer constant used by comparison and arithmetic instructions
/// such as <see cref="StackInstructionKind.Length_Equal_Const"/>,
/// <see cref="StackInstructionKind.Int_Add_Const"/>,
/// and <see cref="StackInstructionKind.Int_Mul_Const"/>.</param>
/// <param name="OptimizedInvocation">An optional <see cref="DirectInvocation"/> describing a direct stack-frame invocation target.</param>
public record StackInstruction(
    StackInstructionKind Kind,
    PineValue? Literal = null,
    BigInteger? IntegerLiteral = null,
    int? LocalIndex = null,
    int? SkipCount = null,
    int? TakeCount = null,
    int? JumpOffset = null,
    int? ShiftCount = null,
    DirectInvocation? OptimizedInvocation = null)
{
    /// <summary>
    /// The linked target stack-frame instructions, delegated from <see cref="OptimizedInvocation"/>.
    /// </summary>
    public StackFrameInstructions? LinkedStackFrameInstructions =>
        OptimizedInvocation?.LinkedStackFrameInstructions;

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Return"/> instruction.
    /// </summary>
    public static readonly StackInstruction Return =
        new(StackInstructionKind.Return);

    /// <summary>
    /// Creates an unconditional jump instruction with the given offset.
    /// </summary>
    public static StackInstruction Jump_Unconditional(int offset) =>
        new(StackInstructionKind.Jump_Const, JumpOffset: offset);

    /// <summary>
    /// Creates a new instruction to jump to the specified offset from the current instruction
    /// if the top value on the stack
    /// is equal to the given literal value.
    /// </summary>
    public static StackInstruction Jump_If_Equal(int offset, PineValue literal) =>
        new(StackInstructionKind.Jump_If_Equal_Const, JumpOffset: offset, Literal: literal);

    /// <summary>
    /// Creates a new instruction to jump to the specified offset if the top value on the stack is true.
    /// This is a convenience wrapper around <see cref="Jump_If_Equal"/> using <see cref="PineKernelValues.TrueValue"/>.
    /// </summary>
    public static StackInstruction Jump_If_True(int offset) =>
        Jump_If_Equal(offset, PineKernelValues.TrueValue);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Push_Literal"/> instruction that pushes the given literal value onto the stack.
    /// </summary>
    public static StackInstruction Push_Literal(PineValue literal) =>
        new(StackInstructionKind.Push_Literal, Literal: literal);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Local_Set"/> instruction that copies the top stack value
    /// into the local variable at the given index.
    /// </summary>
    public static StackInstruction Local_Set(int index) =>
        new(StackInstructionKind.Local_Set, LocalIndex: index);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Local_Get"/> instruction that loads the local variable
    /// at the given index and pushes it onto the stack.
    /// </summary>
    public static StackInstruction Local_Get(int index) =>
        new(StackInstructionKind.Local_Get, LocalIndex: index);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Build_List"/> instruction that builds a list
    /// from the given number of values on the stack.
    /// </summary>
    public static StackInstruction Build_List(int takeCount) =>
        new(StackInstructionKind.Build_List, TakeCount: takeCount);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Build_List_Tagged_Const"/> instruction that builds a tagged list
    /// with the given tag and the given number of values from the stack.
    /// </summary>
    public static StackInstruction Build_List_Tagged_Const(PineValue tag, int takeCount) =>
        new(StackInstructionKind.Build_List_Tagged_Const, Literal: tag, TakeCount: takeCount);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Pop"/> instruction.
    /// </summary>
    public static readonly StackInstruction Pop =
        new(StackInstructionKind.Pop);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Length"/> instruction.
    /// </summary>
    public static readonly StackInstruction Length =
        new(StackInstructionKind.Length);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Length_Equal_Const"/> instruction that checks
    /// if the length of the top value equals the given integer constant.
    /// </summary>
    public static StackInstruction Length_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Length_Equal_Const, IntegerLiteral: integerLiteral);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Reverse"/> instruction.
    /// </summary>
    public static readonly StackInstruction Reverse =
        new(StackInstructionKind.Reverse);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Head_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Head_Generic =
        new(StackInstructionKind.Head_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Concat_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Concat_Generic =
        new(StackInstructionKind.Concat_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Equal_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Equal_Generic =
        new(StackInstructionKind.Equal_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Negate"/> instruction.
    /// </summary>
    public static readonly StackInstruction Negate =
        new(StackInstructionKind.Negate);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Int_Less_Than_Const"/> instruction that checks
    /// if the top value is less than the given integer constant.
    /// </summary>
    public static StackInstruction Int_Less_Than_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Less_Than_Const, IntegerLiteral: integerLiteral);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Int_Add_Const"/> instruction that adds
    /// the given integer constant to the top value on the stack.
    /// </summary>
    public static StackInstruction Int_Add_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Add_Const, IntegerLiteral: integerLiteral);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Int_Unsigned_Add_Const"/> instruction that interprets
    /// the top value as an unsigned integer and adds the given integer constant.
    /// </summary>
    public static StackInstruction Int_Unsigned_Add_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Unsigned_Add_Const, IntegerLiteral: integerLiteral);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Int_Add_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Int_Add_Binary =
        new(StackInstructionKind.Int_Add_Binary);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Int_Add_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Int_Add_Generic =
        new(StackInstructionKind.Int_Add_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Int_Sub_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Int_Sub_Binary =
        new(StackInstructionKind.Int_Sub_Binary);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Int_Mul_Const"/> instruction that multiplies
    /// the top value on the stack by the given integer constant.
    /// </summary>
    public static StackInstruction Int_Mul_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Mul_Const, IntegerLiteral: integerLiteral);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Int_Mul_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Int_Mul_Binary =
        new(StackInstructionKind.Int_Mul_Binary);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Int_Mul_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Int_Mul_Generic =
        new(StackInstructionKind.Int_Mul_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Skip_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Skip_Binary =
        new(StackInstructionKind.Skip_Binary);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Skip_Const"/> instruction that skips
    /// the given constant number of elements from the top value.
    /// </summary>
    public static StackInstruction Skip_Const(int skipCount) =>
        new(StackInstructionKind.Skip_Const, SkipCount: skipCount);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_Not"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_Not =
        new(StackInstructionKind.Bit_Not);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_And_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_And_Generic =
        new(StackInstructionKind.Bit_And_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Skip_Head_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Skip_Head_Binary =
        new(StackInstructionKind.Skip_Head_Binary);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Slice_Skip_Var_Take_Const"/> instruction
    /// that slices with a variable skip offset and a constant take count.
    /// </summary>
    public static StackInstruction Slice_Skip_Var_Take_Const(
        int takeCount) =>
        new(StackInstructionKind.Slice_Skip_Var_Take_Const, TakeCount: takeCount);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Skip_Head_Const"/> instruction
    /// that gets the element at the given constant index from the top value.
    /// </summary>
    public static StackInstruction Skip_Head_Const(
        int skipCount) =>
        new(StackInstructionKind.Skip_Head_Const, SkipCount: skipCount);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Skip_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Skip_Generic =
        new(StackInstructionKind.Skip_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Take_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Take_Generic =
        new(StackInstructionKind.Take_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Take_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Take_Binary =
        new(StackInstructionKind.Take_Binary);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Take_Const"/> instruction that takes
    /// the given constant number of elements from the beginning of the top value.
    /// </summary>
    public static StackInstruction Take_Const(int takeCount) =>
        new(StackInstructionKind.Take_Const, TakeCount: takeCount);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Take_Last_Const"/> instruction that takes
    /// the given constant number of elements from the end of the top value.
    /// </summary>
    public static StackInstruction Take_Last_Const(int takeCount) =>
        new(StackInstructionKind.Take_Last_Const, TakeCount: takeCount);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Concat_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Concat_Binary =
        new(StackInstructionKind.Concat_Binary);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Prepend_List_Items"/> instruction that prepends
    /// the given number of items from the stack to the target list.
    /// </summary>
    public static StackInstruction Prepend_List_Items(int takeCount) =>
        new(StackInstructionKind.Prepend_List_Items, TakeCount: takeCount);

    /// <summary>
    /// Creates an <see cref="StackInstructionKind.Append_List_Items"/> instruction that appends
    /// the given number of items from the stack to the target list.
    /// </summary>
    public static StackInstruction Append_List_Items(int takeCount) =>
        new(StackInstructionKind.Append_List_Items, TakeCount: takeCount);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Equal_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Equal_Binary =
        new(StackInstructionKind.Equal_Binary);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Not_Equal_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Not_Equal_Binary =
        new(StackInstructionKind.Not_Equal_Binary);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Not_Equal_Binary_Const"/> instruction that checks
    /// if the top value is not equal to the given literal.
    /// </summary>
    public static StackInstruction Not_Equal_Binary_Const(PineValue literal) =>
        new(StackInstructionKind.Not_Equal_Binary_Const, Literal: literal);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Equal_Binary_Const"/> instruction that checks
    /// if the top value is equal to the given literal.
    /// </summary>
    public static StackInstruction Equal_Binary_Const(PineValue literal) =>
        new(StackInstructionKind.Equal_Binary_Const, Literal: literal);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Is_List_Value"/> instruction.
    /// </summary>
    public static readonly StackInstruction Is_List_Value =
        new(StackInstructionKind.Is_List_Value);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Is_Blob_Value"/> instruction.
    /// </summary>
    public static readonly StackInstruction Is_Blob_Value =
        new(StackInstructionKind.Is_Blob_Value);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Int_Is_Sorted_Asc_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Int_Is_Sorted_Asc_Generic =
        new(StackInstructionKind.Int_Is_Sorted_Asc_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Int_Less_Than_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Int_Less_Than_Binary =
        new(StackInstructionKind.Int_Less_Than_Binary);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Int_Less_Than_Or_Equal_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Int_Less_Than_Or_Equal_Binary =
        new(StackInstructionKind.Int_Less_Than_Or_Equal_Binary);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Parse_And_Eval_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Parse_And_Eval_Binary =
        new(StackInstructionKind.Parse_And_Eval_Binary);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Invoke_StackFrame_Const"/> instruction that invokes
    /// the given expression as a stack frame, forwarding the given number of arguments from the stack.
    /// The expression is automatically encoded.
    /// </summary>
    public static StackInstruction Invoke_StackFrame_Const(
        Expression expression,
        int takeCount) =>
        Invoke_StackFrame_Const(
            expression,
            expressionEncoded: ExpressionEncoding.EncodeExpressionAsValue(expression),
            takeCount);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Invoke_StackFrame_Const"/> instruction that invokes
    /// the given expression as a stack frame, forwarding the given number of arguments from the stack.
    /// Uses the provided pre-encoded expression value.
    /// </summary>
    public static StackInstruction Invoke_StackFrame_Const(
        Expression expression,
        PineValue expressionEncoded,
        int takeCount) =>
        new(
            StackInstructionKind.Invoke_StackFrame_Const,
            TakeCount: takeCount,
            OptimizedInvocation: new DirectInvocation(expression, expressionEncoded));

    /// <summary>
    /// Links the target stack-frame instructions for this instruction's <see cref="OptimizedInvocation"/>.
    /// Only valid for instructions with <see cref="StackInstructionKind.Invoke_StackFrame_Const"/> kind.
    /// </summary>
    public void SetLinkedStackFrameInstructions(
        StackFrameInstructions linkedStackFrameInstructions)
    {
        if (Kind is not StackInstructionKind.Invoke_StackFrame_Const)
        {
            throw new InvalidOperationException(
                "Can link stack-frame instructions only for " +
                nameof(StackInstructionKind.Invoke_StackFrame_Const) + ".");
        }

        if (OptimizedInvocation is not { } invocation)
        {
            throw new InvalidOperationException(
                "Cannot link: " + nameof(OptimizedInvocation) + " is null.");
        }

        invocation.SetLinkedStackFrameInstructions(linkedStackFrameInstructions);
    }

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Bit_And_Const"/> instruction that computes
    /// the bitwise AND of the top value with the given constant.
    /// </summary>
    public static StackInstruction Bit_And_Const(PineValue blobValue) =>
        new(StackInstructionKind.Bit_And_Const, Literal: blobValue);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_And_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_And_Binary =
        new(StackInstructionKind.Bit_And_Binary);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_Or_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_Or_Generic =
        new(StackInstructionKind.Bit_Or_Generic);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Bit_Or_Const"/> instruction that computes
    /// the bitwise OR of the top value with the given constant.
    /// </summary>
    public static StackInstruction Bit_Or_Const(PineValue blobValue) =>
        new(StackInstructionKind.Bit_Or_Const, Literal: blobValue);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_Or_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_Or_Binary =
        new(StackInstructionKind.Bit_Or_Binary);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_Xor_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_Xor_Generic =
        new(StackInstructionKind.Bit_Xor_Generic);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_Xor_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_Xor_Binary =
        new(StackInstructionKind.Bit_Xor_Binary);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Bit_Shift_Left_Const"/> instruction that shifts
    /// the top value left by the given constant number of bits.
    /// </summary>
    public static StackInstruction Bit_Shift_Left_Const(int shiftCount) =>
        new(StackInstructionKind.Bit_Shift_Left_Const, ShiftCount: shiftCount);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_Shift_Left_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_Shift_Left_Binary =
        new(StackInstructionKind.Bit_Shift_Left_Binary);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_Shift_Left_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_Shift_Left_Generic =
        new(StackInstructionKind.Bit_Shift_Left_Generic);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Bit_Shift_Right_Const"/> instruction that shifts
    /// the top value right by the given constant number of bits.
    /// </summary>
    public static StackInstruction Bit_Shift_Right_Const(int shiftCount) =>
        new(StackInstructionKind.Bit_Shift_Right_Const, ShiftCount: shiftCount);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_Shift_Right_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_Shift_Right_Binary =
        new(StackInstructionKind.Bit_Shift_Right_Binary);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Bit_Shift_Right_Generic"/> instruction.
    /// </summary>
    public static readonly StackInstruction Bit_Shift_Right_Generic =
        new(StackInstructionKind.Bit_Shift_Right_Generic);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Int_Less_Than_Or_Equal_Const"/> instruction that checks
    /// if the top value is less than or equal to the given integer constant.
    /// </summary>
    public static StackInstruction Int_Less_Than_Or_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Less_Than_Or_Equal_Const, IntegerLiteral: integerLiteral);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Int_Greater_Than_Or_Equal_Const"/> instruction that checks
    /// if the top value is greater than or equal to the given integer constant.
    /// </summary>
    public static StackInstruction Int_Greater_Than_Or_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Greater_Than_Or_Equal_Const, IntegerLiteral: integerLiteral);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Int_Unsigned_Less_Than_Or_Equal_Const"/> instruction that
    /// interprets the top value as an unsigned integer and checks if it is less than or equal to the given constant.
    /// </summary>
    public static StackInstruction Int_Unsigned_Less_Than_Or_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Unsigned_Less_Than_Or_Equal_Const, IntegerLiteral: integerLiteral);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Int_Unsigned_Greater_Than_Or_Equal_Const"/> instruction that
    /// interprets the top value as an unsigned integer and checks if it is greater than or equal to the given constant.
    /// </summary>
    public static StackInstruction Int_Unsigned_Greater_Than_Or_Equal_Const(BigInteger integerLiteral) =>
        new(StackInstructionKind.Int_Unsigned_Greater_Than_Or_Equal_Const, IntegerLiteral: integerLiteral);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Slice_Skip_Var_Take_Var"/> instruction.
    /// </summary>
    public static readonly StackInstruction Slice_Skip_Var_Take_Var =
        new(StackInstructionKind.Slice_Skip_Var_Take_Var);

    /// <summary>
    /// A pre-built <see cref="StackInstructionKind.Logical_And_Binary"/> instruction.
    /// </summary>
    public static readonly StackInstruction Logical_And_Binary =
        new(StackInstructionKind.Logical_And_Binary);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Blob_Trim_Leading_Zeros"/> instruction that trims
    /// leading zero bytes from a blob, keeping at least the given number of bytes.
    /// </summary>
    public static StackInstruction Blob_Trim_Leading_Zeros(int minRemainingCount) =>
        new(StackInstructionKind.Blob_Trim_Leading_Zeros, TakeCount: minRemainingCount);

    /// <summary>
    /// Creates a <see cref="StackInstructionKind.Starts_With_Const_At_Offset_Var"/> instruction that checks
    /// whether the source value starting at a variable offset matches the given prefix.
    /// </summary>
    public static StackInstruction Starts_With_Const_At_Offset_Var(PineValue startValue) =>
        new(StackInstructionKind.Starts_With_Const_At_Offset_Var, Literal: startValue);


    /// <inheritdoc/>
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

    /// <summary>
    /// Renders a human-readable display string for an invocation expression.
    /// </summary>
    public static string RenderInvocationExpression(
        Expression expression)
    {
        if (expression is Expression.Environment)
            return nameof(Expression.Environment);

        if (expression is Expression.Literal literalExpression &&
            StringEncoding.StringFromValue(literalExpression.Value).IsOkOrNull() is { } literalString)
        {
            return literalString;
        }

        return expression.ToString();
    }

    /// <summary>
    /// Returns a default human-readable display string for a <see cref="PineValue"/>,
    /// showing type, size, and a compact representation of the content.
    /// </summary>
    public static string LiteralDisplayStringDefault(PineValue value) =>
        value switch
        {
            PineValue.BlobValue blob =>
            "Blob [" +
            CommandLineInterface.FormatIntegerForDisplay(blob.Bytes.Length) +
            "] ("
            +
            (IntegerEncoding.ParseSignedIntegerRelaxed(blob.Bytes.Span).IsOkOrNullable() is { } asInt
            ?
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

            _ =>
            throw new NotImplementedException(
                "Unknown PineValue: " + value)
        };


    /// <summary>
    /// Describes the stack effects and display arguments of a single instruction.
    /// </summary>
    public record struct InstructionDetails(
        int PopCount,
        int PushCount,
        IReadOnlyList<string> Arguments);

    /// <summary>
    /// Gets the <see cref="InstructionDetails"/> for the given instruction,
    /// using <see cref="LiteralDisplayStringDefault"/> for literal formatting.
    /// </summary>
    public static InstructionDetails GetDetails(StackInstruction instruction) =>
        GetDetails(
            instruction,
            LiteralDisplayStringDefault);

    /// <summary>
    /// Gets the <see cref="InstructionDetails"/> for the given instruction,
    /// using the provided function to format literal values for display.
    /// </summary>
    public static InstructionDetails GetDetails(
        StackInstruction instruction,
        Func<PineValue, string> literalDisplayString) =>
        instruction.Kind switch
        {
            StackInstructionKind.Push_Literal =>
            new InstructionDetails(
                PopCount: 0,
                PushCount: 1,
                [
                literalDisplayString(
                    instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for PushLiteral instruction"))
                ]),

            StackInstructionKind.Local_Set =>
            new InstructionDetails(
                PopCount: 0,
                PushCount: 0,
                [
                instruction.LocalIndex?.ToString()
                ?? throw new Exception(
                    "Missing LocalIndex for LocalSet instruction")
                ]),

            StackInstructionKind.Local_Get =>
            new InstructionDetails(
                PopCount: 0,
                PushCount: 1,
                [
                instruction.LocalIndex?.ToString()
                ?? throw new Exception(
                    "Missing LocalIndex for LocalGet instruction")
                ]),

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
                [
                instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for LengthEqualConst instruction")
                ]),

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

            StackInstructionKind.Prepend_List_Items =>
            new InstructionDetails(
                PopCount:
                (instruction.TakeCount
                ?? throw new Exception(
                    "Missing TakeCount for Prepend_List_Items instruction")) + 1,
                PushCount: 1,
                [
                instruction.TakeCount?.ToString()
                ?? throw new Exception(
                    "Missing TakeCount for Prepend_List_Items instruction")
                ]),

            StackInstructionKind.Append_List_Items =>
            new InstructionDetails(
                PopCount:
                (instruction.TakeCount
                ?? throw new Exception(
                    "Missing TakeCount for Append_List_Items instruction")) + 1,
                PushCount: 1,
                [
                instruction.TakeCount?.ToString()
                ?? throw new Exception(
                    "Missing TakeCount for Append_List_Items instruction")
                ]),

            StackInstructionKind.Slice_Skip_Var_Take_Var =>
            new InstructionDetails(
                PopCount: 3,
                PushCount: 1,
                []),

            StackInstructionKind.Slice_Skip_Var_Take_Const =>
            new InstructionDetails(
                PopCount: 2,
                PushCount: 1,
                [
                instruction.TakeCount?.ToString()
                ??
                throw new Exception(
                    "Missing TakeCount for SliceSkipVarTakeConst instruction")
                ]),

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
                [
                instruction.SkipCount?.ToString()
                ??
                throw new Exception(
                    "Missing SkipCount for SkipConst instruction")
                ]),

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
                [
                instruction.TakeCount?.ToString()
                ??
                throw new Exception(
                    "Missing TakeCount for TakeConst instruction")
                ]),

            StackInstructionKind.Take_Last_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [
                instruction.TakeCount?.ToString()
                ??
                throw new Exception(
                    "Missing TakeCount for TakeLastConst instruction")
                ]),

            StackInstructionKind.Skip_Head_Binary =>
            new InstructionDetails(
                PopCount: 2,
                PushCount: 1,
                []),

            StackInstructionKind.Skip_Head_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [
                instruction.SkipCount?.ToString()
                ??
                throw new Exception(
                    "Missing SkipCount for SkipHeadConst instruction")
                ]),

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
                [
                instruction.TakeCount?.ToString()
                ?? throw new Exception(
                    "Missing TakeCount for BuildList instruction")
                ]),

            StackInstructionKind.Build_List_Tagged_Const =>
            new InstructionDetails(
                PopCount:
                instruction.TakeCount
                ?? throw new Exception(
                    "Missing TakeCount for BuildList instruction"),
                PushCount: 1,
                [
                literalDisplayString(
                    instruction.Literal
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
                [
                literalDisplayString(
                    instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for EqualBinaryConst instruction"))
                ]),

            StackInstructionKind.Not_Equal_Binary_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [
                literalDisplayString(
                    instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for NotEqualBinaryConst instruction"))
                ]),

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
                [
                instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for IntLessThanConst instruction")
                ]),

            StackInstructionKind.Int_Less_Than_Or_Equal_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [
                instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for IntLessThanOrEqualConst instruction")
                ]),

            StackInstructionKind.Int_Unsigned_Less_Than_Or_Equal_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [
                instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for Int_Unsigned_Less_Than_Or_Equal_Const instruction")
                ]),

            StackInstructionKind.Int_Greater_Than_Or_Equal_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [
                instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for IntGreaterThanOrEqualConst instruction")
                ]),

            StackInstructionKind.Int_Unsigned_Greater_Than_Or_Equal_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [
                instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for Int_Unsigned_Greater_Than_Or_Equal_Const instruction")
                ]),

            StackInstructionKind.Jump_If_Equal_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 0,
                [
                literalDisplayString(
                    instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for Jump_If_Equal_Const instruction")),
                instruction.JumpOffset?.ToString()
                ?? throw new Exception(
                    "Missing JumpOffset for Jump_If_Equal_Const instruction")
                ]),

            StackInstructionKind.Jump_Const =>
            new InstructionDetails(
                PopCount: 0,
                PushCount: 0,
                [
                instruction.JumpOffset?.ToString()
                ?? throw new Exception(
                    "Missing JumpOffset for JumpConst instruction")
                ]),

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

            StackInstructionKind.Invoke_StackFrame_Const =>
            new InstructionDetails(
                PopCount:
                instruction.TakeCount
                ?? throw new Exception(
                    "Missing TakeCount for Invoke_StackFrame_Const instruction"),
                PushCount: 1,
                [
                RenderInvocationExpression(
                    instruction.OptimizedInvocation?.Expression
                    ?? throw new Exception(
                        "Missing OptimizedInvocation for Invoke_StackFrame_Const instruction")),
                instruction.TakeCount?.ToString()
                ?? throw new Exception(
                    "Missing TakeCount for Invoke_StackFrame_Const instruction")
                ]),

            StackInstructionKind.Int_Add_Binary =>
            new InstructionDetails(
                PopCount: 2,
                PushCount: 1,
                []),

            StackInstructionKind.Int_Add_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [
                instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for IntAddConst instruction")
                ]),

            StackInstructionKind.Int_Unsigned_Add_Const =>
            new InstructionDetails(
                PopCount: 1,
                PushCount: 1,
                [
                instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for IntUnsignedAddConst instruction")
                ]),

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
                [
                instruction.IntegerLiteral?.ToString()
                ?? throw new Exception(
                    "Missing IntegerLiteral for IntMulConst instruction")
                ]),

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
                [
                literalDisplayString(
                    instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for BitAndConst instruction"))
                ]),

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
                [
                literalDisplayString(
                    instruction.Literal
                    ?? throw new Exception(
                        "Missing Literal for BitOrConst instruction"))
                ]),

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
                [
                instruction.ShiftCount?.ToString()
                ?? throw new Exception(
                    "Missing ShiftCount for BitShiftLeftConst instruction")
                ]),

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
                [
                instruction.ShiftCount?.ToString()
                ?? throw new Exception(
                    "Missing ShiftCount for BitShiftRightConst instruction")
                ]),

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
                [
                literalDisplayString(
                    instruction.Literal
                    ??
                    throw new Exception("Missing Literal for Starts_With_Const_At_Offset_Var instruction"))
                ]),

            var otherKind =>
            throw new NotImplementedException(
                "Unknown StackInstructionKind: " + otherKind)
        };
}
