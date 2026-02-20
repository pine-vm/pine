using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class KernelBitwiseFunctionTests
{
    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_kernelEnv =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["elm-kernel-modules"])
                    ?? throw new Exception("Did not find elm-kernel-modules");

                var rootFilePaths =
                    kernelModulesTree.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("Bitwise.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        kernelModulesTree,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Extract(err => throw new Exception("Failed compiling elm-kernel-modules: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing interactive environment: " + err));
            });

    private static PineValue GetBitwiseFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Bitwise")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue ApplyBinary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    // ========== Tests for and ==========

    [Fact]
    public void And_zeros()
    {
        var result = ApplyBinary(GetBitwiseFunction("and"), Integer(0), Integer(0));
        result.Should().Be(Integer(0));
    }

    [Fact]
    public void And_identity()
    {
        // and 0xFF 0xFF == 0xFF
        var result = ApplyBinary(GetBitwiseFunction("and"), Integer(0xFF), Integer(0xFF));

        result.Should().Be(Integer(0xFF));
    }

    [Fact]
    public void And_mask()
    {
        // and 0xFF 0x0F == 0x0F
        var result = ApplyBinary(GetBitwiseFunction("and"), Integer(0xFF), Integer(0x0F));

        result.Should().Be(Integer(0x0F));
    }

    [Fact]
    public void And_no_overlap()
    {
        // and 0xF0 0x0F == 0
        var result = ApplyBinary(GetBitwiseFunction("and"), Integer(0xF0), Integer(0x0F));

        result.Should().Be(Integer(0));
    }

    [Fact]
    public void And_large_values()
    {
        // and 0xDEADBEEF 0xFFFF0000 == 0xDEAD0000
        // Note: 0xDEADBEEF in 32-bit signed = -559038737
        var result =
            ApplyBinary(
                GetBitwiseFunction("and"),
                Integer(unchecked((int)0xDEADBEEF)),
                Integer(unchecked((int)0xFFFF0000)));

        result.Should().Be(Integer(unchecked((int)0xDEAD0000)));
    }

    [Fact]
    public void And_with_negative()
    {
        // In 32-bit two's complement: -1 is 0xFFFFFFFF
        // and -1 0xFF == 0xFF
        var result = ApplyBinary(GetBitwiseFunction("and"), Integer(-1), Integer(0xFF));

        result.Should().Be(Integer(0xFF));
    }

    [Fact]
    public void And_both_negative()
    {
        // and -1 -1 == -1
        var result = ApplyBinary(GetBitwiseFunction("and"), Integer(-1), Integer(-1));

        result.Should().Be(Integer(-1));
    }

    [Fact]
    public void And_with_zero()
    {
        // and x 0 == 0
        var result = ApplyBinary(GetBitwiseFunction("and"), Integer(12345), Integer(0));

        result.Should().Be(Integer(0));
    }

    // ========== Tests for or ==========

    [Fact]
    public void Or_zeros()
    {
        var result = ApplyBinary(GetBitwiseFunction("or"), Integer(0), Integer(0));
        result.Should().Be(Integer(0));
    }

    [Fact]
    public void Or_combine()
    {
        // or 0xF0 0x0F == 0xFF
        var result = ApplyBinary(GetBitwiseFunction("or"), Integer(0xF0), Integer(0x0F));

        result.Should().Be(Integer(0xFF));
    }

    [Fact]
    public void Or_identity()
    {
        // or x 0 == x
        var result = ApplyBinary(GetBitwiseFunction("or"), Integer(42), Integer(0));

        result.Should().Be(Integer(42));
    }

    [Fact]
    public void Or_with_negative()
    {
        // or -1 0 == -1
        var result = ApplyBinary(GetBitwiseFunction("or"), Integer(-1), Integer(0));

        result.Should().Be(Integer(-1));
    }

    [Fact]
    public void Or_both_same()
    {
        // or x x == x
        var result = ApplyBinary(GetBitwiseFunction("or"), Integer(0xAB), Integer(0xAB));

        result.Should().Be(Integer(0xAB));
    }

    [Fact]
    public void Or_all_ones()
    {
        // or 0xFFFF0000 0x0000FFFF == -1 (0xFFFFFFFF in 32-bit signed)
        var result =
            ApplyBinary(
                GetBitwiseFunction("or"),
                Integer(unchecked((int)0xFFFF0000)),
                Integer(0x0000FFFF));

        result.Should().Be(Integer(-1));
    }

    // ========== Tests for xor ==========

    [Fact]
    public void Xor_zeros()
    {
        var result = ApplyBinary(GetBitwiseFunction("xor"), Integer(0), Integer(0));
        result.Should().Be(Integer(0));
    }

    [Fact]
    public void Xor_same_is_zero()
    {
        // xor x x == 0
        var result = ApplyBinary(GetBitwiseFunction("xor"), Integer(0xFF), Integer(0xFF));

        result.Should().Be(Integer(0));
    }

    [Fact]
    public void Xor_complement()
    {
        // xor 0xF0 0x0F == 0xFF
        var result = ApplyBinary(GetBitwiseFunction("xor"), Integer(0xF0), Integer(0x0F));

        result.Should().Be(Integer(0xFF));
    }

    [Fact]
    public void Xor_with_zero()
    {
        // xor x 0 == x
        var result = ApplyBinary(GetBitwiseFunction("xor"), Integer(42), Integer(0));

        result.Should().Be(Integer(42));
    }

    [Fact]
    public void Xor_negative()
    {
        // xor -1 0 == -1
        var result = ApplyBinary(GetBitwiseFunction("xor"), Integer(-1), Integer(0));

        result.Should().Be(Integer(-1));
    }

    [Fact]
    public void Xor_both_negative()
    {
        // xor -1 -1 == 0
        var result = ApplyBinary(GetBitwiseFunction("xor"), Integer(-1), Integer(-1));

        result.Should().Be(Integer(0));
    }

    // ========== Tests for complement ==========

    [Fact]
    public void Complement_zero()
    {
        // complement 0 == -1
        var result = ApplyUnary(GetBitwiseFunction("complement"), Integer(0));

        result.Should().Be(Integer(-1));
    }

    [Fact]
    public void Complement_minus_one()
    {
        // complement -1 == 0
        var result = ApplyUnary(GetBitwiseFunction("complement"), Integer(-1));

        result.Should().Be(Integer(0));
    }

    [Fact]
    public void Complement_positive()
    {
        // complement 5 == -6 (in 32-bit two's complement)
        var result = ApplyUnary(GetBitwiseFunction("complement"), Integer(5));

        result.Should().Be(Integer(-6));
    }

    [Fact]
    public void Complement_negative()
    {
        // complement -6 == 5
        var result = ApplyUnary(GetBitwiseFunction("complement"), Integer(-6));

        result.Should().Be(Integer(5));
    }

    [Fact]
    public void Complement_double_is_identity()
    {
        // complement (complement x) == x (for values in 32-bit range)
        var innerResult = ApplyUnary(GetBitwiseFunction("complement"), Integer(42));

        var result = ApplyUnary(GetBitwiseFunction("complement"), innerResult);
        result.Should().Be(Integer(42));
    }

    [Fact]
    public void Complement_max_int32()
    {
        // complement 2147483647 == -2147483648
        var result = ApplyUnary(GetBitwiseFunction("complement"), Integer(0x7FFFFFFF));

        result.Should().Be(Integer(-0x80000000));
    }

    [Fact]
    public void Complement_min_int32()
    {
        // complement -2147483648 == 2147483647
        var result = ApplyUnary(GetBitwiseFunction("complement"), Integer(-0x80000000));

        result.Should().Be(Integer(0x7FFFFFFF));
    }

    // ========== Tests for shiftLeftBy ==========
    // From elm/core: shiftLeftBy 1 5 == 10
    //               shiftLeftBy 5 1 == 32

    [Fact]
    public void ShiftLeftBy_1_5()
    {
        // shiftLeftBy 1 5 == 10
        var result = ApplyBinary(GetBitwiseFunction("shiftLeftBy"), Integer(1), Integer(5));

        result.Should().Be(Integer(10));
    }

    [Fact]
    public void ShiftLeftBy_5_1()
    {
        // shiftLeftBy 5 1 == 32
        var result = ApplyBinary(GetBitwiseFunction("shiftLeftBy"), Integer(5), Integer(1));

        result.Should().Be(Integer(32));
    }

    [Fact]
    public void ShiftLeftBy_0()
    {
        // shiftLeftBy 0 x == x
        var result = ApplyBinary(GetBitwiseFunction("shiftLeftBy"), Integer(0), Integer(42));

        result.Should().Be(Integer(42));
    }

    [Fact]
    public void ShiftLeftBy_overflow()
    {
        // shiftLeftBy 31 1 == -2147483648 (wraps to negative in 32-bit)
        var result = ApplyBinary(GetBitwiseFunction("shiftLeftBy"), Integer(31), Integer(1));

        result.Should().Be(Integer(-2147483648));
    }

    [Fact]
    public void ShiftLeftBy_negative_input()
    {
        // shiftLeftBy 1 -1 == -2 (in 32-bit)
        var result = ApplyBinary(GetBitwiseFunction("shiftLeftBy"), Integer(1), Integer(-1));

        result.Should().Be(Integer(-2));
    }

    [Fact]
    public void ShiftLeftBy_4_1()
    {
        // shiftLeftBy 4 1 == 16
        var result = ApplyBinary(GetBitwiseFunction("shiftLeftBy"), Integer(4), Integer(1));

        result.Should().Be(Integer(16));
    }

    // ========== Tests for shiftRightBy ==========
    // From elm/core: shiftRightBy 1  32 == 16
    //               shiftRightBy 2  32 == 8
    //               shiftRightBy 1 -32 == -16

    [Fact]
    public void ShiftRightBy_1_32()
    {
        // shiftRightBy 1 32 == 16
        var result = ApplyBinary(GetBitwiseFunction("shiftRightBy"), Integer(1), Integer(32));

        result.Should().Be(Integer(16));
    }

    [Fact]
    public void ShiftRightBy_2_32()
    {
        // shiftRightBy 2 32 == 8
        var result = ApplyBinary(GetBitwiseFunction("shiftRightBy"), Integer(2), Integer(32));

        result.Should().Be(Integer(8));
    }

    [Fact]
    public void ShiftRightBy_1_minus32()
    {
        // shiftRightBy 1 -32 == -16
        var result = ApplyBinary(GetBitwiseFunction("shiftRightBy"), Integer(1), Integer(-32));

        result.Should().Be(Integer(-16));
    }

    [Fact]
    public void ShiftRightBy_0()
    {
        // shiftRightBy 0 x == x
        var result = ApplyBinary(GetBitwiseFunction("shiftRightBy"), Integer(0), Integer(42));

        result.Should().Be(Integer(42));
    }

    [Fact]
    public void ShiftRightBy_negative_sign_extends()
    {
        // shiftRightBy 4 -256 == -16 (sign-propagating)
        var result = ApplyBinary(GetBitwiseFunction("shiftRightBy"), Integer(4), Integer(-256));

        result.Should().Be(Integer(-16));
    }

    [Fact]
    public void ShiftRightBy_4_256()
    {
        // shiftRightBy 4 256 == 16
        var result = ApplyBinary(GetBitwiseFunction("shiftRightBy"), Integer(4), Integer(256));

        result.Should().Be(Integer(16));
    }

    // ========== Tests for shiftRightZfBy ==========
    // From elm/core: shiftRightZfBy 1  32 == 16
    //               shiftRightZfBy 2  32 == 8
    //               shiftRightZfBy 1 -32 == 2147483632

    [Fact]
    public void ShiftRightZfBy_1_32()
    {
        // shiftRightZfBy 1 32 == 16
        var result = ApplyBinary(GetBitwiseFunction("shiftRightZfBy"), Integer(1), Integer(32));

        result.Should().Be(Integer(16));
    }

    [Fact]
    public void ShiftRightZfBy_2_32()
    {
        // shiftRightZfBy 2 32 == 8
        var result = ApplyBinary(GetBitwiseFunction("shiftRightZfBy"), Integer(2), Integer(32));

        result.Should().Be(Integer(8));
    }

    [Fact]
    public void ShiftRightZfBy_1_minus32()
    {
        // shiftRightZfBy 1 -32 == 2147483632
        var result = ApplyBinary(GetBitwiseFunction("shiftRightZfBy"), Integer(1), Integer(-32));

        result.Should().Be(Integer(2147483632));
    }

    [Fact]
    public void ShiftRightZfBy_0()
    {
        // shiftRightZfBy 0 x == x (for positive values)
        var result = ApplyBinary(GetBitwiseFunction("shiftRightZfBy"), Integer(0), Integer(42));

        result.Should().Be(Integer(42));
    }

    [Fact]
    public void ShiftRightZfBy_fills_with_zeros()
    {
        // shiftRightZfBy 1 -1 == 2147483647 (0x7FFFFFFF)
        // -1 is 0xFFFFFFFF, logical right shift by 1 is 0x7FFFFFFF
        var result = ApplyBinary(GetBitwiseFunction("shiftRightZfBy"), Integer(1), Integer(-1));

        result.Should().Be(Integer(2147483647));
    }

    [Fact]
    public void ShiftRightZfBy_4_minus256()
    {
        // shiftRightZfBy 4 -256 == 268435440
        // -256 in 32-bit unsigned = 0xFFFFFF00, >> 4 = 0x0FFFFFF0 = 268435440
        var result = ApplyBinary(GetBitwiseFunction("shiftRightZfBy"), Integer(4), Integer(-256));

        result.Should().Be(Integer(268435440));
    }

    // ========== Combined / edge case tests ==========

    [Fact]
    public void And_or_identity()
    {
        // (and x y) | (and x (complement y)) == x (for 32-bit range)
        // Simplified: or (and 0xFF 0xF0) (and 0xFF 0x0F) == 0xFF
        var and1 = ApplyBinary(GetBitwiseFunction("and"), Integer(0xFF), Integer(0xF0));

        var and2 = ApplyBinary(GetBitwiseFunction("and"), Integer(0xFF), Integer(0x0F));
        var result = ApplyBinary(GetBitwiseFunction("or"), and1, and2);
        result.Should().Be(Integer(0xFF));
    }

    [Fact]
    public void Xor_self_is_zero()
    {
        // xor 12345 12345 == 0
        var result = ApplyBinary(GetBitwiseFunction("xor"), Integer(12345), Integer(12345));

        result.Should().Be(Integer(0));
    }

    [Fact]
    public void ShiftLeftBy_then_shiftRightBy()
    {
        // shiftRightBy 3 (shiftLeftBy 3 1) == 1
        var shifted = ApplyBinary(GetBitwiseFunction("shiftLeftBy"), Integer(3), Integer(1));

        var result = ApplyBinary(GetBitwiseFunction("shiftRightBy"), Integer(3), shifted);
        result.Should().Be(Integer(1));
    }

    [Fact]
    public void Complement_and_or_relationship()
    {
        // or x (complement x) == -1 (for 32-bit range values)
        var comp = ApplyUnary(GetBitwiseFunction("complement"), Integer(0x55));

        var result = ApplyBinary(GetBitwiseFunction("or"), Integer(0x55), comp);
        result.Should().Be(Integer(-1));
    }

    [Fact]
    public void And_commutative()
    {
        var ab = ApplyBinary(GetBitwiseFunction("and"), Integer(0xABCD), Integer(0x1234));
        var ba = ApplyBinary(GetBitwiseFunction("and"), Integer(0x1234), Integer(0xABCD));
        ab.Should().Be(ba);
    }

    [Fact]
    public void Or_commutative()
    {
        var ab = ApplyBinary(GetBitwiseFunction("or"), Integer(0xABCD), Integer(0x1234));
        var ba = ApplyBinary(GetBitwiseFunction("or"), Integer(0x1234), Integer(0xABCD));
        ab.Should().Be(ba);
    }

    [Fact]
    public void Xor_commutative()
    {
        var ab = ApplyBinary(GetBitwiseFunction("xor"), Integer(0xABCD), Integer(0x1234));
        var ba = ApplyBinary(GetBitwiseFunction("xor"), Integer(0x1234), Integer(0xABCD));
        ab.Should().Be(ba);
    }
}
