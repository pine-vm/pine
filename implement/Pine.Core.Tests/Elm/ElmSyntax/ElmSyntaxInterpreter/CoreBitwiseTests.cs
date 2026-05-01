using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Text;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises every function exposed by <c>elm-kernel-modules/Bitwise.elm</c> through
/// <see cref="ElmInterpreter"/>. The Bitwise module is loaded verbatim from
/// <see cref="BundledFiles.CompilerSourceContainerFilesDefault"/>, so the interpreter has to
/// faithfully drive the same control flow that the production compiler would otherwise lower
/// to native Pine - including the use of raw blob values produced by kernel functions like
/// <c>bit_and</c> and <c>bit_or</c> that do not decode as canonical Elm integers or chars.
/// </summary>
public class CoreBitwiseTests
{
    private static readonly Lazy<IReadOnlyDictionary<DeclQualifiedName, Core.Elm.ElmSyntax.SyntaxModel.Declaration>> s_declarations =
        new(LoadBitwiseDeclarations);

    private static IReadOnlyDictionary<DeclQualifiedName, Core.Elm.ElmSyntax.SyntaxModel.Declaration>
        LoadBitwiseDeclarations()
    {
        var bitwiseNode =
            BundledFiles.ElmKernelModulesDefault.Value
            .GetNodeAtPath(["Bitwise.elm"])
            ?? throw new Exception("Did not find elm-kernel-modules/Bitwise.elm in bundled files.");

        if (bitwiseNode is not Files.FileTree.FileNode bitwiseFile)
        {
            throw new Exception(
                "Expected elm-kernel-modules/Bitwise.elm to be a file node, but got: " + bitwiseNode.GetType());
        }

        var bitwiseSource = Encoding.UTF8.GetString(bitwiseFile.Bytes.Span);

        return InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(bitwiseSource);
    }

    private static ElmValue Invoke(string functionName, params ElmValue[] arguments) =>
        ElmInterpreter.Interpret(
            new DeclQualifiedName([], functionName),
            arguments,
            s_declarations.Value)
        .Extract(err => throw new Exception(err.ToString()));

    private static long InvokeBinaryInt(string functionName, int a, int b)
    {
        var result = Invoke(functionName, ElmValue.Integer(a), ElmValue.Integer(b));

        if (result is not ElmValue.ElmInteger elmInt)
        {
            throw new Exception(
                "Expected an ElmInteger result from '" + functionName +
                "' but got: " + result.GetType().FullName + " : " + result);
        }

        return (long)elmInt.Value;
    }

    private static long InvokeUnaryInt(string functionName, int a)
    {
        var result = Invoke(functionName, ElmValue.Integer(a));

        if (result is not ElmValue.ElmInteger elmInt)
        {
            throw new Exception(
                "Expected an ElmInteger result from '" + functionName +
                "' but got: " + result.GetType().FullName + " : " + result);
        }

        return (long)elmInt.Value;
    }

    // ===== Reference implementations: simulate the JavaScript-style 32-bit bitwise semantics =====
    // These mirror the documented behaviour of the Elm `Bitwise` module (which itself emulates
    // the legacy JavaScript bitwise semantics) and are used as the oracle for the interpreter.

    private static int RefAnd(int a, int b) => a & b;

    private static int RefOr(int a, int b) => a | b;

    private static int RefXor(int a, int b) => a ^ b;

    private static int RefComplement(int a) => ~a;

    private static int RefShiftLeftBy(int offset, int a) => a << offset;

    private static int RefShiftRightBy(int offset, int a) => a >> offset;

    private static int RefShiftRightZfBy(int offset, int a) => (int)((uint)a >> offset);

    // ============================================================
    // and
    // ============================================================

    [Theory]
    [InlineData(0, 0)]
    [InlineData(0, 1)]
    [InlineData(1, 0)]
    [InlineData(1, 1)]
    [InlineData(0xFF, 0x0F)]
    [InlineData(0xFFFF, 0xFF00)]
    [InlineData(0xFFFFFFF, 0x0F0F0F0F)]
    [InlineData(0x12345678, 0x0F0F0F0F)]
    [InlineData(0x7FFFFFFF, 0x7FFFFFFF)]
    [InlineData(unchecked((int)0x80000000), unchecked((int)0x80000000))]
    [InlineData(unchecked((int)0xFFFFFFFF), unchecked((int)0xFFFFFFFF))]
    [InlineData(unchecked((int)0xFFFFFFFF), 0)]
    [InlineData(-1, -1)]
    [InlineData(-1, 1)]
    [InlineData(-2, 1)]
    [InlineData(-256, 0xFF)]
    [InlineData(-1, 0x12345678)]
    public void And_matches_reference(int a, int b)
    {
        InvokeBinaryInt("and", a, b)
            .Should().Be(RefAnd(a, b));
    }

    // ============================================================
    // or
    // ============================================================

    [Theory]
    [InlineData(0, 0)]
    [InlineData(0, 1)]
    [InlineData(1, 0)]
    [InlineData(0xF0, 0x0F)]
    [InlineData(0x12345678, 0x0F0F0F0F)]
    [InlineData(0, unchecked((int)0xFFFFFFFF))]
    [InlineData(0x7FFFFFFF, unchecked((int)0x80000000))]
    [InlineData(unchecked((int)0x80000000), 0)]
    [InlineData(-1, 0)]
    [InlineData(-1, 0x7FFFFFFF)]
    [InlineData(-256, 0x000000FF)]
    [InlineData(0x12345678, -1)]
    public void Or_matches_reference(int a, int b)
    {
        InvokeBinaryInt("or", a, b)
            .Should().Be(RefOr(a, b));
    }

    // ============================================================
    // xor
    // ============================================================

    [Theory]
    [InlineData(0, 0)]
    [InlineData(0, 1)]
    [InlineData(1, 1)]
    [InlineData(0xFF, 0x0F)]
    [InlineData(0xFFFFFFF, 0x0F0F0F0F)]
    [InlineData(unchecked((int)0xFFFFFFFF), 0x12345678)]
    [InlineData(0x12345678, 0x12345678)]
    [InlineData(0x7FFFFFFF, unchecked((int)0x80000000))]
    [InlineData(-1, 0)]
    [InlineData(-1, -1)]
    [InlineData(-256, 0xFF)]
    [InlineData(-1, 0x12345678)]
    public void Xor_matches_reference(int a, int b)
    {
        InvokeBinaryInt("xor", a, b)
            .Should().Be(RefXor(a, b));
    }

    // ============================================================
    // complement
    // ============================================================

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(-1)]
    [InlineData(0xFF)]
    [InlineData(0xFFFF)]
    [InlineData(0x7FFFFFFF)]
    [InlineData(unchecked((int)0x80000000))]
    [InlineData(unchecked((int)0xFFFFFFFF))]
    [InlineData(0x12345678)]
    [InlineData(-256)]
    [InlineData(-12345)]
    public void Complement_matches_reference(int a)
    {
        InvokeUnaryInt("complement", a)
            .Should().Be(RefComplement(a));
    }

    // ============================================================
    // shiftLeftBy
    // ============================================================

    [Theory]
    [InlineData(0, 0)]
    [InlineData(0, 1)]
    [InlineData(1, 1)]
    [InlineData(2, 1)]
    [InlineData(4, 0xFF)]
    [InlineData(8, 0xFF)]
    [InlineData(16, 0xFFFF)]
    [InlineData(31, 1)]
    [InlineData(31, 2)]
    [InlineData(1, 0x7FFFFFFF)]
    [InlineData(4, -1)]
    [InlineData(8, -1)]
    [InlineData(16, -1)]
    [InlineData(8, -256)]
    [InlineData(3, 0x12345678)]
    public void ShiftLeftBy_matches_reference(int offset, int a)
    {
        InvokeBinaryInt("shiftLeftBy", offset, a)
            .Should().Be(RefShiftLeftBy(offset, a));
    }

    // ============================================================
    // shiftRightBy (arithmetic / sign-extending shift right)
    // ============================================================

    [Theory]
    [InlineData(0, 0)]
    [InlineData(0, 1)]
    [InlineData(1, 2)]
    [InlineData(1, 0x7FFFFFFF)]
    [InlineData(4, 0xFF)]
    [InlineData(4, 0xFFFFFF)]
    [InlineData(31, 0x7FFFFFFF)]
    [InlineData(1, -1)]
    [InlineData(1, -2)]
    [InlineData(8, -256)]
    [InlineData(1, unchecked((int)0x80000000))]
    [InlineData(8, unchecked((int)0x80000000))]
    [InlineData(31, unchecked((int)0x80000000))]
    [InlineData(4, 0x12345678)]
    public void ShiftRightBy_matches_reference(int offset, int a)
    {
        InvokeBinaryInt("shiftRightBy", offset, a)
            .Should().Be(RefShiftRightBy(offset, a));
    }

    // ============================================================
    // shiftRightZfBy (logical / zero-fill shift right)
    // ============================================================

    [Theory]
    [InlineData(0, 0)]
    [InlineData(0, 1)]
    [InlineData(1, 2)]
    [InlineData(1, 0x7FFFFFFF)]
    [InlineData(4, 0xFF)]
    [InlineData(4, 0xFFFFFF)]
    [InlineData(31, 0x7FFFFFFF)]
    [InlineData(1, -1)]
    [InlineData(1, -2)]
    [InlineData(8, -256)]
    [InlineData(31, -1)]
    [InlineData(1, unchecked((int)0x80000000))]
    [InlineData(8, unchecked((int)0x80000000))]
    [InlineData(31, unchecked((int)0x80000000))]
    [InlineData(4, 0x12345678)]
    [InlineData(4, -12345)]
    public void ShiftRightZfBy_matches_reference(int offset, int a)
    {
        InvokeBinaryInt("shiftRightZfBy", offset, a)
            .Should().Be(RefShiftRightZfBy(offset, a));
    }

    // ============================================================
    // Spot-check a couple of named constants in addition to the
    // exhaustive theories above. These read more clearly when a
    // single regression points at a specific scenario.
    // ============================================================

    [Fact]
    public void And_with_full_mask_returns_low_32_bits()
    {
        InvokeBinaryInt("and", unchecked((int)0xFFFFFFFF), 0x12345678)
            .Should().Be(0x12345678);
    }

    [Fact]
    public void Or_with_zero_is_identity()
    {
        InvokeBinaryInt("or", 0x12345678, 0)
            .Should().Be(0x12345678);
    }

    [Fact]
    public void Xor_self_is_zero()
    {
        InvokeBinaryInt("xor", 0x12345678, 0x12345678)
            .Should().Be(0);
    }

    [Fact]
    public void Complement_zero_is_minus_one()
    {
        InvokeUnaryInt("complement", 0)
            .Should().Be(-1);
    }

    [Fact]
    public void ShiftLeftBy_one_doubles_for_in_range_value()
    {
        InvokeBinaryInt("shiftLeftBy", 1, 0x3FFFFFFF)
            .Should().Be(0x7FFFFFFE);
    }

    [Fact]
    public void ShiftRightBy_sign_extends_negative_value()
    {
        // -1 (0xFFFFFFFF) shifted right by 1 is still -1 with sign extension.
        InvokeBinaryInt("shiftRightBy", 1, -1)
            .Should().Be(-1);
    }

    [Fact]
    public void ShiftRightZfBy_treats_negative_as_unsigned()
    {
        // -1 (0xFFFFFFFF) >>> 1 == 0x7FFFFFFF
        InvokeBinaryInt("shiftRightZfBy", 1, -1)
            .Should().Be(0x7FFFFFFF);
    }

    [Fact]
    public void ShiftRightZfBy_min_int_by_one_is_high_bit_alone()
    {
        // 0x80000000 >>> 1 == 0x40000000
        InvokeBinaryInt("shiftRightZfBy", 1, unchecked((int)0x80000000))
            .Should().Be(0x40000000);
    }

    // ============================================================
    // Characterization tests for known edge cases of this
    // Bitwise.elm implementation that diverge from a strict
    // JavaScript-style 32-bit reference. The expected values were
    // obtained by running the same Bitwise.elm source through the
    // production Pine VM, so the interpreter is required to agree
    // bit-for-bit with the production behavior.
    // ============================================================

    [Fact]
    public void ShiftLeftBy_one_of_min_int_matches_implementation()
    {
        // Pine's Bitwise.elm produces 2048 here (rather than the
        // strict JS-style 0) because of how the negative-padding
        // path interacts with the kernel bit-shift on the small
        // intermediate blob. The production VM agrees on 2048.
        InvokeBinaryInt("shiftLeftBy", 1, unchecked((int)0x80000000))
            .Should().Be(2048);
    }

    [Fact]
    public void ShiftRightBy_thirtyone_of_minus_one_matches_implementation()
    {
        // Strict JS-style arithmetic shift would give -1, but this
        // Bitwise.elm implementation collapses to 0 at offset 31.
        // The production VM agrees on 0.
        InvokeBinaryInt("shiftRightBy", 31, -1)
            .Should().Be(0);
    }

    [Fact]
    public void ShiftRightBy_four_of_neg_12345_matches_implementation()
    {
        // Strict JS-style arithmetic shift would give -772
        // (sign-extending), but this Bitwise.elm implementation
        // produces -771. The production VM agrees on -771.
        InvokeBinaryInt("shiftRightBy", 4, -12345)
            .Should().Be(-771);
    }
}
