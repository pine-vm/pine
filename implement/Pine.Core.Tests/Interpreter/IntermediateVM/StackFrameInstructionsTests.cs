using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class StackFrameInstructionsTests
{
    [Fact]
    public void Equality_returns_true_for_identical_instructions()
    {
        var instructions =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([[0]]),
                Instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(1)),
                StackInstruction.Int_Add_Binary,
                StackInstruction.Return,
                ]);

        var other =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([[0]]),
                Instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(1)),
                StackInstruction.Int_Add_Binary,
                StackInstruction.Return,
                ]);

        instructions.Should().Be(other);
        instructions.GetHashCode().Should().Be(other.GetHashCode());
    }

    [Fact]
    public void Equality_returns_false_when_instructions_differ()
    {
        var a =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([[0]]),
                Instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Return,
                ]);

        var b =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([[0]]),
                Instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(1)),
                StackInstruction.Return,
                ]);

        a.Should().NotBe(b);
    }

    [Fact]
    public void Equality_returns_false_when_parameters_differ()
    {
        var a =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([[0]]),
                Instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Return,
                ]);

        var b =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([[0], [1]]),
                Instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Return,
                ]);

        a.Should().NotBe(b);
    }

    [Fact]
    public void Equality_works_with_minimal_instructions()
    {
        var a =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([[0]]),
                Instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Return,
                ]);

        var b =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([[0]]),
                Instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Return,
                ]);

        a.Should().Be(b);
        a.GetHashCode().Should().Be(b.GetHashCode());
    }
}
