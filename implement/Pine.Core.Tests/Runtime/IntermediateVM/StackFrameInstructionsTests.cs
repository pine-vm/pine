using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class StackFrameInstructionsTests
{
    [Fact]
    public void Stack_effect_queries_for_all_instruction_kinds_do_not_allocate()
    {
        var instructions =
            Enum.GetValues<StackInstructionKind>()
            .Select(kind => new StackInstruction(kind, TakeCount: 3, PopCount: 2, LocalIndices: [0, 1]))
            .ToArray();

        static int Query(StackInstruction[] items)
        {
            var total = 0;

            foreach (var instruction in items)
            {
                var effect = StackInstruction.GetStackEffect(instruction);
                total += effect.PopCount + effect.PushCount;
            }

            return total;
        }

        var expected = Query(instructions);

        for (var iteration = 0; iteration < 100; iteration++)
            Query(instructions);

        var before = GC.GetAllocatedBytesForCurrentThread();
        var actual = 0;

        for (var iteration = 0; iteration < 100; iteration++)
            actual += Query(instructions);

        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;

        actual.Should().Be(expected * 100);
        allocated.Should().Be(0);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(3)]
    public void Direct_call_stack_effects_use_return_slots_without_linking(int returnCount)
    {
        var layout =
            new ReturnValueLayout(
                new ReturnValueLayout.Node.List(
                    [
                    .. Enumerable.Range(0, returnCount).Select(index => new ReturnValueLayout.Node.Component(index, []))
                    ]),
                [.. Enumerable.Range(0, returnCount).Select(index => new ReturnValueLayout.Slot([index], [], []))]);

        var target =
            new DirectInvocation(Expression.EnvironmentInstance, PineValue.EmptyList, StaticFunctionInterface.Generic)
            {
                ReturnLayout = layout,
                StaticInstructions =
                new(
                    () => throw new InvalidOperationException("Stack effects must not link targets."))
            };

        var instruction =
            new StackInstruction(
                StackInstructionKind.Invoke_StackFrame_Const,
                TakeCount: 1,
                OptimizedInvocation: target);

        var returned = StackInstruction.Return with { TakeCount = returnCount };

        StackInstruction.GetStackEffect(instruction).Should().Be(new StackInstruction.StackEffect(1, returnCount));
        StackInstruction.GetStackEffect(returned).Should().Be(new StackInstruction.StackEffect(returnCount, 0));

        StackFrameInstructions.ComputeMaxStackUsage(
            [StackInstruction.Push_Literal(PineValue.EmptyList), instruction, returned])
            .Should().Be(Math.Max(1, returnCount));
    }

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
