using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class PineControlFlowGraphTests
{
    [Fact]
    public void Round_trip_preserves_instructions_and_explicit_invoke_continuation()
    {
        StackInstruction[] instructions =
        [
            StackInstruction.Local_Get(0),
            StackInstruction.Local_Get(1),
            StackInstruction.Parse_And_Eval_Binary,
            StackInstruction.Return,
        ];

        var graph = PineControlFlowGraph.FromInstructions(instructions);

        graph.Blocks.Should().HaveCount(2);
        graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Invoke>();
        graph.Blocks[1].Terminator.Should().BeOfType<PineControlFlowTerminator.Return>();
        graph.LowerToStackInstructions().Should().Equal(instructions);
    }

    [Fact]
    public void Backedge_with_empty_stack_round_trips()
    {
        StackInstruction[] instructions =
        [
            StackInstruction.Local_Get(0),
            StackInstruction.Pop,
            StackInstruction.Jump_Unconditional(-2),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Return,
        ];

        var graph = PineControlFlowGraph.FromInstructions(instructions);

        graph.Blocks[0].Terminator
            .Should().BeEquivalentTo(
                new PineControlFlowTerminator.Jump(
                    Target: new PineBlockId(0),
                    Arguments: [],
                    Instruction: instructions[2]));

        graph.LowerToStackInstructions().Should().Equal(instructions);
    }

    [Fact]
    public void Backedge_with_live_stack_value_is_rejected()
    {
        StackInstruction[] instructions =
        [
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Jump_Unconditional(-1),
        ];

        var act = () => PineControlFlowGraph.FromInstructions(instructions);

        act.Should()
            .Throw<InvalidOperationException>()
            .WithMessage("*Inconsistent stack depth*");
    }
}
