using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Immutable;
using System.Linq;
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
                StackInstruction.Eval_Binary,
                StackInstruction.Return,
            ];

        var graph = PineControlFlowGraph.FromInstructions(instructions);

        graph.Blocks.Should().HaveCount(2);
        graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Invoke>();
        graph.Blocks[1].Terminator.Should().BeOfType<PineControlFlowTerminator.Return>();
        graph.LowerToStackInstructions().Should().Equal(instructions);
    }

    [Fact]
    public void Round_trip_preserves_constant_expression_invoke()
    {
        StackInstruction[] instructions =
            [
                StackInstruction.Local_Get(0),
                StackInstruction.Eval_Const(PineValue.EmptyList),
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

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Constant_boolean_switch_branches_are_forwarded(bool branchWhenTrue)
    {
        var selectorA = PineValue.Blob([1]);
        var selectorB = PineValue.Blob([2]);
        var falseResult = PineValue.Blob([3]);
        var trueResult = PineValue.Blob([4]);
        var comparedBoolean = branchWhenTrue ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;

        StackInstruction[] instructions =
            [
                StackInstruction.Local_Get(0),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty
                    .Add(selectorA, 3)
                    .Add(selectorB, 3)),
                StackInstruction.Push_Literal(PineKernelValues.FalseValue),
                StackInstruction.Jump_Unconditional(2),
                StackInstruction.Push_Literal(PineKernelValues.TrueValue),
                StackInstruction.Jump_If_Equal(3, comparedBoolean),
                StackInstruction.Push_Literal(falseResult),
                StackInstruction.Return,
                StackInstruction.Push_Literal(trueResult),
                StackInstruction.Return,
            ];

        var optimized =
            PineControlFlowGraph
            .FromInstructions(instructions)
            .ForwardConstantBooleanBranches()
            .LowerToStackInstructions();

        optimized.Select(instruction => instruction.ToString()).Should().Equal(
            [
                StackInstruction.Local_Get(0).ToString(),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty
                    .Add(selectorA, 3)
                    .Add(selectorB, 3)).ToString(),
                StackInstruction.Push_Literal(branchWhenTrue ? falseResult : trueResult).ToString(),
                StackInstruction.Return.ToString(),
                StackInstruction.Push_Literal(branchWhenTrue ? trueResult : falseResult).ToString(),
                StackInstruction.Return.ToString(),
            ]);
    }

    [Fact]
    public void Constant_boolean_conditional_branches_are_forwarded()
    {
        var conditionValue = PineValue.Blob([1]);
        var falseResult = PineValue.Blob([2]);
        var trueResult = PineValue.Blob([3]);

        StackInstruction[] instructions =
            [
                StackInstruction.Local_Get(0),
                StackInstruction.Jump_If_Equal(3, conditionValue),
                StackInstruction.Push_Literal(PineKernelValues.FalseValue),
                StackInstruction.Jump_Unconditional(2),
                StackInstruction.Push_Literal(PineKernelValues.TrueValue),
                StackInstruction.Jump_If_Equal(3, PineKernelValues.TrueValue),
                StackInstruction.Push_Literal(falseResult),
                StackInstruction.Return,
                StackInstruction.Push_Literal(trueResult),
                StackInstruction.Return,
            ];

        var optimized =
            PineControlFlowGraph
            .FromInstructions(instructions)
            .ForwardConstantBooleanBranches()
            .LowerToStackInstructions();

        optimized.Should().Equal(
            [
                StackInstruction.Local_Get(0),
                StackInstruction.Jump_If_Equal(3, conditionValue),
                StackInstruction.Push_Literal(falseResult),
                StackInstruction.Return,
                StackInstruction.Push_Literal(trueResult),
                StackInstruction.Return,
            ]);
    }

    [Fact]
    public void Constant_boolean_branches_preserve_other_stack_values()
    {
        var selector = PineValue.Blob([1]);
        var falseResult = PineValue.Blob([2]);
        var trueResult = PineValue.Blob([3]);

        StackInstruction[] instructions =
            [
                StackInstruction.Local_Get(0),
                StackInstruction.Local_Get(1),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty.Add(selector, 3)),
                StackInstruction.Push_Literal(PineKernelValues.FalseValue),
                StackInstruction.Jump_Unconditional(2),
                StackInstruction.Push_Literal(PineKernelValues.TrueValue),
                StackInstruction.Jump_If_Equal(4, PineKernelValues.TrueValue),
                StackInstruction.Pop,
                StackInstruction.Push_Literal(falseResult),
                StackInstruction.Return,
                StackInstruction.Pop,
                StackInstruction.Push_Literal(trueResult),
                StackInstruction.Return,
            ];

        var optimized =
            PineControlFlowGraph
            .FromInstructions(instructions)
            .ForwardConstantBooleanBranches()
            .LowerToStackInstructions();

        optimized.Select(instruction => instruction.ToString()).Should().Equal(
            [
                StackInstruction.Local_Get(0).ToString(),
                StackInstruction.Local_Get(1).ToString(),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty.Add(selector, 4)).ToString(),
                StackInstruction.Pop.ToString(),
                StackInstruction.Push_Literal(falseResult).ToString(),
                StackInstruction.Return.ToString(),
                StackInstruction.Pop.ToString(),
                StackInstruction.Push_Literal(trueResult).ToString(),
                StackInstruction.Return.ToString(),
            ]);
    }

    [Fact]
    public void Constant_boolean_branches_with_an_unknown_predecessor_are_not_forwarded()
    {
        var selectorA = PineValue.Blob([1]);
        var selectorB = PineValue.Blob([2]);

        StackInstruction[] instructions =
            [
                StackInstruction.Local_Get(0),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty
                    .Add(selectorA, 3)
                    .Add(selectorB, 5)),
                StackInstruction.Push_Literal(PineKernelValues.FalseValue),
                StackInstruction.Jump_Unconditional(4),
                StackInstruction.Push_Literal(PineKernelValues.TrueValue),
                StackInstruction.Jump_Unconditional(2),
                StackInstruction.Local_Get(1),
                StackInstruction.Jump_If_Equal(3, PineKernelValues.TrueValue),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return,
                StackInstruction.Push_Literal(PineValue.EmptyBlob),
                StackInstruction.Return,
            ];

        var graph = PineControlFlowGraph.FromInstructions(instructions);

        graph.ForwardConstantBooleanBranches().Should().BeSameAs(graph);
    }

    [Fact]
    public void Constant_boolean_provider_with_multiple_predecessors_is_forwarded()
    {
        var selectorA = PineValue.Blob([1]);
        var selectorB = PineValue.Blob([2]);

        StackInstruction[] instructions =
            [
                StackInstruction.Local_Get(0),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty
                    .Add(selectorA, 5)
                    .Add(selectorB, 3)),
                StackInstruction.Push_Literal(PineKernelValues.FalseValue),
                StackInstruction.Jump_Unconditional(5),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Pop,
                StackInstruction.Push_Literal(PineKernelValues.TrueValue),
                StackInstruction.Jump_Unconditional(1),
                StackInstruction.Jump_If_Equal(3, PineKernelValues.TrueValue),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return,
                StackInstruction.Push_Literal(PineValue.EmptyBlob),
                StackInstruction.Return,
            ];

        var optimized =
            PineControlFlowGraph
            .FromInstructions(instructions)
            .ForwardConstantBooleanBranches();

        optimized.Blocks.Should().HaveCount(4);

        optimized.Blocks
            .SelectMany(block => block.Operations)
            .Should()
            .NotContain(
            operation =>
            operation.Instruction.Literal == PineKernelValues.TrueValue ||
            operation.Instruction.Literal == PineKernelValues.FalseValue);

        optimized.Blocks
            .Should()
            .NotContain(block => block.Terminator is PineControlFlowTerminator.ConditionalJump);

        var switchTerminator =
            optimized.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Switch>().Subject;

        var forwardingBlock = optimized.Blocks[switchTerminator.Branches[selectorB].Value];

        var forwardingJump =
            forwardingBlock.Terminator
            .Should()
            .BeOfType<PineControlFlowTerminator.Jump>()
            .Subject;

        forwardingJump.Target.Should().Be(switchTerminator.Branches[selectorA]);
        forwardingJump.Instruction.Should().BeNull();
    }

    [Fact]
    public void Constant_boolean_branch_forwarding_preserves_backward_switch_edges()
    {
        var selector = PineValue.Blob([1]);

        StackInstruction[] instructions =
            [
                StackInstruction.Local_Get(0),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty.Add(selector, 3)),
                StackInstruction.Push_Literal(PineKernelValues.FalseValue),
                StackInstruction.Jump_Unconditional(2),
                StackInstruction.Push_Literal(PineKernelValues.TrueValue),
                StackInstruction.Jump_If_Equal(-5, PineKernelValues.TrueValue),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return,
            ];

        var optimized =
            PineControlFlowGraph
            .FromInstructions(instructions)
            .ForwardConstantBooleanBranches()
            .LowerToStackInstructions();

        optimized.Select(instruction => instruction.ToString()).Should().Equal(
            [
                StackInstruction.Local_Get(0).ToString(),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty.Add(selector, -1)).ToString(),
                StackInstruction.Push_Literal(PineValue.EmptyList).ToString(),
                StackInstruction.Return.ToString(),
            ]);
    }
}
