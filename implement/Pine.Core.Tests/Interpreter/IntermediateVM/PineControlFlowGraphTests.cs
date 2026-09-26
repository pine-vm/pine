using AwesomeAssertions;
using Pine.Core.Internal;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class PineControlFlowGraphTests
{
    private static PineControlFlowFragment Ops(params StackInstruction[] instructions) =>
        PineControlFlowFragment.FromOperations(instructions);

    private static PineControlFlowNode.Conditional Conditional(
        PineValue literal,
        PineControlFlowFragment fallThrough,
        PineControlFlowFragment branch) =>
        new(literal, fallThrough, branch);

    private static PineControlFlowNode.Switch Switch(
        PineControlFlowFragment defaultBranch,
        params (PineValue Literal, PineControlFlowFragment Branch)[] cases) =>
        new(
            PineSwitchKind.Equal,
            [.. cases.Select((switchCase, index) => new PineSwitchFragmentCase(switchCase.Literal, index))],
            defaultBranch,
            [.. cases.Select(switchCase => switchCase.Branch)]);

    private static ImmutableArray<StackInstruction> Optimize(PineControlFlowFragment fragment) =>
        PineControlFlowGraph
        .FromFragment(fragment)
        .ForwardJumpsToReturn()
        .ForwardConstantBooleanBranches()
        .LowerToStackInstructions();

    [Fact]
    public void Invoke_ends_block_and_continues_in_next_block()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(1),
                    StackInstruction.Eval_Binary));

        graph.Blocks.Should().HaveCount(2);

        var invoke =
            graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Invoke>().Subject;

        invoke.Continuation.Should().Be(new PineBlockId(1));
        invoke.Arguments.Should().HaveCount(1);
        graph.Blocks[1].Parameters.Should().HaveSameCount(invoke.Arguments);
        graph.Blocks[1].Terminator.Should().BeOfType<PineControlFlowTerminator.Return>();

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Local_Get(1),
            StackInstruction.Eval_Binary,
            StackInstruction.Return);
    }

    [Fact]
    public void Constant_expression_invoke_ends_block()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Eval_Const(PineValue.EmptyList)));

        graph.Blocks.Should().HaveCount(2);
        graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Invoke>();
        graph.Blocks[1].Terminator.Should().BeOfType<PineControlFlowTerminator.Return>();

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Eval_Const(PineValue.EmptyList),
            StackInstruction.Return);
    }

    [Fact]
    public void Operation_rejects_control_transfer_instructions()
    {
        StackInstruction[] controlTransfers =
            [
            StackInstruction.Return,
            StackInstruction.Jump_Unconditional(1),
            StackInstruction.Jump_If_Equal(1, PineKernelValues.TrueValue),
            new StackInstruction(
                StackInstructionKind.Switch_Jump_If_Equal_Const,
                SwitchJumpTable: ImmutableDictionary<PineValue, int>.Empty),
            ];

        foreach (var instruction in controlTransfers)
        {
            var act = () => new PineControlFlowNode.Operation(instruction);

            act.Should().Throw<ArgumentException>();
        }
    }

    [Fact]
    public void Conditional_models_edges_with_block_arguments_and_lowers_to_offsets()
    {
        var conditionLiteral = PineValue.Blob([1]);

        var fragment =
            Ops(StackInstruction.Local_Get(1), StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    conditionLiteral,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob))))
            .AppendOperation(StackInstruction.Build_List(2));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.Blocks.Should().HaveCount(4);

        var conditional =
            graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.ConditionalJump>().Subject;

        conditional.Literal.Should().Be(conditionLiteral);
        conditional.FallThrough.Should().Be(new PineBlockId(1));
        conditional.Branch.Should().Be(new PineBlockId(2));

        // The value below the condition is passed along both edges.
        conditional.FallThroughArguments.Should().Equal(graph.Blocks[0].Operations[0].Results);
        conditional.BranchArguments.Should().Equal(graph.Blocks[0].Operations[0].Results);
        graph.Blocks[1].Parameters.Should().HaveCount(1);
        graph.Blocks[2].Parameters.Should().HaveCount(1);

        graph.Blocks[1].Terminator
            .Should().BeOfType<PineControlFlowTerminator.Jump>()
            .Which.IsFallThrough.Should().BeFalse();

        graph.Blocks[2].Terminator
            .Should().BeOfType<PineControlFlowTerminator.Jump>()
            .Which.IsFallThrough.Should().BeTrue();

        graph.Blocks[3].Parameters.Should().HaveCount(2);

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(1),
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, conditionLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Jump_Unconditional(2),
            StackInstruction.Push_Literal(PineValue.EmptyBlob),
            StackInstruction.Build_List(2),
            StackInstruction.Return);
    }

    [Fact]
    public void Jumps_to_return_are_replaced_with_return()
    {
        var conditionLiteral = PineValue.Blob([1]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    conditionLiteral,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob))));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, conditionLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Jump_Unconditional(2),
            StackInstruction.Push_Literal(PineValue.EmptyBlob),
            StackInstruction.Return);

        var forwarded = graph.ForwardJumpsToReturn();

        forwarded.Blocks[1].Terminator.Should().BeOfType<PineControlFlowTerminator.Return>();

        forwarded.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, conditionLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Return,
            StackInstruction.Push_Literal(PineValue.EmptyBlob),
            StackInstruction.Return);

        forwarded.ForwardJumpsToReturn().Should().BeSameAs(forwarded);
    }

    [Fact]
    public void Empty_join_of_nested_conditional_in_last_branch_is_removed()
    {
        var outerLiteral = PineValue.Blob([1]);
        var innerLiteral = PineValue.Blob([2]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    outerLiteral,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Local_Get(1))
                    .Append(
                        Conditional(
                            innerLiteral,
                            Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob)),
                            Ops(StackInstruction.Local_Get(2))))));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.Blocks.Should().NotContain(
            block =>
            block.Operations.IsEmpty &&
            block.Terminator is PineControlFlowTerminator.Jump);

        graph.Blocks.Should().HaveCount(6);

        var innerFallThroughJump =
            graph.Blocks[3].Terminator.Should().BeOfType<PineControlFlowTerminator.Jump>().Subject;

        innerFallThroughJump.Target.Should().Be(graph.Blocks[^1].Id);

        graph.ForwardJumpsToReturn().LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, outerLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Return,
            StackInstruction.Local_Get(1),
            StackInstruction.Jump_If_Equal(3, innerLiteral),
            StackInstruction.Push_Literal(PineValue.EmptyBlob),
            StackInstruction.Return,
            StackInstruction.Local_Get(2),
            StackInstruction.Return);
    }

    [Fact]
    public void Invoke_at_end_of_last_branch_continues_in_join()
    {
        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(
                        StackInstruction.Local_Get(0),
                        StackInstruction.Eval_Const(PineValue.EmptyList))))
            .AppendOperation(StackInstruction.Skip_Head_Const(1));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        var invoke =
            graph.Blocks
            .Select(block => block.Terminator)
            .OfType<PineControlFlowTerminator.Invoke>()
            .Single();

        var join = graph.Blocks[invoke.Continuation.Value];

        join.Operations.Select(operation => operation.Instruction)
            .Should().Equal(StackInstruction.Skip_Head_Const(1));

        graph.Blocks[1].Terminator
            .Should().BeOfType<PineControlFlowTerminator.Jump>()
            .Which.Target.Should().Be(join.Id);
    }

    [Fact]
    public void Switch_lowers_cases_sharing_a_branch_to_the_same_offset()
    {
        var selectorA = PineValue.Blob([1]);
        var selectorB = PineValue.Blob([2]);
        var selectorC = PineValue.Blob([3]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                new PineControlFlowNode.Switch(
                    PineSwitchKind.Equal,
                    [
                    new PineSwitchFragmentCase(selectorA, 0),
                    new PineSwitchFragmentCase(selectorB, 1),
                    new PineSwitchFragmentCase(selectorC, 0),
                    ],
                    Default: Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Branches:
                    [
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob)),
                    Ops(StackInstruction.Local_Get(1)),
                    ]));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        var switchTerminator =
            graph.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Switch>().Subject;

        switchTerminator.Cases.Select(switchCase => switchCase.Literal)
            .Should().Equal(selectorA, selectorB, selectorC);

        switchTerminator.Cases[0].Target.Should().Be(switchTerminator.Cases[2].Target);

        graph.ForwardJumpsToReturn().LowerToStackInstructions()
            .Select(instruction => instruction.ToString())
            .Should().Equal(
            StackInstruction.Local_Get(0).ToString(),
            new StackInstruction(
                StackInstructionKind.Switch_Jump_If_Equal_Const,
                SwitchJumpTable:
                ImmutableDictionary<PineValue, int>.Empty
                .Add(selectorA, 3)
                .Add(selectorB, 5)
                .Add(selectorC, 3)).ToString(),
            StackInstruction.Push_Literal(PineValue.EmptyList).ToString(),
            StackInstruction.Return.ToString(),
            StackInstruction.Push_Literal(PineValue.EmptyBlob).ToString(),
            StackInstruction.Return.ToString(),
            StackInstruction.Local_Get(1).ToString(),
            StackInstruction.Return.ToString());
    }

    [Fact]
    public void Slice_switch_consumes_two_values_and_preserves_case_order()
    {
        var first = PineValue.Blob([4, 5]);
        var second = PineValue.Blob([6, 7]);

        var fragment =
            Ops(StackInstruction.Local_Get(0), StackInstruction.Local_Get(1))
            .Append(
                new PineControlFlowNode.Switch(
                    PineSwitchKind.SliceSkipVarEqual,
                    [
                    new PineSwitchFragmentCase(second, 1),
                    new PineSwitchFragmentCase(first, 0),
                    ],
                    Default: Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Branches:
                    [
                    Ops(StackInstruction.Push_Literal(first)),
                    Ops(StackInstruction.Push_Literal(second)),
                    ]));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.Blocks[0].Terminator
            .Should().BeOfType<PineControlFlowTerminator.Switch>()
            .Which.Arguments.Should().BeEmpty();

        graph.ForwardJumpsToReturn().LowerToStackInstructions()[2]
            .SliceSwitchCases.Should().Equal(
            new SliceSwitchCase(second, 5),
            new SliceSwitchCase(first, 3));
    }

    [Fact]
    public void Jump_to_entry_with_empty_stack_forms_loop()
    {
        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    PineControlFlowFragment.Empty.Append(new PineControlFlowNode.JumpToEntry())));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.Blocks[2].Terminator
            .Should().BeEquivalentTo(
            new PineControlFlowTerminator.Jump(
                Target: graph.Entry,
                Arguments: [],
                IsFallThrough: false));

        graph.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Jump_If_Equal(3, PineKernelValues.TrueValue),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Jump_Unconditional(2),
            StackInstruction.Jump_Unconditional(-4),
            StackInstruction.Return);
    }

    [Fact]
    public void Jump_to_entry_with_live_stack_value_is_rejected()
    {
        var fragment =
            Ops(StackInstruction.Push_Literal(PineValue.EmptyList))
            .Append(new PineControlFlowNode.JumpToEntry());

        var act = () => PineControlFlowGraph.FromFragment(fragment);

        act.Should()
            .Throw<InvalidOperationException>()
            .WithMessage("*Edge 0 -> 0 supplies 1 arguments for 0 parameters*");
    }

    [Fact]
    public void Node_after_jump_to_entry_is_rejected()
    {
        var fragment =
            PineControlFlowFragment.Empty
            .Append(new PineControlFlowNode.JumpToEntry())
            .AppendOperation(StackInstruction.Push_Literal(PineValue.EmptyList));

        var act = () => PineControlFlowGraph.FromFragment(fragment);

        act.Should()
            .Throw<InvalidOperationException>()
            .WithMessage("*after a control transfer*");
    }

    [Fact]
    public void Branches_with_inconsistent_stack_depth_are_rejected()
    {
        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(
                        StackInstruction.Push_Literal(PineValue.EmptyList),
                        StackInstruction.Push_Literal(PineValue.EmptyList))));

        var act = () => PineControlFlowGraph.FromFragment(fragment);

        act.Should()
            .Throw<InvalidOperationException>()
            .WithMessage("*Inconsistent stack depth*");
    }

    [Fact]
    public void Stack_underflow_is_rejected()
    {
        var act = () => PineControlFlowGraph.FromFragment(Ops(StackInstruction.Pop));

        act.Should()
            .Throw<InvalidOperationException>()
            .WithMessage("*underflow*");
    }

    [Fact]
    public void Last_operation_is_only_visible_at_top_level()
    {
        var fragment =
            PineControlFlowFragment.Empty
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Local_Get(1)),
                    Ops(StackInstruction.Local_Get(2))));

        fragment.LastOperationOrNull.Should().BeNull();

        var act = () => fragment.ReplaceLastOperation(StackInstruction.Local_Get(3));

        act.Should().Throw<InvalidOperationException>();

        fragment
            .AppendOperation(StackInstruction.Local_Get(4))
            .ReplaceLastOperation(StackInstruction.Local_Get(5))
            .LastOperationOrNull.Should().Be(StackInstruction.Local_Get(5));
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

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                new PineControlFlowNode.Switch(
                    PineSwitchKind.Equal,
                    [
                    new PineSwitchFragmentCase(selectorA, 0),
                    new PineSwitchFragmentCase(selectorB, 0),
                    ],
                    Default: Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    Branches: [Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue))]))
            .Append(
                Conditional(
                    comparedBoolean,
                    Ops(StackInstruction.Push_Literal(falseResult)),
                    Ops(StackInstruction.Push_Literal(trueResult))));

        Optimize(fragment).Select(instruction => instruction.ToString()).Should().Equal(
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

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    conditionValue,
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(falseResult)),
                    Ops(StackInstruction.Push_Literal(trueResult))));

        Optimize(fragment).Should().Equal(
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

        var fragment =
            Ops(StackInstruction.Local_Get(0), StackInstruction.Local_Get(1))
            .Append(
                Switch(
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    (selector, Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue)))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Pop, StackInstruction.Push_Literal(falseResult)),
                    Ops(StackInstruction.Pop, StackInstruction.Push_Literal(trueResult))));

        Optimize(fragment).Select(instruction => instruction.ToString()).Should().Equal(
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

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Switch(
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    (selectorA, Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue))),
                    (selectorB, Ops(StackInstruction.Local_Get(1)))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob))));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        graph.ForwardConstantBooleanBranches().Should().BeSameAs(graph);
    }

    [Fact]
    public void Constant_boolean_provider_with_multiple_predecessors_is_forwarded()
    {
        var selector = PineValue.Blob([1]);
        var innerLiteral = PineValue.Blob([2]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Switch(
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    (selector,
                    Ops(StackInstruction.Local_Get(1))
                    .Append(
                        Conditional(
                            innerLiteral,
                            Ops(StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop),
                            Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob), StackInstruction.Pop)))
                    .AppendOperation(StackInstruction.Push_Literal(PineKernelValues.TrueValue)))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob))));

        var graph = PineControlFlowGraph.FromFragment(fragment);

        var providerWithMultiplePredecessors =
            graph.Blocks.Single(
                block =>
                block.Operations is [{ Instruction.Kind: StackInstructionKind.Push_Literal } pushTrue] &&
                IsBooleanLiteral(pushTrue.Instruction.Literal) &&
                PineValueInProcess.AreEqual(pushTrue.Instruction.Literal!, PineKernelValues.TrueValue));

        graph.Blocks
            .Count(
            block =>
            block.Terminator is PineControlFlowTerminator.Jump jump &&
            jump.Target == providerWithMultiplePredecessors.Id)
            .Should().Be(2);

        var optimized = graph.ForwardConstantBooleanBranches();

        optimized.Blocks
            .SelectMany(block => block.Operations)
            .Should()
            .NotContain(operation => IsBooleanLiteral(operation.Instruction.Literal));

        optimized.Blocks
            .Should()
            .NotContain(
            block =>
            block.Terminator is PineControlFlowTerminator.ConditionalJump &&
            ((PineControlFlowTerminator.ConditionalJump)block.Terminator).Literal == PineKernelValues.TrueValue);

        var switchTerminator =
            optimized.Blocks[0].Terminator.Should().BeOfType<PineControlFlowTerminator.Switch>().Subject;

        var trueResultBlock =
            optimized.Blocks.Single(
                block =>
                block.Operations is [{ Instruction.Kind: StackInstructionKind.Push_Literal } push] &&
                PineValueInProcess.AreEqual(push.Instruction.Literal!, PineValue.EmptyBlob));

        // Both inner branches now continue directly in the block for the true result.
        optimized.Blocks
            .Count(
            block =>
            block.Terminator is PineControlFlowTerminator.Jump jump &&
            jump.Target == trueResultBlock.Id)
            .Should().Be(2);

        switchTerminator.Cases.Should().ContainSingle();
    }

    [Fact]
    public void Constant_boolean_branch_forwarding_preserves_backward_edges()
    {
        var selector = PineValue.Blob([1]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Switch(
                    Ops(StackInstruction.Push_Literal(PineKernelValues.FalseValue)),
                    (selector, Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue)))))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                    PineControlFlowFragment.Empty.Append(new PineControlFlowNode.JumpToEntry())));

        Optimize(fragment).Select(instruction => instruction.ToString()).Should().Equal(
            [
                StackInstruction.Local_Get(0).ToString(),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable:
                    ImmutableDictionary<PineValue, int>.Empty.Add(selector, 3)).ToString(),
                StackInstruction.Push_Literal(PineValue.EmptyList).ToString(),
                StackInstruction.Return.ToString(),
                StackInstruction.Jump_Unconditional(-4).ToString(),
                StackInstruction.Return.ToString(),
            ]);
    }

    private static bool IsBooleanLiteral(PineValueInProcess? literal) =>
        literal is not null &&
        (PineValueInProcess.AreEqual(literal, PineKernelValues.TrueValue) ||
        PineValueInProcess.AreEqual(literal, PineKernelValues.FalseValue));
}
