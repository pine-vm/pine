using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
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
            [.. cases.Select(switchCase => switchCase.Branch)],
            SkipCountMultiplier: 1);

    private static ImmutableArray<StackInstruction> Optimize(PineControlFlowFragment fragment) =>
        PineControlFlowGraph
        .FromFragment(fragment)
        .ForwardJumpsToReturn()
        .ForwardConstantBooleanBranches()
        .LowerToStackInstructions();

    [Theory]
    [InlineData(0)]
    [InlineData(2)]
    [InlineData(6)]
    public void Fuse_local_list_projection_preserves_result_without_building_a_list(int index)
    {
        var items = Enumerable.Range(0, 7).Select(i => PineValue.Blob([(byte)i])).ToArray();

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Skip_Head_Const(index)));

        var optimized = original.FuseLocalListProjections();

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get_Skip_Head_Const(0, index),
            StackInstruction.Return);

        Evaluate(optimized, PineValue.List(items)).Should()
            .Be(Evaluate(original, PineValue.List(items)));
    }

    [Fact]
    public void Fuse_local_list_projection_does_not_cross_an_intervening_operation()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Skip_Head_Const(1)));

        original.FuseLocalListProjections().LowerToStackInstructions()
            .Should().Equal(original.LowerToStackInstructions());
    }

    [Theory]
    [InlineData(0)]
    [InlineData(2)]
    [InlineData(4)]
    public void Eliminate_local_copy_and_fuse_repeated_projections(int index)
    {
        var input =
            PineValue.List(
                Enumerable.Range(0, 5).Select(i => PineValue.Blob([(byte)i])).ToArray());

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(2),
                    StackInstruction.Skip_Head_Const(index),
                    StackInstruction.Local_Get(2),
                    StackInstruction.Skip_Head_Const(1),
                    StackInstruction.Build_List(2)));

        var optimized = original.EliminateLocalCopies().FuseLocalListProjections();

        optimized.LowerToStackInstructions().Should().Equal(
            StackInstruction.Local_Get_Skip_Head_Const(0, index),
            StackInstruction.Local_Get_Skip_Head_Const(0, 1),
            StackInstruction.Build_List(2),
            StackInstruction.Return);

        Evaluate(optimized, input).Should().Be(Evaluate(original, input));
    }

    [Fact]
    public void Eliminate_local_copy_preserves_alias_when_source_is_overwritten()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Local_Set(0),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(2),
                    StackInstruction.Skip_Head_Const(1)));

        original.EliminateLocalCopies().LowerToStackInstructions()
            .Should().Equal(original.LowerToStackInstructions());
    }

    [Fact]
    public void Eliminate_local_copy_preserves_alias_when_descending_write_overwrites_source()
    {
        var input = PineValue.List([PineValue.Blob([11]), PineValue.Blob([22])]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(3, 1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Local_Set_Descending(1, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get_Skip_Head_Const(3, 1)));

        original.EliminateLocalCopies().LowerToStackInstructions()
            .Should().Equal(original.LowerToStackInstructions());

        Evaluate(original.EliminateLocalCopies(), input).Should().Be(Evaluate(original, input));
    }

    [Fact]
    public void Scalar_replacement_and_local_copy_elimination_share_an_existing_local()
    {
        var item = PineValue.Blob([11]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Build_List(1),
                    StackInstruction.Skip_Head_Const(0)));

        var optimized =
            original
            .ReplaceNonEscapingLists(parameterCount: 1)
            .EliminateLocalCopies()
            .FuseLocalListProjections();

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction =>
            instruction.Kind == StackInstructionKind.Build_List ||
            instruction.Kind == StackInstructionKind.Local_Set_Descending);

        Evaluate(optimized, item).Should().Be(Evaluate(original, item));
    }

    [Fact]
    public void Eliminate_local_copy_preserves_alias_used_in_another_block()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Push_Literal(PineKernelValues.TrueValue))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(2, 0)),
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(2, 1)))));

        original.EliminateLocalCopies().LowerToStackInstructions()
            .Should().Equal(original.LowerToStackInstructions());
    }

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
                    ],
                    SkipCountMultiplier: 1));

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
                    ],
                    SkipCountMultiplier: 1));

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
                    Branches: [Ops(StackInstruction.Push_Literal(PineKernelValues.TrueValue))],
                    SkipCountMultiplier: 1))
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

    [Theory]
    [InlineData(-1)]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    public void List_scalar_replacement_preserves_prefix_and_out_of_bounds_elements(int index)
    {
        var prefix = PineValue.Blob([11]);
        var item = PineValue.Blob([22]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(item),
                StackInstruction.Build_List_With_Prefix(PineValue.List([prefix]), 1),
                StackInstruction.Skip_Head_Const(index));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Should()
            .NotContain(instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

        var expected = index <= 0 ? prefix : index is 1 ? item : PineValue.EmptyList;
        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(expected);
        Evaluate(original, PineValue.EmptyBlob).Should().Be(expected);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_follows_block_arguments_and_branch_specific_projections(bool branch)
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Push_Literal(second),
                StackInstruction.Build_List(2),
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Skip_Head_Const(0)),
                    Ops(StackInstruction.Skip_Head_Const(1))));

        var optimized = PineControlFlowGraph.FromFragment(fragment).ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should()
            .NotContain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(optimized, branch ? PineKernelValues.TrueValue : PineKernelValues.FalseValue)
            .Should().Be(branch ? second : first);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_tracks_local_aliases_and_reassignments(bool branch)
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);
        var other = PineValue.Blob([33]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Push_Literal(second),
                StackInstruction.Build_List(2),
                StackInstruction.Local_Set(1),
                StackInstruction.Pop,
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(
                        StackInstruction.Push_Literal(PineValue.List([other])),
                        StackInstruction.Local_Set(1),
                        StackInstruction.Pop,
                        StackInstruction.Local_Get(1),
                        StackInstruction.Skip_Head_Const(0)),
                    Ops(StackInstruction.Local_Get_Skip_Head_Const(1, 1))));

        var optimized = PineControlFlowGraph.FromFragment(fragment).ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should()
            .NotContain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(optimized, branch ? PineKernelValues.TrueValue : PineKernelValues.FalseValue)
            .Should().Be(branch ? second : other);
    }

    [Fact]
    public void List_scalar_replacement_preserves_two_distinct_local_aliases()
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Build_List(1),
                StackInstruction.Local_Set(1),
                StackInstruction.Pop,
                StackInstruction.Push_Literal(second),
                StackInstruction.Build_List(1),
                StackInstruction.Local_Set(2),
                StackInstruction.Pop,
                StackInstruction.Local_Get_Skip_Head_Const(1, 0),
                StackInstruction.Local_Get_Skip_Head_Const(2, 0),
                StackInstruction.Build_List(2));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Count(
            instruction => instruction.Kind == StackInstructionKind.Build_List).Should().Be(1);

        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(Evaluate(original, PineValue.EmptyBlob));
    }

    [Fact]
    public void List_scalar_replacement_supports_head_length_and_length_comparison()
    {
        var item = PineValue.Blob([11]);

        foreach (var projection in new[]
        {
            StackInstruction.Head_Generic,
            StackInstruction.Length,
            StackInstruction.Length_Equal_Const(2),
            StackInstruction.Length_Equal_Const(3),
        })
        {
            var graph =
                PineControlFlowGraph.FromFragment(
                    Ops(
                        StackInstruction.Push_Literal(item),
                        StackInstruction.Build_List_With_Prefix(PineValue.List([PineValue.EmptyBlob]), 1),
                        projection));

            var optimized = graph.ReplaceNonEscapingLists(1);

            optimized.LowerToStackInstructions().Should().NotContain(
                instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

            Evaluate(optimized, PineValue.EmptyBlob).Should().Be(Evaluate(graph, PineValue.EmptyBlob));
        }
    }

    [Fact]
    public void List_scalar_replacement_follows_local_set_descending()
    {
        var item = PineValue.Blob([11]);

        var graph =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(item),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set_Descending(2, 1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get_Skip_Head_Const(2, 0)));

        var optimized = graph.ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(item);
    }

    [Fact]
    public void List_scalar_replacement_respects_descending_local_indices_for_multiple_values()
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(first),
                    StackInstruction.Push_Literal(second),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set_Descending(7, 2),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get_Skip_Head_Const(7, 0),
                    StackInstruction.Local_Get(6),
                    StackInstruction.Build_List(2)));

        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Count(
            instruction => instruction.Kind == StackInstructionKind.Build_List).Should().Be(1);

        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(PineValue.List([second, first]));
        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(Evaluate(original, PineValue.EmptyBlob));
    }

    [Fact]
    public void List_scalar_replacement_handles_prefixed_list_stored_in_a_reassigned_parameter_local()
    {
        var items =
            Enumerable.Range(0, 7)
            .Select(index => PineValue.Blob([(byte)(index + 1)]))
            .ToArray();

        var fragment =
            Ops(
                StackInstruction.Push_Literal(items[2]),
                StackInstruction.Push_Literal(items[3]),
                StackInstruction.Push_Literal(items[4]),
                StackInstruction.Push_Literal(items[5]),
                StackInstruction.Push_Literal(items[6]),
                StackInstruction.Build_List_With_Prefix(PineValue.List([items[0], items[1]]), 5),
                StackInstruction.Local_Set(0),
                StackInstruction.Skip_Head_Const(1),
                StackInstruction.Local_Get_Skip_Head_Const(0, 3),
                StackInstruction.Build_List(2));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);
        var instructions = optimized.LowerToStackInstructions();

        instructions.Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

        instructions.Count(instruction => instruction.Kind == StackInstructionKind.Build_List)
            .Should().Be(1);

        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(PineValue.List([items[1], items[3]]));
        Evaluate(optimized, PineValue.EmptyBlob).Should().Be(Evaluate(original, PineValue.EmptyBlob));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_follows_prefixed_list_through_jumps_and_tag_check(bool branch)
    {
        var tag = PineValue.Blob([1]);
        var first = PineValue.Blob([2]);
        var second = PineValue.Blob([3]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Push_Literal(second),
                StackInstruction.Build_List_With_Prefix(PineValue.List([PineValue.EmptyBlob, tag]), 2),
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob), StackInstruction.Pop),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop)))
            .Append(
                Ops(
                    StackInstruction.Local_Set(0),
                    StackInstruction.Skip_Head_Const(1)))
            .Append(
                Conditional(
                    tag,
                    Ops(StackInstruction.Local_Get_Skip_Head_Const(0, 2)),
                    Ops(StackInstruction.Local_Get_Skip_Head_Const(0, 3))));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List_With_Prefix);

        var environment = branch ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;
        Evaluate(optimized, environment).Should().Be(second);
        Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void List_scalar_replacement_handles_a_cross_block_local_set_and_multiple_projections(bool branch)
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);
        var third = PineValue.Blob([33]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Push_Literal(second),
                StackInstruction.Push_Literal(third),
                StackInstruction.Build_List(3),
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob), StackInstruction.Pop),
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop)))
            .Append(
                Ops(
                    StackInstruction.Local_Set(7),
                    StackInstruction.Head_Generic,
                    StackInstruction.Local_Get_Skip_Head_Const(7, 1),
                    StackInstruction.Local_Get_Skip_Head_Const(7, 2),
                    StackInstruction.Build_List(3)));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);
        var instructions = optimized.LowerToStackInstructions();

        instructions.Count(instruction => instruction.Kind == StackInstructionKind.Build_List)
            .Should().Be(1);

        var environment = branch ? PineKernelValues.TrueValue : PineKernelValues.FalseValue;
        Evaluate(optimized, environment).Should().Be(PineValue.List([first, second, third]));
        Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
    }

    [Fact]
    public void List_scalar_replacement_keeps_ambiguous_alias_at_a_join()
    {
        var first = PineValue.Blob([11]);
        var second = PineValue.Blob([22]);

        var fragment =
            Ops(
                StackInstruction.Push_Literal(first),
                StackInstruction.Build_List(1),
                StackInstruction.Local_Set(1),
                StackInstruction.Pop,
                StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop),
                    Ops(
                        StackInstruction.Push_Literal(PineValue.List([second])),
                        StackInstruction.Local_Set(1),
                        StackInstruction.Pop,
                        StackInstruction.Push_Literal(PineValue.EmptyList),
                        StackInstruction.Pop)))
            .AppendOperation(StackInstruction.Local_Get_Skip_Head_Const(1, 0));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var optimized = original.ReplaceNonEscapingLists(1);

        optimized.LowerToStackInstructions().Should()
            .Contain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        foreach (var condition in new[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue })
        {
            Evaluate(optimized, condition).Should().Be(Evaluate(original, condition));
        }
    }

    [Fact]
    public void List_scalar_replacement_keeps_a_list_that_escapes_on_one_branch()
    {
        var item = PineValue.Blob([11]);

        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(item),
                    StackInstruction.Build_List(1),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Local_Get(1)),
                        Ops(StackInstruction.Local_Get_Skip_Head_Const(1, 0)))));

        var optimized = original.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Should().Contain(
            instruction => instruction.Kind == StackInstructionKind.Build_List);

        foreach (var environment in new[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue })
        {
            Evaluate(optimized, environment).Should().Be(Evaluate(original, environment));
        }
    }

    [Fact]
    public void List_scalar_replacement_ignores_unrelated_tail_invocation()
    {
        var item = PineValue.Blob([11]);

        var fragment =
            Ops(StackInstruction.Local_Get(0))
            .Append(
                Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Push_Literal(PineValue.EmptyBlob)),
                    Ops(
                        StackInstruction.Push_Literal(item),
                        StackInstruction.Build_List(1),
                        StackInstruction.Head_Generic)));

        var original = PineControlFlowGraph.FromFragment(fragment);
        var blocks = original.Blocks.ToArray();

        blocks[1] =
            blocks[1] with
            {
                Terminator =
                new PineControlFlowTerminator.TailInvoke(
                    StackInstruction.Eval_Const(PineValue.EmptyList))
            };

        var withTailInvoke = new PineControlFlowGraph(original.Entry, [.. blocks]);
        var optimized = withTailInvoke.ReplaceNonEscapingLists(parameterCount: 1);

        optimized.LowerToStackInstructions().Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Build_List);

        Evaluate(optimized, PineKernelValues.TrueValue).Should().Be(item);
    }

    [Fact]
    public void List_scalar_replacement_keeps_a_list_passed_to_tail_invocation()
    {
        var original =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Build_List(1)));

        var blocks = original.Blocks.ToArray();

        blocks[0] =
            blocks[0] with
            {
                Terminator =
                new PineControlFlowTerminator.TailInvoke(
                    StackInstruction.Eval_Const(PineValue.EmptyList))
            };

        var withTailInvoke = new PineControlFlowGraph(original.Entry, [.. blocks]);

        withTailInvoke.ReplaceNonEscapingLists(parameterCount: 1)
            .Should().BeSameAs(withTailInvoke);
    }

    [Fact]
    public void List_scalar_replacement_does_not_change_escaping_or_cyclic_builds()
    {
        var escaping =
            PineControlFlowGraph.FromFragment(
                Ops(StackInstruction.Local_Get(0), StackInstruction.Build_List(1)));

        escaping.ReplaceNonEscapingLists(1).Should().BeSameAs(escaping);

        var cyclic =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Build_List(1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get(0))
                .Append(
                    Conditional(
                        PineKernelValues.TrueValue,
                        Ops(StackInstruction.Push_Literal(PineValue.EmptyList)),
                        PineControlFlowFragment.Empty.Append(new PineControlFlowNode.JumpToEntry()))));

        cyclic.ReplaceNonEscapingLists(1).LowerToStackInstructions().Should()
            .Contain(instruction => instruction.Kind == StackInstructionKind.Build_List);
    }

    [Fact]
    public void Expression_compilation_replaces_a_deconstructed_list()
    {
        var expression =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.head),
                Expression.ListInst([Expression.EnvironmentInstance, Expression.LitralInst(PineValue.EmptyBlob)]));

        var unoptimized =
            ExpressionCompilation.ControlFlowGraphFromExpression(
                expression,
                rootExprAlternativeForms: [],
                envClass: null,
                parametersAsLocals: StaticFunctionInterface.FromExpression(expression),
                parseCache: new());

        unoptimized.LowerToStackInstructions().Should()
            .Contain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        var compiled =
            ExpressionCompilation.CompileExpression(
                expression,
                specializations: [],
                parseCache: new(),
                disableReduction: true,
                enableTailRecursionOptimization: false,
                skipInlining: (_, _) => false);

        compiled.Generic.Instructions.Should()
            .NotContain(instruction => instruction.Kind == StackInstructionKind.Build_List);

        var vm = CreateVm();

        foreach (var environment in new[] { PineValue.EmptyList, PineValue.Blob([1, 2]) })
        {
            vm.EvaluateExpression(expression, environment).IsOkOrNull().Should().Be(environment);
        }
    }

    private static PineValue Evaluate(PineControlFlowGraph graph, PineValue environment)
    {
        var expression = Expression.ListInst([Expression.EnvironmentInstance]);

        var compilation =
            new ExpressionCompilation(
                new StackFrameInstructions(StaticFunctionInterface.Generic, graph.LowerToStackInstructions()),
                Specialized: []);

        var vm =
            CreateVm(new Dictionary<Expression, ExpressionCompilation> { [expression] = compilation });

        return vm.EvaluateExpression(expression, environment).IsOkOrNull()!;
    }

    private static Core.Interpreter.IntermediateVM.PineVM CreateVm(
        IReadOnlyDictionary<Expression, ExpressionCompilation>? overrides = null) =>
        Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves: null,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null,
            expressionCompilationOverrides: overrides);

    private static bool IsBooleanLiteral(PineValueInProcess? literal) =>
        literal is not null &&
        (PineValueInProcess.AreEqual(literal, PineKernelValues.TrueValue) ||
        PineValueInProcess.AreEqual(literal, PineKernelValues.FalseValue));
}
