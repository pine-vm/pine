using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class ValueMapInstructionTests
{
    private static PineValue Integer(int value) => IntegerEncoding.EncodeSignedInteger(value);

    private static ImmutableDictionary<PineValue, PineValue> Map(int count) =>
        Enumerable.Range(0, count)
        .ToImmutableDictionary(index => Integer(index), index => (PineValue)PineValue.List([Integer(index + 100)]));

    private static StackInstruction[] Instructions(
        ImmutableDictionary<PineValue, PineValue> map,
        bool jumpFallback) =>
        jumpFallback
        ?
        [
        StackInstruction.Push_Literal(PineValue.EmptyBlob),
        StackInstruction.Local_Get(0),
        StackInstruction.Select_Value_Const(map, jumpOffset: 2),
        StackInstruction.Jump_Unconditional(2),
        StackInstruction.Push_Literal(PineValue.EmptyList),
        StackInstruction.Build_List(2),
        StackInstruction.Return
        ]
        :
        [
        StackInstruction.Push_Literal(PineValue.EmptyBlob),
        StackInstruction.Local_Get(0),
        StackInstruction.Select_Value_Const(map, fallbackValue: PineValue.EmptyList),
        StackInstruction.Build_List(2),
        StackInstruction.Return
        ];

    [Theory]
    [InlineData(false, 0)]
    [InlineData(true, 0)]
    [InlineData(false, 2)]
    [InlineData(true, 2)]
    [InlineData(false, 40)]
    [InlineData(true, 40)]
    public void Value_map_executes_and_persists_both_fallback_modes(bool jumpFallback, int count)
    {
        var map =
            count is 0
            ?
            Map(0)
            :
            Map(count).Add(PineValue.List([PineValue.EmptyBlob]), PineValue.Blob([255]));

        var instructions = Instructions(map, jumpFallback);
        var frame = new StackFrameInstructions(StaticFunctionInterface.Generic, instructions);
        frame.MaxStackUsage.Should().Be(2);

        var rendered =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                frame,
                StackInstructionTraceRenderer.RenderingOptions.Persistence);

        var restored =
            StackInstructionTextFormat.ParseStackFrameInstructions(
                rendered.Text,
                hash => rendered.ObservedValues[hash]);

        restored.Instructions.Should().Equal(instructions);
        restored.MaxStackUsage.Should().Be(frame.MaxStackUsage);

        StackInstructionTraceRenderer.RenderStackFrameInstructions(
            restored,
            StackInstructionTraceRenderer.RenderingOptions.Persistence)
            .Text.Should().Be(rendered.Text);

        foreach (var key in map.Keys.Append(PineValue.EmptyList))
        {
            var expected =
                PineValue.List(
                    [PineValue.EmptyBlob, map.GetValueOrDefault(key, PineValue.EmptyList)]);

            Evaluate(instructions, key).Should().Be(expected);
            Evaluate(restored.Instructions, key).Should().Be(expected);
        }

        if (jumpFallback)
            rendered.Text.Should().Contain("jumps_arriving_from 1 (2)");
    }

    [Fact]
    public void Value_map_factory_requires_exactly_one_fallback()
    {
        Action neither = () => StackInstruction.Select_Value_Const(Map(0));
        Action both = () => StackInstruction.Select_Value_Const(Map(0), PineValue.EmptyList, 0);
        Action noMap = () => StackInstruction.Select_Value_Const(null!, PineValue.EmptyList);

        neither.Should().Throw<ArgumentException>();
        both.Should().Throw<ArgumentException>();
        noMap.Should().Throw<ArgumentNullException>();

        Evaluate(Instructions(Map(0), jumpFallback: false), PineValue.EmptyList)
            .Should().Be(PineValue.List([PineValue.EmptyBlob, PineValue.EmptyList]));

        Evaluate(Instructions(Map(0), jumpFallback: true), PineValue.EmptyList)
            .Should().Be(PineValue.List([PineValue.EmptyBlob, PineValue.EmptyList]));
    }

    [Fact]
    public void Value_map_equality_hash_and_rendering_are_structural_and_order_independent()
    {
        var map = Map(40);
        var reverse = map.Reverse().ToImmutableDictionary();
        var first = StackInstruction.Select_Value_Const(map, PineValue.EmptyList);
        var second = StackInstruction.Select_Value_Const(reverse, PineValue.EmptyList);

        first.Should().Be(second);
        first.GetHashCode().Should().Be(second.GetHashCode());

        first.Should().NotBe(
            StackInstruction.Select_Value_Const(map.SetItem(Integer(0), PineValue.EmptyBlob), PineValue.EmptyList));

        first.Should().NotBe(StackInstruction.Select_Value_Const(map, jumpOffset: 0));

        StackInstructionTraceRenderer.RenderInstructions(
            [first],
            StackInstructionTraceRenderer.RenderingOptions.Persistence)
            .Text.Should().Be(
            StackInstructionTraceRenderer.RenderInstructions(
                [second],
                StackInstructionTraceRenderer.RenderingOptions.Persistence).Text);

        var display =
            StackInstruction.RenderInstructionDisplay(
                first,
                StackInstruction.LiteralDisplayStringDefault);

        display.Should().Contain("Select_Value_Const");
        display.Should().Contain("fallback");
        display.Should().Contain("map ");
    }

    [Fact]
    public void Value_map_literal_fallback_is_an_ordinary_graph_operation()
    {
        var map = Map(40);

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations(
                    [
                    StackInstruction.Local_Get(0),
                    StackInstruction.Select_Value_Const(map, PineValue.EmptyList)
                    ]));

        var lowered = graph.OptimizeForExecution(1).LowerToStackInstructions();
        lowered.Count(instruction => instruction.Kind is StackInstructionKind.Select_Value_Const).Should().Be(1);
        Evaluate(lowered, Integer(39)).Should().Be(map[Integer(39)]);
        Evaluate(lowered, PineValue.EmptyBlob).Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void Value_map_jump_fallback_is_structural_and_lowers_to_one_instruction()
    {
        var map = Map(40);

        Action rawJumpOperation =
            () => new PineControlFlowNode.Operation(StackInstruction.Select_Value_Const(map, jumpOffset: 2));

        rawJumpOperation.Should().Throw<ArgumentException>();

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations(
                    [
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Local_Get(0)
                    ])
                .Append(
                    new PineControlFlowNode.ValueMap(
                        map,
                        PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(PineValue.EmptyList)])))
                .AppendOperation(StackInstruction.Build_List(2)));

        graph.Blocks.Should().Contain(block => block.Terminator is PineControlFlowTerminator.ValueMap);

        foreach (var current in new[] { graph, graph.OptimizeForExecution(1) })
        {
            var lowered = current.LowerToStackInstructions();
            lowered.Count(instruction => instruction.Kind is StackInstructionKind.Select_Value_Const).Should().Be(1);
            StackFrameInstructions.ComputeMaxStackUsage(lowered).Should().Be(2);

            var rendered =
                StackInstructionTraceRenderer.RenderInstructions(
                    lowered,
                    StackInstructionTraceRenderer.RenderingOptions.Persistence);

            var restored =
                StackInstructionTextFormat.ParseInstructions(
                    rendered.Text,
                    hash => rendered.ObservedValues[hash]);

            foreach (var key in new[] { Integer(0), Integer(39), PineValue.EmptyList })
            {
                var expected = PineValue.List([PineValue.EmptyBlob, map.GetValueOrDefault(key, PineValue.EmptyList)]);
                Evaluate(lowered, key).Should().Be(expected);
                Evaluate(restored, key).Should().Be(expected);
            }
        }
    }

    [Fact]
    public void Value_map_jump_edges_track_their_distinct_stack_depths()
    {
        Action underflow =
            () => StackFrameInstructions.ComputeMaxStackUsage(
                [StackInstruction.Select_Value_Const(Map(0), PineValue.EmptyList), StackInstruction.Return]);

        Action inconsistent =
            () => StackFrameInstructions.ComputeMaxStackUsage(
                [
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Select_Value_Const(Map(1), jumpOffset: 1),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return
                ]);

        underflow.Should().Throw<InvalidOperationException>().WithMessage("*under-flow*");
        inconsistent.Should().Throw<InvalidOperationException>().WithMessage("*Inconsistent stack depth*");

        StackInstruction[] emptyMap =
            [
            StackInstruction.Local_Get(0),
            StackInstruction.Select_Value_Const(Map(0), jumpOffset: 1),
            StackInstruction.Push_Literal(PineValue.EmptyList),
            StackInstruction.Return
            ];

        StackFrameInstructions.ComputeMaxStackUsage(emptyMap).Should().Be(1);
        Evaluate(emptyMap, Integer(0)).Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void Value_map_inlining_preserves_the_live_caller_stack_and_both_edges()
    {
        var map = Map(40);

        var callee =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations([StackInstruction.Local_Get(0)])
                .Append(
                    new PineControlFlowNode.ValueMap(
                        map,
                        PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(PineValue.EmptyList)]))));

        var caller =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations(
                    [
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Local_Get(0),
                    StackInstruction.Invoke_StackFrame_Const(
                        Expression.EnvironmentInstance,
                        StaticFunctionInterface.Generic),
                    StackInstruction.Build_List(2)
                    ]));

        var inlined =
            caller.InlineInvocation(caller.Entry, callee, callerParameterCount: 1, calleeParameterCount: 1)
            .OptimizeForExecution(1)
            .LowerToStackInstructions();

        inlined.Should().NotContain(instruction => PineControlFlowGraph.IsInvocation(instruction.Kind));

        foreach (var key in new[] { Integer(0), Integer(39), PineValue.EmptyList })
        {
            Evaluate(inlined, key)
                .Should().Be(PineValue.List([PineValue.EmptyBlob, map.GetValueOrDefault(key, PineValue.EmptyList)]));
        }
    }

    [Fact]
    public void Value_map_frame_persistence_preserves_return_layout_metadata()
    {
        var layout = new ReturnValueLayout(new ReturnValueLayout.Node.Fixed(Integer(42)), []);

        var target =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Select_Value_Const(Map(40), PineValue.EmptyList),
                StackInstruction.Pop,
                StackInstruction.Return with { TakeCount = 0 }
                ],
                returnLayout: layout);

        var invocation =
            new DirectInvocation(
                Expression.EnvironmentInstance,
                ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                StaticFunctionInterface.Generic)
            {
                ReturnLayout = layout
            };

        var clone = invocation with { ReturnLayout = ReturnValueLayout.Decode(layout.Encoding) };
        invocation.Should().Be(clone);
        invocation.GetHashCode().Should().Be(clone.GetHashCode());
        invocation.Should().NotBe(invocation with { ReturnLayout = ReturnValueLayout.Canonical });
        invocation.SetLinkedStackFrameInstructions(target);

        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Local_Get(0),
                new(StackInstructionKind.Invoke_StackFrame_Const, OptimizedInvocation: invocation, TakeCount: 1),
                StackInstruction.Return with { TakeCount = 0 }
                ],
                returnLayout: layout);

        var rendered =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                frame,
                StackInstructionTraceRenderer.RenderingOptions.Persistence);

        var restored =
            StackInstructionTextFormat.ParseStackFrameInstructions(
                rendered.Text,
                hash => rendered.ObservedValues[hash]);

        restored.ReturnLayout.Hash.Should().Be(layout.Hash);
        restored.Instructions[1].OptimizedInvocation!.ReturnLayout.Hash.Should().Be(layout.Hash);
        restored.Instructions[1].LinkedStackFrameInstructions!.ReturnLayout.Hash.Should().Be(layout.Hash);

        StackInstructionTraceRenderer.RenderStackFrameInstructions(
            restored,
            StackInstructionTraceRenderer.RenderingOptions.Persistence)
            .Text.Should().Be(rendered.Text);
    }

    [Fact]
    public void Value_map_backward_fallback_honors_loop_limits()
    {
        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations([StackInstruction.Local_Get(0)])
                .Append(
                    new PineControlFlowNode.ValueMap(
                        Map(0),
                        PineControlFlowFragment.Empty.Append(PineControlFlowNode.JumpToEntry.Instance))));

        var expression = Expression.ListInst([Expression.EnvironmentInstance]);

        var error =
            CreateVm(expression, graph.LowerToStackInstructions())
            .EvaluateExpressionOnCustomStack(
                expression,
                PineValue.EmptyList,
                new Core.Runtime.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: 3,
                    StackDepthLimit: null))
            .IsErrOrNull();

        error.Should().NotBeNull();
        EvaluationError.RenderDisplayString(error!).Should().Contain("Loop iteration count limit exceeded");
    }

    private static ImmutableDictionary<PineValue, PineValue> ChoiceMap() =>
        Enumerable.Range(0, 3).ToImmutableDictionary(
            Integer,
            index => (PineValue)PineValue.List([PineValue.EmptyList, Integer(index + 10)]));

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void Value_map_projection_comparison_uses_compact_keys(bool negate, bool matchesFallback)
    {
        var choices = ChoiceMap();
        var compared = Integer(matchesFallback ? 10 : 11);

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations(
                    [
                    StackInstruction.Local_Get(0),
                    StackInstruction.Select_Value_Const(choices, choices[Integer(0)]),
                    StackInstruction.Skip_Head_Const(1),
                    negate
                    ?
                    StackInstruction.Not_Equal_Binary_Const(compared)
                    :
                    StackInstruction.Equal_Binary_Const(compared)
                    ]));

        var optimized = graph.OptimizeForExecution(1).LowerToStackInstructions();

        optimized.Should().NotContain(
            instruction =>
            instruction.Kind == StackInstructionKind.Skip_Head_Const ||
            instruction.Kind == StackInstructionKind.Head_Generic);

        if (!matchesFallback)
        {
            optimized.Should().NotContain(instruction => instruction.Kind == StackInstructionKind.Select_Value_Const);

            optimized.Should().Contain(
                negate
                ?
                StackInstruction.Not_Equal_Binary_Const(Integer(1))
                :
                StackInstruction.Equal_Binary_Const(Integer(1)));
        }

        AssertEquivalentMaps(graph, optimized);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Value_map_projected_conditional_compares_the_compact_key_directly(bool matchesFallback)
    {
        var choices = ChoiceMap().Remove(Integer(2));

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations(
                    [
                    StackInstruction.Local_Get(0),
                    StackInstruction.Select_Value_Const(choices, choices[Integer(0)]),
                    StackInstruction.Skip_Head_Const(1)
                    ])
                .Append(
                    new PineControlFlowNode.Conditional(
                        Integer(matchesFallback ? 10 : 11),
                        PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(Integer(100))]),
                        PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(Integer(200))]))));

        var lowered = graph.ComposeValueMaps().LowerToStackInstructions();

        if (matchesFallback)
            lowered.Should().Contain(StackInstruction.Not_Equal_Binary_Const(Integer(1)));

        else
        {
            lowered.Should().Contain(
                instruction =>
                instruction.Kind == StackInstructionKind.Jump_If_Equal_Const &&
                instruction.Literal!.Evaluate().Equals(Integer(1)));
        }

        lowered.Should().NotContain(
            instruction =>
            instruction.Kind == StackInstructionKind.Select_Value_Const ||
            instruction.Kind == StackInstructionKind.Skip_Head_Const);

        AssertEquivalentMaps(graph, lowered);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Value_map_local_consumers_are_rematerialized_only_while_the_compact_key_is_unchanged(bool overwriteKey)
    {
        var choices = ChoiceMap();

        var fragment =
            PineControlFlowFragment.FromOperations(
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Select_Value_Const(choices, choices[Integer(0)]),
                StackInstruction.Local_Set([1], popCount: 1)
                ]);

        if (overwriteKey)
            fragment = fragment.AppendOperation(StackInstruction.Local_Set_Literal(0, Integer(2)));

        var graph =
            PineControlFlowGraph.FromFragment(
                fragment.AppendOperations(
                    [
                    StackInstruction.Local_Get(1),
                    StackInstruction.Skip_Head_Const(1),
                    StackInstruction.Equal_Binary_Const(Integer(11))
                    ]));

        var lowered = graph.OptimizeForExecution(1).LowerToStackInstructions();

        if (!overwriteKey)
            lowered.Should().NotContain(instruction => instruction.Kind == StackInstructionKind.Select_Value_Const);

        AssertEquivalentMaps(graph, lowered);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Value_map_inverse_composition_removes_identity_only_for_proven_admitted_keys(bool admitted)
    {
        var choices = ChoiceMap();
        var inverse = choices.ToImmutableDictionary(item => item.Value, item => item.Key);
        var fragment = PineControlFlowFragment.FromOperations([StackInstruction.Local_Get(0)]);

        if (admitted)
        {
            fragment =
                fragment.AppendOperation(
                    StackInstruction.Select_Value_Const(
                        Enumerable.Range(10, 3).ToImmutableDictionary(Integer, index => Integer(index - 10)),
                        Integer(0)));
        }

        var graph =
            PineControlFlowGraph.FromFragment(
                fragment.AppendOperations(
                    [
                    StackInstruction.Select_Value_Const(choices, choices[Integer(0)]),
                    StackInstruction.Select_Value_Const(inverse, Integer(0))
                    ]));

        var lowered = graph.ComposeValueMaps().LowerToStackInstructions();
        lowered.Count(instruction => instruction.Kind is StackInstructionKind.Select_Value_Const).Should().Be(1);
        AssertEquivalentMaps(graph, lowered);
        Evaluate(lowered, Integer(99)).Should().Be(Integer(0));
    }

    [Fact]
    public void Value_map_inverse_identity_uses_domains_propagated_through_branches_and_locals()
    {
        var choices = ChoiceMap();
        var inverse = choices.ToImmutableDictionary(item => item.Value, item => item.Key);

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations([StackInstruction.Local_Get(0)])
                .Append(
                    new PineControlFlowNode.Conditional(
                        Integer(0),
                        PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(Integer(1))]),
                        PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(Integer(2))])))
                .AppendOperations(
                    [
                    StackInstruction.Local_Set([1], popCount: 1),
                    StackInstruction.Local_Get(1),
                    StackInstruction.Select_Value_Const(choices, choices[Integer(0)]),
                    StackInstruction.Select_Value_Const(inverse, Integer(0))
                    ]));

        var lowered = graph.ComposeValueMaps().LowerToStackInstructions();
        lowered.Should().NotContain(instruction => instruction.Kind == StackInstructionKind.Select_Value_Const);
        AssertEquivalentMaps(graph, lowered);
    }

    [Fact]
    public void Value_map_identity_does_not_assume_an_unknown_incoming_branch_is_admitted()
    {
        var choices = ChoiceMap();
        var inverse = choices.ToImmutableDictionary(item => item.Value, item => item.Key);

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations([StackInstruction.Local_Get(0)])
                .Append(
                    new PineControlFlowNode.Conditional(
                        Integer(0),
                        PineControlFlowFragment.FromOperations([StackInstruction.Local_Get(0)]),
                        PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(Integer(1))])))
                .AppendOperations(
                    [
                    StackInstruction.Select_Value_Const(choices, choices[Integer(0)]),
                    StackInstruction.Select_Value_Const(inverse, Integer(0))
                    ]));

        var lowered = graph.ComposeValueMaps().LowerToStackInstructions();
        lowered.Count(instruction => instruction.Kind is StackInstructionKind.Select_Value_Const).Should().Be(1);
        AssertEquivalentMaps(graph, lowered);
        Evaluate(lowered, Integer(99)).Should().Be(Integer(0));
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(0)]
    [InlineData(100)]
    public void Value_map_composes_head_and_total_out_of_range_projections(int skip)
    {
        var choices = ChoiceMap();

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations(
                    [
                    StackInstruction.Local_Get(0),
                    StackInstruction.Select_Value_Const(choices, PineValue.EmptyBlob),
                    skip is 0 ? StackInstruction.Head_Generic : StackInstruction.Skip_Head_Const(skip),
                    StackInstruction.Equal_Binary_Const(PineValue.EmptyList)
                    ]));

        var lowered = graph.ComposeValueMaps().LowerToStackInstructions();

        lowered.Should().NotContain(
            instruction =>
            instruction.Kind == StackInstructionKind.Head_Generic ||
            instruction.Kind == StackInstructionKind.Skip_Head_Const);

        AssertEquivalentMaps(graph, lowered);
    }

    [Fact]
    public void Value_map_composition_is_idempotent_and_reuses_unchanged_graphs()
    {
        var choices = ChoiceMap();

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations(
                    [
                    StackInstruction.Local_Get(0),
                    StackInstruction.Select_Value_Const(choices, choices[Integer(0)])
                    ]));

        var composed = graph.ComposeValueMaps();
        composed.ComposeValueMaps().Should().BeSameAs(composed);
    }

    [Theory]
    [InlineData(true, 3)]
    [InlineData(false, 3)]
    [InlineData(true, 200)]
    [InlineData(false, 200)]
    public void Value_map_known_literal_keys_fold_to_mapped_or_fallback_literals(bool hit, int count)
    {
        var map =
            count is 3
            ?
            new[] { 71, 73, 79 }.Select((key, index) => KeyValuePair.Create(Integer(key), Integer(index)))
            .ToImmutableDictionary()
            :
            Enumerable.Range(0, count).ToImmutableDictionary(Integer, index => Integer(index + 1000));

        var key = Integer(hit ? count is 3 ? 79 : count - 1 : 999);
        var expected = map.GetValueOrDefault(key, Integer(9));

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations(
                    [
                    StackInstruction.Push_Literal(key),
                    StackInstruction.Select_Value_Const(map, Integer(9))
                    ]));

        var lowered = graph.OptimizeForExecution(0).LowerToStackInstructions();
        lowered.Should().Equal(StackInstruction.Push_Literal(expected), StackInstruction.Return);
        Evaluate(lowered, PineValue.EmptyList).Should().Be(expected);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Value_map_known_keys_through_block_parameters_preserve_the_consumed_stack_value(bool hit)
    {
        var map = ImmutableDictionary<PineValue, PineValue>.Empty.Add(Integer(79), Integer(2));
        var key = Integer(hit ? 79 : 80);
        var branch = PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(key)]);

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations([StackInstruction.Local_Get(0)])
                .Append(new PineControlFlowNode.Conditional(Integer(0), branch, branch))
                .AppendOperation(StackInstruction.Select_Value_Const(map, Integer(9))));

        var lowered = graph.FoldKnownLiteralValues(1).LowerToStackInstructions();
        lowered.Should().NotContain(instruction => instruction.Kind == StackInstructionKind.Select_Value_Const);
        StackFrameInstructions.ComputeMaxStackUsage(lowered).Should().Be(1);
        AssertEquivalentMaps(graph, lowered);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Value_map_literal_facts_propagate_selected_results_to_locals(bool hit)
    {
        var map = ImmutableDictionary<PineValue, PineValue>.Empty.Add(Integer(79), Integer(2));

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations(
                    [
                    StackInstruction.Push_Literal(Integer(hit ? 79 : 80)),
                    StackInstruction.Select_Value_Const(map, Integer(9)),
                    StackInstruction.Local_Set([1], popCount: 1),
                    StackInstruction.Local_Get(1)
                    ]));

        var lowered = graph.FoldKnownLiteralValues(0).LowerToStackInstructions();

        lowered.Should().NotContain(
            instruction =>
            instruction.Kind == StackInstructionKind.Select_Value_Const ||
            instruction.Kind == StackInstructionKind.Local_Get);

        Evaluate(lowered, PineValue.EmptyList).Should().Be(Integer(hit ? 2 : 9));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Value_map_literal_folding_keeps_jump_fallbacks_structural(bool hit)
    {
        var map = ImmutableDictionary<PineValue, PineValue>.Empty.Add(Integer(79), Integer(2));

        var graph =
            PineControlFlowGraph.FromFragment(
                PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(Integer(hit ? 79 : 80))])
                .Append(
                    new PineControlFlowNode.ValueMap(
                        map,
                        PineControlFlowFragment.FromOperations([StackInstruction.Push_Literal(Integer(9))]))));

        var folded = graph.FoldKnownLiteralValues(0);
        folded.Blocks.Should().Contain(block => block.Terminator is PineControlFlowTerminator.ValueMap);
        Evaluate(folded.LowerToStackInstructions(), PineValue.EmptyList).Should().Be(Integer(hit ? 2 : 9));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(2)]
    [InlineData(3)]
    public void Value_map_return_display_includes_explicit_slot_counts(int count)
    {
        StackInstruction.RenderInstructionDisplay(
            StackInstruction.Return with { TakeCount = count },
            StackInstruction.LiteralDisplayStringDefault)
            .Should().Be($"Return ({count})");
    }

    private static void AssertEquivalentMaps(PineControlFlowGraph original, IReadOnlyList<StackInstruction> optimized)
    {
        var baseline = original.LowerToStackInstructions();

        foreach (var key in Enumerable.Range(-1, 15).Select(Integer)
            .Concat([PineValue.EmptyList, PineValue.EmptyBlob, Integer(99)]))
        {
            Evaluate(optimized, key).Should().Be(Evaluate(baseline, key));
        }
    }

    private static PineValue Evaluate(IReadOnlyList<StackInstruction> instructions, PineValue key)
    {
        var expression = Expression.ListInst([Expression.EnvironmentInstance]);

        return
            CreateVm(expression, instructions).EvaluateExpression(expression, key).Extract(
                error => throw new Exception(error));
    }

    private static Core.Runtime.IntermediateVM.PineVM CreateVm(
        Expression expression,
        IReadOnlyList<StackInstruction> instructions) =>
        Core.Runtime.IntermediateVM.PineVM.CreateCustom(
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
            expressionCompilationOverrides:
            new Dictionary<Core.Runtime.InternalExpression.Expression, ExpressionCompilation>
            {
                [Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(expression)] =
                new(
                    new StackFrameInstructions(StaticFunctionInterface.Generic, instructions),
                    Specialized: [])
            });
}
