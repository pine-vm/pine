using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using System.Numerics;
using System.Text.RegularExpressions;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class StackInstructionTextFormatTests
{
    private static readonly StackInstructionTraceRenderer.RenderingOptions s_persistence =
        StackInstructionTraceRenderer.RenderingOptions.Persistence;

    public static IEnumerable<object[]> InstructionKinds() =>
        Enum.GetValues<StackInstructionKind>().Select(kind => new object[] { kind });

    public static IEnumerable<object[]> Values() =>
        new PineValue[]
        {
            PineValue.EmptyBlob,
            PineValue.EmptyList,
            PineValue.Blob([255]),
            PineValue.Blob([4, 0, 1]),
            IntegerEncoding.EncodeSignedInteger(-123),
            StringEncoding.ValueFromString("quote \" slash \\ newline \n (x, y): hash 0x1234"),
            StringEncoding.ValueFromString(new string('x', 10000)),
            PineValue.Blob(Enumerable.Repeat((byte)255, 10000).ToArray()),
            IntegerEncoding.EncodeSignedInteger(BigInteger.One << 40000),
            PineValue.List([PineValue.EmptyBlob, PineValue.List([PineValue.Blob([255])])]),
            PineValue.List(Enumerable.Repeat<PineValue>(PineValue.EmptyList, 10000).ToArray())
        }
        .Select(value => new object[] { value });

    [Theory]
    [MemberData(nameof(InstructionKinds))]
    public void Persistence_round_trips_every_kind_and_every_instruction_field(StackInstructionKind kind)
    {
        var literal = PineValue.List([PineValue.Blob([255]), PineValue.EmptyBlob]);
        var expression = Expression.LitralInst(literal);

        var instruction =
            new StackInstruction(
                kind,
                Literal: PineValueInProcess.CreateFullyRepresented(literal),
                IntegerLiteral: -(BigInteger.One << 400),
                LocalIndices: [3, 0, 3, int.MaxValue],
                PopCount: 0,
                SkipCount: int.MinValue,
                TakeCount: int.MaxValue,
                ShiftCount: -32,
                JumpOffset: -1,
                OptimizedInvocation:
                new DirectInvocation(
                    expression,
                    ExpressionEncoding2024.EncodeExpressionAsValue(expression),
                    StaticFunctionInterface.FromPathsSorted([[1, 0], [], [2]])),
                SwitchJumpTable:
                ImmutableDictionary<PineValue, int>.Empty
                .Add(literal, 1)
                .Add(PineValue.EmptyBlob, -1),
                SliceSwitchCases: [new(literal, 3), new(PineValue.EmptyList, -1)])
            {
                TemplatePlan = PlanWithEveryRecipeVariant(),
                TemplateProducer = true
            };

        var rendered = Render([instruction]);
        var parsed = StackInstructionTextFormat.ParseInstructions(rendered.Text, hash => rendered.ObservedValues[hash]);

        AssertInstruction(instruction, parsed.Should().ContainSingle().Which);
        Render(parsed).Text.Should().Be(rendered.Text);
        AssertFullHashes(rendered);
    }

    [Theory]
    [MemberData(nameof(Values))]
    public void Persistence_restores_exact_value_encodings_with_bounded_summaries(PineValue value)
    {
        var rendered = Render([StackInstruction.Push_Literal(value)]);
        var parsed = StackInstructionTextFormat.ParseInstructions(rendered.Text, hash => rendered.ObservedValues[hash]);

        parsed[0].Literal!.Evaluate().Should().Be(value);
        rendered.Text.Length.Should().BeLessThan(700);
        rendered.ObservedValues[Hash(value)].Should().Be(value);
        AssertFullHashes(rendered);
    }

    [Fact]
    public void Persistence_uses_the_shared_readable_listing_and_full_references()
    {
        var value = IntegerEncoding.EncodeSignedInteger(3);
        var rendered = Render([StackInstruction.Push_Literal(value), StackInstruction.Jump_If_Equal(0, value)]);

        rendered.Text.Should().Be(
            $"""
            pine-ir 1

            sequence
            0: Push_Literal (Literal: Blob [2] (0x0403 | int 3 | hash 0x{Hash(value)}))
            jumps_arriving_from 1 (1)
            1: Jump_If_Equal_Const (Literal: Blob [2] (0x0403 | int 3 | hash 0x{Hash(value)}), JumpOffset: 0 (to 1))
            """);

        var calls = new List<string>();

        StackInstructionTextFormat.ParseInstructions(
            rendered.Text,
            hash =>
            {
                calls.Add(hash);
                return rendered.ObservedValues[hash];
            });

        calls.Should().Equal(Hash(value));
    }

    [Fact]
    public void Persistence_preserves_default_empty_and_null_fields()
    {
        StackInstruction[] instructions =
            [
                StackInstruction.Return,
                new(
                    StackInstructionKind.Return,
                    LocalIndices: [],
                    SliceSwitchCases: [],
                    SwitchJumpTable: ImmutableDictionary<PineValue, int>.Empty),
                new(StackInstructionKind.Local_Set, LocalIndices: [0]),
                StackInstruction.Local_Set([0], popCount: 0),
                StackInstruction.Int_Mul_Const_Add_Binary(-1),
                new(StackInstructionKind.Return) { TemplateProducer = true }
            ];

        var rendered = Render(instructions);
        var parsed = StackInstructionTextFormat.ParseInstructions(rendered.Text, hash => rendered.ObservedValues[hash]);

        for (var index = 0; index < instructions.Length; ++index)
            AssertInstruction(instructions[index], parsed[index]);
    }

    [Fact]
    public void Persistence_preserves_slice_case_priority_and_dictionary_determinism()
    {
        var shortValue = PineValue.Blob([4]);
        var longValue = PineValue.Blob([4, 5]);

        var slices =
            new StackInstruction(
                StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const,
                IntegerLiteral: 4,
                SliceSwitchCases: [new(longValue, 8), new(shortValue, -1)]);

        var firstTable = ImmutableDictionary<PineValue, int>.Empty.Add(shortValue, 4).Add(longValue, 4);
        var secondTable = ImmutableDictionary<PineValue, int>.Empty.Add(longValue, 4).Add(shortValue, 4);
        var first = Render([slices, new(StackInstructionKind.Switch_Jump_If_Equal_Const, SwitchJumpTable: firstTable)]);

        var second =
            Render([slices, new(StackInstructionKind.Switch_Jump_If_Equal_Const, SwitchJumpTable: secondTable)]);

        first.Text.Should().Be(second.Text);
        var parsed = StackInstructionTextFormat.ParseInstructions(first.Text, hash => first.ObservedValues[hash]);
        parsed[0].SliceSwitchCases.Should().Equal(slices.SliceSwitchCases);
    }

    [Theory]
    [InlineData(32)]
    [InlineData(33)]
    [InlineData(1000)]
    public void Persistence_bounds_large_switch_details_without_losing_cases(int count)
    {
        var cases =
            Enumerable.Range(0, count)
            .Select(index => new SliceSwitchCase(IntegerEncoding.EncodeSignedInteger(index), count - index))
            .ToImmutableArray();

        StackInstruction[] instructions =
            [
                new(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable: cases.ToImmutableDictionary(item => item.Literal, item => item.JumpOffset)),
                new(
                    StackInstructionKind.Switch_Jump_If_Slice_Skip_Var_Equal_Const,
                    IntegerLiteral: 4,
                    SliceSwitchCases: cases)
            ];

        var persisted = Render(instructions);

        var parsed =
            StackInstructionTextFormat.ParseInstructions(persisted.Text, hash => persisted.ObservedValues[hash]);

        for (var index = 0; index < instructions.Length; ++index)
            AssertInstruction(instructions[index], parsed[index]);

        if (count > 32)
        {
            persisted.Text.Length.Should().BeLessThan(700);

            StackInstructionTraceRenderer.RenderInstructions(
                instructions,
                new StackInstructionTraceRenderer.RenderingOptions()).Text.Length.Should().BeLessThan(400);

            string.Join("\n", instructions.Select(instruction => instruction.ToString())).Length.Should().BeLessThan(
                400);
        }

        AssertFullHashes(persisted);
    }

    [Fact]
    public void Persistence_bounds_incoming_jump_annotations_and_returns_their_referenced_values()
    {
        StackInstruction[] instructions =
            [
            .. Enumerable.Range(0, 100).Select(index => StackInstruction.Jump_Unconditional(100 - index)),
            StackInstruction.Return
            ];

        var rendered = Render(instructions);

        var annotation =
            rendered.Text.Split('\n').Single(line => line.StartsWith("jumps_arriving_from ", StringComparison.Ordinal));

        annotation.Length.Should().BeLessThan(200);
        annotation.Should().Contain("hash 0x");
        var parsed = StackInstructionTextFormat.ParseInstructions(rendered.Text, hash => rendered.ObservedValues[hash]);
        parsed.Should().Equal(instructions);
        AssertFullHashes(rendered);
    }

    [Fact]
    public void Persistence_restores_frame_metadata_and_recursive_shared_link_graphs()
    {
        var rootCall =
            StackInstruction.Invoke_StackFrame_Const(Expression.EnvironmentInstance, StaticFunctionInterface.Generic);

        var childCall =
            StackInstruction.Invoke_StackFrame_Const(Expression.EnvironmentInstance, StaticFunctionInterface.Generic);

        var root =
            new StackFrameInstructions(
                StaticFunctionInterface.FromPathsSorted([[0, 1], [1]]),
                [StackInstruction.Local_Get(0), rootCall, rootCall, StackInstruction.Return],
                PineValueClass.Create(
                    [
                        new KeyValuePair<IReadOnlyList<int>, PineValue>([0, 0], PineValue.EmptyBlob),
                        new KeyValuePair<IReadOnlyList<int>, PineValue>([1], PineValue.List([PineValue.Blob([255])]))
                    ]))
            {
                LocalsCount = 99,
                MaxStackUsage = 123
            };

        var child =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [StackInstruction.Local_Get(0), childCall, StackInstruction.Return],
                PineValueClass.Empty);

        rootCall.SetLinkedStackFrameInstructions(child);
        childCall.SetLinkedStackFrameInstructions(root);

        var rendered = StackInstructionTraceRenderer.RenderStackFrameInstructions(root, s_persistence);

        var parsed =
            StackInstructionTextFormat.ParseStackFrameInstructions(rendered.Text, hash => rendered.ObservedValues[hash]);

        var restoredChild = parsed.Instructions[1].LinkedStackFrameInstructions!;

        parsed.Parameters.Should().Be(root.Parameters);
        parsed.TrackEnvConstraint.Should().Be(root.TrackEnvConstraint);
        parsed.LocalsCount.Should().Be(99);
        parsed.MaxStackUsage.Should().Be(123);
        restoredChild.TrackEnvConstraint.Should().Be(PineValueClass.Empty);
        parsed.Instructions[2].LinkedStackFrameInstructions.Should().BeSameAs(restoredChild);
        restoredChild.Instructions[1].LinkedStackFrameInstructions.Should().BeSameAs(parsed);

        StackInstructionTraceRenderer.RenderStackFrameInstructions(parsed, s_persistence).Text.Should().Be(
            rendered.Text);

        AssertFullHashes(rendered);
    }

    [Fact]
    public void Persistence_handles_empty_sequences_and_frames_and_unlinked_invocations()
    {
        var empty = Render([]);
        StackInstructionTextFormat.ParseInstructions(empty.Text, _ => null).Should().BeEmpty();

        var frame = new StackFrameInstructions(StaticFunctionInterface.ZeroParameters, [], null, 0, 0);
        var rendered = StackInstructionTraceRenderer.RenderStackFrameInstructions(frame, s_persistence);

        var parsed =
            StackInstructionTextFormat.ParseStackFrameInstructions(rendered.Text, hash => rendered.ObservedValues[hash]);

        parsed.Instructions.Should().BeEmpty();
        parsed.TrackEnvConstraint.Should().BeNull();
        parsed.MaxStackUsage.Should().Be(0);

        var call =
            StackInstruction.Invoke_StackFrame_Const(
                Expression.EnvironmentInstance,
                StaticFunctionInterface.ZeroParameters);

        var unlinked = Render([call]);

        var parsedCall =
            StackInstructionTextFormat.ParseInstructions(unlinked.Text, hash => unlinked.ObservedValues[hash])[0];

        AssertInstruction(call, parsedCall);
        parsedCall.LinkedStackFrameInstructions.Should().BeNull();

        var requireFrame = () => StackInstructionTextFormat.ParseStackFrameInstructions(empty.Text, _ => null);
        requireFrame.Should().Throw<FormatException>().WithMessage("*root frame*");
    }

    [Fact]
    public void Persistence_distinguishes_structurally_equal_linked_frames()
    {
        var child =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Return
                ]);

        var otherChild = child with { TrackEnvConstraint = PineValueClass.Empty };
        var first = StackInstruction.Invoke_StackFrame_Const(Expression.EnvironmentInstance, child.Parameters);
        var second = StackInstruction.Invoke_StackFrame_Const(Expression.EnvironmentInstance, otherChild.Parameters);
        first.SetLinkedStackFrameInstructions(child);
        second.SetLinkedStackFrameInstructions(otherChild);

        var rendered = Render([first, second]);
        var parsed = StackInstructionTextFormat.ParseInstructions(rendered.Text, hash => rendered.ObservedValues[hash]);
        parsed[0].LinkedStackFrameInstructions.Should().NotBeSameAs(parsed[1].LinkedStackFrameInstructions);
        parsed[0].LinkedStackFrameInstructions!.TrackEnvConstraint.Should().BeNull();
        parsed[1].LinkedStackFrameInstructions!.TrackEnvConstraint.Should().Be(PineValueClass.Empty);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Persistence_restores_recognized_template_plan_behavior(bool producer)
    {
        var entry = FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(Expression.EnvironmentInstance, 4)!;
        var plan = ExpressionTemplatePlan.TryParseTemplate(entry, new PineVMParseCache())!;
        var instruction = StackInstruction.CreateTemplate(plan, producer);
        var rendered = Render([instruction]);

        var parsed =
            StackInstructionTextFormat.ParseInstructions(rendered.Text, hash => rendered.ObservedValues[hash])[0];

        AssertInstruction(instruction, parsed);

        PineValueInProcess[] environments =
            [
                PineValueInProcess.Create(PineValue.Blob([1])),
                PineValueInProcess.Create(PineValue.List([PineValue.EmptyBlob])),
                PineValueInProcess.EmptyList,
                PineValueInProcess.EmptyBlob
            ];

        for (var count = 0; count < 4; ++count)
        {
            parsed.TemplatePlan!.Materialize(environments[..count], new DirectInterpreterCounters())
                .Should().Be(plan.Materialize(environments[..count], new DirectInterpreterCounters()));
        }

        parsed.TemplatePlan!.BuildTerminalEnvironment(environments).Evaluate()
            .Should().Be(plan.BuildTerminalEnvironment(environments).Evaluate());
    }

    [Fact]
    public void Persistence_preserves_recipe_DAG_sharing_without_expanding_shared_subtrees()
    {
        TemplateValueRecipe recipe = new TemplateValueRecipe.Slot(0);

        for (var depth = 0; depth < 500; ++depth)
            recipe = new TemplateValueRecipe.List([recipe, recipe]);

        var plan =
            new ExpressionTemplatePlan(
                PineValue.EmptyList,
                ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                Expression.EnvironmentInstance,
                1,
                [recipe, recipe],
                recipe);

        var instruction = StackInstruction.CreateTemplate(plan, producer: true);
        var rendered = Render([instruction]);
        rendered.Text.Length.Should().BeLessThan(700);

        rendered.ObservedValues.Values.OfType<PineValue.ListValue>().Max(value => value.NodesCount).Should().BeLessThan(
            10000);

        var restored =
            StackInstructionTextFormat.ParseInstructions(rendered.Text, hash => rendered.ObservedValues[hash])[0].TemplatePlan!;

        restored.IntermediateRecipes[0].Should().BeSameAs(restored.IntermediateRecipes[1]);
        restored.IntermediateRecipes[0].Should().BeSameAs(restored.TerminalEnvironmentRecipe);
        var current = restored.TerminalEnvironmentRecipe;

        for (var depth = 0; depth < 500; ++depth)
        {
            var list = current.Should().BeOfType<TemplateValueRecipe.List>().Which;
            list.Items[0].Should().BeSameAs(list.Items[1]);
            current = list.Items[0];
        }

        current.Should().BeOfType<TemplateValueRecipe.Slot>().Which.Index.Should().Be(0);

        AssertInstruction(
            instruction,
            StackInstructionTextFormat.ParseInstructions(rendered.Text, hash => rendered.ObservedValues[hash])[0]);
    }

    [Fact]
    public void Persistence_captures_recipe_dependencies_once_for_traversal_and_encoding()
    {
        var shared = new TemplateValueRecipe.Slot(0);
        var path = new SingleReadList<int>([2, 1]);

        var children =
            new SingleReadList<TemplateValueRecipe>(
                [shared, new TemplateValueRecipe.ProjectedSlot(1, path), shared]);

        var recipe = new TemplateValueRecipe.List(children);
        var intermediates = new SingleReadList<TemplateValueRecipe>([recipe, recipe]);

        var plan =
            new ExpressionTemplatePlan(
                PineValue.EmptyList,
                ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                Expression.EnvironmentInstance,
                2,
                intermediates,
                recipe);

        var encoded = StackInstructionMetadataEncoding.EncodePlan(plan);
        var restored = StackInstructionMetadataEncoding.DecodePlan(encoded);
        var list = restored.TerminalEnvironmentRecipe.Should().BeOfType<TemplateValueRecipe.List>().Which;

        path.ReadCounts.Should().Equal(1, 1);
        children.ReadCounts.Should().Equal(1, 1, 1);
        intermediates.ReadCounts.Should().Equal(1, 1);
        restored.IntermediateRecipes[0].Should().BeSameAs(list);
        restored.IntermediateRecipes[1].Should().BeSameAs(list);
        list.Items[0].Should().BeSameAs(list.Items[2]);
        list.Items[1].Should().BeOfType<TemplateValueRecipe.ProjectedSlot>().Which.Path.Should().Equal(2, 1);
        StackInstructionMetadataEncoding.EncodePlan(restored).Should().Be(encoded);
    }

    [Fact]
    public void Persistence_rejects_invalid_recipe_graphs_and_plan_flags()
    {
        var plan = PlanWithEveryRecipeVariant();
        var encoded = StackInstructionMetadataEncoding.EncodePlan(plan).Should().BeOfType<PineValue.ListValue>().Which;
        var mutations = new List<PineValue[]>();

        var badRoot = encoded.Items.ToArray();
        badRoot[5] = IntegerEncoding.EncodeSignedInteger(999);
        mutations.Add(badRoot);

        var badFlag = encoded.Items.ToArray();
        badFlag[6] = IntegerEncoding.EncodeSignedInteger(2);
        mutations.Add(badFlag);

        var forwardReference = encoded.Items.ToArray();

        forwardReference[7] =
            PineValue.List(
                [
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(2),
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(0)])
                    ])
                ]);

        mutations.Add(forwardReference);

        var unknownTag = encoded.Items.ToArray();

        unknownTag[7] =
            PineValue.List(
                [
                PineValue.List([IntegerEncoding.EncodeSignedInteger(99), PineValue.EmptyList])
                ]);

        mutations.Add(unknownTag);

        foreach (var fields in mutations)
        {
            var value = PineValue.List(fields);
            var text = $"pine-ir 1\nsequence\n0: Eval_Const (TemplatePlan: hash 0x{Hash(value)})";
            var parse = () => StackInstructionTextFormat.ParseInstructions(text, _ => value);
            parse.Should().Throw<FormatException>();
        }

        var items = new List<TemplateValueRecipe>();
        var cyclic = new TemplateValueRecipe.List(items);
        items.Add(cyclic);

        var render =
            () => Render(
                [
                StackInstruction.CreateTemplate(plan with { TerminalEnvironmentRecipe = cyclic }, producer: false)
                ]);

        render.Should().Throw<InvalidOperationException>().WithMessage("*Cyclic template recipe*");
    }

    [Theory]
    [InlineData(3)]
    [InlineData(4)]
    public void Persistence_restored_compiler_output_executes_in_PineVM(int argument)
    {
        var expression =
            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst(
                        [
                        Expression.EnvironmentInstance,
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(3))
                        ])),
                falseBranch: Expression.LitralInst(PineValue.EmptyList),
                trueBranch:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.ListInst(
                        [
                        Expression.EnvironmentInstance,
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(1))
                        ])));

        var compiled =
            ExpressionCompilation.CompileExpression(
                expression,
                specializations: [],
                new PineVMParseCache(),
                disableReduction: true,
                skipInlining: (_, _) => false,
                enableTailRecursionOptimization: false);

        var rendered = StackInstructionTraceRenderer.RenderStackFrameInstructions(compiled.Generic, s_persistence);

        var restored =
            StackInstructionTextFormat.ParseStackFrameInstructions(rendered.Text, hash => rendered.ObservedValues[hash]);

        var result = Execute(expression, restored, IntegerEncoding.EncodeSignedInteger(argument));
        result.Should().Be(argument is 3 ? IntegerEncoding.EncodeSignedInteger(4) : PineValue.EmptyList);
    }

    [Fact]
    public void Persistence_restored_linked_invocations_execute_in_PineVM()
    {
        var expression = Expression.EnvironmentInstance;

        var child =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [StackInstruction.Local_Get(0), StackInstruction.Int_Add_Const(1), StackInstruction.Return]);

        var first = StackInstruction.Invoke_StackFrame_Const(expression, child.Parameters);
        var second = StackInstruction.Invoke_StackFrame_Const(expression, child.Parameters);
        first.SetLinkedStackFrameInstructions(child);
        second.SetLinkedStackFrameInstructions(child);

        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [StackInstruction.Local_Get(0), first, second, StackInstruction.Return]);

        var rendered = StackInstructionTraceRenderer.RenderStackFrameInstructions(frame, s_persistence);

        var restored =
            StackInstructionTextFormat.ParseStackFrameInstructions(rendered.Text, hash => rendered.ObservedValues[hash]);

        Execute(expression, restored, IntegerEncoding.EncodeSignedInteger(3))
            .Should().Be(IntegerEncoding.EncodeSignedInteger(5));
    }

    [Fact]
    public void Persistence_ignores_redundant_summaries_and_accepts_CRLF()
    {
        StackInstruction[] instructions =
            [
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(3)),
                StackInstruction.Jump_If_Equal(-1, IntegerEncoding.EncodeSignedInteger(3))
            ];

        var rendered = Render(instructions);

        var modified =
            rendered.Text
            .Replace("Blob [2]", "Blob [999]", StringComparison.Ordinal)
            .Replace("0x0403 | int 3", "0xffff | int 123", StringComparison.Ordinal)
            .Replace("(to 0)", "(to 999)", StringComparison.Ordinal)
            .Replace("jumps_arriving_from 1 (1)", "jumps_arriving_from ignored", StringComparison.Ordinal)
            .Replace("\n", "\r\n", StringComparison.Ordinal);

        var parsed =
            StackInstructionTextFormat.ParseInstructions(
                "\r\n" + modified + "\r\n",
                hash => rendered.ObservedValues[hash]);

        for (var index = 0; index < instructions.Length; ++index)
            AssertInstruction(instructions[index], parsed[index]);
    }

    [Fact]
    public void Persistence_reports_missing_corrupt_and_malformed_references()
    {
        var rendered = Render([StackInstruction.Push_Literal(PineValue.Blob([255]))]);
        var missing = () => StackInstructionTextFormat.ParseInstructions(rendered.Text, _ => null);
        missing.Should().Throw<FormatException>().WithMessage("*Missing value*");
        var corrupt = () => StackInstructionTextFormat.ParseInstructions(rendered.Text, _ => PineValue.EmptyBlob);
        corrupt.Should().Throw<FormatException>().WithMessage("*does not match SHA256*");

        foreach (var replacement in new[] { "abc", new string('a', 63), new string('a', 65), new string('z', 64) })
        {
            var malformed = rendered.Text.Replace(Hash(PineValue.Blob([255])), replacement, StringComparison.Ordinal);

            var parse =
                () => StackInstructionTextFormat.ParseInstructions(
                    malformed,
                    _ => throw new InvalidOperationException("Must not call resolver."));

            parse.Should().Throw<FormatException>().WithMessage("*64 hexadecimal*");
        }

        var failure = new InvalidOperationException("Storage unavailable.");
        var resolverError = () => StackInstructionTextFormat.ParseInstructions(rendered.Text, _ => throw failure);
        resolverError.Should().Throw<InvalidOperationException>().Which.Should().BeSameAs(failure);
    }

    [Theory]
    [InlineData("")]
    [InlineData("pine-ir 2\nsequence")]
    [InlineData("pine-ir 1")]
    [InlineData("pine-ir 1\nsequence\nsequence")]
    [InlineData("pine-ir 1\nsequence\n1: Return")]
    [InlineData("pine-ir 1\nsequence\n0: NotAnInstruction")]
    [InlineData("pine-ir 1\nsequence\n0: 0")]
    [InlineData("pine-ir 1\nsequence\n0: Return (Unknown: 1)")]
    [InlineData("pine-ir 1\nsequence\n0: Pop (PopCount: 1, PopCount: 2)")]
    [InlineData("pine-ir 1\nsequence\n0: Pop (PopCount: 2147483648)")]
    [InlineData("pine-ir 1\nsequence\n0: Return (TemplateProducer: false)")]
    [InlineData("pine-ir 1\nsequence\n0: Return (LinkedFrame: 0)")]
    [InlineData("pine-ir 1\nsequence\n0: Return (LocalIndices: [1, 2)")]
    [InlineData("pine-ir 1\nsequence\n0: Return (Literal: Blob [2] (int 1))")]
    [InlineData("pine-ir 1\nsequence\n0: Return (IntegerLiteral: nonsense)")]
    [InlineData("pine-ir 1\nsequence\n0: Return (LocalIndices: [1, x])")]
    [InlineData("pine-ir 1\nsequence\n0: Return (SwitchJumpTable: 1)\ncase : jump (1, 2)")]
    [InlineData("pine-ir 1\nsequence\n0: Return (SliceSwitchCases: 1)\nslice case : jump (1, 2)")]
    [InlineData("pine-ir 1\nsequence\n0: Return (SwitchJumpTable: 1)\ncase missing jump")]
    [InlineData("pine-ir 1\nsequence\n0: Return (SliceSwitchCases: 1)\nslice case missing jump")]
    public void Persistence_rejects_malformed_documents(string text)
    {
        var parse = () => StackInstructionTextFormat.ParseInstructions(text, _ => null);
        parse.Should().Throw<FormatException>();
    }

    [Theory]
    [InlineData("InvocationExpression")]
    [InlineData("InvocationInterface")]
    [InlineData("LocalIndices")]
    [InlineData("TemplatePlan")]
    public void Persistence_rejects_invalid_referenced_metadata(string field)
    {
        var invalid = IntegerEncoding.EncodeSignedInteger(1);

        var extra =
            field is "InvocationExpression"
            ?
            $", InvocationExpressionEncoded: hash 0x{Hash(invalid)}, InvocationInterface: hash 0x{Hash(invalid)}"
            :
            "";

        var text = $"pine-ir 1\nsequence\n0: Return ({field}: hash 0x{Hash(invalid)}{extra})";
        var parse = () => StackInstructionTextFormat.ParseInstructions(text, _ => invalid);
        parse.Should().Throw<FormatException>();
    }

    [Fact]
    public void Persistence_rejects_duplicate_constraint_paths_and_invalid_metadata_integers()
    {
        var duplicatePath = PineValue.List([PineValue.EmptyList, PineValue.EmptyBlob]);
        var duplicateConstraint = PineValue.List([duplicatePath, duplicatePath]);
        var parameters = PineValue.EmptyList;

        var text =
            $"pine-ir 1\nframe 0 (Parameters: hash 0x{Hash(parameters)}, LocalsCount: 0, MaxStackUsage: 0, TrackEnvConstraint: hash 0x{Hash(duplicateConstraint)})";

        var parse =
            () => StackInstructionTextFormat.ParseStackFrameInstructions(
                text,
                hash => hash == Hash(parameters) ? parameters : duplicateConstraint);

        parse.Should().Throw<FormatException>().WithMessage("*Duplicate environment constraint path*");

        foreach (var invalid in new[]
        {
            PineValue.List([IntegerEncoding.EncodeSignedInteger(BigInteger.One << 32)]),
            PineValue.List([PineValue.EmptyList]),
            PineValue.List([PineValue.Blob([4, 0, 1])])
        })
        {
            var invalidPath = $"pine-ir 1\nsequence\n0: Local_Get (LocalIndices: hash 0x{Hash(invalid)})";
            var load = () => StackInstructionTextFormat.ParseInstructions(invalidPath, _ => invalid);
            load.Should().Throw<FormatException>();
        }
    }

    [Fact]
    public void Persistence_rejects_undefined_links_duplicate_frames_and_cases()
    {
        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Return
                ]);

        var call = StackInstruction.Invoke_StackFrame_Const(Expression.EnvironmentInstance, frame.Parameters);
        call.SetLinkedStackFrameInstructions(frame);
        var rendered = Render([call]);

        var undefined =
            () => StackInstructionTextFormat.ParseInstructions(
                rendered.Text.Replace("LinkedFrame: 0", "LinkedFrame: 99", StringComparison.Ordinal),
                hash => rendered.ObservedValues[hash]);

        undefined.Should().Throw<FormatException>().WithMessage("*Unknown linked frame*");

        var section = rendered.Text[rendered.Text.IndexOf("frame 0", StringComparison.Ordinal)..];

        var duplicate =
            () => StackInstructionTextFormat.ParseInstructions(
                rendered.Text + "\n" + section,
                hash => rendered.ObservedValues[hash]);

        duplicate.Should().Throw<FormatException>().WithMessage("*duplicate frame*");

        var switchText =
            Render(
                [
                new(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable: ImmutableDictionary<PineValue, int>.Empty.Add(PineValue.EmptyBlob, 1))
                ]);

        var detail = switchText.Text.Split('\n').Last();

        var duplicateCase =
            () => StackInstructionTextFormat.ParseInstructions(
                switchText.Text + "\n" + detail,
                hash => switchText.ObservedValues[hash]);

        duplicateCase.Should().Throw<FormatException>().WithMessage("*duplicate switch case*");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Persistence_parses_referenced_case_collections_without_losing_duplicate_semantics(bool slice)
    {
        var value = PineValue.Blob([255]);

        var cases =
            PineValue.List(
                [
                    PineValue.List([value, IntegerEncoding.EncodeSignedInteger(1)]),
                    PineValue.List([value, IntegerEncoding.EncodeSignedInteger(2)])
                ]);

        var field = slice ? "SliceSwitchCases" : "SwitchJumpTable";
        var text = $"pine-ir 1\nsequence\n0: Return ({field}: List [2] (hash 0x{Hash(cases)}))";
        var parse = () => StackInstructionTextFormat.ParseInstructions(text, _ => cases);

        if (slice)
        {
            parse().Should().ContainSingle().Which.SliceSwitchCases.Should().Equal(
                new SliceSwitchCase(value, 1),
                new SliceSwitchCase(value, 2));
        }
        else
            parse.Should().Throw<FormatException>().WithMessage("*Duplicate switch case*");
    }

    [Fact]
    public void Inspection_bounds_all_variable_size_inline_details_and_collects_observed_values()
    {
        var huge = BigInteger.One << 40000;
        var manyLocals = Enumerable.Range(0, 10000).ToImmutableArray();
        var prefix = PineValue.List(Enumerable.Repeat<PineValue>(PineValue.EmptyBlob, 10000).ToArray());
        var expression = Expression.LitralInst(StringEncoding.ValueFromString(new string('x', 10000)));

        StackInstruction[] instructions =
            [
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(huge)),
                StackInstruction.Int_Add_Const(huge),
                StackInstruction.Local_Get(manyLocals),
                StackInstruction.Build_List_With_Prefix(prefix, 1),
                StackInstruction.Invoke_StackFrame_Const(expression, StaticFunctionInterface.ZeroParameters)
            ];

        var rendered =
            StackInstructionTraceRenderer.RenderInstructions(
                instructions,
                new StackInstructionTraceRenderer.RenderingOptions(HashHexLength: 64));

        rendered.Text.Length.Should().BeLessThan(2000);
        string.Join("\n", instructions.Select(instruction => instruction.ToString())).Length.Should().BeLessThan(2000);
        AssertFullHashes(rendered);
        rendered.ObservedValues.Should().ContainKey(Hash(IntegerEncoding.EncodeSignedInteger(huge)));
        rendered.ObservedValues.Should().ContainKey(Hash(prefix));
        rendered.ObservedValues.Should().ContainKey(Hash(ExpressionEncoding.EncodeExpressionAsValue(expression)));
        rendered.ObservedValues.Should().ContainKey(Hash(StackInstructionMetadataEncoding.EncodePath(manyLocals)));

        var persisted = Render(instructions);

        var parsed =
            StackInstructionTextFormat.ParseInstructions(persisted.Text, hash => persisted.ObservedValues[hash]);

        for (var index = 0; index < instructions.Length; ++index)
            AssertInstruction(instructions[index], parsed[index]);
    }

    [Fact]
    public void Inspection_includes_a_reference_when_UTF32_text_is_truncated_at_a_surrogate_pair()
    {
        var blob = StringEncoding.ValueFromString(string.Concat(Enumerable.Repeat(char.ConvertFromUtf32(0x1f600), 32)));

        var rendered =
            StackInstructionTraceRenderer.RenderInstructions(
                [StackInstruction.Push_Literal(blob)],
                new StackInstructionTraceRenderer.RenderingOptions());

        rendered.Text.Should().Contain("hash 0x" + Hash(blob)[..8]);
        rendered.Text.Should().Contain("...");
    }

    [Fact]
    public void Inspection_custom_string_limits_and_large_integers_use_hash_fallbacks()
    {
        var text = StringEncoding.ValueFromString("abcdef");

        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.ZeroParameters,
                [StackInstruction.Push_Literal(text), StackInstruction.Return]);

        var rendered =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                frame,
                StackInstructionTraceRenderer.BuildDefaultBlobRepresentations(2, 2));

        rendered.Should().Contain("UTF32 \"ab...\"");
        rendered.Should().Contain("hash 0x" + Hash(text)[..8]);

        var huge = IntegerEncoding.EncodeSignedInteger(BigInteger.One << 4000);
        var hugeBlob = huge.Should().BeOfType<PineValue.BlobValue>().Which;
        StackInstructionTraceRenderer.BuildBlobRepresentationStrictPineInteger().Render(hugeBlob).Should().BeNull();

        StackInstructionTraceRenderer.BuildBlobRepresentationCanonicalHash().Render(hugeBlob)
            .Should().Be("hash 0x" + Hash(huge)[..8]);
    }

    [Fact]
    public void Persistence_is_culture_independent_and_forces_full_hashes()
    {
        var previous = CultureInfo.CurrentCulture;

        try
        {
            var instructions =
                new[] { StackInstruction.Int_Add_Const(-123), StackInstruction.Push_Literal(PineValue.Blob([255])) };

            var expected = Render(instructions);
            var culture = (CultureInfo)CultureInfo.InvariantCulture.Clone();
            culture.NumberFormat.NegativeSign = "minus";
            CultureInfo.CurrentCulture = culture;

            var actual =
                StackInstructionTraceRenderer.RenderInstructions(
                    instructions,
                    new StackInstructionTraceRenderer.RenderingOptions(ForPersistence: true, HashHexLength: 1));

            actual.Text.Should().Be(expected.Text);
            var parsed = StackInstructionTextFormat.ParseInstructions(actual.Text, hash => actual.ObservedValues[hash]);
            parsed[0].IntegerLiteral.Should().Be(new BigInteger(-123));
        }
        finally
        {
            CultureInfo.CurrentCulture = previous;
        }
    }

    [Theory]
    [InlineData(0)]
    [InlineData(65)]
    public void Inspection_rejects_invalid_hash_lengths(int length)
    {
        var render =
            () => StackInstructionTraceRenderer.RenderInstructions(
                [],
                new StackInstructionTraceRenderer.RenderingOptions(HashHexLength: length));

        render.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Fact]
    public void Inspection_options_preserve_the_existing_listing_format()
    {
        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                    StackInstruction.Local_Get(0),
                    StackInstruction.Push_Literal(
                        PineValue.List(Enumerable.Repeat<PineValue>(PineValue.EmptyBlob, 1234).ToArray())),
                    StackInstruction.Concat_Binary,
                    StackInstruction.Return
                ]);

        StackInstructionTraceRenderer.RenderStackFrameInstructions(
            frame,
            new StackInstructionTraceRenderer.RenderingOptions())
            .Text.Should().Be(StackInstructionTraceRenderer.RenderStackFrameInstructions(frame));
    }

    private static ExpressionTemplatePlan PlanWithEveryRecipeVariant()
    {
        TemplateValueRecipe[] recipes =
            [
                new TemplateValueRecipe.Constant(PineValue.Blob([255])),
                new TemplateValueRecipe.Slot(0),
                new TemplateValueRecipe.ProjectedSlot(1, [0, 2]),
                new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(2), new TemplateValueRecipe.List([])])
            ];

        return
            new(
                PineValue.EmptyBlob,
                ExpressionEncoding2024.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                Expression.EnvironmentInstance,
                5,
                recipes,
                new TemplateValueRecipe.List(recipes),
                ElidesTerminalEval: true);
    }

    private static StackInstructionTraceRenderer.RenderedInstructions Render(
        IReadOnlyList<StackInstruction> instructions) =>
        StackInstructionTraceRenderer.RenderInstructions(instructions, s_persistence);

    private static PineValue Execute(Expression expression, StackFrameInstructions frame, PineValue environment)
    {
        var trace = new List<ExecutedStackInstruction>();

        var vm =
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
                reportExecutedStackInstruction: (in item) => trace.Add(item),
                expressionCompilationOverrides:
                new Dictionary<Expression, ExpressionCompilation> { [expression] = new(frame, []) });

        var result =
            vm.EvaluateExpressionOnCustomStack(
                expression,
                environment,
                new Core.Runtime.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null))
            .Extract(error => throw new InvalidOperationException("Failed eval: " + error));

        trace.Should().Contain(item => ReferenceEquals(item.Instruction, frame.Instructions[0]));
        return result.ReturnValue.Evaluate();
    }

    private static string Hash(PineValue value) =>
        Convert.ToHexStringLower(PineValueHashTree.ComputeHash(value).Span);

    private static void AssertFullHashes(StackInstructionTraceRenderer.RenderedInstructions rendered)
    {
        foreach (Match match in Regex.Matches(rendered.Text, @"hash 0x([0-9a-f]+)"))
        {
            var hash = match.Groups[1].Value;
            hash.Length.Should().Be(64);
            rendered.ObservedValues.Should().ContainKey(hash);
        }

        foreach (var (hash, value) in rendered.ObservedValues)
            hash.Should().Be(Hash(value));
    }

    private static void AssertInstruction(StackInstruction expected, StackInstruction actual)
    {
        actual.Kind.Should().Be(expected.Kind);
        actual.Literal?.Evaluate().Should().Be(expected.Literal?.Evaluate());
        actual.IntegerLiteral.Should().Be(expected.IntegerLiteral);
        actual.SkipCount.Should().Be(expected.SkipCount);
        actual.TakeCount.Should().Be(expected.TakeCount);
        actual.JumpOffset.Should().Be(expected.JumpOffset);
        actual.ShiftCount.Should().Be(expected.ShiftCount);
        actual.PopCount.Should().Be(expected.PopCount);
        actual.LocalIndices.IsDefault.Should().Be(expected.LocalIndices.IsDefault);

        if (!expected.LocalIndices.IsDefault)
            actual.LocalIndices.Should().Equal(expected.LocalIndices);

        actual.TemplateProducer.Should().Be(expected.TemplateProducer);
        (actual.TemplatePlan is null).Should().Be(expected.TemplatePlan is null);

        if (expected.TemplatePlan is { } plan)
        {
            StackInstructionMetadataEncoding.EncodePlan(actual.TemplatePlan!).Should().Be(
                StackInstructionMetadataEncoding.EncodePlan(plan));
        }

        (actual.OptimizedInvocation is null).Should().Be(expected.OptimizedInvocation is null);

        if (expected.OptimizedInvocation is { } invocation)
        {
            actual.OptimizedInvocation!.Expression.Should().Be(invocation.Expression);
            actual.OptimizedInvocation.ExpressionEncoded.Should().Be(invocation.ExpressionEncoded);
            actual.OptimizedInvocation.InvocationInterface.Should().Be(invocation.InvocationInterface);
        }

        (actual.SwitchJumpTable is null).Should().Be(expected.SwitchJumpTable is null);

        if (expected.SwitchJumpTable is { } table)
            actual.SwitchJumpTable.Should().BeEquivalentTo(table);

        actual.SliceSwitchCases.IsDefault.Should().Be(expected.SliceSwitchCases.IsDefault);

        if (!expected.SliceSwitchCases.IsDefault)
            actual.SliceSwitchCases.Should().Equal(expected.SliceSwitchCases);
    }

    private sealed class SingleReadList<T>(IReadOnlyList<T> items) : IReadOnlyList<T>
    {
        public int[] ReadCounts { get; } = new int[items.Count];

        public int Count => ReadCounts.Length;

        public T this[int index]
        {
            get
            {
                if (++ReadCounts[index] != 1)
                    throw new InvalidOperationException("The source item was read more than once.");

                return items[index];
            }
        }

        public IEnumerator<T> GetEnumerator()
        {
            for (var index = 0; index < Count; ++index)
                yield return this[index];
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() =>
            GetEnumerator();
    }
}
