using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class StackInstructionTraceRendererTests
{
    [Theory]
    [InlineData(-1, "Int_Sub_Binary")]
    [InlineData(0, "Int_Mul_Const_Add_Binary (0)")]
    [InlineData(4, "Int_Mul_Const_Add_Binary (4)")]
    public void Int_mul_const_add_binary_uses_legacy_subtraction_rendering_for_minus_one(
        int multiplier,
        string expected)
    {
        StackInstruction.Int_Mul_Const_Add_Binary(multiplier)
            .ToString()
            .Should()
            .Be(expected);
    }

    [Fact]
    public void RenderInstructionTraceWithDefaultBlobRepresentations_renders_index_depth_and_blob_mappings()
    {
        var trace =
            new List<ExecutedStackInstruction>
            {
                new(
                    InstructionIndex: 8,
                    StackFrameDepth: 1,
                    InstructionPointer: 4,
                    EvaluationStackDepth: 0,
                    Instruction:
                    StackInstruction.Build_List_With_Prefix(
                        PineValue.List([StringEncoding.ValueFromString("Literal")]),
                        1),
                    FrameExpression: Expression.EnvironmentInstance,
                    LoadFrameInput: () => StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob)),
                new(
                    InstructionIndex: 12,
                    StackFrameDepth: 2,
                    InstructionPointer: 0,
                    EvaluationStackDepth: 0,
                    Instruction: StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(3)),
                    FrameExpression: Expression.EnvironmentInstance,
                    LoadFrameInput: () => StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
            };

        var rendered =
            StackInstructionTraceRenderer.RenderInstructionTraceWithDefaultBlobRepresentations(
                trace,
                maxBase16ByteCount: 32,
                maxUtf32StringCharCount: 32,
                renderInstructionIndex: true);

        rendered.Should().Be(
            """
             8. depth=1 ip=4 Build_List_With_Prefix (1, 1)
              Blob [28] (0x0000004c00000069000000740000006500000072000000610000006c | UTF32 "Literal")
            12. depth=2 ip=0 Push_Literal (Blob [2] (0x0403 | int 3))
            """);
    }

    [Fact]
    public void RenderStackFrameInstructions_renders_absolute_jump_destinations_and_incoming_locations()
    {
        var switchJumpTable =
            new Dictionary<PineValue, int>
            {
                [IntegerEncoding.EncodeSignedInteger(1)] = 2,
                [IntegerEncoding.EncodeSignedInteger(2)] = 3,
            }
            .ToImmutableDictionary();

        var frameInstructions =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([]),
                Instructions:
                [
                StackInstruction.Jump_Unconditional(6),
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(7)),
                StackInstruction.Jump_If_Equal(4, IntegerEncoding.EncodeSignedInteger(7)),
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(1)),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable: switchJumpTable),
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(0)),
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(1)),
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(2)),
                StackInstruction.Return,
                ]);

        StackInstructionTraceRenderer.RenderStackFrameInstructions(frameInstructions)
            .Should()
            .Be(
                """
                0: Jump_Const (6, 6)
                1: Push_Literal (Blob [2] (0x0407 | int 7))
                2: Jump_If_Equal_Const (Blob [2] (0x0407 | int 7), 4, 6)
                3: Push_Literal (Blob [2] (0x0401 | int 1))
                4: Switch_Jump_If_Equal_Const (2)
                  case Blob [2] (0x0401 | int 1): jump (2, 6)
                  case Blob [2] (0x0402 | int 2): jump (3, 7)
                5: Push_Literal (Blob [2] (0x0400 | int 0))
                jumps_arriving_from 3 (0, 2, 4)
                6: Push_Literal (Blob [2] (0x0401 | int 1))
                jumps_arriving_from 1 (4)
                7: Push_Literal (Blob [2] (0x0402 | int 2))
                8: Return
                """);
    }

    [Fact]
    public void Default_literal_rendering_appends_hash_only_for_non_simple_values()
    {
        var complexBlob = PineValue.Blob([255]);
        var listValue = PineValue.List([complexBlob]);

        var expectedBlobHash =
            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(complexBlob).Span)[..8];

        var expectedListHash =
            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(listValue).Span)[..8];

        var frameInstructions =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([]),
                Instructions:
                [
                StackInstruction.Push_Literal(complexBlob),
                StackInstruction.Push_Literal(listValue),
                StackInstruction.Push_Literal(StringEncoding.ValueFromString("text")),
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(3)),
                StackInstruction.Return,
                ]);

        StackInstructionTraceRenderer.RenderStackFrameInstructions(frameInstructions)
            .Should()
            .Be(
                $"""
                0: Push_Literal (Blob [1] (0xff | hash 0x{expectedBlobHash}))
                1: Push_Literal (List [1] (1 | hash 0x{expectedListHash}))
                2: Push_Literal (Blob [16] (0x00000074000000650000007800000074 | UTF32 "text"))
                3: Push_Literal (Blob [2] (0x0403 | int 3))
                4: Return
                """);
    }

    [Fact]
    public void BuildBlobRepresentationBase16_limits_rendered_bytes()
    {
        var trace =
            new[]
            {
                new ExecutedStackInstruction(
                    InstructionIndex: 1,
                    StackFrameDepth: 1,
                    InstructionPointer: 0,
                    EvaluationStackDepth: 0,
                    Instruction: StackInstruction.Push_Literal(PineValue.Blob([0, 1, 2, 3])),
                    FrameExpression: Expression.EnvironmentInstance,
                    LoadFrameInput: () => StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
            };

        var rendered =
            StackInstructionTraceRenderer.RenderInstructionTrace(
                trace,
                blobRepresentations:
                [
                StackInstructionTraceRenderer.BuildBlobRepresentationBase16(maxByteCount: 2)
                ]);

        rendered.Should().Be(
            "depth=1 ip=0 Push_Literal (Blob [4] (0x0001...))");
    }

    [Fact]
    public void BuildBlobRepresentationUtf32String_can_render_no_string_marker()
    {
        var blobRepresentation =
            StackInstructionTraceRenderer.BuildBlobRepresentationUtf32String(
                maxCharCount: 8,
                noStringRepresentation: "no UTF32 string");

        var rendered =
            StackInstructionTraceRenderer.RenderInstructionTrace(
                [
                new ExecutedStackInstruction(
                    InstructionIndex: 1,
                    StackFrameDepth: 1,
                    InstructionPointer: 0,
                    EvaluationStackDepth: 0,
                    Instruction: StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(3)),
                    FrameExpression: Expression.EnvironmentInstance,
                    LoadFrameInput: () => StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
                ],
                blobRepresentations: [blobRepresentation]);

        rendered.Should().Be(
            "depth=1 ip=0 Push_Literal (Blob [2] (no UTF32 string))");
    }

    [Fact]
    public void BuildBlobRepresentationStrictPineInteger_can_render_no_int_marker()
    {
        var blobRepresentation =
            StackInstructionTraceRenderer.BuildBlobRepresentationStrictPineInteger(
                noIntegerRepresentation: "no int");

        var rendered =
            StackInstructionTraceRenderer.RenderInstructionTrace(
                [
                new ExecutedStackInstruction(
                    InstructionIndex: 1,
                    StackFrameDepth: 1,
                    InstructionPointer: 0,
                    EvaluationStackDepth: 0,
                    Instruction: StackInstruction.Push_Literal(StringEncoding.ValueFromString("Literal")),
                    FrameExpression: Expression.EnvironmentInstance,
                    LoadFrameInput: () => StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
                ],
                blobRepresentations: [blobRepresentation]);

        rendered.Should().Be(
            "depth=1 ip=0 Push_Literal (Blob [28] (no int))");
    }

    [Fact]
    public void RenderInstructionTrace_allows_custom_blob_content_rendering()
    {
        var rendered =
            StackInstructionTraceRenderer.RenderInstructionTraceWithDefaultBlobRepresentations(
                [
                new ExecutedStackInstruction(
                    InstructionIndex: 8,
                    StackFrameDepth: 1,
                    InstructionPointer: 1,
                    EvaluationStackDepth: 0,
                    Instruction: StackInstruction.Push_Literal(StringEncoding.ValueFromString("Literal")),
                    FrameExpression: Expression.EnvironmentInstance,
                    LoadFrameInput: () => StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
                ],
                maxBase16ByteCount: 32,
                maxUtf32StringCharCount: 32,
                renderInstructionIndex: true,
                renderBlobContents:
                (blob, representations) =>
                "tag=" + string.Join(" ; ", representations.Where(text => !string.IsNullOrWhiteSpace(text))));

        rendered.Should().Be(
            """
            8. depth=1 ip=1 Push_Literal (Blob [28] (tag=0x0000004c00000069000000740000006500000072000000610000006c ; UTF32 "Literal"))
            """);
    }

    [Fact]
    public void RenderInstructionTrace_renders_invoke_stack_frame_const_description()
    {
        var linkedInstructions =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([[0]]),
                Instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Return,
                ],
                TrackEnvConstraint: null);

        var invokeInstruction =
            StackInstruction.Invoke_StackFrame_Const(
                expression: Expression.LitralInst(StringEncoding.ValueFromString("increment")),
                takeCount: 1);

        invokeInstruction.SetLinkedStackFrameInstructions(linkedInstructions);

        var rendered =
            StackInstructionTraceRenderer.RenderInstructionTrace(
                [
                new ExecutedStackInstruction(
                    InstructionIndex: 2,
                    StackFrameDepth: 1,
                    InstructionPointer: 1,
                    EvaluationStackDepth: 1,
                    Instruction: invokeInstruction,
                    FrameExpression: Expression.EnvironmentInstance,
                    LoadFrameInput: () => StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
                ]);

        rendered.Should().Be(
            "depth=1 ip=1 Invoke_StackFrame_Const (increment, 1)");
    }
}
