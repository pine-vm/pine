using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class StackInstructionTraceRendererTests
{
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
                    Instruction: StackInstruction.Build_List_Tagged_Const(StringEncoding.ValueFromString("Literal"), 1),
                    FrameExpression: Expression.EnvironmentInstance,
                    FrameInput: StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob)),
                new(
                    InstructionIndex: 12,
                    StackFrameDepth: 2,
                    InstructionPointer: 0,
                    EvaluationStackDepth: 0,
                    Instruction: StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(3)),
                    FrameExpression: Expression.EnvironmentInstance,
                    FrameInput: StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
            };

        var rendered =
            StackInstructionTraceRenderer.RenderInstructionTraceWithDefaultBlobRepresentations(
                trace,
                maxBase16ByteCount: 32,
                maxUtf32StringCharCount: 32,
                renderInstructionIndex: true);

        rendered.Should().Be(
            """
             8. depth=1 ip=4 Build_List_Tagged_Const (Blob [28] (0x0000004c00000069000000740000006500000072000000610000006c | UTF32 "Literal") , 1)
            12. depth=2 ip=0 Push_Literal (Blob [2] (0x0403 | int 3))
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
                    FrameInput: StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
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
                    FrameInput: StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
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
                    FrameInput: StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
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
                    Instruction: StackInstruction.Build_List_Tagged_Const(StringEncoding.ValueFromString("Literal"), 1),
                    FrameExpression: Expression.EnvironmentInstance,
                    FrameInput: StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
                ],
                maxBase16ByteCount: 32,
                maxUtf32StringCharCount: 32,
                renderInstructionIndex: true,
                renderBlobContents:
                (blob, representations) =>
                "tag=" + string.Join(" ; ", representations.Where(text => !string.IsNullOrWhiteSpace(text))));

        rendered.Should().Be(
            """
            8. depth=1 ip=1 Build_List_Tagged_Const (Blob [28] (tag=0x0000004c00000069000000740000006500000072000000610000006c ; UTF32 "Literal") , 1)
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
                expression: Expression.LiteralInstance(StringEncoding.ValueFromString("increment")),
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
                    FrameInput: StackFrameInput.GenericFromEnvironmentValue(PineValue.EmptyBlob))
                ]);

        rendered.Should().Be(
            "depth=1 ip=1 Invoke_StackFrame_Const (increment , 1)");
    }
}
