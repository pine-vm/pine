using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class PineVMInstructionTraceTests
{
    [Fact]
    public void PineVM_reports_each_executed_stack_instruction()
    {
        var trace = new List<ExecutedStackInstruction>();

        var vm =
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
                reportExecutedStackInstruction:
                (in executedStackInstruction) =>
                trace.Add(executedStackInstruction));

        var expression =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.int_add),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(3)),
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(4))
                    ]));

        var result = vm.EvaluateExpression(expression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(7));

        trace.Should().NotBeEmpty();

        trace.Select(item => item.InstructionIndex).Should().BeInAscendingOrder();
        trace[0].InstructionIndex.Should().Be(0);
        trace[0].StackFrameDepth.Should().Be(1);
        trace[0].InstructionPointer.Should().Be(0);
        trace[0].Instruction.Should().Be(StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(3)));
        trace[^1].Instruction.Kind.Should().Be(StackInstructionKind.Return);
    }

    [Fact]
    public void PineVM_instruction_trace_reports_nested_frame_input_arguments()
    {
        var trace = new List<ExecutedStackInstruction>();

        var vm =
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
                reportExecutedStackInstruction:
                (in executedStackInstruction) =>
                trace.Add(executedStackInstruction));

        var nestedEnvironment = IntegerEncoding.EncodeSignedInteger(7);

        var expression =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                environment:
                Expression.LiteralInstance(nestedEnvironment));

        var result = vm.EvaluateExpression(expression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(nestedEnvironment);

        var nestedFrameTrace =
            trace.First(item => item.FrameExpression.Equals(Expression.EnvironmentInstance));

        nestedFrameTrace.FrameExpression.Should().Be(Expression.EnvironmentInstance);
        nestedFrameTrace.FrameInput.EvaluatedArguments.Should().Equal([nestedEnvironment]);
        nestedFrameTrace.Instruction.Should().Be(StackInstruction.Local_Get(0));
    }

    [Fact]
    public void PineVM_reports_each_entered_stack_frame()
    {
        var enteredFrames = new List<EnteredStackFrame>();

        var vm =
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
                reportEnteredStackFrame:
                (in enteredStackFrame) =>
                enteredFrames.Add(enteredStackFrame));

        var nestedEnvironment = IntegerEncoding.EncodeSignedInteger(7);

        var expression =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                environment:
                Expression.LiteralInstance(nestedEnvironment));

        var result = vm.EvaluateExpression(expression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(nestedEnvironment);

        // The root frame is replaced by the nested ParseAndEval frame (tail position),
        // so we see 2 frame entries total.
        enteredFrames.Should().HaveCount(2);

        enteredFrames[0].FrameIndex.Should().Be(0);
        enteredFrames[0].StackFrameDepth.Should().Be(1);

        // The nested frame replaces the root frame (tail call optimization),
        // so stack depth stays at 1.
        enteredFrames[1].FrameIndex.Should().Be(1);
        enteredFrames[1].StackFrameDepth.Should().Be(1);
        enteredFrames[1].FrameExpression.Should().Be(Expression.EnvironmentInstance);
        enteredFrames[1].FrameInput.EvaluatedArguments.Should().Equal([nestedEnvironment]);
        enteredFrames[1].Instructions.Should().NotBeNull();
        enteredFrames[1].Instructions.Instructions.Should().NotBeEmpty();
    }
}
