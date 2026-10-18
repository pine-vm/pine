using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class PineVMInstructionTraceTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Diagnostic_allocations_do_not_grow_with_instruction_count(
        bool reportInstructions,
        bool reportFrames)
    {
        const int longBodyPairs = 10_000;
        var shortExpression = Expression.EnvironmentInstance;
        var longExpression = Expression.LitralInst(PineValue.EmptyBlob);
        var environment = PineValue.Blob([42, 43]);
        var callbackCount = 0;
        var sameLoader = true;
        Func<StackFrameInput>? loader = null;

        void ObserveInstruction(in ExecutedStackInstruction item)
        {
            loader ??= item.LoadFrameInput;
            sameLoader &= ReferenceEquals(loader, item.LoadFrameInput);
            sameLoader &= ReferenceEquals(loader(), item.LoadFrameInput());
            ++callbackCount;
        }

        void ObserveFrame(in EnteredStackFrame item) => loader = item.LoadFrameInput;

        StackFrameInstructions Instructions(int pairs) =>
            new(
                StaticFunctionInterface.Generic,
                Enumerable.Range(0, pairs)
                .SelectMany(_ => new[] { StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop })
                .Concat([StackInstruction.Local_Get(0), StackInstruction.Return])
                .ToArray());

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
                reportExecutedStackInstruction: reportInstructions ? ObserveInstruction : null,
                expressionCompilationOverrides:
                new Dictionary<Expression, ExpressionCompilation>
                {
                    [shortExpression] = new(Instructions(1), []),
                    [longExpression] = new(Instructions(longBodyPairs), [])
                });

        var config = new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(null, null, null);

        EvaluationReport Evaluate(Expression expression)
        {
            loader = null;

            return
                vm.EvaluateExpressionOnCustomStack(
                    expression,
                    environment,
                    config,
                    reportEnteredStackFrame: reportFrames ? ObserveFrame : null)
                .Extract(error => throw new InvalidOperationException(error.ToString()));
        }

        _ = Evaluate(shortExpression);
        _ = Evaluate(longExpression);
        callbackCount = 0;
        sameLoader = true;

        var before = GC.GetAllocatedBytesForCurrentThread();
        var shortReport = Evaluate(shortExpression);
        var shortAllocated = GC.GetAllocatedBytesForCurrentThread() - before;
        before = GC.GetAllocatedBytesForCurrentThread();
        var longReport = Evaluate(longExpression);
        var longAllocated = GC.GetAllocatedBytesForCurrentThread() - before;

        longAllocated.Should().BeLessThanOrEqualTo(
            shortAllocated + 4096,
            "diagnostic closures and delegates must not be allocated for each executed instruction");

        longReport.ReturnValue.Evaluate().Should().Be(environment);
        shortReport.ReturnValue.Evaluate().Should().Be(environment);
        longReport.InstructionCount.Should().Be(2 * longBodyPairs + 2);

        callbackCount.Should().Be(
            reportInstructions ? (int)(shortReport.InstructionCount + longReport.InstructionCount) : 0);

        sameLoader.Should().BeTrue();
    }

    [Fact]
    public void Diagnostic_loader_preserves_lazy_input_and_record_copy_identity()
    {
        var lazyArgument = PineValueInProcess.CreateList([PineValueInProcess.Create(PineValue.Blob([42, 43]))]);
        var input = StackFrameInput.FromArguments(StaticFunctionInterface.Generic, [lazyArgument]);
        var loader = input.DiagnosticLoader;

        loader.Should().BeSameAs(input.DiagnosticLoader);
        loader().Should().BeSameAs(input);
        lazyArgument.EvaluatedOrNull.Should().BeNull("loading diagnostic input must not materialize its arguments");

        var lazyCopy = input with { };
        lazyCopy.DiagnosticLoader().Should().BeSameAs(lazyCopy);
        lazyArgument.EvaluatedOrNull.Should().BeNull();

        input = StackFrameInput.GenericFromEnvironmentValue(PineValue.Blob([42, 43]));
        var equalInput = StackFrameInput.GenericFromEnvironmentValue(PineValue.Blob([42, 43]));
        var hashBefore = input.GetHashCode();
        loader = input.DiagnosticLoader;

        loader.Should().BeSameAs(input.DiagnosticLoader);
        loader().Should().BeSameAs(input);
        input.Should().Be(equalInput);
        input.GetHashCode().Should().Be(hashBefore);

        var copy = input with { };
        copy.DiagnosticLoader().Should().BeSameAs(copy);
        copy.DiagnosticLoader.Should().NotBeSameAs(loader);
        copy.Should().Be(input);
    }

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
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst(
                    [
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(3)),
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(4))
                    ]));

        var result = vm.EvaluateExpression(expression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(7));

        trace.Should().NotBeEmpty();

        trace.Select(item => item.InstructionIndex).Should().BeInAscendingOrder();
        trace[0].InstructionIndex.Should().Be(0);
        trace[0].StackFrameDepth.Should().Be(1);
        trace[0].InstructionPointer.Should().Be(0);

        trace[0].Instruction.Should().Be(
            StackInstruction.Build_List_With_Prefix(
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(3),
                    IntegerEncoding.EncodeSignedInteger(4)
                    ]),
                0));

        var literalItems =
            trace[0].Instruction.Literal
            ?.ListItemsOrNull();

        literalItems.Should().NotBeNull();

        literalItems!
            .Select(item => item.IntegerOrNull)
            .Should()
            .Equal(new BigInteger(3), new BigInteger(4));

        trace[^1].Instruction.Kind.Should().Be(StackInstructionKind.Return);
    }

    [Fact]
    public void Build_List_With_Prefix_prepends_literal_items()
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

        var prefixItemOne = PineValue.Blob([1]);
        var prefixItemTwo = PineValue.Blob([2]);
        var environment = PineValue.Blob([3]);
        var suffixLiteral = PineValue.Blob([4]);

        var expression =
            Expression.ListInst(
                [
                Expression.LitralInst(prefixItemOne),
                Expression.LitralInst(prefixItemTwo),
                Expression.EnvironmentInstance,
                Expression.LitralInst(suffixLiteral)
                ]);

        var result = vm.EvaluateExpression(expression, environment);

        result.IsOkOrNull().Should().Be(
            PineValue.List([prefixItemOne, prefixItemTwo, environment, suffixLiteral]));

        trace.Select(item => item.Instruction).Should().Equal(
            StackInstruction.Local_Get(0),
            StackInstruction.Push_Literal(suffixLiteral),
            StackInstruction.Build_List_With_Prefix(
                PineValue.List([prefixItemOne, prefixItemTwo]),
                2),
            StackInstruction.Return);
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
                disableDirectEvalForSimpleTemplate: true,
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
            new Expression.Eval(
                encoded:
                Expression.LitralInst(
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                environment:
                Expression.LitralInst(nestedEnvironment));

        var result = vm.EvaluateExpression(expression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(nestedEnvironment);

        trace
            .Where(item => item.FrameExpression.Equals(expression))
            .Select(item => item.Instruction)
            .Should().Equal(
            StackInstruction.Push_Literal(nestedEnvironment),
            StackInstruction.Invoke_StackFrame_Const(
                Expression.EnvironmentInstance,
                ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                StaticFunctionInterface.Generic));

        var nestedFrameTrace =
            trace.First(item => item.FrameExpression.Equals(Expression.EnvironmentInstance));

        nestedFrameTrace.FrameExpression.Should().Be(Expression.EnvironmentInstance);
        nestedFrameTrace.LoadFrameInput().EvaluatedArguments.Should().Equal([nestedEnvironment]);
        nestedFrameTrace.Instruction.Should().Be(StackInstruction.Local_Get(0));

        var retainedInput = nestedFrameTrace.LoadFrameInput();
        var differentEnvironment = IntegerEncoding.EncodeSignedInteger(99);

        var differentExpression =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                Expression.LitralInst(differentEnvironment));

        vm.EvaluateExpression(differentExpression, PineValue.EmptyBlob).IsOkOrNull().Should().Be(differentEnvironment);
        nestedFrameTrace.LoadFrameInput().Should().BeSameAs(retainedInput);
        retainedInput.EvaluatedArguments.Should().Equal([nestedEnvironment]);
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
                disableDirectEvalForSimpleTemplate: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null);

        var nestedEnvironment = IntegerEncoding.EncodeSignedInteger(7);

        var expression =
            new Expression.Eval(
                encoded:
                Expression.LitralInst(
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                environment:
                Expression.LitralInst(nestedEnvironment));

        var evalResult =
            vm.EvaluateExpressionOnCustomStack(
                expression,
                PineValue.EmptyBlob,
                config:
                new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null),
                reportEnteredStackFrame:
                (in enteredStackFrame) =>
                enteredFrames.Add(enteredStackFrame));

        var result =
            evalResult
            .Extract(err => throw new System.Exception(EvaluationError.RenderDisplayString(err)));

        result.ReturnValue.Evaluate().Should().Be(nestedEnvironment);

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
        enteredFrames[1].LoadFrameInput().EvaluatedArguments.Should().Equal([nestedEnvironment]);
        enteredFrames[1].Instructions.Should().NotBeNull();
        enteredFrames[1].Instructions.Instructions.Should().NotBeEmpty();
    }
}
