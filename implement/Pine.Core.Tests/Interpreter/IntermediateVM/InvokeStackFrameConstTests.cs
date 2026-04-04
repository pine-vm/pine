using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class InvokeStackFrameConstTests
{
    [Fact]
    public void Invoke_stack_frame_const_supports_unary_recursive_call_shape()
    {
        var trace = new List<ExecutedStackInstruction>();
        var listLengthExpression = EnvironmentPathExpression([0]);
        var expectedReturnValue = IntegerEncoding.EncodeSignedInteger(3);

        var rootEnvironment =
            PineValue.List(
                [
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(3),
                    IntegerEncoding.EncodeSignedInteger(5),
                    IntegerEncoding.EncodeSignedInteger(8),
                    ]),
                ]);

        var listLengthInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: listLengthExpression,
                takeCount: 1);

        var listLengthInstructions =
            BuildForwardedArgumentsFrame(
                parameterCount: 1,
                instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Length_Equal_Const(0),
                StackInstruction.Jump_If_True(6),
                StackInstruction.Local_Get(0),
                StackInstruction.Skip_Const(1),
                listLengthInvoke,
                StackInstruction.Int_Add_Const(1),
                StackInstruction.Return,
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(0)),
                StackInstruction.Return,
                ]);

        listLengthInvoke.SetLinkedStackFrameInstructions(listLengthInstructions);

        EvaluateViaDirectInterpreter(
            targetExpression: listLengthExpression,
            forwardedEncodedExpression: LengthExpression(EnvironmentPathExpression([0])),
            rootEnvironment: rootEnvironment)
            .Should().Be(expectedReturnValue);

        var report =
            EvaluateNamedExpressionWithInjectedCompilation(
                trace,
                targetExpression: listLengthExpression,
                rootEnvironment: rootEnvironment,
                genericInstructions: listLengthInstructions);

        report.ReturnValue.Evaluate().Should().Be(expectedReturnValue);
        report.InstructionCount.Should().Be(32);
        report.InvocationCount.Should().Be(4);
        report.BuildListCount.Should().Be(0);
        report.LoopIterationCount.Should().Be(0);

        trace.Count(item => item.Instruction.Kind is StackInstructionKind.Invoke_StackFrame_Const)
            .Should().Be(3);
    }

    [Fact]
    public void Invoke_stack_frame_const_supports_binary_recursive_call_shape()
    {
        var trace = new List<ExecutedStackInstruction>();
        var incrementExpression = EnvironmentPathExpression([0]);
        var listMapExpression = EnvironmentPathExpression([0]);
        var incrementToken = StringEncoding.ValueFromString("increment");

        var expectedReturnValue =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(2),
                IntegerEncoding.EncodeSignedInteger(3),
                IntegerEncoding.EncodeSignedInteger(4),
                ]);

        var rootEnvironment =
            PineValue.List(
                [
                incrementToken,
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(1),
                    IntegerEncoding.EncodeSignedInteger(2),
                    IntegerEncoding.EncodeSignedInteger(3),
                    ]),
                ]);

        var incrementInstructions =
            BuildForwardedArgumentsFrame(
                parameterCount: 1,
                instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Add_Const(1),
                StackInstruction.Return,
                ]);

        var incrementInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: incrementExpression,
                takeCount: 1);

        incrementInvoke.SetLinkedStackFrameInstructions(incrementInstructions);

        var listMapInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: listMapExpression,
                takeCount: 2);

        var listMapInstructions =
            BuildForwardedArgumentsFrame(
                parameterCount: 2,
                instructions:
                [
                StackInstruction.Local_Get(1),
                StackInstruction.Length_Equal_Const(0),
                StackInstruction.Jump_If_True(10),
                StackInstruction.Local_Get(1),
                StackInstruction.Head_Generic,
                incrementInvoke,
                StackInstruction.Local_Get(0),
                StackInstruction.Local_Get(1),
                StackInstruction.Skip_Const(1),
                listMapInvoke,
                StackInstruction.Prepend_List_Items(1),
                StackInstruction.Return,
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return,
                ]);

        listMapInvoke.SetLinkedStackFrameInstructions(listMapInstructions);

        EvaluateViaDirectInterpreter(
            targetExpression: listMapExpression,
            forwardedEncodedExpression:
            Expression.ListInstance(
                [
                IncrementExpression(EnvironmentPathExpression([1, 0])),
                IncrementExpression(EnvironmentPathExpression([1, 1])),
                IncrementExpression(EnvironmentPathExpression([1, 2])),
                ]),
            rootEnvironment: rootEnvironment)
            .Should().Be(expectedReturnValue);

        var report =
            EvaluateNamedExpressionWithInjectedCompilation(
                trace,
                targetExpression: listMapExpression,
                rootEnvironment: rootEnvironment,
                genericInstructions: listMapInstructions);

        report.ReturnValue.Evaluate().Should().Be(expectedReturnValue);

        report.InstructionCount.Should().Be(53);
        report.InvocationCount.Should().Be(7);
        report.BuildListCount.Should().Be(0);
        report.LoopIterationCount.Should().Be(0);

        trace.Count(item => item.Instruction.Kind is StackInstructionKind.Invoke_StackFrame_Const)
            .Should().Be(6);
    }

    [Fact]
    public void Invoke_stack_frame_const_supports_recursive_factorial()
    {
        var trace = new List<ExecutedStackInstruction>();
        var factorialExpression = EnvironmentPathExpression([0]);

        var recursiveFactorialExpression =
            Expression.ConditionalInstance(
                condition:
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input:
                    Expression.ListInstance(
                        [
                        EnvironmentPathExpression([1]),
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                        ])),
                trueBranch:
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                falseBranch:
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_mul),
                    input:
                    Expression.ListInstance(
                        [
                        new Expression.ParseAndEval(
                            encoded: EnvironmentPathExpression([0]),
                            environment:
                            Expression.ListInstance(
                                [
                                EnvironmentPathExpression([0]),
                                Expression.KernelApplicationInstance(
                                    function: nameof(KernelFunction.int_add),
                                    input:
                                    Expression.ListInstance(
                                        [
                                        EnvironmentPathExpression([1]),
                                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(-1)),
                                        ])),
                                ])),
                        EnvironmentPathExpression([1]),
                        ])));

        var expectedReturnValue = IntegerEncoding.EncodeSignedInteger(120);

        var rootEnvironment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(recursiveFactorialExpression),
                IntegerEncoding.EncodeSignedInteger(5),
                ]);

        var factorialRecursiveInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: factorialExpression,
                takeCount: 1);

        var factorialInstructions =
            BuildForwardedArgumentsFrame(
                parameterCount: 1,
                instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Less_Than_Or_Equal_Const(1),
                StackInstruction.Jump_If_True(7),
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Add_Const(-1),
                factorialRecursiveInvoke,
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Mul_Binary,
                StackInstruction.Return,
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(1)),
                StackInstruction.Return,
                ]);

        factorialRecursiveInvoke.SetLinkedStackFrameInstructions(factorialInstructions);

        var factorialEntryInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: factorialExpression,
                takeCount: 1);

        factorialEntryInvoke.SetLinkedStackFrameInstructions(factorialInstructions);

        var factorialEntryInstructions =
            BuildEnvironmentValueFrame(
                parameterPaths: [[1]],
                instructions:
                [
                StackInstruction.Local_Get(0),
                factorialEntryInvoke,
                StackInstruction.Return,
                ]);

        EvaluateViaDirectInterpreter(
            targetExpression: factorialExpression,
            forwardedEncodedExpression: recursiveFactorialExpression,
            rootEnvironment: rootEnvironment)
            .Should().Be(expectedReturnValue);

        var report =
            EvaluateNamedExpressionWithInjectedCompilation(
                trace,
                targetExpression: factorialExpression,
                rootEnvironment: rootEnvironment,
                genericInstructions: factorialEntryInstructions);

        report.ReturnValue.Evaluate().Should().Be(expectedReturnValue);
        report.InstructionCount.Should().Be(46);
        report.InvocationCount.Should().Be(6);
        report.BuildListCount.Should().Be(0);
        report.LoopIterationCount.Should().Be(0);

        trace.Count(item => item.Instruction.Kind is StackInstructionKind.Invoke_StackFrame_Const)
            .Should().Be(5);
    }

    [Fact]
    public void Invoke_stack_frame_const_supports_recursive_fibonacci()
    {
        var trace = new List<ExecutedStackInstruction>();
        var fibonacciExpression = EnvironmentPathExpression([0]);

        var recursiveFibonacciExpression =
            Expression.ConditionalInstance(
                condition:
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_is_sorted_asc),
                    input:
                    Expression.ListInstance(
                        [
                        EnvironmentPathExpression([1]),
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                        ])),
                trueBranch:
                EnvironmentPathExpression([1]),
                falseBranch:
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.int_add),
                    input:
                    Expression.ListInstance(
                        [
                        new Expression.ParseAndEval(
                            encoded: EnvironmentPathExpression([0]),
                            environment:
                            Expression.ListInstance(
                                [
                                EnvironmentPathExpression([0]),
                                Expression.KernelApplicationInstance(
                                    function: nameof(KernelFunction.int_add),
                                    input:
                                    Expression.ListInstance(
                                        [
                                        EnvironmentPathExpression([1]),
                                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(-2)),
                                        ])),
                                ])),
                        new Expression.ParseAndEval(
                            encoded: EnvironmentPathExpression([0]),
                            environment:
                            Expression.ListInstance(
                                [
                                EnvironmentPathExpression([0]),
                                Expression.KernelApplicationInstance(
                                    function: nameof(KernelFunction.int_add),
                                    input:
                                    Expression.ListInstance(
                                        [
                                        EnvironmentPathExpression([1]),
                                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(-1)),
                                        ])),
                                ])),
                        ])));

        var expectedReturnValue = IntegerEncoding.EncodeSignedInteger(13);

        var rootEnvironment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(recursiveFibonacciExpression),
                IntegerEncoding.EncodeSignedInteger(7),
                ]);

        var fibonacciRecursiveInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: fibonacciExpression,
                takeCount: 1);

        var fibonacciInstructions =
            BuildForwardedArgumentsFrame(
                parameterCount: 1,
                instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Less_Than_Or_Equal_Const(1),
                StackInstruction.Jump_If_True(9),
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Add_Const(-2),
                fibonacciRecursiveInvoke,
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Add_Const(-1),
                fibonacciRecursiveInvoke,
                StackInstruction.Int_Add_Binary,
                StackInstruction.Return,
                StackInstruction.Local_Get(0),
                StackInstruction.Return,
                ]);

        fibonacciRecursiveInvoke.SetLinkedStackFrameInstructions(fibonacciInstructions);

        var fibonacciEntryInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: fibonacciExpression,
                takeCount: 1);

        fibonacciEntryInvoke.SetLinkedStackFrameInstructions(fibonacciInstructions);

        var fibonacciEntryInstructions =
            BuildEnvironmentValueFrame(
                parameterPaths: [[1]],
                instructions:
                [
                StackInstruction.Local_Get(0),
                fibonacciEntryInvoke,
                StackInstruction.Return,
                ]);

        EvaluateViaDirectInterpreter(
            targetExpression: fibonacciExpression,
            forwardedEncodedExpression: recursiveFibonacciExpression,
            rootEnvironment: rootEnvironment)
            .Should().Be(expectedReturnValue);

        var report =
            EvaluateNamedExpressionWithInjectedCompilation(
                trace,
                targetExpression: fibonacciExpression,
                rootEnvironment: rootEnvironment,
                genericInstructions: fibonacciEntryInstructions);

        report.ReturnValue.Evaluate().Should().Be(expectedReturnValue);
        report.InstructionCount.Should().Be(330);
        report.InvocationCount.Should().Be(42);
        report.BuildListCount.Should().Be(0);
        report.LoopIterationCount.Should().Be(0);

        trace.Count(item => item.Instruction.Kind is StackInstructionKind.Invoke_StackFrame_Const)
            .Should().Be(41);
    }

    [Fact]
    public void Invoke_stack_frame_const_supports_unary_direct_callee_shape()
    {
        var trace = new List<ExecutedStackInstruction>();
        var incrementExpression = EnvironmentPathExpression([0]);
        var directRootExpression = EnvironmentPathExpression([0]);
        var expectedReturnValue = IntegerEncoding.EncodeSignedInteger(42);

        var rootEnvironment =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(41),
                ]);

        var incrementInstructions =
            BuildForwardedArgumentsFrame(
                parameterCount: 1,
                instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Add_Const(1),
                StackInstruction.Return,
                ]);

        var incrementInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: incrementExpression,
                takeCount: 1);

        incrementInvoke.SetLinkedStackFrameInstructions(incrementInstructions);

        var rootInstructions =
            BuildForwardedArgumentsFrame(
                parameterCount: 1,
                instructions:
                [
                StackInstruction.Local_Get(0),
                incrementInvoke,
                StackInstruction.Return,
                ]);

        EvaluateViaDirectInterpreter(
            targetExpression: directRootExpression,
            forwardedEncodedExpression: IncrementExpression(EnvironmentPathExpression([0])),
            rootEnvironment: rootEnvironment)
            .Should().Be(expectedReturnValue);

        var report =
            EvaluateNamedExpressionWithInjectedCompilation(
                trace,
                targetExpression: directRootExpression,
                rootEnvironment: rootEnvironment,
                genericInstructions: rootInstructions);

        report.ReturnValue.Evaluate().Should().Be(expectedReturnValue);
        report.InstructionCount.Should().Be(8);
        report.InvocationCount.Should().Be(2);
        report.BuildListCount.Should().Be(0);
        report.LoopIterationCount.Should().Be(0);

        StackInstructionTraceRenderer.RenderInstructionTrace(trace)
            .Should().Contain("Invoke_StackFrame_Const");
    }

    [Fact]
    public void Invoke_stack_frame_const_enters_specialized_unary_callee_via_local_dispatch_without_root_input_override()
    {
        var trace = new List<ExecutedStackInstruction>();

        var incrementToken = StringEncoding.ValueFromString("increment");
        var incrementExpression = EnvironmentPathExpression([0]);
        var expectedReturnValue = IntegerEncoding.EncodeSignedInteger(42);

        var rootEnvironment =
            PineValue.List(
                [
                incrementToken,
                IntegerEncoding.EncodeSignedInteger(41),
                ]);

        var incrementEnvironmentClass =
            PineValueClass.Create(
                [new KeyValuePair<IReadOnlyList<int>, PineValue>([0], incrementToken)]);

        EvaluateViaDirectInterpreter(
            targetExpression: incrementExpression,
            forwardedEncodedExpression: IncrementExpression(EnvironmentPathExpression([1])),
            rootEnvironment: rootEnvironment)
            .Should().Be(expectedReturnValue);

        var report =
            EvaluateNamedExpressionWithInjectedCompilation(
                trace,
                targetExpression: incrementExpression,
                rootEnvironment: rootEnvironment,
                genericInstructions:
                BuildEnvironmentValueFrame(
                    parameterPaths: [[1]],
                    instructions:
                    [
                    StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(-1)),
                    StackInstruction.Return,
                    ]),
                specializedInstructions:
                [
                (constraint: [new EnvConstraintItem(new[] { 0 }, incrementToken)],
                instructions:
                        BuildEnvironmentValueFrame(
                    parameterPaths: [[1]],
                    instructions:
                    [
                    StackInstruction.Local_Get(0),
                    StackInstruction.Int_Add_Const(1),
                    StackInstruction.Return,
                    ],
                    trackEnvConstraint: incrementEnvironmentClass))
                ]);

        report.ReturnValue.Evaluate().Should().Be(expectedReturnValue);
        report.InstructionCount.Should().Be(6);
        report.InvocationCount.Should().Be(1);
        report.BuildListCount.Should().Be(0);
        report.LoopIterationCount.Should().Be(0);

        StackInstructionTraceRenderer.RenderInstructionTrace(trace)
            .Should().Contain("Parse_And_Eval_Binary");
    }

    [Fact]
    public void Invoke_stack_frame_const_enters_specialized_binary_recursive_target_via_local_dispatch_without_root_input_override()
    {
        var trace = new List<ExecutedStackInstruction>();
        var incrementExpression = EnvironmentPathExpression([0]);
        var listMapDispatchExpression = EnvironmentPathExpression([0]);

        var expectedReturnValue =
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(2),
                IntegerEncoding.EncodeSignedInteger(3),
                IntegerEncoding.EncodeSignedInteger(4),
                ]);

        var incrementInstructions =
            BuildForwardedArgumentsFrame(
                parameterCount: 1,
                instructions:
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Add_Const(1),
                StackInstruction.Return,
                ]);

        var incrementInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: incrementExpression,
                takeCount: 1);

        incrementInvoke.SetLinkedStackFrameInstructions(incrementInstructions);

        var incrementToken = StringEncoding.ValueFromString("increment");

        var mapEnvironmentClass =
            PineValueClass.Create(
                [new KeyValuePair<IReadOnlyList<int>, PineValue>([0], incrementToken)]);

        var listMapRecursiveInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: listMapDispatchExpression,
                takeCount: 2);

        var listMapRecursiveInstructions =
            BuildForwardedArgumentsFrame(
                parameterCount: 2,
                instructions:
                [
                StackInstruction.Local_Get(1),
                StackInstruction.Length_Equal_Const(0),
                StackInstruction.Jump_If_True(10),
                StackInstruction.Local_Get(1),
                StackInstruction.Head_Generic,
                incrementInvoke,
                StackInstruction.Local_Get(0),
                StackInstruction.Local_Get(1),
                StackInstruction.Skip_Const(1),
                listMapRecursiveInvoke,
                StackInstruction.Prepend_List_Items(1),
                StackInstruction.Return,
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return,
                ]);

        listMapRecursiveInvoke.SetLinkedStackFrameInstructions(listMapRecursiveInstructions);

        var specializedEntryInvoke =
            StackInstruction.Invoke_StackFrame_Const(
                expression: listMapDispatchExpression,
                takeCount: 2);

        specializedEntryInvoke.SetLinkedStackFrameInstructions(listMapRecursiveInstructions);

        var rootEnvironment =
            PineValue.List(
                [
                incrementToken,
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(1),
                    IntegerEncoding.EncodeSignedInteger(2),
                    IntegerEncoding.EncodeSignedInteger(3),
                    ]),
                ]);

        EvaluateViaDirectInterpreter(
            targetExpression: listMapDispatchExpression,
            forwardedEncodedExpression:
            Expression.ListInstance(
                [
                IncrementExpression(EnvironmentPathExpression([1, 0])),
                IncrementExpression(EnvironmentPathExpression([1, 1])),
                IncrementExpression(EnvironmentPathExpression([1, 2])),
                ]),
            rootEnvironment: rootEnvironment)
            .Should().Be(expectedReturnValue);

        var report =
            EvaluateNamedExpressionWithInjectedCompilation(
                trace,
                targetExpression: listMapDispatchExpression,
                rootEnvironment: rootEnvironment,
                genericInstructions:
                BuildEnvironmentValueFrame(
                    parameterPaths: [[0], [1]],
                    instructions:
                    [
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Return,
                    ]),
                specializedInstructions:
                [
                (constraint: [new EnvConstraintItem(new[] { 0 }, incrementToken)],
                instructions:
                    BuildEnvironmentValueFrame(
                    parameterPaths: [[0], [1]],
                    instructions:
                    [
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(1),
                    specializedEntryInvoke,
                    StackInstruction.Return,
                    ],
                    trackEnvConstraint: mapEnvironmentClass))
                ]);

        report.ReturnValue.Evaluate().Should().Be(expectedReturnValue);

        report.InstructionCount.Should().Be(56);
        report.InvocationCount.Should().Be(8);
        report.BuildListCount.Should().Be(0);
        report.LoopIterationCount.Should().Be(0);

        trace.Count(item => item.Instruction.Kind is StackInstructionKind.Invoke_StackFrame_Const)
            .Should().Be(7);
    }

    private static EvaluationReport EvaluateExpressionWithInjectedCompilation(
        List<ExecutedStackInstruction> trace,
        Expression rootExpression,
        PineValue rootEnvironment,
        IReadOnlyDictionary<Expression, ExpressionCompilation> expressionCompilationOverrides)
    {
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
                trace.Add(executedStackInstruction),
                expressionCompilationOverrides: expressionCompilationOverrides);

        return
            vm.EvaluateExpressionOnCustomStack(
                rootExpression: rootExpression,
                rootEnvironment: rootEnvironment,
                config: new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(ParseAndEvalCountLimit: null))
            .Extract(err => throw new InvalidOperationException("Failed eval: " + err));
    }

    private static EvaluationReport EvaluateNamedExpressionWithInjectedCompilation(
        List<ExecutedStackInstruction> trace,
        Expression targetExpression,
        PineValue rootEnvironment,
        StackFrameInstructions genericInstructions,
        IReadOnlyList<(IReadOnlyList<EnvConstraintItem> constraint, StackFrameInstructions instructions)>? specializedInstructions = null)
    {
        var rootExpression =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(
                    ExpressionEncoding.EncodeExpressionAsValue(targetExpression)),
                environment: Expression.EnvironmentInstance);

        return
            EvaluateExpressionWithInjectedCompilation(
                trace,
                rootExpression,
                rootEnvironment,
                new Dictionary<Expression, ExpressionCompilation>
                {
                    [targetExpression] =
                    new ExpressionCompilation(
                        Generic: genericInstructions,
                        Specialized: specializedInstructions ?? [])
                });
    }

    private static PineValue EvaluateViaDirectInterpreter(
        Expression targetExpression,
        Expression forwardedEncodedExpression,
        PineValue rootEnvironment)
    {
        var wrappedRootExpression =
            new Expression.ParseAndEval(
                encoded: targetExpression,
                environment: EnvironmentPathExpression([1]));

        var wrappedRootEnvironment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(forwardedEncodedExpression),
                rootEnvironment,
                ]);

        var directInterpreter = new DirectInterpreter(new PineVMParseCache(), evalCache: null);

        return
            directInterpreter.EvaluateExpressionDefault(
                wrappedRootExpression,
                wrappedRootEnvironment);
    }

    private static StackFrameInstructions BuildForwardedArgumentsFrame(
        int parameterCount,
        IReadOnlyList<StackInstruction> instructions) =>
        new(
            Parameters: ForwardedArgumentsInterface(parameterCount),
            Instructions: instructions,
            TrackEnvConstraint: null);

    private static StackFrameInstructions BuildEnvironmentValueFrame(
        IReadOnlyList<IReadOnlyList<int>> parameterPaths,
        IReadOnlyList<StackInstruction> instructions,
        PineValueClass? trackEnvConstraint = null) =>
        new(
            Parameters: StaticFunctionInterface.FromPathsSorted(parameterPaths),
            Instructions: instructions,
            TrackEnvConstraint: trackEnvConstraint);

    private static StaticFunctionInterface ForwardedArgumentsInterface(
        int parameterCount) =>
        StaticFunctionInterface.FromPathsSorted(
            [
            .. Enumerable.Range(0, parameterCount)
            .Select(index => (IReadOnlyList<int>)[index])
            ]);

    private static Expression EnvironmentPathExpression(
        ReadOnlySpan<int> path) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            path,
            Expression.EnvironmentInstance);

    private static Expression LengthExpression(
        Expression expression) =>
        Expression.KernelApplicationInstance(
            function: nameof(KernelFunction.length),
            input: expression);

    private static Expression IncrementExpression(
        Expression expression) =>
        Expression.KernelApplicationInstance(
            function: nameof(KernelFunction.int_add),
            input:
            Expression.ListInstance(
                [
                expression,
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                ]));
}
