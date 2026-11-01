using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Threading;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class IntegerDivisionTests
{
    [Theory]
    [InlineData(7)]
    [InlineData(-7)]
    [InlineData(1)]
    [InlineData(-1)]
    public void Integer_division_compiles_inlined_constant_divisor(int divisor)
    {
        var expression =
            InlinedDivisionExpression(
                Expression.EnvironmentInstance,
                Expression.LitralInst(Integer(divisor)));

        var frame = Compile(expression, reduce: true);

        frame.Instructions.Select(instruction => instruction.Kind).Should().Contain(StackInstructionKind.Int_Div_Const);
        Execute(frame, Integer(22)).Should().Be(Integer(22 / divisor));
        Execute(frame, Integer(-22)).Should().Be(Integer(-22 / divisor));
        AssertPersistence(frame);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Integer_division_compiles_binary_with_literal_zero_result(bool reduce)
    {
        var expression = CoreBasics.Int_div(Argument(0), Argument(1));
        var frame = Compile(expression, reduce);

        frame.Instructions.Should().Contain(
            instruction =>
            instruction.Kind == StackInstructionKind.Int_Div_Binary &&
            instruction.Literal!.Evaluate() == Integer(0) &&
            instruction.JumpOffset == null);

        foreach (var (dividend, divisor) in new (BigInteger, BigInteger)[]
        {
            (21, 7), (22, 7), (-22, 7), (22, -7), (-22, -7),
            (3, 7), (-3, 7), (0, 7), (22, 0),
            (BigInteger.One << 300, 257), (-(BigInteger.One << 300), -257)
        })
        {
            Execute(frame, PineValue.List([Integer(dividend), Integer(divisor)]))
                .Should().Be(Integer(divisor.IsZero ? 0 : dividend / divisor));
        }

        AssertPersistence(frame);
    }

    [Theory]
    [InlineData(false, 7)]
    [InlineData(true, 7)]
    [InlineData(false, -7)]
    [InlineData(true, -7)]
    [InlineData(true, 1)]
    [InlineData(true, -1)]
    public void Integer_division_compiles_nonzero_constant_divisor(bool reduce, int divisor)
    {
        var expression = CoreBasics.Int_div(Expression.EnvironmentInstance, Expression.LitralInst(Integer(divisor)));
        var frame = Compile(expression, reduce);

        frame.Instructions.Should().Contain(StackInstruction.Int_Div_Const(divisor));
        frame.Instructions.Should().NotContain(instruction => instruction.Kind == StackInstructionKind.Int_Div_Binary);

        foreach (var dividend in new BigInteger[] { 0, 3, 22, -22, BigInteger.One << 300 })
        {
            Execute(frame, Integer(dividend)).Should().Be(Integer(dividend / divisor));
        }

        AssertPersistence(frame);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Integer_division_compiles_constant_zero_divisor_without_dividing(bool reduce)
    {
        var expression = CoreBasics.Int_div(Expression.EnvironmentInstance, Expression.LitralInst(Integer(0)));
        var frame = Compile(expression, reduce);

        frame.Instructions.Should().NotContain(
            instruction => instruction.Kind == StackInstructionKind.Int_Div_Const ||
                instruction.Kind == StackInstructionKind.Int_Div_Binary);

        Execute(frame, Integer(22)).Should().Be(Integer(0));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Integer_division_compiles_custom_zero_handler_as_jump(bool reduce)
    {
        var division = InlinedDivision(Argument(0), Argument(1));

        var handler =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst([Argument(0), Expression.LitralInst(Integer(100))]));

        var expression =
            Expression.ConditionalInst(
                condition: division.Condition,
                falseBranch: division.FalseBranch,
                trueBranch: handler);

        var frame = Compile(expression, reduce);

        var instruction =
            frame.Instructions.Should().ContainSingle(
                instruction => instruction.Kind == StackInstructionKind.Int_Div_Binary).Which;

        instruction.Literal.Should().BeNull();
        instruction.JumpOffset.Should().NotBeNull();

        Execute(frame, PineValue.List([Integer(22), Integer(7)])).Should().Be(Integer(3));
        Execute(frame, PineValue.List([Integer(22), Integer(0)])).Should().Be(Integer(122));
        Execute(frame, PineValue.List([Integer(-22), Integer(0)])).Should().Be(Integer(78));

        var zeroTarget = frame.Instructions.ToList().IndexOf(instruction) + instruction.JumpOffset!.Value;
        zeroTarget.Should().BeInRange(0, frame.Instructions.Count - 1);
        AssertPersistence(frame);
    }

    [Fact]
    public void Integer_division_compiles_arbitrary_literal_zero_result()
    {
        var division = InlinedDivision(Argument(0), Argument(1));
        var defaultValue = PineValue.List([PineValue.EmptyBlob, Integer(123)]);

        var expression =
            Expression.ConditionalInst(
                condition: division.Condition,
                falseBranch: division.FalseBranch,
                trueBranch: Expression.LitralInst(defaultValue));

        var frame = Compile(expression, reduce: true);

        frame.Instructions.Should().Contain(StackInstruction.Int_Div_Binary(defaultValue));
        Execute(frame, PineValue.List([Integer(22), Integer(0)])).Should().Be(defaultValue);
        Execute(frame, PineValue.List([Integer(22), Integer(7)])).Should().Be(Integer(3));
    }

    [Fact]
    public void Integer_division_instructions_handle_large_constants_and_invalid_operands()
    {
        var divisor = (BigInteger.One << 200) + 7;
        var dividend = divisor * ((BigInteger.One << 100) + 3) + 1;

        var constantFrame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [StackInstruction.Local_Get(0), StackInstruction.Int_Div_Const(divisor), StackInstruction.Return]);

        Execute(constantFrame, Integer(dividend)).Should().Be(Integer(dividend / divisor));
        Execute(constantFrame, PineValue.EmptyList).Should().Be(Integer(0));
        Execute(constantFrame, PineValue.EmptyBlob).Should().Be(Integer(0));
        AssertPersistence(constantFrame);

        foreach (var operands in new[]
        {
            PineValue.List([PineValue.EmptyList, Integer(7)]),
            PineValue.List([Integer(22), PineValue.EmptyBlob]),
            PineValue.List([PineValue.EmptyList, PineValue.EmptyList])
        })
        {
            var frame = Compile(CoreBasics.Int_div(Argument(0), Argument(1)), reduce: false);
            Execute(frame, operands).Should().Be(Integer(0));
        }
    }

    [Fact]
    public void Integer_division_preserves_reference_results_for_non_integer_operands()
    {
        var expression = CoreBasics.Int_div(Argument(0), Argument(1));
        var interpreter = DirectInterpreter.WithLocalEvalCache(new PineVMParseCache());
        var frame = Compile(expression, reduce: true);

        foreach (var operands in new[]
        {
            PineValue.List([PineValue.EmptyList, Integer(7)]),
            PineValue.List([Integer(22), PineValue.EmptyBlob]),
            PineValue.List([PineValue.EmptyList, PineValue.EmptyList]),
            PineValue.List([PineValue.EmptyList, Integer(0)])
        })
        {
            Execute(frame, operands).Should().Be(interpreter.EvaluateExpressionDefault(expression, operands));
        }
    }

    [Fact]
    public void Integer_division_custom_handler_survives_static_invocation_inlining()
    {
        var division = InlinedDivision(Argument(0), Argument(1));

        var withHandler =
            Expression.ConditionalInst(
                condition: division.Condition,
                falseBranch: division.FalseBranch,
                trueBranch: Argument(0));

        var expression =
            Expression.ListInst(
                [
                Expression.LitralInst(Integer(99)),
                new Expression.Eval(
                    Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(withHandler)),
                    Expression.EnvironmentInstance),
                Expression.LitralInst(Integer(100))
                ]);

        var frame = Compile(expression, reduce: false);

        frame.Instructions.Select(instruction => instruction.Kind).Should().Contain(StackInstructionKind.Int_Div_Binary);

        Execute(frame, PineValue.List([Integer(22), Integer(7)]))
            .Should().Be(PineValue.List([Integer(99), Integer(3), Integer(100)]));

        Execute(frame, PineValue.List([Integer(22), Integer(0)]))
            .Should().Be(PineValue.List([Integer(99), Integer(22), Integer(100)]));

        AssertPersistence(frame);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Integer_division_by_constant_zero_preserves_argument_evaluation_errors(bool reduce)
    {
        var dividend =
            new Expression.Eval(
                Expression.LitralInst(PineValue.EmptyList),
                Expression.EnvironmentInstance);

        var expression = CoreBasics.Int_div(dividend, Expression.LitralInst(Integer(0)));
        var frame = Compile(expression, reduce);

        var result =
            CreateVM(frame).EvaluateExpressionOnCustomStack(
                Expression.EnvironmentInstance,
                PineValue.EmptyList,
                Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Default);

        result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>();
    }

    [Fact]
    public void Integer_division_does_not_eagerly_compile_a_lazy_dividend()
    {
        var dividend =
            new Expression.Eval(
                Expression.LitralInst(PineValue.EmptyList),
                Expression.EnvironmentInstance);

        var division = InlinedDivision(dividend, Argument(1));
        var frame = Compile(division, reduce: false);

        frame.Instructions.Select(instruction => instruction.Kind).Should().NotContain(
            StackInstructionKind.Int_Div_Binary);

        Execute(frame, PineValue.List([Integer(22), Integer(0)])).Should().Be(Integer(0));
    }

    [Fact]
    public void Integer_division_backward_zero_jump_observes_loop_quota()
    {
        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Push_Literal(Integer(0)),
                StackInstruction.Int_Div_Binary(jumpOffset: -2),
                StackInstruction.Return
                ]);

        var result =
            CreateVM(frame).EvaluateExpressionOnCustomStack(
                Expression.EnvironmentInstance,
                Integer(22),
                Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Default with { LoopIterationCountLimit = 2 });

        result.IsErrOrNull()!.Reason.Should().Be(
            new EvaluationErrorReason.QuotaExhausted(EvaluationQuotaKind.LoopIterationCount, 2));
    }

    [Fact]
    public void Integer_division_zero_jump_observes_cancellation_at_the_instruction()
    {
        using var cancellation = new CancellationTokenSource();

        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Push_Literal(Integer(0)),
                StackInstruction.Int_Div_Binary(jumpOffset: 2),
                StackInstruction.Return,
                StackInstruction.Push_Literal(Integer(100)),
                StackInstruction.Return
                ]);

        void Observe(in ExecutedStackInstruction executed)
        {
            if (executed.Instruction.Kind == StackInstructionKind.Int_Div_Binary)
            {
                cancellation.Cancel();
            }
        }

        var result =
            CreateVM(frame, Observe).EvaluateExpressionOnCustomStack(
                Expression.EnvironmentInstance,
                Integer(22),
                Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Default,
                cancellation.Token);

        result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.CancellationRequested>();
    }

    [Fact]
    public void Integer_division_zero_jump_preserves_surrounding_stack_and_accounts_for_handler_depth()
    {
        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Push_Literal(Integer(99)),
                StackInstruction.Push_Literal(Integer(22)),
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Div_Binary(jumpOffset: 3),
                StackInstruction.Build_List(2),
                StackInstruction.Return,
                StackInstruction.Push_Literal(Integer(100)),
                StackInstruction.Push_Literal(Integer(200)),
                StackInstruction.Push_Literal(Integer(300)),
                StackInstruction.Build_List(3),
                StackInstruction.Build_List(2),
                StackInstruction.Return
                ]);

        frame.MaxStackUsage.Should().Be(4);
        Execute(frame, Integer(7)).Should().Be(PineValue.List([Integer(99), Integer(3)]));

        Execute(frame, Integer(0)).Should().Be(
            PineValue.List([Integer(99), PineValue.List([Integer(100), Integer(200), Integer(300)])]));

        Execute(frame, PineValue.EmptyList).Should().Be(PineValue.List([Integer(99), Integer(0)]));
        AssertPersistence(frame);
    }

    [Fact]
    public void Integer_division_rejects_invalid_instruction_forms_and_inconsistent_stack_edges()
    {
        foreach (var instruction in new[]
        {
            new StackInstruction(StackInstructionKind.Int_Div_Binary),
            StackInstruction.Int_Div_Binary(PineValue.EmptyList) with { JumpOffset = 2 },
            new StackInstruction(StackInstructionKind.Int_Div_Const),
            new StackInstruction(StackInstructionKind.Int_Div_Const, IntegerLiteral: 0)
        })
        {
            Action createFrame =
                () => _ =
                    new StackFrameInstructions(
                        StaticFunctionInterface.Generic,
                        [StackInstruction.Local_Get([0, 0]), instruction, StackInstruction.Return]);

            createFrame.Should().Throw<InvalidOperationException>();
        }

        Action createZeroConstant = () => StackInstruction.Int_Div_Const(0);
        createZeroConstant.Should().Throw<ArgumentOutOfRangeException>();

        Action createSameSuccessor =
            () => _ =
                new StackFrameInstructions(
                    StaticFunctionInterface.Generic,
                    [
                    StackInstruction.Local_Get([0, 0]),
                    StackInstruction.Int_Div_Binary(jumpOffset: 1),
                    StackInstruction.Return
                    ]);

        createSameSuccessor.Should().Throw<InvalidOperationException>();

        Action appendJumpOperation =
            () => PineControlFlowFragment.FromOperations([StackInstruction.Int_Div_Binary(jumpOffset: 2)]);

        appendJumpOperation.Should().Throw<ArgumentException>();
    }

    private static PineValue Integer(BigInteger value) => IntegerEncoding.EncodeSignedInteger(value);

    private static Expression Argument(int index) =>
        ExpressionBuilder.BuildExpressionForPathInExpression([index], Expression.EnvironmentInstance);

    private static Expression.Conditional InlinedDivision(Expression dividend, Expression divisor) =>
        InlinedDivisionExpression(dividend, divisor).Should().BeOfType<Expression.Conditional>().Subject;

    private static Expression InlinedDivisionExpression(Expression dividend, Expression divisor)
    {
        var parseCache = new PineVMParseCache();

        var plan =
            ExpressionTemplatePlan.TryParseTemplate(CoreBasics.Int_div_FunctionValue(), parseCache)
            ?? throw new InvalidOperationException("Integer division must be a curried function template.");

        return
            ReducePineExpression.ReduceExpressionBottomUp(
                ExpressionSubstituteEnvironment.SubstituteEnvironmentNode(
                    plan.TerminalExpression,
                    Expression.ListInst([Expression.EmptyList, dividend, divisor])),
                ReductionConfig.Default,
                parseCache);
    }

    private static StackFrameInstructions Compile(Expression expression, bool reduce) =>
        ExpressionCompilation.CompileExpression(
            expression,
            specializations: [],
            new PineVMParseCache(),
            disableReduction: !reduce,
            enableTailRecursionOptimization: false,
            skipInlining: (_, _) => false)
        .Generic;

    private static PineValue Execute(StackFrameInstructions frame, PineValue environment)
    {
        return
            CreateVM(frame).EvaluateExpressionOnCustomStack(
                Expression.EnvironmentInstance,
                environment,
                Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Default)
            .Extract(error => throw new InvalidOperationException(error.ToString()))
            .ReturnValue.Evaluate();
    }

    private static Core.Runtime.IntermediateVM.PineVM CreateVM(
        StackFrameInstructions frame,
        ReportExecutedStackInstruction? observeInstruction = null) =>
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
            reportExecutedStackInstruction: observeInstruction,
            expressionCompilationOverrides:
            new Dictionary<Expression, ExpressionCompilation>
            {
                [Expression.EnvironmentInstance] = new(frame, Specialized: [])
            });

    private static void AssertPersistence(StackFrameInstructions frame)
    {
        var rendered =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                frame,
                StackInstructionTraceRenderer.RenderingOptions.Persistence);

        var restored =
            StackInstructionTextFormat.ParseStackFrameInstructions(
                rendered.Text,
                hash => rendered.ObservedValues[hash]);

        restored.Instructions.Should().Equal(frame.Instructions);
        restored.MaxStackUsage.Should().Be(frame.MaxStackUsage);
        StackInstructionTraceRenderer.RenderStackFrameInstructions(restored).Should().Contain("Int_Div_");
    }
}
