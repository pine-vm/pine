using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;

using IntermediatePineVM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class PartialApplicationRuntimeTests
{
    [Fact]
    public void Under_application_matches_direct_interpreter_without_materializing_intermediate_value()
    {
        var functionValue = BuildFunctionValue(parameterCount: 3);
        var argument = IntegerEncoding.EncodeSignedInteger(11);

        var expression =
            new Expression.Eval(
                Expression.LitralInst(functionValue),
                Expression.LitralInst(argument));

        var report = EvaluateWithPineVm(expression);
        var expected = EvaluateWithDirectInterpreter(expression);

        report.ReturnValue.PartialApplicationOrNull.Should().NotBeNull();
        report.ReturnValue.EvaluatedOrNull.Should().BeNull();
        report.Counters.CurriedFunctionPlanParseCount.Should().Be(1);
        report.Counters.PartialApplicationAllocationCount.Should().Be(1);
        report.Counters.DirectSaturatedApplicationCount.Should().Be(0);
        report.Counters.PartialApplicationMaterializationCount.Should().Be(0);
        report.ReturnValue.Evaluate().Should().Be(expected);
    }

    [Fact]
    public void Saturated_application_matches_direct_interpreter()
    {
        var functionValue = BuildFunctionValue(parameterCount: 3);

        var expression =
            Apply(
                functionValue,
                IntegerEncoding.EncodeSignedInteger(11),
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17));

        var report = EvaluateWithPineVm(expression);
        var expected = EvaluateWithDirectInterpreter(expression);

        report.ReturnValue.PartialApplicationOrNull.Should().BeNull();
        report.Counters.CurriedFunctionPlanParseCount.Should().Be(1);
        report.Counters.PartialApplicationAllocationCount.Should().Be(0);
        report.Counters.DirectSaturatedApplicationCount.Should().Be(1);
        report.Counters.PartialApplicationMaterializationCount.Should().Be(0);
        report.ReturnValue.Evaluate().Should().Be(expected);
    }

    [Fact]
    public void Partial_application_after_multiple_arguments_matches_direct_interpreter()
    {
        var functionValue = BuildFunctionValue(parameterCount: 4);

        var expression =
            Apply(
                functionValue,
                IntegerEncoding.EncodeSignedInteger(11),
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17));

        var report = EvaluateWithPineVm(expression);
        var expected = EvaluateWithDirectInterpreter(expression);

        report.ReturnValue.PartialApplicationOrNull.Should().NotBeNull();
        report.ReturnValue.EvaluatedOrNull.Should().BeNull();
        report.Counters.PartialApplicationAllocationCount.Should().Be(1);
        report.Counters.DirectSaturatedApplicationCount.Should().Be(0);
        report.Counters.PartialApplicationMaterializationCount.Should().Be(0);
        report.ReturnValue.Evaluate().Should().Be(expected);
    }

    [Fact]
    public void Applying_materialized_partial_value_matches_direct_interpreter()
    {
        var functionValue = BuildFunctionValue(parameterCount: 3);
        var firstArgument = IntegerEncoding.EncodeSignedInteger(11);

        var partialExpression = Apply(functionValue, firstArgument);
        var partialValue = EvaluateWithDirectInterpreter(partialExpression);

        var remainingExpression =
            Apply(
                partialValue,
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17));

        EvaluateWithPineVm(remainingExpression).ReturnValue.Evaluate()
            .Should().Be(EvaluateWithDirectInterpreter(remainingExpression));
    }

    [Fact]
    public void Structural_inspection_reports_partial_application_materialization()
    {
        var partialExpression =
            Apply(
                BuildFunctionValue(parameterCount: 3),
                IntegerEncoding.EncodeSignedInteger(11));

        var expression =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.length),
                partialExpression);

        var report = EvaluateWithPineVm(expression);

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        report.Counters.PartialApplicationAllocationCount.Should().Be(1);
        report.Counters.PartialApplicationMaterializationCount.Should().Be(1);
        report.Counters.DirectSaturatedApplicationCount.Should().Be(0);
    }

    [Fact]
    public void Nested_eval_expression_executes_fused_eval_var_instruction()
    {
        var expression =
            Apply(
                BuildFunctionValue(parameterCount: 3),
                IntegerEncoding.EncodeSignedInteger(11),
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17));

        var executedInstructions = new List<StackInstructionKind>();
        var report = EvaluateWithPineVm(expression, executedInstructions);

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        executedInstructions.Should().Contain(StackInstructionKind.Eval_Multi);
        report.Counters.PartialApplicationAllocationCount.Should().Be(0);
        report.Counters.DirectSaturatedApplicationCount.Should().Be(1);
    }

    [Fact]
    public void Fused_application_falls_back_for_noncanonical_function_value()
    {
        var identityFunction =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.EnvironmentInstance);

        var constantFunction =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)));

        var expression =
            Apply(
                Expression.EnvironmentInstance,
                constantFunction,
                IntegerEncoding.EncodeSignedInteger(43));

        var executedInstructions = new List<StackInstructionKind>();

        var report =
            EvaluateWithPineVm(
                expression,
                executedInstructions,
                environment: identityFunction);

        report.ReturnValue.Evaluate()
            .Should().Be(EvaluateWithDirectInterpreter(expression, identityFunction));

        executedInstructions.Should().Contain(StackInstructionKind.Eval_Multi);
        report.Counters.DirectSaturatedApplicationCount.Should().Be(0);
    }

    [Fact]
    public void Tail_position_fallback_does_not_retain_the_parent_frame()
    {
        var identityFunction =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.EnvironmentInstance);

        var constantFunction =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)));

        var expression =
            Apply(
                Expression.EnvironmentInstance,
                constantFunction,
                IntegerEncoding.EncodeSignedInteger(43));

        var result =
            EvaluateResultWithPineVm(
                expression,
                environment: identityFunction,
                config:
                new IntermediatePineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: 2));

        result.IsOkOrNull()!.ReturnValue.Evaluate()
            .Should().Be(EvaluateWithDirectInterpreter(expression, identityFunction));
    }

    [Fact]
    public void Fused_over_application_matches_direct_interpreter()
    {
        var innerFunction =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [1],
                    Expression.EnvironmentInstance),
                parameterCount: 1)
            ??
            throw new System.InvalidOperationException("Failed to build inner function value.");

        var outerFunction =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                Expression.LitralInst(innerFunction),
                parameterCount: 1)
            ??
            throw new System.InvalidOperationException("Failed to build outer function value.");

        var expression =
            Apply(
                outerFunction,
                IntegerEncoding.EncodeSignedInteger(41),
                IntegerEncoding.EncodeSignedInteger(43));

        var report = EvaluateWithPineVm(expression);

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        report.Counters.PartialApplicationAllocationCount.Should().Be(0);
        report.Counters.DirectSaturatedApplicationCount.Should().Be(1);
    }

    [Fact]
    public void Fused_application_preserves_structured_parse_error()
    {
        var expression =
            Apply(
                PineValue.EmptyBlob,
                IntegerEncoding.EncodeSignedInteger(41),
                IntegerEncoding.EncodeSignedInteger(43));

        var result = EvaluateResultWithPineVm(expression);
        var error = result.IsErrOrNull();

        error.Should().NotBeNull();
        error!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>();

        var parseError = (EvaluationErrorReason.ParseExpressionFailed)error.Reason;
        parseError.ExpressionValue.Should().Be(PineValue.EmptyBlob);
        parseError.EnvironmentValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(41));
    }

    [Fact]
    public void Fused_fallback_reports_parse_error_before_later_invocation_quota()
    {
        var expression =
            Apply(
                Expression.EnvironmentInstance,
                IntegerEncoding.EncodeSignedInteger(41),
                IntegerEncoding.EncodeSignedInteger(43));

        var result =
            EvaluateResultWithPineVm(
                expression,
                environment: PineValue.EmptyBlob,
                config:
                new IntermediatePineVM.EvaluationConfig(
                    InvocationCountLimit: 1,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null));

        var error = result.IsErrOrNull();

        error.Should().NotBeNull();
        error!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>();
        error.Counters.InvocationCount.Should().Be(1);
    }

    [Fact]
    public void Fused_application_preserves_argument_evaluation_order()
    {
        var innerInvalidExpression = StringEncoding.ValueFromString("inner-invalid");
        var outerInvalidExpression = StringEncoding.ValueFromString("outer-invalid");

        var innerArgument =
            new Expression.Eval(
                Expression.LitralInst(innerInvalidExpression),
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)));

        var outerArgument =
            new Expression.Eval(
                Expression.LitralInst(outerInvalidExpression),
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(43)));

        var expression =
            new Expression.Eval(
                new Expression.Eval(
                    Expression.LitralInst(PineValue.EmptyList),
                    innerArgument),
                outerArgument);

        var vmError = EvaluateResultWithPineVm(expression).IsErrOrNull();

        vmError.Should().NotBeNull();

        var parseError =
            vmError!.Reason.Should()
            .BeOfType<EvaluationErrorReason.ParseExpressionFailed>()
            .Subject;

        parseError.ExpressionValue.Should().Be(outerInvalidExpression);

        var directAction =
            () => EvaluateWithDirectInterpreter(expression);

        directAction.Should()
            .Throw<ParseExpressionException>()
            .WithMessage("*outer-invalid*");
    }

    [Fact]
    public void Default_counter_format_omits_PAP_diagnostics()
    {
        var counters =
            new PerformanceCounters(
                InvocationCount: 1,
                BuildListCount: 2,
                LoopIterationCount: 3,
                InstructionCount: 4,
                CurriedFunctionPlanParseCount: 5,
                PartialApplicationAllocationCount: 6,
                DirectSaturatedApplicationCount: 7,
                PartialApplicationMaterializationCount: 8);

        PerformanceCountersFormatting.FormatCounts(counters).Should().Be(
            """
            InvocationCount: 1
            BuildListCount: 2
            LoopIterationCount: 3
            InstructionCount: 4
            """);

        PerformanceCountersFormatting.FormatAllCounts(counters).Should().Contain(
            "PartialApplicationAllocationCount: 6");
    }

    private static PineValue BuildFunctionValue(int parameterCount)
    {
        var bodyItems = new Expression[parameterCount];

        for (var i = 0; i < parameterCount; ++i)
        {
            bodyItems[i] =
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [i + 1],
                    Expression.EnvironmentInstance);
        }

        return
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                Expression.ListInst(bodyItems),
                parameterCount)
            ??
            throw new System.InvalidOperationException("Failed to build test function value.");
    }

    private static Expression Apply(
        PineValue functionValue,
        params PineValue[] arguments)
        =>
        Apply(Expression.LitralInst(functionValue), arguments);

    private static Expression Apply(
        Expression functionExpression,
        params PineValue[] arguments)
    {
        var expression = functionExpression;

        for (var i = 0; i < arguments.Length; ++i)
        {
            expression =
                new Expression.Eval(
                    expression,
                    Expression.LitralInst(arguments[i]));
        }

        return expression;
    }

    private static PineValue EvaluateWithDirectInterpreter(
        Expression expression,
        PineValue? environment = null) =>
        DirectInterpreter
        .WithoutEvalCaching(new PineVMParseCache())
        .EvaluateExpressionDefault(expression, environment ?? PineValue.EmptyList);

    private static EvaluationReport EvaluateWithPineVm(
        Expression expression,
        List<StackInstructionKind>? executedInstructions = null,
        PineValue? environment = null)
    {
        return
            EvaluateResultWithPineVm(expression, executedInstructions, environment)
            .Extract(
                error =>
                throw new System.InvalidOperationException(
                    EvaluationError.RenderDisplayString(error)));
    }

    private static Result<EvaluationError, EvaluationReport> EvaluateResultWithPineVm(
        Expression expression,
        List<StackInstructionKind>? executedInstructions = null,
        PineValue? environment = null,
        IntermediatePineVM.EvaluationConfig? config = null)
    {
        void ReportInstruction(in ExecutedStackInstruction executed) =>
            executedInstructions?.Add(executed.Instruction.Kind);

        var vm =
            IntermediatePineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: (_, _, _) => null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves:
                ImmutableDictionary<PineValue, PrecompiledLeaf>.Empty,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                disableDirectContinueForSimpleEval: true,
                disableDirectEvalForSimpleTemplate: true,
                reportExecutedStackInstruction:
                executedInstructions is null ? null : ReportInstruction);

        return
            vm.EvaluateExpressionOnCustomStack(
                expression,
                environment ?? PineValue.EmptyList,
                config ?? IntermediatePineVM.EvaluationConfig.Unbounded,
                cancellationToken: default);
    }
}
