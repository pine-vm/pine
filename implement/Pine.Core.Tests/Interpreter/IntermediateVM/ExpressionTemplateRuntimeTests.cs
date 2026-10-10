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

public class ExpressionTemplateRuntimeTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Expression_producer_with_embedded_template_does_not_pack_environments_as_a_pair(bool literalTemplate)
    {
        var body =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [1],
                Expression.EnvironmentInstance);

        var function =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(body, parameterCount: 1)!;

        // The first Eval builds head([environment, literal encoded expression]).
        // The second Eval evaluates that expression and returns its environment.
        var producer =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ListInst(
                    [
                    Expression.LitralInst(StringEncoding.ValueFromString("Builtin")),
                    Expression.LitralInst(StringEncoding.ValueFromString("head")),
                    Expression.ListInst(
                        [
                        Expression.LitralInst(StringEncoding.ValueFromString("List")),
                        Expression.LitralInst(
                            ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                        Expression.ListInst(
                            [
                            Expression.LitralInst(StringEncoding.ValueFromString("Litral")),
                            Expression.LitralInst(function)
                            ])
                        ])
                    ]));

        var state =
            PineValue.List(
                [
                StringEncoding.ValueFromString("<Record>"),
                StringEncoding.ValueFromString("nextId"),
                IntegerEncoding.EncodeSignedInteger(1)
                ]);

        var expression =
            BuildEvalChain(
                literalTemplate
                ?
                Expression.LitralInst(producer)
                :
                ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance),
                IntegerEncoding.EncodeSignedInteger(11),
                state);

        var environment = PineValue.List([producer]);
        var expected = EvaluateWithDirectInterpreter(expression, environment);
        expected.Should().Be(state);

        var result =
            EvaluateResultWithPineVm(
                expression,
                environment: environment,
                enableDirectInvocation: literalTemplate,
                config:
                new IntermediatePineVM.EvaluationConfig(
                    InvocationCountLimit: 100,
                    LoopIterationCountLimit: 100,
                    StackDepthLimit: 100));

        result.IsErrOrNull().Should().BeNull();
        result.IsOkOrNull()!.ReturnValue.Evaluate().Should().Be(expected);
        result.IsOkOrNull()!.Counters.TemplateDirectInvocationCount.Should().Be(0);
    }

    [Fact]
    public void Expression_producer_does_not_freeze_a_probe_dependent_terminal_expression()
    {
        var producer =
            FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgsFromEncodedBodyExpression(
                innerExprEncodedExpression:
                Expression.ListInst(
                    [
                    Expression.LitralInst(StringEncoding.ValueFromString("Litral")),
                    Expression.EnvironmentInstance
                    ]),
                parameterCount: 2,
                leadingArgExpressions: [Expression.EnvironmentInstance],
                envFunctionsExpression: Expression.LitralInst(PineValue.EmptyList));

        var expression =
            BuildEvalChain(
                ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance),
                IntegerEncoding.EncodeSignedInteger(11),
                IntegerEncoding.EncodeSignedInteger(22));

        var environment = PineValue.List([ExpressionEncoding.EncodeExpressionAsValue(producer)]);
        var expected = EvaluateWithDirectInterpreter(expression, environment);
        expected.Should().Be(IntegerEncoding.EncodeSignedInteger(11));

        EvaluateWithPineVm(expression, environment: environment).ReturnValue.Evaluate().Should().Be(expected);
    }

    [Fact]
    public void Function_record_parsing_does_not_execute_deferred_template_computations()
    {
        var parseCache = new PineVMParseCache();
        var templateValue = BuildTemplateValue(environmentCount: 2);
        var template = (Expression.List)parseCache.ParseExpression(templateValue).IsOkOrNull()!;
        var items = new List<Expression>(template.Items);

        items[1] =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(items[1])),
                Expression.EnvironmentInstance);

        var deferredTemplate =
            ExpressionEncoding.EncodeExpressionAsValue(Expression.ListInst(items));

        FunctionRecord.ParseFunctionRecordTagged(deferredTemplate, parseCache)
            .IsErrOrNull().Should().NotBeNull();

        var expression =
            BuildEvalChain(
                deferredTemplate,
                IntegerEncoding.EncodeSignedInteger(11),
                IntegerEncoding.EncodeSignedInteger(13));

        EvaluateWithPineVm(expression).ReturnValue.Evaluate()
            .Should().Be(EvaluateWithDirectInterpreter(expression));
    }

    [Fact]
    public void Deferred_template_result_matches_direct_interpreter_without_materializing_intermediate_value()
    {
        var templateValue = BuildTemplateValue(environmentCount: 3);
        var environmentValue = IntegerEncoding.EncodeSignedInteger(11);

        var expression =
            new Expression.Eval(
                Expression.LitralInst(templateValue),
                Expression.LitralInst(environmentValue));

        var report = EvaluateWithPineVm(expression);
        var expected = EvaluateWithDirectInterpreter(expression);

        report.ReturnValue.DeferredTemplateValueOrNull.Should().NotBeNull();
        report.ReturnValue.EvaluatedOrNull.Should().BeNull();
        report.Counters.ExpressionTemplatePlanParseCount.Should().Be(1);
        report.Counters.DeferredTemplateValueAllocationCount.Should().Be(1);
        report.Counters.TemplateDirectInvocationCount.Should().Be(0);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(0);
        report.ReturnValue.Evaluate().Should().Be(expected);
    }

    [Fact]
    public void Template_direct_invocation_matches_direct_interpreter()
    {
        var templateValue = BuildTemplateValue(environmentCount: 3);

        var expression =
            BuildEvalChain(
                templateValue,
                IntegerEncoding.EncodeSignedInteger(11),
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17));

        var report = EvaluateWithPineVm(expression);
        var expected = EvaluateWithDirectInterpreter(expression);

        report.ReturnValue.DeferredTemplateValueOrNull.Should().BeNull();
        report.Counters.ExpressionTemplatePlanParseCount.Should().Be(1);
        report.Counters.DeferredTemplateValueAllocationCount.Should().Be(0);
        report.Counters.TemplateDirectInvocationCount.Should().Be(1);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(0);
        report.ReturnValue.Evaluate().Should().Be(expected);
    }

    [Fact]
    public void Deferred_template_value_after_multiple_environments_matches_direct_interpreter()
    {
        var templateValue = BuildTemplateValue(environmentCount: 4);

        var expression =
            BuildEvalChain(
                templateValue,
                IntegerEncoding.EncodeSignedInteger(11),
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17));

        var report = EvaluateWithPineVm(expression);
        var expected = EvaluateWithDirectInterpreter(expression);

        report.ReturnValue.DeferredTemplateValueOrNull.Should().NotBeNull();
        report.ReturnValue.EvaluatedOrNull.Should().BeNull();
        report.Counters.DeferredTemplateValueAllocationCount.Should().Be(1);
        report.Counters.TemplateDirectInvocationCount.Should().Be(0);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(0);
        report.ReturnValue.Evaluate().Should().Be(expected);
    }

    [Fact]
    public void Evaluating_materialized_template_result_matches_direct_interpreter()
    {
        var templateValue = BuildTemplateValue(environmentCount: 3);
        var firstEnvironment = IntegerEncoding.EncodeSignedInteger(11);

        var intermediateExpression = BuildEvalChain(templateValue, firstEnvironment);
        var intermediateValue = EvaluateWithDirectInterpreter(intermediateExpression);

        var remainingExpression =
            BuildEvalChain(
                intermediateValue,
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17));

        EvaluateWithPineVm(remainingExpression).ReturnValue.Evaluate()
            .Should().Be(EvaluateWithDirectInterpreter(remainingExpression));
    }

    [Fact]
    public void Structural_inspection_reports_deferred_template_value_materialization()
    {
        var intermediateExpression =
            BuildEvalChain(
                BuildTemplateValue(environmentCount: 3),
                IntegerEncoding.EncodeSignedInteger(11));

        var expression =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.length),
                intermediateExpression);

        var report = EvaluateWithPineVm(expression);

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        report.Counters.DeferredTemplateValueAllocationCount.Should().Be(1);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(1);
        report.Counters.TemplateDirectInvocationCount.Should().Be(0);
        report.CountersByOrigin.Total.Should().Be(report.Counters);
        report.CountersByOrigin.ExpressionTemplatePlanParsing.DirectInterpreterInvocationCount.Should().BeGreaterThan(0);
        report.CountersByOrigin.DeferredTemplateValueMaterialization.DirectInterpreterInvocationCount.Should().Be(1);
        report.CountersByOrigin.DeferredTemplateValueMaterialization.DirectInterpreterEvalCount.Should().Be(1);
        report.CountersByOrigin.DeferredTemplateValueMaterialization.BuildListItemCount.Should().BeGreaterThan(0);
    }

    [Fact]
    public void Nested_eval_expression_executes_fused_eval_var_instruction()
    {
        var expression =
            BuildEvalChain(
                BuildTemplateValue(environmentCount: 3),
                IntegerEncoding.EncodeSignedInteger(11),
                IntegerEncoding.EncodeSignedInteger(13),
                IntegerEncoding.EncodeSignedInteger(17));

        var executedInstructions = new List<StackInstructionKind>();
        var report = EvaluateWithPineVm(expression, executedInstructions);

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        executedInstructions.Should().Contain(StackInstructionKind.Eval_Multi);
        report.Counters.DeferredTemplateValueAllocationCount.Should().Be(0);
        report.Counters.TemplateDirectInvocationCount.Should().Be(1);
    }

    [Fact]
    public void Fused_eval_falls_back_for_noncanonical_template_value()
    {
        var identityFunction =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.EnvironmentInstance);

        var constantFunction =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)));

        var expression =
            BuildEvalChain(
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
        report.Counters.TemplateDirectInvocationCount.Should().Be(0);
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
            BuildEvalChain(
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
    public void Fused_eval_beyond_template_terminal_matches_direct_interpreter()
    {
        var innerFunction =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [1],
                    Expression.EnvironmentInstance),
                parameterCount: 1)
            ??
            throw new System.InvalidOperationException("Failed to build inner template value.");

        var outerFunction =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                Expression.LitralInst(innerFunction),
                parameterCount: 1)
            ??
            throw new System.InvalidOperationException("Failed to build outer template value.");

        var expression =
            BuildEvalChain(
                outerFunction,
                IntegerEncoding.EncodeSignedInteger(41),
                IntegerEncoding.EncodeSignedInteger(43));

        var report = EvaluateWithPineVm(expression);

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        report.Counters.DeferredTemplateValueAllocationCount.Should().Be(0);
        report.Counters.TemplateDirectInvocationCount.Should().Be(1);
    }

    [Fact]
    public void Fused_eval_preserves_structured_parse_error()
    {
        var expression =
            BuildEvalChain(
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
            BuildEvalChain(
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
    public void Fused_eval_preserves_environment_evaluation_order()
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
    public void Default_counter_format_includes_every_counter()
    {
        var counters =
            new PerformanceCounters(
                InvocationCount: 1,
                BuildListCount: 2,
                LoopIterationCount: 3,
                InstructionCount: 4,
                ExpressionTemplatePlanParseCount: 5,
                DeferredTemplateValueAllocationCount: 6,
                TemplateDirectInvocationCount: 7,
                DeferredTemplateValueMaterializationCount: 8,
                BuildListItemCount: 9,
                DirectInterpreterInvocationCount: 10,
                DirectInterpreterExpressionCount: 11,
                DirectInterpreterLiteralCount: 12,
                DirectInterpreterListCount: 13,
                DirectInterpreterEvalCount: 14,
                DirectInterpreterBuiltinCount: 15,
                DirectInterpreterConditionalCount: 16,
                DirectInterpreterEnvironmentCount: 17);

        PerformanceCountersFormatting.FormatCounts(counters).ShouldBeWithDiff(
            """
            InvocationCount: 1
            BuildListCount: 2
            BuildListItemCount: 9
            LoopIterationCount: 3
            InstructionCount: 4
            ExpressionTemplatePlanParseCount: 5
            DeferredTemplateValueAllocationCount: 6
            TemplateDirectInvocationCount: 7
            DeferredTemplateValueMaterializationCount: 8
            DirectInterpreterInvocationCount: 10
            DirectInterpreterExpressionCount: 11
            DirectInterpreterLiteralCount: 12
            DirectInterpreterListCount: 13
            DirectInterpreterEvalCount: 14
            DirectInterpreterBuiltinCount: 15
            DirectInterpreterConditionalCount: 16
            DirectInterpreterEnvironmentCount: 17
            """);

        PerformanceCountersFormatting.FormatAllCounts(counters).Should().Contain(
            "DeferredTemplateValueAllocationCount: 6");

        PerformanceCountersFormatting.FormatAllCounts(counters).ShouldBeWithDiff(
            PerformanceCountersFormatting.FormatCounts(counters));

        System.Text
            .Json.JsonSerializer.Serialize(counters).Should().Contain("\"BuildListCount\":2,\"BuildListItemCount\":9")
            .And.NotContain("Label");

        PerformanceCounters.Add(counters, counters).Should().Be(PerformanceCounters.Aggregate([counters, counters]));
        PerformanceCounters.Subtract(PerformanceCounters.Add(counters, counters), counters).Should().Be(counters);
    }

    private static PineValue BuildTemplateValue(int environmentCount)
    {
        var bodyItems = new Expression[environmentCount];

        for (var i = 0; i < environmentCount; ++i)
        {
            bodyItems[i] =
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [i + 1],
                    Expression.EnvironmentInstance);
        }

        return
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                Expression.ListInst(bodyItems),
                environmentCount)
            ??
            throw new System.InvalidOperationException("Failed to build test template value.");
    }

    private static Expression BuildEvalChain(
        PineValue templateValue,
        params PineValue[] environments)
        =>
        BuildEvalChain(Expression.LitralInst(templateValue), environments);

    private static Expression BuildEvalChain(
        Expression encodedExpression,
        params PineValue[] environments)
    {
        var expression = encodedExpression;

        for (var i = 0; i < environments.Length; ++i)
        {
            expression =
                new Expression.Eval(
                    expression,
                    Expression.LitralInst(environments[i]));
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
        IntermediatePineVM.EvaluationConfig? config = null,
        bool enableDirectInvocation = false)
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
                selectPrecompiled: enableDirectInvocation ? null : (_, _, _) => null,
                skipInlineForExpression: _ => enableDirectInvocation,
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
