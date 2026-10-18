using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
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
        result.IsOkOrNull()!.Counters.TemplateDirectInvocationCount.Should().Be(1);
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
        report.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().Be(1);
        report.Counters.DeferredTemplateValueAllocationCount.Should().Be(1);
        report.Counters.TemplateDirectInvocationCount.Should().Be(0);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(0);
        report.Counters.TemplateConsolidatedEvalCount.Should().Be(1);
        report.Counters.InvocationCount.Should().Be(0);
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
        report.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().Be(1);
        report.Counters.DeferredTemplateValueAllocationCount.Should().Be(0);
        report.Counters.TemplateDirectInvocationCount.Should().Be(1);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(0);
        report.Counters.TemplateConsolidatedEvalCount.Should().Be(3);
        report.Counters.InvocationCount.Should().Be(1);
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
    public void Structural_inspection_preserves_deferred_template_value()
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
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(0);
        report.Counters.TemplateDirectInvocationCount.Should().Be(0);
        report.CountersByOrigin.Total.Should().Be(report.Counters);

        report.CountersByOrigin
            .CachedWorkCounters.ExpressionTemplatePlanParsing.DirectInterpreterInvocationCount.Should().Be(0);

        report.CountersByOrigin.DeferredTemplateValueMaterialization.Should().Be(default(EvalWorkCounters));
        report.Counters.TemplateConsolidatedEvalCount.Should().Be(1);
        report.Counters.InvocationCount.Should().Be(0);
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
        report.Counters.TemplateDirectInvocationCount.Should().Be(2);
    }

    [Fact]
    public void Deferred_template_structural_observation_and_equality_do_not_materialize_the_root()
    {
        var expression =
            new Expression.Eval(
                Expression.LitralInst(BuildTemplateValue(3)),
                Expression.ListInst([Expression.EnvironmentInstance]));

        var environment = IntegerEncoding.EncodeSignedInteger(37);
        var report = EvaluateWithPineVm(expression, environment: environment);
        var partial = report.ReturnValue;
        var expected = EvaluateWithDirectInterpreter(expression, environment);
        var captured = partial.DeferredTemplateValueOrNull!.Environments[0];

        partial.IsList().Should().BeTrue();
        partial.IsBlob().Should().BeFalse();
        partial.GetLength().Should().Be(((PineValue.ListValue)expected).Items.Length);
        partial.GetElementAt(0).Evaluate().Should().Be(((PineValue.ListValue)expected).Items.Span[0]);
        PineValueInProcess.AreEqual(partial, expected).Should().BeTrue();
        PineValueInProcess.AreEqual(partial, PineValueInProcess.Create(expected)).Should().BeTrue();
        PineValueInProcess.AreEqual(partial, PineValue.EmptyBlob).Should().BeFalse();

        PineValueInProcess.ValueFromPathOrNull(partial, (IReadOnlyList<int>)[0])!.Evaluate()
            .Should().Be(((PineValue.ListValue)expected).Items.Span[0]);

        partial.EvaluatedOrNull.Should().BeNull();
        captured.EvaluatedOrNull.Should().BeNull();
    }

    [Fact]
    public void Collected_template_environments_are_immutable_and_reusable()
    {
        var prefix = CollectedTemplateEnvironments.Empty;

        for (var index = 0; index < 4_096; ++index)
            prefix = prefix.Append(PineValueInProcess.CreateInteger(index));

        var first = prefix.Append(PineValueInProcess.EmptyBlob);
        var second = prefix.Append(PineValueInProcess.EmptyList);

        prefix.Count.Should().Be(4_096);
        first.Count.Should().Be(4_097);
        second.Count.Should().Be(4_097);
        first[0].Should().BeSameAs(prefix[0]);
        second[2_000].Should().BeSameAs(prefix[2_000]);
        first[4_096].Should().BeSameAs(PineValueInProcess.EmptyBlob);
        second[4_096].Should().BeSameAs(PineValueInProcess.EmptyList);
        prefix[4_095].AsInteger().Should().Be(new System.Numerics.BigInteger(4_095));
    }

    [Fact]
    public void Materialized_partial_can_be_reused_without_a_new_plan_cache_lookup()
    {
        var cache = new PineVMExpressionTemplatePlanCache();
        var vm = CreateVm(templateCache: cache);
        var template = BuildTemplateValue(3);
        var firstEnvironment = IntegerEncoding.EncodeSignedInteger(11);

        var first =
            vm.EvaluateExpressionOnCustomStack(
                BuildEvalChain(Expression.EnvironmentInstance, firstEnvironment),
                template,
                IntermediatePineVM.EvaluationConfig.Unbounded,
                cancellationToken: default).IsOkOrNull()!;

        var partial = first.ReturnValue;
        var plan = partial.DeferredTemplateValueOrNull!.TemplatePlan;
        partial.Evaluate();
        partial.DeferredTemplateValueOrNull!.TemplatePlan.Should().BeSameAs(plan);
        cache.Count.Should().Be(1);

        var provider = Expression.LitralInst(PineValue.EmptyBlob);
        var alternateProvider = Expression.LitralInst(PineValue.EmptyList);

        var generic =
            new StackFrameInstructions(
                StaticFunctionInterface.ZeroParameters,
                [
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(13)),
                new StackInstruction(StackInstructionKind.Push_Literal, Literal: partial),
                StackInstruction.Eval_Binary,
                StackInstruction.Return
                ]);

        var alternateInstructions =
            new StackFrameInstructions(
                StaticFunctionInterface.ZeroParameters,
                [
                StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(17)),
                new StackInstruction(StackInstructionKind.Push_Literal, Literal: partial),
                StackInstruction.Eval_Binary,
                StackInstruction.Return
                ]);

        var continuingVm =
            CreateVm(
                templateCache: cache,
                compilationOverrides:
                new Dictionary<Expression, ExpressionCompilation>
                {
                    [provider] = new(generic, []),
                    [alternateProvider] = new(alternateInstructions, [])
                });

        var second =
            continuingVm.EvaluateExpressionOnCustomStack(
                provider, PineValue.EmptyList, IntermediatePineVM.EvaluationConfig.Unbounded, cancellationToken: default)
            .IsOkOrNull()!;

        second.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().Be(0);
        second.ReturnValue.DeferredTemplateValueOrNull!.TemplatePlan.Should().BeSameAs(plan);
        second.ReturnValue.DeferredTemplateValueOrNull!.Environments.Count.Should().Be(2);
        partial.DeferredTemplateValueOrNull!.Environments.Count.Should().Be(1);
        cache.Count.Should().Be(1);

        second.ReturnValue.Evaluate().Should().Be(
            EvaluateWithDirectInterpreter(
                BuildEvalChain(template, firstEnvironment, IntegerEncoding.EncodeSignedInteger(13))));

        var third =
            continuingVm.EvaluateExpressionOnCustomStack(
                alternateProvider, PineValue.EmptyList, IntermediatePineVM.EvaluationConfig.Unbounded, cancellationToken: default)
            .IsOkOrNull()!;

        third.ReturnValue.Should().NotBeSameAs(second.ReturnValue);

        third.ReturnValue.Evaluate().Should().Be(
            EvaluateWithDirectInterpreter(
                BuildEvalChain(template, firstEnvironment, IntegerEncoding.EncodeSignedInteger(17))));

        third.ReturnValue.Evaluate().Should().NotBe(second.ReturnValue.Evaluate());
        partial.DeferredTemplateValueOrNull!.Environments.Count.Should().Be(1);
        cache.Count.Should().Be(1);
    }

    [Fact]
    public void Varying_supplied_environments_do_not_create_template_plan_cache_entries()
    {
        var cache = new PineVMExpressionTemplatePlanCache();
        var vm = CreateVm(templateCache: cache);
        var template = BuildTemplateValue(3);

        for (var index = 0; index < 8; ++index)
        {
            var report =
                vm.EvaluateExpressionOnCustomStack(
                    BuildEvalChain(Expression.EnvironmentInstance, IntegerEncoding.EncodeSignedInteger(index)),
                    template,
                    IntermediatePineVM.EvaluationConfig.Unbounded,
                    cancellationToken: default).IsOkOrNull()!;

            report.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().Be(index is 0 ? 1 : 0);
        }

        cache.Count.Should().Be(1);
    }

    [Theory]
    [InlineData(1, 0, false)]
    [InlineData(3, 1, false)]
    [InlineData(3, 0, true)]
    public void Template_invocation_quotas_count_terminal_execution_not_saved_evals(
        int suppliedCount,
        int quota,
        bool fails)
    {
        var environments = new PineValue[suppliedCount];
        System.Array.Fill(environments, PineValue.EmptyBlob);

        var result =
            EvaluateResultWithPineVm(
                BuildEvalChain(BuildTemplateValue(3), environments),
                config: new(InvocationCountLimit: quota, LoopIterationCountLimit: null, StackDepthLimit: null));

        if (fails)
        {
            result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>();
            result.IsErrOrNull()!.Counters.InvocationCount.Should().Be(1);
            result.IsErrOrNull()!.Counters.TemplateConsolidatedEvalCount.Should().Be(2);
        }
        else
        {
            result.IsOkOrNull()!.Counters.InvocationCount.Should().Be(suppliedCount is 3 ? 1 : 0);
            result.IsOkOrNull()!.Counters.TemplateConsolidatedEvalCount.Should().Be(suppliedCount);
        }
    }

    [Theory]
    [InlineData(StackInstructionKind.Eval_Const)]
    [InlineData(StackInstructionKind.Eval_Binary)]
    [InlineData(StackInstructionKind.Eval_Multi)]
    public void Nonterminal_template_application_observes_cancellation_without_charging_invocation_quota(
        StackInstructionKind instructionKind)
    {
        using var cancellation = new System.Threading.CancellationTokenSource();
        var template = BuildTemplateValue(3);
        var first = IntegerEncoding.EncodeSignedInteger(37);

        var expression =
            instructionKind is StackInstructionKind.Eval_Multi
            ?
            BuildEvalChain(template, first, IntegerEncoding.EncodeSignedInteger(41))
            :
            BuildEvalChain(
                instructionKind is StackInstructionKind.Eval_Binary
                ?
                Expression.EnvironmentInstance
                :
                Expression.LitralInst(template),
                first);

        void Instruction(in ExecutedStackInstruction executed)
        {
            if (executed.Instruction.Kind == instructionKind)
                cancellation.Cancel();
        }

        var vm = CreateVm(reportInstruction: Instruction);

        var result =
            vm.EvaluateExpressionOnCustomStack(
                expression,
                instructionKind is StackInstructionKind.Eval_Binary ? template : PineValue.EmptyList,
                new(InvocationCountLimit: 0, LoopIterationCountLimit: null, StackDepthLimit: null),
                cancellationToken: cancellation.Token);

        result.IsErrOrNull().Should().NotBeNull();
        result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.CancellationRequested>();
        result.IsErrOrNull()!.Counters.InvocationCount.Should().Be(0);
        result.IsErrOrNull()!.Counters.TemplateConsolidatedEvalCount.Should().Be(0);
        result.IsErrOrNull()!.Counters.DeferredTemplateValueAllocationCount.Should().Be(0);
    }

    [Fact]
    public void Terminal_arguments_use_the_inferred_generic_interface_without_constructing_the_environment()
    {
        var terminal =
            ExpressionBuilder.BuildExpressionForPathInExpression([2, 1], Expression.EnvironmentInstance);

        var entry = BuildRepeatedEnvironmentTemplate(terminal);
        var first = PineValue.List([IntegerEncoding.EncodeSignedInteger(37)]);
        var second = PineValue.List([IntegerEncoding.EncodeSignedInteger(41)]);
        StackFrameInput? terminalInput = null;

        void Entered(in EnteredStackFrame frame)
        {
            if (frame.FrameExpression.Equals(terminal))
                terminalInput = frame.LoadFrameInput();
        }

        var vm = CreateVm();
        var expression = BuildEvalChain(entry, first, second);

        var report =
            vm.EvaluateExpressionOnCustomStack(
                expression, PineValue.EmptyList, IntermediatePineVM.EvaluationConfig.Unbounded,
                Entered, default).IsOkOrNull()!;

        report.ReturnValue.Evaluate().Should().Be(first);
        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        terminalInput.Should().NotBeNull();
        terminalInput!.Parameters.Should().Be(StaticFunctionInterface.FromPathsSorted([[2, 1]]));
        terminalInput.Arguments.Count.Should().Be(1);
        terminalInput.Arguments[0].Evaluate().Should().Be(first);
        report.Counters.BuildListCount.Should().Be(0);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(0);
    }

    [Fact]
    public void Terminal_guard_projection_checks_unforwarded_and_descendant_paths_without_materialization()
    {
        var first = PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(37)]);
        var second = PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(41)]);

        TemplateValueRecipe recipe =
            new TemplateValueRecipe.List(
                [
                new TemplateValueRecipe.Constant(PineValue.EmptyBlob),
                new TemplateValueRecipe.Slot(0),
                new TemplateValueRecipe.List([new TemplateValueRecipe.Slot(1), new TemplateValueRecipe.Slot(0)])
                ]);

        var parameters = StaticFunctionInterface.FromPathsSorted([[2, 1]]);
        var generic = new StackFrameInstructions(parameters, [StackInstruction.Local_Get(0), StackInstruction.Return]);

        var specialized =
            new StackFrameInstructions(parameters, [StackInstruction.Local_Get(0), StackInstruction.Return]);

        var compilation =
            new ExpressionCompilation(
                generic,
                [
                ([
                new EnvConstraintItem(new[] { 0 }, PineValue.EmptyBlob),
                new EnvConstraintItem(new[] { 2, 1, 0 }, IntegerEncoding.EncodeSignedInteger(37))
                ],
                specialized)
                ]);

        compilation.SelectInstructionsForTemplate(recipe, [first, second]).Should().BeSameAs(specialized);
        first.EvaluatedOrNull.Should().BeNull();
        second.EvaluatedOrNull.Should().BeNull();

        var missingGuard =
            new ExpressionCompilation(
                generic,
                [([new EnvConstraintItem(new[] { 9 }, PineValue.EmptyList)], specialized)]);

        missingGuard.SelectInstructionsForTemplate(recipe, [first, second]).Should().BeSameAs(generic);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Terminal_guard_projection_does_not_treat_a_path_inside_a_blob_as_an_empty_list(bool collectedBlob)
    {
        TemplateValueRecipe recipe =
            new TemplateValueRecipe.List(
                [
                collectedBlob
                ?
                new TemplateValueRecipe.Slot(0)
                :
                new TemplateValueRecipe.Constant(PineValue.EmptyBlob)
                ]);

        var generic =
            new StackFrameInstructions(
                StaticFunctionInterface.ZeroParameters,
                [StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Return]);

        var specialized =
            new StackFrameInstructions(
                StaticFunctionInterface.ZeroParameters,
                [StackInstruction.Push_Literal(PineValue.EmptyBlob), StackInstruction.Return]);

        var missingBlobField =
            new ExpressionCompilation(
                generic,
                [([new EnvConstraintItem(new[] { 0, 0 }, PineValue.EmptyList)], specialized)]);

        missingBlobField.SelectInstructionsForTemplate(recipe, [PineValueInProcess.EmptyBlob])
            .Should().BeSameAs(generic);

        var blobItself =
            new ExpressionCompilation(
                generic,
                [([new EnvConstraintItem(new[] { 0 }, PineValue.EmptyBlob)], specialized)]);

        blobItself.SelectInstructionsForTemplate(recipe, [PineValueInProcess.EmptyBlob])
            .Should().BeSameAs(specialized);
    }

    [Fact]
    public void Compiler_creates_deferred_producer_results_from_established_shape()
    {
        var entry = BuildRepeatedEnvironmentTemplate(Expression.EnvironmentInstance);
        var producer = new PineVMParseCache().ParseExpression(entry).IsOkOrNull()!;
        var vm = CreateVm(enableDirectInvocation: true);
        var environment = PineValue.List([IntegerEncoding.EncodeSignedInteger(37)]);

        var report =
            vm.EvaluateExpressionOnCustomStack(
                producer, environment, IntermediatePineVM.EvaluationConfig.Unbounded, cancellationToken: default)
            .IsOkOrNull()!;

        report.ReturnValue.DeferredTemplateValueOrNull.Should().NotBeNull();
        report.ReturnValue.EvaluatedOrNull.Should().BeNull();
        report.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().Be(0);
        report.Counters.BuildListCount.Should().Be(0);
        report.Counters.InvocationCount.Should().Be(0);
        report.Counters.TemplateConsolidatedEvalCount.Should().Be(0);
        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(producer, environment));
    }

    [Fact]
    public void Explicit_result_materialization_counts_only_nonterminal_materialization_work()
    {
        var expression = BuildEvalChain(BuildTemplateValue(3), IntegerEncoding.EncodeSignedInteger(37));

        var report =
            EvaluateResultWithPineVm(
                expression,
                config: IntermediatePineVM.EvaluationConfig.Unbounded with { MaterializeResult = true })
            .IsOkOrNull()!;

        report.ReturnValue.EvaluatedOrNull.Should().Be(EvaluateWithDirectInterpreter(expression));
        report.ReturnValue.DeferredTemplateValueOrNull.Should().NotBeNull();
        report.Counters.InvocationCount.Should().Be(0);
        report.Counters.TemplateConsolidatedEvalCount.Should().Be(1);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(1);

        report.CountersByOrigin
            .DeferredTemplateValueMaterialization.DeferredTemplateValueMaterializationCount.Should().Be(1);

        report.CountersByOrigin.VirtualMachine.TemplateConsolidatedEvalCount.Should().Be(1);
        report.CountersByOrigin.Total.Should().Be(report.Counters);
    }

    [Fact]
    public void Escaped_deferred_template_value_does_not_retain_its_vm_through_accounting_callbacks()
    {
        var (partial, owner) = CreateEscapedDeferredTemplate();
        System.GC.Collect();
        System.GC.WaitForPendingFinalizers();
        System.GC.Collect();

        owner.IsAlive.Should().BeFalse();
        partial.EvaluatedOrNull.Should().BeNull();

        partial.Evaluate().Should().Be(
            EvaluateWithDirectInterpreter(BuildEvalChain(BuildTemplateValue(3), IntegerEncoding.EncodeSignedInteger(37))));

        partial.DeferredTemplateValueOrNull.Should().NotBeNull();
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static (PineValueInProcess partial, System.WeakReference owner) CreateEscapedDeferredTemplate()
    {
        var vm = CreateVm();

        var report =
            vm.EvaluateExpressionOnCustomStack(
                BuildEvalChain(BuildTemplateValue(3), IntegerEncoding.EncodeSignedInteger(37)),
                PineValue.EmptyList,
                IntermediatePineVM.EvaluationConfig.Unbounded,
                cancellationToken: default).IsOkOrNull()!;

        return (report.ReturnValue, new System.WeakReference(vm));
    }

    [Fact]
    public void Compiler_known_literal_application_does_not_need_runtime_plan_recognition()
    {
        var vm = CreateVm(enableDirectInvocation: true);
        var expression = BuildEvalChain(BuildTemplateValue(3), IntegerEncoding.EncodeSignedInteger(37));

        var report =
            vm.EvaluateExpressionOnCustomStack(
                expression, PineValue.EmptyList, IntermediatePineVM.EvaluationConfig.Unbounded, cancellationToken: default)
            .IsOkOrNull()!;

        report.ReturnValue.DeferredTemplateValueOrNull.Should().NotBeNull();
        report.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().Be(0);
        report.Counters.TemplateConsolidatedEvalCount.Should().Be(1);
        report.Counters.InvocationCount.Should().Be(0);
        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
    }

    [Fact]
    public void Compiler_independent_constructor_producer_does_not_require_an_environment_parameter()
    {
        var producer =
            Expression.ListInst(
                [
                Expression.LitralInst(StringEncoding.ValueFromString("Builtin")),
                Expression.LitralInst(StringEncoding.ValueFromString("length")),
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance))
                ]);

        var vm = CreateVm(enableDirectInvocation: true);

        var report =
            vm.EvaluateExpressionOnCustomStack(
                producer, PineValue.EmptyBlob, IntermediatePineVM.EvaluationConfig.Unbounded, cancellationToken: default)
            .IsOkOrNull()!;

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(producer, PineValue.EmptyBlob));
        report.Counters.InvocationCount.Should().Be(0);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Invariant_constructor_terminal_runs_after_a_recognized_prefix(bool literalTerminal)
    {
        var terminal =
            literalTerminal
            ?
            Expression.LitralInst(PineValue.Blob([37]))
            :
            Expression.EnvironmentInstance;

        var entry =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(terminal)));

        var expression =
            BuildEvalChain(
                entry,
                IntegerEncoding.EncodeSignedInteger(41),
                PineValue.List([IntegerEncoding.EncodeSignedInteger(43)]));

        var report = EvaluateWithPineVm(expression);

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        report.Counters.InvocationCount.Should().Be(1);
        report.Counters.TemplateDirectInvocationCount.Should().Be(1);
        report.Counters.TemplateConsolidatedEvalCount.Should().Be(1);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(0);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Terminal_native_fallback_receives_the_recipe_environment_and_runs_once(bool overapply)
    {
        var terminal =
            overapply
            ?
            Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance))
            :
            ExpressionBuilder.BuildExpressionForPathInExpression([2, 1], Expression.EnvironmentInstance);

        var entry = BuildRepeatedEnvironmentTemplate(terminal);
        var first = PineValue.List([IntegerEncoding.EncodeSignedInteger(37)]);
        var second = PineValue.List([IntegerEncoding.EncodeSignedInteger(41)]);
        var final = IntegerEncoding.EncodeSignedInteger(43);
        var nativeCount = 0;

        System.Func<PrecompiledResult>? Select(
            Expression expression,
            PineValueInProcess environment,
            PineVMParseCache _)
        {
            if (!expression.Equals(terminal))
                return null;

            ++nativeCount;

            return
                () =>
                {
                    environment.Evaluate().Should().Be(PineValue.List([first, second, PineValue.List([second, first])]));

                    return
                        new PrecompiledResult.FinalValue(
                            overapply
                            ?
                            ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)
                            :
                            first,
                            StackFrameCount: 0);
                };
        }

        var vm = CreateVm(nativeSelector: Select);
        var expression = BuildEvalChain(entry, overapply ? [first, second, final] : [first, second]);

        var report =
            vm.EvaluateExpressionOnCustomStack(
                expression, PineValue.EmptyList, IntermediatePineVM.EvaluationConfig.Unbounded, cancellationToken: default)
            .IsOkOrNull()!;

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        nativeCount.Should().Be(1);
        report.Counters.InvocationCount.Should().Be(overapply ? 2 : 1);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Terminal_leaf_fallback_does_not_repeat_a_native_selector_miss(bool overapply, bool leafDeclines)
    {
        var terminal =
            overapply
            ?
            Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance))
            :
            ExpressionBuilder.BuildExpressionForPathInExpression([2, 1], Expression.EnvironmentInstance);

        var terminalValue = ExpressionEncoding.EncodeExpressionAsValue(terminal);
        var first = IntegerEncoding.EncodeSignedInteger(37);
        var second = IntegerEncoding.EncodeSignedInteger(41);
        var final = IntegerEncoding.EncodeSignedInteger(43);
        var selections = 0;
        var leaves = 0;

        System.Func<PrecompiledResult>? Select(
            Expression expression, PineValueInProcess environment, PineVMParseCache cache)
        {
            if (expression.Equals(terminal))
                ++selections;

            return null;
        }

        PineValueInProcess? Leaf(PineValueInProcess environment)
        {
            ++leaves;

            return
                leafDeclines
                ?
                null
                :
                PineValueInProcess.Create(EvaluateWithDirectInterpreter(terminal, environment.Evaluate()));
        }

        var vm =
            CreateVm(
                nativeSelector: Select,
                leaves: new Dictionary<PineValue, PrecompiledLeaf> { [terminalValue] = Leaf });

        var expression =
            BuildEvalChain(
                BuildRepeatedEnvironmentTemplate(terminal),
                overapply ? [first, second, final] : [first, second]);

        var report =
            vm.EvaluateExpressionOnCustomStack(
                expression, PineValue.EmptyList, IntermediatePineVM.EvaluationConfig.Unbounded, cancellationToken: default)
            .IsOkOrNull()!;

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        selections.Should().Be(1);
        leaves.Should().Be(1);
    }

    [Fact]
    public void Materializing_an_escaped_partial_is_charged_to_the_current_evaluation()
    {
        var partial =
            EvaluateWithPineVm(
                BuildEvalChain(BuildTemplateValue(3), IntegerEncoding.EncodeSignedInteger(37))).ReturnValue;

        partial.EvaluatedOrNull.Should().BeNull();
        var provider = Expression.LitralInst(PineValue.EmptyBlob);

        var instructions =
            new StackFrameInstructions(
                StaticFunctionInterface.ZeroParameters,
                [new StackInstruction(StackInstructionKind.Push_Literal, Literal: partial), StackInstruction.Return]);

        var vm =
            CreateVm(
                compilationOverrides:
                new Dictionary<Expression, ExpressionCompilation> { [provider] = new(instructions, []) });

        var report =
            vm.EvaluateExpressionOnCustomStack(
                provider,
                PineValue.EmptyList,
                IntermediatePineVM.EvaluationConfig.Unbounded with { MaterializeResult = true },
                cancellationToken: default).IsOkOrNull()!;

        report.ReturnValue.Should().BeSameAs(partial);
        report.Counters.DeferredTemplateValueMaterializationCount.Should().Be(1);

        report.CountersByOrigin
            .DeferredTemplateValueMaterialization.DeferredTemplateValueMaterializationCount.Should().Be(1);

        report.CountersByOrigin.Total.Should().Be(report.Counters);
        partial.EvaluatedOrNull.Should().NotBeNull();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Saved_eval_counter_includes_elided_terminal_wrappers_only_when_present(bool wrapper)
    {
        var terminal = Expression.BuiltinInst("length", Expression.EnvironmentInstance);
        var encodedTerminal = ExpressionEncoding.EncodeExpressionAsValue(terminal);

        var entry =
            wrapper
            ?
            ExpressionEncoding.EncodeExpressionAsValue(
                new Expression.Eval(Expression.LitralInst(encodedTerminal), Expression.EnvironmentInstance))
            :
            ExpressionEncoding.EncodeExpressionAsValue(Expression.LitralInst(encodedTerminal));

        var expression =
            BuildEvalChain(entry, wrapper ? [PineValue.EmptyList] : [PineValue.EmptyList, PineValue.EmptyList]);

        var report = EvaluateWithPineVm(expression);

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        report.Counters.InvocationCount.Should().Be(1);
        report.Counters.TemplateConsolidatedEvalCount.Should().Be(1);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Mixed_encoding_future_code_fails_only_when_applied(bool eval, bool compileTimeRecognition)
    {
        var legacyLiteral =
            ExpressionEncoding2024.EncodeExpressionAsValue(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)));

        var malformed =
            PineValue.List(
                eval
                ?
                [
                StringEncoding.ValueFromString("Eval"), legacyLiteral,
                ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)
                ]
                :
                [StringEncoding.ValueFromString("List"), legacyLiteral]);

        var entry = ExpressionEncoding.EncodeExpressionAsValue(Expression.LitralInst(malformed));
        var argument = IntegerEncoding.EncodeSignedInteger(41);

        var expression =
            BuildEvalChain(
                compileTimeRecognition ? Expression.LitralInst(entry) : Expression.EnvironmentInstance,
                PineValue.EmptyList,
                argument);

        var optimized = CreateVm(enableDirectInvocation: compileTimeRecognition);
        var ordinary = CreateVm(disableDirectInvocation: true);

        EvaluateWithPineVm(BuildEvalChain(entry, PineValue.EmptyList)).ReturnValue.Evaluate().Should().Be(malformed);

        var optimizedError =
            optimized.EvaluateExpressionOnCustomStack(
                expression, entry, IntermediatePineVM.EvaluationConfig.Unbounded, cancellationToken: default).IsErrOrNull()!;

        var ordinaryError =
            ordinary.EvaluateExpressionOnCustomStack(
                expression, entry, IntermediatePineVM.EvaluationConfig.Unbounded, cancellationToken: default).IsErrOrNull()!;

        optimizedError.Should().NotBeNull();
        ordinaryError.Should().NotBeNull();
        var reason = optimizedError.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>().Subject;

        var ordinaryReason =
            ordinaryError.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>().Subject;

        reason.ExpressionValue.Should().Be(malformed);
        reason.EnvironmentValue.Evaluate().Should().Be(argument);
        reason.ParseError.Should().Be(ordinaryReason.ParseError);
    }

    [Fact]
    public void Legacy_template_constructors_use_ordinary_evaluation()
    {
        var entry =
            ExpressionEncoding2024.EncodeExpressionAsValue(
                Expression.LitralInst(
                    ExpressionEncoding2024.EncodeExpressionAsValue(Expression.EnvironmentInstance)));

        var expression = BuildEvalChain(entry, PineValue.EmptyList, IntegerEncoding.EncodeSignedInteger(37));
        var report = EvaluateWithPineVm(expression);

        report.ReturnValue.Evaluate().Should().Be(EvaluateWithDirectInterpreter(expression));
        report.Counters.TemplateConsolidatedEvalCount.Should().Be(0);
        report.Counters.TemplateDirectInvocationCount.Should().Be(0);
        report.Counters.InvocationCount.Should().Be(2);
    }

    private static PineValue BuildRepeatedEnvironmentTemplate(Expression terminal)
    {
        var encodedEnvironment = ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance);

        Expression Captured() =>
            Expression.ListInst(
                [Expression.LitralInst(StringEncoding.ValueFromString("Litral")), Expression.EnvironmentInstance]);

        return
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ListInst(
                    [
                    Expression.LitralInst(StringEncoding.ValueFromString("Eval")),
                    Expression.LitralInst(
                        ExpressionEncoding.EncodeExpressionAsValue(
                            Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(terminal)))),
                    Expression.ListInst(
                        [
                        Expression.LitralInst(StringEncoding.ValueFromString("List")),
                        Captured(),
                        Expression.LitralInst(encodedEnvironment),
                        Expression.ListInst(
                            [
                            Expression.LitralInst(StringEncoding.ValueFromString("List")),
                            Expression.LitralInst(encodedEnvironment),
                            Captured()
                            ])
                        ])
                    ]));
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

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Live_events_preserve_separate_cached_work_on_success_and_error(bool fail)
    {
        var template =
            BuildEvalChain(
                BuildTemplateValue(environmentCount: 3),
                IntegerEncoding.EncodeSignedInteger(11));

        var expression =
            fail
            ?
            Expression.ListInst(
                [
                template,
                new Expression.Eval(
                    Expression.LitralInst(PineValue.EmptyBlob),
                    Expression.LitralInst(PineValue.EmptyList))
                ])
            :
            template;

        var lastCached = default(CachedWorkCounters);
        var eventCount = 0;

        void Observe(in EvaluationEvent evaluationEvent)
        {
            var origins = evaluationEvent.LoadCountersByOrigin!();
            evaluationEvent.LoadCounters().Should().Be(origins.Total);
            lastCached = origins.CachedWorkCounters;
            ++eventCount;
        }

        var result =
            CreateVm(reportEvaluationEvent: Observe).EvaluateExpressionOnCustomStack(
                expression,
                PineValue.EmptyList,
                IntermediatePineVM.EvaluationConfig.Unbounded,
                cancellationToken: default);

        eventCount.Should().BeGreaterThan(0);
        lastCached.ExpressionTemplatePlanParseCount.Should().BeGreaterThan(0);

        if (fail)
        {
            var error = result.IsErrOrNull();
            error.Should().NotBeNull();
            error!.CachedWorkCounters.Should().Be(lastCached);
            error.Counters.Should().Be(error.CountersByOrigin.Total);
        }
        else
        {
            var report = result.IsOkOrNull();
            report.Should().NotBeNull();
            report!.CachedWorkCounters.Should().Be(lastCached);
            report.Counters.Should().Be(report.CountersByOrigin.Total);
        }
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
            new EvalWorkCounters(
                InvocationCount: 1,
                BuildListCount: 2,
                LoopIterationCount: 3,
                InstructionCount: 4,
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
                DirectInterpreterEnvironmentCount: 17,
                TemplateConsolidatedEvalCount: 18);

        EvalWorkCountersFormatting.FormatCounts(counters).ShouldBeWithDiff(
            """
            InvocationCount: 1
            BuildListCount: 2
            BuildListItemCount: 9
            LoopIterationCount: 3
            InstructionCount: 4
            DeferredTemplateValueAllocationCount: 6
            TemplateDirectInvocationCount: 7
            DeferredTemplateValueMaterializationCount: 8
            TemplateConsolidatedEvalCount: 18
            DirectInterpreterInvocationCount: 10
            DirectInterpreterExpressionCount: 11
            DirectInterpreterLiteralCount: 12
            DirectInterpreterListCount: 13
            DirectInterpreterEvalCount: 14
            DirectInterpreterBuiltinCount: 15
            DirectInterpreterConditionalCount: 16
            DirectInterpreterEnvironmentCount: 17
            """);

        EvalWorkCountersFormatting.FormatAllCounts(counters).Should().Contain(
            "DeferredTemplateValueAllocationCount: 6");

        EvalWorkCountersFormatting.FormatAllCounts(counters).ShouldBeWithDiff(
            EvalWorkCountersFormatting.FormatCounts(counters));

        System.Text
            .Json.JsonSerializer.Serialize(counters).Should().Contain("\"BuildListCount\":2,\"BuildListItemCount\":9")
            .And.NotContain("Label");

        EvalWorkCounters.Add(counters, counters).Should().Be(EvalWorkCounters.Aggregate([counters, counters]));
        EvalWorkCounters.Subtract(EvalWorkCounters.Add(counters, counters), counters).Should().Be(counters);
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
            CreateVm(
                enableDirectInvocation,
                reportInstruction: executedInstructions is null ? null : ReportInstruction);

        return
            vm.EvaluateExpressionOnCustomStack(
                expression,
                environment ?? PineValue.EmptyList,
                config ?? IntermediatePineVM.EvaluationConfig.Unbounded,
                cancellationToken: default);
    }

    private static IntermediatePineVM CreateVm(
        bool enableDirectInvocation = false,
        PineVMExpressionTemplatePlanCache? templateCache = null,
        IReadOnlyDictionary<Expression, ExpressionCompilation>? compilationOverrides = null,
        ReportExecutedStackInstruction? reportInstruction = null,
        System.Func<Expression, PineValueInProcess, PineVMParseCache, System.Func<PrecompiledResult>?>? nativeSelector = null,
        IReadOnlyDictionary<PineValue, PrecompiledLeaf>? leaves = null,
        bool disableDirectInvocation = false,
        ReportEvaluationEvent? reportEvaluationEvent = null) =>
        IntermediatePineVM.CreateCustom(
            expressionTemplatePlanCache: templateCache ?? new PineVMExpressionTemplatePlanCache(),
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            selectPrecompiled: nativeSelector ?? (enableDirectInvocation ? null : (_, _, _) => null),
            skipInlineForExpression: _ => enableDirectInvocation,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves:
            leaves ?? ImmutableDictionary<PineValue, PrecompiledLeaf>.Empty,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null,
            disableDirectContinueForSimpleEval: true,
            disableDirectEvalForSimpleTemplate: true,
            reportExecutedStackInstruction:
            reportInstruction,
            expressionCompilationOverrides: compilationOverrides,
            disableDirectInvocation: disableDirectInvocation,
            reportEvaluationEvent: reportEvaluationEvent);
}
