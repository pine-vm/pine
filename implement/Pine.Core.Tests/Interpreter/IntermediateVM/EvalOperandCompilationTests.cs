using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Linq;
using Xunit;

using IntermediatePineVM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class EvalOperandCompilationTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Eval_environment_failure_precedes_encoded_operand_failure(
        bool enableTailRecursionOptimization,
        bool outerIsTail)
    {
        var environmentFailure = StringEncoding.ValueFromString("environment operand failed");
        var encodedFailure = StringEncoding.ValueFromString("encoded operand failed");
        var eval =
            new Expression.Eval(
                encoded: FailingOperand(encodedFailure),
                environment: FailingOperand(environmentFailure));
        var expression = outerIsTail ? (Expression)eval : Expression.ListInst([eval]);

        var directError =
            FluentActions.Invoking(
                () => new DirectInterpreter(new(), evalCache: null)
                    .EvaluateExpressionDefault(expression, PineValue.EmptyList))
            .Should().Throw<ParseExpressionException>().Which;

        directError.Message.Should().Contain("environment operand failed");
        directError.Message.Should().NotContain("encoded operand failed");

        var error =
            Evaluate(expression, PineValue.EmptyList, enableTailRecursionOptimization)
            .IsErrOrNull();

        error.Should().NotBeNull();
        error!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>()
            .Which.ExpressionValue.Should().Be(environmentFailure);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Eval_environment_failure_precedes_parsing_literal_target(
        bool enableTailRecursionOptimization)
    {
        var environmentFailure = StringEncoding.ValueFromString("environment before target parsing");
        var expression =
            new Expression.Eval(
                encoded: Expression.LitralInst(PineValue.EmptyList),
                environment: FailingOperand(environmentFailure));

        FluentActions.Invoking(
            () => new DirectInterpreter(new(), evalCache: null)
                .EvaluateExpressionDefault(expression, PineValue.EmptyList))
            .Should().Throw<ParseExpressionException>().Which
            .Message.Should().Contain("environment before target parsing");

        var error =
            Evaluate(expression, PineValue.EmptyList, enableTailRecursionOptimization)
            .IsErrOrNull();

        error.Should().NotBeNull();
        error!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>()
            .Which.ExpressionValue.Should().Be(environmentFailure);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Eval_operand_invoking_root_returns_to_outer_invocation(
        bool enableTailRecursionOptimization,
        bool recursiveEnvironmentOperand)
    {
        var marker = StringEncoding.ValueFromString("outer invocation completed");
        var payload = IntegerEncoding.EncodeSignedInteger(7);
        var target =
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ListInst([Expression.LitralInst(marker), Expression.EnvironmentInstance]));
        var recursiveOperand =
            new Expression.Eval(
                encoded: EnvironmentPath(0),
                environment:
                Expression.ListInst(
                    [
                    EnvironmentPath(0),
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(0)),
                    EnvironmentPath(2),
                    EnvironmentPath(3),
                    ]));
        var outerEval =
            new Expression.Eval(
                encoded: recursiveEnvironmentOperand ? EnvironmentPath(2) : recursiveOperand,
                environment: recursiveEnvironmentOperand ? recursiveOperand : EnvironmentPath(3));
        var expression =
            Expression.ConditionalInst(
                condition:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst(
                        [
                        EnvironmentPath(1),
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(0)),
                        ])),
                falseBranch: outerEval,
                trueBranch: Expression.LitralInst(recursiveEnvironmentOperand ? payload : target));
        var environment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(expression),
                IntegerEncoding.EncodeSignedInteger(1),
                target,
                payload,
                ]);
        var expected = PineValue.List([marker, payload]);

        new DirectInterpreter(new(), evalCache: null)
            .EvaluateExpressionDefault(expression, environment).Should().Be(expected);

        var report =
            Evaluate(expression, environment, enableTailRecursionOptimization)
            .Extract(error => throw new InvalidOperationException(EvaluationError.RenderDisplayString(error)));

        report.ReturnValue.Evaluate().Should().Be(expected);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Eval_operand_order_is_preserved_when_an_operand_diverges(
        bool enableTailRecursionOptimization,
        bool environmentDiverges)
    {
        var loop =
            new Expression.Eval(
                encoded: EnvironmentPath(0),
                environment: Expression.EnvironmentInstance);
        var failure = StringEncoding.ValueFromString("finite operand failure");
        var failingOperand = FailingOperand(failure);
        var expression =
            new Expression.Eval(
                encoded: environmentDiverges ? failingOperand : loop,
                environment: environmentDiverges ? loop : failingOperand);
        var environment = PineValue.List([ExpressionEncoding.EncodeExpressionAsValue(loop)]);

        // The direct interpreter has no quota; only use it when the first operand fails finitely.
        if (!environmentDiverges)
        {
            FluentActions.Invoking(
                () => new DirectInterpreter(new(), evalCache: null)
                    .EvaluateExpressionDefault(expression, environment))
                .Should().Throw<ParseExpressionException>().Which
                .Message.Should().Contain("finite operand failure");
        }

        var error =
            Evaluate(expression, environment, enableTailRecursionOptimization)
            .IsErrOrNull();

        error.Should().NotBeNull();

        if (environmentDiverges)
        {
            error!.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>();
        }
        else
        {
            error!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>()
                .Which.ExpressionValue.Should().Be(failure);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Guarded_loop_materializes_invoking_environment_before_parallel_parameter_updates(
        bool enableTailRecursionOptimization)
    {
        var first =
            ExpressionBuilder.BuildExpressionForPathInExpression([2, 0], Expression.EnvironmentInstance);
        var second =
            ExpressionBuilder.BuildExpressionForPathInExpression([2, 1], Expression.EnvironmentInstance);
        var count = EnvironmentPath(1);
        var nextEnvironment =
            Expression.ListInst(
                [
                EnvironmentPath(0),
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.ListInst(
                        [
                        count,
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(-1)),
                        ])),
                Expression.ListInst([second, first]),
                ]);
        var expression =
            Expression.ConditionalInst(
                condition:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst(
                        [
                        count,
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(0)),
                        ])),
                falseBranch:
                new Expression.Eval(
                    encoded: EnvironmentPath(0),
                    environment:
                    new Expression.Eval(
                        encoded:
                        Expression.LitralInst(
                            ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                        environment: nextEnvironment)),
                trueBranch: Expression.ListInst([first, second]));
        var left = IntegerEncoding.EncodeSignedInteger(11);
        var right = IntegerEncoding.EncodeSignedInteger(13);
        var environment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(expression),
                IntegerEncoding.EncodeSignedInteger(3),
                PineValue.List([left, right]),
                ]);
        var expected = PineValue.List([right, left]);

        new DirectInterpreter(new(), evalCache: null)
            .EvaluateExpressionDefault(expression, environment).Should().Be(expected);

        var report =
            Evaluate(expression, environment, enableTailRecursionOptimization)
            .Extract(error => throw new InvalidOperationException(EvaluationError.RenderDisplayString(error)));

        report.ReturnValue.Evaluate().Should().Be(expected);
        report.Counters.LoopIterationCount.Should().Be(enableTailRecursionOptimization ? 3 : 0);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Guarded_eval_does_not_skip_failing_unused_environment_component(
        bool enableTailRecursionOptimization)
    {
        var failure = StringEncoding.ValueFromString("unused environment component failed");
        var expression =
            new Expression.Eval(
                encoded: EnvironmentPath(0),
                environment:
                Expression.ListInst(
                    [
                    EnvironmentPath(0),
                    new Expression.Eval(
                        encoded: Expression.LitralInst(failure),
                        environment: EnvironmentPath(0)),
                    ]));
        var environment = PineValue.List([ExpressionEncoding.EncodeExpressionAsValue(expression)]);

        FluentActions.Invoking(
            () => new DirectInterpreter(new(), evalCache: null)
                .EvaluateExpressionDefault(expression, environment))
            .Should().Throw<ParseExpressionException>().Which
            .Message.Should().Contain("unused environment component failed");

        var error =
            Evaluate(expression, environment, enableTailRecursionOptimization)
            .IsErrOrNull();

        error.Should().NotBeNull();
        error!.Reason.Should().BeOfType<EvaluationErrorReason.ParseExpressionFailed>()
            .Which.ExpressionValue.Should().Be(failure);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Safe_guarded_eval_keeps_loop_and_normal_invocation_paths(
        bool enableTailRecursionOptimization,
        bool targetIsRoot)
    {
        var count = EnvironmentPath(1);
        var expression =
            Expression.ConditionalInst(
                condition:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst(
                        [
                        count,
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(0)),
                        ])),
                falseBranch:
                new Expression.Eval(
                    encoded: EnvironmentPath(0),
                    environment:
                    Expression.ListInst(
                        [
                        EnvironmentPath(0),
                        Expression.BuiltinInst(
                            nameof(BuiltinFunction.int_add),
                            Expression.ListInst(
                                [
                                count,
                                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(-1)),
                                ])),
                        ])),
                trueBranch: count);
        var environment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(
                    targetIsRoot ? expression : Expression.EnvironmentInstance),
                IntegerEncoding.EncodeSignedInteger(3),
                ]);
        var expected =
            targetIsRoot
            ? IntegerEncoding.EncodeSignedInteger(0)
            : PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                IntegerEncoding.EncodeSignedInteger(2),
                ]);

        new DirectInterpreter(new(), evalCache: null)
            .EvaluateExpressionDefault(expression, environment).Should().Be(expected);

        var compilation =
            ExpressionCompilation.CompileExpression(
                expression,
                specializations: [],
                parseCache: new(),
                disableReduction: true,
                enableTailRecursionOptimization: enableTailRecursionOptimization,
                skipInlining: (_, _) => false);

        compilation.Generic.Instructions.Any(
            instruction => instruction.Kind == StackInstructionKind.Jump_Const && instruction.JumpOffset < 0)
            .Should().Be(enableTailRecursionOptimization);

        var report =
            Evaluate(expression, environment, enableTailRecursionOptimization)
            .Extract(error => throw new InvalidOperationException(EvaluationError.RenderDisplayString(error)));

        report.ReturnValue.Evaluate().Should().Be(expected);
        report.Counters.LoopIterationCount.Should().Be(
            enableTailRecursionOptimization && targetIsRoot ? 3 : 0);
        if (targetIsRoot)
        {
            report.Counters.InvocationCount.Should().Be(enableTailRecursionOptimization ? 0 : 3);
        }
        else
        {
            report.Counters.InvocationCount.Should().BeGreaterThan(0);
        }
    }

    private static Expression FailingOperand(PineValue invalidEncoding) =>
        new Expression.Eval(
            encoded: Expression.LitralInst(invalidEncoding),
            environment: Expression.EnvironmentInstance);

    private static Expression EnvironmentPath(int index) =>
        ExpressionBuilder.BuildExpressionForPathInExpression([index], Expression.EnvironmentInstance);

    private static Result<EvaluationError, EvaluationReport> Evaluate(
        Expression expression,
        PineValue environment,
        bool enableTailRecursionOptimization) =>
        IntermediatePineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: enableTailRecursionOptimization,
            parseCache: null,
            precompiledLeaves: null,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null)
        .EvaluateExpressionOnCustomStack(
            expression,
            environment,
            new IntermediatePineVM.EvaluationConfig(
                InvocationCountLimit: 32,
                LoopIterationCountLimit: 32,
                StackDepthLimit: 32));
}
