using AwesomeAssertions;
using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class DirectTailLoopCompilationTests
{
    [Fact]
    public void Direct_tail_recursion_compiles_to_backward_jump()
    {
        var expression = BuildCountdownExpression();
        var expressionValue = ExpressionEncoding.EncodeExpressionAsValue(expression);

        var compilation =
            ExpressionCompilation.CompileExpression(
                expression,
                specializations: [],
                parseCache: new(),
                disableReduction: true,
                enableTailRecursionOptimization: true,
                skipInlining: (_, _) => false);

        compilation.Generic.Instructions
            .Any(
            instruction =>
            instruction.Kind == StackInstructionKind.Jump_Const &&
            instruction.JumpOffset < 0)
            .Should().BeTrue();

        var report =
            CreateVM(reportFunctionApplication: null)
            .EvaluateExpressionOnCustomStack(
                expression,
                PineValue.List(
                    [
                    expressionValue,
                    IntegerEncoding.EncodeSignedInteger(5),
                    ]),
                new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null))
            .IsOkOrNull();

        report.Should().NotBeNull();
        report!.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(0));
        report.Counters.InvocationCount.Should().Be(0);
        report.Counters.LoopIterationCount.Should().Be(5);

        var reportWithoutTailLoop =
            CreateVM(enableTailRecursionOptimization: false)
            .EvaluateExpressionOnCustomStack(
                expression,
                PineValue.List(
                    [
                    expressionValue,
                    IntegerEncoding.EncodeSignedInteger(5),
                    ]),
                new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null))
            .IsOkOrNull();

        reportWithoutTailLoop.Should().NotBeNull();
        reportWithoutTailLoop!.ReturnValue.Evaluate().Should().Be(report.ReturnValue.Evaluate());
        reportWithoutTailLoop.Counters.InvocationCount.Should().BeGreaterThan(report.Counters.InvocationCount);
        reportWithoutTailLoop.Counters.LoopIterationCount.Should().Be(0);
    }

    [Fact]
    public void Tail_call_to_different_expression_uses_normal_invocation()
    {
        var expression = BuildCountdownExpression();

        var result =
            CreateVM(reportFunctionApplication: null)
            .EvaluateExpression(
                expression,
                PineValue.List(
                    [
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                    IntegerEncoding.EncodeSignedInteger(1),
                    ]));

        result.IsOkOrNull().Should().Be(
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance),
                IntegerEncoding.EncodeSignedInteger(0),
                ]));
    }

    [Fact]
    public void Structurally_equal_call_is_lowered_only_in_tail_position()
    {
        var self = EnvironmentPath([0]);
        var count = EnvironmentPath([1]);
        var useTailCall = EnvironmentPath([2]);

        var recursiveCall =
            new Expression.Eval(
                encoded: self,
                environment:
                Expression.ListInst(
                    [
                    self,
                    Expression.BuiltinInst(
                        nameof(BuiltinFunction.int_add),
                        Expression.ListInst(
                            [
                            count,
                            Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(-1)),
                            ])),
                    useTailCall,
                    ]));

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
                Expression.ConditionalInst(
                    condition: useTailCall,
                    falseBranch:
                    Expression.BuiltinInst(
                        nameof(BuiltinFunction.int_add),
                        Expression.ListInst(
                            [
                            recursiveCall,
                            Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(1)),
                            ])),
                    trueBranch: recursiveCall),
                trueBranch: count);

        var compilation =
            ExpressionCompilation.CompileExpression(
                expression,
                specializations: [],
                parseCache: new(),
                disableReduction: true,
                enableTailRecursionOptimization: true,
                skipInlining: (_, _) => false);

        compilation.Generic.Instructions.Count(
            instruction =>
            instruction.Kind == StackInstructionKind.Jump_Const &&
            instruction.JumpOffset < 0)
            .Should().Be(1);

        compilation.Generic.Instructions.Count(
            instruction => instruction.Kind == StackInstructionKind.Eval_Binary)
            .Should().Be(2, "the non-tail occurrence and guarded fallback remain normal invocations");

        var environment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(expression),
                IntegerEncoding.EncodeSignedInteger(1),
                PineKernelValues.FalseValue,
                ]);

        Evaluate(CreateVM(true), expression, environment)
            .Should().Be(Evaluate(CreateVM(false), expression, environment));
    }

    [Fact]
    public void Tail_loop_carries_split_locals_without_clobbering()
    {
        var self = EnvironmentPath([0]);
        var left = EnvironmentPath([1]);
        var right = EnvironmentPath([2]);
        var count = EnvironmentPath([3]);

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
                    encoded: self,
                    environment:
                    Expression.ListInst(
                        [
                        self,
                        right,
                        Expression.BuiltinInst(
                            nameof(BuiltinFunction.int_add),
                            Expression.ListInst([left, right])),
                        Expression.BuiltinInst(
                            nameof(BuiltinFunction.int_add),
                            Expression.ListInst(
                                [
                                count,
                                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(-1)),
                                ])),
                        ])),
                trueBranch: Expression.ListInst([left, right]));

        var compilation =
            ExpressionCompilation.CompileExpression(
                expression,
                specializations: [],
                parseCache: new(),
                disableReduction: true,
                enableTailRecursionOptimization: true,
                skipInlining: (_, _) => false);

        compilation.Generic.Parameters.ParamsPaths
            .Should().BeEquivalentTo(
            ImmutableArray.Create<IReadOnlyList<int>>(
                [0],
                [1],
                [2],
                [3]),
            options => options.WithStrictOrdering());

        var environment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(expression),
                IntegerEncoding.EncodeSignedInteger(1),
                IntegerEncoding.EncodeSignedInteger(2),
                IntegerEncoding.EncodeSignedInteger(2),
                ]);

        Evaluate(CreateVM(true), expression, environment)
            .Should().Be(
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(3),
                IntegerEncoding.EncodeSignedInteger(5),
                ]));
    }

    [Fact]
    public void Tail_loop_can_swap_split_locals()
    {
        var self = EnvironmentPath([0]);
        var left = EnvironmentPath([1]);
        var right = EnvironmentPath([2]);
        var count = EnvironmentPath([3]);

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
                    encoded: self,
                    environment:
                    Expression.ListInst(
                        [
                        self,
                        right,
                        left,
                        Expression.BuiltinInst(
                            nameof(BuiltinFunction.int_add),
                            Expression.ListInst(
                                [
                                count,
                                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(-1)),
                                ])),
                        ])),
                trueBranch: Expression.ListInst([left, right]));

        var environment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(expression),
                IntegerEncoding.EncodeSignedInteger(1),
                IntegerEncoding.EncodeSignedInteger(2),
                IntegerEncoding.EncodeSignedInteger(1),
                ]);

        Evaluate(CreateVM(true), expression, environment)
            .Should().Be(
            PineValue.List(
                [
                IntegerEncoding.EncodeSignedInteger(2),
                IntegerEncoding.EncodeSignedInteger(1),
                ]));
    }

    [Fact]
    public void Tail_loop_reuses_same_iteration_environment_dependent_cse()
    {
        var self = EnvironmentPath([0]);
        var count = EnvironmentPath([1]);

        var decrementedCount =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst(
                    [
                    count,
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(-1)),
                    ]));

        var expression =
            Expression.ConditionalInst(
                condition:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst(
                        [
                        decrementedCount,
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(0)),
                        ])),
                falseBranch:
                new Expression.Eval(
                    encoded: self,
                    environment: Expression.ListInst([self, decrementedCount])),
                trueBranch: decrementedCount);

        var compilation =
            ExpressionCompilation.CompileExpression(
                expression,
                specializations: [],
                parseCache: new(),
                disableReduction: true,
                enableTailRecursionOptimization: true,
                skipInlining: (_, _) => false);

        compilation.Generic.Instructions.Count(
            instruction => instruction.Kind == StackInstructionKind.Int_Add_Const)
            .Should().Be(1);

        Evaluate(
            CreateVM(true),
            expression,
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(expression),
                IntegerEncoding.EncodeSignedInteger(3),
                ]))
            .Should().Be(IntegerEncoding.EncodeSignedInteger(0));
    }

    private static Expression BuildCountdownExpression()
    {
        var self = EnvironmentPath([0]);
        var count = EnvironmentPath([1]);

        return
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
                    encoded: self,
                    environment:
                    Expression.ListInst(
                        [
                        self,
                        Expression.BuiltinInst(
                            nameof(BuiltinFunction.int_add),
                            Expression.ListInst(
                                [
                                count,
                                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(-1)),
                                ])),
                        ])),
                trueBranch: count);
    }

    private static Expression EnvironmentPath(System.ReadOnlySpan<int> path) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            path,
            Expression.EnvironmentInstance);

    private static PineValue Evaluate(
        Core.Interpreter.IntermediateVM.PineVM vm,
        Expression expression,
        PineValue environment) =>
        vm.EvaluateExpression(expression, environment)
        .Extract(error => throw new System.InvalidOperationException(error.ToString()));

    private static Core.Interpreter.IntermediateVM.PineVM CreateVM(
        bool enableTailRecursionOptimization = true,
        System.Action<EvaluationReport>? reportFunctionApplication = null) =>
        Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: reportFunctionApplication,
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
            cacheFileStore: null);
}
