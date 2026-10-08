using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using VM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class EvaluationInstrumentationTests
{
    [Fact]
    public void Backward_jump_reporting_exposes_lazy_current_frame_first_stack_and_locals()
    {
        var child = Expression.EnvironmentInstance;

        var root =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(child)),
                Expression.EnvironmentInstance);

        var jumps = 0;
        var stops = 0;

        var vm =
            Create(
                root,
                child,
                (in EvaluationEvent item) =>
                {
                    if (item.Kind is EvaluationEventKind.BackwardJump)
                    {
                        jumps++;
                        var sequence = item.LoadStackTrace();
                        sequence.Should().NotBeAssignableTo<IReadOnlyList<EvaluationStackTraceFrame>>();
                        var current = sequence.Take(1).Single();
                        current.Expression.Should().Be(child);
                        current.LoadLocals.Should().NotBeNull();
                        var locals = current.LoadLocals!();
                        locals.Should().HaveCount(2);
                        locals[0].Should().NotBeNull();
                        locals[1].Should().BeNull();
                        item.LoadCounters().LoopIterationCount.Should().Be(jumps);
                    }

                    if (item.Kind is EvaluationEventKind.EvaluationStopped)
                    {
                        stops++;
                        item.LoadStackTrace().Select(frame => frame.Expression).Should().Equal(child, root);
                    }
                });

        var error =
            vm.EvaluateExpressionOnCustomStack(root, PineValue.EmptyList,
            new VM.EvaluationConfig(null, 3, null)).IsErrOrNull()!;

        error.Reason.Should().BeOfType<EvaluationErrorReason.QuotaExhausted>();
        jumps.Should().Be(4);
        stops.Should().Be(1);
    }

    [Fact]
    public void Loop_budget_stops_infinite_loop_and_preserves_instruction_statistics_without_instruction_callbacks()
    {
        var child = Expression.EnvironmentInstance;

        var root =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(child)),
                Expression.EnvironmentInstance);

        var vm = Create(root, child, null);

        var error =
            vm.EvaluateExpressionOnCustomStack(root, PineValue.EmptyList,
            new VM.EvaluationConfig(null, 3, null)).IsErrOrNull()!;

        error.Counters.InstructionCount.Should().BeGreaterThan(0);
        error.Counters.LoopIterationCount.Should().Be(4);
        error.Reason.Should().Be(new EvaluationErrorReason.QuotaExhausted(EvaluationQuotaKind.LoopIterationCount, 3));
        error.StackTrace.Count.Should().Be(2);
    }

    private static VM Create(Expression root, Expression child, ReportEvaluationEvent? report) =>
        VM.CreateCustom(
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
            reportEvaluationEvent: report,
            expressionCompilationOverrides: new Dictionary<Expression, ExpressionCompilation>
            {
                [root] =
                new(
                    new StackFrameInstructions(
                        StaticFunctionInterface.Generic,
                        [
                        StackInstruction.Local_Get(0), StackInstruction.Eval_Const(
                                                        ExpressionEncoding.EncodeExpressionAsValue(child)),
                        StackInstruction.Push_Literal(PineValue.EmptyList), StackInstruction.Pop, StackInstruction.Return
                        ],
                        null),
                    []),
                [child] =
                new(
                    new StackFrameInstructions(
                        StaticFunctionInterface.Generic,
                        [
                        StackInstruction.Push_Literal(PineValue.EmptyList),
                        StackInstruction.Pop,
                        StackInstruction.Jump_Unconditional(-2),
                        StackInstruction.Local_Get(1),
                        ],
                        null),
                    []),
            });
}
