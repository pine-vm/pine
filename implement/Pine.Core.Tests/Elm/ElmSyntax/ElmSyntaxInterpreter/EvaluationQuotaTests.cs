using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Internal;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Verifies that deterministic evaluation quotas stop non-terminating or excessively deep
/// interpreter workloads without attempting to classify them as infinite recursion.
/// </summary>
public class EvaluationQuotaTests
{
    private const string CountUpModule =
        """
        module Test exposing (..)


        countUp n =
            countUp (Pine_builtin.int_add [ n, 1 ])
        """;

    private static ElmInterpretationError InterpretAndGetError(
        string elmModuleText,
        ElmInterpreter.EvaluationConfig evaluationConfig)
    {
        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.InterpretAsElmValue(
                mainBody,
                declarations,
                evaluationConfig);

        return
            result.IsErrOrNull()
            ?? throw new System.Exception(
                "Expected error result, got " + result.GetType().FullName);
    }

    [Fact]
    public void Instruction_count_quota_stops_recursion_with_changing_arguments()
    {
        const int instructionCountLimit = 100;

        var elmModuleText =
            """
            module Test exposing (..)


            countUp n =
                countUp (Pine_builtin.int_add [ n, 1 ])


            main =
                countUp 0
            """;

        var error =
            InterpretAndGetError(
                elmModuleText,
                new ElmInterpreter.EvaluationConfig(
                    InstructionCountLimit: instructionCountLimit,
                    ContinuationDepthLimit: null));

        error.QuotaExceeded.Should().Be(
            new ElmInterpreter.EvaluationQuotaExceeded(
                ElmInterpreter.EvaluationQuotaKind.InstructionCount,
                Limit: instructionCountLimit,
                Observed: instructionCountLimit + 1));

        error.Message.Should().Be("Instruction count limit exceeded: 100");
        error.CallStack.Should().NotBeEmpty();
    }

    [Fact]
    public void Continuation_depth_quota_stops_deep_recursion()
    {
        const int continuationDepthLimit = 20;

        var elmModuleText =
            """
            module Test exposing (..)


            countUp n =
                countUp (Pine_builtin.int_add [ n, 1 ])


            main =
                countUp 0
            """;

        var error =
            InterpretAndGetError(
                elmModuleText,
                new ElmInterpreter.EvaluationConfig(
                    InstructionCountLimit: null,
                    ContinuationDepthLimit: continuationDepthLimit));

        error.QuotaExceeded.Should().NotBeNull();
        error.QuotaExceeded!.QuotaKind.Should().Be(
            ElmInterpreter.EvaluationQuotaKind.ContinuationDepth);
        error.QuotaExceeded.Limit.Should().Be(continuationDepthLimit);
        error.QuotaExceeded.Observed.Should().BeGreaterThan(continuationDepthLimit);

        error.Message.Should().Be("Continuation depth limit exceeded: 20");
        error.CallStack.Should().NotBeEmpty();
    }

    [Fact]
    public void Finite_evaluation_completes_within_both_quotas()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            add a b =
                Pine_builtin.int_add [ a, b ]


            main =
                add 20 22
            """;

        var declarations = InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.InterpretAsElmValue(
                mainBody,
                declarations,
                new ElmInterpreter.EvaluationConfig(
                    InstructionCountLimit: 100,
                    ContinuationDepthLimit: 20));

        result
            .Extract(error => throw new System.Exception(error.ToString()))
            .Should().Be(ElmValue.Integer(42));
    }

    [Fact]
    public void Default_configuration_has_finite_limits()
    {
        ElmInterpreter.EvaluationConfig.Default.InstructionCountLimit
            .Should().Be(1_000_000_000);

        ElmInterpreter.EvaluationConfig.Default.ContinuationDepthLimit
            .Should().Be(1_000_000);
    }

    [Fact]
    public void Prepared_direct_entry_point_honors_instruction_quota()
    {
        const int instructionCountLimit = 50;

        var prepared =
            ElmInterpreter.PrepareModules([CountUpModule])
            .Extract(error => throw new System.Exception(error));

        var result =
            ElmInterpreter.Interpret(
                DeclQualifiedName.Create(["Test"], "countUp"),
                [PineValueInProcess.CreateInteger(0)],
                prepared,
                new ElmInterpreter.EvaluationConfig(
                    InstructionCountLimit: instructionCountLimit,
                    ContinuationDepthLimit: null));

        var error =
            result.IsErrOrNull()
            ?? throw new System.Exception(
                "Expected error result, got " + result.GetType().FullName);

        error.QuotaExceeded.Should().Be(
            new ElmInterpreter.EvaluationQuotaExceeded(
                ElmInterpreter.EvaluationQuotaKind.InstructionCount,
                Limit: instructionCountLimit,
                Observed: instructionCountLimit + 1));
    }

    [Fact]
    public void Counter_entry_point_reports_work_through_quota_exhaustion()
    {
        const int instructionCountLimit = 50;

        var prepared =
            ElmInterpreter.PrepareModules([CountUpModule])
            .Extract(error => throw new System.Exception(error));

        var (result, counters) =
            ElmInterpreter.ParseAndInterpretWithCounters(
                "Test.countUp 0",
                prepared,
                new ElmInterpreter.EvaluationConfig(
                    InstructionCountLimit: instructionCountLimit,
                    ContinuationDepthLimit: null));

        result.IsErrOrNull()!.QuotaExceeded.Should().NotBeNull();
        counters.InstructionLoopCount.Should().Be(instructionCountLimit + 1);
    }
}
