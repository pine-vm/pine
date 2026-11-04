using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class RuntimeErrorParityTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Record_runtime_failures_share_interpreter_diagnostics(bool update, bool missingField)
    {
        var record =
            missingField
            ?
            ElmValueEncoding.ElmRecordAsPineValue([("present", IntegerEncoding.EncodeSignedInteger(1))])
            :
            IntegerEncoding.EncodeSignedInteger(1234);

        var field = StringEncoding.ValueFromString("missing");

        var argument =
            update
            ?
            PineValue.List([PineValue.List([field, IntegerEncoding.EncodeSignedInteger(2)])])
            :
            field;

        var environment = PineValue.List([record, argument]);

        var function =
            update
            ?
            RecordRuntime.PineFunctionForRecordUpdateAsValue
            :
            RecordRuntime.PineFunctionForRecordAccessAsValue;

        var expression =
            ExpressionEncoding.ParseExpressionFromValue(function).Extract(error => throw new Exception(error));

        var expected =
            update
            ?
            missingField
            ?
            ElmRuntimeErrorMessages.RecordUpdateFieldNotFound("missing")
            :
            ElmRuntimeErrorMessages.RecordUpdateOnNonRecord
            :
            missingField
            ?
            ElmRuntimeErrorMessages.RecordFieldNotFound("missing")
            :
            ElmRuntimeErrorMessages.RecordAccessOnNonRecord;

        foreach (var vm in new IPineVM[]
        {
            DirectInterpreter.WithoutEvalCaching(new PineVMParseCache()),
            ElmCompilerTestHelper.PineVMForProfiling(_ => { }),
        })
        {
            Failure(() => vm.EvaluateExpression(expression, environment)).Should().Contain(expected);
        }

        var leaf =
            update
            ?
            CoreRecordPrecompiledLeaves.RecordUpdateLeafDelegate(PineValueInProcess.Create(environment))
            :
            CoreRecordPrecompiledLeaves.RecordAccessLeafDelegate(PineValueInProcess.Create(environment));

        leaf.Should().BeNull("invalid inputs must fall through to the diagnostic-producing runtime, not succeed");

        var source =
            missingField
            ?
            update ? "let r = { present = 1 } in { r | missing = 2 }" : "{ present = 1 }.missing"
            :
            update ? "let r = 1234 in { r | missing = 2 }" : "let r = 1234 in r.missing";

        ElmInterpreterEnvironment.Create(Array.Empty<string>())
            .Extract(error => throw new Exception(error)).Evaluate(source)
            .IsErrOrNull()!.Message.Should().Contain(expected);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void Unmatched_compiled_case_fails_instead_of_executing_the_last_arm(
        bool disableInlining,
        bool largeScrutinee)
    {
        var caseSource =
            largeScrutinee
            ?
            "case [ " + string.Join(", ", Enumerable.Repeat("n", 401)) +
            " ] of\n        [] -> 10\n        [ one ] -> 20"
            :
            "case n of\n        0 -> 10\n        1 -> 20";

        var compiled =
            ElmCompilerTestHelper.CompileElmModules(
                [
                $$"""
                module Test exposing (..)
                f n =
                    {{caseSource}}
                """
                ],
                disableInlining).parsedEnv;

        var function =
            compiled.Modules.Single(module => module.moduleName is "Test").moduleContent.FunctionDeclarations["f"];

        foreach (var vm in new IPineVM[]
        {
            DirectInterpreter.WithoutEvalCaching(new PineVMParseCache()),
            ElmCompilerTestHelper.PineVMForProfiling(_ => { }),
        })
        {
            var record = FunctionRecord.ParseFunctionRecordTagged(function, new PineVMParseCache()).IsOkOrNull();

            Failure(
                () =>
                record is null
                ?
                vm.EvaluateExpression(
                    new Expression.Eval(Expression.LitralInst(function), Expression.EnvironmentInstance),
                    IntegerEncoding.EncodeSignedInteger(3))
                :
                ElmInteractiveEnvironment.ApplyFunction(
                    vm,
                    record,
                    [
                    IntegerEncoding.EncodeSignedInteger(3)
                    ]))
                .Should().Contain(ElmRuntimeErrorMessages.CaseNoMatch);
        }
    }

    [Theory]
    [InlineData("let r = 1234 in r.missing", ElmRuntimeErrorMessages.RecordAccessOnNonRecord)]
    [InlineData("let r = 1234 in { r | missing = 2 }", ElmRuntimeErrorMessages.RecordUpdateOnNonRecord)]
    [InlineData("{ present = 1 }.missing", "Record does not contain a field named 'missing'.")]
    [InlineData("let r = { present = 1 } in { r | missing = 2 }", "Cannot update field \"missing\" because it does not exist on the record.")]
    public void Complete_compiler_route_preserves_scenario_error_text(string source, string expected)
    {
        foreach (var disableInlining in new[] { false, true })
        {
            Action evaluate =
                () => ElmCompilerTestHelper.CompileElmModules(
                    ["module Test exposing (..)\nvalue =\n    " + source],
                    disableInlining);

            evaluate.Should().Throw<Exception>().Which.ToString().Should().Contain(expected);
        }
    }

    private static string Failure(Func<Result<string, PineValue>> evaluate)
    {
        try
        {
            return evaluate().IsErrOrNull() ?? throw new Exception("Expected an evaluation failure.");
        }
        catch (ParseExpressionException exception)
        {
            return exception.Message;
        }
    }
}
