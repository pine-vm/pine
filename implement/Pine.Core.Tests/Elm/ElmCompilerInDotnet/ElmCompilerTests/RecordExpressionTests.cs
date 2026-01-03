using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class RecordExpressionTests
{
    [Fact]
    public void Module_level_declaration_record_value()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa =
                { x = 13, z = 17, y = 23 }

            """";

        var parseCache = new PineVMParseCache();

        // Use the simpler compile method that doesn't require static analysis
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        // Module-level declarations are compiled as zero-parameter functions
        // Parse and evaluate to get the actual value
        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing function: " + err));

        functionRecord.ParameterCount.Should().Be(0);

        var evalResult =
            ElmCompilerTestHelper.EvaluateWithProfiling(functionRecord.InnerFunction, PineValue.EmptyBlob);

        var actualValue =
            evalResult.evalReport.ReturnValue.Evaluate();

        actualValue.Should().Be(
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                ("x", IntegerEncoding.EncodeSignedInteger(13)),
                ("y", IntegerEncoding.EncodeSignedInteger(23)),
                ("z", IntegerEncoding.EncodeSignedInteger(17))
                ]));
    }

    [Fact]
    public void Module_level_function_returning_record_value()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa a b c =
                { x = a, z = c, y = b }

            """";

        var parseCache = new PineVMParseCache();

        // Use the simpler compile method that doesn't require static analysis
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        // Parsing and rendering not implemented yet, therefore no snapshot test here.


        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArguments(
            PineValue a,
            PineValue b,
            PineValue c)
        {
            var (applyRunResult, _) = invokeFunction([a, b, c]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        ElmValue ApplyForElmArguments(
            ElmValue a,
            ElmValue b,
            ElmValue c)
        {
            var pineValue =
                ApplyForArguments(
                    ElmValueEncoding.ElmValueAsPineValue(a),
                    ElmValueEncoding.ElmValueAsPineValue(b),
                    ElmValueEncoding.ElmValueAsPineValue(c));

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.Integer(41),
                    ElmValue.Integer(47),
                    ElmValue.Integer(49));

            resultValue.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.Integer(41)),
                    ("y", ElmValue.Integer(47)),
                    ("z", ElmValue.Integer(49))
                    ]));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.ElmFloat.Convert(1.3),
                    ElmValue.ElmFloat.Convert(2.7),
                    ElmValue.ElmFloat.Convert(0));

            resultValue.Should().Be(
                new ElmValue.ElmRecord(
                    [
                    ("x", ElmValue.ElmFloat.Convert(1.3)),
                    ("y", ElmValue.ElmFloat.Convert(2.7)),
                    ("z", ElmValue.ElmFloat.Convert(0))
                    ]));
        }
    }
}
