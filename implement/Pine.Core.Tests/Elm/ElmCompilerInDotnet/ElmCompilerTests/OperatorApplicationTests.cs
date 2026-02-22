using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class OperatorApplicationTests
{
    [Fact]
    public void Cons_infix_prepend_to_list()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> List Int
            alfa x =
                x :: [ 2, 3 ]

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        {
            var (applyRunResult, _) = invokeFunction([ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmInteger(1))]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(
                new ElmValue.ElmList(
                    [new ElmValue.ElmInteger(1), new ElmValue.ElmInteger(2), new ElmValue.ElmInteger(3)]));
        }

        {
            var (applyRunResult, _) = invokeFunction([ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmInteger(99))]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(
                new ElmValue.ElmList(
                    [new ElmValue.ElmInteger(99), new ElmValue.ElmInteger(2), new ElmValue.ElmInteger(3)]));
        }
    }

    [Fact]
    public void Cons_infix_prepend_to_empty_list()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> List Int
            alfa x =
                x :: []

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        {
            var (applyRunResult, _) = invokeFunction([ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmInteger(7))]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(
                new ElmValue.ElmList([new ElmValue.ElmInteger(7)]));
        }
    }

    [Fact]
    public void Cons_infix_chain()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : List Int
            alfa =
                1 :: 2 :: 3 :: []

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        declParsed.ParameterCount.Should().Be(0);

        var evalResult =
            ElmCompilerTestHelper.EvaluateWithProfiling(declParsed.InnerFunction, PineValue.EmptyBlob);

        var resultValue = evalResult.evalReport.ReturnValue.Evaluate();

        var resultElm =
            ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
            .Extract(err => throw new Exception("Failed decoding result: " + err));

        resultElm.Should().Be(
            new ElmValue.ElmList(
                [new ElmValue.ElmInteger(1), new ElmValue.ElmInteger(2), new ElmValue.ElmInteger(3)]));
    }

    [Fact]
    public void Cons_prefix_function_prepend_to_list()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> List Int -> List Int
            alfa x xs =
                (::) x xs

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        {
            var headArg = ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmInteger(10));

            var tailArg = ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmList(
                    [new ElmValue.ElmInteger(20), new ElmValue.ElmInteger(30)]));

            var (applyRunResult, _) = invokeFunction([headArg, tailArg]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(
                new ElmValue.ElmList(
                    [new ElmValue.ElmInteger(10), new ElmValue.ElmInteger(20), new ElmValue.ElmInteger(30)]));
        }
    }

    [Fact]
    public void Cons_prefix_function_passed_as_value()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            applyFunc : (a -> List a -> List a) -> a -> List a -> List a
            applyFunc f head tail =
                f head tail


            alfa : Int -> List Int -> List Int
            alfa x xs =
                applyFunc (::) x xs

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        {
            var headArg = ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmInteger(1));

            var tailArg = ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmList(
                    [new ElmValue.ElmInteger(2), new ElmValue.ElmInteger(3)]));

            var (applyRunResult, _) = invokeFunction([headArg, tailArg]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(
                new ElmValue.ElmList(
                    [new ElmValue.ElmInteger(1), new ElmValue.ElmInteger(2), new ElmValue.ElmInteger(3)]));
        }
    }
}
