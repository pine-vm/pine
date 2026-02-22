using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class RecordAccessTests
{
    [Fact]
    public void Record_access_function_applied_directly()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : { name : String, age : Int } -> String
            alfa record =
                record.name

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
            var recordArg = ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmRecord(
                    [("age", new ElmValue.ElmInteger(25)),
                     ("name", ElmValue.StringInstance("Alice"))]));

            var (applyRunResult, _) = invokeFunction([recordArg]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(ElmValue.StringInstance("Alice"));
        }

        {
            var recordArg = ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmRecord(
                    [("age", new ElmValue.ElmInteger(30)),
                     ("name", ElmValue.StringInstance("Bob"))]));

            var (applyRunResult, _) = invokeFunction([recordArg]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(ElmValue.StringInstance("Bob"));
        }
    }

    [Fact]
    public void Record_access_function_passed_to_higher_order_function()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            getField : (a -> b) -> a -> b
            getField accessor record =
                accessor record


            alfa : { name : String, age : Int } -> Int
            alfa record =
                getField (\r -> r.age) record

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
            var recordArg = ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmRecord(
                    [("age", new ElmValue.ElmInteger(25)),
                     ("name", ElmValue.StringInstance("Alice"))]));

            var (applyRunResult, _) = invokeFunction([recordArg]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(new ElmValue.ElmInteger(25));
        }
    }

    [Fact]
    public void Record_access_function_used_with_custom_map()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            myMap : (a -> b) -> List a -> List b
            myMap f list =
                case list of
                    [] ->
                        []

                    x :: xs ->
                        f x :: myMap f xs


            alfa : List { name : String } -> List String
            alfa records =
                myMap (\r -> r.name) records

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
            var listArg = ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmList(
                    [new ElmValue.ElmRecord([("name", ElmValue.StringInstance("Alice"))]),
                     new ElmValue.ElmRecord([("name", ElmValue.StringInstance("Bob"))]),
                     new ElmValue.ElmRecord([("name", ElmValue.StringInstance("Carol"))])]));

            var (applyRunResult, _) = invokeFunction([listArg]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(
                new ElmValue.ElmList(
                    [ElmValue.StringInstance("Alice"),
                     ElmValue.StringInstance("Bob"),
                     ElmValue.StringInstance("Carol")]));
        }
    }

    [Fact]
    public void Record_access_function_stored_in_let_binding()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : { name : String, age : Int } -> Int
            alfa record =
                let
                    getAge r =
                        r.age
                in
                getAge record

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
            var recordArg = ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmRecord(
                    [("age", new ElmValue.ElmInteger(30)),
                     ("name", ElmValue.StringInstance("Alice"))]));

            var (applyRunResult, _) = invokeFunction([recordArg]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(new ElmValue.ElmInteger(30));
        }
    }

    [Fact]
    public void Record_access_function_returned_from_function()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            nameAccessor : { name : String } -> String
            nameAccessor record =
                record.name


            alfa : { name : String } -> String
            alfa record =
                nameAccessor record

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
            var recordArg = ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmRecord(
                    [("name", ElmValue.StringInstance("Hello"))]));

            var (applyRunResult, _) = invokeFunction([recordArg]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(ElmValue.StringInstance("Hello"));
        }
    }

    [Fact]
    public void Record_access_function_in_pipeline()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : { x : Int, y : Int } -> Int
            alfa record =
                record.x

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
            var recordArg = ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmRecord(
                    [("x", new ElmValue.ElmInteger(42)),
                     ("y", new ElmValue.ElmInteger(7))]));

            var (applyRunResult, _) = invokeFunction([recordArg]);

            var resultValue = applyRunResult.ReturnValue.Evaluate();

            var resultElm =
                ElmValueEncoding.PineValueAsElmValue(resultValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result: " + err));

            resultElm.Should().Be(new ElmValue.ElmInteger(42));
        }
    }
}
