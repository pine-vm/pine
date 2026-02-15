using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class AsPatternMatchingTests
{
    [Fact]
    public void Case_of_as_pattern_matching()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            decl m =
                case m of
                    Nothing ->
                        0

                    (Just x) as whole ->
                        Pine_builtin.int_add
                            [ x
                            , case whole of
                                Just _ -> 100
                                Nothing -> -100
                            ]

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
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        string ResultAsExpressionString(PineValue argument)
        {
            var applyResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        // Helper to create Nothing: ["Nothing", []] - tag with no arguments
        PineValue Nothing()
        {
            return ElmValueEncoding.TagAsPineValue("Nothing", []);
        }

        // Helper to create Just value: ["Just", [value]] - tag with one argument
        PineValue Just(PineValue value)
        {
            return ElmValueEncoding.TagAsPineValue("Just", [value]);
        }

        {
            var nothingResult =
                ResultAsExpressionString(Nothing());

            nothingResult.Should().Be("0");
        }

        {
            // Just 42 -> x = 42, whole = Just 42, result = 42 + 100 = 142
            var just42Result =
                ResultAsExpressionString(Just(IntegerEncoding.EncodeSignedInteger(42)));

            just42Result.Should().Be("142");
        }

        {
            // Just 7 -> x = 7, whole = Just 7, result = 7 + 100 = 107
            var just7Result =
                ResultAsExpressionString(Just(IntegerEncoding.EncodeSignedInteger(7)));

            just7Result.Should().Be("107");
        }
    }

    [Fact]
    public void Case_of_as_pattern_with_list()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl lst =
                case lst of
                    [] ->
                        0

                    (head :: tail) as whole ->
                        Pine_builtin.int_add
                            [ head
                            , Pine_builtin.length tail
                            , Pine_builtin.length whole
                            ]

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
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        string ResultAsExpressionString(PineValue argument)
        {
            var applyResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        {
            var emptyListResult =
                ResultAsExpressionString(PineValue.List([]));

            emptyListResult.Should().Be("0");
        }

        {
            // [5, 1, 2] -> head = 5, tail = [1, 2], whole = [5, 1, 2]
            // result = 5 + 2 + 3 = 10
            var threeElementListResult =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(5),
                        IntegerEncoding.EncodeSignedInteger(1),
                        IntegerEncoding.EncodeSignedInteger(2),
                        ]));

            threeElementListResult.Should().Be("10");
        }

        {
            // [10] -> head = 10, tail = [], whole = [10]
            // result = 10 + 0 + 1 = 11
            var singleElementListResult =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(10),
                        ]));

            singleElementListResult.Should().Be("11");
        }
    }

    [Fact]
    public void Case_of_as_pattern_with_tuple()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl : ( Int, Int ) -> Int
            decl tuple =
                case tuple of
                    ( 0, _ ) ->
                        0

                    ( x, y ) as whole ->
                        Pine_builtin.int_add
                            [ x
                            , y
                            , case whole of
                                ( a, b ) -> Pine_builtin.int_mul [ a, b ]
                            ]

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
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        string ResultAsExpressionString(PineValue argument)
        {
            var applyResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        {
            // (0, 5) -> 0 (first pattern matches)
            var zeroResult =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(0),
                        IntegerEncoding.EncodeSignedInteger(5),
                        ]));

            zeroResult.Should().Be("0");
        }

        {
            // (3, 4) -> x = 3, y = 4, whole = (3, 4)
            // result = 3 + 4 + (3 * 4) = 3 + 4 + 12 = 19
            var tupleResult =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(3),
                        IntegerEncoding.EncodeSignedInteger(4),
                        ]));

            tupleResult.Should().Be("19");
        }
    }
}
