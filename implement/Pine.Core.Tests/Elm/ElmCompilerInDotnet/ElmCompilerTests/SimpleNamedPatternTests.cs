using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleNamedPatternTests
{
    [Fact]
    public void Case_of_maybe_tuple_patterns()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            decl arg =
                case arg of
                    ( Nothing, Nothing ) ->
                        100

                    ( Nothing, Just right ) ->
                        Pine_builtin.int_add
                            [ 200
                            , right
                            ]

                    ( Just 17, Just 19 ) ->
                        300

                    ( Just 17, Nothing ) ->
                        400

                    ( Just 17, Just right ) ->
                        Pine_builtin.int_add
                            [ 500
                            , right
                            ]

                    _ ->
                        0

            """;

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

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var applyRunResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null);

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

        // Helper to create a tuple: [first, second]
        PineValue Tuple(PineValue first, PineValue second)
        {
            return PineValue.List([first, second]);
        }

        // Test case: ( Nothing, Nothing ) -> 100
        {
            var result =
                ApplyForArgumentAsExpressionString(Tuple(Nothing(), Nothing()));

            result.Should().Be("100");
        }

        // Test case: ( Nothing, Just right ) -> 200 + right
        // Test with Just 7
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    Tuple(Nothing(), Just(IntegerEncoding.EncodeSignedInteger(7))));

            // 200 + 7 = 207
            result.Should().Be("207");
        }

        // Test case: ( Nothing, Just right ) -> 200 + right
        // Test with Just 13
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    Tuple(Nothing(), Just(IntegerEncoding.EncodeSignedInteger(13))));

            // 200 + 13 = 213
            result.Should().Be("213");
        }

        // Test case: ( Just 17, Just 19 ) -> 300
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    Tuple(
                        Just(IntegerEncoding.EncodeSignedInteger(17)),
                        Just(IntegerEncoding.EncodeSignedInteger(19))));

            result.Should().Be("300");
        }

        // Test case: ( Just 17, Nothing ) -> 400
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    Tuple(
                        Just(IntegerEncoding.EncodeSignedInteger(17)),
                        Nothing()));

            result.Should().Be("400");
        }

        // Test case: ( Just 17, Just right ) -> 500 + right
        // Test with Just 23 (not 19, to avoid matching the specific pattern)
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    Tuple(
                        Just(IntegerEncoding.EncodeSignedInteger(17)),
                        Just(IntegerEncoding.EncodeSignedInteger(23))));

            // 500 + 23 = 523
            result.Should().Be("523");
        }

        // Test case: ( Just 17, Just right ) -> 500 + right
        // Test with Just 0
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    Tuple(
                        Just(IntegerEncoding.EncodeSignedInteger(17)),
                        Just(IntegerEncoding.EncodeSignedInteger(0))));

            // 500 + 0 = 500
            result.Should().Be("500");
        }

        // Test case: wildcard _ -> 0
        // Test with ( Just 5, Nothing ) - does not match any specific pattern
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    Tuple(
                        Just(IntegerEncoding.EncodeSignedInteger(5)),
                        Nothing()));

            result.Should().Be("0");
        }

        // Test case: wildcard _ -> 0
        // Test with ( Just 5, Just 10 ) - does not match any specific pattern
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    Tuple(
                        Just(IntegerEncoding.EncodeSignedInteger(5)),
                        Just(IntegerEncoding.EncodeSignedInteger(10))));

            result.Should().Be("0");
        }

        // Test case: wildcard _ -> 0
        // Test with ( Just 18, Just 19 ) - Just 18 does not match Just 17
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    Tuple(
                        Just(IntegerEncoding.EncodeSignedInteger(18)),
                        Just(IntegerEncoding.EncodeSignedInteger(19))));

            result.Should().Be("0");
        }
    }

    [Fact]
    public void Named_pattern_with_const_items_optimized()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            decl arg =
                case arg of
                    ( Nothing, Nothing ) ->
                        100

                    ( Nothing, Just 7 ) ->
                        200

                    ( Just 17, Just 19 ) ->
                        300

                    _ ->
                        0

            """;

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.FullName is "Test.decl",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.decl param_1_0 =
                if
                    Pine_builtin.equal
                        [ param_1_0
                        , [ Nothing, Nothing ]
                        ]
                then
                    100

                else if
                    Pine_builtin.equal
                        [ param_1_0
                        , [ Nothing, Just 7 ]
                        ]
                then
                    200

                else if
                    Pine_builtin.equal
                        [ param_1_0
                        , [ Just 17, Just 19 ]
                        ]
                then
                    300

                else
                    0

            """"
            .Trim());
    }

    [Fact]
    public void Case_of_bool_True_and_False_patterns()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl arg =
                case arg of
                    True ->
                        10

                    False ->
                        20

            """;

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

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            var result = applyRunResult.ReturnValue.Evaluate();

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(result, null, null);

            return
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)))
                .expressionString;
        }

        // True -> 10
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    KernelFunction.ValueFromBool(true));

            result.Should().Be("10");
        }

        // False -> 20
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    KernelFunction.ValueFromBool(false));

            result.Should().Be("20");
        }
    }

    [Fact]
    public void Case_of_nested_bool_in_constructor_pattern()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Wrapper
                = WrapBool Bool
                | WrapInt Int


            decl arg =
                case arg of
                    WrapBool True ->
                        10

                    WrapBool False ->
                        20

                    WrapInt n ->
                        Pine_builtin.int_add
                            [ 100
                            , n
                            ]

                    _ ->
                        0

            """;

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

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            var result = applyRunResult.ReturnValue.Evaluate();

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(result, null, null);

            return
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)))
                .expressionString;
        }

        // WrapBool True -> 10
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    ElmValueEncoding.TagAsPineValue("WrapBool", [KernelFunction.ValueFromBool(true)]));

            result.Should().Be("10");
        }

        // WrapBool False -> 20
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    ElmValueEncoding.TagAsPineValue("WrapBool", [KernelFunction.ValueFromBool(false)]));

            result.Should().Be("20");
        }

        // WrapInt 7 -> 107
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    ElmValueEncoding.TagAsPineValue("WrapInt", [IntegerEncoding.EncodeSignedInteger(7)]));

            result.Should().Be("107");
        }
    }

    [Fact]
    public void Case_of_bool_in_tuple_pattern()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl arg =
                case arg of
                    ( True, True ) ->
                        1

                    ( True, False ) ->
                        2

                    ( False, True ) ->
                        3

                    ( False, False ) ->
                        4

            """;

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

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            var result = applyRunResult.ReturnValue.Evaluate();

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(result, null, null);

            return
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)))
                .expressionString;
        }

        PineValue BoolVal(bool b) => KernelFunction.ValueFromBool(b);

        // ( True, True ) -> 1
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    PineValue.List([BoolVal(true), BoolVal(true)]));

            result.Should().Be("1");
        }

        // ( True, False ) -> 2
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    PineValue.List([BoolVal(true), BoolVal(false)]));

            result.Should().Be("2");
        }

        // ( False, True ) -> 3
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    PineValue.List([BoolVal(false), BoolVal(true)]));

            result.Should().Be("3");
        }

        // ( False, False ) -> 4
        {
            var result =
                ApplyForArgumentAsExpressionString(
                    PineValue.List([BoolVal(false), BoolVal(false)]));

            result.Should().Be("4");
        }
    }
}
