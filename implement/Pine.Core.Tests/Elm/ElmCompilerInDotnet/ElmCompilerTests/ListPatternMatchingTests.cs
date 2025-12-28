using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class ListPatternMatchingTests
{
    [Fact]
    public void Case_of_list_empty_or_not()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl : List a -> String
            decl lst =
                case lst of
                    [] ->
                        "empty"

                    _ ->
                        "not empty"

            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: false,
                includeDeclaration: qualifiedName => qualifiedName.FullName is "Test.decl",
                parseCache: parseCache);

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
            return applyRunResult;
        }

        {
            var emptyListResult =
                ApplyForArgument(PineValue.List([]));

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(emptyListResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            resultAsElmExpr.expressionString.Should().Be("\"empty\"");
        }

        {
            var nonEmptyListResult =
                ApplyForArgument(
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(1),
                            IntegerEncoding.EncodeSignedInteger(2),
                        ]));

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(nonEmptyListResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            resultAsElmExpr.expressionString.Should().Be("\"not empty\"");
        }
    }

    [Fact]
    public void Case_of_list_matching_const_integer_patterns()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl : List Int -> Int
            decl lst =
                case lst of
                    [] ->
                        -1

                    [ 0 ] ->
                        100

                    0 :: _ ->
                        200

                    [ 13, 42 ] ->
                        300

                    [ 13, 42 ] :: rest ->
                        Pine_builtin.int_add
                            [ 400
                            , Pine_builtin.length rest
                            ]

                    _ ->
                        500
            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: false,
                includeDeclaration: qualifiedName => qualifiedName.FullName is "Test.decl",
                parseCache: parseCache);

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
            return applyRunResult;
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

            emptyListResult.Should().Be("-1");
        }

        {
            var singleZeroListResult =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(0),
                        ]));

            singleZeroListResult.Should().Be("100");
        }

        {
            var headZeroListResult =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(0),
                            IntegerEncoding.EncodeSignedInteger(5),
                            IntegerEncoding.EncodeSignedInteger(10),
                        ]));

            headZeroListResult.Should().Be("200");
        }

        {
            var list13_42Result =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(13),
                            IntegerEncoding.EncodeSignedInteger(42),
                        ]));

            list13_42Result.Should().Be("300");
        }

        {
            var listOfList13_42Result =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(13),
                                    IntegerEncoding.EncodeSignedInteger(42),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(1),
                                    IntegerEncoding.EncodeSignedInteger(2),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(3),
                                ]),
                        ]));

            listOfList13_42Result.Should().Be("402");
        }

        {
            var otherListResult =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(7),
                            IntegerEncoding.EncodeSignedInteger(8),
                        ]));
            otherListResult.Should().Be("500");
        }
    }

    [Fact]
    public void Case_of_list_matching_nested_integer_list_patterns()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl : List (List Int) -> Int
            decl lst =
                case lst of
                    [] ->
                        -1

                    [ [ 0 ] ] ->
                        100

                    [ [ 0 ] ] :: _ ->
                        200

                    [ [ 13, 42 ] ] ->
                        300

                    [ 13, 42 ] :: rest ->
                        Pine_builtin.int_add
                            [ 400
                            , Pine_builtin.length rest
                            ]

                    [ 13, otherInt ] :: rest ->
                        Pine_builtin.int_add
                            [ 500
                            , otherInt
                            , Pine_builtin.length rest
                            ]

                    (13 :: otherInt :: rest_inner) :: rest_outer ->
                        Pine_builtin.int_add
                            [ 600
                            , Pine_builtin.int_mul
                                [ 7
                                , otherInt
                                ]
                            , Pine_builtin.int_mul
                                [ 5
                                , Pine_builtin.length rest_inner
                                ]
                            , Pine_builtin.length rest_outer
                            ]

                    [ 17 ] :: (firstInner :: otherInt :: rest_inner) :: rest_outer ->
                        Pine_builtin.int_add
                            [ 700
                            , Pine_builtin.int_mul
                                [ 3
                                , firstInner
                                ]
                            , Pine_builtin.int_mul
                                [ 7
                                , otherInt
                                ]
                            , Pine_builtin.int_mul
                                [ 5
                                , Pine_builtin.length rest_inner
                                ]
                            , Pine_builtin.length rest_outer
                            ]

                    _ ->
                        -999
            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: false,
                includeDeclaration: qualifiedName => qualifiedName.FullName is "Test.decl",
                parseCache: parseCache);

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
            return applyRunResult;
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

        {
            var emptyListResult =
                ApplyForArgumentAsExpressionString(PineValue.List([]));

            emptyListResult.Should().Be("-1");
        }

        {
            var singleList13_42Result =
                ApplyForArgumentAsExpressionString(
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(13),
                                    IntegerEncoding.EncodeSignedInteger(42),
                                ]),
                        ]));

            singleList13_42Result.Should().Be("300");
        }

        {
            var listOfList13_42Result =
                ApplyForArgumentAsExpressionString(
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(13),
                                    IntegerEncoding.EncodeSignedInteger(42),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(1),
                                    IntegerEncoding.EncodeSignedInteger(2),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(3),
                                ]),
                        ]));

            listOfList13_42Result.Should().Be("402");
        }

        {
            var listOfList13_otherIntResult =
                ApplyForArgumentAsExpressionString(
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(13),
                                    IntegerEncoding.EncodeSignedInteger(7),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(1),
                                ]),
                        ]));

            // Pattern [ 13, otherInt ] :: rest matches with otherInt = 7, rest = [[1]]
            // Result: 500 + 7 + length([[1]]) = 500 + 7 + 1 = 508
            listOfList13_otherIntResult.Should().Be("508");
        }

        {
            var listOfList_pattern1_Result =
                ApplyForArgumentAsExpressionString(
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(13),
                                    IntegerEncoding.EncodeSignedInteger(4),
                                    IntegerEncoding.EncodeSignedInteger(5),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(1),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(2),
                                ]),
                        ]));

            listOfList_pattern1_Result.Should().Be("635");
        }

        {
            var singleListZeroResult =
                ApplyForArgumentAsExpressionString(
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(0),
                                ]),
                        ]));

            singleListZeroResult.Should().Be("100");
        }

        {
            var headListZeroResult =
                ApplyForArgumentAsExpressionString(
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(0),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(1),
                                ]),
                        ]));

            // Pattern [ [ 0 ] ] :: _ does NOT match [[0], [1]] because:
            // - Head is [0] which is List Int
            // - Pattern [ [ 0 ] ] expects head to be [[0]] which is List (List Int)
            // - Type mismatch, so pattern doesn't match
            // Falls through to wildcard pattern _ which returns -999
            headListZeroResult.Should().Be("-999");
        }

        {
            var listOfList_pattern2_Result =
                ApplyForArgumentAsExpressionString(
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(17),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(3),
                                    IntegerEncoding.EncodeSignedInteger(4),
                                    IntegerEncoding.EncodeSignedInteger(5),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(1),
                                ]),
                        ]));

            // 700 + (3 * 3) + (7 * 4) + (5 * 1) + 1 = 700 + 9 + 28 + 5 + 1 = 743
            listOfList_pattern2_Result.Should().Be("743");
        }

        {
            var notQuite700Result =
                ApplyForArgumentAsExpressionString(
                    // First element matches, but second element is a single-item list, not matching the pattern
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(17),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(3),
                                ]),
                        ]));

            notQuite700Result.Should().Be("-999");
        }

        {
            var notQuite700Result =
                ApplyForArgumentAsExpressionString(
                    // Second element matches, but first element does not match the pattern
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(16),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(3),
                                    IntegerEncoding.EncodeSignedInteger(4),
                                    IntegerEncoding.EncodeSignedInteger(5),
                                ]),
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(1),
                                ]),
                        ]));

            notQuite700Result.Should().Be("-999");
        }
    }

    [Fact]
    public void List_pattern_with_const_items_optimized()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl : List (List Int) -> Int
            decl lst =
                case lst of
                    [] ->
                        -1

                    [ [ 0 ] ] ->
                        100

                    [ [ 13, 42 ] ] ->
                        300

                    _ ->
                        -999
            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: false,
                includeDeclaration: qualifiedName => qualifiedName.FullName is "Test.decl",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.decl param_1_0 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.length
                            param_1_0
                        , 0
                        ]
                then
                    -1

                else if
                    Pine_kernel.equal
                        [ param_1_0
                        , [ [ 0 ] ]
                        ]
                then
                    100
            
                else if
                    Pine_kernel.equal
                        [ param_1_0
                        , [ [ 13, 42 ] ]
                        ]
                then
                    300

                else
                    -999

            """"
            .Trim());

    }
}
