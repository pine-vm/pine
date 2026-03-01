using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleLetBlockTests
{
    [Fact]
    public void Let_block_reusing_single_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    alfa =
                        41
                in
                [ Pine_builtin.int_add
                    [ alfa
                    , arg
                    ]
                , Pine_builtin.int_mul
                    [ alfa
                    , arg
                    ]
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
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(1));

            resultExprString.Should().Be("[ 42, 41 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(2));

            resultExprString.Should().Be("[ 43, 82 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(-1));

            resultExprString.Should().Be("[ 40, -41 ]");
        }
    }

    [Fact]
    public void Consecutive_let_blocks()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    alfa =
                        19
                in
                let
                    beta =
                        Pine_builtin.int_add
                            [ alfa
                            , 1
                            ]
                in
                [ Pine_builtin.int_add
                    [ alfa
                    , arg
                    ]
                , Pine_builtin.int_mul
                    [ beta
                    , arg
                    ]
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
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(1));

            resultExprString.Should().Be("[ 20, 20 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(2));

            resultExprString.Should().Be("[ 21, 40 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(-1));

            resultExprString.Should().Be("[ 18, -20 ]");
        }
    }

    [Fact]
    public void Let_declarations_out_of_order()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    beta =
                        Pine_builtin.int_add
                            [ alfa
                            , 1
                            ]

                    alfa =
                        19
                in
                [ Pine_builtin.int_add
                    [ alfa
                    , arg
                    ]
                , Pine_builtin.int_mul
                    [ beta
                    , arg
                    ]
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
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(1));

            resultExprString.Should().Be("[ 20, 20 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(2));

            resultExprString.Should().Be("[ 21, 40 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(-1));

            resultExprString.Should().Be("[ 18, -20 ]");
        }
    }


    [Fact]
    public void Let_declaration_pattern_triple()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    ( alfa, gamma, beta ) =
                        arg
                in
                [ Pine_builtin.int_add
                    [ alfa
                    , 13
                    ]
                , Pine_builtin.int_mul
                    [ beta
                    , 17
                    ]
                , Pine_builtin.int_add
                    [ gamma
                    , 19
                    ]
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
            var resultExprString =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(1),
                        IntegerEncoding.EncodeSignedInteger(3),
                        IntegerEncoding.EncodeSignedInteger(5),
                        ]));

            resultExprString.Should().Be("[ 14, 85, 22 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(2),
                        IntegerEncoding.EncodeSignedInteger(4),
                        IntegerEncoding.EncodeSignedInteger(6),
                        ]));

            resultExprString.Should().Be("[ 15, 102, 23 ]");
        }
    }

    [Fact]
    public void Let_block_recursive_function()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    factorial n =
                        if Pine_builtin.int_is_sorted_asc [ n, 1 ] then
                            1

                        else
                            Pine_builtin.int_mul
                                [ factorial (Pine_builtin.int_add [ n, -1 ])
                                , n
                                ]
                in
                factorial arg

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
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(1));

            resultExprString.Should().Be("1");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(5));

            resultExprString.Should().Be("120");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(7));

            resultExprString.Should().Be("5040");
        }
    }

    [Fact]
    public void Let_block_recursive_function_with_outer_transformation()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    factorial n =
                        if Pine_builtin.int_is_sorted_asc [ n, 1 ] then
                            1

                        else
                            Pine_builtin.int_mul
                                [ factorial (Pine_builtin.int_add [ n, -1 ])
                                , n
                                ]
                in
                Pine_builtin.int_add
                    [ factorial arg
                    , 1000
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
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(1));

            resultExprString.Should().Be("1001");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(5));

            resultExprString.Should().Be("1120");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(7));

            resultExprString.Should().Be("6040");
        }
    }

    /// <summary>
    /// Verifies that a trivial let declaration (only a literal) is inlined,
    /// producing the same compiled code as the manually inlined version.
    /// </summary>
    [Fact]
    public void Let_block_trivial_literal_is_inlined()
    {
        var elmModuleWithLet =
            """"
            module TestA exposing (..)


            decl arg =
                let
                    alfa =
                        41
                in
                Pine_builtin.int_add
                    [ alfa
                    , arg
                    ]

            """";

        var elmModuleInlined =
            """"
            module TestB exposing (..)


            decl arg =
                Pine_builtin.int_add
                    [ 41
                    , arg
                    ]

            """";

        AssertLetBlockProducesSameCodeAsInlined(elmModuleWithLet, elmModuleInlined);
    }

    /// <summary>
    /// Verifies that a let declaration using only Pine_builtin functions is inlined,
    /// producing the same compiled code as the manually inlined version.
    /// (Pine_builtin calls don't count as function applications for the inlining exception.)
    /// </summary>
    [Fact]
    public void Let_block_pine_builtin_only_is_inlined()
    {
        var elmModuleWithLet =
            """"
            module TestA exposing (..)


            decl arg =
                let
                    alfa =
                        Pine_builtin.int_add
                            [ arg
                            , 1
                            ]
                in
                Pine_builtin.int_mul
                    [ alfa
                    , 3
                    ]

            """";

        var elmModuleInlined =
            """"
            module TestB exposing (..)


            decl arg =
                Pine_builtin.int_mul
                    [ Pine_builtin.int_add
                        [ arg
                        , 1
                        ]
                    , 3
                    ]

            """";

        AssertLetBlockProducesSameCodeAsInlined(elmModuleWithLet, elmModuleInlined);
    }

    /// <summary>
    /// Verifies that a let declaration containing a function application is inlined
    /// when it is only referenced once. Per the implementation guide, a declaration
    /// is only prevented from inlining if it contains a function application AND
    /// is referenced more than once.
    /// </summary>
    [Fact]
    public void Let_block_function_application_used_once_is_inlined()
    {
        var elmModuleWithLet =
            """"
            module TestA exposing (..)


            helper x =
                Pine_builtin.int_add [ x, 100 ]


            decl arg =
                let
                    alfa =
                        helper arg
                in
                Pine_builtin.int_mul
                    [ alfa
                    , 3
                    ]

            """";

        var elmModuleInlined =
            """"
            module TestB exposing (..)


            helper x =
                Pine_builtin.int_add [ x, 100 ]


            decl arg =
                Pine_builtin.int_mul
                    [ helper arg
                    , 3
                    ]

            """";

        AssertLetBlockProducesSameCodeAsInlined(elmModuleWithLet, elmModuleInlined);
    }

    /// <summary>
    /// Compiles two modules (one with let block, one manually inlined) and asserts
    /// that the compiled declaration value for 'decl' is identical in both cases.
    /// </summary>
    private static void AssertLetBlockProducesSameCodeAsInlined(
        string elmModuleWithLet,
        string elmModuleInlined)
    {
        var parsedEnvWithLet =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleWithLet],
                disableInlining: false);

        var parsedEnvInlined =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleInlined],
                disableInlining: false);

        var testModuleWithLet =
            parsedEnvWithLet.Modules.FirstOrDefault(c => c.moduleName is "TestA");

        var testModuleInlined =
            parsedEnvInlined.Modules.FirstOrDefault(c => c.moduleName is "TestB");

        var declValueWithLet =
            testModuleWithLet.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl").Value;

        var declValueInlined =
            testModuleInlined.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl").Value;

        declValueWithLet.Should().NotBeNull("decl should exist in TestA module");
        declValueInlined.Should().NotBeNull("decl should exist in TestB module");

        declValueWithLet.Should().Be(
            declValueInlined,
            "Let-block version should produce the same compiled code as the manually inlined version");
    }
}
