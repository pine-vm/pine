using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleRecursiveFunctionTests
{
    [Fact]
    public void Function_Fibonacci()
    {
        var elmModuleText =
            """"
            module Test exposing (..)

            fibonacci : Int -> Int
            fibonacci n =
                if Pine_kernel.int_is_sorted_asc [ n, 1 ] then
                    n

                else
                    Pine_kernel.int_add
                        [ fibonacci (Pine_kernel.int_add [ n, -2 ])
                        , fibonacci (Pine_kernel.int_add [ n, -1 ])
                        ]

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "fibonacci",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.fibonacci param_1_0 =
                if
                    Pine_builtin.int_is_sorted_asc
                        [ param_1_0
                        , 1
                        ]
                then
                    param_1_0

                else
                    Pine_builtin.int_add
                        [ Test.fibonacci
                            (Pine_builtin.int_add
                                [ param_1_0
                                , -2
                                ]
                            )
                        , Test.fibonacci
                            (Pine_builtin.int_add
                                [ param_1_0
                                , -1
                                ]
                            )
                        ]
            """"
            .Trim());

        // Dynamic test: invoke the fibonacci function and verify results
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var fibonacciDecl =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "fibonacci");

        var fibonacciParsed =
            FunctionRecord.ParseFunctionRecordTagged(fibonacciDecl.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing fibonacci: " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(fibonacciParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        string ResultAsExpressionString(int n)
        {
            var result = ApplyForArgument(IntegerEncoding.EncodeSignedInteger(n));

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(result, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        ResultAsExpressionString(0).Should().Be("0");
        ResultAsExpressionString(1).Should().Be("1");
        ResultAsExpressionString(2).Should().Be("1");
        ResultAsExpressionString(3).Should().Be("2");
        ResultAsExpressionString(4).Should().Be("3");
        ResultAsExpressionString(5).Should().Be("5");
        ResultAsExpressionString(6).Should().Be("8");
        ResultAsExpressionString(7).Should().Be("13");
    }

    [Fact]
    public void Function_Mutual_Recursion_IsEven_IsOdd()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            isEven x =
                if Pine_kernel.equal [ x, 0 ] then
                    True

                else
                    isOdd (Pine_kernel.int_add [ x, -1 ])


            isOdd y =
                if Pine_kernel.equal [ y, 0 ] then
                    False

                else
                    isEven (Pine_kernel.int_add [ y, -1 ])

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "isEven",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"
            
            Test.isEven param_1_0 =
                if
                    Pine_builtin.equal
                        [ param_1_0
                        , 0
                        ]
                then
                    True

                else
                    Test.isOdd
                        (Pine_builtin.int_add
                            [ param_1_0
                            , -1
                            ]
                        )

            
            Test.isOdd param_1_0 =
                if
                    Pine_builtin.equal
                        [ param_1_0
                        , 0
                        ]
                then
                    False

                else
                    Test.isEven
                        (Pine_builtin.int_add
                            [ param_1_0
                            , -1
                            ]
                        )

            """".Trim());

        // Dynamic test: invoke the functions and verify results
        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var isEvenDecl =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "isEven");

        var isEvenParsed =
            FunctionRecord.ParseFunctionRecordTagged(isEvenDecl.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing isEven: " + err));

        var invokeIsEven = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(isEvenParsed);

        var isOddDecl =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "isOdd");

        var isOddParsed =
            FunctionRecord.ParseFunctionRecordTagged(isOddDecl.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing isOdd: " + err));

        var invokeIsOdd = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(isOddParsed);

        PineValue ApplyIsEven(int n)
        {
            var (result, _) = invokeIsEven([IntegerEncoding.EncodeSignedInteger(n)]);

            return result.ReturnValue.Evaluate();
        }

        PineValue ApplyIsOdd(int n)
        {
            var (result, _) = invokeIsOdd([IntegerEncoding.EncodeSignedInteger(n)]);

            return result.ReturnValue.Evaluate();
        }

        string ResultAsExpressionString(PineValue result)
        {
            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(result, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        // Test isEven: 0 is even, 1 is not, 2 is even, etc.
        ResultAsExpressionString(ApplyIsEven(0)).Should().Be("True");
        ResultAsExpressionString(ApplyIsEven(1)).Should().Be("False");
        ResultAsExpressionString(ApplyIsEven(2)).Should().Be("True");
        ResultAsExpressionString(ApplyIsEven(3)).Should().Be("False");
        ResultAsExpressionString(ApplyIsEven(4)).Should().Be("True");
        ResultAsExpressionString(ApplyIsEven(5)).Should().Be("False");

        // Test isOdd: 0 is not odd, 1 is odd, 2 is not odd, etc.
        ResultAsExpressionString(ApplyIsOdd(0)).Should().Be("False");
        ResultAsExpressionString(ApplyIsOdd(1)).Should().Be("True");
        ResultAsExpressionString(ApplyIsOdd(2)).Should().Be("False");
        ResultAsExpressionString(ApplyIsOdd(3)).Should().Be("True");
        ResultAsExpressionString(ApplyIsOdd(4)).Should().Be("False");
        ResultAsExpressionString(ApplyIsOdd(5)).Should().Be("True");
    }
}
