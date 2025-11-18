using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleOperatorApplicationTests
{
    [Fact]
    public void Int_mul_parens_int_add()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                x * (y + 3)

            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_kernel.int_mul
                    [ param_1_0
                    , Pine_kernel.int_add
                        [ param_1_1
                        , 3
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Int_mul_parens_int_sub()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                x * (11 - y)

            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_kernel.int_mul
                    [ param_1_0
                    , Pine_kernel.int_add
                        [ 11
                        , Pine_kernel.int_mul
                            [ -1
                            , param_1_1
                            ]
                        ]
                    ]
            
            """"
            .Trim());
    }
}
