using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleFunctionTests
{
    [Fact]
    public void Function_int_add_constant()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                x + 41

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

            Test.alfa param_1_0 =
                Pine_kernel.int_add
                    [ param_1_0
                    , 41
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_branch_if_simple()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                if Pine_kernel.int_is_sorted_asc [ x, 17 ] then
                    Pine_kernel.int_add [ x, 11 ]

                else
                    Pine_kernel.int_mul [ x, 13 ]

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

            Test.alfa param_1_0 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_0
                        , 17
                        ]
                then
                    Pine_kernel.int_add
                        [ param_1_0
                        , 11
                        ]

                else
                    Pine_kernel.int_mul
                        [ param_1_0
                        , 13
                        ]
            
            """"
            .Trim());
    }
}
