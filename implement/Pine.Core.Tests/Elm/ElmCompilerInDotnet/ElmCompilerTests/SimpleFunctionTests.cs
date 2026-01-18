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

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_0 =
                Pine_builtin.int_add
                    [ param_0
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

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_0 =
                if
                    Pine_builtin.int_is_sorted_asc
                        [ param_0
                        , 17
                        ]
                then
                    Pine_builtin.int_add
                        [ param_0
                        , 11
                        ]

                else
                    Pine_builtin.int_mul
                        [ param_0
                        , 13
                        ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_invoking_other_function()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> List Int
            alfa x =
                [ x
                , beta x
                ]


            beta : Int -> Int
            beta y =
                Pine_builtin.int_add
                    [ y
                    , 41
                    ]

            """;

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_0 =
                [ param_0
                , Test.beta
                    param_0
                ]


            Test.beta param_0 =
                Pine_builtin.int_add
                    [ param_0
                    , 41
                    ]
            
            """"
            .Trim());
    }

}
