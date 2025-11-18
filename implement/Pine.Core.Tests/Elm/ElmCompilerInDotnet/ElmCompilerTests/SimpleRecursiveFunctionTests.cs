using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleRecursiveFunctionTests
{
    [Fact]
    public void Function_Fibonacci()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            fibonacci : Int -> Int
            fibonacci n =
                if Pine_kernel.int_is_sorted_asc [ n, 2 ] then
                    n

                else
                    Pine_kernel.int_add
                        [ fibonacci (Pine_kernel.int_add [ n, -2 ])
                        , fibonacci (Pine_kernel.int_add [ n, -1 ])
                        ]

            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "fibonacci",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.fibonacci param_1_0 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_0
                        , 2
                        ]
                then
                    param_1_0

                else
                    Pine_kernel.int_add
                        [ Test.fibonacci
                            (Pine_kernel.int_add
                                [ param_1_0
                                , -2
                                ]
                            )
                        , Test.fibonacci
                            (Pine_kernel.int_add
                                [ param_1_0
                                , -1
                                ]
                            )
                        ]
            """"
            .Trim());
    }
}
