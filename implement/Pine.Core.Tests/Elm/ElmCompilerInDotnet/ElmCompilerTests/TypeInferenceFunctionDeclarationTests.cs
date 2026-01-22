using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class TypeInferenceFunctionDeclarationTests
{
    /*
     * In these tests, we compose expressions using applications of operators that accept operands of the `number` type class.
     * Therefore, without additional typing information, the compiler could not replace these operations with integer-specific builtins.
     * 
     * These tests cover the propagation of type information via function arguments and return values.
     * */

    [Fact]
    public void Function_return_type_constrains_to_Int()
    {
        // Return value from alfa constrains type to Int

        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : List a -> Int
            alfa l =
                case l of
                    [] ->
                        13

                    _ ->
                        17

            beta a b =
                b * (alfa a + 17)

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "beta",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                if
                    Pine_builtin.equal
                        [ Pine_builtin.length
                            param_1_0
                        , 0
                        ]
                then
                    13

                else
                    17


            Test.beta param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_1
                    , Pine_builtin.int_add
                        [ Test.alfa
                            param_1_0
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Function_argument_type_constrains_to_Int()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> String
            alfa i =
                case 0 of
                    0 ->
                        ""

                    _ ->
                        "not zero"

            beta a b =
                let
                    c = alfa a
                in
                b * (a + 17)

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "beta",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.beta param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_1
                    , Pine_builtin.int_add
                        [ param_1_0
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }
}
