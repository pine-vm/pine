using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class TypeInferenceLetBlockTests
{
    /*
     * In these tests, we compose expressions using applications of operators that accept operands of the `number` type class.
     * Therefore, without additional typing information, the compiler could not replace these operations with integer-specific builtins.
     * */

    [Fact]
    public void Int_mul_for_operand_from_let_binding_alias()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                let
                    a = x
                in
                a * (y + 3)

            """";

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

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , Pine_builtin.int_add
                        [ param_1_1
                        , 3
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Int_mul_for_operand_from_let_binding_chained_alias()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                let
                    a = x

                    b = a
                in
                b * (y + 3)

            """";

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

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , Pine_builtin.int_add
                        [ param_1_1
                        , 3
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Int_mul_for_operand_from_chained_let_binding_alias()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                let
                    a = x
                in
                let
                    b = a
                in
                b * (y + 3)

            """";

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

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , Pine_builtin.int_add
                        [ param_1_1
                        , 3
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Int_mul_for_operand_from_let_binding_sum()
    {
        // Should infer that 'a' is constrained to 'Int' as result from (Int + number)

        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                let
                    a =
                        x + 13
                in
                a * (y + 7)

            """";

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

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ Pine_builtin.int_add
                        [ param_1_0
                        , 13
                        ]
                    , Pine_builtin.int_add
                        [ param_1_1
                        , 7
                        ]
                    ]
            
            """"
            .Trim());
    }
}
