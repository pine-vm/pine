using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class TypeInferenceTupleTests
{
    /*
     * In these tests, we compose expressions using applications of operators that accept operands of the `number` type class.
     * Therefore, without additional typing information, the compiler could not replace these operations with integer-specific builtins.
     * 
     * These tests cover the propagation of type information via tuple patterns and tuple expressions.
     * */

    [Fact]
    public void Tuple_pattern_int_int_via_function_parameter()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : (Int, Int) -> Int
            alfa (x, y) =
                x * (y + 17)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText =
            StaticExpressionDisplay.RenderStaticProgram(
                staticProgram,
                kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Pine_builtin.int_mul
                    [ param_1_0[0]
                    , Pine_builtin.int_add
                        [ param_1_0[1]
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Tuple_pattern_int_int_via_let_destructuring()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : (Int, Int) -> Int
            alfa t =
                let
                    (x, y) = t
                in
                x * (y + 17)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText =
            StaticExpressionDisplay.RenderStaticProgram(
                staticProgram,
                kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Pine_builtin.int_mul
                    [ param_1_0[0]
                    , Pine_builtin.int_add
                        [ param_1_0[1]
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }


    [Fact]
    public void Tuple_pattern_nested_int_int_via_function_parameter()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : (o, (Int, Int)) -> Int
            alfa (_, (x, y)) =
                x * (y + 17)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText =
            StaticExpressionDisplay.RenderStaticProgram(
                staticProgram,
                kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Pine_builtin.int_mul
                    [ param_1_0[1][0]
                    , Pine_builtin.int_add
                        [ param_1_0[1][1]
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Tuple_pattern_int_int_via_case_arm()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : (Int, Int) -> Int
            alfa t =
                case t of
                    (x, y) ->
                        x * (y + 17)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText =
            StaticExpressionDisplay.RenderStaticProgram(
                staticProgram,
                kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                if
                    Pine_builtin.equal
                        [ Pine_builtin.length
                            param_1_0
                        , 2
                        ]
                then
                    Pine_builtin.int_mul
                        [ param_1_0[0]
                        , Pine_builtin.int_add
                            [ param_1_0[1]
                            , 17
                            ]
                        ]

                else
                    Pine_builtin.int_mul
                        [ param_1_0[0]
                        , Pine_builtin.int_add
                            [ param_1_0[1]
                            , 17
                            ]
                        ]

            """"
            .Trim());
    }

    [Fact]
    public void Tuple_expression_with_typed_elements()
    {
        // Test type inference when destructuring a tuple expression with typed elements

        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa a b =
                let
                    (x, y) = (a, b)
                in
                x * (y + 17)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText =
            StaticExpressionDisplay.RenderStaticProgram(
                staticProgram,
                kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ Pine_builtin.head
                        [ param_1_0
                        , param_1_1
                        ]
                    , Pine_builtin.int_add
                        [ Pine_builtin.head
                            (Pine_builtin.skip
                                [ 1
                                , [ param_1_0
                                  , param_1_1
                                  ]
                                ]
                            )
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Tuple_expression_nested_with_typed_elements()
    {
        // Test type inference through nested tuple expressions

        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa a b =
                let
                    (_, (x, y)) = (0, (a, b))
                in
                x * (y + 17)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText =
            StaticExpressionDisplay.RenderStaticProgram(
                staticProgram,
                kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ Pine_builtin.head
                        (Pine_builtin.head
                            (Pine_builtin.skip
                                [ 1
                                , [ 0
                                  , [ param_1_0
                                    , param_1_1
                                    ]
                                  ]
                                ]
                            )
                        )
                    , Pine_builtin.int_add
                        [ Pine_builtin.head
                            (Pine_builtin.skip
                                [ 1
                                , Pine_builtin.head
                                    (Pine_builtin.skip
                                        [ 1
                                        , [ 0
                                          , [ param_1_0
                                            , param_1_1
                                            ]
                                          ]
                                        ]
                                    )
                                ]
                            )
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Tuple_expression_via_let_binding()
    {
        // Test type inference when a tuple expression is bound to a variable and then destructured
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa a b =
                let
                    t = (a, b)
                    (x, y) = t
                in
                x * (y + 17)

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText =
            StaticExpressionDisplay.RenderStaticProgram(
                staticProgram,
                kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ Pine_builtin.head
                        [ param_1_0
                        , param_1_1
                        ]
                    , Pine_builtin.int_add
                        [ Pine_builtin.head
                            (Pine_builtin.skip
                                [ 1
                                , [ param_1_0
                                  , param_1_1
                                  ]
                                ]
                            )
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }
}
