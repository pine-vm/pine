using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests for type inference when operators are canonicalized to function applications.
/// After canonicalization, `a + b` becomes `Basics.add a b`.
/// These tests verify that type inference correctly specializes the polymorphic
/// numeric operations to integer operations when types are constrained to Int.
/// </summary>
public class TypeInferenceCanonicalizedOperatorsTests
{
    /// <summary>
    /// Basic test: Int parameters should cause Basics.add to be compiled as int_add.
    /// </summary>
    [Fact]
    public void Basics_add_with_int_parameters_uses_int_add()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa a b =
                a + b

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
                Pine_builtin.int_add
                    [ param_1_0
                    , param_1_1
                    ]
            
            """"
            .Trim());
    }

    /// <summary>
    /// Test that Basics.mul uses int_mul when both operands are Int.
    /// </summary>
    [Fact]
    public void Basics_mul_with_int_parameters_uses_int_mul()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa a b =
                a * b

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
                    , param_1_1
                    ]
            
            """"
            .Trim());
    }

    /// <summary>
    /// Test that local bindings with Int types are used for type inference.
    /// This tests that localBindingTypes is checked before parameterTypes.
    /// </summary>
    [Fact]
    public void Local_binding_int_type_propagates_to_operator()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa a b c =
                let
                    remainder = modBy a b
                in
                remainder * c

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

        // remainder is Int (from modBy), so c is constrained to Int, and * uses int_mul
        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 param_1_2 =
                Pine_builtin.int_mul
                    [ Basics.modBy
                        param_1_0
                        param_1_1
                    , param_1_2
                    ]
            
            """"
            .Trim());
    }

    /// <summary>
    /// Test that type constraints from function applications propagate through
    /// canonicalized operators (Basics.add etc.) in nested expressions.
    /// </summary>
    [Fact]
    public void Type_constraints_propagate_through_nested_canonicalized_operators()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            beta : Int -> Int
            beta x =
                x + 1


            alfa x =
                beta (x + 7) * 2

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

        // Since beta: Int -> Int, x + 7 must be Int, so x is Int
        // The multiplication should use int_mul
        wholeProgramText.Trim().Should().Contain("Pine_builtin.int_mul");
    }

    /// <summary>
    /// Test that tuple destructuring with Int types correctly propagates
    /// type information through canonicalized operators.
    /// </summary>
    [Fact]
    public void Tuple_destructuring_int_types_propagate_to_canonicalized_operators()
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

    /// <summary>
    /// Test that direct tuple pattern with Int types propagates correctly.
    /// </summary>
    [Fact]
    public void Direct_tuple_pattern_int_types_propagate_to_canonicalized_operators()
    {
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
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }
}
