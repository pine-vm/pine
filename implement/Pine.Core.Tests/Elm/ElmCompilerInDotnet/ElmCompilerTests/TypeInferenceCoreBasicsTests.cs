using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests for type inference with core Basics module functions that constrain types to Int.
/// These tests verify that using functions like modBy, remainderBy, and idiv (both infix and prefix)
/// properly constrains the types of expressions to Int, enabling the compiler to use
/// integer-specific builtins instead of generic number operations.
/// </summary>
public class TypeInferenceCoreBasicsTests
{
    [Fact]
    public void ModBy_constrains_arguments_to_Int()
    {
        // Using modBy constrains both arguments to Int
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa a b =
                modBy a b + 17

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

        // Since modBy returns Int, the + operator should use int_add
        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_add
                    [ Basics.modBy
                        param_1_0
                        param_1_1
                    , 17
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void RemainderBy_constrains_arguments_to_Int()
    {
        // Using remainderBy constrains both arguments to Int
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa a b =
                remainderBy a b * 3

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

        // Since remainderBy returns Int, the * operator should use int_mul
        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ Basics.remainderBy
                        param_1_0
                        param_1_1
                    , 3
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Idiv_infix_constrains_to_Int()
    {
        // Using // operator constrains both operands to Int
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa a b =
                (a // b) + 5

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

        // Since // returns Int, the + operator should use int_add
        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_add
                    [ Basics.idiv
                        param_1_0
                        param_1_1
                    , 5
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Idiv_prefix_constrains_to_Int()
    {
        // Using (//) as prefix operator constrains both operands to Int
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa a b =
                (//) a b + 7

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

        // Since (//) returns Int, the + operator should use int_add
        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_add
                    [ Basics.idiv
                        param_1_0
                        param_1_1
                    , 7
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void ModBy_result_propagates_Int_type()
    {
        // The result of modBy should propagate Int type through further operations
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

    [Fact]
    public void Idiv_operand_type_constrains_to_Int()
    {
        // If a parameter is used as an argument to //, it should be constrained to Int
        // and subsequent uses in + should use int_add
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa a b =
                (a // b) + 10

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

        // Since a and b are used in //, they're Int, so + uses int_add
        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_add
                    [ Basics.idiv
                        param_1_0
                        param_1_1
                    , 10
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void ModBy_partial_application_infers_function_type()
    {
        // Partial application of Basics.modBy with a single argument should infer type Int -> Int
        // Note: We use qualified Basics.modBy because type inference only recognizes
        // qualified references. Unqualified references should be resolved by canonicalization.
        var moduleText =
            """"
            module Test exposing (..)


            alfa =
                Basics.modBy 7

            """";

        var inferredType = ElmCompilerTestHelper.GetInferredTypeForDeclaration(moduleText, "alfa");

        // Expected type: Int -> Int
        // modBy has type Int -> Int -> Int, so applying one Int argument yields Int -> Int
        inferredType.Should().Be(
            TypeInference.InferredType.Function(
                argType: TypeInference.InferredType.Int(),
                returnType: TypeInference.InferredType.Int()));
    }
}
