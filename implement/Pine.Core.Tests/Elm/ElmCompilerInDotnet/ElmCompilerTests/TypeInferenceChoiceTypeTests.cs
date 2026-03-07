using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class TypeInferenceChoiceTypeTests
{
    /*
     * In these tests, we compose expressions using applications of operators that accept operands of the `number` type class.
     * Therefore, without additional typing information, the compiler could not replace these operations with integer-specific builtins.
     * 
     * These tests cover the propagation of type information via choice type tags.
     * */

    [Fact]
    public void Deconstruction_from_concrete_tag_constrains_to_Int()
    {
        // Since 'a' is obtained by deconstructing choice type tag `TagAlfa`, its type is constrained to Int

        var elmModuleText =
            """"
            module Test exposing (..)


            type ChoiceType
                = TagAlfa Int

            alfa (TagAlfa a) b =
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
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_1
                    , Pine_builtin.int_add
                        [ param_1_1[0]
                        , 17
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Construction_of_concrete_tag_constrains_to_Int()
    {
        // Since 'a' is used to construct a choice type tag of type `ChoiceType`, its type is constrained to Int

        var elmModuleText =
            """"
            module Test exposing (..)


            type ChoiceType
                = TagAlfa Int


            alfa a b =
                let
                    c = TagAlfa a
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
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
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

    [Fact]
    public void Deconstruction_from_concrete_tag_constrains_to_Int_despite_containing_type_generic()
    {
        // Since 'a' is obtained by deconstructing choice tag `TagAlfa`, its type is constrained to Int

        var elmModuleText =
            """"
            module Test exposing (..)


            type ChoiceType a
                = TagAlfa Int
                | TagBeta a

            alfa a b =
                case a of
                    TagAlfa x ->
                        b * (x + 17)

                    _ ->
                        0

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
                if
                    if
                        Pine_builtin.equal
                            [ Pine_builtin.length
                                param_1_1
                            , 1
                            ]
                    then
                        Pine_builtin.equal
                            [ param_1_0
                            , TagAlfa
                            ]

                    else
                        False
                then
                    Pine_builtin.int_mul
                        [ param_1_1
                        , Pine_builtin.int_add
                            [ param_1_1[0]
                            , 17
                            ]
                        ]

                else
                    0

            """"
            .Trim());
    }

    [Fact]
    public void Deconstruction_from_concrete_tag_tuple_item_constrains_to_Int()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type ChoiceType
                = TagAlfa (Int, Int)
                | TagBeta

            alfa a b =
                case a of
                    TagAlfa (y, x) ->
                        b * (x + 17)

                    _ ->
                        0

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
                if
                    if
                        Pine_builtin.equal
                            [ Pine_builtin.length
                                param_1_1[0]
                            , 2
                            ]
                    then
                        if
                            Pine_builtin.equal
                                [ Pine_builtin.length
                                    param_1_1
                                , 1
                                ]
                        then
                            Pine_builtin.equal
                                [ param_1_0
                                , TagAlfa
                                ]

                        else
                            False

                    else
                        False
                then
                    Pine_builtin.int_mul
                        [ param_1_1
                        , Pine_builtin.int_add
                            [ param_1_1[0][1]
                            , 17
                            ]
                        ]

                else
                    0

            """"
            .Trim());
    }

    [Fact]
    public void Construction_from_qualified_tag_constrains_to_Int_despite_cross_module_same_name()
    {
        var moduleA =
            """"
            module ModuleA exposing (ChoiceType(..))


            type ChoiceType
                = SharedTag Int

            """";

        var moduleB =
            """"
            module ModuleB exposing (ChoiceType(..))


            type ChoiceType
                = SharedTag String Bool

            """";

        var moduleTest =
            """"
            module Test exposing (..)

            import ModuleA
            import ModuleB


            alfa a b =
                let
                    c =
                        ModuleA.SharedTag a
                in
                b * (a + 17)

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [moduleA, moduleB, moduleTest],
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
