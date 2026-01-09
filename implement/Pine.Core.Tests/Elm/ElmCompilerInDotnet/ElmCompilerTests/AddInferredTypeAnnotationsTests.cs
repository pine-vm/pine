using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class AddInferredTypeAnnotationsTests
{
    [Fact]
    public void Infers_type_from_top_level_literal_int()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            alfa =
                41

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)


            alfa : number
            alfa =
                41
            
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_literal_string()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            alfa =
                "Hello World"

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)


            alfa : String
            alfa =
                "Hello World"
            
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_operator_integer_division()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            alfa =
                41 // 17

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)


            alfa : Int
            alfa =
                41 // 17
            
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_function_with_operator_multiply()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            alfa a b =
                a * b

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)


            alfa : number -> number -> number
            alfa a b =
                a * b
                        
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_list_expression_containing_integer_literals()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            alfa =
                [ 13, 17, 19 ]

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)


            alfa : List number
            alfa =
                [ 13, 17, 19 ]

            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_concrete_choice_tag_application_complete()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            type Alfa
                = ChoiceA Int
                | ChoiceB String


            type Beta
                = ChoiceC Float
                | ChoiceD Bool


            alfa =
                ChoiceD True

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)
            
            
            type Alfa
                = ChoiceA Int
                | ChoiceB String
            
            
            type Beta
                = ChoiceC Float
                | ChoiceD Bool
            

            alfa : Beta
            alfa =
                ChoiceD True
            
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_concrete_choice_tag_application_partial_zero()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            type Alfa
                = ChoiceA Int
                | ChoiceB String


            type Beta
                = ChoiceC Float
                | ChoiceD Bool


            alfa =
                ChoiceD

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)
            
            
            type Alfa
                = ChoiceA Int
                | ChoiceB String
            
            
            type Beta
                = ChoiceC Float
                | ChoiceD Bool
            

            alfa : Bool -> Beta
            alfa =
                ChoiceD
            
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_concrete_choice_tag_application_partial_first()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            type Combined
                = ChoiceA () Int
                | ChoiceB ( String, Int ) Int String


            alfa =
                ChoiceA ()


            beta =
                ChoiceB ("Test", 17)

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)
            
            
            type Combined
                = ChoiceA () Int
                | ChoiceB ( String, Int ) Int String
            

            alfa : Int -> Combined
            alfa =
                ChoiceA ()
            
            
            beta : Int -> String -> Combined
            beta =
                ChoiceB ( "Test", 17 )
            
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_let_declaration_literal_int()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            alfa =
                let
                    t0 =
                        41
                in
                t0

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)


            alfa : number
            alfa =
                let
                    t0 : number
                    t0 =
                        41
                in
                t0

            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_via_application_argument_from_second_parameter()
    {
        // Since 'a' is passed to 'beta' as second parameter which is Int, 'a' must be Int.

        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            alfa a =
                let
                    t0 =
                        beta "Test" a
                in
                "Test"


            beta : String -> Int -> {}
            beta s i =
                ""

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> String
            alfa a =
                let
                    t0 : {}
                    t0 =
                        beta "Test" a
                in
                "Test"


            beta : String -> Int -> {}
            beta s i =
                ""
                                    
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_record_expression()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            alfa =
                { field = "Hello"
                , anotherField = 17
                }

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)


            alfa : { anotherField : number, field : String }
            alfa =
                { field = "Hello"
                , anotherField = 17
                }
            
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_record_constructor_application_partial_zero()
    {
        var scenario = TestCase.DefaultAppWithoutPackages(
        [
            """"
            module Test exposing (..)


            type alias Alfa =
                { field : String
                , anotherField : Int
                }


            type alias Beta =
                { anotherField : Int
                , field : String
                }


            alfa =
                Alfa


            beta =
                Beta

            """",
        ]);

        var expectedModuleText =
            """"
            module Test exposing (..)
            
            
            type alias Alfa =
                { field : String
                , anotherField : Int
                }
            
            
            type alias Beta =
                { anotherField : Int
                , field : String
                }
            

            alfa : String -> Int -> { anotherField : Int, field : String }
            alfa =
                Alfa
            

            beta : Int -> String -> { anotherField : Int, field : String }
            beta =
                Beta

            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                scenario,
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    private static string AddAllTypeAnnotationsAndFormatToString(
        TestCase scenario,
        IReadOnlyList<string> moduleName)
    {
        static bool IncludeDeclaration(IReadOnlyList<string> declarationPath)
        {
            return true;
        }

        return AddTypeAnnotationsAndFormatToString(
            scenario,
            moduleName,
            IncludeDeclaration);
    }

    private static string AddTypeAnnotationsAndFormatToString(
        TestCase scenario,
        IReadOnlyList<string> moduleName,
        Func<IReadOnlyList<string>, bool> includeDeclaration)
    {
        var asFileTree = scenario.AsFileTree();

        var withAnnotationsFromModuleName =
            AddInferredTypeAnnotations.InferAndAddTypeAnnotationsByModuleName(asFileTree)
            .Extract(err => throw new Exception(err));

        var fileQueryable =
            withAnnotationsFromModuleName(moduleName)
            .Extract(err => throw new Exception(err));

        var fileWithAnnotations =
            fileQueryable(includeDeclaration);

        return Avh4Format.FormatToString(fileWithAnnotations);
    }
}
