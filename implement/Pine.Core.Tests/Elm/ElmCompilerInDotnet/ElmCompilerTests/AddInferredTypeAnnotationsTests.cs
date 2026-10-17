using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using Xunit;

using ConcreteSyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class AddInferredTypeAnnotationsTests
{
    [Fact]
    public void Infers_type_from_top_level_literal_int()
    {
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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

        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
    public void Infers_type_int_from_chained_if_block_subexpression()
    {
        // Use integer-specific operator in one of the branches to constrain the return type to Int.

        var moduleText =
            """"
            module Test exposing (..)


            alfa a b =
                if a > b then
                    a * b
                else if a == b then
                    a // b
                else
                    a - b

            """";

        var inferredType = ElmCompilerTestHelper.GetInferredTypeForDeclaration(moduleText, "alfa");

        // Expected type: Int -> Int -> Int

        inferredType.Should().Be(
            TypeInference.InferredType.Function(
                argType: TypeInference.InferredType.Int(),
                returnType: TypeInference.InferredType.Function(
                    argType: TypeInference.InferredType.Int(),
                    returnType: TypeInference.InferredType.Int())));
    }

    [Fact]
    public void Infers_type_int_from_case_arm_expression()
    {
        // Use integer-specific operator in one of the branches to constrain the return type to Int.

        var moduleText =
            """"
            module Test exposing (..)


            alfa a b =
                case compare a b of
                    GT ->
                        a * b
                    EQ ->
                        a // b
                    LT ->
                        a - b

            """";

        var inferredType = ElmCompilerTestHelper.GetInferredTypeForDeclaration(moduleText, "alfa");

        // Expected type: Int -> Int -> Int

        inferredType.Should().Be(
            TypeInference.InferredType.Function(
                argType: TypeInference.InferredType.Int(),
                returnType: TypeInference.InferredType.Function(
                    argType: TypeInference.InferredType.Int(),
                    returnType: TypeInference.InferredType.Int())));
    }

    [Fact]
    public void Infers_type_int_from_addition_of_let_bound_int_values()
    {
        var moduleText =
            """"
            module Test exposing (..)


            alfa =
                let
                    sColInt : Int
                    sColInt =
                        41

                    kwdLengthInt : Int
                    kwdLengthInt =
                        1
                in
                sColInt + kwdLengthInt

            """";

        var inferredType = ElmCompilerTestHelper.GetInferredTypeForDeclaration(moduleText, "alfa");

        inferredType.Should().Be(TypeInference.InferredType.Int());
    }

    [Fact]
    public void Infers_type_float_from_let_block_expression_subexpression()
    {
        /*
         * 'a' is connected via arithmetic operations to a Float literal (t0 + 1.0).
         * 
         * 'b' is only used in an addition with an Int literal (b + 7), which constrains it to the Number type class.
         * */

        var moduleText =
            """"
            module Test exposing (..)


            alfa a b =
                let
                    t0 =
                        a * 13

                    t1 =
                        b + 7
                in
                t0 + 1.0

            """";

        var inferredType = ElmCompilerTestHelper.GetInferredTypeForDeclaration(moduleText, "alfa");

        // Expected type: Float -> number -> Float

        inferredType.Should().Be(
            TypeInference.InferredType.Function(
                argType: TypeInference.InferredType.Float(),
                returnType: TypeInference.InferredType.Function(
                    argType: TypeInference.InferredType.Number(),
                    returnType: TypeInference.InferredType.Float())));
    }

    [Fact]
    public void Infers_concrete_choice_type_from_application_of_generic_tag_with_concrete_argument_type()
    {
        var moduleText =
            """"
            module Test exposing (..)


            type ChoiceType a b
                = ChoiceOne a
                | ChoiceTwo b


            alfa =
                if a < b then
                    ChoiceOne "Test"

                else
                    ChoiceTwo (b // 13)

            """";

        var expectedModuleText =
            """"
            module Test exposing (..)
            
            
            type ChoiceType a b
                = ChoiceOne a
                | ChoiceTwo b


            alfa : ChoiceType String Int
            alfa =
                if a < b then
                    ChoiceOne "Test"
            
                else
                    ChoiceTwo (b // 13)
            
            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                TestCase.DefaultAppWithoutPackages([moduleText]),
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_concrete_choice_type_from_foreign_module_with_concrete_argument_type()
    {
        // Type declaration is in a separate "Types" module, imported by the main module
        var typesModuleText =
            """"
            module Types exposing (..)


            type ChoiceType a b
                = ChoiceOne a
                | ChoiceTwo b

            """";

        var mainModuleText =
            """"
            module Test exposing (..)

            import Types exposing (ChoiceType(..))


            alfa =
                if a < b then
                    ChoiceOne "Test"

                else
                    ChoiceTwo (b // 13)

            """";

        var expectedModuleText =
            """"
            module Test exposing (..)

            import Types exposing (ChoiceType(..))


            alfa : ChoiceType String Int
            alfa =
                if a < b then
                    ChoiceOne "Test"

                else
                    ChoiceTwo (b // 13)

            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                TestCase.DefaultAppWithoutPackages([typesModuleText, mainModuleText]),
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_through_foreign_generic_function_return_alias()
    {
        var typesModuleText =
            """"
            module Types exposing (base)


            type alias Callback value =
                Int -> value


            base : Bool -> Callback String
            base _ _ =
                "done"

            """";

        var mainModuleText =
            """"
            module Test exposing (..)

            import Types exposing (base)


            result =
                base True 1

            """";

        var expectedModuleText =
            """"
            module Test exposing (..)

            import Types exposing (base)


            result : String
            result =
                base True 1

            """";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                TestCase.DefaultAppWithoutPackages([typesModuleText, mainModuleText]),
                ["Test"]);

        result.Trim().Should().Be(expectedModuleText.Trim());
    }

    [Fact]
    public void Infers_type_from_top_level_record_expression()
    {
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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
        var scenario =
            TestCase.DefaultAppWithoutPackages(
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

    [Theory]
    [InlineData("BODY", "Int")]
    [InlineData("((BODY))", "Int")]
    [InlineData("identity (BODY)", "Int")]
    [InlineData("(\\_ -> BODY) ()", "Int")]
    [InlineData("if BODY == 41 then 1 else 0", "Int")]
    [InlineData("if True then BODY else 0", "Int")]
    [InlineData("if True then 0 else BODY", "Int")]
    [InlineData("case BODY of\n    _ ->\n        0", "Int")]
    [InlineData("case True of\n    _ ->\n        BODY", "Int")]
    [InlineData("\\_ -> BODY", "a -> Int")]
    [InlineData("( BODY, 0 )", "( Int, Int )")]
    [InlineData("[ BODY ]", "List Int")]
    [InlineData("{ field = BODY }", "{ field : Int }")]
    [InlineData("BODY + 1", "Int")]
    [InlineData("1 + BODY", "Int")]
    [InlineData("let\n    outer =\n        0\nin\nBODY", "Int")]
    public void Infers_local_annotations_through_abstract_expression_variants(
        string expressionTemplate,
        string signature)
    {
        var localExpression =
            """
            let
                local =
                    41
            in
            local
            """;

        var bodyIndex = expressionTemplate.IndexOf("BODY", StringComparison.Ordinal);
        var lineStart = expressionTemplate.LastIndexOf('\n', bodyIndex) + 1;
        var indentation = new string(' ', bodyIndex - lineStart);

        var expression =
            expressionTemplate.Replace(
                "BODY",
                localExpression.Replace("\n", "\n" + indentation, StringComparison.Ordinal),
                StringComparison.Ordinal);

        var moduleText =
            "module Test exposing (..)\n\nalfa : " + signature + "\nalfa =\n    " +
            expression.Replace("\n", "\n    ", StringComparison.Ordinal) + "\n";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                TestCase.DefaultAppWithoutPackages([moduleText]),
                ["Test"]);

        result.Should().Contain("local : number");
        result.Should().Contain("alfa : " + signature);
    }

    [Theory]
    [InlineData("import Types", "Types.Choice", "Types.Choice")]
    [InlineData("import Types as Alias", "Alias.Choice", "Alias.Choice")]
    [InlineData("import Types exposing (Choice)", "Types.Choice", "Choice")]
    [InlineData("import Types exposing (Choice(..))", "Choice", "Choice")]
    [InlineData("import Types exposing (..)", "Choice", "Choice")]
    [InlineData("import Types as Alias exposing (Choice)", "Alias.Choice", "Choice")]
    public void Preserves_source_module_names_for_inferred_choice_annotations(
        string import,
        string constructor,
        string annotation)
    {
        var typesModuleText =
            """
            module Types exposing (..)

            type Choice
                = Choice String
            """;

        var moduleText =
            "module Test exposing (..)\n\n" + import +
            "\n\nresult =\n    " + constructor + " \"done\"\n";

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                TestCase.DefaultAppWithoutPackages([typesModuleText, moduleText]),
                ["Test"]);

        result.Should().Contain("result : " + annotation);
        result.Should().Contain(constructor + " \"done\"");
    }

    [Fact]
    public void Preserves_concrete_source_details_and_existing_annotations()
    {
        var moduleText =
            """
            module Test exposing (..)


            {-| Keep this documentation.
            -}
            existing : (String -> String)
            existing text =
                (text)


            -- Keep hexadecimal spelling and redundant parentheses.
            value =
                ((0x2A))


            text =
                "line\n\u{0041}"

            """;

        var original =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception(err.ToString()));

        var annotatedModule =
            AddInferredTypeAnnotations.InferAndAddTypeAnnotationsForAllDeclarationsByModuleName(
                TestCase.DefaultAppWithoutPackages([moduleText]).AsFileTree())
            .Extract(err => throw new Exception(err));

        var annotated =
            annotatedModule(["Test"])
            .Extract(err => throw new Exception(err));

        annotated.Comments.Should().BeEquivalentTo(original.Comments);
        annotated.Declarations.Count.Should().Be(original.Declarations.Count);

        for (var index = 0; index < original.Declarations.Count; index++)
        {
            var originalFunction =
                (ConcreteSyntaxTypes.Declaration.FunctionDeclaration)original.Declarations[index].Value;

            var annotatedFunction =
                (ConcreteSyntaxTypes.Declaration.FunctionDeclaration)annotated.Declarations[index].Value;

            annotatedFunction.Function.Declaration.Should().BeEquivalentTo(originalFunction.Function.Declaration);

            if (originalFunction.Function.Signature is { } originalSignature)
            {
                annotatedFunction.Function.Signature.Should().BeEquivalentTo(originalSignature);
            }
            else
            {
                annotatedFunction.Function.Signature.Should().NotBeNull();
            }
        }

        var formatted = Avh4Format.FormatToString(annotated);
        formatted.Should().Contain("value : number");
        formatted.Should().Contain("text : String");
    }

    [Fact]
    public void Filters_nested_declaration_paths_under_existing_annotations()
    {
        var moduleText =
            """
            module Test exposing (..)


            alfa : Int
            alfa =
                let
                    nested : Int
                    nested =
                        let
                            selected =
                                41

                            excluded =
                                "keep"
                        in
                        selected
                in
                nested

            """;

        var visitedPaths = new List<string>();

        var result =
            AddTypeAnnotationsAndFormatToString(
                TestCase.DefaultAppWithoutPackages([moduleText]),
                ["Test"],
                path =>
                {
                    var joinedPath = string.Join(".", path);
                    visitedPaths.Add(joinedPath);
                    return joinedPath != "alfa.nested.excluded";
                });

        visitedPaths.Should().Contain("alfa.nested.selected");
        visitedPaths.Should().Contain("alfa.nested.excluded");
        result.Should().Contain("selected : number");
        result.Should().NotContain("excluded :");
        result.Should().Contain("alfa : Int");
        result.Should().Contain("nested : Int");
    }

    [Fact]
    public void Infers_function_types_from_unwrapped_argument_patterns()
    {
        var moduleText =
            """
            module Test exposing (..)


            alfa (value) =
                value // 2


            beta =
                let
                    local (value) =
                        value // 3
                in
                local 41

            """;

        var result =
            AddAllTypeAnnotationsAndFormatToString(
                TestCase.DefaultAppWithoutPackages([moduleText]),
                ["Test"]);

        result.Should().Contain("alfa : number -> Int");
        result.Should().Contain("alfa (value) =");
        result.Should().Contain("local : number -> Int");
        result.Should().Contain("local (value) =");
    }

    private static string AddAllTypeAnnotationsAndFormatToString(
        TestCase scenario,
        IReadOnlyList<string> moduleName)
    {
        static bool IncludeDeclaration(IReadOnlyList<string> declarationPath)
        {
            return true;
        }

        return
            AddTypeAnnotationsAndFormatToString(
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
