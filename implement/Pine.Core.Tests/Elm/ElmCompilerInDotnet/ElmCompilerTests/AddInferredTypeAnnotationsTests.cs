using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

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

        var inferredType = GetInferredTypeForDeclaration(moduleText, "alfa");

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

        var inferredType = GetInferredTypeForDeclaration(moduleText, "alfa");

        // Expected type: Int -> Int -> Int

        inferredType.Should().Be(
            TypeInference.InferredType.Function(
                argType: TypeInference.InferredType.Int(),
                returnType: TypeInference.InferredType.Function(
                    argType: TypeInference.InferredType.Int(),
                    returnType: TypeInference.InferredType.Int())));
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

        var inferredType = GetInferredTypeForDeclaration(moduleText, "alfa");

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

    /// <summary>
    /// Gets the inferred type for a specific top-level declaration in a module.
    /// </summary>
    private static TypeInference.InferredType GetInferredTypeForDeclaration(
        string moduleText,
        string declarationName)
    {
        var parseResult = Core.Elm.ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText);

        if (parseResult.IsErrOrNull() is { } err)
        {
            throw new Exception($"Failed to parse module: {err}");
        }

        if (parseResult.IsOkOrNull() is not { } parsedFile)
        {
            throw new NotImplementedException("Unexpected parse result type");
        }

        var moduleName = Module.GetModuleName(parsedFile.ModuleDefinition.Value).Value;
        var moduleNameStr = string.Join(".", moduleName);

        // Convert the concretized file to abstract syntax for type inference
        var abstractFile = SyntaxTypes.FromStil4mConcretized.Convert(parsedFile);

        // Build a map of function signatures from the file using TypeInference
        var functionSignatures = TypeInference.BuildFunctionSignaturesMap(abstractFile, moduleNameStr);

        // Find the declaration
        foreach (var declaration in parsedFile.Declarations)
        {
            if (declaration.Value is Declaration.FunctionDeclaration funcDecl)
            {
                var funcName = funcDecl.Function.Declaration.Value.Name.Value;

                if (funcName != declarationName)
                {
                    continue;
                }

                // Convert expression and arguments to abstract syntax
                var abstractExpression = SyntaxTypes.FromStil4mConcretized.ConvertExpressionNode(
                    funcDecl.Function.Declaration.Value.Expression);

                var abstractArguments = funcDecl.Function.Declaration.Value.Arguments
                    .Select(arg => new SyntaxTypes.Node<SyntaxTypes.Pattern>(
                        arg.Range,
                        SyntaxTypes.FromStil4mConcretized.Convert(arg.Value)))
                    .ToList();

                // Use TypeInference to infer the function type
                var (returnType, parameterTypes) = TypeInference.InferFunctionDeclarationType(
                    abstractExpression.Value,
                    abstractArguments,
                    moduleNameStr,
                    functionSignatures);

                // Build the full function type using TypeInference
                return TypeInference.BuildFunctionType(
                    abstractArguments,
                    parameterTypes,
                    returnType);
            }
        }

        throw new Exception($"Declaration '{declarationName}' not found in module");
    }
}
