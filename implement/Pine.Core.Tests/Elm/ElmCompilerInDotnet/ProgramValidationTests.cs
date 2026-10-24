using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Immutable;
using Xunit;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>Validation checks structural scope and references, never synthetic source-name conventions.</summary>
public class ProgramValidationTests
{
    private static readonly Model.FunctionId Owner =
        new Model.FunctionId.Source(DeclQualifiedName.Create(["Test"], "root"));

    private static Model.Program Program(Model.Expression body) =>
        new(
            ImmutableDictionary<Model.FunctionId, Model.Function>.Empty.Add(Owner, new([], body)),
            []);

    [Fact]
    public void Validator_rejects_multiple_definitions_of_one_lexical_identity()
    {
        var id = new Model.BindingId.Local(Owner, 0);

        var program =
            new Model.Program(
                ImmutableDictionary<Model.FunctionId, Model.Function>.Empty.Add(
                    Owner,
                    new(
                        [
                        new Model.Pattern.VarPattern(id),
                        new Model.Pattern.VarPattern(id)
                        ],
                        new Model.Expression.Literal(PineValue.EmptyList))),
                []);

        var validate = () => Model.ProgramValidation.Validate(program);
        validate.Should().Throw<InvalidOperationException>().WithMessage("*more than once*");
    }

    [Fact]
    public void Validator_rejects_free_captured_binding()
    {
        var program =
            Program(
                new Model.Expression.ReferenceExpression(new Model.Reference.Local(new Model.BindingId.Local(Owner, 0))));

        var validate = () => Model.ProgramValidation.Validate(program);
        validate.Should().Throw<InvalidOperationException>().WithMessage("*unbound lexical*");
    }

    [Fact]
    public void Validator_rejects_dangling_function_reference()
    {
        var program =
            Program(
                new Model.Expression.ReferenceExpression(
                    new Model.Reference.Function(new Model.FunctionId.Lifted(Owner, 0))));

        var validate = () => Model.ProgramValidation.Validate(program);
        validate.Should().Throw<InvalidOperationException>().WithMessage("*missing function*");
    }

    [Fact]
    public void Validator_rejects_residual_lambda_in_nested_expression()
    {
        var program =
            Program(
                new Model.Expression.List(
                    [
                    new Model.Expression.Lambda(
                        new Model.FunctionId.Lifted(Owner, 0),
                        [],
                        new Model.Expression.Literal(PineValue.EmptyList))
                    ]));

        var validate = () => Model.ProgramValidation.Validate(program);
        validate.Should().Throw<InvalidOperationException>().WithMessage("*unlifted function*");
    }

    [Fact]
    public void Validator_rejects_residual_local_function()
    {
        var binding = new Model.BindingId.Local(Owner, 0);

        var program =
            Program(
                new Model.Expression.Let(
                    [
                    new(
                        new Model.Pattern.VarPattern(binding),
                        new Model.Expression.Lambda(
                            new Model.FunctionId.Lifted(Owner, 0),
                            [],
                            new Model.Expression.Literal(PineValue.EmptyList)))
                    ],
                    new Model.Expression.Literal(PineValue.EmptyList)));

        var validate = () => Model.ProgramValidation.Validate(program);
        validate.Should().Throw<InvalidOperationException>().WithMessage("*unlifted function*");
    }

    [Fact]
    public void Validator_does_not_interpret_source_names_as_generated_identity()
    {
        ValidateSource("module Test exposing (..)\nroot__lifted__lambda1 x = x\nroot = root__lifted__lambda1 42");
    }

    private static void ValidateSource(string source)
    {
        var file =
            ConvertFromConcrete.FromFile(
                ElmSyntaxParser.ParseModuleText(source)
                .Extract(error => throw new InvalidOperationException(error.ToString())));

        var declarations = Core.Elm.ElmCompilerInDotnet.ElmCompiler.FlattenModulesToDeclarationDictionary([file]);
        Model.ProgramValidation.Validate(Model.LambdaLifting.Lift(Model.FromCanonicalSyntax.Convert(declarations)));
    }

    [Fact]
    public void Validator_accepts_simplest_closure_lifted_output()
    {
        var source =
            """"
            module Test exposing (..)


            addN : Int -> Int -> Int
            addN n x =
                let
                    add y = y + n
                in
                add x
            """";

        ValidateSource(source);
    }

    [Fact]
    public void Validator_accepts_two_local_functions_lifted_output()
    {
        var source =
            """"
            module Test exposing (..)


            compute : Int -> Int
            compute n =
                let
                    double x =
                        x * 2

                    applyTwice y =
                        double (double y)
                in
                applyTwice n
            """";

        ValidateSource(source);
    }

    [Fact]
    public void Validator_accepts_mutually_recursive_lifted_output()
    {
        var source =
            """"
            module Test exposing (..)


            checkEven : Int -> Bool
            checkEven n =
                let
                    isEven x =
                        if x == 0 then
                            True
                        else
                            isOdd (x - 1)

                    isOdd y =
                        if y == 0 then
                            False
                        else
                            isEven (y - 1)
                in
                isEven n
            """";

        ValidateSource(source);
    }

    [Fact]
    public void Validator_accepts_captured_sibling_lifted_output()
    {
        var source =
            """"
            module Test exposing (..)


            run : Int -> List ( Int, String )
            run n =
                let
                    sl =
                        ( n, "captured" )

                    inner _ =
                        Just sl

                    outer name =
                        case inner name of
                            Just s ->
                                [ s ]

                            Nothing ->
                                []
                in
                outer 1
            """";

        ValidateSource(source);
    }

    [Fact]
    public void Validator_accepts_anonymous_lambda_passed_to_higher_order_function()
    {
        var source =
            """"
            module Test exposing (..)


            applyToEach f items =
                case items of
                    [] ->
                        []

                    first :: rest ->
                        f first :: applyToEach f rest


            doubleAll input =
                applyToEach (\x -> x + x) input


            scaleAll factor input =
                applyToEach (\x -> x * factor) input
            """";

        ValidateSource(source);
    }

    [Fact]
    public void Validator_accepts_module_without_any_lifted_functions()
    {
        var source =
            """"
            module Test exposing (..)


            identity : a -> a
            identity x =
                x
            """";

        ValidateSource(source);
    }
}
