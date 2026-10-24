using AwesomeAssertions;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Linq;
using Xunit;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>Structural lifting invariants over the production compiler model, independent of rendered names.</summary>
public class LambdaLiftingTests
{
    private static void AssertLifted(string source, int expectedHelpers)
    {
        var file =
            ConvertFromConcrete.FromFile(
                ElmSyntaxParser.ParseModuleText(source)
                .Extract(error => throw new InvalidOperationException(error.ToString())));

        var canonical = Core.Elm.ElmCompilerInDotnet.ElmCompiler.FlattenModulesToDeclarationDictionary([file]);
        var original = Model.FromCanonicalSyntax.Convert(canonical);
        var program = Model.LambdaLifting.Lift(original);
        Model.ProgramValidation.Validate(program);
        program.Functions.Keys.OfType<Model.FunctionId.Lifted>().Should().HaveCount(expectedHelpers);

        foreach (var id in original.Functions.Keys)
            program.Functions.Should().ContainKey(id);

        Model.LambdaLifting.Lift(program).Functions.Should().BeEquivalentTo(program.Functions);
    }

    [Fact]
    public void Simplest_closure_single_capture_in_let_function()
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

        AssertLifted(source, 1);
    }

    [Fact]
    public void Simplest_closure_single_capture_in_let_decl_lambda()
    {
        var source =
            """"
            module Test exposing (..)


            addN : Int -> Int -> Int
            addN n x =
                let
                    add = \y -> y + n
                in
                add x
            """";

        AssertLifted(source, 1);
    }

    [Fact]
    public void Example1_simple_lambda_with_single_capture()
    {
        var source =
            """"
            module Example exposing (map)


            map : (a -> b) -> List a -> List b
            map f xs =
                foldr (\x acc -> f x :: acc) [] xs
            """";

        AssertLifted(source, 1);
    }

    [Fact]
    public void Lambda_with_multiple_captures()
    {
        var source =
            """"
            module Example exposing (transform)


            transform : Int -> Int -> List Int -> List Int
            transform offset multiplier items =
                List.map (\x -> (x + offset) * multiplier) items
            """";

        AssertLifted(source, 1);
    }

    [Fact]
    public void Nested_lambdas()
    {
        var source =
            """"
            module Example exposing (compose)


            compose : (b -> c) -> (a -> b) -> List a -> List c
            compose g f items =
                List.map (\x -> g (f x)) items
            """";

        AssertLifted(source, 1);
    }

    [Fact]
    public void Lambda_in_let_expression()
    {
        var source =
            """"
            module Example exposing (process)


            process : Int -> List Int -> List Int
            process n items =
                let
                    threshold =
                        n * 2

                    filterFn =
                        \x -> x > threshold
                in
                List.filter filterFn items
            """";

        AssertLifted(source, 1);
    }

    [Fact]
    public void No_captures_closed_lambda()
    {
        var source =
            """"
            module Example exposing (double)


            double : List Int -> List Int
            double items =
                List.map (\x -> x * 2) items
            """";

        AssertLifted(source, 1);
    }

    [Fact]
    public void Lambda_captures_from_nested_scopes()
    {
        var source =
            """"
            module Test exposing (..)


            compute : Int -> Int -> Int
            compute a b =
                let
                    c = a + b
                in
                (\x -> x * a + c) 10
            """";

        AssertLifted(source, 1);
    }

    [Fact]
    public void Multiple_lambdas_get_unique_identifiers()
    {
        var source =
            """"
            module Test exposing (..)


            processBoth : Int -> List Int -> (List Int, List Int)
            processBoth n items =
                ( List.map (\x -> x + n) items
                , List.filter (\x -> x > n) items
                )
            """";

        AssertLifted(source, 2);
    }

    [Fact]
    public void Single_recursive_local_function_to_be_lifted()
    {
        var source =
            """"
            module Test exposing (..)


            computeFactorial : Int -> Int
            computeFactorial n =
                let
                    factorial x =
                        if x <= 1 then
                            1
                        else
                            x * factorial (x - 1)
                in
                factorial n
            """";

        AssertLifted(source, 1);
    }

    [Fact]
    public void Two_independent_local_functions_to_be_lifted()
    {
        var source =
            """"
            module Test exposing (..)


            process : Int -> Int -> ( Int, Int )
            process a b =
                let
                    double x =
                        x * 2

                    triple y =
                        y * 3
                in
                ( double a, triple b )
            """";

        AssertLifted(source, 2);
    }

    [Fact]
    public void Two_local_functions_where_one_invokes_the_other()
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

        AssertLifted(source, 2);
    }

    [Fact]
    public void Two_mutually_recursive_local_functions()
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

        AssertLifted(source, 2);
    }

    [Fact]
    public void Lambda_in_constructor_application_arg_with_single_named_pattern_param_capture()
    {
        var source =
            """"
            module Test exposing (..)


            type Wrap a
                = Wrap a


            applyWrapped : Wrap Int -> Wrap Int
            applyWrapped (Wrap g) =
                Wrap ((\x -> x + g) 1)
            """";

        AssertLifted(source, 1);
    }

    [Fact]
    public void Lambda_in_constructor_application_arg_with_named_pattern_param_capture()
    {
        var source =
            """"
            module Parser.Advanced exposing (..)


            type Parser context problem value
                = Parser (State context -> PStep context problem value)


            type State context
                = State String Int


            type PStep context problem value
                = Good Bool value (State context)
                | Bad Bool (Bag context problem)


            type Bag context problem
                = Empty


            map2 : (a -> b -> value) -> Parser c x a -> Parser c x b -> Parser c x value
            map2 func (Parser parseA) (Parser parseB) =
                Parser
                    (\s0 ->
                        case parseA s0 of
                            Bad p x ->
                                Bad p x

                            Good p1 a s1 ->
                                case parseB s1 of
                                    Bad p2 x ->
                                        Bad (p1 || p2) x

                                    Good p2 b s2 ->
                                        Good (p1 || p2) (func a b) s2
                    )
            """";

        AssertLifted(source, 1);
    }
}
