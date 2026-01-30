using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Tests for the lambda lifting compilation stage.
/// Lambda lifting transforms closures into top-level functions by making captured variables explicit parameters.
/// 
/// These tests define the expected behavior based on the design document:
/// implement/Pine.Core/Elm/ElmCompilerInDotnet/closure-implementation-designs.md
/// 
/// Design rules:
/// - Naming convention: containingFunction__lifted__lambdaIdentifier
/// - Zero captured bindings: no extra first parameter (lifted function has same signature as original lambda)
/// - Single captured binding: plain parameter (e.g., `f`)
/// - Multiple captured bindings: tuple parameter, ordered alphabetically (e.g., `( a, b, c )`)
/// - Lifted functions appear AFTER the originating function declaration
/// </summary>
public class LambdaLiftingTests
{
    private static File ParseModuleText(string moduleText)
    {
        var concreteSyntax =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Failed parsing: " + err));

        return FromFullSyntaxModel.Convert(concreteSyntax);
    }

    private static string LiftAndFormat(string inputModuleText)
    {
        var parsedModule = ParseModuleText(inputModuleText);

        var liftedModule = LambdaLifting.LiftLambdas(parsedModule);

        // Use FormatToString directly which formats and renders in one step
        return Avh4Format.FormatToString(
            ToFullSyntaxModel.Convert(liftedModule));
    }

    /// <summary>
    /// The simplest possible scenario requiring lambda lifting:
    /// A let function that captures a single variable from its enclosing scope.
    /// Single capture uses plain parameter, not tuple.
    /// </summary>
    [Fact]
    public void Simplest_closure_single_capture_in_let_function()
    {
        // lambda captures 'n' from enclosing scope
        var inputModuleText =
            """"
            module Test exposing (..)


            addN : Int -> Int -> Int
            addN n x =
                let
                    add y = y + n
                in
                add x
            """";

        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            addN : Int -> Int -> Int
            addN n x =
                let
                    add =
                        addN__lifted__add n
                in
                add x


            addN__lifted__add n y =
                y + n
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    [Fact]
    public void Simplest_closure_single_capture_in_let_decl_lambda()
    {
        // lambda captures 'n' from enclosing scope
        var inputModuleText =
            """"
            module Test exposing (..)


            addN : Int -> Int -> Int
            addN n x =
                let
                    add = \y -> y + n
                in
                add x
            """";

        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            addN : Int -> Int -> Int
            addN n x =
                let
                    add =
                        addN__lifted__add n
                in
                add x


            addN__lifted__add n y =
                y + n
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    [Fact]
    public void Example1_simple_lambda_with_single_capture()
    {
        var inputModuleText =
            """"
            module Example exposing (map)


            map : (a -> b) -> List a -> List b
            map f xs =
                foldr (\x acc -> f x :: acc) [] xs
            """";

        // Expected output after lambda lifting
        // Note: Single capture uses plain parameter 'f', not tuple '(f)'
        // Lifted function appears AFTER the originating function
        // Lifted functions don't have type signatures (they're generated without them)
        // Note: Parentheses around (a -> b) are preserved after round-trip through abstract syntax
        var expectedOutputModuleText =
            """"
            module Example exposing (map)


            map : (a -> b) -> List a -> List b
            map f xs =
                foldr (map__lifted__lambda1 f) [] xs


            map__lifted__lambda1 f x acc =
                f x :: acc
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    [Fact]
    public void Lambda_with_multiple_captures()
    {
        var inputModuleText =
            """"
            module Example exposing (transform)


            transform : Int -> Int -> List Int -> List Int
            transform offset multiplier items =
                List.map (\x -> (x + offset) * multiplier) items
            """";

        // Note: Multiple captures use tuple, ordered alphabetically: (multiplier, offset)
        // Lifted function appears AFTER the originating function
        // Lifted functions don't have type signatures (they're generated without them)
        var expectedOutputModuleText =
            """"
            module Example exposing (transform)


            transform : Int -> Int -> List Int -> List Int
            transform offset multiplier items =
                List.map (transform__lifted__lambda1 ( multiplier, offset )) items


            transform__lifted__lambda1 ( multiplier, offset ) x =
                (x + offset) * multiplier
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    [Fact]
    public void Nested_lambdas()
    {
        var inputModuleText =
            """"
            module Example exposing (compose)


            compose : (b -> c) -> (a -> b) -> List a -> List c
            compose g f items =
                List.map (\x -> g (f x)) items
            """";

        // Note: Multiple captures use tuple, ordered alphabetically: (f, g)
        // Lifted function appears AFTER the originating function
        // Lifted functions don't have type signatures (they're generated without them)
        // Note: Parentheses around function types are preserved
        var expectedOutputModuleText =
            """"
            module Example exposing (compose)


            compose : (b -> c) -> (a -> b) -> List a -> List c
            compose g f items =
                List.map (compose__lifted__lambda1 ( f, g )) items


            compose__lifted__lambda1 ( f, g ) x =
                g (f x)
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    [Fact]
    public void Lambda_in_let_expression()
    {
        var inputModuleText =
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

        // Note: Single capture uses plain parameter 'threshold', not tuple '(threshold)'
        // Lifted function uses "filterFn" as lambda identifier since it's a named let-binding
        // Lifted function appears AFTER the originating function
        // Lifted functions don't have type signatures (they're generated without them)
        var expectedOutputModuleText =
            """"
            module Example exposing (process)


            process : Int -> List Int -> List Int
            process n items =
                let
                    threshold =
                        n * 2

                    filterFn =
                        process__lifted__filterFn threshold
                in
                List.filter filterFn items


            process__lifted__filterFn threshold x =
                x > threshold
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// No captures (closed lambda) - lifted without any extra first parameter.
    /// </summary>
    [Fact]
    public void No_captures_closed_lambda()
    {
        var inputModuleText =
            """"
            module Example exposing (double)


            double : List Int -> List Int
            double items =
                List.map (\x -> x * 2) items
            """";

        // Note: No captures - lifted function has no extra first parameter
        // Just use the lifted function directly without any arguments for captures
        // Lifted function appears AFTER the originating function
        // Lifted functions don't have type signatures (they're generated without them)
        var expectedOutputModuleText =
            """"
            module Example exposing (double)


            double : List Int -> List Int
            double items =
                List.map double__lifted__lambda1 items


            double__lifted__lambda1 x =
                x * 2
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// Lambda that captures from nested scopes (both function parameter and let binding).
    /// Multiple captures use tuple, ordered alphabetically.
    /// </summary>
    [Fact]
    public void Lambda_captures_from_nested_scopes()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            compute : Int -> Int -> Int
            compute a b =
                let
                    c = a + b
                in
                (\x -> x * a + c) 10
            """";

        // Captures 'a' (function param) and 'c' (let binding), ordered alphabetically
        // Lifted function appears AFTER the originating function
        // Lifted functions don't have type signatures (they're generated without them)
        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            compute : Int -> Int -> Int
            compute a b =
                let
                    c =
                        a + b
                in
                (compute__lifted__lambda1 ( a, c )) 10


            compute__lifted__lambda1 ( a, c ) x =
                x * a + c
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// Multiple lambdas in the same function should get unique identifiers.
    /// Single captures use plain parameter, not tuple.
    /// </summary>
    [Fact]
    public void Multiple_lambdas_get_unique_identifiers()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            processBoth : Int -> List Int -> (List Int, List Int)
            processBoth n items =
                ( List.map (\x -> x + n) items
                , List.filter (\x -> x > n) items
                )
            """";

        // Expected output with unique identifiers lambda1 and lambda2
        // Single captures use plain parameter 'n', not tuple '(n)'
        // Lifted functions appear AFTER the originating function
        // Lifted functions don't have type signatures (they're generated without them)
        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            processBoth : Int -> List Int -> ( List Int, List Int )
            processBoth n items =
                ( List.map (processBoth__lifted__lambda1 n) items, List.filter (processBoth__lifted__lambda2 n) items )


            processBoth__lifted__lambda1 n x =
                x + n


            processBoth__lifted__lambda2 n x =
                x > n
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// A single recursive locally declared function to be lifted.
    /// The function calls itself, so it captures itself from scope.
    /// </summary>
    [Fact]
    public void Single_recursive_local_function_to_be_lifted()
    {
        // recursive local function 'factorial'
        var inputModuleText =
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

        // Expected output after lambda lifting
        // The recursive function 'factorial' is lifted. Since it has no captures from
        // the enclosing scope (other than itself which becomes the lifted function),
        // it has no extra parameter.
        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            computeFactorial : Int -> Int
            computeFactorial n =
                let
                    factorial =
                        computeFactorial__lifted__factorial
                in
                factorial n


            computeFactorial__lifted__factorial x =
                if x <= 1 then
                    1

                else
                    x * computeFactorial__lifted__factorial (x - 1)
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// Two independent functions declared in a let block to be lifted.
    /// Neither function references the other.
    /// </summary>
    [Fact]
    public void Two_independent_local_functions_to_be_lifted()
    {
        var inputModuleText =
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

        // Expected output after lambda lifting
        // Both functions are lifted independently, with no captures
        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            process : Int -> Int -> ( Int, Int )
            process a b =
                let
                    double =
                        process__lifted__double

                    triple =
                        process__lifted__triple
                in
                ( double a, triple b )


            process__lifted__double x =
                x * 2


            process__lifted__triple y =
                y * 3
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// Two functions declared in a let block to be lifted, where one invokes the other.
    /// The second function 'applyTwice' calls 'double'.
    /// </summary>
    [Fact]
    public void Two_local_functions_where_one_invokes_the_other()
    {
        var inputModuleText =
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

        // Expected output after lambda lifting
        // 'double' is lifted with no captures
        // 'applyTwice' references 'double' but since 'double' is also being lifted
        // from the same let block, we reference the lifted version directly
        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            compute : Int -> Int
            compute n =
                let
                    double =
                        compute__lifted__double

                    applyTwice =
                        compute__lifted__applyTwice
                in
                applyTwice n


            compute__lifted__double x =
                x * 2


            compute__lifted__applyTwice y =
                compute__lifted__double (compute__lifted__double y)
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }

    /// <summary>
    /// Two functions declared in a let block to be lifted, which are mutually recursive.
    /// 'isEven' calls 'isOdd' and vice versa.
    /// Since the invoked functions are known at compile time, they directly reference
    /// the lifted representations instead of passing them as arguments.
    /// </summary>
    [Fact]
    public void Two_mutually_recursive_local_functions()
    {
        var inputModuleText =
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

        // Expected output after lambda lifting
        // Since both functions are lifted from the same let block and their lifted names
        // are known at compile time, they reference each other directly without captures
        var expectedOutputModuleText =
            """"
            module Test exposing (..)


            checkEven : Int -> Bool
            checkEven n =
                let
                    isEven =
                        checkEven__lifted__isEven

                    isOdd =
                        checkEven__lifted__isOdd
                in
                isEven n


            checkEven__lifted__isEven x =
                if x == 0 then
                    True

                else
                    checkEven__lifted__isOdd (x - 1)


            checkEven__lifted__isOdd y =
                if y == 0 then
                    False

                else
                    checkEven__lifted__isEven (y - 1)
            """";

        var result = LiftAndFormat(inputModuleText);
        result.Trim().Should().Be(expectedOutputModuleText.Trim());
    }
}
