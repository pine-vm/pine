using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers application of choice type tag constructors. Each test parses the type declarations
/// from an Elm module and then evaluates one or more constructor expressions via
/// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{Core.CodeAnalysis.DeclQualifiedName, Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>.
/// Both full application and partial application (in batches of one or two arguments,
/// for constructor arities up to three) are exercised here; partial application of
/// arbitrary user-defined functions is additionally covered in
/// <see cref="FunctionApplicationPartialTests"/>.
/// </summary>
public class ChoiceTypeTagConstructorTests
{
    [Fact]
    public void Nullary_tag_constructor()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Color
                = Red
                | Green
                | Blue
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrash("Green", elmModuleText)
            .Should().Be(ElmValue.TagInstance("Green", []));
    }

    [Fact]
    public void Unary_tag_constructor_with_integer_argument()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered("Just 17", elmModuleText)
            .Should().Be("Just 17");
    }

    [Fact]
    public void Binary_tag_constructor_with_two_arguments()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Pair a b
                = Pair a b
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered("Pair 1 2", elmModuleText)
            .Should().Be("Pair 1 2");
    }

    [Fact]
    public void Binary_tag_constructor_partial_then_remaining_argument()
    {
        // Pair is a two-argument tag. The constructor is first applied to one argument,
        // producing a function value, which is then applied to the remaining argument
        // at a different call site.
        var elmModuleText =
            """
            module Test exposing (..)


            type Pair a b
                = Pair a b
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                partial =
                    Pair 1
            in
            partial 2
            """,
            elmModuleText)
            .Should().Be("Pair 1 2");
    }

    [Fact]
    public void Ternary_tag_constructor_full_application()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Triple a b c
                = Triple a b c
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered("Triple 1 2 3", elmModuleText)
            .Should().Be("Triple 1 2 3");
    }

    [Fact]
    public void Ternary_tag_constructor_partial_one_then_batch_of_two()
    {
        // Triple is a three-argument tag. First call site supplies one argument; the
        // resulting closure is then applied to the remaining two arguments as a single
        // batch at a later call site.
        var elmModuleText =
            """
            module Test exposing (..)


            type Triple a b c
                = Triple a b c
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                partial =
                    Triple 1
            in
            partial 2 3
            """,
            elmModuleText)
            .Should().Be("Triple 1 2 3");
    }

    [Fact]
    public void Ternary_tag_constructor_partial_batch_of_two_then_one()
    {
        // Triple is a three-argument tag. First call site supplies two arguments in a
        // single batch, producing a one-argument closure; the remaining argument is
        // supplied at a later call site.
        var elmModuleText =
            """
            module Test exposing (..)


            type Triple a b c
                = Triple a b c
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                partial =
                    Triple 1 2
            in
            partial 3
            """,
            elmModuleText)
            .Should().Be("Triple 1 2 3");
    }

    [Fact]
    public void Ternary_tag_constructor_partial_one_at_a_time()
    {
        // Triple is a three-argument tag. Each argument is supplied at its own call site,
        // passing through three nested let-bindings.
        var elmModuleText =
            """
            module Test exposing (..)


            type Triple a b c
                = Triple a b c
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                f1 =
                    Triple 1
            in
            let
                f2 =
                    f1 2
            in
            f2 3
            """,
            elmModuleText)
            .Should().Be("Triple 1 2 3");
    }
}
