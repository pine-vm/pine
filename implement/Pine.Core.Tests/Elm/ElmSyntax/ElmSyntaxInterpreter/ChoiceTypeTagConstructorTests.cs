using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers application of choice type tag constructors. Each test parses the type declarations
/// from an Elm module and then evaluates one or more constructor expressions via
/// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{Core.CodeAnalysis.DeclQualifiedName, Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>.
/// Only full application is exercised here; partial application is covered in
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
}
