using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers application of choice type tag constructors. Each test parses the type declarations
/// from an Elm module and then evaluates one or more constructor expressions via
/// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{Pine.Core.CodeAnalysis.DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>.
/// Only full application is exercised here; partial application is covered in
/// <see cref="FunctionApplicationPartialTests"/>.
/// </summary>
public class ChoiceTypeTagConstructorTests
{
    /// <summary>
    /// Parses <paramref name="elmModuleText"/> for its top-level declarations and then evaluates
    /// <paramref name="expression"/> against those declarations. Returns the resulting Elm value.
    /// </summary>
    private static ElmValue Evaluate(string elmModuleText, string expression)
    {
        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        return
            ElmInterpreter.ParseAndInterpret(expression, declarations)
            .Extract(err => throw new System.Exception(err.ToString()));
    }

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

        Evaluate(elmModuleText, "Green")
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

        Evaluate(elmModuleText, "Just 17")
            .Should().Be(ElmValue.TagInstance("Just", [ElmValue.Integer(17)]));
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

        Evaluate(elmModuleText, "Pair 1 2")
            .Should().Be(
            ElmValue.TagInstance(
                "Pair",
                [ElmValue.Integer(1), ElmValue.Integer(2)]));
    }
}
