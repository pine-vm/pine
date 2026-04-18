using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>:
/// the caller passes the root expression as a string (to be parsed and evaluated) together with
/// a declarations dictionary resolving any referenced top-level bindings.
/// </summary>
public class ParseAndInterpretTests
{
    [Fact]
    public void ParseAndInterpret_integer_literal_returns_integer_value()
    {
        var result =
            ElmInterpreter.ParseAndInterpret(
                "42",
                System.Collections
                .Immutable.ImmutableDictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration>.Empty);

        result.Should().Be(Result<Pine.Core.Elm.ElmSyntax.ElmInterpretationError, ElmValue>.ok(ElmValue.Integer(42)));
    }

    [Fact]
    public void ParseAndInterpret_Pine_builtin_application()
    {
        var result =
            ElmInterpreter.ParseAndInterpret(
                "Pine_builtin.int_add [ 3, 4 ]",
                System.Collections
                .Immutable.ImmutableDictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration>.Empty);

        result.Should().Be(Result<Pine.Core.Elm.ElmSyntax.ElmInterpretationError, ElmValue>.ok(ElmValue.Integer(7)));
    }

    [Fact]
    public void ParseAndInterpret_references_declaration_from_dictionary()
    {
        var declarations =
            InterpreterTestHelper.ParseDeclarations(
                """
                module Test exposing (..)


                answer =
                    Pine_builtin.int_mul [ 6, 7 ]
                """);

        var result = ElmInterpreter.ParseAndInterpret("answer", declarations);

        result.Should().Be(Result<Pine.Core.Elm.ElmSyntax.ElmInterpretationError, ElmValue>.ok(ElmValue.Integer(42)));
    }

    [Fact]
    public void ParseAndInterpret_multi_line_expression_with_let()
    {
        var result =
            ElmInterpreter.ParseAndInterpret(
                """
                let
                    a =
                        10

                    b =
                        20
                in
                Pine_builtin.int_add [ a, b ]
                """,
                System.Collections
                .Immutable.ImmutableDictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration>.Empty);

        result.Should().Be(Result<Pine.Core.Elm.ElmSyntax.ElmInterpretationError, ElmValue>.ok(ElmValue.Integer(30)));
    }

    [Fact]
    public void ParseAndInterpret_invalid_syntax_returns_error()
    {
        var result =
            ElmInterpreter.ParseAndInterpret(
                "let in",
                System.Collections
                .Immutable.ImmutableDictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration>.Empty);

        result.IsErrOrNull().Should().NotBeNull();
    }
}
