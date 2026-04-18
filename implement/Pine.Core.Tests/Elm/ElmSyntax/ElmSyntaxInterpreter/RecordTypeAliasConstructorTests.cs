using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers application of record type alias constructors. Each test parses the type alias
/// declaration from an Elm module and then evaluates one or more constructor expressions via
/// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{Pine.Core.CodeAnalysis.DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>.
/// Elm semantics: parameter order corresponds to field order in the alias declaration; the
/// resulting record's fields are sorted alphabetically.
/// Only full application is exercised here; partial application is covered in
/// <see cref="FunctionApplicationPartialTests"/>.
/// </summary>
public class RecordTypeAliasConstructorTests
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
    public void Record_constructor_with_alphabetical_field_order()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { x : Int, y : Int }
            """;

        Evaluate(elmModuleText, "Point 10 20")
            .Should().Be(
            new ElmValue.ElmRecord(
                [
                ("x", ElmValue.Integer(10)),
                ("y", ElmValue.Integer(20)),
                ]));
    }

    [Fact]
    public void Record_constructor_with_reverse_alphabetical_field_order()
    {
        // type alias Point = { y : Int, x : Int }
        // First argument binds to 'y' (first field in alias declaration),
        // second argument binds to 'x' (second field in alias declaration).
        // The resulting record's fields are sorted alphabetically.
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { y : Int, x : Int }
            """;

        Evaluate(elmModuleText, "Point 10 20")
            .Should().Be(
            new ElmValue.ElmRecord(
                [
                ("x", ElmValue.Integer(20)),
                ("y", ElmValue.Integer(10)),
                ]));
    }

    [Fact]
    public void Record_constructor_with_three_non_alphabetical_fields()
    {
        // type alias Rec = { z : Int, a : Int, m : Int }
        // Rec 1 2 3 binds: z = 1, a = 2, m = 3
        // Result record's fields are sorted alphabetically: a, m, z.
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Rec =
                { z : Int, a : Int, m : Int }
            """;

        Evaluate(elmModuleText, "Rec 1 2 3")
            .Should().Be(
            new ElmValue.ElmRecord(
                [
                ("a", ElmValue.Integer(2)),
                ("m", ElmValue.Integer(3)),
                ("z", ElmValue.Integer(1)),
                ]));
    }
}
