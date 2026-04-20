using AwesomeAssertions;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers application of record type alias constructors. Each test parses the type alias
/// declaration from an Elm module and then evaluates one or more constructor expressions via
/// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{Core.CodeAnalysis.DeclQualifiedName, Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>.
/// Elm semantics: parameter order corresponds to field order in the alias declaration; the
/// resulting record's fields are sorted alphabetically.
/// Both full application and partial application (in batches of one or two arguments,
/// for constructor arities up to three) are exercised here; partial application of
/// arbitrary user-defined functions is additionally covered in
/// <see cref="FunctionApplicationPartialTests"/>.
/// </summary>
public class RecordTypeAliasConstructorTests
{
    [Fact]
    public void Record_constructor_with_alphabetical_field_order()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { x : Int, y : Int }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered("Point 10 20", elmModuleText)
            .Should().Be("{ x = 10, y = 20 }");
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

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered("Point 10 20", elmModuleText)
            .Should().Be("{ x = 20, y = 10 }");
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

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered("Rec 1 2 3", elmModuleText)
            .Should().Be("{ a = 2, m = 3, z = 1 }");
    }

    [Fact]
    public void Record_constructor_two_fields_partial_then_remaining_argument()
    {
        // Point's constructor takes two arguments. The first call site supplies one
        // argument, producing a function value; the remaining argument is supplied at a
        // later call site.
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { x : Int, y : Int }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                partial =
                    Point 10
            in
            partial 20
            """,
            elmModuleText)
            .Should().Be("{ x = 10, y = 20 }");
    }

    [Fact]
    public void Record_constructor_three_fields_partial_one_then_batch_of_two()
    {
        // Rec's constructor takes three arguments (in declaration-field order: z, a, m).
        // First call site supplies one argument; the resulting closure is then applied
        // to the remaining two arguments as a single batch at a later call site.
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Rec =
                { z : Int, a : Int, m : Int }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                partial =
                    Rec 1
            in
            partial 2 3
            """,
            elmModuleText)
            .Should().Be("{ a = 2, m = 3, z = 1 }");
    }

    [Fact]
    public void Record_constructor_three_fields_partial_batch_of_two_then_one()
    {
        // Rec's constructor takes three arguments. First call site supplies two arguments
        // as a single batch, producing a one-argument closure; the remaining argument is
        // supplied at a later call site.
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Rec =
                { z : Int, a : Int, m : Int }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                partial =
                    Rec 1 2
            in
            partial 3
            """,
            elmModuleText)
            .Should().Be("{ a = 2, m = 3, z = 1 }");
    }

    [Fact]
    public void Record_constructor_three_fields_partial_one_at_a_time()
    {
        // Rec's constructor takes three arguments. Each argument is supplied at its own
        // call site, passing through three nested let-bindings.
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Rec =
                { z : Int, a : Int, m : Int }
            """;

        InterpreterTestHelper.EvaluateInModuleOrCrashRendered(
            """
            let
                f1 =
                    Rec 1
            in
            let
                f2 =
                    f1 2
            in
            f2 3
            """,
            elmModuleText)
            .Should().Be("{ a = 2, m = 3, z = 1 }");
    }
}
