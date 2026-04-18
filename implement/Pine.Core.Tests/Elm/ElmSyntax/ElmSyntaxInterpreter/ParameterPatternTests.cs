using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers deconstruction patterns that can appear in a function parameter position.
/// Each test propagates values obtained from the deconstruction into the final return value
/// and asserts via <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/>.
/// </summary>
public class ParameterPatternTests
{
    private static string Evaluate(string elmModuleText)
    {
        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var mainBody = InterpreterTestHelper.GetFunctionBody(declarations, "main");

        var result =
            ElmInterpreter.Interpret(mainBody, declarations).Extract(err => throw new System.Exception(err.ToString()));

        return ElmValue.RenderAsElmExpression(result).expressionString;
    }

    [Fact]
    public void Simple_tuple_deconstruction_in_parameter()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            swap ( a, b ) =
                ( b, a )


            main =
                swap ( 10, 20 )
            """;

        Evaluate(elmModuleText).Should().Be("[ 20, 10 ]");
    }

    [Fact]
    public void Nested_tuple_deconstruction_in_parameter()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            sumThree ( ( a, b ), c ) =
                Pine_builtin.int_add [ Pine_builtin.int_add [ a, b ], c ]


            main =
                sumThree ( ( 1, 2 ), 3 )
            """;

        Evaluate(elmModuleText).Should().Be("6");
    }

    [Fact]
    public void Choice_type_tag_deconstruction_in_parameter()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Pair
                = Pair Int Int


            addPair (Pair a b) =
                Pine_builtin.int_add [ a, b ]


            main =
                addPair (Pair 17 25)
            """;

        Evaluate(elmModuleText).Should().Be("42");
    }

    [Fact]
    public void Choice_type_tag_deconstruction_in_parameter_combined_with_as_pattern()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Pair
                = Pair Int Int


            describe ((Pair a b) as whole) =
                ( whole, a, b )


            main =
                describe (Pair 3 4)
            """;

        // The result is a 3-tuple whose first element is the original `Pair 3 4` value, preserved via
        // the outer `as` binding, and whose remaining two elements come from the deconstructed arguments.
        Evaluate(elmModuleText).Should().Be("(Pair 3 4, 3, 4)");
    }

    [Fact]
    public void Nested_tuple_deconstruction_in_parameter_with_inner_and_outer_as_patterns()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            describe ((( a, b ) as inner, c ) as outer) =
                ( outer, inner, a, b, c )


            main =
                describe ( ( 1, 2 ), 3 )
            """;

        Evaluate(elmModuleText).Should().Be("[ ([ 1, 2 ], 3), [ 1, 2 ], 1, 2, 3 ]");
    }

    [Fact]
    public void Record_pattern_deconstruction_in_parameter()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { x : Int, y : Int }


            sumCoords { x, y } =
                Pine_builtin.int_add [ x, y ]


            main =
                sumCoords (Point 11 31)
            """;

        Evaluate(elmModuleText).Should().Be("42");
    }

    [Fact]
    public void All_parameter_patterns_combined()
    {
        // Exercises every pattern kind still accepted in a parameter position that is not already
        // covered by the other tests: AllPattern (`_`), VarPattern (`x`), UnitPattern (`()`), and an
        // AsPattern aliasing a VarPattern (`(alias as asBind)`).
        var elmModuleText =
            """
            module Test exposing (..)


            collect _ x () (alias as asBind) =
                ( x, alias, asBind )


            main =
                collect 99 1 () 2
            """;

        Evaluate(elmModuleText).Should().Be("[ 1, 2, 2 ]");
    }
}
