using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers <c>case … of</c> expressions for every Elm pattern kind that can appear in a
/// case arm. Each test defines an Elm module with the helper functions used by the test, then
/// invokes <see cref="Evaluate(string, string)"/> repeatedly with different scrutinee
/// arguments encoded directly in the expression string, exercising every arm at least once.
/// The result of each invocation is converted to an Elm-expression string via
/// <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/> so that both the conditional choice
/// of the matching arm and the propagation of values bound by patterns can be observed.
/// <para />
/// In addition, <see cref="No_arm_matches_produces_runtime_error_with_rendered_value_and_stack_trace"/>
/// asserts the exact text of the runtime error returned when no arm matches: a top line
/// containing the rendered scrutinee value, followed by the Elm call stack.
/// </summary>
public class CaseBlockTests
{
    /// <summary>
    /// Parses <paramref name="elmModuleText"/> for its top-level declarations and then evaluates
    /// <paramref name="expression"/> against those declarations using
    /// <see cref="ElmInterpreter.ParseAndInterpret(string, System.Collections.Generic.IReadOnlyDictionary{Pine.Core.CodeAnalysis.DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration})"/>.
    /// Returns the rendered Elm-expression form of the resulting value.
    /// </summary>
    private static string Evaluate(string elmModuleText, string expression)
    {
        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var result =
            ElmInterpreter.ParseAndInterpret(expression, declarations)
            .Extract(err => throw new System.Exception(err.ToString()));

        return ElmValue.RenderAsElmExpression(result).expressionString;
    }

    [Fact]
    public void Uncons_versus_empty_list()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            classify xs =
                case xs of
                    [] ->
                        ( "empty", -1 )

                    head :: tail ->
                        ( "cons", Pine_builtin.length tail )
            """;

        Evaluate(elmModuleText, "classify []")
            .Should().Be("(\"empty\", -1)");

        Evaluate(elmModuleText, "classify [ 7 ]")
            .Should().Be("(\"cons\", 0)");

        Evaluate(elmModuleText, "classify [ 1, 2, 3 ]")
            .Should().Be("(\"cons\", 2)");
    }

    [Fact]
    public void Nested_uncons()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            classify xs =
                case xs of
                    [] ->
                        ( "empty", 0 )

                    [ only ] ->
                        ( "single", only )

                    first :: second :: rest ->
                        ( "twoOrMore", Pine_builtin.int_add [ first, Pine_builtin.int_add [ second, Pine_builtin.length rest ] ] )
            """;

        Evaluate(elmModuleText, "classify []")
            .Should().Be("(\"empty\", 0)");

        Evaluate(elmModuleText, "classify [ 7 ]")
            .Should().Be("(\"single\", 7)");

        Evaluate(elmModuleText, "classify [ 10, 20 ]")
            .Should().Be("(\"twoOrMore\", 30)");

        Evaluate(elmModuleText, "classify [ 1, 2, 3, 4 ]")
            .Should().Be("(\"twoOrMore\", 5)");
    }

    [Fact]
    public void List_pattern_with_three_items_versus_other_list_deconstructions()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            classify xs =
                case xs of
                    [] ->
                        ( "empty", 0 )

                    [ a ] ->
                        ( "single", a )

                    [ a, b ] ->
                        ( "pair", Pine_builtin.int_add [ a, b ] )

                    [ a, b, c ] ->
                        ( "triple", Pine_builtin.int_add [ Pine_builtin.int_add [ a, b ], c ] )

                    a :: b :: rest ->
                        ( "many", Pine_builtin.length rest )
            """;

        Evaluate(elmModuleText, "classify []")
            .Should().Be("(\"empty\", 0)");

        Evaluate(elmModuleText, "classify [ 5 ]")
            .Should().Be("(\"single\", 5)");

        Evaluate(elmModuleText, "classify [ 3, 4 ]")
            .Should().Be("(\"pair\", 7)");

        Evaluate(elmModuleText, "classify [ 1, 2, 3 ]")
            .Should().Be("(\"triple\", 6)");

        Evaluate(elmModuleText, "classify [ 10, 20, 30, 40, 50 ]")
            .Should().Be("(\"many\", 3)");
    }

    [Fact]
    public void NamedPattern_with_shuffled_order_in_source_code()
    {
        // Arms appear in a non-canonical order to make sure the interpreter checks each
        // condition (tag name) rather than relying on declaration order in the type.
        var elmModuleText =
            """
            module Test exposing (..)


            type Shape
                = Circle Int
                | Square Int
                | Rect Int Int
                | Dot


            classify s =
                case s of
                    Rect w h ->
                        ( "rect", Pine_builtin.int_mul [ w, h ] )

                    Dot ->
                        ( "dot", 0 )

                    Square side ->
                        ( "square", Pine_builtin.int_mul [ side, side ] )

                    Circle r ->
                        ( "circle", r )
            """;

        Evaluate(elmModuleText, "classify Dot")
            .Should().Be("(\"dot\", 0)");

        Evaluate(elmModuleText, "classify (Circle 7)")
            .Should().Be("(\"circle\", 7)");

        Evaluate(elmModuleText, "classify (Square 3)")
            .Should().Be("(\"square\", 9)");

        Evaluate(elmModuleText, "classify (Rect 4 5)")
            .Should().Be("(\"rect\", 20)");
    }

    [Fact]
    public void Nested_combinations_of_uncons_and_NamedPattern()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            classify xs =
                case xs of
                    [] ->
                        ( "empty", 0 )

                    Nothing :: rest ->
                        ( "nothingFirst", Pine_builtin.length rest )

                    (Just first) :: (Just second) :: _ ->
                        ( "twoJusts", Pine_builtin.int_add [ first, second ] )

                    (Just only) :: rest ->
                        ( "justThenOther", Pine_builtin.int_add [ only, Pine_builtin.length rest ] )
            """;

        Evaluate(elmModuleText, "classify []")
            .Should().Be("(\"empty\", 0)");

        Evaluate(elmModuleText, "classify [ Nothing ]")
            .Should().Be("(\"nothingFirst\", 0)");

        Evaluate(elmModuleText, "classify [ Nothing, Just 9 ]")
            .Should().Be("(\"nothingFirst\", 1)");

        Evaluate(elmModuleText, "classify [ Just 10, Just 20 ]")
            .Should().Be("(\"twoJusts\", 30)");

        Evaluate(elmModuleText, "classify [ Just 10, Just 20, Just 30 ]")
            .Should().Be("(\"twoJusts\", 30)");

        Evaluate(elmModuleText, "classify [ Just 5, Nothing ]")
            .Should().Be("(\"justThenOther\", 6)");

        Evaluate(elmModuleText, "classify [ Just 5, Nothing, Nothing ]")
            .Should().Be("(\"justThenOther\", 7)");
    }

    [Fact]
    public void Int_pattern()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            classify n =
                case n of
                    0 ->
                        ( "zero", 0 )

                    1 ->
                        ( "one", 1 )

                    42 ->
                        ( "answer", 42 )

                    other ->
                        ( "other", other )
            """;

        Evaluate(elmModuleText, "classify 0")
            .Should().Be("(\"zero\", 0)");

        Evaluate(elmModuleText, "classify 1")
            .Should().Be("(\"one\", 1)");

        Evaluate(elmModuleText, "classify 42")
            .Should().Be("(\"answer\", 42)");

        Evaluate(elmModuleText, "classify 7")
            .Should().Be("(\"other\", 7)");

        Evaluate(elmModuleText, "classify -3")
            .Should().Be("(\"other\", -3)");
    }

    [Fact]
    public void Char_pattern()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            classify c =
                case c of
                    'a' ->
                        ( "a", c )

                    'z' ->
                        ( "z", c )

                    other ->
                        ( "other", other )
            """;

        Evaluate(elmModuleText, "classify 'a'")
            .Should().Be("(\"a\", 'a')");

        Evaluate(elmModuleText, "classify 'z'")
            .Should().Be("(\"z\", 'z')");

        Evaluate(elmModuleText, "classify 'm'")
            .Should().Be("(\"other\", 'm')");
    }

    [Fact]
    public void Tuple_pattern_containing_other_patterns_introducing_conditions()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            classify t =
                case t of
                    ( 0, x ) ->
                        ( "zeroLeft", x )

                    ( x, 0 ) ->
                        ( "zeroRight", x )

                    ( 1, 1 ) ->
                        ( "ones", 11 )

                    ( a, b ) ->
                        ( "general", Pine_builtin.int_add [ a, b ] )
            """;

        Evaluate(elmModuleText, "classify ( 0, 5 )")
            .Should().Be("(\"zeroLeft\", 5)");

        Evaluate(elmModuleText, "classify ( 5, 0 )")
            .Should().Be("(\"zeroRight\", 5)");

        Evaluate(elmModuleText, "classify ( 1, 1 )")
            .Should().Be("(\"ones\", 11)");

        Evaluate(elmModuleText, "classify ( 3, 4 )")
            .Should().Be("(\"general\", 7)");

        Evaluate(elmModuleText, "classify ( 0, 0 )")
            .Should().Be("(\"zeroLeft\", 0)");
    }

    [Fact]
    public void Nested_tuple_pattern_containing_literals_as_conditions_and_bindings()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            classify t =
                case t of
                    ( ( 0, x ), y ) ->
                        ( "innerZero", Pine_builtin.int_add [ x, y ] )

                    ( ( 1, x ), 0 ) ->
                        ( "oneZero", x )

                    ( ( n, x ), y ) ->
                        ( "general", Pine_builtin.int_add [ n, Pine_builtin.int_add [ x, y ] ] )
            """;

        Evaluate(elmModuleText, "classify ( ( 0, 10 ), 1 )")
            .Should().Be("(\"innerZero\", 11)");

        Evaluate(elmModuleText, "classify ( ( 1, 7 ), 0 )")
            .Should().Be("(\"oneZero\", 7)");

        Evaluate(elmModuleText, "classify ( ( 2, 3 ), 4 )")
            .Should().Be("(\"general\", 9)");

        Evaluate(elmModuleText, "classify ( ( 1, 7 ), 5 )")
            .Should().Be("(\"general\", 13)");
    }

    [Fact]
    public void Bindings_introduced_by_all_pattern_kinds_that_can_contain_other_patterns()
    {
        // Each arm exercises a different aggregating pattern kind and propagates a value bound
        // by an inner pattern (eventually a VarPattern) into the result.
        //   * NamedPattern wrapping VarPattern arguments
        //   * NamedPattern wrapping ListPattern wrapping VarPattern arguments
        //   * UnConsPattern wrapping VarPattern head and tail
        //   * AsPattern wrapping a TuplePattern of VarPatterns
        //   * ParenthesizedPattern around a NamedPattern with VarPattern arguments
        //   * RecordPattern binding field names
        var elmModuleText =
            """
            module Test exposing (..)


            type Wrapped a
                = Wrap a a


            type alias Point =
                { x : Int, y : Int }


            describe w =
                case w of
                    Wrap [ a, b, c ] _ ->
                        ( "wrapList", Pine_builtin.int_add [ a, Pine_builtin.int_add [ b, c ] ] )

                    Wrap (Just inner) _ ->
                        ( "wrapJust", inner )

                    Wrap _ _ ->
                        ( "wrapOther", 0 )


            describeList xs =
                case xs of
                    head :: tail ->
                        ( "cons", head, Pine_builtin.length tail )

                    _ ->
                        ( "empty", 0, 0 )


            describeAs t =
                case t of
                    (( a, b ) as whole) ->
                        ( whole, a, b )


            describeParens v =
                case v of
                    ((Just x)) ->
                        ( "parensJust", x )

                    _ ->
                        ( "parensOther", 0 )


            describeRecord p =
                case p of
                    { x, y } ->
                        ( "record", Pine_builtin.int_add [ x, y ] )


            type Maybe a
                = Nothing
                | Just a
            """;

        Evaluate(elmModuleText, "describe (Wrap [ 1, 2, 3 ] [ 9, 9, 9 ])")
            .Should().Be("(\"wrapList\", 6)");

        Evaluate(elmModuleText, "describe (Wrap (Just 100) (Just 200))")
            .Should().Be("(\"wrapJust\", 100)");

        Evaluate(elmModuleText, "describe (Wrap Nothing Nothing)")
            .Should().Be("(\"wrapOther\", 0)");

        Evaluate(elmModuleText, "describeList [ 7, 8, 9, 10 ]")
            .Should().Be("(\"cons\", 7, 3)");

        Evaluate(elmModuleText, "describeList []")
            .Should().Be("(\"empty\", 0, 0)");

        Evaluate(elmModuleText, "describeAs ( 11, 22 )")
            .Should().Be("([ 11, 22 ], 11, 22)");

        Evaluate(elmModuleText, "describeParens (Just 55)")
            .Should().Be("(\"parensJust\", 55)");

        Evaluate(elmModuleText, "describeParens Nothing")
            .Should().Be("(\"parensOther\", 0)");

        Evaluate(elmModuleText, "describeRecord (Point 4 38)")
            .Should().Be("(\"record\", 42)");
    }

    [Fact]
    public void Nested_case_blocks_where_inner_depends_on_outer_bindings()
    {
        // The inner case depends on `head` and `rest` introduced by the outer case's
        // uncons pattern, demonstrating that arm-bindings are visible inside nested case
        // expressions.
        var elmModuleText =
            """
            module Test exposing (..)


            classify xs =
                case xs of
                    [] ->
                        ( "empty", 0 )

                    head :: rest ->
                        case rest of
                            [] ->
                                ( "single", head )

                            second :: _ ->
                                ( "two-or-more", Pine_builtin.int_add [ head, second ] )
            """;

        Evaluate(elmModuleText, "classify []")
            .Should().Be("(\"empty\", 0)");

        Evaluate(elmModuleText, "classify [ 9 ]")
            .Should().Be("(\"single\", 9)");

        Evaluate(elmModuleText, "classify [ 3, 4 ]")
            .Should().Be("(\"two-or-more\", 7)");

        Evaluate(elmModuleText, "classify [ 10, 20, 30 ]")
            .Should().Be("(\"two-or-more\", 30)");
    }

    [Fact]
    public void No_arm_matches_produces_runtime_error_with_rendered_value_and_stack_trace()
    {
        // The classifier only handles `Just`. Calling it with `Nothing` reaches the end of the
        // case block without any matching arm, raising an interpretation error whose
        // ToString() begins with the rendered scrutinee value (preceded by a brief description
        // line) and ends with the Elm call stack.
        var elmModuleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            classify m =
                case m of
                    Just x ->
                        x
            """;

        var declarations = InterpreterTestHelper.ParseDeclarations(elmModuleText);

        var result = ElmInterpreter.ParseAndInterpret("classify Nothing", declarations);

        result.IsErr().Should().BeTrue("calling classify with Nothing leaves no matching arm");

        var errorText =
            result.Unpack(
                fromErr: err => err.ToString(),
                fromOk: _ => throw new System.Exception("Expected error but got a value."));

        errorText.Should().Be(
            "Case expression did not match any arm.\n"
            + "Scrutinee value: Nothing\n"
            + "Elm call stack (innermost first):\n"
            + "  at classify Nothing");
    }
}
