using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Regression tests for the parenthesization behaviour of
/// <see cref="Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel.Convert(Core.Elm.ElmSyntax.Stil4mElmSyntax7.TypeAnnotation)"/>
/// applied to type annotations.
///
/// <para>
/// Background: the Stil4m simplified syntax model deliberately drops
/// single-element <c>Tupled</c> wrappers in
/// <see cref="Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(Core.Elm.ElmSyntax.SyntaxModel.TypeAnnotation)"/>
/// so its representation matches the original <c>stil4m/elm-syntax</c>
/// library. The information that a sub-annotation was originally
/// parenthesised therefore has to be RE-DERIVED at conversion time
/// back to the full syntax model whenever the surrounding shape would
/// be re-parsed differently without the parens.
/// </para>
///
/// <para>
/// These tests parse a module text, round-trip the parsed file
/// through Stil4m
/// (<c>FromFullSyntaxModel.Convert</c> &#x2192; <c>ToFullSyntaxModel.Convert</c>)
/// and re-format it with <see cref="Avh4Format"/>, then assert the
/// resulting text matches the original input. This exercises the full
/// "generated type-signature syntax node" rendering path the
/// problem statement describes, and pins down exactly which
/// parens-positions the renderer is responsible for restoring.
/// </para>
/// </summary>
public class ToFullSyntaxModelTypeAnnotationParensTests
{
    private static string FormatViaStil4mRoundTrip(string moduleText)
    {
        var parsedFull =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new System.Exception("parse failed: " + err));

        var stil4m =
            Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(parsedFull);

        var roundTrippedFull =
            Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel.Convert(stil4m);

        return Core.Elm.ElmSyntax.Avh4Format.FormatToString(roundTrippedFull);
    }

    private static void AssertModuleRoundTripsToItself(string moduleText)
    {
        FormatViaStil4mRoundTrip(moduleText).Trim().Should().Be(moduleText.Trim());
    }

    // -------------------------------------------------------------------
    // Cases where parens MUST be inserted around a type-argument because
    // dropping them would re-parse to a different annotation.
    // -------------------------------------------------------------------

    /// <summary>
    /// Type argument of a <c>Typed</c> annotation is itself a
    /// <c>FunctionTypeAnnotation</c>. This is the case from the problem
    /// statement (`Wrap (a -> a) -> Wrap (a -> a)`); without parens the
    /// signature reparses as `Wrap a -> a -> Wrap a -> a`, which is a
    /// 3-argument function type returning `a`.
    /// </summary>
    [Fact]
    public void Typed_argument_is_function_type_keeps_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            applyWrapped : Wrap (a -> a) -> Wrap (a -> a)
            applyWrapped x =
                x
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Type argument of a <c>Typed</c> annotation is itself a
    /// <c>Typed</c> with non-empty type arguments (e.g. `Maybe a`).
    /// Without parens, `List (Maybe a) -> Int` reparses as
    /// `List Maybe a -> Int`, where `List` takes two arguments.
    /// </summary>
    [Fact]
    public void Typed_argument_is_typed_with_args_keeps_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            firstJust : List (Maybe a) -> Maybe a
            firstJust xs =
                xs
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Constructor argument that is itself a function type — same
    /// reasoning as <see cref="Typed_argument_is_function_type_keeps_parens"/>
    /// but applied at the custom-type-constructor position. Without
    /// parens, `Tag (a -> b)` reparses as `Tag a -> b`, where `Tag`
    /// takes a single argument `a` and the `-&gt; b` becomes a function
    /// type built on top of `Tag a`.
    /// </summary>
    [Fact]
    public void Custom_type_constructor_argument_is_function_type_keeps_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            type Tag a b
                = Tag (a -> b)
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Constructor argument that is itself a <c>Typed</c> with type
    /// arguments. Without parens, `Box (Maybe a)` reparses as
    /// `Box Maybe a`, where `Box` takes two arguments.
    /// </summary>
    [Fact]
    public void Custom_type_constructor_argument_is_typed_with_args_keeps_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            type Box a
                = Box (Maybe a)
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Function-type argument (left of the outermost arrow) that is
    /// itself a function type. Already covered by the existing
    /// <see cref="Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel"/>
    /// implementation but pinned down here so the parens-restoration
    /// invariant is documented in one place.
    /// </summary>
    [Fact]
    public void Function_type_argument_is_function_type_keeps_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            apply : (a -> b) -> a -> b
            apply f x =
                f x
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Nested case: a <c>Typed</c> argument whose argument is itself a
    /// <c>Typed</c> with arguments. The parens-restoration must
    /// recurse so the inner shape gets its own parens.
    /// </summary>
    [Fact]
    public void Typed_argument_is_typed_with_typed_arg_keeps_parens_recursively()
    {
        var moduleText =
            """
            module Test exposing (..)


            outer : List (Maybe (List a))
            outer =
                []
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Multiple type arguments, mixed parens-needing and parens-free.
    /// Pins down that the wrapping decision is made per-argument.
    /// </summary>
    [Fact]
    public void Typed_multiple_arguments_only_parens_where_needed()
    {
        var moduleText =
            """
            module Test exposing (..)


            mix : Result String (Maybe a)
            mix =
                mix
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    // -------------------------------------------------------------------
    // Cases where parens MUST NOT be inserted because the inner shape is
    // already a single lexical token (so it cannot be re-grouped at
    // parse time).
    // -------------------------------------------------------------------

    /// <summary>
    /// Type argument that is a <see cref="Core.Elm.ElmSyntax.Stil4mElmSyntax7.TypeAnnotation.GenericType"/>
    /// (e.g. `List a`). Adding parens here would be valid Elm but is
    /// not the canonical form and would diverge from the original
    /// source.
    /// </summary>
    [Fact]
    public void Typed_argument_is_generic_type_does_not_add_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            head : List a -> Maybe a
            head xs =
                Nothing
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Type argument that is a zero-arity <see cref="Core.Elm.ElmSyntax.Stil4mElmSyntax7.TypeAnnotation.Typed"/>
    /// (e.g. `List Int`). Same reasoning as the generic-type case.
    /// </summary>
    [Fact]
    public void Typed_argument_is_zero_arg_typed_does_not_add_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            ints : List Int
            ints =
                []
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Type argument that is a multi-element <see cref="Core.Elm.ElmSyntax.Stil4mElmSyntax7.TypeAnnotation.Tupled"/>
    /// (e.g. `List ( Int, String )`). The tuple already brings its own
    /// parens; the renderer must not nest a second pair.
    /// </summary>
    [Fact]
    public void Typed_argument_is_tuple_does_not_add_extra_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            pairs : List ( Int, String )
            pairs =
                []
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Type argument that is a record. The braces already delimit the
    /// inner annotation; the renderer must not wrap it in parens.
    /// </summary>
    [Fact]
    public void Typed_argument_is_record_does_not_add_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            records : List { x : Int, y : Int }
            records =
                []
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Constructor argument that is a generic type (no parens needed).
    /// </summary>
    [Fact]
    public void Custom_type_constructor_argument_is_generic_does_not_add_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            type Wrap a
                = Wrap a
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Constructor argument that is a zero-arg typed (no parens needed).
    /// </summary>
    [Fact]
    public void Custom_type_constructor_argument_is_zero_arg_typed_does_not_add_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            type Foo a
                = Foo Int a
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }

    /// <summary>
    /// Return-type position of a <c>FunctionTypeAnnotation</c> never
    /// needs parens, even when the return type is itself a function
    /// (right-associativity of <c>-&gt;</c>).
    /// </summary>
    [Fact]
    public void Function_return_type_function_does_not_add_parens()
    {
        var moduleText =
            """
            module Test exposing (..)


            add : Int -> Int -> Int
            add a b =
                a
            """;

        AssertModuleRoundTripsToItself(moduleText);
    }
}
