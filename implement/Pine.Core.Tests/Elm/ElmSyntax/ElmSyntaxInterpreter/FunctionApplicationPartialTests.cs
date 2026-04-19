using AwesomeAssertions;
using Pine.Core.Elm;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Covers partial / incremental application in the interpreter:
/// <list type="bullet">
/// <item>A user-defined function can be supplied with fewer arguments than its arity at one
/// call site and have the remaining arguments supplied at a later call site (possibly across
/// a return boundary or after being stored in a list / record / let-binding).</item>
/// <item>A reference to a top-level function can escape as a first-class value with no
/// arguments at all.</item>
/// <item>An anonymous lambda expression is itself a callable value.</item>
/// <item>A lambda captures its surrounding lexical environment (let-bindings, the enclosing
/// function's parameters) and continues to see those captures after the binding scope
/// has ended.</item>
/// <item>Over-application: when a function returning a function is applied with more arguments
/// than its declared arity in one go, the extra arguments flow into the returned closure.
/// This includes the case where the first batch of arguments at one call site spans <i>both</i>
/// the outer function's parameters and the returned inner function's parameters.</item>
/// </list>
/// Each test parses the helper functions for its scenario from a module text, then invokes
/// <see cref="Evaluate(string, string)"/> repeatedly with different argument expressions
/// encoded directly in the expression string. The result of each invocation is converted to
/// an Elm-expression string via <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/>.
/// Function values that escape into the result render as <c>&lt;function&gt;</c>, mirroring
/// Elm's <c>Debug.toString</c> convention.
/// </summary>
public class FunctionApplicationPartialTests
{
    /// <summary>
    /// Delegates to <see cref="InterpreterTestHelper.EvaluateInModuleOrCrash(string, string)"/>
    /// and renders the resulting value back to its Elm-expression form.
    /// </summary>
    private static string Evaluate(string elmModuleText, string expression) =>
        ElmValue.RenderAsElmExpression(
            InterpreterTestHelper.EvaluateInModuleOrCrash(expression, elmModuleText))
        .expressionString;

    /// <summary>
    /// A two-argument function applied to one argument at one site, then to the second at a
    /// different site. The intermediate value travels through a let-binding.
    /// </summary>
    [Fact]
    public void Partial_application_supplies_remaining_argument_via_let_binding()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            add a b =
                Pine_builtin.int_add [ a, b ]
            """;

        Evaluate(
            elmModuleText,
            """
            let
                addTen =
                    add 10
            in
            addTen 32
            """)
            .Should().Be("42");
    }

    /// <summary>
    /// A three-argument function applied incrementally as
    /// <c>(((f 1) 2) 3)</c> through three nested let-bindings, asserting that each
    /// intermediate closure captures its arguments correctly.
    /// </summary>
    [Fact]
    public void Three_argument_function_applied_one_at_a_time()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            sum3 a b c =
                Pine_builtin.int_add [ a, Pine_builtin.int_add [ b, c ] ]
            """;

        Evaluate(
            elmModuleText,
            """
            let
                f1 =
                    sum3 100
            in
            let
                f2 =
                    f1 20
            in
            f2 3
            """)
            .Should().Be("123");
    }

    /// <summary>
    /// A partially applied function escapes as a value and is rendered via
    /// <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/> as <c>&lt;function&gt;</c>.
    /// </summary>
    [Fact]
    public void Partially_applied_function_renders_as_function_when_returned()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            add a b =
                Pine_builtin.int_add [ a, b ]
            """;

        Evaluate(elmModuleText, "add 7")
            .Should().Be("<function>");
    }

    /// <summary>
    /// A bare reference to a top-level function with no application escapes as a function value.
    /// </summary>
    [Fact]
    public void Bare_reference_to_top_level_function_escapes_as_function_value()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            add a b =
                Pine_builtin.int_add [ a, b ]
            """;

        Evaluate(elmModuleText, "add")
            .Should().Be("<function>");
    }

    /// <summary>
    /// A list of partially-applied closures, each with a different first argument, is later
    /// fully applied via subsequent call sites; the final list contains the saturated values.
    /// </summary>
    [Fact]
    public void List_of_partially_applied_closures_can_be_saturated_later()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            add a b =
                Pine_builtin.int_add [ a, b ]


            apply3 f =
                f 3
            """;

        Evaluate(
            elmModuleText,
            """
            let
                fns =
                    [ add 10, add 20, add 30 ]
            in
            case fns of
                [ a, b, c ] ->
                    [ apply3 a, apply3 b, apply3 c ]

                _ ->
                    []
            """)
            .Should().Be("[ 13, 23, 33 ]");
    }

    /// <summary>
    /// A function that returns a partially-applied function from another function. The returned
    /// closure outlives the returning function's own activation.
    /// </summary>
    [Fact]
    public void Function_returning_a_partially_applied_function()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            add a b =
                Pine_builtin.int_add [ a, b ]


            makeAdder n =
                add n
            """;

        Evaluate(
            elmModuleText,
            """
            let
                addFive =
                    makeAdder 5
            in
            addFive 37
            """)
            .Should().Be("42");
    }

    /// <summary>
    /// A function explicitly written as returning an inner lambda (curried by hand). Each
    /// argument applied at a separate call site.
    /// </summary>
    [Fact]
    public void Function_returning_lambda_applied_incrementally()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            outer a =
                \b -> \c -> Pine_builtin.int_add [ a, Pine_builtin.int_add [ b, c ] ]
            """;

        Evaluate(
            elmModuleText,
            """
            let
                f1 =
                    outer 100
            in
            let
                f2 =
                    f1 20
            in
            f2 3
            """)
            .Should().Be("123");
    }

    /// <summary>
    /// A bare lambda used directly at a call site. Confirms that
    /// <see cref="Core.Elm.ElmSyntax.SyntaxModel.Expression.LambdaExpression"/> is a
    /// callable value of its own.
    /// </summary>
    [Fact]
    public void Lambda_expression_applied_directly()
    {
        var elmModuleText =
            """
            module Test exposing (..)
            """;

        Evaluate(elmModuleText, "(\\x -> Pine_builtin.int_mul [ x, x ]) 7")
            .Should().Be("49");
    }

    /// <summary>
    /// A lambda captures a let-bound name from its surrounding lexical scope. The captured
    /// value is observable when the lambda is invoked outside of the original let-block.
    /// </summary>
    [Fact]
    public void Lambda_captures_let_binding_from_enclosing_scope()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            apply f x =
                f x


            mkClosure =
                let
                    base =
                        1000
                in
                \n -> Pine_builtin.int_add [ base, n ]
            """;

        Evaluate(elmModuleText, "apply mkClosure 23")
            .Should().Be("1023");
    }

    /// <summary>
    /// A lambda captures a parameter of its enclosing function. The closure is returned past
    /// the enclosing function's activation and applied externally. Each combination is
    /// exercised as a separate evaluation to confirm that the captured factor is independent
    /// across invocations.
    /// </summary>
    [Fact]
    public void Lambda_captures_enclosing_function_parameter()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            multiplier factor =
                \x -> Pine_builtin.int_mul [ factor, x ]
            """;

        Evaluate(elmModuleText, "multiplier 3 10").Should().Be("30");
        Evaluate(elmModuleText, "multiplier 3 11").Should().Be("33");
        Evaluate(elmModuleText, "multiplier 4 10").Should().Be("40");
        Evaluate(elmModuleText, "multiplier 4 11").Should().Be("44");
    }

    /// <summary>
    /// Over-application via a function returning a function: <c>f x y z</c> where <c>f</c> takes
    /// only one parameter and returns a two-argument function (here, an explicitly curried
    /// lambda). All three arguments are supplied in a single application node.
    /// </summary>
    [Fact]
    public void Over_application_to_function_returning_lambda_in_single_call()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            outer a =
                \b -> \c -> Pine_builtin.int_add [ a, Pine_builtin.int_add [ b, c ] ]
            """;

        Evaluate(elmModuleText, "outer 100 20 3")
            .Should().Be("123");
    }

    /// <summary>
    /// The user's specific scenario: a single application supplies a batch of arguments where
    /// some are consumed by the outer function and some by the inner function returned from it.
    /// Here <c>mkAdder a b</c> takes two arguments and returns a lambda taking two more, and
    /// the call site supplies all four at once: <c>mkAdder 1 2 3 4</c>.
    /// </summary>
    [Fact]
    public void Single_call_distributes_arguments_across_outer_and_returned_inner()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            mkAdder a b =
                \c d -> Pine_builtin.int_add [ a, Pine_builtin.int_add [ b, Pine_builtin.int_add [ c, d ] ] ]
            """;

        Evaluate(elmModuleText, "mkAdder 1 2 3 4")
            .Should().Be("10");
    }

    /// <summary>
    /// A single application supplies arguments that span three layers: the outer function takes
    /// one argument and returns a two-argument function (a lambda) that itself returns another
    /// one-argument lambda. <c>outer a b c d</c> distributes <c>a</c> to <c>outer</c>,
    /// <c>b c</c> to the first lambda, and <c>d</c> to the second.
    /// </summary>
    [Fact]
    public void Single_call_distributes_arguments_across_three_layers()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            outer a =
                \b c -> \d -> Pine_builtin.int_add [ a, Pine_builtin.int_add [ b, Pine_builtin.int_add [ c, d ] ] ]
            """;

        Evaluate(elmModuleText, "outer 1 2 3 4")
            .Should().Be("10");
    }

    /// <summary>
    /// First call site supplies more arguments than the outer function accepts (entering the
    /// inner lambda); a later call site supplies the remaining argument that the inner lambda
    /// still needs. Mixes over-application and partial application across two call sites.
    /// </summary>
    [Fact]
    public void Over_application_then_partial_application_across_two_call_sites()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            outer a =
                \b c -> Pine_builtin.int_add [ a, Pine_builtin.int_add [ b, c ] ]
            """;

        Evaluate(
            elmModuleText,
            """
            let
                waiting =
                    outer 100 20
            in
            waiting 3
            """)
            .Should().Be("123");
    }

    /// <summary>
    /// A higher-order function takes a function value and applies it. The function value is a
    /// partially applied user-defined top-level function passed into the higher-order function.
    /// </summary>
    [Fact]
    public void Higher_order_function_receives_partially_applied_function()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            apply f x =
                f x


            add a b =
                Pine_builtin.int_add [ a, b ]
            """;

        Evaluate(elmModuleText, "apply (add 100) 23")
            .Should().Be("123");
    }

    /// <summary>
    /// Distinct partial applications remain independent — each <c>add k</c> closure carries its
    /// own captured first argument when later applied.
    /// </summary>
    [Fact]
    public void Distinct_partial_applications_remain_independent()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            apply f x =
                f x


            add a b =
                Pine_builtin.int_add [ a, b ]
            """;

        Evaluate(elmModuleText, "apply (add 1) 100").Should().Be("101");
        Evaluate(elmModuleText, "apply (add 2) 100").Should().Be("102");
        Evaluate(elmModuleText, "apply (add 1) 200").Should().Be("201");
        Evaluate(elmModuleText, "apply (add 2) 200").Should().Be("202");
    }

    /// <summary>
    /// A multi-argument lambda (a single <c>\</c> binding several patterns) is applied
    /// incrementally across two call sites — the first site supplies one argument and yields a
    /// closure, the second site supplies the remaining argument.
    /// </summary>
    [Fact]
    public void Multi_argument_lambda_applied_incrementally()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            apply f x =
                f x
            """;

        Evaluate(
            elmModuleText,
            """
            let
                addPair =
                    \a b -> Pine_builtin.int_add [ a, b ]
            in
            let
                addTen =
                    addPair 10
            in
            apply addTen 5
            """)
            .Should().Be("15");
    }

    /// <summary>
    /// A partial application of a record-type-alias constructor is a function value that
    /// completes the construction when supplied with the remaining field arguments. Mirrors
    /// Elm's behaviour where a record alias declaration introduces a constructor function.
    /// </summary>
    [Fact]
    public void Partial_application_of_record_alias_constructor()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type alias Point =
                { x : Int, y : Int }


            apply f v =
                f v
            """;

        Evaluate(
            elmModuleText,
            """
            let
                pointWithX5 =
                    Point 5
            in
            apply pointWithX5 7
            """)
            .Should().Be("{ x = 5, y = 7 }");
    }

    /// <summary>
    /// A partial application of a choice-type tag constructor: <c>Just</c>'s arity is 1, so a
    /// bare reference to <c>Just</c> is a function value that completes to a <c>Just x</c>
    /// when applied.
    /// </summary>
    [Fact]
    public void Partial_application_of_choice_type_tag_constructor()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a


            apply f x =
                f x
            """;

        Evaluate(elmModuleText, "apply Just 42")
            .Should().Be("Just 42");
    }
}
