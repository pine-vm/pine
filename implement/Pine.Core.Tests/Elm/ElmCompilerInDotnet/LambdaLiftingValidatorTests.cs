using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Tests for <see cref="LambdaLiftingValidator"/>.
///
/// <para>
/// Acceptance tests confirm that the actual output of
/// <see cref="LambdaLifting.LiftLambdas(File)"/> on representative Elm
/// programs passes validation, including the captured-sibling shape that
/// triggered the original defect, and the higher-order-function patterns
/// that the lifter intentionally produces with partial applications.
/// </para>
///
/// <para>
/// Rejection tests confirm that hand-constructed Elm syntax files
/// containing the forbidden form described in the postmortem
/// (<c>explore/internal-analysis/2026-04-25-lambda-lifting-sibling-capture-defect-postmortem.md</c>)
/// — a let-bound lifted top-level function with captures whose lifted
/// name is referenced from another function's body (other than its
/// partial-application let-binding slot or its own self-recursive body) —
/// cause <see cref="LambdaLiftingValidator.Validate(File)"/> to throw a
/// <see cref="LambdaLiftingValidationException"/> with a precise
/// diagnostic.
/// </para>
/// </summary>
public class LambdaLiftingValidatorTests
{
    private static File ParseModuleText(string moduleText)
    {
        var concreteSyntax =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Failed parsing: " + err));

        return FromFullSyntaxModel.Convert(concreteSyntax);
    }

    private static File ParseAndLift(string moduleText) =>
        LambdaLifting.LiftLambdas(ParseModuleText(moduleText));

    [Fact]
    public void Validator_accepts_simplest_closure_lifted_output()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            addN : Int -> Int -> Int
            addN n x =
                let
                    add y = y + n
                in
                add x
            """";

        LambdaLiftingValidator.Validate(ParseAndLift(inputModuleText));
    }

    [Fact]
    public void Validator_accepts_two_local_functions_lifted_output()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            compute : Int -> Int
            compute n =
                let
                    double x =
                        x * 2

                    applyTwice y =
                        double (double y)
                in
                applyTwice n
            """";

        LambdaLiftingValidator.Validate(ParseAndLift(inputModuleText));
    }

    [Fact]
    public void Validator_accepts_mutually_recursive_lifted_output()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            checkEven : Int -> Bool
            checkEven n =
                let
                    isEven x =
                        if x == 0 then
                            True
                        else
                            isOdd (x - 1)

                    isOdd y =
                        if y == 0 then
                            False
                        else
                            isEven (y - 1)
                in
                isEven n
            """";

        LambdaLiftingValidator.Validate(ParseAndLift(inputModuleText));
    }

    /// <summary>
    /// The captured-sibling scenario (the exact original-bug shape, with
    /// the fix in place). The lifter must produce output that passes
    /// validation: <c>inner</c> has captures, so it must NOT be substituted
    /// into <c>outer</c>'s lifted body — instead, <c>outer</c>'s lifted
    /// body should refer to <c>inner</c> via a captured local name.
    /// </summary>
    [Fact]
    public void Validator_accepts_captured_sibling_lifted_output()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            run : Int -> List ( Int, String )
            run n =
                let
                    sl =
                        ( n, "captured" )

                    inner _ =
                        Just sl

                    outer name =
                        case inner name of
                            Just s ->
                                [ s ]

                            Nothing ->
                                []
                in
                outer 1
            """";

        LambdaLiftingValidator.Validate(ParseAndLift(inputModuleText));
    }

    /// <summary>
    /// Anonymous lambdas passed to higher-order functions: the lifter
    /// substitutes the lambda expression with the bare lifted name (zero
    /// captures) or with a partial-application expression (with captures),
    /// in argument position. These intentional shapes must NOT be
    /// rejected.
    /// </summary>
    [Fact]
    public void Validator_accepts_anonymous_lambda_passed_to_higher_order_function()
    {
        var inputModuleText =
            """"
            module Test exposing (..)


            applyToEach f items =
                case items of
                    [] ->
                        []

                    first :: rest ->
                        f first :: applyToEach f rest


            doubleAll input =
                applyToEach (\x -> x + x) input


            scaleAll factor input =
                applyToEach (\x -> x * factor) input
            """";

        LambdaLiftingValidator.Validate(ParseAndLift(inputModuleText));
    }

    /// <summary>
    /// A module with no lifted functions is trivially well-formed.
    /// </summary>
    [Fact]
    public void Validator_accepts_module_without_any_lifted_functions()
    {
        var moduleText =
            """"
            module Test exposing (..)


            identity : a -> a
            identity x =
                x
            """";

        LambdaLiftingValidator.Validate(ParseModuleText(moduleText));
    }

    /// <summary>
    /// The exact original-defect shape from the postmortem, hand-written so
    /// that we can drive the validator with the forbidden form. The
    /// let-bound lifted function <c>compute__lifted__inner_1</c> has a
    /// capture (the partial-app slot supplies <c>n</c>) and is referenced
    /// by name from inside <c>compute__lifted__outer_2</c>'s body. The
    /// validator MUST throw.
    /// </summary>
    [Fact]
    public void Validator_rejects_substituted_captured_sibling_reference_inside_lifted_body()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute n =
                let
                    inner =
                        compute__lifted__inner_1 n

                    outer =
                        compute__lifted__outer_2
                in
                outer "x"


            compute__lifted__inner_1 n_capture _ =
                Just ( n_capture, "doc" )


            compute__lifted__outer_2 name =
                compute__lifted__inner_1 name
            """";

        var module = ParseModuleText(moduleText);

        var act = () => LambdaLiftingValidator.Validate(module);

        var ex = act.Should().Throw<LambdaLiftingValidationException>().Which;

        ex.Violations.Should().Contain(
            v =>
                v.Contains("compute__lifted__inner_1", StringComparison.Ordinal) &&
                v.Contains("compute__lifted__outer_2", StringComparison.Ordinal));
    }

    /// <summary>
    /// A bare reference to a lifted-with-captures function inside ANOTHER
    /// lifted body (not the partial-app slot, not self-recursion) is also
    /// the forbidden shape. Models a future buggy rewrite that hoists a
    /// lifted-name reference into the body of a sibling lifted function.
    /// </summary>
    [Fact]
    public void Validator_rejects_bare_lifted_reference_inside_sibling_lifted_body()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute n =
                let
                    helper =
                        compute__lifted__helper_1 n

                    user =
                        compute__lifted__user_2 helper
                in
                user 0


            compute__lifted__helper_1 n_capture x =
                n_capture + x


            compute__lifted__user_2 helperVal x =
                if x > 0 then
                    helperVal x
                else
                    compute__lifted__helper_1 0
            """";

        var module = ParseModuleText(moduleText);

        var act = () => LambdaLiftingValidator.Validate(module);

        var ex = act.Should().Throw<LambdaLiftingValidationException>().Which;

        ex.Violations.Should().Contain(
            v =>
                v.Contains("compute__lifted__helper_1", StringComparison.Ordinal) &&
                v.Contains("compute__lifted__user_2", StringComparison.Ordinal));
    }

    /// <summary>
    /// A reference to a lifted-with-captures function from inside the
    /// containing function's body but OUTSIDE the partial-application
    /// let-binding slot (e.g. as an extra bare reference somewhere else
    /// in the same containing function) is also rejected.
    /// </summary>
    [Fact]
    public void Validator_rejects_extra_lifted_reference_in_containing_function_body()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute n =
                let
                    add =
                        compute__lifted__add_1 n
                in
                if n > 0 then
                    add 0
                else
                    compute__lifted__add_1 0 0


            compute__lifted__add_1 n_capture y =
                n_capture + y
            """";

        var module = ParseModuleText(moduleText);

        var act = () => LambdaLiftingValidator.Validate(module);

        var ex = act.Should().Throw<LambdaLiftingValidationException>().Which;

        ex.Violations.Should().Contain(
            v =>
                v.Contains("compute__lifted__add_1", StringComparison.Ordinal) &&
                v.Contains("compute", StringComparison.Ordinal));
    }

    /// <summary>
    /// All violations for the same module are collected and reported in
    /// a single exception, not just the first one encountered.
    /// </summary>
    [Fact]
    public void Validator_collects_all_violations_in_a_single_exception()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute n m =
                let
                    inner =
                        compute__lifted__inner_1 n

                    outer =
                        compute__lifted__outer_2 m
                in
                outer "x"


            compute__lifted__inner_1 n_capture _ =
                compute__lifted__outer_2 1 2


            compute__lifted__outer_2 m_capture name =
                compute__lifted__inner_1 name
            """";

        var module = ParseModuleText(moduleText);

        var act = () => LambdaLiftingValidator.Validate(module);

        var ex = act.Should().Throw<LambdaLiftingValidationException>().Which;

        ex.Violations.Should().HaveCountGreaterThanOrEqualTo(2);
    }

    /// <summary>
    /// Self-recursive references inside a lifted-with-captures function's
    /// own body are allowed — the function is naturally recursive.
    /// </summary>
    [Fact]
    public void Validator_accepts_self_recursive_reference_inside_own_lifted_body()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute n =
                let
                    loop x =
                        if x <= 0 then
                            n
                        else
                            loop (x - 1)
                in
                loop n


            compute__lifted__loop_1 n_capture x =
                if x <= 0 then
                    n_capture
                else
                    compute__lifted__loop_1 n_capture (x - 1)
            """";

        var module = ParseModuleText(moduleText);

        LambdaLiftingValidator.Validate(module);
    }

    /// <summary>
    /// Lifted functions with NO captures (zero-capture lifted functions)
    /// are exempt from the cross-reference restriction: their value is
    /// just the bare name, and other lifted bodies are free to reference
    /// them by name (this is the "sibling without external captures"
    /// substitution case).
    /// </summary>
    [Fact]
    public void Validator_accepts_cross_reference_to_zero_capture_lifted_function()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute n =
                let
                    pure x =
                        x + 1

                    user y =
                        pure y
                in
                user n


            compute__lifted__pure_1 x =
                x + 1


            compute__lifted__user_2 y =
                compute__lifted__pure_1 y
            """";

        var module = ParseModuleText(moduleText);

        LambdaLiftingValidator.Validate(module);
    }

    /// <summary>
    /// <see cref="LambdaLiftingValidator.CollectViolations(File)"/> on a
    /// well-formed module returns the empty list and never throws.
    /// </summary>
    [Fact]
    public void CollectViolations_returns_empty_for_well_formed_module()
    {
        var moduleText =
            """"
            module Test exposing (..)


            addN : Int -> Int -> Int
            addN n x =
                let
                    add y = y + n
                in
                add x
            """";

        var lifted = ParseAndLift(moduleText);

        var violations = LambdaLiftingValidator.CollectViolations(lifted);

        violations.Should().BeEmpty();
    }
}
