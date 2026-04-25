using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Tests for <see cref="LambdaLiftingValidator"/>: confirms that
/// (a) well-formed lifted modules (the actual output of <see cref="LambdaLifting.LiftLambdas(File)"/>)
/// pass validation, and
/// (b) deliberately corrupted lifted modules — each modelling a distinct
/// shape from the postmortem document
/// (<c>explore/internal-analysis/2026-04-25-lambda-lifting-sibling-capture-defect-postmortem.md</c>) —
/// are rejected with a precise diagnostic.
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

    /// <summary>
    /// The actual output of the lifter on the simplest closure scenario
    /// (<see cref="LambdaLiftingTests.Simplest_closure_single_capture_in_let_function"/>)
    /// must pass validation.
    /// </summary>
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

        var lifted = ParseAndLift(inputModuleText);

        LambdaLiftingValidator.Validate(lifted);
    }

    /// <summary>
    /// The actual output of the lifter on two cooperating local functions
    /// without external captures must pass validation.
    /// </summary>
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

        var lifted = ParseAndLift(inputModuleText);

        LambdaLiftingValidator.Validate(lifted);
    }

    /// <summary>
    /// The actual output of the lifter on two mutually-recursive local
    /// functions (no external captures) must pass validation.
    /// </summary>
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

        var lifted = ParseAndLift(inputModuleText);

        LambdaLiftingValidator.Validate(lifted);
    }

    /// <summary>
    /// The actual output of the lifter on the captured-sibling scenario
    /// (the original bug) must pass validation now that the fix is in
    /// place. This locks in that the new validator and the new lifter
    /// agree.
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

        var lifted = ParseAndLift(inputModuleText);

        LambdaLiftingValidator.Validate(lifted);
    }

    /// <summary>
    /// L3 violation: a sibling lifted function's body contains an
    /// unsaturated bare reference to another lifted function. This is
    /// exactly the shape produced by the original buggy lifter: it
    /// substituted the sibling local name with the bare lifted name even
    /// though the sibling had captures, so the call site supplied fewer
    /// arguments than the lifted function's declared arity.
    /// </summary>
    [Fact]
    public void Validator_rejects_unsaturated_sibling_call_site()
    {
        // After lifting, "compute" should hold:
        //   compute n =
        //       let
        //           inner = compute__lifted__inner_1 n      -- partial app, captures `n`
        //           outer = compute__lifted__outer_2        -- no captures
        //       in
        //       outer "x"
        //
        // We hand-construct the buggy variant where outer's lifted body
        // calls "compute__lifted__inner_1" with 1 argument (the original
        // outer parameter `name`) instead of via the partial-application
        // let-binding — exactly as the buggy lifter substituted it.
        var moduleText =
            """"
            module Test exposing (..)


            compute : Int -> String -> String
            compute n name =
                let
                    inner =
                        compute__lifted__inner_1 n

                    outer =
                        compute__lifted__outer_2
                in
                outer name


            compute__lifted__inner_1 n_capture x =
                String.fromInt (n_capture + x)


            compute__lifted__outer_2 name =
                compute__lifted__inner_1 name
            """";

        var module = ParseModuleText(moduleText);

        System.Action act = () => LambdaLiftingValidator.Validate(module);

        var ex = act.Should().Throw<LambdaLiftingValidationException>().Which;

        ex.Violations.Should().Contain(
            v =>
            v.Contains("L3", StringComparison.Ordinal) &&
            v.Contains("compute__lifted__inner_1", StringComparison.Ordinal) &&
            v.Contains("supplies 1 arguments", StringComparison.Ordinal));
    }

    /// <summary>
    /// L3 violation: a bare reference to a lifted function appears outside
    /// any <see cref="Expression.Application"/> head and outside the
    /// let-binding partial-application slot. A future buggy rewrite that
    /// hoists a lifted-name reference into, say, the body of an
    /// <c>if</c>-expression must be rejected.
    /// </summary>
    [Fact]
    public void Validator_rejects_bare_lifted_reference_in_arbitrary_position()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute : Int -> Int -> Int
            compute n x =
                let
                    add =
                        compute__lifted__add_1 n
                in
                if x > 0 then
                    add x
                else
                    compute__lifted__add_1


            compute__lifted__add_1 n_capture y =
                n_capture + y
            """";

        var module = ParseModuleText(moduleText);

        System.Action act = () => LambdaLiftingValidator.Validate(module);

        var ex = act.Should().Throw<LambdaLiftingValidationException>().Which;

        ex.Violations.Should().Contain(
            v =>
            v.Contains("L3", StringComparison.Ordinal) &&
            v.Contains("compute__lifted__add_1", StringComparison.Ordinal) &&
            v.Contains("bare reference", StringComparison.Ordinal));
    }

    /// <summary>
    /// L1 violation: the lifted function exists but no let-binding partial
    /// application references it. Models a future bug that emits a lifted
    /// function but forgets to insert (or accidentally removes) its
    /// invocation slot.
    /// </summary>
    [Fact]
    public void Validator_rejects_missing_partial_application_let_binding()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute : Int -> Int -> Int
            compute n x =
                let
                    add z =
                        z + 1
                in
                add x


            compute__lifted__orphan_1 n_capture y =
                n_capture + y
            """";

        var module = ParseModuleText(moduleText);

        System.Action act = () => LambdaLiftingValidator.Validate(module);

        var ex = act.Should().Throw<LambdaLiftingValidationException>().Which;

        ex.Violations.Should().Contain(
            v =>
            v.Contains("L1", StringComparison.Ordinal) &&
            v.Contains("compute__lifted__orphan_1", StringComparison.Ordinal) &&
            v.Contains("0 partial-application", StringComparison.Ordinal));
    }

    /// <summary>
    /// L1 violation: two distinct let-binding partial applications point at
    /// the same lifted function. Models a duplicated emission bug.
    /// </summary>
    [Fact]
    public void Validator_rejects_duplicate_partial_application_let_binding()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute : Int -> Int -> Int
            compute n x =
                let
                    add =
                        compute__lifted__add_1 n

                    addAgain =
                        compute__lifted__add_1 n
                in
                add (addAgain x)


            compute__lifted__add_1 n_capture y =
                n_capture + y
            """";

        var module = ParseModuleText(moduleText);

        System.Action act = () => LambdaLiftingValidator.Validate(module);

        var ex = act.Should().Throw<LambdaLiftingValidationException>().Which;

        ex.Violations.Should().Contain(
            v =>
            v.Contains("L1", StringComparison.Ordinal) &&
            v.Contains("compute__lifted__add_1", StringComparison.Ordinal) &&
            v.Contains("2 partial-application", StringComparison.Ordinal));
    }

    /// <summary>
    /// L3 violation: a call site supplies MORE arguments than the lifted
    /// function declares. Models a bug where the lifter inserts spurious
    /// extra arguments at a call site.
    /// </summary>
    [Fact]
    public void Validator_rejects_oversaturated_call_site()
    {
        var moduleText =
            """"
            module Test exposing (..)


            compute : Int -> Int -> Int
            compute n x =
                let
                    add =
                        compute__lifted__add_1 n
                in
                compute__lifted__add_1 n x x


            compute__lifted__add_1 n_capture y =
                n_capture + y
            """";

        var module = ParseModuleText(moduleText);

        System.Action act = () => LambdaLiftingValidator.Validate(module);

        var ex = act.Should().Throw<LambdaLiftingValidationException>().Which;

        ex.Violations.Should().Contain(
            v =>
            v.Contains("L3", StringComparison.Ordinal) &&
            v.Contains("compute__lifted__add_1", StringComparison.Ordinal) &&
            v.Contains("supplies 3 arguments", StringComparison.Ordinal));
    }

    /// <summary>
    /// Modules that contain no lifted functions are trivially well-formed.
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
