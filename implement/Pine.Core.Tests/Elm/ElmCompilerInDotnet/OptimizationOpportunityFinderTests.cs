using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using System.Collections.Immutable;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class OptimizationOpportunityFinderTests
{
    [Fact]
    public void Reports_no_opportunities_for_fully_monomorphic_module()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                addInts : Int -> Int -> Int
                addInts left right =
                    Pine_kernel.int_add [ left, right ]
                """);

        rendered.Should().Be("");
    }

    [Fact]
    public void Reports_record_access_on_open_record_helper()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                showName : { a | name : String } -> String
                showName r =
                    r.name
                """);

        rendered.Should().Be(
            """
            Test.showName: record-access: name
            """.Trim());
    }

    [Fact]
    public void Reports_record_update_on_open_record_helper()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                bumpAge : { a | age : Int } -> { a | age : Int }
                bumpAge r =
                    { r | age = Pine_kernel.int_add [ r.age, 1 ] }
                """);

        rendered.Should().Be(
            """
            Test.bumpAge: record-access: age
            Test.bumpAge: record-update: age
            """.Trim());
    }

    [Fact]
    public void Reports_generic_arithmetic_via_canonicalized_operator()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                addThings : a -> a -> a
                addThings left right =
                    left + right
                """);

        rendered.Should().Be(
            """
            Test.addThings: Basics.arithmetic: add
            """.Trim());
    }

    [Fact]
    public void Reports_generic_compare_and_equality_in_same_function()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                check : a -> a -> Bool
                check left right =
                    if left == right then
                        Basics.True

                    else
                        left < right
                """);

        rendered.Should().Be(
            """
            Test.check: Basics.compare: lt
            Test.check: Basics.eq: eq
            """.Trim());
    }

    [Fact]
    public void Reports_generic_append()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                joinTwo : appendable -> appendable -> appendable
                joinTwo left right =
                    left ++ right
                """);

        rendered.Should().Be(
            """
            Test.joinTwo: Basics.append: append
            """.Trim());
    }

    [Fact]
    public void Reports_explicit_basics_function_reference_used_as_value()
    {
        // `Basics.compare` passed as an argument to another function still
        // performs the generic compare dispatch.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                pickCompare : (a -> a -> Order)
                pickCompare =
                    Basics.compare
                """);

        rendered.Should().Be(
            """
            Test.pickCompare: Basics.compare: compare
            """.Trim());
    }

    [Fact]
    public void Reports_findings_inside_nested_let_and_lambda()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                deeplyNested : { a | value : Int } -> Int -> Int
                deeplyNested record extra =
                    let
                        helper x =
                            (\y -> x + y + record.value) extra
                    in
                    helper extra
                """);

        rendered.Should().Be(
            """
            Test.deeplyNested: record-access: value
            Test.deeplyNested: Basics.arithmetic: add
            """.Trim());
    }

    [Fact]
    public void Whitelist_with_exact_decl_name_suppresses_only_matching_findings()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                hot : { a | x : Int } -> Int
                hot r =
                    r.x

                cold : { a | y : Int } -> Int
                cold r =
                    r.y
                """,
                ignoreRecordOperation: ["Test.hot"]);

        rendered.Should().Be(
            """
            Test.cold: record-access: y
            """.Trim());
    }

    [Fact]
    public void Whitelist_prefix_covers_specialized_variants()
    {
        // A whitelist entry "Test.hot" must also cover a synthetic
        // specialized variant such as "Test.hot_specialized_1".
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                hot : { a | x : Int } -> Int
                hot r =
                    r.x

                cold : { a | y : Int } -> Int
                cold r =
                    r.y
                """);

        // Synthesize a renamed variant of `hot` so we can assert the
        // prefix-matching behaviour without depending on the inliner.
        var hotKey = DeclQualifiedName.FromString("Test.hot");
        var specializedKey = DeclQualifiedName.FromString("Test.hot_specialized_1");

        var withSpecialized =
            ImmutableDictionary.CreateRange(declarations)
            .Add(specializedKey, declarations[hotKey]);

        var opportunities =
            OptimizationOpportunityFinder.FindOptimizationOpportunities(
                withSpecialized,
                ignoreRecordOperation: ["Test.hot"]);

        var rendered = OptimizationOpportunityFinder.RenderOpportunities(opportunities);

        rendered.Should().Be(
            """
            Test.cold: record-access: y
            """.Trim());
    }

    [Fact]
    public void Whitelist_is_per_category_so_other_categories_still_report()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                mixed : { a | x : Int } -> a -> a -> Int
                mixed r left right =
                    if left == right then
                        r.x

                    else
                        Pine_kernel.int_add [ r.x, 1 ]
                """,
                ignoreBasicsEq: ["Test.mixed"]);

        // The == finding is suppressed but record-access.x still shows.
        rendered.Should().Be(
            """
            Test.mixed: record-access: x
            """.Trim());
    }

    [Fact]
    public void Reports_partial_application_of_top_level_function()
    {
        // `addThree` has arity 3 but is called with only 2 arguments.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                addThree : Int -> Int -> Int -> Int
                addThree a b c =
                    Pine_kernel.int_add [ Pine_kernel.int_add [ a, b ], c ]


                makeAdder : Int -> Int -> (Int -> Int)
                makeAdder a b =
                    addThree a b
                """);

        rendered.Should().Be(
            """
            Test.makeAdder: partial-application: Test.addThree(2/3)
            """.Trim());
    }

    [Fact]
    public void Reports_no_finding_for_fully_applied_top_level_function()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                addThree : Int -> Int -> Int -> Int
                addThree a b c =
                    Pine_kernel.int_add [ Pine_kernel.int_add [ a, b ], c ]


                useFully : Int -> Int -> Int -> Int
                useFully a b c =
                    addThree a b c
                """);

        rendered.Should().Be("");
    }

    [Fact]
    public void Reports_partial_application_of_let_bound_function()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                outer : Int -> Int -> Int
                outer x y =
                    let
                        inner a b c =
                            Pine_kernel.int_add [ Pine_kernel.int_add [ a, b ], c ]
                    in
                    Pine_kernel.int_mul [ x, y ]


                useInnerPartially : Int -> Int -> Int
                useInnerPartially x y =
                    let
                        inner a b c =
                            Pine_kernel.int_add [ Pine_kernel.int_add [ a, b ], c ]

                        partial =
                            inner x y
                    in
                    partial 0
                """);

        rendered.Should().Be(
            """
            Test.useInnerPartially: partial-application: inner(2/3)
            """.Trim());
    }

    [Fact]
    public void Reports_partial_application_of_prefix_operator()
    {
        // `(+) x` supplies one argument to a binary operator.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                addOnePartial : Int -> (Int -> Int)
                addOnePartial x =
                    (+) x
                """);

        // Note: `(+)` itself is also reported as Basics.arithmetic: (+)
        // by the existing Basics-arithmetic detector.
        rendered.Should().Be(
            """
            Test.addOnePartial: Basics.arithmetic: (+)
            Test.addOnePartial: partial-application: (+)(1/2)
            """.Trim());
    }

    [Fact]
    public void Whitelist_for_partial_application_suppresses_only_that_category()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                addThree : Int -> Int -> Int -> Int
                addThree a b c =
                    Pine_kernel.int_add [ Pine_kernel.int_add [ a, b ], c ]


                hot : Int -> Int -> (Int -> Int)
                hot a b =
                    addThree a b


                cold : Int -> Int -> (Int -> Int)
                cold a b =
                    addThree a b
                """,
                ignorePartialApplication: ["Test.hot"]);

        rendered.Should().Be(
            """
            Test.cold: partial-application: Test.addThree(2/3)
            """.Trim());
    }

    [Fact]
    public void Description_format_uses_added_over_total_arity()
    {
        // Calling a 4-arity function with just 1 argument reports "1/4".
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                fourArgs : Int -> Int -> Int -> Int -> Int
                fourArgs a b c d =
                    Pine_kernel.int_add
                        [ Pine_kernel.int_add [ a, b ]
                        , Pine_kernel.int_add [ c, d ]
                        ]


                onlyOne : Int -> (Int -> Int -> Int -> Int)
                onlyOne a =
                    fourArgs a
                """);

        rendered.Should().Be(
            """
            Test.onlyOne: partial-application: Test.fourArgs(1/4)
            """.Trim());
    }

    [Fact]
    public void Reports_higher_order_parameter_applied_in_top_level_function()
    {
        // Detection of higher-order parameters is purely structural:
        // a parameter that appears as the head of a function application
        // expression in the body. No type annotation is required.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                apply f x =
                    f x
                """);

        rendered.Should().Be(
            """
            Test.apply: higher-order-parameter: f
            """.Trim());
    }

    [Fact]
    public void Reports_higher_order_parameter_only_once_per_param_even_if_applied_multiple_times()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                runTwice : (Int -> Int) -> Int -> Int
                runTwice f x =
                    f (f x)
                """);

        rendered.Should().Be(
            """
            Test.runTwice: higher-order-parameter: f
            """.Trim());
    }

    [Fact]
    public void Reports_no_higher_order_parameter_when_param_is_only_passed_through()
    {
        // `g` is referenced as data but never applied, so it does not
        // need to be specialized away.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                hold : (Int -> Int) -> (Int -> Int)
                hold g =
                    g
                """);

        rendered.Should().Be("");
    }

    [Fact]
    public void Reports_higher_order_parameter_inside_lambda_body()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                wrap : (Int -> Int) -> (Int -> Int)
                wrap f =
                    \x -> f x
                """);

        rendered.Should().Be(
            """
            Test.wrap: higher-order-parameter: f
            """.Trim());
    }

    [Fact]
    public void Reports_higher_order_parameter_for_let_bound_function_parameter()
    {
        // `helper` is a let-bound function whose own parameter `g` is
        // applied inside the helper body. The finding is attributed to the
        // containing top-level decl with the let-fn name as a qualifier so
        // identically-named parameters in different scopes do not collide.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                outer : Int -> Int
                outer x =
                    let
                        helper g a =
                            g a
                    in
                    helper (\n -> Pine_kernel.int_add [ n, 1 ]) x
                """);

        rendered.Should().Be(
            """
            Test.outer: higher-order-parameter: helper.g
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_does_not_report_when_param_is_shadowed_by_let_binding()
    {
        // `f` is the outer param. Inside `helper`, the let-bound function
        // also names its parameter `f`; the inner `f x` applies the
        // let-fn parameter, not the outer one. The outer `f` is never
        // applied directly, so no higher-order-parameter finding for the
        // outer scope.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                outer : (Int -> Int) -> Int -> Int
                outer f x =
                    let
                        helper f =
                            f x
                    in
                    helper 7
                """);

        rendered.Should().Be(
            """
            Test.outer: higher-order-parameter: helper.f
            """.Trim());
    }

    [Fact]
    public void Whitelist_for_higher_order_parameter_suppresses_only_that_category()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                hot : (Int -> Int) -> Int -> Int
                hot f x =
                    f x


                cold : (Int -> Int) -> Int -> Int
                cold f x =
                    f x
                """,
                ignoreHigherOrderParameter: ["Test.hot"]);

        rendered.Should().Be(
            """
            Test.cold: higher-order-parameter: f
            """.Trim());
    }

    [Fact]
    public void RenderOpportunitiesByCategory_groups_findings_under_category_headers()
    {
        var opportunities =
            OptimizationOpportunityFinder.FindOptimizationOpportunities(
                [
                """
                module Test exposing (..)


                apply : (Int -> Int) -> Int -> Int
                apply f x =
                    f x


                showName : { a | name : String } -> String
                showName r =
                    r.name


                addThings : a -> a -> a
                addThings left right =
                    left + right


                makeAdder : Int -> Int -> Int -> Int
                makeAdder a b c =
                    Pine_kernel.int_add
                        [ Pine_kernel.int_add [ a, b ]
                        , c
                        ]


                partialAdd : Int -> Int -> (Int -> Int)
                partialAdd a b =
                    makeAdder a b
                """
                ]);

        var rendered =
            OptimizationOpportunityFinder.RenderOpportunitiesByCategory(opportunities);

        // Categories appear in the declared OpportunityCategory order
        // (RecordAccess, ..., HigherOrderParameter); empty categories are
        // omitted; within each category findings are sorted by containing
        // declaration then by description.
        rendered.Should().Be(
            """
            record-access:
              Test.showName: name

            Basics.arithmetic:
              Test.addThings: add

            partial-application:
              Test.partialAdd: Test.makeAdder(2/3)

            higher-order-parameter:
              Test.apply: f
            """.Trim());
    }

    [Fact]
    public void RenderOpportunitiesByCategory_returns_empty_for_no_findings()
    {
        OptimizationOpportunityFinder
            .RenderOpportunitiesByCategory(System.Array.Empty<Opportunity>())
            .Should().Be(string.Empty);
    }

    [Fact]
    public void RenderOpportunitiesByCategory_emits_single_group_when_only_one_category_has_findings()
    {
        var opportunities =
            OptimizationOpportunityFinder.FindOptimizationOpportunities(
                [
                """
                module Test exposing (..)


                apply : (Int -> Int) -> Int -> Int
                apply f x =
                    f x


                runTwice : (Int -> Int) -> Int -> Int
                runTwice g x =
                    g (g x)
                """
                ]);

        var rendered =
            OptimizationOpportunityFinder.RenderOpportunitiesByCategory(opportunities);

        rendered.Should().Be(
            """
            higher-order-parameter:
              Test.apply: f
              Test.runTwice: g
            """.Trim());
    }

    private static string FindAndRender(
        string elmModuleText,
        ImmutableHashSet<string>? ignoreRecordOperation = null,
        ImmutableHashSet<string>? ignoreBasicsArithmetic = null,
        ImmutableHashSet<string>? ignoreBasicsCompare = null,
        ImmutableHashSet<string>? ignoreBasicsEq = null,
        ImmutableHashSet<string>? ignoreBasicsAppend = null,
        ImmutableHashSet<string>? ignorePartialApplication = null,
        ImmutableHashSet<string>? ignoreHigherOrderParameter = null)
    {
        var opportunities =
            OptimizationOpportunityFinder.FindOptimizationOpportunities(
                [elmModuleText],
                ignoreRecordOperation,
                ignoreBasicsArithmetic,
                ignoreBasicsCompare,
                ignoreBasicsEq,
                ignoreBasicsAppend,
                ignorePartialApplication,
                ignoreHigherOrderParameter);

        return OptimizationOpportunityFinder.RenderOpportunities(opportunities);
    }

    private static System.Collections.Generic.IReadOnlyDictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Declaration>
        ParseAndCanonicalize(string elmModuleText)
    {
        var parsed =
            Pine.Core.Elm.ElmSyntax.ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var converted =
            new[] { Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(parsed) };

        var canonicalized =
            Pine.Core.Elm.ElmCompilerInDotnet.Canonicalization.Canonicalize(converted)
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var orderedModules =
            System.Linq.Enumerable.ToList(
                System.Linq.Enumerable.Select(
                    converted,
                    module =>
                    canonicalized[
                        Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module.GetModuleName(
                            module.ModuleDefinition.Value).Value]
                    .Extract(err => throw new System.Exception("Module has errors: " + err))));

        return Pine.Core.Elm.ElmCompilerInDotnet.ElmCompiler.FlattenModulesToDeclarationDictionary(orderedModules);
    }
}
