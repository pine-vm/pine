using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Immutable;
using System.Linq;
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
                withSpecialized)
            .Where(
                o =>
                !(o.Category is OpportunityCategory.RecordAccess
                              or OpportunityCategory.RecordUpdate &&
                  o.ContainingDecl.FullName.StartsWith("Test.hot", System.StringComparison.Ordinal)))
            .ToImmutableHashSet();

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
            Test.apply: higher-order-parameter-direct: f
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
            Test.runTwice: higher-order-parameter-direct: f
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


                wrap f =
                    \x -> f x
                """);

        rendered.Should().Be(
            """
            Test.wrap: higher-order-parameter-direct: f
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
            Test.outer: higher-order-parameter-direct: helper.g
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
            Test.outer: higher-order-parameter-direct: helper.f
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
            Test.cold: higher-order-parameter-direct: f
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_reports_function_from_record_field_on_parameter()
    {
        // The parameter `r` itself isn't a function, but contains a
        // function that gets applied via record-field access.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                useField r x =
                    r.go x
                """,
                ignoreRecordOperation: ["Test.useField"]);

        rendered.Should().Be(
            """
            Test.useField: higher-order-parameter-direct: r.go
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_reports_function_from_nested_record_field_on_parameter()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                useField r x =
                    r.inner.go x
                """,
                ignoreRecordOperation: ["Test.useField"]);

        rendered.Should().Be(
            """
            Test.useField: higher-order-parameter-direct: r.inner.go
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_reports_function_extracted_by_tag_pattern_in_parameter()
    {
        // `(Wrap inner)` only type-checks because `Wrap` is the single
        // tag of `Wrap a`. The bound name `inner` is then applied, so
        // the higher-order opportunity is reported.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                useWrap (Wrap inner) x =
                    inner x
                """,
                ignoreRootLevelChoiceTagWrapper: ["Test.useWrap"]);

        rendered.Should().Be(
            """
            Test.useWrap: higher-order-parameter-direct: inner
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_reports_function_extracted_by_tag_pattern_in_let_destructuring()
    {
        // Destructuring `p` in a `let` block via `(Wrap inner) = p` is
        // only well-typed when `Wrap` is the single tag of its type;
        // the resulting `inner` then participates as a higher-order
        // application head.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                useWrap p x =
                    let
                        (Wrap inner) = p
                    in
                    inner x
                """,
                ignoreRootLevelChoiceTagWrapper: ["Test.useWrap"]);

        rendered.Should().Be(
            """
            Test.useWrap: higher-order-parameter-direct: inner
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_reports_function_extracted_by_tag_pattern_in_case_of()
    {
        // The user-supplied example: `alfa` is a multi-tag value; in
        // the `Delta delta -> delta beta` branch, the bound `delta` is
        // applied. No type annotation present.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Choice = Other | Delta (Int -> Int)


                fun alfa beta =
                    case alfa of
                        Other -> 41

                        Delta delta -> delta beta
                """);

        rendered.Should().Be(
            """
            Test.fun: higher-order-parameter-direct: delta
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_reports_each_function_extracted_by_multi_arg_tag_pattern_in_case_of()
    {
        // A multi-argument constructor binds multiple function-typed
        // names; each one that is applied is reported separately.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Choice = Pair (Int -> Int) (Int -> Int)


                fun alfa beta =
                    case alfa of
                        Pair p1 p2 -> p1 (p2 beta)
                """);

        rendered.Should().Be(
            """
            Test.fun: higher-order-parameter-direct: p1
            Test.fun: higher-order-parameter-direct: p2
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_reports_functions_extracted_by_tuple_parameter_pattern()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                useTuple (f, g) x =
                    f (g x)
                """);

        rendered.Should().Be(
            """
            Test.useTuple: higher-order-parameter-direct: f
            Test.useTuple: higher-order-parameter-direct: g
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_reports_function_extracted_by_record_parameter_pattern()
    {
        // A record pattern `{go}` binds the field name as a local; if
        // the bound `go` is then applied, the opportunity is reported.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                useRecord {go} x =
                    go x
                """);

        rendered.Should().Be(
            """
            Test.useRecord: higher-order-parameter-direct: go
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_reports_function_extracted_by_as_over_named_parameter_pattern()
    {
        // The `as` pattern binds the alias `whole`; the inner named
        // pattern still binds `inner`. Only `inner` is applied here,
        // so only `inner` is reported as a higher-order parameter.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                useAs ((Wrap inner) as whole) x =
                    inner x
                """,
                ignoreRootLevelChoiceTagWrapper: ["Test.useAs"]);

        rendered.Should().Be(
            """
            Test.useAs: higher-order-parameter-direct: inner
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_does_not_report_for_pattern_extracted_name_when_never_applied()
    {
        // `inner` is bound by destructuring but never appears as the
        // head of an application — it is only returned. No
        // higher-order-parameter finding should be emitted.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                justExtract (Wrap inner) =
                    inner
                """,
                ignoreRootLevelChoiceTagWrapper: ["Test.justExtract"]);

        rendered.Should().Be("");
    }

    [Fact]
    public void Higher_order_parameter_reports_function_extracted_in_let_destructuring_inside_let_body()
    {
        // The let-bound destructured `inner` is in scope only within
        // the `in` body; the application happens there. Verifies the
        // scoping extension is applied to the body and not the RHS.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                useLater p x =
                    let
                        (Wrap inner) =
                            p
                    in
                    inner x
                """,
                ignoreRootLevelChoiceTagWrapper: ["Test.useLater"]);

        rendered.Should().Be(
            """
            Test.useLater: higher-order-parameter-direct: inner
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

            higher-order-parameter-direct:
              Test.apply: f
            """.Trim());
    }

    [Fact]
    public void RenderOpportunitiesByCategory_returns_empty_for_no_findings()
    {
        OptimizationOpportunityFinder
            .RenderOpportunitiesByCategory([])
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
            higher-order-parameter-direct:
              Test.apply: f
              Test.runTwice: g
            """.Trim());
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_detected_via_signature_when_pattern_is_bare_var()
    {
        // Evidence source: type annotation alone. The parameter pattern
        // is a bare variable so it carries no destructuring evidence,
        // and the body simply returns the parameter so there is no
        // wrap-aware return either.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                identityWrap : Wrap Int -> Wrap Int
                identityWrap value =
                    value
                """);

        rendered.Should().Be(
            """
            Test.identityWrap: root-level-choice-tag-wrapper: parameter[0] value: Test.Wrap -> Basics.Int
            Test.identityWrap: root-level-choice-tag-wrapper: return: Test.Wrap -> Basics.Int
            """.Trim());
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_detected_via_parameter_pattern_destructuring()
    {
        // Evidence source: the parameter pattern destructures the
        // single-tag constructor. There is no signature on the
        // function, so the report falls back to the constructor's
        // own argument types (no generic substitution available).
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                unwrap (Wrap inner) =
                    inner
                """);

        rendered.Should().Be(
            """
            Test.unwrap: root-level-choice-tag-wrapper: parameter[0] inner: Test.Wrap -> a
            """.Trim());
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_detected_via_let_block_destructuring_of_parameter()
    {
        // Evidence source: a top-level let block destructures the
        // bare parameter through a single-tag constructor pattern.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                useWrap p =
                    let
                        (Wrap inner) = p
                    in
                    inner
                """);

        rendered.Should().Be(
            """
            Test.useWrap: root-level-choice-tag-wrapper: parameter[0] p: Test.Wrap -> a
            """.Trim());
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_detected_when_all_return_leaves_use_the_same_constructor()
    {
        // Evidence source: every leaf of the body's return position
        // wraps with the same single-tag constructor. The function
        // has no signature; the parameter is unrelated to any wrap.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Box a = Box a


                choose flag =
                    if flag then
                        Box 1

                    else
                        Box 2
                """);

        rendered.Should().Be(
            """
            Test.choose: root-level-choice-tag-wrapper: return: Test.Box -> a
            """.Trim());
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_detected_via_return_type_annotation()
    {
        // Evidence source: the function's signature names a single-tag
        // type as the return type. The body builds the wrap inside a
        // let block; the parameter is a bare Int that does not match
        // a single-tag type.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Box a = Box a


                wrapInt : Int -> Box Int
                wrapInt n =
                    let
                        wrapped = Box n
                    in
                    wrapped
                """);

        rendered.Should().Be(
            """
            Test.wrapInt: root-level-choice-tag-wrapper: return: Test.Box -> Basics.Int
            """.Trim());
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_detects_parameter_and_return_in_canonical_parser_example()
    {
        // The user-supplied example: a Parser-shaped wrapper over a
        // function-typed inner. Both the parameter and the return
        // root types are reported and rendered with concrete generic
        // substitution applied.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type State = State Int


                type PStep a = PStep a


                type Parser a = Parser (State -> PStep a)


                skipWhileWhitespaceFollowedBy : Parser next -> Parser next
                skipWhileWhitespaceFollowedBy (Parser parseNext) =
                    Parser
                        (\s0 ->
                            parseNext s0
                        )
                """);

        rendered.Should().Be(
            """
            Test.skipWhileWhitespaceFollowedBy: higher-order-parameter-direct: parseNext
            Test.skipWhileWhitespaceFollowedBy: root-level-choice-tag-wrapper: parameter[0] parseNext: Test.Parser -> (Test.State -> Test.PStep next)
            Test.skipWhileWhitespaceFollowedBy: root-level-choice-tag-wrapper: return: Test.Parser -> (Test.State -> Test.PStep next)
            """.Trim());
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_renders_two_argument_constructor_as_pair_tuple()
    {
        // A single-tag constructor with two arguments is rendered as
        // a 2-tuple of the substituted argument types.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Pair a b = Pair a b


                mk : Pair Int String -> Pair Int String
                mk x =
                    x
                """);

        rendered.Should().Be(
            """
            Test.mk: root-level-choice-tag-wrapper: parameter[0] x: Test.Pair -> (Basics.Int, String.String)
            Test.mk: root-level-choice-tag-wrapper: return: Test.Pair -> (Basics.Int, String.String)
            """.Trim());
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_renders_three_argument_constructor_as_triple_tuple()
    {
        // A single-tag constructor with three arguments is rendered
        // as a 3-tuple. Tuples of arity > 3 are also permissible at
        // this stage of the compilation.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Triple a b c = Triple a b c


                id3 : Triple Int String Int -> Triple Int String Int
                id3 x =
                    x
                """);

        rendered.Should().Be(
            """
            Test.id3: root-level-choice-tag-wrapper: parameter[0] x: Test.Triple -> (Basics.Int, String.String, Basics.Int)
            Test.id3: root-level-choice-tag-wrapper: return: Test.Triple -> (Basics.Int, String.String, Basics.Int)
            """.Trim());
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_does_not_report_for_multi_constructor_type()
    {
        // A custom type with more than one constructor is intentionally
        // excluded from the single-tag registry; no opportunity is
        // reported even though the parameter and return both reference
        // the type at their root.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type ChoiceOfTwo a = First a | Second a


                idChoice : ChoiceOfTwo Int -> ChoiceOfTwo Int
                idChoice x =
                    x
                """);

        rendered.Should().Be("");
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_does_not_report_for_nested_wrapping()
    {
        // The single-tag constructor appears only inside another type
        // constructor (`List`). Root-level scope deliberately excludes
        // nested wrappings.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                onList : List (Wrap Int) -> List (Wrap Int)
                onList xs =
                    xs
                """);

        rendered.Should().Be("");
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_does_not_report_when_return_leaves_use_different_constructors()
    {
        // Two different single-tag constructors at distinct return
        // leaves prevent return-value detection from firing — there is
        // no single agreed-upon constructor.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type BoxA a = BoxA a


                type BoxB a = BoxB a


                pickByFlag flag =
                    if flag then
                        BoxA 1

                    else
                        BoxB 2
                """);

        rendered.Should().Be("");
    }

    [Fact]
    public void RootLevelChoiceTagWrapper_can_be_whitelisted_per_decl_prefix()
    {
        // The ignore list suppresses findings for matching containing
        // declarations. Other categories continue to fire normally
        // (here there are no other categories to report).
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                identityWrap : Wrap Int -> Wrap Int
                identityWrap value =
                    value
                """,
                ignoreRootLevelChoiceTagWrapper: ["Test.identityWrap"]);

        rendered.Should().Be("");
    }

    [Fact]
    public void TryRenderTransformedSignature_unwraps_parameter_and_return_root_types()
    {
        // The transformation utility produces a function-arrow chain
        // in which every root-level wrapping has been replaced with
        // the constructor's unwrapped (substituted) inner type — a
        // function arrow on the parameter side gets parenthesised so
        // the rendered form is unambiguous.
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                type State = State Int


                type PStep a = PStep a


                type Parser a = Parser (State -> PStep a)


                skipWhileWhitespaceFollowedBy : Parser next -> Parser next
                skipWhileWhitespaceFollowedBy (Parser parseNext) =
                    Parser
                        (\s0 ->
                            parseNext s0
                        )
                """);

        var transformed =
            OptimizationOpportunityFinder.TryRenderTransformedSignature(
                declarations,
                DeclQualifiedName.Create(["Test"], "skipWhileWhitespaceFollowedBy"));

        transformed.Should().Be(
            "(Test.State -> Test.PStep next) -> Test.State -> Test.PStep next");
    }

    [Fact]
    public void TryRenderTransformedSignature_unwraps_two_argument_constructor_to_tuple()
    {
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                type Pair a b = Pair a b


                mk : Pair Int String -> Pair Int String
                mk x =
                    x
                """);

        var transformed =
            OptimizationOpportunityFinder.TryRenderTransformedSignature(
                declarations,
                DeclQualifiedName.Create(["Test"], "mk"));

        transformed.Should().Be(
            "(Basics.Int, String.String) -> (Basics.Int, String.String)");
    }

    [Fact]
    public void TryRenderTransformedSignature_returns_null_when_function_has_no_signature()
    {
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                type Wrap a = Wrap a


                unwrap (Wrap inner) =
                    inner
                """);

        var transformed =
            OptimizationOpportunityFinder.TryRenderTransformedSignature(
                declarations,
                DeclQualifiedName.Create(["Test"], "unwrap"));

        transformed.Should().BeNull();
    }

    [Fact]
    public void Higher_order_parameter_indirect_distance_1_reported_for_pure_forwarder()
    {
        // `indirect` only forwards `f` to `direct`; `direct` is the
        // function that actually applies `f`. `direct` is reported as
        // higher-order-parameter-direct and `indirect` as
        // higher-order-parameter-indirect with distance 1.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                direct : (Int -> Int) -> Int -> Int
                direct f x =
                    f x


                indirect : (Int -> Int) -> Int -> Int
                indirect f x =
                    direct f x
                """);

        rendered.Should().Be(
            """
            Test.direct: higher-order-parameter-direct: f
            Test.indirect: higher-order-parameter-indirect: f @ distance 1
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_indirect_distance_2_reported_for_two_forwarders()
    {
        // Two hops of forwarding to a `direct` callee. The middle decl
        // is distance 1 and the outermost decl is distance 2.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                direct : (Int -> Int) -> Int -> Int
                direct f x =
                    f x


                indirect1 : (Int -> Int) -> Int -> Int
                indirect1 f x =
                    direct f x


                indirect2 : (Int -> Int) -> Int -> Int
                indirect2 f x =
                    indirect1 f x
                """);

        rendered.Should().Be(
            """
            Test.direct: higher-order-parameter-direct: f
            Test.indirect1: higher-order-parameter-indirect: f @ distance 1
            Test.indirect2: higher-order-parameter-indirect: f @ distance 2
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_indirect_distance_3_reported_for_three_forwarders()
    {
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                direct : (Int -> Int) -> Int -> Int
                direct f x =
                    f x


                indirect1 : (Int -> Int) -> Int -> Int
                indirect1 f x =
                    direct f x


                indirect2 : (Int -> Int) -> Int -> Int
                indirect2 f x =
                    indirect1 f x


                indirect3 : (Int -> Int) -> Int -> Int
                indirect3 f x =
                    indirect2 f x
                """);

        rendered.Should().Be(
            """
            Test.direct: higher-order-parameter-direct: f
            Test.indirect1: higher-order-parameter-indirect: f @ distance 1
            Test.indirect2: higher-order-parameter-indirect: f @ distance 2
            Test.indirect3: higher-order-parameter-indirect: f @ distance 3
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_indirect_takes_min_distance_across_paths()
    {
        // `mixed` forwards `f` twice: once directly to `direct` (distance 1)
        // and once through `indirect1` (distance 2). The minimum wins.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                direct : (Int -> Int) -> Int -> Int
                direct f x =
                    f x


                indirect1 : (Int -> Int) -> Int -> Int
                indirect1 f x =
                    direct f x


                mixed : (Int -> Int) -> Int -> Int
                mixed f x =
                    Pine_kernel.int_add [ direct f x, indirect1 f x ]
                """);

        rendered.Should().Be(
            """
            Test.direct: higher-order-parameter-direct: f
            Test.indirect1: higher-order-parameter-indirect: f @ distance 1
            Test.mixed: higher-order-parameter-indirect: f @ distance 1
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_indirect_not_reported_when_decl_also_applies_param_directly()
    {
        // A function that both forwards `f` to a `_Direct` callee and
        // applies `f` itself is reported as `_Direct` only; the
        // `_Indirect` finding is intentionally suppressed because the
        // direct report already drives the specialization opportunity.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                direct : (Int -> Int) -> Int -> Int
                direct f x =
                    f x


                both : (Int -> Int) -> Int -> Int
                both f x =
                    Pine_kernel.int_add [ direct f x, f x ]
                """);

        rendered.Should().Be(
            """
            Test.both: higher-order-parameter-direct: f
            Test.direct: higher-order-parameter-direct: f
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_indirect_does_not_propagate_for_non_function_argument_position()
    {
        // `outer` passes its plain Int param `n` to `direct` at the
        // second positional slot. `direct.x` is not a higher-order
        // parameter, so no indirect finding is emitted for `outer.n`.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                direct : (Int -> Int) -> Int -> Int
                direct f x =
                    f x


                outer : Int -> Int
                outer n =
                    direct (\y -> Pine_kernel.int_add [ y, 1 ]) n
                """);

        rendered.Should().Be(
            """
            Test.direct: higher-order-parameter-direct: f
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_full_listing_snapshot_with_direct_and_indirect()
    {
        // End-to-end snapshot exercising direct + indirect at multiple
        // distances mixed with non-HO findings.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                apply : (Int -> Int) -> Int -> Int
                apply f x =
                    f x


                showName : { a | name : String } -> String
                showName r =
                    r.name


                forwarderOne : (Int -> Int) -> Int -> Int
                forwarderOne f x =
                    apply f x


                forwarderTwo : (Int -> Int) -> Int -> Int
                forwarderTwo f x =
                    forwarderOne f x
                """);

        rendered.Should().Be(
            """
            Test.apply: higher-order-parameter-direct: f
            Test.forwarderOne: higher-order-parameter-indirect: f @ distance 1
            Test.forwarderTwo: higher-order-parameter-indirect: f @ distance 2
            Test.showName: record-access: name
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_indirect_through_record_pattern_parameter_and_record_argument()
    {
        // Analog of HigherOrderParameterAnalysisTests
        // .Reports_indirect_higher_order_through_record_pattern_parameter_and_record_argument.
        //
        // The callee `liftedLambda` destructures a record parameter
        // whose two fields are both directly-HO. The caller supplies a
        // record-expression literal that forwards bare references for
        // each field, so both of the caller's parameters `h` and `f`
        // must be reported as indirect-HO at distance 1.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                liftedLambda { handler, finalizer } x =
                    handler (finalizer x)


                caller h f x =
                    liftedLambda { handler = h, finalizer = f } x
                """);

        rendered.Should().Be(
            """
            Test.caller: higher-order-parameter-indirect: f @ distance 1
            Test.caller: higher-order-parameter-indirect: h @ distance 1
            Test.liftedLambda: higher-order-parameter-direct: finalizer
            Test.liftedLambda: higher-order-parameter-direct: handler
            """.Trim());
    }

    [Fact]
    public void Higher_order_parameter_indirect_distance_1_for_let_destructured_bindings_forwarded_through_tuple_to_tuple_pattern_parameter()
    {
        // Analog of HigherOrderParameterAnalysisTests
        // .Reports_indirect_distance_1_for_let_destructured_bindings_forwarded_through_tuple_to_tuple_pattern_parameter.
        //
        // `liftedLambda` takes a tuple-pattern parameter whose three
        // destructured names are each used as application heads. `caller`
        // has no parameters; it let-destructures three Parser-wrapped
        // values via the NamedPattern shape `(Parser x) = ...` into
        // local bindings, then forwards them as a tuple expression to
        // `liftedLambda`. Each let-destructured binding must be reported
        // at indirect distance 1.
        var rendered =
            FindAndRender(
                """
                module Test exposing (..)


                type Parser a = Parser a


                sourceA : Parser Int
                sourceA = Parser 0


                sourceB : Parser Int
                sourceB = Parser 0


                sourceC : Parser Int
                sourceC = Parser 0


                liftedLambda ( parseA, parseB, parseC ) s0 =
                    parseA (parseB (parseC s0))


                caller =
                    let
                        (Parser parseA_0_0) = sourceA

                        (Parser parseB_0_0) = sourceB

                        (Parser parseC_0) = sourceC
                    in
                    liftedLambda ( parseA_0_0, parseB_0_0, parseC_0 ) 0
                """,
                ignoreRootLevelChoiceTagWrapper: ["Test.sourceA", "Test.sourceB", "Test.sourceC"]);

        rendered.Should().Be(
            """
            Test.caller: higher-order-parameter-indirect: parseA_0_0 @ distance 1
            Test.caller: higher-order-parameter-indirect: parseB_0_0 @ distance 1
            Test.caller: higher-order-parameter-indirect: parseC_0 @ distance 1
            Test.liftedLambda: higher-order-parameter-direct: parseA
            Test.liftedLambda: higher-order-parameter-direct: parseB
            Test.liftedLambda: higher-order-parameter-direct: parseC
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
        ImmutableHashSet<string>? ignoreHigherOrderParameter = null,
        ImmutableHashSet<string>? ignoreRootLevelChoiceTagWrapper = null)
    {
        var opportunities =
            OptimizationOpportunityFinder.FindOptimizationOpportunities(
                [elmModuleText]);

        // The whitelist parameters are no longer part of
        // OptimizationOpportunityFinder's API; callers filter the
        // returned set themselves. The helper preserves the old
        // per-category prefix-match semantics for the existing tests
        // that exercised the old surface.
        var filtered =
            opportunities
            .Where(
                o =>
                !MatchesAnyPrefix(o, ignoreRecordOperation,
                    OpportunityCategory.RecordAccess, OpportunityCategory.RecordUpdate) &&
                !MatchesAnyPrefix(o, ignoreBasicsArithmetic, OpportunityCategory.BasicsArithmetic) &&
                !MatchesAnyPrefix(o, ignoreBasicsCompare, OpportunityCategory.BasicsCompare) &&
                !MatchesAnyPrefix(o, ignoreBasicsEq, OpportunityCategory.BasicsEq) &&
                !MatchesAnyPrefix(o, ignoreBasicsAppend, OpportunityCategory.BasicsAppend) &&
                !MatchesAnyPrefix(o, ignorePartialApplication, OpportunityCategory.PartialApplication) &&
                !MatchesAnyPrefix(o, ignoreHigherOrderParameter,
                    OpportunityCategory.HigherOrderParameter_Direct,
                    OpportunityCategory.HigherOrderParameter_Indirect) &&
                !MatchesAnyPrefix(o, ignoreRootLevelChoiceTagWrapper,
                    OpportunityCategory.RootLevelChoiceTagWrapper))
            .ToImmutableHashSet();

        return OptimizationOpportunityFinder.RenderOpportunities(filtered);
    }

    private static bool MatchesAnyPrefix(
        Opportunity opportunity,
        ImmutableHashSet<string>? prefixes,
        params OpportunityCategory[] applicableCategories)
    {
        if (prefixes is null || prefixes.Count is 0)
            return false;

        if (System.Array.IndexOf(applicableCategories, opportunity.Category) < 0)
            return false;

        var fullName = opportunity.ContainingDecl.FullName;

        foreach (var prefix in prefixes)
        {
            if (fullName.StartsWith(prefix, System.StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    private static System.Collections.Generic.IReadOnlyDictionary<DeclQualifiedName, Core.Elm.ElmSyntax.Stil4mElmSyntax7.Declaration>
        ParseAndCanonicalize(string elmModuleText)
    {
        var parsed =
            Core.Elm.ElmSyntax.ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var converted =
            new[] { Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(parsed) };

        var canonicalized =
            Canonicalization.Canonicalize(converted)
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var orderedModules =
            Enumerable.ToList(
                Enumerable.Select(
                    converted,
                    module =>
                    canonicalized[
                        Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module.GetModuleName(
                            module.ModuleDefinition.Value).Value]
                    .Extract(err => throw new System.Exception("Module has errors: " + err))));

        return ElmCompiler.FlattenModulesToDeclarationDictionary(orderedModules);
    }
}
