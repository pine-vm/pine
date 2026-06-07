using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Tests for <see cref="HigherOrderParameterAnalysis"/>. Every test in
/// this file goes exclusively through the generic API:
/// <see cref="HigherOrderParameterAnalysis.FindHigherOrderFindingsForDeclaration"/>
/// (for assertions about a single declaration) and
/// <see cref="HigherOrderParameterAnalysis.FindAllHigherOrderFindings"/>
/// (for assertions that span multiple declarations). The analysis is
/// responsible for aggregating intra-procedural and cross-declaration
/// higher-order findings — across destructured parameter names AND
/// let-introduced bindings — into one syntax-form-independent result
/// set.
/// </summary>
public class HigherOrderParameterAnalysisTests
{
    [Fact]
    public void Reports_no_findings_for_first_order_function()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            addOne n = n + 1
            """,
            declName: "addOne",
            expected: []);
    }

    [Fact]
    public void Reports_first_parameter_of_apply()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            apply f x = f x
            """,
            declName: "apply",
            expected: [("f", 0)]);
    }

    [Fact]
    public void Reports_only_function_parameter_for_callOn()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            callOn x f = f x
            """,
            declName: "callOn",
            expected: [("f", 0)]);
    }

    [Fact]
    public void Reports_both_function_parameters_of_compose()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            compose f g x = f (g x)
            """,
            declName: "compose",
            expected: [("f", 0), ("g", 0)]);
    }

    [Fact]
    public void Reports_parameter_when_used_via_let_value()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            apply f x =
                let
                    g = f
                in
                g x
            """,
            declName: "apply",
            expected: [("f", 0)]);
    }

    [Fact]
    public void Reports_parameter_when_used_via_let_function_body()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            apply f x =
                let
                    g a = f a
                in
                g x
            """,
            declName: "apply",
            expected: [("f", 0)]);
    }

    [Fact]
    public void Reports_parameter_passed_as_argument_to_local_let_function()
    {
        // For `apply f x = let g a = a x in g f`:
        //
        // * The data-flow analysis traces the application head `g` back
        //   through the let-binding to its synthetic lambda `\a -> a x`,
        //   whose free variables are { x }. Therefore `x` is reported
        //   as flowing into a function head.
        //
        // * The argument `f` is bound to the local parameter `a` inside
        //   `g`'s body; detecting that `f` reaches the inner head `a`
        //   would require beta-reducing the call `g f`, which the
        //   current analysis does not perform. So `f` is *not* reported
        //   here — same documented limitation as the
        //   <c>ElmExpressionDataFlowTests.Flow_through_argument_to_local_let_function</c>
        //   case.
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            apply f x =
                let
                    g a = a x
                in
                g f
            """,
            declName: "apply",
            expected: [("x", 0)]);
    }

    [Fact]
    public void Reports_parameter_used_only_inside_nested_lambda()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            wrap f = (\x -> f x)
            """,
            declName: "wrap",
            expected: [("f", 0)]);
    }

    [Fact]
    public void Reports_parameter_used_only_as_argument_at_distance_zero()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            applyToSeven f = f 7
            """,
            declName: "applyToSeven",
            expected: [("f", 0)]);
    }

    [Fact]
    public void Does_not_report_parameter_used_only_as_data()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            wrapData x = [ x, x ]
            """,
            declName: "wrapData",
            expected: []);
    }

    [Fact]
    public void Reports_constructor_arg_parameter_when_unwrapped_and_invoked()
    {
        // The pattern `(Wrap inner)` binds `inner`; the body invokes
        // `inner` as a function, so the destructured `inner` name is
        // higher-order.
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            type Wrap a = Wrap a

            invoke (Wrap inner) x = inner x
            """,
            declName: "invoke",
            expected: [("inner", 0)]);
    }

    [Fact]
    public void Reports_tuple_pattern_member_used_as_function()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            useFirst (f, x) = f x
            """,
            declName: "useFirst",
            expected: [("f", 0)]);
    }

    [Fact]
    public void Reports_record_pattern_member_used_as_function()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            dispatch { handler, value } = handler value
            """,
            declName: "dispatch",
            expected: [("handler", 0)]);
    }

    [Fact]
    public void Does_not_report_when_only_qualified_reference_is_invoked()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            useStd a = List.map (\x -> a) [ 1 ]
            """,
            declName: "useStd",
            expected: []);
    }

    [Fact]
    public void Reports_parameter_via_chained_let_aliases()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            apply f x =
                let
                    g = f
                    h = g
                in
                h x
            """,
            declName: "apply",
            expected: [("f", 0)]);
    }

    [Fact]
    public void Reports_parameter_used_inside_if_branch_application_head()
    {
        AssertHigherOrderFindingsForDecl(
            """
            module Test exposing (..)

            choose cond f g x =
                if cond then
                    f x
                else
                    g x
            """,
            declName: "choose",
            expected: [("f", 0), ("g", 0)]);
    }

    // ------------------------------------------------------------------
    // Cross-top-level-declaration (transitive) higher-order parameter
    // detection — mirrors the call graph in the failing test
    // `Wrapper_then_intermediate_around_recursive_higher_order_helper_…`.
    //
    // Every assertion below queries the generic
    // <see cref="HigherOrderParameterAnalysis.FindAllHigherOrderFindings"/>
    // and inspects the per-declaration findings (name + distance).
    // ------------------------------------------------------------------

    [Fact]
    public void Reports_parameter_forwarded_to_higher_order_callee_parameter()
    {
        // The shape from the failing test
        // `Wrapper_then_intermediate_around_recursive_higher_order_helper_…`:
        // `applyN` is intra-procedurally higher-order at `f`. `while_`
        // only mentions `f` as an argument to `applyN`, never in head
        // position — so a flow analysis local to `while_`'s body finds
        // nothing. The cross-declaration aggregation must detect that
        // `f` flows into `applyN`'s higher-order argument position and
        // therefore mark `while_`'s `f` as higher-order at distance 1.
        const string ModuleText =
            """
            module Test exposing (..)

            type Parser a = Parser (Int -> a)


            applyN f n x =
                if n == 0 then
                    x
                else
                    applyN f (n - 1) (f x)


            while_ f =
                Parser
                    (\x ->
                        applyN f 10 x
                    )
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "applyN").Should().BeEquivalentTo([("f", 0)]);
        FindingsOf(all, "while_").Should().BeEquivalentTo([("f", 1)]);
    }

    [Fact]
    public void Reports_chain_of_forwarded_higher_order_parameters()
    {
        // Two-hop chain: `applyN.f` is HO; `mid` forwards its first
        // parameter into `applyN`; `outer` forwards its first parameter
        // into `mid`. Distances should grow by 1 along the chain.
        const string ModuleText =
            """
            module Test exposing (..)

            applyN f n x =
                if n == 0 then
                    x
                else
                    applyN f (n - 1) (f x)


            mid g x =
                applyN g 5 x


            outer h x =
                mid h x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "applyN").Should().BeEquivalentTo([("f", 0)]);
        FindingsOf(all, "mid").Should().BeEquivalentTo([("g", 1)]);
        FindingsOf(all, "outer").Should().BeEquivalentTo([("h", 2)]);
    }

    [Fact]
    public void Does_not_report_parameter_passed_to_non_higher_order_callee_position()
    {
        // `applyN`'s parameter `n` and `x` are NOT higher-order (only
        // `f` is). A wrapper that passes its parameter into one of
        // those positions must NOT be reported as HO.
        const string ModuleText =
            """
            module Test exposing (..)

            applyN f n x =
                if n == 0 then
                    x
                else
                    applyN f (n - 1) (f x)


            constN c x =
                applyN (\v -> v) c x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "applyN").Should().BeEquivalentTo([("f", 0)]);
        // constN's `c` flows into applyN's parameter `n` and `x` into
        // parameter `x`; neither of those is HO at applyN, so constN
        // has no findings.
        FindingsOf(all, "constN").Should().BeEmpty();
    }

    [Fact]
    public void Propagation_traces_argument_through_let_binding_alias()
    {
        // `wrap` aliases its `f` parameter to `g` via a let-binding
        // before forwarding it to applyN. The generic aggregation must
        // report both `f` (traced through let-RHS) and `g` (the let
        // binding itself flowing into the HO arg position) at distance
        // 1.
        const string ModuleText =
            """
            module Test exposing (..)

            applyN f n x =
                if n == 0 then
                    x
                else
                    applyN f (n - 1) (f x)


            wrap f x =
                let
                    g = f
                in
                applyN g 5 x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "applyN").Should().BeEquivalentTo([("f", 0)]);
        FindingsOf(all, "wrap").Should().BeEquivalentTo([("f", 1), ("g", 1)]);
    }

    // ------------------------------------------------------------------
    // Indirect higher-order finding through chains of pure forwarders.
    // ------------------------------------------------------------------

    [Fact]
    public void Reports_indirect_higher_order_for_one_forwarder()
    {
        const string ModuleText =
            """
            module Test exposing (..)


            direct f x =
                f x


            indirect f x =
                direct f x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "direct").Should().BeEquivalentTo([("f", 0)]);
        FindingsOf(all, "indirect").Should().BeEquivalentTo([("f", 1)]);
    }

    [Fact]
    public void Reports_indirect_higher_order_for_two_forwarders()
    {
        const string ModuleText =
            """
            module Test exposing (..)


            direct f x =
                f x


            indirect1 f x =
                direct f x


            indirect2 f x =
                indirect1 f x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "direct").Should().BeEquivalentTo([("f", 0)]);
        FindingsOf(all, "indirect1").Should().BeEquivalentTo([("f", 1)]);
        FindingsOf(all, "indirect2").Should().BeEquivalentTo([("f", 2)]);
    }

    [Fact]
    public void Reports_indirect_higher_order_for_three_forwarders()
    {
        const string ModuleText =
            """
            module Test exposing (..)


            direct f x =
                f x


            indirect1 f x =
                direct f x


            indirect2 f x =
                indirect1 f x


            indirect3 f x =
                indirect2 f x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "direct").Should().BeEquivalentTo([("f", 0)]);
        FindingsOf(all, "indirect1").Should().BeEquivalentTo([("f", 1)]);
        FindingsOf(all, "indirect2").Should().BeEquivalentTo([("f", 2)]);
        FindingsOf(all, "indirect3").Should().BeEquivalentTo([("f", 3)]);
    }

    // ------------------------------------------------------------------
    // Challenging data-flow shapes: the value that is later applied as
    // a function is not a bare reference but reaches the call site
    // through a destructuring or record-projection step. The generic
    // API must still aggregate all caller-owned names whose values flow
    // through those steps.
    // ------------------------------------------------------------------

    [Fact]
    public void Reports_indirect_higher_order_through_let_tuple_destructuring()
    {
        // `viaLetTuple` destructures its parameter `pair` with a
        // let-pattern and forwards the destructured `g` to the
        // higher-order callee `direct`. The aggregation must report
        // both:
        //   * the parameter `pair` (its value flows into the call site
        //     argument via the let-RHS), and
        //   * the let-introduced `g` (the bare reference at the call
        //     site).
        const string ModuleText =
            """
            module Test exposing (..)


            direct f x =
                f x


            viaLetTuple pair x =
                let
                    ( g, _ ) = pair
                in
                direct g x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "direct").Should().BeEquivalentTo([("f", 0)]);
        FindingsOf(all, "viaLetTuple").Should().BeEquivalentTo([("g", 1), ("pair", 1)]);
    }

    [Fact]
    public void Reports_indirect_higher_order_through_case_branch_destructuring()
    {
        // `viaCase` destructures its parameter `pair` in a case-branch
        // pattern and forwards `g` to `direct`. The case-branch binding
        // `g` is not a top-level owned name, but the data-flow analysis
        // traces it back through the scrutinee mapping recorded by
        // <see cref="SyntaxTypes.SyntaxAnalysis.VisitApplications"/>'s
        // case-branch handling, so the caller's parameter `pair` is
        // reported as higher-order at distance 1.
        const string ModuleText =
            """
            module Test exposing (..)


            direct f x =
                f x


            viaCase pair x =
                case pair of
                    ( g, _ ) ->
                        direct g x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "direct").Should().BeEquivalentTo([("f", 0)]);
        FindingsOf(all, "viaCase").Should().BeEquivalentTo([("pair", 1)]);
    }

    [Fact]
    public void Reports_indirect_higher_order_through_record_expression_and_access()
    {
        // `viaRecord` packs its parameter `fn` into a record literal
        // and later projects it back out via `.fn` before forwarding to
        // `direct`. The aggregation must report:
        //   * the parameter `fn` (traced back through the record
        //     literal's value expression for the `fn` field);
        //   * the let-introduced `record` (the value flows into the
        //     argument via the record-access expression `record.fn`).
        const string ModuleText =
            """
            module Test exposing (..)


            direct f x =
                f x


            viaRecord fn x =
                let
                    record = { fn = fn, n = 0 }
                in
                direct record.fn x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "direct").Should().BeEquivalentTo([("f", 0)]);
        FindingsOf(all, "viaRecord").Should().BeEquivalentTo([("fn", 1), ("record", 1)]);
    }

    // ------------------------------------------------------------------
    // Regression tests for the section-16 shape from the parseFile
    // monomorphization report: a callee whose parameter is a
    // destructuring pattern (tuple, record, named-constructor) bound to
    // names that are themselves intra-procedurally higher-order; a
    // caller that passes per-position values into that destructured
    // parameter via a matching tuple-expression / record-expression /
    // named-constructor argument.
    // ------------------------------------------------------------------

    [Fact]
    public void Reports_indirect_higher_order_through_tuple_pattern_parameter_and_tuple_argument()
    {
        // Most reduced version of the section-16 shape using top-level
        // parameters only (no let-destructuring): forwarding `(f, g)`
        // into a callee whose first parameter is a tuple pattern that
        // destructures to two directly-HO names must report the
        // caller's `f` and `g` as indirect-HO.
        const string ModuleText =
            """
            module Test exposing (..)


            liftedLambda ( a, b ) x =
                a (b x)


            caller f g x =
                liftedLambda ( f, g ) x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "liftedLambda").Should().BeEquivalentTo([("a", 0), ("b", 0)]);
        FindingsOf(all, "caller").Should().BeEquivalentTo([("f", 1), ("g", 1)]);
    }

    [Fact]
    public void Reports_indirect_higher_order_through_nested_tuple_pattern_and_tuple_argument()
    {
        // The callee's tuple-pattern parameter nests another tuple
        // pattern. Caller passes a nested tuple expression. Each
        // forwarded name is reported at distance 1.
        const string ModuleText =
            """
            module Test exposing (..)


            liftedLambda ( a, ( b, c ) ) x =
                a (b (c x))


            caller f g h x =
                liftedLambda ( f, ( g, h ) ) x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "liftedLambda").Should().BeEquivalentTo([("a", 0), ("b", 0), ("c", 0)]);
        FindingsOf(all, "caller").Should().BeEquivalentTo([("f", 1), ("g", 1), ("h", 1)]);
    }

    [Fact]
    public void Reports_indirect_higher_order_through_named_constructor_pattern_and_constructor_argument()
    {
        // The callee's parameter is a NamedPattern (constructor)
        // destructuring `(Wrap inner)`; the inner name is directly HO.
        // Caller forwards `(Wrap f)` as the argument expression. The
        // caller's `f` must be reported as indirect-HO.
        const string ModuleText =
            """
            module Test exposing (..)


            type Wrap a = Wrap a


            liftedLambda (Wrap inner) x =
                inner x


            caller f x =
                liftedLambda (Wrap f) x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "liftedLambda").Should().BeEquivalentTo([("inner", 0)]);
        FindingsOf(all, "caller").Should().BeEquivalentTo([("f", 1)]);
    }

    [Fact]
    public void Reports_indirect_higher_order_through_record_pattern_parameter_and_record_argument()
    {
        // The callee's parameter is a RecordPattern destructuring two
        // fields, both directly HO. Caller passes a record-expression
        // literal that supplies bare references for each field. Both of
        // the caller's parameters must be reported as indirect-HO.
        const string ModuleText =
            """
            module Test exposing (..)


            liftedLambda { handler, finalizer } x =
                handler (finalizer x)


            caller h f x =
                liftedLambda { handler = h, finalizer = f } x
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "liftedLambda").Should().BeEquivalentTo([("finalizer", 0), ("handler", 0)]);
        FindingsOf(all, "caller").Should().BeEquivalentTo([("f", 1), ("h", 1)]);
    }

    [Fact]
    public void Reports_indirect_higher_order_through_nested_record_pattern_and_nested_record_argument()
    {
        // The callee destructures a record whose outer field's value is
        // itself a record (modelled as a wrapped named-pattern around a
        // record pattern, since Elm does not allow direct nested record
        // deconstruction). Use a hybrid: outer record pattern holds the
        // directly-HO outerHandler plus the wrapped record whose
        // let-destructured names are also HO heads.
        const string ModuleText =
            """
            module Test exposing (..)


            type Wrap a = Wrap a


            liftedLambda { outerHandler, innerPair } x =
                let
                    (Wrap { innerHandler, innerFinal }) = innerPair
                in
                outerHandler (innerHandler (innerFinal x))


            caller h1 h2 f x =
                liftedLambda
                    { outerHandler = h1
                    , innerPair = Wrap { innerHandler = h2, innerFinal = f }
                    }
                    x
            """;

        var all = BuildAllFindings(ModuleText);

        // For liftedLambda: `outerHandler` is directly HO; `innerHandler`
        // and `innerFinal` trace back through the let-destructure RHS to
        // `innerPair`, so they aren't seeds in their own right but the
        // wrapping `innerPair` is.
        FindingsOf(all, "liftedLambda")
            .Should().BeEquivalentTo([("innerPair", 0), ("outerHandler", 0)]);

        FindingsOf(all, "caller")
            .Should().BeEquivalentTo([("f", 1), ("h1", 1), ("h2", 1)]);
    }

    // ------------------------------------------------------------------
    // Section-16 shape — let-destructured bindings in the caller flow
    // into a tuple-pattern HO callee parameter. The generic API
    // aggregates these alongside top-level parameter findings, so the
    // assertion below uses the same helpers as every other test in this
    // file.
    // ------------------------------------------------------------------

    [Fact]
    public void Reports_indirect_distance_1_for_let_destructured_bindings_forwarded_through_tuple_to_tuple_pattern_parameter()
    {
        // Direct reduction of section 16 of
        // explore/internal-analysis/2026-05-10-monomorphization-remaining-opportunities-after-lowering-for-parse-file.md
        //
        //   * `liftedLambda` (mirrors
        //     `expressionAfterOpeningSquareBracket__lifted__lambda3`) takes
        //     a tuple-pattern parameter at index 0 whose three
        //     destructured names are each used as application heads.
        //   * `caller` (mirrors `expressionAfterOpeningSquareBracket`) has
        //     no parameters; it let-destructures three Parser-wrapped
        //     values (via the NamedPattern shape `(Parser x) = ...`)
        //     into local bindings, then forwards them as a tuple
        //     expression to `liftedLambda`.
        //
        // All three let-destructured bindings must be reported at
        // indirect distance 1: each forwards one tuple-element hop into
        // a directly-HO destructured-name position of `liftedLambda`'s
        // tuple-pattern parameter.
        const string ModuleText =
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
            """;

        var all = BuildAllFindings(ModuleText);

        FindingsOf(all, "liftedLambda")
            .Should().BeEquivalentTo([("parseA", 0), ("parseB", 0), ("parseC", 0)]);

        FindingsOf(all, "caller")
            .Should().BeEquivalentTo(
            [
            ("parseA_0_0", 1),
            ("parseB_0_0", 1),
            ("parseC_0", 1),
            ]);
    }

    // ------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------

    /// <summary>
    /// Asserts the generic per-declaration findings of
    /// <see cref="HigherOrderParameterAnalysis.FindHigherOrderFindingsForDeclaration"/>
    /// for one declaration of the module under test.
    /// </summary>
    private static void AssertHigherOrderFindingsForDecl(
        string moduleText,
        string declName,
        IReadOnlyList<(string Name, int Distance)> expected)
    {
        var declarations = BuildDeclarationDictionary(moduleText);
        var decl = QName("Test", declName);

        var actual =
            HigherOrderParameterAnalysis
            .FindHigherOrderFindingsForDeclaration(declarations, decl);

        ToSortedTuples(actual).Should().BeEquivalentTo(SortExpected(expected));
    }

    /// <summary>
    /// Returns the full all-declaration generic findings dictionary for
    /// the given Elm module text.
    /// </summary>
    private static ImmutableDictionary<DeclQualifiedName, ImmutableArray<HigherOrderParameterAnalysis.HigherOrderFinding>>
        BuildAllFindings(string moduleText)
    {
        var declarations = BuildDeclarationDictionary(moduleText);
        return HigherOrderParameterAnalysis.FindAllHigherOrderFindings(declarations);
    }

    /// <summary>
    /// Extracts and sorts the findings for one named declaration from
    /// an aggregate findings dictionary.
    /// </summary>
    private static IReadOnlyList<(string Name, int Distance)> FindingsOf(
        ImmutableDictionary<DeclQualifiedName, ImmutableArray<HigherOrderParameterAnalysis.HigherOrderFinding>> all,
        string declName)
    {
        var key = QName("Test", declName);

        if (!all.TryGetValue(key, out var findings))
            return [];

        return ToSortedTuples(findings);
    }

    private static IReadOnlyList<(string Name, int Distance)> ToSortedTuples(
        IReadOnlyList<HigherOrderParameterAnalysis.HigherOrderFinding> findings)
    {
        return
            [
            .. findings
            .Select(f => (f.Name, f.Distance))
            .OrderBy(t => t.Name, System.StringComparer.Ordinal)
            .ThenBy(t => t.Distance)
            ];
    }

    private static IReadOnlyList<(string Name, int Distance)> SortExpected(
        IReadOnlyList<(string Name, int Distance)> expected)
    {
        return
            [
            .. expected
            .OrderBy(t => t.Name, System.StringComparer.Ordinal)
            .ThenBy(t => t.Distance)
            ];
    }

    private static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        BuildDeclarationDictionary(string moduleText)
    {
        var parsed =
            Core.Elm.ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var converted =
            SyntaxTypes.FromFullSyntaxModel.Convert(parsed);

        var moduleName =
            SyntaxTypes.Module
            .GetModuleName(converted.ModuleDefinition.Value).Value;

        var builder = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var declNode in converted.Declarations)
        {
            switch (declNode.Value)
            {
                case SyntaxTypes.Declaration.FunctionDeclaration funcDecl:
                    {
                        var name = funcDecl.Function.Declaration.Value.Name.Value;
                        builder[DeclQualifiedName.Create(moduleName, name)] = funcDecl;
                        break;
                    }
            }
        }

        return builder.ToImmutable();
    }

    private static DeclQualifiedName QName(string moduleName, string declName) =>
        DeclQualifiedName.Create([moduleName], declName);
}
