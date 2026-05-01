using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Text;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises functions from <c>elm-kernel-modules/Basics.elm</c> through
/// <see cref="ElmInterpreter"/>, with an emphasis on <c>compare</c>. The Basics module is
/// loaded verbatim from <see cref="BundledFiles.CompilerSourceContainerFilesDefault"/> so the
/// interpreter has to faithfully drive the same control flow that the production compiler
/// would otherwise lower to native Pine — including pattern-matching named constructors
/// (<c>String stringA</c>, <c>Elm_Float numA denomA</c>) against the corresponding primitive
/// <see cref="ElmValue"/> variants, and recursive <c>compareList</c> over heterogeneous
/// nested values.
/// <para />
/// Test cases are modeled as Elm expression strings whose evaluated result is rendered back
/// to its Elm-expression form via <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/> and
/// compared against a string snapshot — the same style as
/// <c>CaseBlockTests.NamedPattern_with_shuffled_order_in_source_code</c>.
/// </summary>
public class CoreBasicsTests
{
    private static readonly Lazy<IReadOnlyDictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration>> s_declarations =
        new(LoadBasicsDeclarations);

    private static IReadOnlyDictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration>
        LoadBasicsDeclarations()
    {
        // Merge Basics.elm and Dict.elm into a single empty-namespace declarations
        // dictionary. Basics.elm is the primary unit under test; Dict.elm is loaded
        // alongside so that tests for `eq` on Dict values can construct Dicts via
        // `empty`, `insert`, `fromList`, etc. The two modules' declaration names do
        // not collide.
        var merged = new Dictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration>();

        foreach (var (key, value) in LoadKernelModuleDeclarations("Basics.elm"))
        {
            merged[key] = value;
        }

        foreach (var (key, value) in LoadKernelModuleDeclarations("Dict.elm"))
        {
            merged[key] = value;
        }

        return merged;
    }

    private static IReadOnlyDictionary<DeclQualifiedName, Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration>
        LoadKernelModuleDeclarations(string fileName)
    {
        var moduleNode =
            BundledFiles.ElmKernelModulesDefault.Value
            .GetNodeAtPath([fileName])
            ?? throw new Exception("Did not find elm-kernel-modules/" + fileName + " in bundled files.");

        if (moduleNode is not Files.FileTree.FileNode moduleFile)
        {
            throw new Exception(
                "Expected elm-kernel-modules/" + fileName + " to be a file node, but got: " + moduleNode.GetType());
        }

        var moduleSource = Encoding.UTF8.GetString(moduleFile.Bytes.Span);

        return InterpreterTestHelper.ParseDeclarationsRemovingModuleNames(moduleSource);
    }

    /// <summary>
    /// Evaluates the supplied Elm <paramref name="expression"/> against the Basics.elm
    /// declarations and returns the rendered Elm-expression form of the resulting value.
    /// </summary>
    private static string Evaluate(string expression) =>
        ElmValue.RenderAsElmExpression(
            ElmInterpreter.ParseAndInterpret(expression, s_declarations.Value)
            .Extract(err => throw new Exception(err.ToString())))
        .expressionString;

    // ============================================================
    // compare on integers
    // ============================================================

    [Fact]
    public void Compare_two_equal_zero_integers_returns_EQ()
    {
        Evaluate("compare 0 0").Should().Be("EQ");
    }

    [Fact]
    public void Compare_two_equal_positive_integers_returns_EQ()
    {
        Evaluate("compare 42 42").Should().Be("EQ");
    }

    [Fact]
    public void Compare_two_equal_negative_integers_returns_EQ()
    {
        Evaluate("compare (-7) (-7)").Should().Be("EQ");
    }

    [Fact]
    public void Compare_smaller_positive_to_larger_positive_returns_LT()
    {
        Evaluate("compare 3 4").Should().Be("LT");
    }

    [Fact]
    public void Compare_larger_positive_to_smaller_positive_returns_GT()
    {
        Evaluate("compare 5 4").Should().Be("GT");
    }

    [Fact]
    public void Compare_negative_to_positive_returns_LT()
    {
        Evaluate("compare (-1) 1").Should().Be("LT");
    }

    [Fact]
    public void Compare_positive_to_negative_returns_GT()
    {
        Evaluate("compare 1 (-1)").Should().Be("GT");
    }

    [Fact]
    public void Compare_two_negative_integers_orders_by_magnitude()
    {
        Evaluate("compare (-9) (-3)").Should().Be("LT");
        Evaluate("compare (-3) (-9)").Should().Be("GT");
    }

    [Fact]
    public void Compare_zero_to_negative_is_GT()
    {
        Evaluate("compare 0 (-1)").Should().Be("GT");
    }

    [Fact]
    public void Compare_zero_to_positive_is_LT()
    {
        Evaluate("compare 0 1").Should().Be("LT");
    }

    [Fact]
    public void Compare_large_integers()
    {
        Evaluate("compare 1000000000 999999999").Should().Be("GT");
        Evaluate("compare 999999999 1000000000").Should().Be("LT");
        Evaluate("compare 1000000000 1000000000").Should().Be("EQ");
    }

    // ============================================================
    // compare on strings
    // ============================================================

    [Fact]
    public void Compare_two_empty_strings_returns_EQ()
    {
        Evaluate("compare \"\" \"\"").Should().Be("EQ");
    }

    [Fact]
    public void Compare_empty_string_to_non_empty_returns_LT()
    {
        Evaluate("compare \"\" \"a\"").Should().Be("LT");
    }

    [Fact]
    public void Compare_non_empty_string_to_empty_returns_GT()
    {
        Evaluate("compare \"a\" \"\"").Should().Be("GT");
    }

    [Fact]
    public void Compare_two_equal_non_empty_strings_returns_EQ()
    {
        Evaluate("compare \"hello\" \"hello\"").Should().Be("EQ");
    }

    [Fact]
    public void Compare_strings_lexicographically_first_char_differs()
    {
        Evaluate("compare \"apple\" \"banana\"").Should().Be("LT");
        Evaluate("compare \"banana\" \"apple\"").Should().Be("GT");
    }

    [Fact]
    public void Compare_strings_where_one_is_prefix_of_other_orders_shorter_first()
    {
        Evaluate("compare \"abc\" \"abcd\"").Should().Be("LT");
        Evaluate("compare \"abcd\" \"abc\"").Should().Be("GT");
    }

    [Fact]
    public void Compare_strings_differ_in_later_char()
    {
        Evaluate("compare \"abcdef\" \"abcdez\"").Should().Be("LT");
        Evaluate("compare \"abcdez\" \"abcdef\"").Should().Be("GT");
    }

    [Fact]
    public void Compare_single_char_strings()
    {
        Evaluate("compare \"a\" \"b\"").Should().Be("LT");
        Evaluate("compare \"b\" \"a\"").Should().Be("GT");
        Evaluate("compare \"a\" \"a\"").Should().Be("EQ");
    }

    [Fact]
    public void Compare_strings_at_4_byte_chunk_boundary()
    {
        // compareStrings advances 4 bytes (one UTF-32 char) at a time; force the
        // difference to land at the second chunk so we exercise the recursive call.
        Evaluate("compare \"abcdA\" \"abcdB\"").Should().Be("LT");
        Evaluate("compare \"abcdB\" \"abcdA\"").Should().Be("GT");
    }

    [Fact]
    public void Compare_strings_with_uppercase_vs_lowercase()
    {
        // ASCII uppercase precedes lowercase.
        Evaluate("compare \"A\" \"a\"").Should().Be("LT");
        Evaluate("compare \"a\" \"A\"").Should().Be("GT");
    }

    [Fact]
    public void Compare_strings_with_high_codepoint()
    {
        // Multi-byte UTF-32 character (above the ASCII range).
        Evaluate("compare \"é\" \"e\"").Should().Be("GT");
        Evaluate("compare \"e\" \"é\"").Should().Be("LT");
        Evaluate("compare \"é\" \"é\"").Should().Be("EQ");
    }

    // ============================================================
    // compare on lists of integers
    // ============================================================

    [Fact]
    public void Compare_two_empty_lists_returns_EQ()
    {
        Evaluate("compare [] []").Should().Be("EQ");
    }

    [Fact]
    public void Compare_empty_list_to_non_empty_returns_LT()
    {
        Evaluate("compare [] [ 1 ]").Should().Be("LT");
    }

    [Fact]
    public void Compare_non_empty_list_to_empty_returns_GT()
    {
        Evaluate("compare [ 1 ] []").Should().Be("GT");
    }

    [Fact]
    public void Compare_two_equal_lists_returns_EQ()
    {
        Evaluate("compare [ 1, 2, 3 ] [ 1, 2, 3 ]").Should().Be("EQ");
    }

    [Fact]
    public void Compare_lists_first_element_differs()
    {
        Evaluate("compare [ 1, 99, 99 ] [ 2, 0, 0 ]").Should().Be("LT");
        Evaluate("compare [ 2, 0, 0 ] [ 1, 99, 99 ] ").Should().Be("GT");
    }

    [Fact]
    public void Compare_lists_where_one_is_prefix_of_other()
    {
        Evaluate("compare [ 1, 2 ] [ 1, 2, 3 ]").Should().Be("LT");
        Evaluate("compare [ 1, 2, 3 ] [ 1, 2 ]").Should().Be("GT");
    }

    [Fact]
    public void Compare_lists_differ_in_later_element()
    {
        Evaluate("compare [ 1, 2, 3, 4 ] [ 1, 2, 3, 5 ]").Should().Be("LT");
        Evaluate("compare [ 1, 2, 3, 5 ] [ 1, 2, 3, 4 ]").Should().Be("GT");
    }

    [Fact]
    public void Compare_lists_with_negative_elements()
    {
        Evaluate("compare [ -1, -2 ] [ -1, -1 ]").Should().Be("LT");
        Evaluate("compare [ -1, -1 ] [ -1, -2 ]").Should().Be("GT");
    }

    // ============================================================
    // compare on lists of strings (recursive compare via String pattern)
    // ============================================================

    [Fact]
    public void Compare_lists_of_strings()
    {
        Evaluate("compare [ \"a\", \"b\" ] [ \"a\", \"c\" ]").Should().Be("LT");
        Evaluate("compare [ \"a\", \"c\" ] [ \"a\", \"b\" ]").Should().Be("GT");
        Evaluate("compare [ \"a\", \"b\" ] [ \"a\", \"b\" ]").Should().Be("EQ");
    }

    [Fact]
    public void Compare_nested_lists_of_integers()
    {
        Evaluate("compare [ [ 1, 2 ], [ 3 ] ] [ [ 1, 2 ], [ 4 ] ]").Should().Be("LT");
        Evaluate("compare [ [ 1, 2 ], [ 4 ] ] [ [ 1, 2 ], [ 3 ] ]").Should().Be("GT");
        Evaluate("compare [ [ 1, 2 ], [ 3 ] ] [ [ 1, 2 ], [ 3 ] ]").Should().Be("EQ");
    }

    // ============================================================
    // Direct calls to compareList / compareStrings helpers
    // ============================================================

    [Fact]
    public void CompareList_directly_on_integer_lists()
    {
        Evaluate("compareList [ 1, 2 ] [ 1, 2 ]").Should().Be("EQ");
        Evaluate("compareList [ 1, 2 ] [ 1, 3 ]").Should().Be("LT");
        Evaluate("compareList [ 1, 3 ] [ 1, 2 ]").Should().Be("GT");
        Evaluate("compareList [] [ 1 ]").Should().Be("LT");
        Evaluate("compareList [ 1 ] []").Should().Be("GT");
        Evaluate("compareList [] []").Should().Be("EQ");
    }

    // ============================================================
    // compare on chars
    // ============================================================

    [Fact]
    public void Compare_two_equal_chars_returns_EQ()
    {
        // Equal scrutinees short-circuit through the leading
        // `Pine_kernel.equal [ a, b ]` check, so chars don't
        // depend on the wildcard-arm path.
        Evaluate("compare 'a' 'a'").Should().Be("EQ");
    }

    // ============================================================
    // Use compare from related comparison operators
    // ============================================================

    [Fact]
    public void Lt_operator_reuses_compare_on_integers_and_strings()
    {
        Evaluate("lt 1 2").Should().Be("True");
        Evaluate("lt 2 1").Should().Be("False");
        Evaluate("lt 1 1").Should().Be("False");
        Evaluate("lt \"a\" \"b\"").Should().Be("True");
        Evaluate("lt \"b\" \"a\"").Should().Be("False");
    }

    [Fact]
    public void Gt_operator_reuses_compare_on_integers_and_strings()
    {
        Evaluate("gt 2 1").Should().Be("True");
        Evaluate("gt 1 2").Should().Be("False");
        Evaluate("gt 1 1").Should().Be("False");
        Evaluate("gt \"b\" \"a\"").Should().Be("True");
        Evaluate("gt \"a\" \"b\"").Should().Be("False");
    }

    [Fact]
    public void Le_operator_reuses_compare()
    {
        Evaluate("le 1 2").Should().Be("True");
        Evaluate("le 2 2").Should().Be("True");
        Evaluate("le 3 2").Should().Be("False");
    }

    [Fact]
    public void Ge_operator_reuses_compare()
    {
        Evaluate("ge 3 2").Should().Be("True");
        Evaluate("ge 2 2").Should().Be("True");
        Evaluate("ge 1 2").Should().Be("False");
    }

    [Fact]
    public void Min_max_reuse_compare_on_integers_and_strings()
    {
        Evaluate("min 3 7").Should().Be("3");
        Evaluate("min 7 3").Should().Be("3");
        Evaluate("max 3 7").Should().Be("7");
        Evaluate("max 7 3").Should().Be("7");
        Evaluate("min \"abc\" \"abd\"").Should().Be("\"abc\"");
        Evaluate("max \"abc\" \"abd\"").Should().Be("\"abd\"");
    }

    // ============================================================
    // eq on primitives and structural values
    // ============================================================

    [Fact]
    public void Eq_on_equal_integers_returns_True()
    {
        Evaluate("eq 0 0").Should().Be("True");
        Evaluate("eq 42 42").Should().Be("True");
        Evaluate("eq (-7) (-7)").Should().Be("True");
    }

    [Fact]
    public void Eq_on_unequal_integers_returns_False()
    {
        Evaluate("eq 0 1").Should().Be("False");
        Evaluate("eq 42 (-42)").Should().Be("False");
    }

    [Fact]
    public void Eq_on_strings()
    {
        Evaluate("eq \"\" \"\"").Should().Be("True");
        Evaluate("eq \"hello\" \"hello\"").Should().Be("True");
        Evaluate("eq \"hello\" \"Hello\"").Should().Be("False");
        Evaluate("eq \"abc\" \"abcd\"").Should().Be("False");
        Evaluate("eq \"abcd\" \"abc\"").Should().Be("False");
    }

    [Fact]
    public void Eq_on_chars()
    {
        Evaluate("eq 'a' 'a'").Should().Be("True");
        Evaluate("eq 'a' 'b'").Should().Be("False");
    }

    [Fact]
    public void Eq_on_lists()
    {
        Evaluate("eq [] []").Should().Be("True");
        Evaluate("eq [ 1, 2, 3 ] [ 1, 2, 3 ]").Should().Be("True");
        Evaluate("eq [ 1, 2, 3 ] [ 1, 2, 4 ]").Should().Be("False");
        Evaluate("eq [ 1, 2 ] [ 1, 2, 3 ]").Should().Be("False");
        Evaluate("eq [ 1, 2, 3 ] [ 1, 2 ]").Should().Be("False");
        Evaluate("eq [ \"a\", \"b\" ] [ \"a\", \"b\" ]").Should().Be("True");
        Evaluate("eq [ [ 1, 2 ], [ 3 ] ] [ [ 1, 2 ], [ 3 ] ]").Should().Be("True");
        Evaluate("eq [ [ 1, 2 ], [ 3 ] ] [ [ 1, 2 ], [ 4 ] ]").Should().Be("False");
    }

    [Fact]
    public void Eq_on_tuples()
    {
        Evaluate("eq ( 1, 2 ) ( 1, 2 )").Should().Be("True");
        Evaluate("eq ( 1, 2 ) ( 1, 3 )").Should().Be("False");
        Evaluate("eq ( \"a\", [ 1 ] ) ( \"a\", [ 1 ] )").Should().Be("True");
        Evaluate("eq ( \"a\", [ 1 ] ) ( \"a\", [ 2 ] )").Should().Be("False");
    }

    [Fact]
    public void Eq_on_records()
    {
        Evaluate("eq { x = 1, y = 2 } { x = 1, y = 2 }").Should().Be("True");
        Evaluate("eq { x = 1, y = 2 } { x = 1, y = 3 }").Should().Be("False");
    }

    [Fact]
    public void Eq_on_tag_values_with_arguments()
    {
        // Tag values without `String`/`RBNode_elm_builtin`/`Set_elm_builtin` reach
        // the wildcard arm of `eq`'s case expression and are then handed to
        // `listsEqualRecursive` over the tag payload. Use the `LT`/`EQ`/`GT`
        // constructors of the `Order` type defined in Basics.elm — that keeps the
        // test self-contained in the modules currently loaded by this test class
        // (Basics + Dict).
        Evaluate("eq LT LT").Should().Be("True");
        Evaluate("eq EQ EQ").Should().Be("True");
        Evaluate("eq GT GT").Should().Be("True");
        Evaluate("eq LT EQ").Should().Be("False");
        Evaluate("eq EQ GT").Should().Be("False");
    }

    // ============================================================
    // eq on Dict values — independent of internal balancing
    //
    // Dict.elm is a red-black tree, so different insertion orders can produce
    // structurally different trees that nevertheless represent the same key/value
    // mapping. The `eq` implementation in Basics.elm handles this by routing
    // RBNode_elm_builtin values through `dictToList` (in-order traversal) before
    // comparing — these tests pin that behaviour.
    // ============================================================

    [Fact]
    public void Eq_on_two_empty_dicts_returns_True()
    {
        Evaluate("eq empty empty").Should().Be("True");
    }

    [Fact]
    public void Eq_on_singleton_dicts_with_same_key_value_returns_True()
    {
        Evaluate("eq (singleton \"a\" 1) (singleton \"a\" 1)").Should().Be("True");
    }

    [Fact]
    public void Eq_on_singleton_dicts_with_different_value_returns_False()
    {
        Evaluate("eq (singleton \"a\" 1) (singleton \"a\" 2)").Should().Be("False");
    }

    [Fact]
    public void Eq_on_singleton_dicts_with_different_key_returns_False()
    {
        Evaluate("eq (singleton \"a\" 1) (singleton \"b\" 1)").Should().Be("False");
    }

    [Fact]
    public void Eq_on_dicts_built_from_same_pairs_in_different_insertion_orders_returns_True()
    {
        // Inserting the same key/value pairs in two completely different orders
        // produces red-black trees with different internal balancing — the equality
        // check must treat them as equal anyway.
        Evaluate(
            """
            let
                dictAscending =
                    insert "c" 3 (insert "b" 2 (insert "a" 1 empty))

                dictDescending =
                    insert "a" 1 (insert "b" 2 (insert "c" 3 empty))
            in
            eq dictAscending dictDescending
            """)
            .Should().Be("True");
    }

    [Fact]
    public void Eq_on_dicts_built_via_fromList_with_permuted_input_returns_True()
    {
        // `fromList` folds insert over the input list, so the order of the input
        // controls the order of insertion. Permuting the input must not change the
        // observable equality of the resulting dicts.
        Evaluate(
            """
            let
                dictA =
                    fromList [ ( "a", 1 ), ( "b", 2 ), ( "c", 3 ), ( "d", 4 ), ( "e", 5 ) ]

                dictB =
                    fromList [ ( "e", 5 ), ( "c", 3 ), ( "a", 1 ), ( "d", 4 ), ( "b", 2 ) ]
            in
            eq dictA dictB
            """)
            .Should().Be("True");
    }

    [Fact]
    public void Eq_on_dicts_with_enough_keys_to_force_rebalancing_returns_True_regardless_of_order()
    {
        // Insert seven keys in two different orders; with seven nodes the red-black
        // tree's balancing rotations are exercised, so the underlying tree shapes
        // really do differ. The equality must still hold.
        Evaluate(
            """
            let
                ascending =
                    fromList
                        [ ( 1, "a" )
                        , ( 2, "b" )
                        , ( 3, "c" )
                        , ( 4, "d" )
                        , ( 5, "e" )
                        , ( 6, "f" )
                        , ( 7, "g" )
                        ]

                descending =
                    fromList
                        [ ( 7, "g" )
                        , ( 6, "f" )
                        , ( 5, "e" )
                        , ( 4, "d" )
                        , ( 3, "c" )
                        , ( 2, "b" )
                        , ( 1, "a" )
                        ]

                jumbled =
                    fromList
                        [ ( 4, "d" )
                        , ( 1, "a" )
                        , ( 7, "g" )
                        , ( 3, "c" )
                        , ( 6, "f" )
                        , ( 2, "b" )
                        , ( 5, "e" )
                        ]
            in
            ( eq ascending descending, eq ascending jumbled, eq descending jumbled )
            """)
            .Should().Be("[ True, True, True ]");
    }

    [Fact]
    public void Eq_on_dicts_with_one_value_differing_returns_False()
    {
        // Same keys and same insertion order, but the value for one key differs.
        Evaluate(
            """
            let
                dictA =
                    fromList [ ( "a", 1 ), ( "b", 2 ), ( "c", 3 ) ]

                dictB =
                    fromList [ ( "a", 1 ), ( "b", 99 ), ( "c", 3 ) ]
            in
            eq dictA dictB
            """)
            .Should().Be("False");
    }

    [Fact]
    public void Eq_on_dicts_with_one_extra_key_returns_False()
    {
        Evaluate(
            """
            let
                dictA =
                    fromList [ ( "a", 1 ), ( "b", 2 ) ]

                dictB =
                    fromList [ ( "a", 1 ), ( "b", 2 ), ( "c", 3 ) ]
            in
            eq dictA dictB
            """)
            .Should().Be("False");
    }

    [Fact]
    public void Eq_on_dicts_with_swapped_key_returns_False()
    {
        // Same set of keys, same set of values, but a value moved from one key to
        // another — the in-order traversals of the two trees diverge at the affected
        // pair, so equality must be False.
        Evaluate(
            """
            let
                dictA =
                    fromList [ ( "a", 1 ), ( "b", 2 ), ( "c", 3 ) ]

                dictB =
                    fromList [ ( "a", 2 ), ( "b", 1 ), ( "c", 3 ) ]
            in
            eq dictA dictB
            """)
            .Should().Be("False");
    }

    // ============================================================
    // Infix-operator forms of every operator declared in Basics.elm
    //
    // These tests use the operator symbols directly in expressions (not the
    // underlying `add` / `sub` / `mul` / `eq` / ... function names). They exercise
    // the OperatorApplication AST path going through the `infix … = funcName`
    // declarations in Basics.elm, end-to-end against real source code.
    // ============================================================

    [Fact]
    public void Plus_operator_adds_integers()
    {
        Evaluate("1 + 2").Should().Be("3");
        Evaluate("-3 + 5").Should().Be("2");
        Evaluate("1 + 2 + 3 + 4").Should().Be("10");
    }

    [Fact]
    public void Minus_operator_subtracts_integers()
    {
        Evaluate("5 - 3").Should().Be("2");
        Evaluate("3 - 5").Should().Be("-2");
        // Left-associative: (10 - 3) - 2 = 5
        Evaluate("10 - 3 - 2").Should().Be("5");
    }

    [Fact]
    public void Star_operator_multiplies_integers()
    {
        Evaluate("3 * 4").Should().Be("12");
        Evaluate("0 * 99").Should().Be("0");
        Evaluate("-3 * 4").Should().Be("-12");
    }

    [Fact]
    public void Slash_slash_operator_does_integer_division()
    {
        Evaluate("10 // 3").Should().Be("3");
        Evaluate("9 // 3").Should().Be("3");
        Evaluate("0 // 5").Should().Be("0");
        // Division by zero in Elm produces 0, per `idiv` in Basics.elm.
        Evaluate("5 // 0").Should().Be("0");
    }

    [Fact]
    public void Caret_operator_raises_to_a_power()
    {
        Evaluate("2 ^ 0").Should().Be("1");
        Evaluate("2 ^ 1").Should().Be("2");
        Evaluate("2 ^ 8").Should().Be("256");
        Evaluate("3 ^ 4").Should().Be("81");
    }

    [Fact]
    public void Operator_precedence_multiplication_over_addition()
    {
        // `*` has precedence 7, `+` and `-` have precedence 6 — so `*` binds tighter.
        Evaluate("2 + 3 * 4").Should().Be("14");
        Evaluate("2 * 3 + 4").Should().Be("10");
        Evaluate("10 - 2 * 3").Should().Be("4");
    }

    [Fact]
    public void Operator_precedence_caret_over_multiplication()
    {
        // `^` has precedence 8, `*` has precedence 7 — so `^` binds tighter.
        Evaluate("2 * 3 ^ 2").Should().Be("18");
        Evaluate("2 ^ 3 * 4").Should().Be("32");
    }

    [Fact]
    public void Eq_operator_compares_values()
    {
        Evaluate("1 == 1").Should().Be("True");
        Evaluate("1 == 2").Should().Be("False");
        Evaluate("\"a\" == \"a\"").Should().Be("True");
        Evaluate("\"a\" == \"b\"").Should().Be("False");
        Evaluate("[ 1, 2 ] == [ 1, 2 ]").Should().Be("True");
        Evaluate("[ 1, 2 ] == [ 1, 3 ]").Should().Be("False");
    }

    [Fact]
    public void Neq_operator_compares_values()
    {
        Evaluate("1 /= 1").Should().Be("False");
        Evaluate("1 /= 2").Should().Be("True");
        Evaluate("\"a\" /= \"b\"").Should().Be("True");
    }

    [Fact]
    public void Lt_operator_compares_values()
    {
        Evaluate("1 < 2").Should().Be("True");
        Evaluate("2 < 1").Should().Be("False");
        Evaluate("1 < 1").Should().Be("False");
        Evaluate("\"a\" < \"b\"").Should().Be("True");
    }

    [Fact]
    public void Gt_operator_compares_values()
    {
        Evaluate("2 > 1").Should().Be("True");
        Evaluate("1 > 2").Should().Be("False");
        Evaluate("1 > 1").Should().Be("False");
        Evaluate("\"b\" > \"a\"").Should().Be("True");
    }

    [Fact]
    public void Le_operator_compares_values()
    {
        Evaluate("1 <= 2").Should().Be("True");
        Evaluate("2 <= 2").Should().Be("True");
        Evaluate("3 <= 2").Should().Be("False");
    }

    [Fact]
    public void Ge_operator_compares_values()
    {
        Evaluate("3 >= 2").Should().Be("True");
        Evaluate("2 >= 2").Should().Be("True");
        Evaluate("1 >= 2").Should().Be("False");
    }

    [Fact]
    public void And_operator_short_circuits_on_False()
    {
        Evaluate("True && True").Should().Be("True");
        Evaluate("True && False").Should().Be("False");
        Evaluate("False && True").Should().Be("False");
        Evaluate("False && False").Should().Be("False");
        // Mix with a comparison expression
        Evaluate("(1 < 2) && (3 == 3)").Should().Be("True");
        Evaluate("(1 < 2) && (3 == 4)").Should().Be("False");
    }

    [Fact]
    public void Or_operator_short_circuits_on_True()
    {
        Evaluate("True || True").Should().Be("True");
        Evaluate("True || False").Should().Be("True");
        Evaluate("False || True").Should().Be("True");
        Evaluate("False || False").Should().Be("False");
        Evaluate("(1 < 2) || (3 == 4)").Should().Be("True");
        Evaluate("(1 > 2) || (3 == 4)").Should().Be("False");
    }

    [Fact]
    public void Plusplus_operator_appends_strings()
    {
        Evaluate("\"abc\" ++ \"def\"").Should().Be("\"abcdef\"");
        Evaluate("\"hello \" ++ \"world\"").Should().Be("\"hello world\"");
        Evaluate("\"a\" ++ \"b\"").Should().Be("\"ab\"");
        Evaluate("\"\" ++ \"\"").Should().Be("\"\"");
        Evaluate("\"hello\" ++ \"\"").Should().Be("\"hello\"");
        Evaluate("\"\" ++ \"world\"").Should().Be("\"world\"");
    }

    [Fact]
    public void Plusplus_operator_appends_lists()
    {
        Evaluate("[ 1, 2 ] ++ [ 3, 4 ]").Should().Be("[ 1, 2, 3, 4 ]");
        // `++` is right-associative — the result must be the flat concatenation.
        Evaluate("[ 1 ] ++ [ 2 ] ++ [ 3 ]").Should().Be("[ 1, 2, 3 ]");
        Evaluate("[ \"a\" ] ++ [ \"b\", \"c\" ]").Should().Be("[ \"a\", \"b\", \"c\" ]");
    }

    [Fact]
    public void Backwards_pipe_operator_applies_function_to_argument()
    {
        // `f <| x` desugars to `f x`. Right-associative at precedence 0.
        Evaluate("add 1 <| 2").Should().Be("3");
        Evaluate("mul 2 <| 3 + 4").Should().Be("14");
    }

    [Fact]
    public void Backwards_pipe_operator_chains_right_associatively()
    {
        // `f <| g <| x` parses as `f <| (g <| x)` = `f (g x)`.
        Evaluate("add 1 <| add 2 <| 3").Should().Be("6");
    }

    [Fact]
    public void Forwards_pipe_operator_applies_argument_to_function()
    {
        // `x |> f` desugars to `f x`. Left-associative at precedence 0.
        Evaluate("2 |> add 1").Should().Be("3");
        Evaluate("10 |> sub 3").Should().Be("-7");
    }

    [Fact]
    public void Forwards_pipe_operator_chains_left_associatively()
    {
        // `x |> f |> g` parses as `(x |> f) |> g` = `g (f x)`.
        Evaluate("1 |> add 2 |> mul 3").Should().Be("9");
    }

    [Fact]
    public void Compose_left_operator_composes_functions()
    {
        // `(f << g) x = f (g x)`. We pick functions whose composition is observable:
        //   add 1     : Int -> Int
        //   mul 2     : Int -> Int
        // (add 1 << mul 2) 3 = add 1 (mul 2 3) = add 1 6 = 7
        Evaluate("(add 1 << mul 2) 3").Should().Be("7");
    }

    [Fact]
    public void Compose_right_operator_composes_functions()
    {
        // `(f >> g) x = g (f x)`.
        // (add 1 >> mul 2) 3 = mul 2 (add 1 3) = mul 2 4 = 8
        Evaluate("(add 1 >> mul 2) 3").Should().Be("8");
    }

    [Fact]
    public void Operators_combined_in_a_realistic_expression()
    {
        // A combination of arithmetic, comparison, boolean, and pipeline operators
        // — exercises precedence and associativity of several operators at once.
        Evaluate("(1 + 2 * 3 == 7) && (10 - 4 // 2 >= 8)").Should().Be("True");
        Evaluate("(1 + 2 * 3 == 7) && (10 - 4 // 2 >= 9)").Should().Be("False");

        // Mix `|>`, `++`, and arithmetic in a small pipeline.
        Evaluate("[ 1, 2, 3 ] ++ [ 4 ] |> compareList [ 1, 2, 3, 4 ]").Should().Be("EQ");

        // Mix `<|` with arithmetic precedence.
        Evaluate("add 1 <| 2 * 3").Should().Be("7");
    }
}
