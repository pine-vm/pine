using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;

using AbstractExpr = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract.Expression;
using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;
using ElmValue = Pine.Core.Elm.ElmValue;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises <c>Dict</c> functions from <c>elm-kernel-modules/Dict.elm</c>, analogous to
/// <see cref="BasicsBuiltinTests"/>.
/// <para>
/// Two complementary styles of test live here:
/// </para>
/// <list type="bullet">
/// <item>
/// <description>
/// <em>Integration</em> tests canonicalize the kernel modules into a single
/// <see cref="ElmInterpreter.Prepared"/> program and evaluate each scenario to a concrete
/// <see cref="PineValue"/> via
/// <see cref="InterpreterTestHelper.EvaluateInModulesToPineValue(string, ElmInterpreter.Prepared)"/>,
/// then compare against the <see cref="PineValue"/> of an expected Elm expression. <c>Dict</c> is not
/// implicitly imported, so it is addressed by its fully-qualified canonical name.
/// </description>
/// </item>
/// <item>
/// <description>
/// <em>Direct</em> unit tests invoke the builtin resolvers (for example
/// <see cref="ElmInterpreter.ResolveDictGet"/>) on the interpreter's value model
/// (<see cref="PineValueInProcess"/>) without going through the kernel modules. These focus on the
/// requirement that the <c>Dict</c> builtins must support dictionaries whose <em>values</em> are
/// function closures (<see cref="ElmInterpreter.ElmClosureInProcess"/>). Such values have no concrete
/// <see cref="PineValue"/> encoding, so the implementations must never call
/// <see cref="PineValueInProcess.Evaluate"/> on them. Tests that store a closure assert that the very
/// same closure instance flows through (reference identity), proving it passed through unevaluated.
/// </description>
/// </item>
/// </list>
/// </summary>
public class DictBuiltinTests
{
    // ============================================================
    // integration test harness (through the kernel modules)
    // ============================================================

    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(() => InterpreterTestHelper.PrepareKernelModules(
            "Basics.elm",
            "List.elm",
            "Maybe.elm",
            "Char.elm",
            "Dict.elm"));

    private static PineValue Evaluate(string expression) =>
        InterpreterTestHelper.EvaluateInModulesToPineValue(expression, s_prepared.Value);

    private static void AssertEvaluatesEqual(string expression, string expectedExpression) =>
        Evaluate(expression).Should().Be(Evaluate(expectedExpression));

    private const string Dict =
        "Dict.fromList [ ( \"a\", 1 ), ( \"b\", 2 ), ( \"c\", 3 ) ]";

    // ============================================================
    // direct value-model construction helpers
    // ============================================================

    private static PineValueInProcess Str(string value) =>
        ElmInterpreter.ToProcess(ElmValue.StringInstance(value));

    private static PineValueInProcess Int(int value) =>
        PineValueInProcess.CreateInteger(value);

    private static PineValueInProcess TagNameValue(string tagName) =>
        PineValueInProcess.Create(StringEncoding.ValueFromString(tagName));

    // The NColor constructors, encoded as no-argument tagged values, as the interpreter produces them.
    private static readonly PineValueInProcess s_colorRed =
        PineValueInProcess.CreateTagged(TagNameValue("Red"), []);

    private static readonly PineValueInProcess s_colorBlack =
        PineValueInProcess.CreateTagged(TagNameValue("Black"), []);

    private static readonly PineValueInProcess s_emptyDict =
        PineValueInProcess.CreateTagged(TagNameValue("RBEmpty_elm_builtin"), []);

    /// <summary>
    /// Builds an <c>RBNode_elm_builtin</c> node in the same canonical
    /// <c>[ tag, [ color, key, value, left, right ] ]</c> shape the interpreter produces.
    /// </summary>
    private static PineValueInProcess Node(
        PineValueInProcess color,
        PineValueInProcess key,
        PineValueInProcess value,
        PineValueInProcess left,
        PineValueInProcess right) =>
        PineValueInProcess.CreateTagged(
            TagNameValue("RBNode_elm_builtin"),
            [color, key, value, left, right]);

    private static ElmInterpreter.ElmClosureInProcess Closure() =>
        new(
            new ElmInterpreter.ElmClosureInProcess.SourceRef.Lambda(
                new AbstractExpr.LambdaExpression([], AbstractExpr.UnitExpr.Instance)),
            parameterCount: 1,
            argumentsAlreadyCollected: [],
            capturedBindings: ImmutableDictionary<string, PineValueInProcess>.Empty,
            capturedTopLevel: DeclQualifiedName.Create([], "Top"));

    private static PineValueInProcess Get(PineValueInProcess key, PineValueInProcess dict) =>
        ElmInterpreter.ResolveDictGet([key, dict])!;

    // ---- result inspection helpers ----

    private static readonly PineValue s_justTagNameValue =
        StringEncoding.ValueFromString("Just");

    private static readonly PineValue s_nothingTagNameValue =
        StringEncoding.ValueFromString("Nothing");

    private static bool IsNothing(PineValueInProcess result) =>
        result.GetElementAt(0).Evaluate() == s_nothingTagNameValue;

    private static bool IsJust(PineValueInProcess result) =>
        result.GetElementAt(0).Evaluate() == s_justTagNameValue;

    /// <summary>The single payload of a <c>Just</c> result, as the in-process value as stored.</summary>
    private static PineValueInProcess JustValue(PineValueInProcess result) =>
        result.GetElementAt(1).GetElementAt(0);

    /// <summary>
    /// The structural items of an in-process list value, without forcing evaluation of the items.
    /// The empty list has no unevaluated structure (it is the cached
    /// <see cref="PineValue.EmptyList"/>), so it is recognized explicitly and yields no items.
    /// </summary>
    private static IReadOnlyList<PineValueInProcess> ListItems(PineValueInProcess value)
    {
        if (value.UnevaluatedStructuralItemsOrNull() is { } items)
        {
            return items;
        }

        if (value.Evaluate() == PineValue.EmptyList)
        {
            return [];
        }

        throw new InvalidOperationException("Expected an unevaluated list value.");
    }

    private static bool EvaluatingThrows(PineValueInProcess value)
    {
        try
        {
            value.Evaluate();
            return false;
        }
        catch (Exception)
        {
            return true;
        }
    }

    // ============================================================
    // get — present keys
    // ============================================================

    [Theory]
    [InlineData("\"a\"", "Just 1")]
    [InlineData("\"b\"", "Just 2")]
    [InlineData("\"c\"", "Just 3")]
    public void Get_present_key_returns_Just_value(string key, string expected) =>
        AssertEvaluatesEqual("Dict.get " + key + " (" + Dict + ")", expected);

    // ============================================================
    // get — absent keys and empty dicts
    // ============================================================

    [Theory]
    [InlineData("\"z\"")]
    [InlineData("\"A\"")]
    public void Get_absent_key_returns_Nothing(string key) =>
        AssertEvaluatesEqual("Dict.get " + key + " (" + Dict + ")", "Nothing");

    [Fact]
    public void Get_from_empty_dict_returns_Nothing() =>
        AssertEvaluatesEqual("Dict.get \"a\" Dict.empty", "Nothing");

    [Fact]
    public void Get_with_integer_keys_returns_Just_value() =>
        AssertEvaluatesEqual(
            "Dict.get 2 (Dict.fromList [ ( 1, \"a\" ), ( 2, \"b\" ), ( 3, \"c\" ) ])",
            "Just \"b\"");

    [Fact]
    public void Get_returns_the_last_value_for_a_duplicated_key() =>
        // fromList folds insert, so a later pair for the same key overwrites the earlier one.
        AssertEvaluatesEqual(
            "Dict.get \"a\" (Dict.fromList [ ( \"a\", 1 ), ( \"a\", 2 ) ])",
            "Just 2");

    [Fact]
    public void Get_is_independent_of_insertion_order()
    {
        const string Ascending =
            "Dict.fromList [ ( 1, \"a\" ), ( 2, \"b\" ), ( 3, \"c\" ), ( 4, \"d\" ), ( 5, \"e\" ) ]";

        const string Descending =
            "Dict.fromList [ ( 5, \"e\" ), ( 4, \"d\" ), ( 3, \"c\" ), ( 2, \"b\" ), ( 1, \"a\" ) ]";

        AssertEvaluatesEqual(
            "Dict.get 3 (" + Ascending + ")",
            "Dict.get 3 (" + Descending + ")");

        AssertEvaluatesEqual("Dict.get 3 (" + Descending + ")", "Just \"c\"");
    }

    // ============================================================
    // get — values that are closures pass through unevaluated
    // ============================================================

    [Fact]
    public void Get_present_key_returns_Just_with_the_exact_closure_instance()
    {
        var closureA = Closure();
        var closureB = Closure();
        var closureC = Closure();

        // BST keyed on strings: "b" at the root, "a" left, "c" right.
        var dict =
            Node(
                s_colorBlack,
                Str("b"),
                closureB,
                Node(s_colorRed, Str("a"), closureA, s_emptyDict, s_emptyDict),
                Node(s_colorRed, Str("c"), closureC, s_emptyDict, s_emptyDict));

        var resultRoot = Get(Str("b"), dict);
        IsJust(resultRoot).Should().BeTrue();
        JustValue(resultRoot).Should().BeSameAs(closureB);

        var resultLeft = Get(Str("a"), dict);
        IsJust(resultLeft).Should().BeTrue();
        JustValue(resultLeft).Should().BeSameAs(closureA);

        var resultRight = Get(Str("c"), dict);
        IsJust(resultRight).Should().BeTrue();
        JustValue(resultRight).Should().BeSameAs(closureC);
    }

    [Fact]
    public void Get_absent_key_returns_Nothing_even_when_dict_holds_closures()
    {
        var dict =
            Node(
                s_colorBlack,
                Str("b"),
                Closure(),
                Node(s_colorRed, Str("a"), Closure(), s_emptyDict, s_emptyDict),
                Node(s_colorRed, Str("c"), Closure(), s_emptyDict, s_emptyDict));

        IsNothing(Get(Str("z"), dict)).Should().BeTrue();
        IsNothing(Get(Str("a0"), dict)).Should().BeTrue();
    }

    [Fact]
    public void Get_from_empty_dict_in_process_returns_Nothing()
    {
        IsNothing(Get(Str("a"), s_emptyDict)).Should().BeTrue();
    }

    [Fact]
    public void Get_with_integer_keys_returns_the_stored_closure()
    {
        var closure2 = Closure();

        var dict =
            Node(
                s_colorBlack,
                Int(2),
                closure2,
                Node(s_colorRed, Int(1), Closure(), s_emptyDict, s_emptyDict),
                Node(s_colorRed, Int(3), Closure(), s_emptyDict, s_emptyDict));

        var result = Get(Int(2), dict);

        IsJust(result).Should().BeTrue();
        JustValue(result).Should().BeSameAs(closure2);
    }

    [Fact]
    public void Get_descends_past_closure_valued_nodes_to_reach_a_concrete_value()
    {
        var dict =
            Node(
                s_colorBlack,
                Str("b"),
                Closure(),
                Node(s_colorRed, Str("a"), Closure(), s_emptyDict, s_emptyDict),
                Node(s_colorRed, Str("c"), Str("target"), s_emptyDict, s_emptyDict));

        var result = Get(Str("c"), dict);

        IsJust(result).Should().BeTrue();
        ElmInterpreter.RenderAsElmExpression(JustValue(result)).expressionString
            .Should().Be("\"target\"");
    }

    [Fact]
    public void Get_works_even_though_evaluating_the_whole_dict_throws()
    {
        var closure = Closure();

        var dict = Node(s_colorRed, Str("a"), closure, s_emptyDict, s_emptyDict);

        EvaluatingThrows(dict).Should().BeTrue(
            "a dict whose value is a closure has no concrete PineValue encoding");

        var result = Get(Str("a"), dict);

        IsJust(result).Should().BeTrue();
        JustValue(result).Should().BeSameAs(closure);
    }

    [Fact]
    public void Get_returns_Just_for_concrete_values()
    {
        var dict =
            Node(
                s_colorBlack,
                Str("b"),
                Int(2),
                Node(s_colorRed, Str("a"), Int(1), s_emptyDict, s_emptyDict),
                Node(s_colorRed, Str("c"), Int(3), s_emptyDict, s_emptyDict));

        var result = Get(Str("a"), dict);

        IsJust(result).Should().BeTrue();
        ElmInterpreter.RenderAsElmExpression(JustValue(result)).expressionString
            .Should().Be("1");
    }

    // ============================================================
    // values
    // ============================================================

    [Fact]
    public void Values_returns_values_in_key_order() =>
        AssertEvaluatesEqual("Dict.values (" + Dict + ")", "[ 1, 2, 3 ]");

    [Fact]
    public void Values_from_empty_dict_is_empty_list() =>
        AssertEvaluatesEqual("Dict.values Dict.empty", "[]");

    [Fact]
    public void Values_with_integer_keys_is_sorted_by_key() =>
        AssertEvaluatesEqual(
            "Dict.values (Dict.fromList [ ( 3, \"c\" ), ( 1, \"a\" ), ( 2, \"b\" ) ])",
            "[ \"a\", \"b\", \"c\" ]");

    [Fact]
    public void Values_returns_the_exact_closure_instances_in_key_order()
    {
        var closureA = Closure();
        var closureB = Closure();
        var closureC = Closure();

        var dict =
            Node(
                s_colorBlack,
                Str("b"),
                closureB,
                Node(s_colorRed, Str("a"), closureA, s_emptyDict, s_emptyDict),
                Node(s_colorRed, Str("c"), closureC, s_emptyDict, s_emptyDict));

        var items = ListItems(ElmInterpreter.ResolveDictValues([dict])!);

        items.Should().HaveCount(3);
        items[0].Should().BeSameAs(closureA);
        items[1].Should().BeSameAs(closureB);
        items[2].Should().BeSameAs(closureC);
    }

    [Fact]
    public void Values_from_empty_dict_in_process_is_empty_list()
    {
        ListItems(ElmInterpreter.ResolveDictValues([s_emptyDict])!).Should().BeEmpty();
    }

    [Fact]
    public void Values_works_even_though_evaluating_the_whole_dict_throws()
    {
        var closure = Closure();

        var dict = Node(s_colorRed, Str("a"), closure, s_emptyDict, s_emptyDict);

        EvaluatingThrows(dict).Should().BeTrue();

        var items = ListItems(ElmInterpreter.ResolveDictValues([dict])!);

        items.Should().ContainSingle();
        items[0].Should().BeSameAs(closure);
    }

    // ============================================================
    // toList
    // ============================================================

    [Fact]
    public void ToList_returns_pairs_in_key_order() =>
        AssertEvaluatesEqual(
            "Dict.toList (" + Dict + ")",
            "[ ( \"a\", 1 ), ( \"b\", 2 ), ( \"c\", 3 ) ]");

    [Fact]
    public void ToList_from_empty_dict_is_empty_list() =>
        AssertEvaluatesEqual("Dict.toList Dict.empty", "[]");

    [Fact]
    public void ToList_with_integer_keys_is_sorted_by_key() =>
        AssertEvaluatesEqual(
            "Dict.toList (Dict.fromList [ ( 3, \"c\" ), ( 1, \"a\" ), ( 2, \"b\" ) ])",
            "[ ( 1, \"a\" ), ( 2, \"b\" ), ( 3, \"c\" ) ]");

    [Fact]
    public void ToList_returns_keys_with_the_exact_closure_values_in_key_order()
    {
        var closureA = Closure();
        var closureB = Closure();

        var dict =
            Node(
                s_colorBlack,
                Str("b"),
                closureB,
                Node(s_colorRed, Str("a"), closureA, s_emptyDict, s_emptyDict),
                s_emptyDict);

        var pairs = ListItems(ElmInterpreter.ResolveDictToList([dict])!);

        pairs.Should().HaveCount(2);

        var firstPair = ListItems(pairs[0]);
        firstPair.Should().HaveCount(2);
        ElmInterpreter.RenderAsElmExpression(firstPair[0]).expressionString.Should().Be("\"a\"");
        firstPair[1].Should().BeSameAs(closureA);

        var secondPair = ListItems(pairs[1]);
        secondPair.Should().HaveCount(2);
        ElmInterpreter.RenderAsElmExpression(secondPair[0]).expressionString.Should().Be("\"b\"");
        secondPair[1].Should().BeSameAs(closureB);
    }

    [Fact]
    public void ToList_from_empty_dict_in_process_is_empty_list()
    {
        ListItems(ElmInterpreter.ResolveDictToList([s_emptyDict])!).Should().BeEmpty();
    }

    [Fact]
    public void ToList_works_even_though_evaluating_the_whole_dict_throws()
    {
        var closure = Closure();

        var dict = Node(s_colorRed, Str("a"), closure, s_emptyDict, s_emptyDict);

        EvaluatingThrows(dict).Should().BeTrue();

        var pairs = ListItems(ElmInterpreter.ResolveDictToList([dict])!);

        pairs.Should().ContainSingle();
        ListItems(pairs[0])[1].Should().BeSameAs(closure);
    }

    // ============================================================
    // insertHelp (and insert, which delegates to it)
    // ============================================================

    [Fact]
    public void Insert_adds_a_new_pair() =>
        AssertEvaluatesEqual(
            "Dict.get \"d\" (Dict.insert \"d\" 4 (" + Dict + "))",
            "Just 4");

    [Fact]
    public void Insert_replaces_the_value_for_an_existing_key() =>
        AssertEvaluatesEqual(
            "Dict.get \"a\" (Dict.insert \"a\" 9 (" + Dict + "))",
            "Just 9");

    [Fact]
    public void Insert_keeps_all_other_pairs_intact() =>
        AssertEvaluatesEqual(
            "Dict.toList (Dict.insert \"b\" 9 (" + Dict + "))",
            "[ ( \"a\", 1 ), ( \"b\", 9 ), ( \"c\", 3 ) ]");

    [Fact]
    public void Insert_grows_the_dictionary_size() =>
        AssertEvaluatesEqual(
            "Dict.size (Dict.insert \"d\" 4 (" + Dict + "))",
            "4");

    [Fact]
    public void InsertHelp_into_empty_dict_stores_the_exact_closure_instance()
    {
        var closure = Closure();

        var dict = ElmInterpreter.ResolveDictInsertHelp([Str("a"), closure, s_emptyDict])!;

        var result = Get(Str("a"), dict);

        IsJust(result).Should().BeTrue();
        JustValue(result).Should().BeSameAs(closure);
    }

    [Fact]
    public void InsertHelp_preserves_existing_closure_values_when_inserting_a_closure()
    {
        var closureA = Closure();
        var closureB = Closure();
        var closureNew = Closure();

        var dict =
            Node(
                s_colorBlack,
                Str("b"),
                closureB,
                Node(s_colorRed, Str("a"), closureA, s_emptyDict, s_emptyDict),
                s_emptyDict);

        var updated = ElmInterpreter.ResolveDictInsertHelp([Str("c"), closureNew, dict])!;

        JustValue(Get(Str("a"), updated)).Should().BeSameAs(closureA);
        JustValue(Get(Str("b"), updated)).Should().BeSameAs(closureB);
        JustValue(Get(Str("c"), updated)).Should().BeSameAs(closureNew);
    }

    [Fact]
    public void InsertHelp_replaces_the_value_for_an_existing_key_with_a_closure()
    {
        var oldClosure = Closure();
        var newClosure = Closure();

        var dict = Node(s_colorBlack, Str("a"), oldClosure, s_emptyDict, s_emptyDict);

        var updated = ElmInterpreter.ResolveDictInsertHelp([Str("a"), newClosure, dict])!;

        JustValue(Get(Str("a"), updated)).Should().BeSameAs(newClosure);
    }

    [Fact]
    public void InsertHelp_balances_a_run_of_closure_valued_insertions()
    {
        // Insert keys 1..7 in ascending order, each with its own closure value. Ascending insertion
        // is the worst case for an unbalanced tree, so this also exercises the balance rotations.
        var closures = new ElmInterpreter.ElmClosureInProcess[8];

        var dict = s_emptyDict;

        for (var key = 1; key <= 7; key++)
        {
            closures[key] = Closure();
            dict = ElmInterpreter.ResolveDictInsertHelp([Int(key), closures[key], dict])!;
        }

        // insertHelp leaves the root red; get and sizeHelp do not depend on the root color.
        ElmInterpreter.ResolveDictSizeHelp([Int(0), dict])!.Evaluate()
            .Should().Be(Int(7).Evaluate());

        for (var key = 1; key <= 7; key++)
        {
            JustValue(Get(Int(key), dict)).Should().BeSameAs(closures[key]);
        }
    }

    // ============================================================
    // sizeHelp (and size, which delegates to it)
    // ============================================================

    [Fact]
    public void Size_counts_the_pairs() =>
        AssertEvaluatesEqual("Dict.size (" + Dict + ")", "3");

    [Fact]
    public void Size_of_empty_dict_is_zero() =>
        AssertEvaluatesEqual("Dict.size Dict.empty", "0");

    [Fact]
    public void SizeHelp_counts_nodes_even_when_values_are_closures()
    {
        var dict =
            Node(
                s_colorBlack,
                Str("b"),
                Closure(),
                Node(s_colorRed, Str("a"), Closure(), s_emptyDict, s_emptyDict),
                Node(s_colorRed, Str("c"), Closure(), s_emptyDict, s_emptyDict));

        ElmInterpreter.ResolveDictSizeHelp([Int(0), dict])!.Evaluate()
            .Should().Be(Int(3).Evaluate());
    }

    [Fact]
    public void SizeHelp_adds_the_accumulator()
    {
        var dict =
            Node(
                s_colorBlack,
                Str("b"),
                Closure(),
                Node(s_colorRed, Str("a"), Closure(), s_emptyDict, s_emptyDict),
                Node(s_colorRed, Str("c"), Closure(), s_emptyDict, s_emptyDict));

        ElmInterpreter.ResolveDictSizeHelp([Int(10), dict])!.Evaluate()
            .Should().Be(Int(13).Evaluate());
    }

    [Fact]
    public void SizeHelp_of_empty_dict_returns_the_accumulator()
    {
        ElmInterpreter.ResolveDictSizeHelp([Int(0), s_emptyDict])!.Evaluate()
            .Should().Be(Int(0).Evaluate());
    }

    [Fact]
    public void SizeHelp_works_even_though_evaluating_the_whole_dict_throws()
    {
        var dict = Node(s_colorRed, Str("a"), Closure(), s_emptyDict, s_emptyDict);

        EvaluatingThrows(dict).Should().BeTrue();

        ElmInterpreter.ResolveDictSizeHelp([Int(0), dict])!.Evaluate()
            .Should().Be(Int(1).Evaluate());
    }
}
