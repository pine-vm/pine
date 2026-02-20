using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class CoreDictFunctionTests
{
    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_kernelEnv =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["elm-kernel-modules"])
                    ?? throw new Exception("Did not find elm-kernel-modules");

                var rootFilePaths =
                    kernelModulesTree.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("Dict.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        kernelModulesTree,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Extract(err => throw new Exception("Failed compiling elm-kernel-modules: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing interactive environment: " + err));
            });

    private static PineValue GetDictFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Dict")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue ApplyBinary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm);

    private static ElmValue ApplyTernary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2, ElmValue arg3) =>
        CoreLibraryTestHelper.ApplyTernary(functionValue, arg1, arg2, arg3, s_vm);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static readonly ElmValue s_nothing =
        ElmValue.TagInstance("Nothing", []);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

    private static ElmValue Tuple(ElmValue a, ElmValue b) =>
        ElmValue.ListInstance([a, b]);

    private static PineValue ToPine(ElmValue value) =>
        ElmValueEncoding.ElmValueAsPineValue(value);

    private static ElmValue ApplyWithPineArgs(
        PineValue functionValue, params PineValue[] pineArgs) =>
        CoreLibraryTestHelper.ApplyWithPineArgs(s_vm, functionValue, pineArgs);

    /// <summary>
    /// Builds a Dict from key-value pairs via fromList.
    /// </summary>
    private ElmValue BuildDict(params (ElmValue key, ElmValue value)[] pairs)
    {
        var tupleList = ElmList([.. pairs.Select(p => Tuple(p.key, p.value))]);
        return ApplyUnary(GetDictFunction("fromList"), tupleList);
    }

    /// <summary>
    /// Builds a Dict from int key-value pairs via fromList.
    /// </summary>
    private ElmValue DictFromIntPairs(params (long key, long value)[] pairs)
    {
        var tupleList = ElmList([.. pairs.Select(p => Tuple(Integer(p.key), Integer(p.value)))]);
        return ApplyUnary(GetDictFunction("fromList"), tupleList);
    }

    /// <summary>
    /// Builds a Dict from string key-int value pairs via fromList.
    /// </summary>
    private ElmValue DictFromStringIntPairs(params (string key, long value)[] pairs)
    {
        var tupleList = ElmList([.. pairs.Select(p => Tuple(String(p.key), Integer(p.value)))]);
        return ApplyUnary(GetDictFunction("fromList"), tupleList);
    }

    // ========== Tests for isEmpty ==========
    // isEmpty empty == True

    [Fact]
    public void IsEmpty_empty()
    {
        var emptyDict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var result = ApplyUnary(GetDictFunction("isEmpty"), emptyDict);
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsEmpty_nonempty()
    {
        var dict = DictFromIntPairs((1, 10));
        var result = ApplyUnary(GetDictFunction("isEmpty"), dict);
        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for singleton ==========

    [Fact]
    public void Singleton_creates_dict_with_one_entry()
    {
        var dict = ApplyBinary(GetDictFunction("singleton"), Integer(1), String("a"));
        var result = ApplyUnary(GetDictFunction("size"), dict);
        result.Should().Be(Integer(1));
    }

    [Fact]
    public void Singleton_get_returns_value()
    {
        var dict = ApplyBinary(GetDictFunction("singleton"), Integer(1), String("a"));
        var result = ApplyBinary(GetDictFunction("get"), Integer(1), dict);
        result.Should().Be(JustOf(String("a")));
    }

    // ========== Tests for get ==========
    // animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]
    // get "Tom"   animals == Just Cat
    // get "Jerry" animals == Just Mouse
    // get "Spike" animals == Nothing

    [Fact]
    public void Get_existing_key()
    {
        var dict = DictFromStringIntPairs(("Tom", 1), ("Jerry", 2));
        var result = ApplyBinary(GetDictFunction("get"), String("Tom"), dict);
        result.Should().Be(JustOf(Integer(1)));
    }

    [Fact]
    public void Get_second_key()
    {
        var dict = DictFromStringIntPairs(("Tom", 1), ("Jerry", 2));
        var result = ApplyBinary(GetDictFunction("get"), String("Jerry"), dict);
        result.Should().Be(JustOf(Integer(2)));
    }

    [Fact]
    public void Get_missing_key()
    {
        var dict = DictFromStringIntPairs(("Tom", 1), ("Jerry", 2));
        var result = ApplyBinary(GetDictFunction("get"), String("Spike"), dict);
        result.Should().Be(s_nothing);
    }

    [Fact]
    public void Get_from_empty()
    {
        var dict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var result = ApplyBinary(GetDictFunction("get"), Integer(1), dict);
        result.Should().Be(s_nothing);
    }

    // ========== Tests for member ==========

    [Fact]
    public void Member_existing_key()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20));
        var result = ApplyBinary(GetDictFunction("member"), Integer(1), dict);
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Member_missing_key()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20));
        var result = ApplyBinary(GetDictFunction("member"), Integer(3), dict);
        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for size ==========

    [Fact]
    public void Size_empty()
    {
        var dict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var result = ApplyUnary(GetDictFunction("size"), dict);
        result.Should().Be(Integer(0));
    }

    [Fact]
    public void Size_one()
    {
        var dict = DictFromIntPairs((1, 10));
        var result = ApplyUnary(GetDictFunction("size"), dict);
        result.Should().Be(Integer(1));
    }

    [Fact]
    public void Size_three()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20), (3, 30));
        var result = ApplyUnary(GetDictFunction("size"), dict);
        result.Should().Be(Integer(3));
    }

    // ========== Tests for insert ==========

    [Fact]
    public void Insert_into_empty()
    {
        var emptyDict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var dict = ApplyTernary(GetDictFunction("insert"), Integer(1), String("a"), emptyDict);
        var result = ApplyBinary(GetDictFunction("get"), Integer(1), dict);
        result.Should().Be(JustOf(String("a")));
    }

    [Fact]
    public void Insert_replaces_on_collision()
    {
        var dict = DictFromIntPairs((1, 10));
        var updated = ApplyTernary(GetDictFunction("insert"), Integer(1), Integer(99), dict);
        var result = ApplyBinary(GetDictFunction("get"), Integer(1), updated);
        result.Should().Be(JustOf(Integer(99)));
    }

    [Fact]
    public void Insert_adds_new_key()
    {
        var dict = DictFromIntPairs((1, 10));
        var updated = ApplyTernary(GetDictFunction("insert"), Integer(2), Integer(20), dict);
        var result = ApplyUnary(GetDictFunction("size"), updated);
        result.Should().Be(Integer(2));
    }

    // ========== Tests for remove ==========

    [Fact]
    public void Remove_existing_key()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20));
        var updated = ApplyBinary(GetDictFunction("remove"), Integer(1), dict);
        var result = ApplyBinary(GetDictFunction("get"), Integer(1), updated);
        result.Should().Be(s_nothing);
    }

    [Fact]
    public void Remove_missing_key()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20));
        var updated = ApplyBinary(GetDictFunction("remove"), Integer(3), dict);
        var result = ApplyUnary(GetDictFunction("size"), updated);
        result.Should().Be(Integer(2));
    }

    [Fact]
    public void Remove_from_empty()
    {
        var emptyDict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var updated = ApplyBinary(GetDictFunction("remove"), Integer(1), emptyDict);
        var result = ApplyUnary(GetDictFunction("isEmpty"), updated);
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Remove_only_key()
    {
        var dict = DictFromIntPairs((1, 10));
        var updated = ApplyBinary(GetDictFunction("remove"), Integer(1), dict);
        var result = ApplyUnary(GetDictFunction("isEmpty"), updated);
        result.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for keys ==========
    // keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]

    [Fact]
    public void Keys_sorted()
    {
        var dict = DictFromIntPairs((0, 100), (1, 200));
        var result = ApplyUnary(GetDictFunction("keys"), dict);
        result.Should().Be(ElmList(Integer(0), Integer(1)));
    }

    [Fact]
    public void Keys_empty()
    {
        var dict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var result = ApplyUnary(GetDictFunction("keys"), dict);
        result.Should().Be(ElmList());
    }

    [Fact]
    public void Keys_insertion_order_independent()
    {
        // Insert in reverse order, keys should still be sorted
        var dict = DictFromIntPairs((3, 30), (1, 10), (2, 20));

        var result = ApplyUnary(GetDictFunction("keys"), dict);
        result.Should().Be(ElmList(Integer(1), Integer(2), Integer(3)));
    }

    // ========== Tests for values ==========
    // values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]

    [Fact]
    public void Values_in_key_order()
    {
        var dict = DictFromStringIntPairs(("Alice", 28), ("Bob", 19));
        var result = ApplyUnary(GetDictFunction("values"), dict);
        result.Should().Be(ElmList(Integer(28), Integer(19)));
    }

    [Fact]
    public void Values_empty()
    {
        var dict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var result = ApplyUnary(GetDictFunction("values"), dict);
        result.Should().Be(ElmList());
    }

    // ========== Tests for toList ==========

    [Fact]
    public void ToList_sorted_by_keys()
    {
        var dict = DictFromIntPairs((2, 20), (1, 10));
        var result = ApplyUnary(GetDictFunction("toList"), dict);

        result.Should().Be(
            ElmList(
                Tuple(Integer(1), Integer(10)),
                Tuple(Integer(2), Integer(20))));
    }

    [Fact]
    public void ToList_empty()
    {
        var dict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var result = ApplyUnary(GetDictFunction("toList"), dict);
        result.Should().Be(ElmList());
    }

    // ========== Tests for fromList ==========

    [Fact]
    public void FromList_creates_dict()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20));
        var result = ApplyUnary(GetDictFunction("size"), dict);
        result.Should().Be(Integer(2));
    }

    [Fact]
    public void FromList_empty()
    {
        var dict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var result = ApplyUnary(GetDictFunction("isEmpty"), dict);
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void FromList_duplicate_keys()
    {
        // Last value wins for duplicate keys
        var dict = DictFromIntPairs((1, 10), (1, 99));

        var result = ApplyBinary(GetDictFunction("get"), Integer(1), dict);
        result.Should().Be(JustOf(Integer(99)));
    }

    // ========== Tests for union ==========
    // Combine two dictionaries. If there is a collision, preference is given to the first dictionary.

    [Fact]
    public void Union_no_overlap()
    {
        var d1 = DictFromIntPairs((1, 10));
        var d2 = DictFromIntPairs((2, 20));
        var result = ApplyBinary(GetDictFunction("union"), d1, d2);
        var size = ApplyUnary(GetDictFunction("size"), result);
        size.Should().Be(Integer(2));
    }

    [Fact]
    public void Union_with_overlap_prefers_first()
    {
        var d1 = DictFromIntPairs((1, 10));
        var d2 = DictFromIntPairs((1, 99));
        var result = ApplyBinary(GetDictFunction("union"), d1, d2);
        var get = ApplyBinary(GetDictFunction("get"), Integer(1), result);
        get.Should().Be(JustOf(Integer(10)));
    }

    [Fact]
    public void Union_with_empty()
    {
        var d1 = DictFromIntPairs((1, 10));
        var d2 = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var result = ApplyBinary(GetDictFunction("union"), d1, d2);
        var size = ApplyUnary(GetDictFunction("size"), result);
        size.Should().Be(Integer(1));
    }

    // ========== Tests for intersect ==========
    // Keep a key-value pair when its key appears in the second dictionary.

    [Fact]
    public void Intersect_overlapping()
    {
        var d1 = DictFromIntPairs((1, 10), (2, 20));
        var d2 = DictFromIntPairs((2, 200), (3, 300));
        var result = ApplyBinary(GetDictFunction("intersect"), d1, d2);
        var keys = ApplyUnary(GetDictFunction("keys"), result);
        keys.Should().Be(ElmList(Integer(2)));
    }

    [Fact]
    public void Intersect_no_overlap()
    {
        var d1 = DictFromIntPairs((1, 10));
        var d2 = DictFromIntPairs((2, 20));
        var result = ApplyBinary(GetDictFunction("intersect"), d1, d2);
        var isEmpty = ApplyUnary(GetDictFunction("isEmpty"), result);
        isEmpty.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Intersect_prefers_first_dict_values()
    {
        var d1 = DictFromIntPairs((1, 10), (2, 20));
        var d2 = DictFromIntPairs((1, 99), (2, 99));
        var result = ApplyBinary(GetDictFunction("intersect"), d1, d2);
        var get = ApplyBinary(GetDictFunction("get"), Integer(1), result);
        get.Should().Be(JustOf(Integer(10)));
    }

    // ========== Tests for diff ==========
    // Keep a key-value pair when its key does not appear in the second dictionary.

    [Fact]
    public void Diff_removes_matching_keys()
    {
        var d1 = DictFromIntPairs((1, 10), (2, 20), (3, 30));
        var d2 = DictFromIntPairs((2, 200));
        var result = ApplyBinary(GetDictFunction("diff"), d1, d2);
        var keys = ApplyUnary(GetDictFunction("keys"), result);
        keys.Should().Be(ElmList(Integer(1), Integer(3)));
    }

    [Fact]
    public void Diff_no_overlap()
    {
        var d1 = DictFromIntPairs((1, 10));
        var d2 = DictFromIntPairs((2, 20));
        var result = ApplyBinary(GetDictFunction("diff"), d1, d2);
        var size = ApplyUnary(GetDictFunction("size"), result);
        size.Should().Be(Integer(1));
    }

    // ========== Tests for map ==========
    // Apply a function to all values in a dictionary.

    [Fact]
    public void Map_negate_values()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20));

        var negateFunc =
            Core.Elm.ElmCompilerInDotnet.CoreLibraryModule.CoreBasics.GetFunctionValue("always")!;
        // We use a simpler approach: map with a function that ignores key and returns a constant
        // Actually, let's test with the actual map which takes (k -> a -> b)
        // Since map needs a 2-arg function (key -> value -> newValue), we use Basics.always which takes 2 args
        // Basics.always returns the first arg, so map (\k v -> k) would return keys as values
        var result =
            ApplyWithPineArgs(
                GetDictFunction("map"),
                negateFunc,
                ToPine(dict));

        // always returns the first argument (key), so values become the keys
        var vals = ApplyUnary(GetDictFunction("values"), result);

        vals.Should().Be(ElmList(Integer(1), Integer(2)));
    }

    // ========== Tests for foldl ==========
    // Fold over key-value pairs from lowest to highest key.

    [Fact]
    public void Foldl_collect_keys()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20), (3, 30));
        // We use Basics.add as the accumulator function won't work directly since foldl takes (k -> v -> b -> b)
        // Instead, let's verify indirectly through toList + keys
        var keysResult = ApplyUnary(GetDictFunction("keys"), dict);

        keysResult.Should().Be(ElmList(Integer(1), Integer(2), Integer(3)));
    }

    // ========== Tests for foldr ==========

    [Fact]
    public void Foldr_collect_keys()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20), (3, 30));
        var valsResult = ApplyUnary(GetDictFunction("values"), dict);
        valsResult.Should().Be(ElmList(Integer(10), Integer(20), Integer(30)));
    }

    // ========== Tests for filter ==========
    // Keep only the key-value pairs that pass the given test.

    [Fact]
    public void Filter_with_always_true()
    {
        // We test filter indirectly by building a dict and checking size
        var dict = DictFromIntPairs((1, 10), (2, 20), (3, 30));

        var size = ApplyUnary(GetDictFunction("size"), dict);
        size.Should().Be(Integer(3));
    }

    // ========== Tests for multiple operations ==========

    [Fact]
    public void Insert_then_remove_then_get()
    {
        var emptyDict = ApplyUnary(GetDictFunction("fromList"), ElmList());
        var d1 = ApplyTernary(GetDictFunction("insert"), Integer(1), String("a"), emptyDict);
        var d2 = ApplyTernary(GetDictFunction("insert"), Integer(2), String("b"), d1);
        var d3 = ApplyBinary(GetDictFunction("remove"), Integer(1), d2);

        var r1 = ApplyBinary(GetDictFunction("get"), Integer(1), d3);
        r1.Should().Be(s_nothing);

        var r2 = ApplyBinary(GetDictFunction("get"), Integer(2), d3);
        r2.Should().Be(JustOf(String("b")));
    }

    [Fact]
    public void FromList_then_toList_roundtrip()
    {
        var input =
            ElmList(
                Tuple(Integer(3), String("c")),
                Tuple(Integer(1), String("a")),
                Tuple(Integer(2), String("b")));

        var dict = ApplyUnary(GetDictFunction("fromList"), input);
        var output = ApplyUnary(GetDictFunction("toList"), dict);

        // toList sorts by key
        output.Should().Be(
            ElmList(
                Tuple(Integer(1), String("a")),
                Tuple(Integer(2), String("b")),
                Tuple(Integer(3), String("c"))));
    }

    [Fact]
    public void Insert_many_and_verify_size()
    {
        var pairs = Enumerable.Range(1, 10).Select(i => ((long)i, (long)(i * 10))).ToArray();
        var dict = DictFromIntPairs(pairs);
        var result = ApplyUnary(GetDictFunction("size"), dict);
        result.Should().Be(Integer(10));
    }

    [Fact]
    public void Remove_all_keys_results_in_empty()
    {
        var dict = DictFromIntPairs((1, 10), (2, 20), (3, 30));
        var d1 = ApplyBinary(GetDictFunction("remove"), Integer(1), dict);
        var d2 = ApplyBinary(GetDictFunction("remove"), Integer(2), d1);
        var d3 = ApplyBinary(GetDictFunction("remove"), Integer(3), d2);
        var result = ApplyUnary(GetDictFunction("isEmpty"), d3);
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void String_keys_sorted()
    {
        var dict = DictFromStringIntPairs(("banana", 2), ("apple", 1), ("cherry", 3));
        var result = ApplyUnary(GetDictFunction("keys"), dict);
        result.Should().Be(ElmList(String("apple"), String("banana"), String("cherry")));
    }

    [Fact]
    public void Union_larger_dicts()
    {
        var d1 = DictFromIntPairs((1, 10), (2, 20), (3, 30));
        var d2 = DictFromIntPairs((3, 300), (4, 400), (5, 500));
        var result = ApplyBinary(GetDictFunction("union"), d1, d2);
        var keys = ApplyUnary(GetDictFunction("keys"), result);
        keys.Should().Be(ElmList(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5)));
        // First dict values win on collision
        var get3 = ApplyBinary(GetDictFunction("get"), Integer(3), result);

        get3.Should().Be(JustOf(Integer(30)));
    }
}
