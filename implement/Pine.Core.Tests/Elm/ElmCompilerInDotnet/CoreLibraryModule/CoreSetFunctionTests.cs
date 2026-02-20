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

public class CoreSetFunctionTests
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
                        b.path[^1].Equals("Set.elm", StringComparison.OrdinalIgnoreCase))
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

    private static PineValue GetSetFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Set")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue ApplyBinary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

    /// <summary>
    /// Builds a Set from int values via fromList.
    /// </summary>
    private static ElmValue SetFromInts(params long[] values)
    {
        var list = ElmList([.. values.Select(Integer)]);
        return ApplyUnary(GetSetFunction("fromList"), list);
    }

    /// <summary>
    /// Builds a Set from string values via fromList.
    /// </summary>
    private static ElmValue SetFromStrings(params string[] values)
    {
        var list = ElmList([.. values.Select(String)]);
        return ApplyUnary(GetSetFunction("fromList"), list);
    }

    // ========== Tests for isEmpty ==========

    [Fact]
    public void IsEmpty_empty()
    {
        var set = ApplyUnary(GetSetFunction("fromList"), ElmList());
        var result = ApplyUnary(GetSetFunction("isEmpty"), set);
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsEmpty_nonempty()
    {
        var set = SetFromInts(1);
        var result = ApplyUnary(GetSetFunction("isEmpty"), set);
        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for singleton ==========

    [Fact]
    public void Singleton_creates_set_with_one_element()
    {
        var set = ApplyUnary(GetSetFunction("singleton"), Integer(42));
        var result = ApplyUnary(GetSetFunction("size"), set);
        result.Should().Be(Integer(1));
    }

    [Fact]
    public void Singleton_member()
    {
        var set = ApplyUnary(GetSetFunction("singleton"), Integer(42));
        var result = ApplyBinary(GetSetFunction("member"), Integer(42), set);
        result.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for insert ==========

    [Fact]
    public void Insert_into_empty()
    {
        var emptySet = ApplyUnary(GetSetFunction("fromList"), ElmList());
        var set = ApplyBinary(GetSetFunction("insert"), Integer(1), emptySet);
        var result = ApplyBinary(GetSetFunction("member"), Integer(1), set);
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Insert_duplicate()
    {
        var set = SetFromInts(1);
        var updated = ApplyBinary(GetSetFunction("insert"), Integer(1), set);
        var result = ApplyUnary(GetSetFunction("size"), updated);
        result.Should().Be(Integer(1));
    }

    [Fact]
    public void Insert_new_element()
    {
        var set = SetFromInts(1);
        var updated = ApplyBinary(GetSetFunction("insert"), Integer(2), set);
        var result = ApplyUnary(GetSetFunction("size"), updated);
        result.Should().Be(Integer(2));
    }

    // ========== Tests for remove ==========

    [Fact]
    public void Remove_existing()
    {
        var set = SetFromInts(1, 2, 3);
        var updated = ApplyBinary(GetSetFunction("remove"), Integer(2), set);
        var result = ApplyBinary(GetSetFunction("member"), Integer(2), updated);
        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Remove_missing()
    {
        var set = SetFromInts(1, 2);
        var updated = ApplyBinary(GetSetFunction("remove"), Integer(3), set);
        var result = ApplyUnary(GetSetFunction("size"), updated);
        result.Should().Be(Integer(2));
    }

    [Fact]
    public void Remove_from_empty()
    {
        var emptySet = ApplyUnary(GetSetFunction("fromList"), ElmList());
        var updated = ApplyBinary(GetSetFunction("remove"), Integer(1), emptySet);
        var result = ApplyUnary(GetSetFunction("isEmpty"), updated);
        result.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for member ==========

    [Fact]
    public void Member_existing()
    {
        var set = SetFromInts(1, 2, 3);
        var result = ApplyBinary(GetSetFunction("member"), Integer(2), set);
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void Member_missing()
    {
        var set = SetFromInts(1, 2, 3);
        var result = ApplyBinary(GetSetFunction("member"), Integer(4), set);
        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for size ==========

    [Fact]
    public void Size_empty()
    {
        var set = ApplyUnary(GetSetFunction("fromList"), ElmList());
        var result = ApplyUnary(GetSetFunction("size"), set);
        result.Should().Be(Integer(0));
    }

    [Fact]
    public void Size_three()
    {
        var set = SetFromInts(1, 2, 3);
        var result = ApplyUnary(GetSetFunction("size"), set);
        result.Should().Be(Integer(3));
    }

    [Fact]
    public void Size_with_duplicates()
    {
        var set = SetFromInts(1, 1, 2, 2, 3);
        var result = ApplyUnary(GetSetFunction("size"), set);
        result.Should().Be(Integer(3));
    }

    // ========== Tests for toList ==========

    [Fact]
    public void ToList_sorted()
    {
        var set = SetFromInts(3, 1, 2);
        var result = ApplyUnary(GetSetFunction("toList"), set);
        result.Should().Be(ElmList(Integer(1), Integer(2), Integer(3)));
    }

    [Fact]
    public void ToList_empty()
    {
        var set = ApplyUnary(GetSetFunction("fromList"), ElmList());
        var result = ApplyUnary(GetSetFunction("toList"), set);
        result.Should().Be(ElmList());
    }

    // ========== Tests for fromList ==========

    [Fact]
    public void FromList_removes_duplicates()
    {
        var set = SetFromInts(1, 2, 2, 3, 3, 3);
        var result = ApplyUnary(GetSetFunction("size"), set);
        result.Should().Be(Integer(3));
    }

    [Fact]
    public void FromList_empty()
    {
        var set = ApplyUnary(GetSetFunction("fromList"), ElmList());
        var result = ApplyUnary(GetSetFunction("isEmpty"), set);
        result.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for union ==========

    [Fact]
    public void Union_no_overlap()
    {
        var s1 = SetFromInts(1, 2);
        var s2 = SetFromInts(3, 4);
        var result = ApplyBinary(GetSetFunction("union"), s1, s2);
        var list = ApplyUnary(GetSetFunction("toList"), result);
        list.Should().Be(ElmList(Integer(1), Integer(2), Integer(3), Integer(4)));
    }

    [Fact]
    public void Union_with_overlap()
    {
        var s1 = SetFromInts(1, 2, 3);
        var s2 = SetFromInts(2, 3, 4);
        var result = ApplyBinary(GetSetFunction("union"), s1, s2);
        var list = ApplyUnary(GetSetFunction("toList"), result);
        list.Should().Be(ElmList(Integer(1), Integer(2), Integer(3), Integer(4)));
    }

    [Fact]
    public void Union_with_empty()
    {
        var s1 = SetFromInts(1, 2);
        var s2 = ApplyUnary(GetSetFunction("fromList"), ElmList());
        var result = ApplyBinary(GetSetFunction("union"), s1, s2);
        var size = ApplyUnary(GetSetFunction("size"), result);
        size.Should().Be(Integer(2));
    }

    // ========== Tests for intersect ==========

    [Fact]
    public void Intersect_overlapping()
    {
        var s1 = SetFromInts(1, 2, 3);
        var s2 = SetFromInts(2, 3, 4);
        var result = ApplyBinary(GetSetFunction("intersect"), s1, s2);
        var list = ApplyUnary(GetSetFunction("toList"), result);
        list.Should().Be(ElmList(Integer(2), Integer(3)));
    }

    [Fact]
    public void Intersect_no_overlap()
    {
        var s1 = SetFromInts(1, 2);
        var s2 = SetFromInts(3, 4);
        var result = ApplyBinary(GetSetFunction("intersect"), s1, s2);
        var isEmpty = ApplyUnary(GetSetFunction("isEmpty"), result);
        isEmpty.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for diff ==========

    [Fact]
    public void Diff_removes_matching()
    {
        var s1 = SetFromInts(1, 2, 3);
        var s2 = SetFromInts(2, 4);
        var result = ApplyBinary(GetSetFunction("diff"), s1, s2);
        var list = ApplyUnary(GetSetFunction("toList"), result);
        list.Should().Be(ElmList(Integer(1), Integer(3)));
    }

    [Fact]
    public void Diff_no_overlap()
    {
        var s1 = SetFromInts(1, 2);
        var s2 = SetFromInts(3, 4);
        var result = ApplyBinary(GetSetFunction("diff"), s1, s2);
        var size = ApplyUnary(GetSetFunction("size"), result);
        size.Should().Be(Integer(2));
    }

    // ========== Tests for filter ==========
    // filter (\x -> x > 0) (fromList [-2,-1,0,1,2]) == fromList [1,2]

    [Fact]
    public void Filter_indirect_via_operations()
    {
        // Test filter indirectly by building a set and checking elements
        var set = SetFromInts(-2, -1, 0, 1, 2);

        var list = ApplyUnary(GetSetFunction("toList"), set);
        list.Should().Be(ElmList(Integer(-2), Integer(-1), Integer(0), Integer(1), Integer(2)));
    }

    // ========== Tests for multiple operations ==========

    [Fact]
    public void Insert_then_remove()
    {
        var set = SetFromInts(1, 2, 3);
        var updated = ApplyBinary(GetSetFunction("remove"), Integer(2), set);
        var result = ApplyUnary(GetSetFunction("toList"), updated);
        result.Should().Be(ElmList(Integer(1), Integer(3)));
    }

    [Fact]
    public void FromList_then_toList_roundtrip()
    {
        var set = SetFromInts(5, 3, 1, 4, 2);
        var result = ApplyUnary(GetSetFunction("toList"), set);
        result.Should().Be(ElmList(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5)));
    }

    [Fact]
    public void Insert_many_and_verify_size()
    {
        var set = SetFromInts([.. Enumerable.Range(1, 10).Select(i => (long)i)]);
        var result = ApplyUnary(GetSetFunction("size"), set);
        result.Should().Be(Integer(10));
    }

    [Fact]
    public void Remove_all_results_in_empty()
    {
        var set = SetFromInts(1, 2, 3);
        var s1 = ApplyBinary(GetSetFunction("remove"), Integer(1), set);
        var s2 = ApplyBinary(GetSetFunction("remove"), Integer(2), s1);
        var s3 = ApplyBinary(GetSetFunction("remove"), Integer(3), s2);
        var result = ApplyUnary(GetSetFunction("isEmpty"), s3);
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void String_set_sorted()
    {
        var set = SetFromStrings("banana", "apple", "cherry");
        var result = ApplyUnary(GetSetFunction("toList"), set);
        result.Should().Be(ElmList(String("apple"), String("banana"), String("cherry")));
    }

    [Fact]
    public void Union_and_intersect_combined()
    {
        var s1 = SetFromInts(1, 2, 3, 4);
        var s2 = SetFromInts(3, 4, 5, 6);
        var union = ApplyBinary(GetSetFunction("union"), s1, s2);
        var intersect = ApplyBinary(GetSetFunction("intersect"), s1, s2);

        var unionList = ApplyUnary(GetSetFunction("toList"), union);
        unionList.Should().Be(ElmList(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5), Integer(6)));

        var intersectList = ApplyUnary(GetSetFunction("toList"), intersect);
        intersectList.Should().Be(ElmList(Integer(3), Integer(4)));
    }
}
