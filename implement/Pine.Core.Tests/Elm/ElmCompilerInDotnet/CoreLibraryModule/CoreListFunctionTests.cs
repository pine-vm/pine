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

public class CoreListFunctionTests
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
                        b.path[^1].Equals("List.elm", StringComparison.OrdinalIgnoreCase))
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

    private static PineValue GetListFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "List")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static readonly ElmValue s_nothing =
        ElmValue.TagInstance("Nothing", []);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

    private static PineValue ToPine(ElmValue value) =>
        ElmValueEncoding.ElmValueAsPineValue(value);

    private static PineValue GetNegateFunction() =>
        Core.Elm.ElmCompilerInDotnet.CoreLibraryModule.CoreBasics.GetFunctionValue("negate")!;

    private static PineValue GetAddFunction() =>
        Core.Elm.ElmCompilerInDotnet.CoreLibraryModule.CoreBasics.GetFunctionValue("add")!;

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue ApplyBinary(PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm);

    private static ElmValue ApplyWithPineArgs(PineValue functionValue, params PineValue[] pineArgs) =>
        CoreLibraryTestHelper.ApplyWithPineArgs(s_vm, functionValue, pineArgs);

    // ========== Tests for singleton ==========
    // singleton 1234 == [1234]

    [Fact]
    public void Singleton_1234()
    {
        var result = ApplyUnary(GetListFunction("singleton"), Integer(1234));
        result.Should().Be(ElmList(Integer(1234)));
    }

    // ========== Tests for repeat ==========
    // repeat 3 1 == [1,1,1]

    [Fact]
    public void Repeat_3_1()
    {
        var result = ApplyBinary(GetListFunction("repeat"), Integer(3), Integer(1));
        result.Should().Be(ElmList(Integer(1), Integer(1), Integer(1)));
    }

    // ========== Tests for range ==========
    // range 3 6 == [3,4,5,6]
    // range 3 3 == [3]
    // range 6 3 == []

    [Fact]
    public void Range_3_6()
    {
        var result = ApplyBinary(GetListFunction("range"), Integer(3), Integer(6));
        result.Should().Be(ElmList(Integer(3), Integer(4), Integer(5), Integer(6)));
    }

    [Fact]
    public void Range_3_3()
    {
        var result = ApplyBinary(GetListFunction("range"), Integer(3), Integer(3));
        result.Should().Be(ElmList(Integer(3)));
    }

    [Fact]
    public void Range_6_3()
    {
        var result = ApplyBinary(GetListFunction("range"), Integer(6), Integer(3));
        result.Should().Be(ElmList());
    }

    // ========== Tests for length ==========
    // length [1,2,3] == 3

    [Fact]
    public void Length_3()
    {
        var result = ApplyUnary(GetListFunction("length"), ElmList(Integer(1), Integer(2), Integer(3)));
        result.Should().Be(Integer(3));
    }

    // ========== Tests for reverse ==========
    // reverse [1,2,3,4] == [4,3,2,1]

    [Fact]
    public void Reverse_1234()
    {
        var result =
            ApplyUnary(GetListFunction("reverse"), ElmList(Integer(1), Integer(2), Integer(3), Integer(4)));

        result.Should().Be(ElmList(Integer(4), Integer(3), Integer(2), Integer(1)));
    }

    // ========== Tests for member ==========
    // member 9 [1,2,3,4] == False
    // member 4 [1,2,3,4] == True

    [Fact]
    public void Member_9_not_found()
    {
        var result =
            ApplyBinary(GetListFunction("member"), Integer(9), ElmList(Integer(1), Integer(2), Integer(3), Integer(4)));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void Member_4_found()
    {
        var result =
            ApplyBinary(GetListFunction("member"), Integer(4), ElmList(Integer(1), Integer(2), Integer(3), Integer(4)));

        result.Should().Be(ElmValue.TrueValue);
    }

    // ========== Tests for maximum ==========
    // maximum [1,4,2] == Just 4
    // maximum [] == Nothing

    [Fact]
    public void Maximum_1_4_2()
    {
        var result = ApplyUnary(GetListFunction("maximum"), ElmList(Integer(1), Integer(4), Integer(2)));
        result.Should().Be(JustOf(Integer(4)));
    }

    [Fact]
    public void Maximum_empty()
    {
        var result = ApplyUnary(GetListFunction("maximum"), ElmList());
        result.Should().Be(s_nothing);
    }

    // ========== Tests for minimum ==========
    // minimum [3,2,1] == Just 1
    // minimum [] == Nothing

    [Fact]
    public void Minimum_3_2_1()
    {
        var result = ApplyUnary(GetListFunction("minimum"), ElmList(Integer(3), Integer(2), Integer(1)));
        result.Should().Be(JustOf(Integer(1)));
    }

    [Fact]
    public void Minimum_empty()
    {
        var result = ApplyUnary(GetListFunction("minimum"), ElmList());
        result.Should().Be(s_nothing);
    }

    // ========== Tests for sum ==========
    // sum [1,2,3] == 6
    // sum [1,1,1] == 3
    // sum [] == 0

    [Fact]
    public void Sum_1_2_3()
    {
        var result = ApplyUnary(GetListFunction("sum"), ElmList(Integer(1), Integer(2), Integer(3)));
        result.Should().Be(Integer(6));
    }

    [Fact]
    public void Sum_1_1_1()
    {
        var result = ApplyUnary(GetListFunction("sum"), ElmList(Integer(1), Integer(1), Integer(1)));
        result.Should().Be(Integer(3));
    }

    [Fact]
    public void Sum_empty()
    {
        var result = ApplyUnary(GetListFunction("sum"), ElmList());
        result.Should().Be(Integer(0));
    }

    // ========== Tests for product ==========
    // product [2,2,2] == 8
    // product [3,3,3] == 27
    // product [] == 1

    [Fact]
    public void Product_2_2_2()
    {
        var result = ApplyUnary(GetListFunction("product"), ElmList(Integer(2), Integer(2), Integer(2)));
        result.Should().Be(Integer(8));
    }

    [Fact]
    public void Product_3_3_3()
    {
        var result = ApplyUnary(GetListFunction("product"), ElmList(Integer(3), Integer(3), Integer(3)));
        result.Should().Be(Integer(27));
    }

    [Fact]
    public void Product_empty()
    {
        var result = ApplyUnary(GetListFunction("product"), ElmList());
        result.Should().Be(Integer(1));
    }

    // ========== Tests for append ==========
    // append [1,1,2] [3,5,8] == [1,1,2,3,5,8]

    [Fact]
    public void Append_two_lists()
    {
        var result =
            ApplyBinary(
                GetListFunction("append"),
                ElmList(Integer(1), Integer(1), Integer(2)),
                ElmList(Integer(3), Integer(5), Integer(8)));

        result.Should().Be(ElmList(Integer(1), Integer(1), Integer(2), Integer(3), Integer(5), Integer(8)));
    }

    // ========== Tests for concat ==========
    // concat [[1,2],[3],[4,5]] == [1,2,3,4,5]

    [Fact]
    public void Concat_nested_lists()
    {
        var result =
            ApplyUnary(
                GetListFunction("concat"),
                ElmList(
                    ElmList(Integer(1), Integer(2)),
                    ElmList(Integer(3)),
                    ElmList(Integer(4), Integer(5))));

        result.Should().Be(ElmList(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5)));
    }

    // ========== Tests for intersperse ==========
    // intersperse 0 [1,2,3] == [1,0,2,0,3]

    [Fact]
    public void Intersperse_0_in_1_2_3()
    {
        var result =
            ApplyBinary(
                GetListFunction("intersperse"),
                Integer(0),
                ElmList(Integer(1), Integer(2), Integer(3)));

        result.Should().Be(ElmList(Integer(1), Integer(0), Integer(2), Integer(0), Integer(3)));
    }

    // ========== Tests for sort ==========
    // sort [3,1,5] == [1,3,5]

    [Fact]
    public void Sort_3_1_5()
    {
        var result =
            ApplyUnary(GetListFunction("sort"), ElmList(Integer(3), Integer(1), Integer(5)));

        result.Should().Be(ElmList(Integer(1), Integer(3), Integer(5)));
    }

    // ========== Tests for isEmpty ==========
    // isEmpty [] == True

    [Fact]
    public void IsEmpty_empty_list()
    {
        var result = ApplyUnary(GetListFunction("isEmpty"), ElmList());
        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsEmpty_non_empty_list()
    {
        var result = ApplyUnary(GetListFunction("isEmpty"), ElmList(Integer(1)));
        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for head ==========
    // head [1,2,3] == Just 1
    // head [] == Nothing

    [Fact]
    public void Head_1_2_3()
    {
        var result =
            ApplyUnary(GetListFunction("head"), ElmList(Integer(1), Integer(2), Integer(3)));

        result.Should().Be(JustOf(Integer(1)));
    }

    [Fact]
    public void Head_empty()
    {
        var result = ApplyUnary(GetListFunction("head"), ElmList());
        result.Should().Be(s_nothing);
    }

    // ========== Tests for tail ==========
    // tail [1,2,3] == Just [2,3]
    // tail [] == Nothing

    [Fact]
    public void Tail_1_2_3()
    {
        var result =
            ApplyUnary(GetListFunction("tail"), ElmList(Integer(1), Integer(2), Integer(3)));

        result.Should().Be(JustOf(ElmList(Integer(2), Integer(3))));
    }

    [Fact]
    public void Tail_empty()
    {
        var result = ApplyUnary(GetListFunction("tail"), ElmList());
        result.Should().Be(s_nothing);
    }

    // ========== Tests for take ==========
    // take 2 [1,2,3,4] == [1,2]

    [Fact]
    public void Take_2()
    {
        var result =
            ApplyBinary(
                GetListFunction("take"),
                Integer(2),
                ElmList(Integer(1), Integer(2), Integer(3), Integer(4)));

        result.Should().Be(ElmList(Integer(1), Integer(2)));
    }

    // ========== Tests for drop ==========
    // drop 2 [1,2,3,4] == [3,4]

    [Fact]
    public void Drop_2()
    {
        var result =
            ApplyBinary(
                GetListFunction("drop"),
                Integer(2),
                ElmList(Integer(1), Integer(2), Integer(3), Integer(4)));

        result.Should().Be(ElmList(Integer(3), Integer(4)));
    }

    // ========== Tests for map (higher-order) ==========
    // map negate [1,2,3] == [-1,-2,-3]

    [Fact]
    public void Map_negate()
    {
        var result =
            ApplyWithPineArgs(
                GetListFunction("map"),
                GetNegateFunction(),
                ToPine(ElmList(Integer(1), Integer(2), Integer(3))));

        result.Should().Be(ElmList(Integer(-1), Integer(-2), Integer(-3)));
    }

    // ========== Tests for foldl (higher-order) ==========
    // foldl (+) 0 [1,2,3] == 6

    [Fact]
    public void Foldl_add_0()
    {
        var result =
            ApplyWithPineArgs(
                GetListFunction("foldl"),
                GetAddFunction(),
                ToPine(Integer(0)),
                ToPine(ElmList(Integer(1), Integer(2), Integer(3))));

        result.Should().Be(Integer(6));
    }

    // ========== Tests for foldr (higher-order) ==========
    // foldr (+) 0 [1,2,3] == 6

    [Fact]
    public void Foldr_add_0()
    {
        var result =
            ApplyWithPineArgs(
                GetListFunction("foldr"),
                GetAddFunction(),
                ToPine(Integer(0)),
                ToPine(ElmList(Integer(1), Integer(2), Integer(3))));

        result.Should().Be(Integer(6));
    }

    // ========== Tests for sortBy (higher-order) ==========
    // sortBy negate [1,3,2] == [3,2,1]

    [Fact]
    public void SortBy_negate()
    {
        var result =
            ApplyWithPineArgs(
                GetListFunction("sortBy"),
                GetNegateFunction(),
                ToPine(ElmList(Integer(1), Integer(3), Integer(2))));

        result.Should().Be(ElmList(Integer(3), Integer(2), Integer(1)));
    }

    // ========== Tests for concatMap (higher-order) ==========
    // concatMap singleton [1,2,3] == [1,2,3]

    [Fact]
    public void ConcatMap_singleton()
    {
        var result =
            ApplyWithPineArgs(
                GetListFunction("concatMap"),
                GetListFunction("singleton"),
                ToPine(ElmList(Integer(1), Integer(2), Integer(3))));

        result.Should().Be(ElmList(Integer(1), Integer(2), Integer(3)));
    }
}
