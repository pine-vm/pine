using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests for compiler features needed by the Dict and Set kernel modules.
/// These test specific compiler capabilities (tuple destructuring in cons patterns,
/// lambdas passed to higher-order functions, custom type constructors, etc.)
/// in isolation to identify any compiler gaps.
/// </summary>
public class DictCompilerSupportTests
{
    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    #region Helpers

    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment CompileModule(
        string elmModuleText)
    {
        return ElmCompilerTestHelper.CompileElmModules(
            [elmModuleText],
            disableInlining: true);
    }

    private static PineValue GetFunction(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment env,
        string moduleName,
        string funcName)
    {
        return env.Modules
            .First(m => m.moduleName == moduleName)
            .moduleContent.FunctionDeclarations[funcName];
    }

    private static ElmValue Invoke1(PineValue func, ElmValue arg)
    {
        return CoreLibraryModule.CoreLibraryTestHelper.ApplyUnary(func, arg, s_vm);
    }

    private static ElmValue Invoke2(PineValue func, ElmValue arg1, ElmValue arg2)
    {
        return CoreLibraryModule.CoreLibraryTestHelper.ApplyBinary(func, arg1, arg2, s_vm);
    }

    private static ElmValue Invoke3(PineValue func, ElmValue arg1, ElmValue arg2, ElmValue arg3)
    {
        return CoreLibraryModule.CoreLibraryTestHelper.ApplyTernary(func, arg1, arg2, arg3, s_vm);
    }

    private static ElmValue Int(long v) => ElmValue.Integer(v);
    private static ElmValue Str(string v) => ElmValue.StringInstance(v);
    private static ElmValue EList(params ElmValue[] items) => ElmValue.ListInstance([.. items]);
    private static ElmValue Pair(ElmValue a, ElmValue b) => ElmValue.ListInstance([a, b]);

    #endregion

    // ========== Test: Recursive list traversal with cons pattern ==========

    [Fact]
    public void Recursive_list_length_with_cons_pattern()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            listLength : List a -> Int
            listLength lst =
                case lst of
                    [] ->
                        0

                    _ :: rest ->
                        1 + listLength rest

            """");

        var func = GetFunction(env, "Test", "listLength");

        Invoke1(func, EList()).Should().Be(Int(0));
        Invoke1(func, EList(Int(1))).Should().Be(Int(1));
        Invoke1(func, EList(Int(1), Int(2), Int(3))).Should().Be(Int(3));
    }

    // ========== Test: Tuple destructuring in cons pattern ==========

    [Fact]
    public void Tuple_destructuring_in_cons_pattern()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            firstKey : List ( Int, String ) -> Int
            firstKey pairs =
                case pairs of
                    [] ->
                        0

                    ( key, value ) :: rest ->
                        key

            """");

        var func = GetFunction(env, "Test", "firstKey");

        Invoke1(func, EList()).Should().Be(Int(0));
        Invoke1(func, EList(Pair(Int(42), Str("a")))).Should().Be(Int(42));
        Invoke1(func, EList(Pair(Int(7), Str("x")), Pair(Int(8), Str("y")))).Should().Be(Int(7));
    }

    // ========== Test: Recursive tuple-cons pattern (like insertFromList) ==========

    [Fact]
    public void Recursive_tuple_cons_sum_keys()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            sumKeys : List ( Int, a ) -> Int
            sumKeys pairs =
                case pairs of
                    [] ->
                        0

                    ( key, _ ) :: rest ->
                        key + sumKeys rest

            """");

        var func = GetFunction(env, "Test", "sumKeys");

        Invoke1(func, EList()).Should().Be(Int(0));
        Invoke1(func, EList(Pair(Int(1), Str("a")))).Should().Be(Int(1));
        Invoke1(func, EList(Pair(Int(1), Str("a")), Pair(Int(2), Str("b")), Pair(Int(3), Str("c"))))
            .Should().Be(Int(6));
    }

    // ========== Test: Custom type with recursive case matching ==========

    [Fact]
    public void Custom_type_recursive_case_matching()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            type MyTree
                = Leaf
                | Node Int MyTree MyTree


            treeSize : MyTree -> Int
            treeSize tree =
                case tree of
                    Leaf ->
                        0

                    Node _ left right ->
                        1 + treeSize left + treeSize right

            """");

        var func = GetFunction(env, "Test", "treeSize");

        // Leaf
        var leaf = ElmValue.TagInstance("Leaf", []);
        Invoke1(func, leaf).Should().Be(Int(0));

        // Node 1 Leaf Leaf
        var node1 = ElmValue.TagInstance("Node", [Int(1), leaf, leaf]);
        Invoke1(func, node1).Should().Be(Int(1));

        // Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)
        var node2 = ElmValue.TagInstance("Node", [Int(2), leaf, leaf]);
        var node3 = ElmValue.TagInstance("Node", [Int(3), leaf, leaf]);
        var tree = ElmValue.TagInstance("Node", [Int(1), node2, node3]);
        Invoke1(func, tree).Should().Be(Int(3));
    }

    // ========== Test: if-then-else with comparison (like removeHelp uses <) ==========

    [Fact]
    public void If_then_else_with_less_than()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            classify : Int -> String
            classify n =
                if n < 0 then
                    "negative"

                else if n == 0 then
                    "zero"

                else
                    "positive"

            """");

        var func = GetFunction(env, "Test", "classify");

        Invoke1(func, Int(-5)).Should().Be(Str("negative"));
        Invoke1(func, Int(0)).Should().Be(Str("zero"));
        Invoke1(func, Int(3)).Should().Be(Str("positive"));
    }

    // ========== Test: Lambda passed as argument (like foldl uses) ==========

    [Fact]
    public void Lambda_as_argument_to_function()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            applyTwice : (Int -> Int) -> Int -> Int
            applyTwice f x =
                f (f x)


            doubleApply : Int -> Int
            doubleApply x =
                applyTwice (\n -> n + 1) x

            """");

        var func = GetFunction(env, "Test", "doubleApply");

        Invoke1(func, Int(0)).Should().Be(Int(2));
        Invoke1(func, Int(5)).Should().Be(Int(7));
    }

    // ========== Test: Multi-arg lambda (like Dict.foldl uses \k v t -> ...) ==========

    [Fact]
    public void Multi_arg_lambda_in_foldl_style()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            myFoldl : (a -> b -> b) -> b -> List a -> b
            myFoldl func acc list =
                case list of
                    [] ->
                        acc

                    x :: rest ->
                        myFoldl func (func x acc) rest


            sumList : List Int -> Int
            sumList list =
                myFoldl (\x acc -> x + acc) 0 list

            """");

        var func = GetFunction(env, "Test", "sumList");

        Invoke1(func, EList()).Should().Be(Int(0));
        Invoke1(func, EList(Int(1), Int(2), Int(3))).Should().Be(Int(6));
        Invoke1(func, EList(Int(10))).Should().Be(Int(10));
    }

    // ========== Test: Three-arg lambda (like Dict.union's foldl \k v t -> insert k v t) ==========

    [Fact]
    public void Three_arg_lambda_foldl()
    {
        var env = CompileModule(
            """"
            module Test exposing (..)


            myFoldl3 : (a -> b -> c -> c) -> c -> List ( a, b ) -> c
            myFoldl3 func acc pairs =
                case pairs of
                    [] ->
                        acc

                    ( a, b ) :: rest ->
                        myFoldl3 func (func a b acc) rest


            concatPairs : List ( String, String ) -> String
            concatPairs pairs =
                myFoldl3 (\a b acc -> acc ++ a ++ b) "" pairs

            """");

        var func = GetFunction(env, "Test", "concatPairs");

        Invoke1(func, EList()).Should().Be(Str(""));
        Invoke1(func, EList(Pair(Str("a"), Str("b"))))
            .Should().Be(Str("ab"));
        Invoke1(func, EList(Pair(Str("a"), Str("b")), Pair(Str("c"), Str("d"))))
            .Should().Be(Str("abcd"));
    }

    // ========== Test: Compile actual Dict module and test basic operations ==========

    [Fact]
    public void Dict_module_compiles_and_singleton_works()
    {
        var dictModule = GetDictModule();
        var singletonFunc = dictModule.moduleContent.FunctionDeclarations["singleton"];
        var getFunc = dictModule.moduleContent.FunctionDeclarations["get"];

        // Dict.singleton 1 "hello" |> Dict.get 1 == Just "hello"
        var dict = Invoke2(singletonFunc, Int(1), Str("hello"));
        var result = Invoke2(getFunc, Int(1), dict);
        result.Should().Be(ElmValue.TagInstance("Just", [Str("hello")]));
    }

    // ========== Test: Dict.insert via kernel module ==========

    [Fact]
    public void Dict_module_insert_works()
    {
        var dictModule = GetDictModule();
        var singletonFunc = dictModule.moduleContent.FunctionDeclarations["singleton"];
        var insertFunc = dictModule.moduleContent.FunctionDeclarations["insert"];
        var getFunc = dictModule.moduleContent.FunctionDeclarations["get"];
        var sizeFunc = dictModule.moduleContent.FunctionDeclarations["size"];

        // Build: singleton 1 "a" |> insert 2 "b"
        var dict1 = Invoke2(singletonFunc, Int(1), Str("a"));
        var dict2 = Invoke3(insertFunc, Int(2), Str("b"), dict1);

        Invoke1(sizeFunc, dict2).Should().Be(Int(2));
        Invoke2(getFunc, Int(1), dict2).Should().Be(ElmValue.TagInstance("Just", [Str("a")]));
        Invoke2(getFunc, Int(2), dict2).Should().Be(ElmValue.TagInstance("Just", [Str("b")]));
        Invoke2(getFunc, Int(3), dict2).Should().Be(ElmValue.TagInstance("Nothing", []));
    }

    // ========== Test: Dict.fromList via kernel module ==========

    [Fact]
    public void Dict_module_fromList_empty()
    {
        var dictModule = GetDictModule();
        var fromListFunc = dictModule.moduleContent.FunctionDeclarations["fromList"];
        var isEmptyFunc = dictModule.moduleContent.FunctionDeclarations["isEmpty"];
        var singletonFunc = dictModule.moduleContent.FunctionDeclarations["singleton"];
        var sizeFunc = dictModule.moduleContent.FunctionDeclarations["size"];

        // First verify: isEmpty (singleton 1 "x") == False
        var singletonDict = Invoke2(singletonFunc, Int(1), Str("x"));
        Invoke1(isEmptyFunc, singletonDict).Should().Be(ElmValue.FalseValue);
        Invoke1(sizeFunc, singletonDict).Should().Be(Int(1));

        // Now test: fromList [] should produce an empty dict
        var fromListResult = Invoke1(fromListFunc, EList());
        // Let's check what fromList [] actually returns as Pine value
        var fromListPine = CoreLibraryModule.CoreLibraryTestHelper.ApplyGenericPine(
            fromListFunc,
            [ElmValueEncoding.ElmValueAsPineValue(EList())],
            s_vm);
        var fromListElm =
            ElmValueEncoding.PineValueAsElmValue(fromListPine, null, null)
            .Extract(err => throw new Exception("Failed: " + err));

        // Check isEmpty on the result - debug what we got
        var emptyResult = Invoke1(isEmptyFunc, fromListElm);
        emptyResult.Should().Be(ElmValue.TrueValue,
            "fromList [] should be isEmpty but got dict: " +
            ElmValue.RenderAsElmExpression(fromListElm).expressionString);
    }

    [Fact]
    public void Dict_module_fromList_one_pair()
    {
        var dictModule = GetDictModule();
        var fromListFunc = dictModule.moduleContent.FunctionDeclarations["fromList"];
        var sizeFunc = dictModule.moduleContent.FunctionDeclarations["size"];
        var getFunc = dictModule.moduleContent.FunctionDeclarations["get"];

        // fromList [ (1, "a") ]
        var pairs = EList(Pair(Int(1), Str("a")));
        var dict = Invoke1(fromListFunc, pairs);

        Invoke1(sizeFunc, dict).Should().Be(Int(1));
        Invoke2(getFunc, Int(1), dict).Should().Be(ElmValue.TagInstance("Just", [Str("a")]));
    }

    [Fact]
    public void Dict_module_fromList_two_pairs()
    {
        var dictModule = GetDictModule();
        var fromListFunc = dictModule.moduleContent.FunctionDeclarations["fromList"];
        var sizeFunc = dictModule.moduleContent.FunctionDeclarations["size"];
        var getFunc = dictModule.moduleContent.FunctionDeclarations["get"];

        // fromList [ (1, "a"), (2, "b") ]
        var pairs = EList(Pair(Int(1), Str("a")), Pair(Int(2), Str("b")));
        var dict = Invoke1(fromListFunc, pairs);

        Invoke1(sizeFunc, dict).Should().Be(Int(2));
        Invoke2(getFunc, Int(1), dict).Should().Be(ElmValue.TagInstance("Just", [Str("a")]));
        Invoke2(getFunc, Int(2), dict).Should().Be(ElmValue.TagInstance("Just", [Str("b")]));
    }

    private static (string moduleName, PineValue moduleValue, ElmInteractiveEnvironment.ElmModule moduleContent) GetDictModule()
    {
        return s_dictEnv.Value.Modules.First(m => m.moduleName is "Dict");
    }

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_dictEnv =
        new(() =>
        {
            var kernelModulesTree =
                BundledFiles.CompilerSourceContainerFilesDefault.Value
                .GetNodeAtPath(["elm-kernel-modules"])
                ?? throw new Exception("Did not find elm-kernel-modules");

            var rootFilePaths =
                kernelModulesTree.EnumerateFilesTransitive()
                .Where(b => b.path[^1].Equals("Dict.elm", StringComparison.OrdinalIgnoreCase))
                .Select(b => (IReadOnlyList<string>)b.path)
                .ToList();

            var compiledEnv =
                ElmCompiler.CompileInteractiveEnvironment(
                    kernelModulesTree,
                    rootFilePaths: rootFilePaths,
                    disableInlining: false)
                .Extract(err => throw new Exception("Failed compiling Dict: " + err));

            return
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                .Extract(err => throw new Exception("Failed parsing environment: " + err));
        });
}
