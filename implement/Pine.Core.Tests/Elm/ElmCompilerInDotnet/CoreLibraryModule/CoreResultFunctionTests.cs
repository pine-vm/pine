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

public class CoreResultFunctionTests
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
                        b.path[^1].Equals("Result.elm", StringComparison.OrdinalIgnoreCase))
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

    private static PineValue GetResultFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Result")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyBinary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue OkOf(ElmValue inner) =>
        ElmValue.TagInstance("Ok", [inner]);

    private static ElmValue ErrOf(ElmValue inner) =>
        ElmValue.TagInstance("Err", [inner]);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static readonly ElmValue s_nothing =
        ElmValue.TagInstance("Nothing", []);

    private static PineValue ToPine(ElmValue value) =>
        ElmValueEncoding.ElmValueAsPineValue(value);

    private static ElmValue ApplyWithPineArgs(
        PineValue functionValue, params PineValue[] pineArgs) =>
        CoreLibraryTestHelper.ApplyWithPineArgs(s_vm, functionValue, pineArgs);

    private static PineValue GetNegateFunction() =>
        Core.Elm.ElmCompilerInDotnet.CoreLibraryModule.CoreBasics.GetFunctionValue("negate")!;

    private static PineValue GetAddFunction() =>
        Core.Elm.ElmCompilerInDotnet.CoreLibraryModule.CoreBasics.GetFunctionValue("add")!;

    // ========== Tests for withDefault ==========
    // Result.withDefault 0 (Ok 123) == 123
    // Result.withDefault 0 (Err "no") == 0

    [Fact]
    public void WithDefault_Ok_123()
    {
        var result =
            ApplyBinary(GetResultFunction("withDefault"), Integer(0), OkOf(Integer(123)));

        result.Should().Be(Integer(123));
    }

    [Fact]
    public void WithDefault_Err()
    {
        var result =
            ApplyBinary(GetResultFunction("withDefault"), Integer(0), ErrOf(String("no")));

        result.Should().Be(Integer(0));
    }

    [Fact]
    public void WithDefault_Ok_string()
    {
        var result =
            ApplyBinary(GetResultFunction("withDefault"), String("default"), OkOf(String("hello")));

        result.Should().Be(String("hello"));
    }

    [Fact]
    public void WithDefault_Err_returns_default_string()
    {
        var result =
            ApplyBinary(GetResultFunction("withDefault"), String("default"), ErrOf(Integer(42)));

        result.Should().Be(String("default"));
    }

    // ========== Tests for map ==========
    // map negate (Ok 4) == Ok (-4)
    // map negate (Err "bad input") == Err "bad input"

    [Fact]
    public void Map_negate_Ok()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map"),
                GetNegateFunction(),
                ToPine(OkOf(Integer(4))));

        result.Should().Be(OkOf(Integer(-4)));
    }

    [Fact]
    public void Map_negate_Err()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map"),
                GetNegateFunction(),
                ToPine(ErrOf(String("bad input"))));

        result.Should().Be(ErrOf(String("bad input")));
    }

    [Fact]
    public void Map_negate_Ok_zero()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map"),
                GetNegateFunction(),
                ToPine(OkOf(Integer(0))));

        result.Should().Be(OkOf(Integer(0)));
    }

    [Fact]
    public void Map_negate_Ok_negative()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map"),
                GetNegateFunction(),
                ToPine(OkOf(Integer(-7))));

        result.Should().Be(OkOf(Integer(7)));
    }

    // ========== Tests for map2 ==========
    // map2 add (Ok 42) (Ok 13) == Ok 55
    // map2 add (Err "x") (Ok 13) == Err "x"
    // map2 add (Ok 42) (Err "y") == Err "y"
    // map2 add (Err "x") (Err "y") == Err "x"

    [Fact]
    public void Map2_add_Ok_Ok()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map2"),
                GetAddFunction(),
                ToPine(OkOf(Integer(42))),
                ToPine(OkOf(Integer(13))));

        result.Should().Be(OkOf(Integer(55)));
    }

    [Fact]
    public void Map2_add_Err_Ok()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map2"),
                GetAddFunction(),
                ToPine(ErrOf(String("x"))),
                ToPine(OkOf(Integer(13))));

        result.Should().Be(ErrOf(String("x")));
    }

    [Fact]
    public void Map2_add_Ok_Err()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map2"),
                GetAddFunction(),
                ToPine(OkOf(Integer(42))),
                ToPine(ErrOf(String("y"))));

        result.Should().Be(ErrOf(String("y")));
    }

    [Fact]
    public void Map2_add_Err_Err()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map2"),
                GetAddFunction(),
                ToPine(ErrOf(String("x"))),
                ToPine(ErrOf(String("y"))));

        result.Should().Be(ErrOf(String("x")));
    }

    // ========== Tests for map3 ==========

    [Fact]
    public void Map3_Err_first()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map3"),
                GetAddFunction(),
                ToPine(ErrOf(String("x"))),
                ToPine(OkOf(Integer(2))),
                ToPine(OkOf(Integer(3))));

        result.Should().Be(ErrOf(String("x")));
    }

    [Fact]
    public void Map3_Err_second()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map3"),
                GetAddFunction(),
                ToPine(OkOf(Integer(1))),
                ToPine(ErrOf(String("y"))),
                ToPine(OkOf(Integer(3))));

        result.Should().Be(ErrOf(String("y")));
    }

    [Fact]
    public void Map3_Err_third()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map3"),
                GetAddFunction(),
                ToPine(OkOf(Integer(1))),
                ToPine(OkOf(Integer(2))),
                ToPine(ErrOf(String("z"))));

        result.Should().Be(ErrOf(String("z")));
    }

    // ========== Tests for map4 ==========

    [Fact]
    public void Map4_Err_first()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map4"),
                GetAddFunction(),
                ToPine(ErrOf(String("a"))),
                ToPine(OkOf(Integer(2))),
                ToPine(OkOf(Integer(3))),
                ToPine(OkOf(Integer(4))));

        result.Should().Be(ErrOf(String("a")));
    }

    [Fact]
    public void Map4_Err_second()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map4"),
                GetAddFunction(),
                ToPine(OkOf(Integer(1))),
                ToPine(ErrOf(String("b"))),
                ToPine(OkOf(Integer(3))),
                ToPine(OkOf(Integer(4))));

        result.Should().Be(ErrOf(String("b")));
    }

    [Fact]
    public void Map4_Err_third()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map4"),
                GetAddFunction(),
                ToPine(OkOf(Integer(1))),
                ToPine(OkOf(Integer(2))),
                ToPine(ErrOf(String("c"))),
                ToPine(OkOf(Integer(4))));

        result.Should().Be(ErrOf(String("c")));
    }

    [Fact]
    public void Map4_Err_fourth()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map4"),
                GetAddFunction(),
                ToPine(OkOf(Integer(1))),
                ToPine(OkOf(Integer(2))),
                ToPine(OkOf(Integer(3))),
                ToPine(ErrOf(String("d"))));

        result.Should().Be(ErrOf(String("d")));
    }

    // ========== Tests for map5 ==========

    [Fact]
    public void Map5_Err_first()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map5"),
                GetAddFunction(),
                ToPine(ErrOf(String("a"))),
                ToPine(OkOf(Integer(2))),
                ToPine(OkOf(Integer(3))),
                ToPine(OkOf(Integer(4))),
                ToPine(OkOf(Integer(5))));

        result.Should().Be(ErrOf(String("a")));
    }

    [Fact]
    public void Map5_Err_third()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map5"),
                GetAddFunction(),
                ToPine(OkOf(Integer(1))),
                ToPine(OkOf(Integer(2))),
                ToPine(ErrOf(String("c"))),
                ToPine(OkOf(Integer(4))),
                ToPine(OkOf(Integer(5))));

        result.Should().Be(ErrOf(String("c")));
    }

    [Fact]
    public void Map5_Err_fifth()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("map5"),
                GetAddFunction(),
                ToPine(OkOf(Integer(1))),
                ToPine(OkOf(Integer(2))),
                ToPine(OkOf(Integer(3))),
                ToPine(OkOf(Integer(4))),
                ToPine(ErrOf(String("e"))));

        result.Should().Be(ErrOf(String("e")));
    }

    // ========== Tests for andThen ==========
    // andThen callback (Ok value) == callback value
    // andThen callback (Err msg) == Err msg

    [Fact]
    public void AndThen_Err()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("andThen"),
                GetNegateFunction(),
                ToPine(ErrOf(String("error"))));

        result.Should().Be(ErrOf(String("error")));
    }

    [Fact]
    public void AndThen_Err_with_integer_error()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("andThen"),
                GetNegateFunction(),
                ToPine(ErrOf(Integer(404))));

        result.Should().Be(ErrOf(Integer(404)));
    }

    // ========== Tests for mapError ==========
    // mapError f (Ok v) == Ok v
    // mapError f (Err e) == Err (f e)

    [Fact]
    public void MapError_Ok()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("mapError"),
                GetNegateFunction(),
                ToPine(OkOf(Integer(123))));

        result.Should().Be(OkOf(Integer(123)));
    }

    [Fact]
    public void MapError_Err()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("mapError"),
                GetNegateFunction(),
                ToPine(ErrOf(Integer(5))));

        result.Should().Be(ErrOf(Integer(-5)));
    }

    [Fact]
    public void MapError_Ok_string()
    {
        var result =
            ApplyWithPineArgs(
                GetResultFunction("mapError"),
                GetNegateFunction(),
                ToPine(OkOf(String("hello"))));

        result.Should().Be(OkOf(String("hello")));
    }

    // ========== Tests for toMaybe ==========
    // toMaybe (Ok v) == Just v
    // toMaybe (Err _) == Nothing

    [Fact]
    public void ToMaybe_Ok()
    {
        var result =
            CoreLibraryTestHelper.ApplyUnary(GetResultFunction("toMaybe"), OkOf(Integer(42)), s_vm);

        result.Should().Be(JustOf(Integer(42)));
    }

    [Fact]
    public void ToMaybe_Err()
    {
        var result =
            CoreLibraryTestHelper.ApplyUnary(GetResultFunction("toMaybe"), ErrOf(String("error")), s_vm);

        result.Should().Be(s_nothing);
    }

    [Fact]
    public void ToMaybe_Ok_string()
    {
        var result =
            CoreLibraryTestHelper.ApplyUnary(GetResultFunction("toMaybe"), OkOf(String("hello")), s_vm);

        result.Should().Be(JustOf(String("hello")));
    }

    [Fact]
    public void ToMaybe_Err_integer()
    {
        var result =
            CoreLibraryTestHelper.ApplyUnary(GetResultFunction("toMaybe"), ErrOf(Integer(0)), s_vm);

        result.Should().Be(s_nothing);
    }

    // ========== Tests for fromMaybe ==========
    // fromMaybe err (Just v) == Ok v
    // fromMaybe err Nothing == Err err

    [Fact]
    public void FromMaybe_Just()
    {
        var result =
            ApplyBinary(GetResultFunction("fromMaybe"), String("error"), JustOf(Integer(42)));

        result.Should().Be(OkOf(Integer(42)));
    }

    [Fact]
    public void FromMaybe_Nothing()
    {
        var result =
            ApplyBinary(GetResultFunction("fromMaybe"), String("error"), s_nothing);

        result.Should().Be(ErrOf(String("error")));
    }

    [Fact]
    public void FromMaybe_Just_string()
    {
        var result =
            ApplyBinary(GetResultFunction("fromMaybe"), Integer(0), JustOf(String("hello")));

        result.Should().Be(OkOf(String("hello")));
    }

    [Fact]
    public void FromMaybe_Nothing_integer_error()
    {
        var result =
            ApplyBinary(GetResultFunction("fromMaybe"), Integer(404), s_nothing);

        result.Should().Be(ErrOf(Integer(404)));
    }
}
