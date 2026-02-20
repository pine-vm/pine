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

public class CoreMaybeFunctionTests
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
                        b.path[^1].Equals("Maybe.elm", StringComparison.OrdinalIgnoreCase))
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

    private static PineValue GetMaybeFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Maybe")
        .moduleContent.FunctionDeclarations[name];

    private static ElmValue ApplyBinary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyGeneric(functionValue, [arg1, arg2]);

    private static ElmValue I(long i) =>
        ElmValue.Integer(i);

    private static ElmValue JustOf(ElmValue inner) =>
        ElmValue.TagInstance("Just", [inner]);

    private static readonly ElmValue Nothing =
        ElmValue.TagInstance("Nothing", []);

    private static PineValue ToPine(ElmValue value) =>
        ElmValueEncoding.ElmValueAsPineValue(value);

    private static ElmValue FromPine(PineValue value) =>
        ElmValueEncoding.PineValueAsElmValue(value, null, null)
        .Extract(err => throw new Exception("Failed decode as Elm value: " + err));

    private static ElmValue ApplyWithPineArgs(
        PineValue functionValue, params PineValue[] pineArgs) =>
        FromPine(CoreLibraryTestHelper.ApplyGenericPine(functionValue, pineArgs));

    private static PineValue GetNegateFunction() =>
        Core.Elm.ElmCompilerInDotnet.CoreLibraryModule.CoreBasics.GetFunctionValue("negate")!;

    private static PineValue GetAddFunction() =>
        Core.Elm.ElmCompilerInDotnet.CoreLibraryModule.CoreBasics.GetFunctionValue("add")!;

    // ========== Tests for withDefault ==========
    // withDefault 100 (Just 42) == 42
    // withDefault 100 Nothing == 100

    [Fact]
    public void WithDefault_Just_42()
    {
        var result =
            ApplyBinary(GetMaybeFunction("withDefault"), I(100), JustOf(I(42)));

        result.Should().Be(I(42));
    }

    [Fact]
    public void WithDefault_Nothing()
    {
        var result =
            ApplyBinary(GetMaybeFunction("withDefault"), I(100), Nothing);

        result.Should().Be(I(100));
    }

    // ========== Tests for map ==========
    // map negate (Just 9) == Just (-9)
    // map negate Nothing == Nothing

    [Fact]
    public void Map_negate_Just_9()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map"),
                GetNegateFunction(),
                ToPine(JustOf(I(9))));

        result.Should().Be(JustOf(I(-9)));
    }

    [Fact]
    public void Map_negate_Nothing()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map"),
                GetNegateFunction(),
                ToPine(Nothing));

        result.Should().Be(Nothing);
    }

    // ========== Tests for map2 ==========
    // map2 add (Just 3) (Just 4) == Just 7
    // map2 add (Just 3) Nothing == Nothing
    // map2 add Nothing (Just 4) == Nothing

    [Fact]
    public void Map2_add_Just_3_Just_4()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map2"),
                GetAddFunction(),
                ToPine(JustOf(I(3))),
                ToPine(JustOf(I(4))));

        result.Should().Be(JustOf(I(7)));
    }

    [Fact]
    public void Map2_add_Just_3_Nothing()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map2"),
                GetAddFunction(),
                ToPine(JustOf(I(3))),
                ToPine(Nothing));

        result.Should().Be(Nothing);
    }

    [Fact]
    public void Map2_add_Nothing_Just_4()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map2"),
                GetAddFunction(),
                ToPine(Nothing),
                ToPine(JustOf(I(4))));

        result.Should().Be(Nothing);
    }

    // ========== Tests for map3 ==========
    // map3: Nothing propagation

    [Fact]
    public void Map3_Nothing_first()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map3"),
                GetAddFunction(),
                ToPine(Nothing),
                ToPine(JustOf(I(2))),
                ToPine(JustOf(I(3))));

        result.Should().Be(Nothing);
    }

    [Fact]
    public void Map3_Nothing_second()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map3"),
                GetAddFunction(),
                ToPine(JustOf(I(1))),
                ToPine(Nothing),
                ToPine(JustOf(I(3))));

        result.Should().Be(Nothing);
    }

    [Fact]
    public void Map3_Nothing_third()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map3"),
                GetAddFunction(),
                ToPine(JustOf(I(1))),
                ToPine(JustOf(I(2))),
                ToPine(Nothing));

        result.Should().Be(Nothing);
    }

    // ========== Tests for map4 ==========
    // map4: Nothing propagation

    [Fact]
    public void Map4_Nothing_first()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map4"),
                GetAddFunction(),
                ToPine(Nothing),
                ToPine(JustOf(I(2))),
                ToPine(JustOf(I(3))),
                ToPine(JustOf(I(4))));

        result.Should().Be(Nothing);
    }

    [Fact]
    public void Map4_Nothing_second()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map4"),
                GetAddFunction(),
                ToPine(JustOf(I(1))),
                ToPine(Nothing),
                ToPine(JustOf(I(3))),
                ToPine(JustOf(I(4))));

        result.Should().Be(Nothing);
    }

    [Fact]
    public void Map4_Nothing_third()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map4"),
                GetAddFunction(),
                ToPine(JustOf(I(1))),
                ToPine(JustOf(I(2))),
                ToPine(Nothing),
                ToPine(JustOf(I(4))));

        result.Should().Be(Nothing);
    }

    [Fact]
    public void Map4_Nothing_fourth()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map4"),
                GetAddFunction(),
                ToPine(JustOf(I(1))),
                ToPine(JustOf(I(2))),
                ToPine(JustOf(I(3))),
                ToPine(Nothing));

        result.Should().Be(Nothing);
    }

    // ========== Tests for map5 ==========
    // map5: Nothing propagation

    [Fact]
    public void Map5_Nothing_first()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map5"),
                GetAddFunction(),
                ToPine(Nothing),
                ToPine(JustOf(I(2))),
                ToPine(JustOf(I(3))),
                ToPine(JustOf(I(4))),
                ToPine(JustOf(I(5))));

        result.Should().Be(Nothing);
    }

    [Fact]
    public void Map5_Nothing_third()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map5"),
                GetAddFunction(),
                ToPine(JustOf(I(1))),
                ToPine(JustOf(I(2))),
                ToPine(Nothing),
                ToPine(JustOf(I(4))),
                ToPine(JustOf(I(5))));

        result.Should().Be(Nothing);
    }

    [Fact]
    public void Map5_Nothing_fifth()
    {
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("map5"),
                GetAddFunction(),
                ToPine(JustOf(I(1))),
                ToPine(JustOf(I(2))),
                ToPine(JustOf(I(3))),
                ToPine(JustOf(I(4))),
                ToPine(Nothing));

        result.Should().Be(Nothing);
    }

    // ========== Tests for andThen ==========
    // andThen with negate wrapped in Just: andThen (\x -> Just (negate x)) (Just 5) == Just (-5)
    // andThen on Nothing returns Nothing

    [Fact]
    public void AndThen_Nothing()
    {
        // Any function applied via andThen to Nothing should return Nothing.
        var result =
            ApplyWithPineArgs(
                GetMaybeFunction("andThen"),
                GetNegateFunction(),
                ToPine(Nothing));

        result.Should().Be(Nothing);
    }
}
