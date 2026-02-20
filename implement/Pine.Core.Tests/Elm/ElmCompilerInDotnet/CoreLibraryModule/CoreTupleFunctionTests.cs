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

public class CoreTupleFunctionTests
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
                        b.path[^1].Equals("Tuple.elm", StringComparison.OrdinalIgnoreCase))
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

    private static PineValue GetTupleFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Tuple")
        .moduleContent.FunctionDeclarations[name];

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyDirectUnary(functionValue, argument);

    private static ElmValue ApplyBinary(
        PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyGeneric(functionValue, [arg1, arg2]);

    private static ElmValue S(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue I(long i) =>
        ElmValue.Integer(i);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

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

    // ========== Tests for pair ==========
    // pair 3 4 == (3, 4)

    [Fact]
    public void Pair_3_4()
    {
        var result = ApplyBinary(GetTupleFunction("pair"), I(3), I(4));
        result.Should().Be(ElmList(I(3), I(4)));
    }

    // ========== Tests for first ==========
    // first (3, 4) == 3
    // first ("john", "doe") == "john"

    [Fact]
    public void First_3_4()
    {
        var result = ApplyUnary(GetTupleFunction("first"), ElmList(I(3), I(4)));
        result.Should().Be(I(3));
    }

    [Fact]
    public void First_john_doe()
    {
        var result = ApplyUnary(GetTupleFunction("first"), ElmList(S("john"), S("doe")));
        result.Should().Be(S("john"));
    }

    // ========== Tests for second ==========
    // second (3, 4) == 4
    // second ("john", "doe") == "doe"

    [Fact]
    public void Second_3_4()
    {
        var result = ApplyUnary(GetTupleFunction("second"), ElmList(I(3), I(4)));
        result.Should().Be(I(4));
    }

    [Fact]
    public void Second_john_doe()
    {
        var result = ApplyUnary(GetTupleFunction("second"), ElmList(S("john"), S("doe")));
        result.Should().Be(S("doe"));
    }

    // ========== Tests for mapFirst ==========
    // mapFirst negate (3, 4) == (-3, 4)

    [Fact]
    public void MapFirst_negate()
    {
        var negateFn = GetNegateFunction();

        var result =
            ApplyWithPineArgs(
                GetTupleFunction("mapFirst"),
                negateFn,
                ToPine(ElmList(I(3), I(4))));

        result.Should().Be(ElmList(I(-3), I(4)));
    }

    // ========== Tests for mapSecond ==========
    // mapSecond negate (3, 4) == (3, -4)

    [Fact]
    public void MapSecond_negate()
    {
        var negateFn = GetNegateFunction();

        var result =
            ApplyWithPineArgs(
                GetTupleFunction("mapSecond"),
                negateFn,
                ToPine(ElmList(I(3), I(4))));

        result.Should().Be(ElmList(I(3), I(-4)));
    }

    // ========== Tests for mapBoth ==========
    // mapBoth negate negate (3, 4) == (-3, -4)

    [Fact]
    public void MapBoth_negate_negate()
    {
        var negateFn = GetNegateFunction();

        var result =
            ApplyWithPineArgs(
                GetTupleFunction("mapBoth"),
                negateFn,
                negateFn,
                ToPine(ElmList(I(3), I(4))));

        result.Should().Be(ElmList(I(-3), I(-4)));
    }
}
