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

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue String(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue ElmList(params ElmValue[] items) =>
        ElmValue.ListInstance([.. items]);

    private static PineValue ToPine(ElmValue value) =>
        ElmValueEncoding.ElmValueAsPineValue(value);

    private static PineValue GetNegateFunction() =>
        Core.Elm.ElmCompilerInDotnet.CoreLibraryModule.CoreBasics.GetFunctionValue("negate")!;

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    private static ElmValue ApplyBinary(PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm);

    private static ElmValue ApplyWithPineArgs(PineValue functionValue, params PineValue[] pineArgs) =>
        CoreLibraryTestHelper.ApplyWithPineArgs(s_vm, functionValue, pineArgs);

    // ========== Tests for pair ==========
    // pair 3 4 == (3, 4)

    [Fact]
    public void Pair_3_4()
    {
        var result = ApplyBinary(GetTupleFunction("pair"), Integer(3), Integer(4));
        result.Should().Be(ElmList(Integer(3), Integer(4)));
    }

    // ========== Tests for first ==========
    // first (3, 4) == 3
    // first ("john", "doe") == "john"

    [Fact]
    public void First_3_4()
    {
        var result = ApplyUnary(GetTupleFunction("first"), ElmList(Integer(3), Integer(4)));
        result.Should().Be(Integer(3));
    }

    [Fact]
    public void First_john_doe()
    {
        var result = ApplyUnary(GetTupleFunction("first"), ElmList(String("john"), String("doe")));
        result.Should().Be(String("john"));
    }

    // ========== Tests for second ==========
    // second (3, 4) == 4
    // second ("john", "doe") == "doe"

    [Fact]
    public void Second_3_4()
    {
        var result = ApplyUnary(GetTupleFunction("second"), ElmList(Integer(3), Integer(4)));
        result.Should().Be(Integer(4));
    }

    [Fact]
    public void Second_john_doe()
    {
        var result = ApplyUnary(GetTupleFunction("second"), ElmList(String("john"), String("doe")));
        result.Should().Be(String("doe"));
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
                ToPine(ElmList(Integer(3), Integer(4))));

        result.Should().Be(ElmList(Integer(-3), Integer(4)));
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
                ToPine(ElmList(Integer(3), Integer(4))));

        result.Should().Be(ElmList(Integer(3), Integer(-4)));
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
                ToPine(ElmList(Integer(3), Integer(4))));

        result.Should().Be(ElmList(Integer(-3), Integer(-4)));
    }
}
