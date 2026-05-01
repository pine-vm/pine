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

public class CoreTimeFunctionTests
{
    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_kernelEnv =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.ElmKernelModulesDefault.Value;

                var rootFilePaths =
                    kernelModulesTree.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("Time.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        kernelModulesTree,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Map(r => r.compiledEnvValue)
                    .Extract(err => throw new Exception("Failed compiling elm-kernel-modules: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing interactive environment: " + err));
            });

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        // Shared like the other CoreLibraryModule test VMs; the tests assert return values only
        // and do not inspect profiling state.
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static PineValue GetTimeFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Time")
        .moduleContent.FunctionDeclarations[name];

    private static string ApplyUnaryAsExpression(PineValue functionValue, ElmValue argument) =>
        ElmValue.RenderAsElmExpression(
            CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm))
        .expressionString;

    private static string ApplyBinaryAsExpression(PineValue functionValue, ElmValue arg1, ElmValue arg2) =>
        ElmValue.RenderAsElmExpression(
            CoreLibraryTestHelper.ApplyBinary(functionValue, arg1, arg2, s_vm))
        .expressionString;

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue Posix(long millis) =>
        ElmValue.TagInstance("Posix", [Integer(millis)]);

    private static ElmValue Utc() =>
        ElmValue.TagInstance("Zone", [Integer(0), ElmValue.ListInstance([])]);

    [Fact]
    public void To_adjusted_minutes_for_utc_unix_epoch()
    {
        ApplyBinaryAsExpression(GetTimeFunction("toAdjustedMinutes"), Utc(), Posix(0))
            .Should().Be("0");
    }

    [Fact]
    public void To_adjusted_minutes_for_utc_after_one_day()
    {
        ApplyBinaryAsExpression(GetTimeFunction("toAdjustedMinutes"), Utc(), Posix(24 * 60 * 60 * 1000))
            .Should().Be("1440");
    }

    [Fact]
    public void To_civil_for_unix_epoch()
    {
        ApplyUnaryAsExpression(GetTimeFunction("toCivil"), Integer(0))
            .Should().Be("{ day = 1, month = 1, year = 1970 }");
    }

    [Fact]
    public void To_civil_for_leap_day_2020()
    {
        ApplyUnaryAsExpression(GetTimeFunction("toCivil"), Integer(26_382_240))
            .Should().Be("{ day = 29, month = 2, year = 2020 }");
    }

    [Fact]
    public void To_civil_for_day_before_unix_epoch()
    {
        ApplyUnaryAsExpression(GetTimeFunction("toCivil"), Integer(-1_440))
            .Should().Be("{ day = 31, month = 12, year = 1969 }");
    }

    [Fact]
    public void To_year_uses_to_adjusted_minutes_and_to_civil()
    {
        ApplyBinaryAsExpression(GetTimeFunction("toYear"), Utc(), Posix(1_582_934_400_000))
            .Should().Be("2020");
    }

    [Fact]
    public void To_month_uses_to_adjusted_minutes_and_to_civil()
    {
        ApplyBinaryAsExpression(GetTimeFunction("toMonth"), Utc(), Posix(1_582_934_400_000))
            .Should().Be("Feb");
    }
}
