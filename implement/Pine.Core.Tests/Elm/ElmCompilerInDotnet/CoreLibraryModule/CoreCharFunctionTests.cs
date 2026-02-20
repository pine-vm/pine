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

public class CoreCharFunctionTests
{
    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_kernelEnv =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value
                    .GetNodeAtPath(["elm-kernel-modules"])
                    ?? throw new Exception("Did not find elm-kernel-modules");

                // Only Char.elm is the root; its transitive dependencies will be resolved automatically.
                var rootFilePaths =
                    kernelModulesTree.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("Char.elm", StringComparison.OrdinalIgnoreCase))
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

    private static PineValue GetCharFunction(string name) =>
        s_kernelEnv.Value.Modules
        .First(m => m.moduleName is "Char")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ApplyUnary(PineValue functionValue, ElmValue argument) =>
        CoreLibraryTestHelper.ApplyUnary(functionValue, argument, s_vm);

    // ========== Tests for toCode ==========

    [Fact]
    public void ToCode_A()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toCode"),
                ElmValue.CharInstance('A'));

        result.Should().Be(ElmValue.Integer(65));
    }

    [Fact]
    public void ToCode_a()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toCode"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.Integer(97));
    }

    [Fact]
    public void ToCode_zero()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toCode"),
                ElmValue.CharInstance('0'));

        result.Should().Be(ElmValue.Integer(48));
    }

    [Fact]
    public void ToCode_space()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toCode"),
                ElmValue.CharInstance(' '));

        result.Should().Be(ElmValue.Integer(32));
    }

    // ========== Tests for fromCode ==========

    [Fact]
    public void FromCode_65()
    {
        var result =
            ApplyUnary(
                GetCharFunction("fromCode"),
                ElmValue.Integer(65));

        result.Should().Be(ElmValue.CharInstance('A'));
    }

    [Fact]
    public void FromCode_97()
    {
        var result =
            ApplyUnary(
                GetCharFunction("fromCode"),
                ElmValue.Integer(97));

        result.Should().Be(ElmValue.CharInstance('a'));
    }

    [Fact]
    public void FromCode_48()
    {
        var result =
            ApplyUnary(
                GetCharFunction("fromCode"),
                ElmValue.Integer(48));

        result.Should().Be(ElmValue.CharInstance('0'));
    }

    // ========== Tests for isDigit ==========

    [Fact]
    public void IsDigit_0_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isDigit"),
                ElmValue.CharInstance('0'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsDigit_9_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isDigit"),
                ElmValue.CharInstance('9'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsDigit_a_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isDigit"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void IsDigit_A_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isDigit"),
                ElmValue.CharInstance('A'));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for isOctDigit ==========

    [Fact]
    public void IsOctDigit_0_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isOctDigit"),
                ElmValue.CharInstance('0'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsOctDigit_7_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isOctDigit"),
                ElmValue.CharInstance('7'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsOctDigit_8_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isOctDigit"),
                ElmValue.CharInstance('8'));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void IsOctDigit_a_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isOctDigit"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for isHexDigit ==========

    [Fact]
    public void IsHexDigit_0_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isHexDigit"),
                ElmValue.CharInstance('0'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsHexDigit_9_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isHexDigit"),
                ElmValue.CharInstance('9'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsHexDigit_a_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isHexDigit"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsHexDigit_f_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isHexDigit"),
                ElmValue.CharInstance('f'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsHexDigit_A_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isHexDigit"),
                ElmValue.CharInstance('A'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsHexDigit_F_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isHexDigit"),
                ElmValue.CharInstance('F'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsHexDigit_g_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isHexDigit"),
                ElmValue.CharInstance('g'));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void IsHexDigit_G_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isHexDigit"),
                ElmValue.CharInstance('G'));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for isUpper ==========

    [Fact]
    public void IsUpper_A_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isUpper"),
                ElmValue.CharInstance('A'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsUpper_Z_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isUpper"),
                ElmValue.CharInstance('Z'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsUpper_a_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isUpper"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void IsUpper_digit_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isUpper"),
                ElmValue.CharInstance('5'));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for isLower ==========

    [Fact]
    public void IsLower_a_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isLower"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsLower_z_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isLower"),
                ElmValue.CharInstance('z'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsLower_A_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isLower"),
                ElmValue.CharInstance('A'));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void IsLower_digit_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isLower"),
                ElmValue.CharInstance('5'));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for isAlpha ==========

    [Fact]
    public void IsAlpha_a_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isAlpha"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsAlpha_A_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isAlpha"),
                ElmValue.CharInstance('A'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsAlpha_digit_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isAlpha"),
                ElmValue.CharInstance('5'));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void IsAlpha_space_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isAlpha"),
                ElmValue.CharInstance(' '));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for isAlphaNum ==========

    [Fact]
    public void IsAlphaNum_a_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isAlphaNum"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsAlphaNum_A_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isAlphaNum"),
                ElmValue.CharInstance('A'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsAlphaNum_digit_is_true()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isAlphaNum"),
                ElmValue.CharInstance('5'));

        result.Should().Be(ElmValue.TrueValue);
    }

    [Fact]
    public void IsAlphaNum_space_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isAlphaNum"),
                ElmValue.CharInstance(' '));

        result.Should().Be(ElmValue.FalseValue);
    }

    [Fact]
    public void IsAlphaNum_hyphen_is_false()
    {
        var result =
            ApplyUnary(
                GetCharFunction("isAlphaNum"),
                ElmValue.CharInstance('-'));

        result.Should().Be(ElmValue.FalseValue);
    }

    // ========== Tests for toUpper ==========

    [Fact]
    public void ToUpper_a()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toUpper"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.CharInstance('A'));
    }

    [Fact]
    public void ToUpper_z()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toUpper"),
                ElmValue.CharInstance('z'));

        result.Should().Be(ElmValue.CharInstance('Z'));
    }

    [Fact]
    public void ToUpper_already_upper()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toUpper"),
                ElmValue.CharInstance('A'));

        result.Should().Be(ElmValue.CharInstance('A'));
    }

    [Fact]
    public void ToUpper_digit_unchanged()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toUpper"),
                ElmValue.CharInstance('5'));

        result.Should().Be(ElmValue.CharInstance('5'));
    }

    // ========== Tests for toLower ==========

    [Fact]
    public void ToLower_A()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toLower"),
                ElmValue.CharInstance('A'));

        result.Should().Be(ElmValue.CharInstance('a'));
    }

    [Fact]
    public void ToLower_Z()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toLower"),
                ElmValue.CharInstance('Z'));

        result.Should().Be(ElmValue.CharInstance('z'));
    }

    [Fact]
    public void ToLower_already_lower()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toLower"),
                ElmValue.CharInstance('a'));

        result.Should().Be(ElmValue.CharInstance('a'));
    }

    [Fact]
    public void ToLower_digit_unchanged()
    {
        var result =
            ApplyUnary(
                GetCharFunction("toLower"),
                ElmValue.CharInstance('5'));

        result.Should().Be(ElmValue.CharInstance('5'));
    }
}
