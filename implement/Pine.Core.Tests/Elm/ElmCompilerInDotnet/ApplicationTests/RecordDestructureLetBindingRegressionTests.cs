using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Minimal isolated regression tests for a bug in
/// <see cref="ElmCompiler.CompileInteractiveEnvironment"/>'s
/// record-pattern destructuring of let bindings.
/// <para>
/// Bisected from
/// <see cref="LanguageServiceProvideHoverShrinkingTests"/>: the lang-server
/// migration surfaced that
/// <c>let { hoverItems } = hoverItemsFromParsedModule ... in hoverItems</c>
/// returns the value of <c>fromDeclarations</c> (a sibling record field)
/// instead of <c>hoverItems</c>. The probes there proved that direct
/// dot-access (<c>record.hoverItems</c>) returns the correct value, so
/// the corruption is specifically in the record-pattern <c>let</c>
/// destructure.
/// </para>
/// <para>
/// These tests reproduce the same defect from a single hand-written Elm
/// module, with no LanguageService dependency, so the compiler bug is
/// pinned with a tight diff.
/// </para>
/// </summary>
public class RecordDestructureLetBindingRegressionTests
{
    private static PineValue ApplyZero(string moduleText, string functionName)
    {
        var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

        var tree =
            kernelModulesTree.SetNodeAtPathSorted(
                ["M.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(moduleText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                tree,
                rootFilePaths: [["M.elm"]],
                disableInlining: true,
                maxOptimizationRounds: 1)
            .Map(r => r.compiledEnvValue)
            .Extract(err => throw new Exception("Failed compiling: " + err));

        var env =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing: " + err));

        var func =
            env.Modules.First(m => m.moduleName is "M")
            .moduleContent.FunctionDeclarations[functionName];

        var fr =
            FunctionRecord.ParseFunctionRecordTagged(func, new PineVMParseCache())
            .Extract(err => throw new Exception(err.ToString()));

        var ev =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                fr,
                [ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(0))])
            .Extract(err => throw new Exception(err.ToString()));

        var vm =
            ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        return
            vm.EvaluateExpressionOnCustomStack(
                ev.expression,
                ev.environment,
                config: ElmCompilerTestHelper.DefaultTestEvaluationConfig)
            .Extract(err => throw new Exception("Failed eval: " + err))
            .ReturnValue.Evaluate();
    }

    private static string Render(PineValue v) =>
        ElmValue.RenderAsElmExpression(
            ElmValueEncoding.PineValueAsElmValue(v, null, null)
            .Extract(err => throw new Exception(err)))
        .expressionString;

    /// <summary>
    /// Minimal reproducer: a record with two fields, the second of which
    /// is destructured via a <c>let { secondField } = record</c> pattern.
    /// The destructure binds the name <c>secondField</c> to the value of
    /// the FIRST field instead of the named one.
    /// </summary>
    [Fact]
    public void Record_pattern_let_picks_named_field_not_first_field()
    {
        const string moduleText =
            """
            module M exposing (..)


            buildRecord : Int -> { fromDeclarations : List Int, hoverItems : List String }
            buildRecord _ =
                { fromDeclarations = [ 1, 2, 3 ]
                , hoverItems = [ "a", "b" ]
                }


            extractHoverItems : Int -> List String
            extractHoverItems _ =
                let
                    { hoverItems } =
                        buildRecord 0
                in
                hoverItems
            """;

        var rendered = Render(ApplyZero(moduleText, "extractHoverItems"));

        rendered.Should().Be(
            """[ "a", "b" ]""",
            because:
                "`let { hoverItems } = record in hoverItems` must bind the " +
                "name `hoverItems` to the value of the field named " +
                "`hoverItems`, not to the value of the first field of the " +
                "record. Got: " + rendered);
    }

    /// <summary>
    /// Same shape as
    /// <see cref="Record_pattern_let_picks_named_field_not_first_field"/>
    /// but using dot access. This control case must always pass; together
    /// with the failing companion it pins the defect to the record-pattern
    /// destructure rather than to record construction or field lookup.
    /// </summary>
    [Fact]
    public void Record_dot_access_returns_named_field()
    {
        const string moduleText =
            """
            module M exposing (..)


            buildRecord : Int -> { fromDeclarations : List Int, hoverItems : List String }
            buildRecord _ =
                { fromDeclarations = [ 1, 2, 3 ]
                , hoverItems = [ "a", "b" ]
                }


            extractHoverItems : Int -> List String
            extractHoverItems _ =
                (buildRecord 0).hoverItems
            """;

        var rendered = Render(ApplyZero(moduleText, "extractHoverItems"));

        rendered.Should().Be("""[ "a", "b" ]""");
    }
}
