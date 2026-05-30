using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Regression test for the bug described in
/// <c>explore/internal-analysis/2026-05-18-eliminate-higher-order-parameters-in-focused-tests.md</c>
/// §7.0: <see cref="WrapperReturnStripping"/> emits stripped sibling
/// declarations whose bodies contain unqualified
/// <c>FunctionOrValue</c> references to other top-level decls in the
/// same module (typically the original decl's sibling lifted lambda).
///
/// <para>
/// <see cref="SnapshotTestFormat.RenderQualifiedDeclarations"/> rejects
/// any non-local unqualified reference with
/// <c>"contains unqualified reference '…' that is not a local
/// binding"</c>, so the snapshot harness used by neighbouring tests
/// (and by <see cref="DeclarationDeduplication.GetStructuralFingerprint"/>)
/// throws on the fixture-E module. This test pins the fix so the
/// harness keeps working.
/// </para>
/// </summary>
public class WrapperReturnStrippingQualificationRegressionTests
{
    /// <summary>
    /// Fixture-E module text from
    /// <see cref="WholeProgramMonomorphizationFocusedTests"/>: two
    /// mutually recursive <c>Parser Int</c> producers wired through a
    /// <c>lazy</c> combinator. Both <c>lazy</c> and <c>oneOf2</c> get
    /// their inner lambdas lifted; the strip planner then synthesises
    /// <c>lazy__stripped</c> / <c>oneOf2__stripped</c> whose bodies
    /// reference the lifted lambdas in the same <c>Test</c> module.
    /// </summary>
    private const string ParserNewtypeWrapperReproModuleText =
        """
        module Test exposing (entry)


        type Parser a = Parser (Int -> a)


        lazy : (() -> Parser a) -> Parser a
        lazy thunk =
            Parser
                (\s ->
                    let
                        (Parser parse) =
                            thunk ()
                    in
                    parse s
                )


        oneOf2 : Parser Int -> Parser Int -> Parser Int
        oneOf2 (Parser pA) (Parser pB) =
            Parser
                (\s ->
                    let
                        a =
                            pA s
                    in
                    if Pine_kernel.equal [ a, 0 ] then
                        pB s

                    else
                        a
                )


        litP : Int -> Parser Int
        litP k =
            Parser (\s -> Pine_kernel.int_add [ k, s ])


        rec1 : () -> Parser Int
        rec1 () =
            oneOf2 (litP 1) (lazy rec2)


        rec2 : () -> Parser Int
        rec2 () =
            oneOf2 (litP 2) (lazy rec1)


        entry : Int -> Int
        entry n =
            let
                (Parser fn) =
                    lazy rec1
            in
            fn n
        """;

    [Fact]
    public void Rendering_Test_module_decls_after_optimization_does_not_throw_on_unqualified_lifted_lambda_reference()
    {
        var postLoweringDecls = CompileAndGetPostLoweringDecls(ParserNewtypeWrapperReproModuleText);

        var testModuleDecls =
            postLoweringDecls
            .Where(kvp => kvp.Key.Namespaces.SequenceEqual(["Test"]))
            .ToImmutableDictionary(kvp => kvp.Key, kvp => kvp.Value);

        // Sanity: the fixture must actually trigger WrapperReturnStripping.
        testModuleDecls.Keys
            .Any(k => k.DeclName.EndsWith(WrapperReturnStripping.StrippedSuffix, StringComparison.Ordinal))
            .Should().BeTrue(
            "the fixture is meant to exercise WrapperReturnStripping; "
            + "no `__stripped` sibling was emitted, so the regression "
            + "coverage is gone.");

        // Pre-fix behaviour: SnapshotTestFormat.RenderQualifiedDeclarations
        // throws InvalidOperationException with
        //   "Declaration 'Test.lazy__stripped' contains unqualified
        //    reference 'lazy__lifted__lambda1' that is not a local
        //    binding."
        var renderAction =
            () =>
            SnapshotTestFormat.RenderQualifiedDeclarations(
                testModuleDecls,
                SnapshotTestFormat.DeclarationSortOrder.NameAsc);

        renderAction.Should().NotThrow(
            "WrapperReturnStripping must qualify same-module top-level "
            + "references when emitting stripped-sibling bodies.");
    }

    private static ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        CompileAndGetPostLoweringDecls(string moduleText)
    {
        var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

        const string FileName = "Test.elm";

        var treeWithTest =
            kernelModulesTree.SetNodeAtPathSorted(
                [FileName],
                FileTree.File(Encoding.UTF8.GetBytes(moduleText)));

        var rootFilePaths =
            treeWithTest.EnumerateFilesTransitive()
            .Where(f => f.path[^1].Equals(FileName, StringComparison.OrdinalIgnoreCase))
            .Select(f => (IReadOnlyList<string>)f.path)
            .ToList();

        var compileResult =
            ElmCompiler.CompileInteractiveEnvironment(
                treeWithTest,
                rootFilePaths: rootFilePaths)
            .Extract(err => throw new Exception("Failed compiling: " + err));

        return
            ElmCompiler.FlattenModulesToDeclarationDictionary(
                compileResult.pipelineStageResults.ModulesForCompilation);
    }
}
