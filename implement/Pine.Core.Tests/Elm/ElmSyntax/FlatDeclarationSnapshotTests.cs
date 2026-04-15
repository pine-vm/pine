using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax;

/// <summary>
/// Tests for the flat declaration dictionary rendering used in snapshot tests.
/// Verifies that individual declarations can be rendered with qualified names,
/// that modules can be converted to a flat dictionary, and that declarations
/// can be filtered by module name.
/// </summary>
public class FlatDeclarationSnapshotTests
{
    [Fact]
    public void Render_function_declaration_with_qualified_name()
    {
        var moduleText =
            """
            module MyApp exposing (..)


            declName : Int -> Int
            declName a =
                a + 13
            """;

        var (_, pipelineStageResults) =
            ElmCompilerTestHelper.CompileElmModules(
                [moduleText],
                disableInlining: true);

        var flatDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.Canonicalized);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclarations(flatDict, SnapshotTestFormat.DeclarationSortOrder.NameAsc);

        rendered.Should().Be(
            """
            MyApp.declName : Basics.Int -> Basics.Int
            MyApp.declName a =
                Basics.add
                    a
                    13
            """);
    }

    [Fact]
    public void Render_function_without_signature_uses_qualified_name()
    {
        var moduleText =
            """
            module Helpers exposing (..)


            add x y =
                x + y
            """;

        var (_, pipelineStageResults) =
            ElmCompilerTestHelper.CompileElmModules(
                [moduleText],
                disableInlining: true);

        var flatDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.Canonicalized);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclarations(flatDict, SnapshotTestFormat.DeclarationSortOrder.NameAsc);

        rendered.Should().Be(
            """
            Helpers.add x y =
                Basics.add
                    x
                    y
            """);
    }

    [Fact]
    public void ModulesToFlatDeclarationDictionary_includes_all_declarations()
    {
        var moduleA =
            """
            module ModuleA exposing (..)


            helper : Int -> Int
            helper x =
                x + 1


            identity : a -> a
            identity x =
                x
            """;

        var moduleB =
            """
            module ModuleB exposing (..)

            import ModuleA


            combined : Int -> Int
            combined x =
                ModuleA.helper x
            """;

        var (_, pipelineStageResults) =
            ElmCompilerTestHelper.CompileElmModules(
                [moduleA, moduleB],
                disableInlining: true);

        var flatDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.Canonicalized);

        // Should contain declarations from both modules
        flatDict.Should().ContainKey(DeclQualifiedName.FromString("ModuleA.helper"));
        flatDict.Should().ContainKey(DeclQualifiedName.FromString("ModuleA.identity"));
        flatDict.Should().ContainKey(DeclQualifiedName.FromString("ModuleB.combined"));
    }

    [Fact]
    public void FilterDeclarationsByModuleName_returns_matching_declarations()
    {
        var moduleA =
            """
            module ModuleA exposing (..)


            helper : Int -> Int
            helper x =
                x + 1
            """;

        var moduleB =
            """
            module ModuleB exposing (..)

            import ModuleA


            combined : Int -> Int
            combined x =
                ModuleA.helper x
            """;

        var (_, pipelineStageResults) =
            ElmCompilerTestHelper.CompileElmModules(
                [moduleA, moduleB],
                disableInlining: true);

        var flatDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.Canonicalized);

        var moduleADecls =
            SnapshotTestFormat.FilterDeclarationsByModuleName(flatDict, ["ModuleA"]);

        moduleADecls.Should().HaveCount(1);
        moduleADecls.Should().ContainKey(DeclQualifiedName.FromString("ModuleA.helper"));

        var moduleBDecls =
            SnapshotTestFormat.FilterDeclarationsByModuleName(flatDict, ["ModuleB"]);

        moduleBDecls.Should().HaveCount(1);
        moduleBDecls.Should().ContainKey(DeclQualifiedName.FromString("ModuleB.combined"));
    }

    [Fact]
    public void Render_multiple_declarations_separated_by_blank_lines()
    {
        var moduleText =
            """
            module MyModule exposing (..)


            first : Int -> Int
            first x =
                x + 1


            second : Int -> Int
            second x =
                x + 2
            """;

        var (_, pipelineStageResults) =
            ElmCompilerTestHelper.CompileElmModules(
                [moduleText],
                disableInlining: true);

        var flatDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.Canonicalized);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclarations(flatDict, SnapshotTestFormat.DeclarationSortOrder.NameAsc);

        rendered.Should().Be(
            """
            MyModule.first : Basics.Int -> Basics.Int
            MyModule.first x =
                Basics.add
                    x
                    1


            MyModule.second : Basics.Int -> Basics.Int
            MyModule.second x =
                Basics.add
                    x
                    2
            """);
    }

    [Fact]
    public void Multi_module_pipeline_flat_declaration_snapshot()
    {
        // This test mirrors PipelineStageSnapshotTests.Two_module_pipeline_with_cross_module_call
        // but uses the flat declaration dictionary format instead of module-based snapshots.

        var moduleA =
            """
            module ModuleA exposing (..)


            helper : Int -> Int
            helper x =
                x + 1
            """;

        var moduleB =
            """
            module ModuleB exposing (..)

            import ModuleA


            combined : Int -> Int
            combined x =
                ModuleA.helper x
            """;

        var (_, pipelineStageResults) =
            ElmCompilerTestHelper.CompileElmModules(
                [moduleA, moduleB],
                disableInlining: false);

        // --- Assert canonicalized stage using flat declarations ---
        var canonicalizedDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.Canonicalized);

        var canonicalizedModuleA =
            SnapshotTestFormat.FilterDeclarationsByModuleName(canonicalizedDict, ["ModuleA"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(
            canonicalizedModuleA,
            SnapshotTestFormat.DeclarationSortOrder.NameAsc)
            .Should().Be(
            """
            ModuleA.helper : Basics.Int -> Basics.Int
            ModuleA.helper x =
                Basics.add
                    x
                    1
            """);

        var canonicalizedModuleB =
            SnapshotTestFormat.FilterDeclarationsByModuleName(canonicalizedDict, ["ModuleB"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(
            canonicalizedModuleB,
            SnapshotTestFormat.DeclarationSortOrder.NameAsc)
            .Should().Be(
            """
            ModuleB.combined : Basics.Int -> Basics.Int
            ModuleB.combined x =
                ModuleA.helper
                    x
            """);

        // --- Assert final stage (after operator lowering) using flat declarations ---
        var finalDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.ModulesForCompilation);

        var finalModuleA =
            SnapshotTestFormat.FilterDeclarationsByModuleName(finalDict, ["ModuleA"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(finalModuleA, SnapshotTestFormat.DeclarationSortOrder.NameAsc)
            .Should().Be(
            """
            ModuleA.helper : Basics.Int -> Basics.Int
            ModuleA.helper x =
                Pine_builtin.int_add
                    [ x, 1 ]
            """);

        var finalModuleB =
            SnapshotTestFormat.FilterDeclarationsByModuleName(finalDict, ["ModuleB"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(finalModuleB, SnapshotTestFormat.DeclarationSortOrder.NameAsc)
            .Should().Be(
            """
            ModuleB.combined : Basics.Int -> Basics.Int
            ModuleB.combined x =
                Pine_builtin.int_add
                    [ x, 1 ]
            """);
    }
}
