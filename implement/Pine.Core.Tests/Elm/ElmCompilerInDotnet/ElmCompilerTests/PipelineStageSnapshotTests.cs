using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests that verify the intermediate results of each compilation pipeline stage
/// are correctly captured and can be rendered back to Elm source text.
/// These tests compile multiple Elm modules and assert declaration text snapshots
/// for the canonicalized, specialized, inlined, and final (operator-lowered) stages.
/// </summary>
public class PipelineStageSnapshotTests
{
    [Fact]
    public void Multi_module_pipeline_produces_expected_intermediate_snapshots()
    {
        // Two modules: a provider with a higher-order function and a consumer that calls it.
        // This exercises canonicalization (name resolution across modules),
        // specialization (creating specialized variants), and
        // inlining (substituting the specialized function bodies).

        var providerModule =
            """
            module Provider exposing (..)


            applyTwice : (a -> a) -> a -> a
            applyTwice fn value =
                fn (fn value)
            """;

        var consumerModule =
            """
            module Consumer exposing (..)

            import Provider


            increment : Int -> Int
            increment value =
                value + 1


            result : Int -> Int
            result n =
                Provider.applyTwice increment n
            """;

        var (_, pipelineStageResults) =
            ElmCompilerTestHelper.CompileElmModules(
                [providerModule, consumerModule],
                disableInlining: false);

        // --- Assert canonicalized stage ---
        var canonicalizedDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.Canonicalized);

        var canonicalizedProvider =
            SnapshotTestFormat.FilterDeclarationsByModuleName(canonicalizedDict, ["Provider"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(canonicalizedProvider, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            Provider.applyTwice : (a -> a) -> a -> a
            Provider.applyTwice fn value =
                fn
                    (fn
                        value
                    )
            """);

        var canonicalizedConsumer =
            SnapshotTestFormat.FilterDeclarationsByModuleName(canonicalizedDict, ["Consumer"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(canonicalizedConsumer, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            Consumer.increment : Basics.Int -> Basics.Int
            Consumer.increment value =
                Basics.add
                    value
                    1


            Consumer.result : Basics.Int -> Basics.Int
            Consumer.result n =
                Provider.applyTwice
                    Consumer.increment
                    n
            """);

        // --- Assert specialized stage ---
        var specializedProvider =
            SnapshotTestFormat.FilterDeclarationsByModuleName(pipelineStageResults.Specialized!, ["Provider"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(specializedProvider, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            Provider.applyTwice : (a -> a) -> a -> a
            Provider.applyTwice fn value =
                fn
                    (fn
                        value
                    )
            """);

        var specializedConsumer =
            SnapshotTestFormat.FilterDeclarationsByModuleName(pipelineStageResults.Specialized!, ["Consumer"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(specializedConsumer, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            Consumer.increment : Basics.Int -> Basics.Int
            Consumer.increment value =
                Basics.add
                    value
                    1


            Consumer.result : Basics.Int -> Basics.Int
            Consumer.result n =
                Provider.applyTwice
                    Consumer.increment
                    n
            """);

        // --- Assert inlined stage ---
        var inlinedProvider =
            SnapshotTestFormat.FilterDeclarationsByModuleName(pipelineStageResults.Inlined!, ["Provider"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(inlinedProvider, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            Provider.applyTwice : (a -> a) -> a -> a
            Provider.applyTwice fn value =
                fn
                    (fn
                        value
                    )
            """);

        // After inlining, the Consumer module's 'result' function has the body
        // of applyTwice inlined: instead of calling Provider.applyTwice, the body
        // is expanded to two nested calls to Consumer.increment.
        var inlinedConsumer =
            SnapshotTestFormat.FilterDeclarationsByModuleName(pipelineStageResults.Inlined!, ["Consumer"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(inlinedConsumer, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            Consumer.increment : Basics.Int -> Basics.Int
            Consumer.increment value =
                Basics.add
                    value
                    1


            Consumer.result : Basics.Int -> Basics.Int
            Consumer.result n =
                Consumer.increment
                    (Consumer.increment
                        n
                    )
            """);

        // --- Assert ModulesForCompilation (final stage after operator lowering) ---
        var finalDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.ModulesForCompilation);

        var finalProvider =
            SnapshotTestFormat.FilterDeclarationsByModuleName(finalDict, ["Provider"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(finalProvider, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            Provider.applyTwice : (a -> a) -> a -> a
            Provider.applyTwice fn value =
                fn
                    (fn
                        value
                    )
            """);

        // After operator lowering, Basics.add is replaced with Pine_builtin.int_add.
        var finalConsumer =
            SnapshotTestFormat.FilterDeclarationsByModuleName(finalDict, ["Consumer"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(finalConsumer, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            Consumer.increment : Basics.Int -> Basics.Int
            Consumer.increment value =
                Pine_builtin.int_add
                    [ value, 1 ]


            Consumer.result : Basics.Int -> Basics.Int
            Consumer.result n =
                Pine_builtin.int_add
                    [ (Consumer.increment
                        n
                      ), 1 ]
            """);
    }

    [Fact]
    public void Pipeline_stages_are_null_when_inlining_disabled()
    {
        var moduleText =
            """
            module TestModule exposing (..)


            identity : a -> a
            identity x =
                x
            """;

        var (_, pipelineStageResults) =
            ElmCompilerTestHelper.CompileElmModules(
                [moduleText],
                disableInlining: true);

        // When inlining is disabled, optimization pipeline stages should be null
        pipelineStageResults.Specialized.Should().BeNull();
        pipelineStageResults.Inlined.Should().BeNull();

        // But canonicalized and lambda-lifted should still be present
        pipelineStageResults.Canonicalized.Should().NotBeEmpty();
        pipelineStageResults.LambdaLifted.Should().NotBeEmpty();

        // ModulesForCompilation should equal LambdaLifted when inlining is disabled
        pipelineStageResults.ModulesForCompilation.Should()
            .HaveCount(pipelineStageResults.LambdaLifted.Count);
    }

    [Fact]
    public void Two_module_pipeline_with_cross_module_call()
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
                disableInlining: false);

        // --- Assert canonicalized stage ---
        var canonicalizedDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.Canonicalized);

        var canonicalizedModuleA =
            SnapshotTestFormat.FilterDeclarationsByModuleName(canonicalizedDict, ["ModuleA"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(canonicalizedModuleA, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleA.helper : Basics.Int -> Basics.Int
            ModuleA.helper x =
                Basics.add
                    x
                    1
            """);

        var canonicalizedModuleB =
            SnapshotTestFormat.FilterDeclarationsByModuleName(canonicalizedDict, ["ModuleB"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(canonicalizedModuleB, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleB.combined : Basics.Int -> Basics.Int
            ModuleB.combined x =
                ModuleA.helper
                    x
            """);

        // --- Assert specialized stage ---
        var specializedModuleA =
            SnapshotTestFormat.FilterDeclarationsByModuleName(pipelineStageResults.Specialized!, ["ModuleA"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(specializedModuleA, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleA.helper : Basics.Int -> Basics.Int
            ModuleA.helper x =
                Basics.add
                    x
                    1
            """);

        var specializedModuleB =
            SnapshotTestFormat.FilterDeclarationsByModuleName(pipelineStageResults.Specialized!, ["ModuleB"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(specializedModuleB, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleB.combined : Basics.Int -> Basics.Int
            ModuleB.combined x =
                ModuleA.helper
                    x
            """);

        // --- Assert inlined stage ---
        var inlinedModuleA =
            SnapshotTestFormat.FilterDeclarationsByModuleName(pipelineStageResults.Inlined!, ["ModuleA"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(inlinedModuleA, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleA.helper : Basics.Int -> Basics.Int
            ModuleA.helper x =
                Basics.add
                    x
                    1
            """);

        var inlinedModuleB =
            SnapshotTestFormat.FilterDeclarationsByModuleName(pipelineStageResults.Inlined!, ["ModuleB"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(inlinedModuleB, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleB.combined : Basics.Int -> Basics.Int
            ModuleB.combined x =
                ModuleA.helper
                    x
            """);

        // --- Assert final stage (after operator lowering) ---
        var finalDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.ModulesForCompilation);

        var finalModuleA =
            SnapshotTestFormat.FilterDeclarationsByModuleName(finalDict, ["ModuleA"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(finalModuleA, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleA.helper : Basics.Int -> Basics.Int
            ModuleA.helper x =
                Pine_builtin.int_add
                    [ x, 1 ]
            """);

        var finalModuleB =
            SnapshotTestFormat.FilterDeclarationsByModuleName(finalDict, ["ModuleB"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(finalModuleB, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleB.combined : Basics.Int -> Basics.Int
            ModuleB.combined x =
                Pine_builtin.int_add
                    [ x, 1 ]
            """);
    }

    /// <summary>
    /// Equivalent of <see cref="Two_module_pipeline_with_cross_module_call"/> but using the
    /// flat declaration dictionary format instead of whole-module text snapshots.
    /// This demonstrates that the new rendering format produces the same declaration content
    /// with qualified names derived from the dictionary keys.
    /// </summary>
    [Fact]
    public void Two_module_pipeline_flat_declaration_format()
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
                disableInlining: false);

        // --- Assert canonicalized stage using flat declaration dictionary ---
        var canonicalizedDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.Canonicalized);

        var canonicalizedModuleA =
            SnapshotTestFormat.FilterDeclarationsByModuleName(canonicalizedDict, ["ModuleA"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(canonicalizedModuleA, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleA.helper : Basics.Int -> Basics.Int
            ModuleA.helper x =
                Basics.add
                    x
                    1
            """);

        var canonicalizedModuleB =
            SnapshotTestFormat.FilterDeclarationsByModuleName(canonicalizedDict, ["ModuleB"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(canonicalizedModuleB, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleB.combined : Basics.Int -> Basics.Int
            ModuleB.combined x =
                ModuleA.helper
                    x
            """);

        // --- Assert final stage (after operator lowering) using flat declaration dictionary ---
        var finalDict =
            SnapshotTestFormat.ModulesToFlatDeclarationDictionary(
                pipelineStageResults.ModulesForCompilation);

        var finalModuleA =
            SnapshotTestFormat.FilterDeclarationsByModuleName(finalDict, ["ModuleA"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(finalModuleA, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleA.helper : Basics.Int -> Basics.Int
            ModuleA.helper x =
                Pine_builtin.int_add
                    [ x, 1 ]
            """);

        var finalModuleB =
            SnapshotTestFormat.FilterDeclarationsByModuleName(finalDict, ["ModuleB"]);

        SnapshotTestFormat.RenderQualifiedDeclarations(finalModuleB, SnapshotTestFormat.DeclarationSortOrder.NameAsc).Should().Be(
            """
            ModuleB.combined : Basics.Int -> Basics.Int
            ModuleB.combined x =
                Pine_builtin.int_add
                    [ x, 1 ]
            """);
    }
}
