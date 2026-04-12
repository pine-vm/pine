using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests that verify the intermediate results of each compilation pipeline stage
/// are correctly captured and can be rendered back to Elm source text.
/// These tests compile multiple Elm modules and assert complete module text snapshots
/// for the canonicalized, specialized, inlined, and final (operator-lowered) stages.
/// </summary>
public class PipelineStageSnapshotTests
{
    /// <summary>
    /// Helper to render a list of pipeline stage modules (abstract syntax) to a dictionary
    /// of module name → formatted Elm source text, for easy snapshot comparison.
    /// </summary>
    private static IReadOnlyDictionary<string, string> RenderModules(
        IReadOnlyList<File> modules)
    {
        return
            modules.ToDictionary(
                file =>
                {
                    var moduleName = Module.GetModuleName(file.ModuleDefinition.Value).Value;
                    return string.Join(".", moduleName);
                },
                file =>
                {
                    var fullSyntax = ToFullSyntaxModel.Convert(file);
                    return Avh4Format.FormatToString(fullSyntax);
                });
    }

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
        var canonicalized = RenderModules(pipelineStageResults.Canonicalized);

        canonicalized["Provider"].Should().Be(
            """
            module Provider exposing (..)


            applyTwice : (a -> a) -> a -> a
            applyTwice fn value =
                fn (fn value)

            """);

        canonicalized["Consumer"].Should().Be(
            """
            module Consumer exposing (..)


            increment : Basics.Int -> Basics.Int
            increment value =
                Basics.add value 1


            result : Basics.Int -> Basics.Int
            result n =
                Provider.applyTwice Consumer.increment n

            """);

        // --- Assert specialized stage ---
        var specialized = RenderModules(pipelineStageResults.Specialized!);

        specialized["Provider"].Should().Be(
            """
            module Provider exposing (..)


            applyTwice : (a -> a) -> a -> a
            applyTwice fn value =
                fn (fn value)

            """);

        specialized["Consumer"].Should().Be(
            """
            module Consumer exposing (..)


            increment : Basics.Int -> Basics.Int
            increment value =
                Basics.add value 1


            result : Basics.Int -> Basics.Int
            result n =
                Provider.applyTwice Consumer.increment n

            """);

        // --- Assert inlined stage ---
        var inlined = RenderModules(pipelineStageResults.Inlined!);

        inlined["Provider"].Should().Be(
            """
            module Provider exposing (..)


            applyTwice : (a -> a) -> a -> a
            applyTwice fn value =
                fn (fn value)

            """);

        // After inlining, the Consumer module's 'result' function has the body
        // of applyTwice inlined: instead of calling Provider.applyTwice, the body
        // is expanded to two nested calls to Consumer.increment.
        inlined["Consumer"].Should().Be(
            """
            module Consumer exposing (..)


            increment : Basics.Int -> Basics.Int
            increment value =
                Basics.add value 1


            result : Basics.Int -> Basics.Int
            result n =
                Consumer.increment (Consumer.increment n)

            """);

        // --- Assert ModulesForCompilation (final stage after operator lowering) ---
        var finalModules = RenderModules(pipelineStageResults.ModulesForCompilation);

        finalModules["Provider"].Should().Be(
            """
            module Provider exposing (..)


            applyTwice : (a -> a) -> a -> a
            applyTwice fn value =
                fn (fn value)

            """);

        // After operator lowering, Basics.add is replaced with Pine_builtin.int_add.
        finalModules["Consumer"].Should().Be(
            """
            module Consumer exposing (..)


            increment : Basics.Int -> Basics.Int
            increment value =
                Pine_builtin.int_add [ value, 1 ]


            result : Basics.Int -> Basics.Int
            result n =
                Pine_builtin.int_add [ (Consumer.increment n), 1 ]

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
        var canonicalized = RenderModules(pipelineStageResults.Canonicalized);

        canonicalized["ModuleA"].Should().Be(
            """
            module ModuleA exposing (..)


            helper : Basics.Int -> Basics.Int
            helper x =
                Basics.add x 1

            """);

        canonicalized["ModuleB"].Should().Be(
            """
            module ModuleB exposing (..)


            combined : Basics.Int -> Basics.Int
            combined x =
                ModuleA.helper x

            """);

        // --- Assert specialized stage ---
        var specialized = RenderModules(pipelineStageResults.Specialized!);

        specialized["ModuleA"].Should().Be(
            """
            module ModuleA exposing (..)


            helper : Basics.Int -> Basics.Int
            helper x =
                Basics.add x 1

            """);

        specialized["ModuleB"].Should().Be(
            """
            module ModuleB exposing (..)


            combined : Basics.Int -> Basics.Int
            combined x =
                ModuleA.helper x

            """);

        // --- Assert inlined stage ---
        var inlined = RenderModules(pipelineStageResults.Inlined!);

        inlined["ModuleA"].Should().Be(
            """
            module ModuleA exposing (..)


            helper : Basics.Int -> Basics.Int
            helper x =
                Basics.add x 1

            """);

        inlined["ModuleB"].Should().Be(
            """
            module ModuleB exposing (..)


            combined : Basics.Int -> Basics.Int
            combined x =
                ModuleA.helper x

            """);

        // --- Assert final stage (after operator lowering) ---
        var finalModules = RenderModules(pipelineStageResults.ModulesForCompilation);

        finalModules["ModuleA"].Should().Be(
            """
            module ModuleA exposing (..)


            helper : Basics.Int -> Basics.Int
            helper x =
                Pine_builtin.int_add [ x, 1 ]

            """);

        finalModules["ModuleB"].Should().Be(
            """
            module ModuleB exposing (..)


            combined : Basics.Int -> Basics.Int
            combined x =
                ModuleA.helper x

            """);
    }
}
