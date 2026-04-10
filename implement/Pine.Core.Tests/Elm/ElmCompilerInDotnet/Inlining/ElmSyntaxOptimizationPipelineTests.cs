using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;
using SyntaxTypes = Core.Elm.ElmSyntax.Stil4mElmSyntax7;

public class ElmSyntaxOptimizationPipelineTests
{
    [Fact]
    public void Split_specialization_and_inlining_stages_preserve_existing_higher_order_rewrite()
    {
        var elmModuleText =
            """
            module App exposing (..)


            apply func value =
                func value


            result =
                apply (\item -> item) 7
            """;

        var legacy =
            InliningTestHelper.CanonicalizeAndInlineAndGetSingleModule(
                [elmModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        var split =
            RunSeparatedSpecializationAndInlining(
                [elmModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        InliningTestHelper.RenderModuleForSnapshotTests(split)
            .Should()
            .Be(InliningTestHelper.RenderModuleForSnapshotTests(legacy));
    }

    [Fact]
    public void Builtin_operator_lowering_rewrites_proven_int_additions()
    {
        var elmModuleText =
            """
            module App exposing (..)


            sum : Int -> Int -> Int
            sum left right =
                left + right


            result : Int
            result =
                1 + 2
            """;

        var optimized =
            InliningTestHelper.CanonicalizeAndOptimizeAndGetSingleModule(
                [elmModuleText],
                ["App"],
                Inlining.Config.OnlyFunctions);

        var rendered =
            InliningTestHelper.RenderModuleForSnapshotTests(optimized);

        rendered.Should().Contain(
            """
            sum left right =
                Pine_builtin.int_add
            """);
    }

    private static SyntaxTypes.File RunSeparatedSpecializationAndInlining(
        IReadOnlyList<string> elmModulesTexts,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var parsedModules =
            elmModulesTexts
            .Select(
                text =>
                ElmSyntaxParser.ParseModuleText(text)
                .Extract(err => throw new System.Exception("Failed parsing: " + err)))
            .Select(SyntaxTypes.FromFullSyntaxModel.Convert)
            .ToList();

        var canonicalizedModules =
            Canonicalization.Canonicalize(parsedModules)
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err))
            .ToDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value
                .Extract(err => throw new System.Exception($"Module {string.Join(".", kvp.Key)} has errors: " + err)));

        var orderedModules =
            parsedModules
            .Select(
                module =>
                canonicalizedModules[SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value])
            .ToList();

        var specialized =
            ElmSyntaxSpecialization.Apply(orderedModules, config)
            .Extract(err => throw new System.Exception("Failed specialization: " + err));

        var specializedOrdered =
            orderedModules
            .Select(
                module =>
                specialized[SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value])
            .ToList();

        var inlined =
            ElmSyntaxInlining.Apply(specializedOrdered, config)
            .Extract(err => throw new System.Exception("Failed inlining stage: " + err));

        return inlined[moduleName];
    }
}
