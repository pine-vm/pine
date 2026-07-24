using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;
using System;
using System.Linq;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxAbstract;

/// <summary>
/// Helpers rendering the abstract Elm syntax model into a canonical textual form
/// used by snapshot-style tests (e.g. operator lowering).
/// </summary>
public static class ElmSyntaxAbstractTestHelper
{
    public static File AssertModulePreservedAcrossRenderedSyntax(string moduleText)
    {
        var firstConcrete =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(error => throw new InvalidOperationException("Input module failed to parse: " + error));

        if (firstConcrete.IncompleteDeclarations.Count is not 0)
        {
            throw new InvalidOperationException(
                "Input module contains incomplete declarations:\n\n" + moduleText);
        }

        var firstAbstract = ConvertFromConcrete.FromFile(firstConcrete);
        var firstRendered = RenderModuleForSyntaxRoundtrip(firstAbstract);
        var secondAbstract = ParseCompleteAbstract(firstRendered, "First rendered module");

        NormalizeForSemanticComparison(secondAbstract).Should().Be(
            NormalizeForSemanticComparison(firstAbstract),
            "rendering and reparsing must preserve the abstract syntax.\n\nRendered module:\n{0}",
            firstRendered);

        var secondRendered = RenderModuleForSyntaxRoundtrip(secondAbstract);
        var thirdAbstract = ParseCompleteAbstract(secondRendered, "Second rendered module");

        NormalizeForSemanticComparison(thirdAbstract).Should().Be(
            NormalizeForSemanticComparison(secondAbstract),
            "conversion should converge after another render-reparse cycle.\n\nRendered module:\n{0}",
            secondRendered);

        return firstAbstract;
    }

    public static string RenderModuleForSyntaxRoundtrip(File file)
        => Rendering.ToString(SnapshotTestFormat.Format(ConvertToConcrete.FromFile(file)));

    public static File NormalizeForSemanticComparison(File file) =>
        file with
        {
            Imports =
            [
                .. file.Imports.OrderBy(
                    ElmSyntaxAbstractJson.ToJsonString,
                    StringComparer.Ordinal)
            ],
            Declarations =
            [
                .. file.Declarations.OrderBy(
                    ElmSyntaxAbstractJson.ToJsonString,
                    StringComparer.Ordinal)
            ],
        };

    private static File ParseCompleteAbstract(string moduleText, string stage)
    {
        var concrete =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(
                error =>
                throw new InvalidOperationException(
                    stage + " failed to parse: " + error + "\n\n" + moduleText));

        if (concrete.IncompleteDeclarations.Count is not 0)
        {
            throw new InvalidOperationException(
                stage + " contains incomplete declarations:\n\n" + moduleText);
        }

        return ConvertFromConcrete.FromFile(concrete);
    }

    /// <summary>
    /// Renders the given abstract syntax <see cref="File"/> into a canonical,
    /// elm-format-inspired textual representation suitable for snapshot assertions.
    /// </summary>
    public static string RenderModuleForSnapshotTests(File module)
    {
        var moduleStil4mElmSyntax7 =
            Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(
                ConvertToConcrete.FromFile(module));

        return InliningTestHelper.RenderModuleForSnapshotTests(moduleStil4mElmSyntax7);
    }
}
