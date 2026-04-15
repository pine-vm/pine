using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmSyntax;

/// <summary>
/// Tests verifying that the <see cref="SnapshotTestFormat.DeclarationSortOrder"/> parameter
/// produces different orderings from the same source dictionary.
/// </summary>
public class DeclarationSortOrderTests
{
    /// <summary>
    /// Helper that parses a single module text through canonicalization and returns
    /// the flat declaration dictionary.
    /// </summary>
    private static System.Collections.Generic.IReadOnlyDictionary<
        Core.CodeAnalysis.DeclQualifiedName,
        SyntaxTypes.Declaration> GetFlatDict(string moduleText)
    {
        var parsedModule =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var parsedModules =
            new[] { SyntaxTypes.FromFullSyntaxModel.Convert(parsedModule) };

        var canonicalizedModules =
            Canonicalization.Canonicalize(parsedModules)
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var orderedModules =
            parsedModules
            .Select(
                module =>
                canonicalizedModules[SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value]
                .Extract(err => throw new System.Exception("Module has errors: " + err)))
            .ToList();

        return SnapshotTestFormat.ModulesToFlatDeclarationDictionary(orderedModules);
    }

    [Fact]
    public void NameAsc_sorts_declarations_alphabetically_by_qualified_name()
    {
        var moduleText =
            """
            module App exposing (..)


            zebra x =
                x


            alfa x =
                x


            middle x =
                x
            """;

        var flatDict = GetFlatDict(moduleText);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclarations(
                flatDict,
                SnapshotTestFormat.DeclarationSortOrder.NameAsc);

        rendered.Should().Be(
            """
            App.alfa x =
                x


            App.middle x =
                x


            App.zebra x =
                x
            """);
    }

    [Fact]
    public void DependenciesDesc_puts_declarations_with_more_dependencies_first()
    {
        // 'root' depends on 'helper' which depends on 'leaf'.
        // Transitive dependency counts:
        //   root   -> helper, leaf  = 2
        //   helper -> leaf          = 1
        //   leaf   -> (none)        = 0
        //
        // So DependenciesDesc order should be: root, helper, leaf
        // (within same dep count, alphabetical ascending)

        var moduleText =
            """
            module App exposing (..)


            leaf x =
                Pine_kernel.int_add [ x, 1 ]


            helper x =
                App.leaf x


            root x =
                App.helper x
            """;

        var flatDict = GetFlatDict(moduleText);

        var renderedDepsDesc =
            SnapshotTestFormat.RenderQualifiedDeclarations(
                flatDict,
                SnapshotTestFormat.DeclarationSortOrder.DependenciesDesc);

        renderedDepsDesc.Should().Be(
            """
            App.root x =
                App.helper
                    x


            App.helper x =
                App.leaf
                    x


            App.leaf x =
                Pine_kernel.int_add
                    [ x, 1 ]
            """);

        // Verify NameAsc produces a different (alphabetical) order
        var renderedNameAsc =
            SnapshotTestFormat.RenderQualifiedDeclarations(
                flatDict,
                SnapshotTestFormat.DeclarationSortOrder.NameAsc);

        renderedNameAsc.Should().Be(
            """
            App.helper x =
                App.leaf
                    x


            App.leaf x =
                Pine_kernel.int_add
                    [ x, 1 ]


            App.root x =
                App.helper
                    x
            """);
    }

    [Fact]
    public void DependenciesDesc_sorts_by_name_within_same_dependency_count()
    {
        // Three independent declarations (no dependencies between them).
        // All have 0 transitive dependencies, so they should be sorted by name ascending.

        var moduleText =
            """
            module App exposing (..)


            charlie x =
                x


            alfa x =
                x


            bravo x =
                x
            """;

        var flatDict = GetFlatDict(moduleText);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclarations(
                flatDict,
                SnapshotTestFormat.DeclarationSortOrder.DependenciesDesc);

        // Same as NameAsc since all dependency counts are equal (0)
        rendered.Should().Be(
            """
            App.alfa x =
                x


            App.bravo x =
                x


            App.charlie x =
                x
            """);
    }

    [Fact]
    public void DependenciesDesc_handles_diamond_dependency_pattern()
    {
        // Diamond:
        //   top depends on left and right
        //   left depends on bottom
        //   right depends on bottom
        //   bottom has no dependencies
        //
        // Transitive counts:
        //   top    -> left, right, bottom = 3
        //   left   -> bottom              = 1
        //   right  -> bottom              = 1
        //   bottom -> (none)              = 0
        //
        // Order: top, left, right (alphabetical tie at 1), bottom

        var moduleText =
            """
            module App exposing (..)


            bottom x =
                Pine_kernel.int_add [ x, 1 ]


            left x =
                App.bottom x


            right x =
                App.bottom x


            top x =
                Pine_kernel.int_add [ App.left x, App.right x ]
            """;

        var flatDict = GetFlatDict(moduleText);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclarations(
                flatDict,
                SnapshotTestFormat.DeclarationSortOrder.DependenciesDesc);

        rendered.Should().Be(
            """
            App.top x =
                Pine_kernel.int_add
                    [ App.left
                        x
                    , App.right
                        x
                    ]


            App.left x =
                App.bottom
                    x


            App.right x =
                App.bottom
                    x


            App.bottom x =
                Pine_kernel.int_add
                    [ x, 1 ]
            """);
    }

    [Fact]
    public void DeriveTransitiveDependencyCounts_returns_correct_counts()
    {
        var moduleText =
            """
            module App exposing (..)


            leaf x =
                Pine_kernel.int_add [ x, 1 ]


            mid x =
                App.leaf x


            top x =
                App.mid x
            """;

        var flatDict = GetFlatDict(moduleText);

        var counts = SnapshotTestFormat.DeriveTransitiveDependencyCounts(flatDict);

        var leafKey = flatDict.Keys.Single(k => k.DeclName == "leaf");
        var midKey = flatDict.Keys.Single(k => k.DeclName == "mid");
        var topKey = flatDict.Keys.Single(k => k.DeclName == "top");

        counts[leafKey].Should().Be(0);
        counts[midKey].Should().Be(1);
        counts[topKey].Should().Be(2);
    }

    [Fact]
    public void DependenciesDesc_ensures_declaration_appears_before_its_dependencies()
    {
        // This is the key invariant: with DependenciesDesc, any declaration must appear
        // further up (earlier) than its dependencies.

        var moduleText =
            """
            module App exposing (..)


            a x =
                App.b x


            b x =
                App.c x


            c x =
                x
            """;

        var flatDict = GetFlatDict(moduleText);

        var rendered =
            SnapshotTestFormat.RenderQualifiedDeclarations(
                flatDict,
                SnapshotTestFormat.DeclarationSortOrder.DependenciesDesc);

        // a depends on b and c (transitively), b depends on c, c depends on nothing
        // So order should be: a (2 deps), b (1 dep), c (0 deps)
        var lines = rendered.Split('\n');
        var aIndex = System.Array.FindIndex(lines, l => l.StartsWith("App.a "));
        var bIndex = System.Array.FindIndex(lines, l => l.StartsWith("App.b "));
        var cIndex = System.Array.FindIndex(lines, l => l.StartsWith("App.c "));

        aIndex.Should().BeLessThan(bIndex, "a should appear before b (a depends on b)");
        bIndex.Should().BeLessThan(cIndex, "b should appear before c (b depends on c)");
    }
}
