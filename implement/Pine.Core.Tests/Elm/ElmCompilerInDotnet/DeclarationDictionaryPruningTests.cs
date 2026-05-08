using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class DeclarationDictionaryPruningTests
{
    [Fact]
    public void Keeps_only_the_root_when_nothing_else_is_referenced()
    {
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                used : Int -> Int
                used x =
                    Pine_kernel.int_add [ x, 1 ]


                unused : Int -> Int
                unused x =
                    x
                """);

        var pruned =
            DeclarationDictionaryPruning.PruneToReachable(
                declarations,
                new HashSet<DeclQualifiedName> { DeclQualifiedName.FromString("Test.used") });

        FunctionDeclarationNames(pruned).Should().BeEquivalentTo(["Test.used"]);
    }

    [Fact]
    public void Keeps_transitively_referenced_declarations()
    {
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                root : Int -> Int
                root x =
                    middle (Pine_kernel.int_add [ x, 1 ])


                middle : Int -> Int
                middle x =
                    leaf x


                leaf : Int -> Int
                leaf x =
                    x


                stranger : Int -> Int
                stranger x =
                    x
                """);

        var pruned =
            DeclarationDictionaryPruning.PruneToReachable(
                declarations,
                new HashSet<DeclQualifiedName> { DeclQualifiedName.FromString("Test.root") });

        FunctionDeclarationNames(pruned).Should().BeEquivalentTo(
            ["Test.root", "Test.middle", "Test.leaf"]);
    }

    [Fact]
    public void Keeps_references_inside_nested_let_lambda_and_case()
    {
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                root : Int -> Int
                root x =
                    let
                        local y =
                            (\z -> helperB z) y
                    in
                    case x of
                        0 ->
                            local x

                        _ ->
                            helperC x


                helperB : Int -> Int
                helperB x =
                    x


                helperC : Int -> Int
                helperC x =
                    x


                stranger : Int -> Int
                stranger x =
                    x
                """);

        var pruned =
            DeclarationDictionaryPruning.PruneToReachable(
                declarations,
                new HashSet<DeclQualifiedName> { DeclQualifiedName.FromString("Test.root") });

        // helperB is reachable through the let-bound `local`'s lambda
        // body; helperC is reachable through the case branch.
        FunctionDeclarationNames(pruned).Should().BeEquivalentTo(
            ["Test.root", "Test.helperB", "Test.helperC"]);
    }

    [Fact]
    public void Preserves_non_function_declarations_unconditionally()
    {
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                type Color
                    = Red
                    | Green
                    | Blue


                type alias Pair =
                    ( Int, Int )


                used : Int -> Int
                used x =
                    x


                unused : Int -> Int
                unused x =
                    x
                """);

        var pruned =
            DeclarationDictionaryPruning.PruneToReachable(
                declarations,
                new HashSet<DeclQualifiedName> { DeclQualifiedName.FromString("Test.used") });

        // The Color custom type and Pair alias survive even though the
        // root set does not name them.
        var keys =
            pruned.Keys.Select(k => k.FullName).OrderBy(s => s, System.StringComparer.Ordinal).ToList();

        keys.Should().BeEquivalentTo(["Test.Color", "Test.Pair", "Test.used"]);
    }

    [Fact]
    public void Multiple_roots_take_union_of_reachability()
    {
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                rootA : Int -> Int
                rootA x =
                    leafA x


                rootB : Int -> Int
                rootB x =
                    leafB x


                leafA : Int -> Int
                leafA x =
                    x


                leafB : Int -> Int
                leafB x =
                    x


                lonely : Int -> Int
                lonely x =
                    x
                """);

        var pruned =
            DeclarationDictionaryPruning.PruneToReachable(
                declarations,
                new HashSet<DeclQualifiedName>
                {
                    DeclQualifiedName.FromString("Test.rootA"),
                    DeclQualifiedName.FromString("Test.rootB"),
                });

        FunctionDeclarationNames(pruned).Should().BeEquivalentTo(
            ["Test.rootA", "Test.rootB", "Test.leafA", "Test.leafB"]);
    }

    [Fact]
    public void Roots_that_are_not_present_are_silently_ignored()
    {
        var declarations =
            ParseAndCanonicalize(
                """
                module Test exposing (..)


                used : Int -> Int
                used x =
                    x
                """);

        var pruned =
            DeclarationDictionaryPruning.PruneToReachable(
                declarations,
                new HashSet<DeclQualifiedName>
                {
                    DeclQualifiedName.FromString("Test.used"),
                    DeclQualifiedName.FromString("Test.doesNotExist"),
                });

        FunctionDeclarationNames(pruned).Should().BeEquivalentTo(["Test.used"]);
    }

    private static IReadOnlyList<string> FunctionDeclarationNames(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations) =>
        [
        .. declarations
        .Where(kvp => kvp.Value is SyntaxTypes.Declaration.FunctionDeclaration)
        .Select(kvp => kvp.Key.FullName)
        .OrderBy(s => s, System.StringComparer.Ordinal)
        ];

    private static IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        ParseAndCanonicalize(string elmModuleText)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var converted =
            new[] { SyntaxTypes.FromFullSyntaxModel.Convert(parsed) };

        var canonicalized =
            Canonicalization.Canonicalize(converted)
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var orderedModules =
            converted
            .Select(
                module =>
                canonicalized[SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value]
                .Extract(err => throw new System.Exception("Module has errors: " + err)))
            .ToList();

        return ElmCompiler.FlattenModulesToDeclarationDictionary(orderedModules);
    }
}
