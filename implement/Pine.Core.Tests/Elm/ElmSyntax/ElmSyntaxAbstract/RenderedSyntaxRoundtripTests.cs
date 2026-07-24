using AwesomeAssertions;
using System.Collections.Generic;
using Xunit;

using Abstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxAbstract;

public class RenderedSyntaxRoundtripTests
{
    public static IEnumerable<object[]> GrammaticalContextModules()
    {
        yield return Module(
            "compound expression in function position",
            "value =\n    (if condition then left else right) argument");

        yield return Module(
            "compound expressions in argument positions",
            "value =\n    outer (inner argument) (let nested = argument in nested)");

        yield return Module(
            "compound function argument pattern",
            "unwrap fallback (Wrapped value) =\n    value");

        yield return Module(
            "compound lambda argument pattern",
            "unwrap =\n    \\(Wrapped value) -> value");

        yield return Module(
            "constructor argument containing compound pattern",
            "value tree =\n    case tree of\n        Node (Just item) rest ->\n            item\n\n        _ ->\n            fallback");

        yield return Module(
            "as pattern",
            "unwrap ((Wrapped value) as wrapped) =\n    ( value, wrapped )");

        yield return Module(
            "let destructuring",
            "unwrap wrapped =\n    let\n        (Wrapped value) =\n            wrapped\n    in\n    value");

        yield return Module(
            "nested pattern contexts",
            "unwrap wrapped =\n    let\n        ((Wrapped (Just value)) as whole) =\n            wrapped\n    in\n    case whole of\n        Wrapped ((Just nested) as optional) ->\n            ( value, nested, optional )\n\n        _ ->\n            fallback");

        yield return Module(
            "as pattern in cons pattern",
            "value items =\n    case items of\n        ((Wrapped item) as wrapped) :: rest ->\n            ( item, wrapped, rest )\n\n        [] ->\n            fallback");

        yield return Module(
            "operator precedence and associativity",
            "value =\n    ((a + b) * c) :: (d ^ (e ^ f)) :: (g |> h) :: []");

        yield return Module(
            "type annotation grouping",
            "apply : ((a -> b) -> c) -> (a -> b) -> c\napply consume transform =\n    consume transform");
    }

    [Theory]
    [MemberData(nameof(GrammaticalContextModules))]
    public void Source_module_is_preserved_across_rendered_syntax(string _, string moduleText)
    {
        ElmSyntaxAbstractTestHelper.AssertModulePreservedAcrossRenderedSyntax(moduleText);
    }

    [Fact]
    public void Grammar_preserving_source_variants_have_the_same_abstract_syntax()
    {
        var variants =
            new[]
            {
                """
                module Test exposing (..)

                value =
                    outer (inner 255)
                """,
                """
                module Test exposing (..)

                value =
                    (((outer)) (((inner)) (255)))
                """,
                """
                module Test exposing (..)

                {- Comments are absent from the abstract model. -}
                value =
                    outer {- between function and argument -} (inner 255)
                """,
                """
                module Test exposing (..)

                value =
                    outer
                        (inner
                            255
                        )
                """,
                """
                module Test exposing (..)

                value =
                    outer (inner 0xFF)
                """,
            };

        Abstract.File? expected = null;

        foreach (var variant in variants)
        {
            var abstractFile =
                ElmSyntaxAbstractTestHelper.AssertModulePreservedAcrossRenderedSyntax(variant);

            expected ??= abstractFile;
            abstractFile.Should().Be(expected);
        }
    }

    [Fact]
    public void Generated_expression_context_compositions_are_preserved()
    {
        var childExpressions =
            new[]
            {
                "inner argument",
                "left + right",
                "-number",
                "if condition then yes else no",
                "\\item -> item",
                "let nested = argument in nested",
                "case optional of\n            Just item -> item\n\n            Nothing -> fallback",
            };

        var parentContexts =
            new[]
            {
                "outer ({0})",
                "({0}) argument",
                "[ ({0}) ]",
                "if condition then ({0}) else fallback",
            };

        foreach (var child in childExpressions)
        {
            foreach (var parent in parentContexts)
            {
                var declaration = "value =\n    " + string.Format(parent, child);

                ElmSyntaxAbstractTestHelper.AssertModulePreservedAcrossRenderedSyntax(
                    ModuleText(declaration));
            }
        }
    }

    [Fact]
    public void Generated_pattern_context_compositions_are_preserved()
    {
        var compoundPatterns =
            new[]
            {
                "Wrapped value",
                "Just (Wrapped value)",
                "first :: rest",
                "( first, rest )",
                "{ name, age }",
            };

        foreach (var pattern in compoundPatterns)
        {
            var declarations =
                new[]
                {
                    $"function ({pattern}) =\n    result",
                    $"lambda =\n    \\({pattern}) -> result",
                    $"caseValue subject =\n    case subject of\n        ({pattern}) ->\n            result",
                    $"destructure subject =\n    let\n        ({pattern}) =\n            subject\n    in\n    result",
                    $"alias subject =\n    case subject of\n        (({pattern}) as whole) ->\n            whole",
                    $"constructor subject =\n    case subject of\n        Parent ({pattern}) ->\n            result",
                };

            foreach (var declaration in declarations)
            {
                ElmSyntaxAbstractTestHelper.AssertModulePreservedAcrossRenderedSyntax(
                    ModuleText(declaration));
            }
        }
    }

    [Fact]
    public void Generated_type_annotation_compositions_are_preserved()
    {
        var childTypes =
            new[]
            {
                "a -> b",
                "{ value : a }",
                "( a, b )",
                "Maybe a",
            };

        var parentTypes =
            new[]
            {
                "({0}) -> result",
                "List ({0})",
                "Maybe ({0})",
                "{ field : ({0}) }",
            };

        foreach (var child in childTypes)
        {
            foreach (var parent in parentTypes)
            {
                var declaration =
                    "value : " + parent.Replace("{0}", child) + "\nvalue =\n    implementation";

                ElmSyntaxAbstractTestHelper.AssertModulePreservedAcrossRenderedSyntax(
                    ModuleText(declaration));
            }
        }
    }

    private static object[] Module(string name, string declaration) =>
        [name, ModuleText(declaration)];

    private static string ModuleText(string declaration) =>
        "module Test exposing (..)\n\n" + declaration + "\n";
}
