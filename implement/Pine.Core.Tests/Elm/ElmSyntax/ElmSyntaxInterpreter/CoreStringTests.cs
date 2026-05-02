using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Text;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises the public API of <c>elm-kernel-modules/String.elm</c> through
/// <see cref="ElmInterpreter"/>. The String module — together with its dependency
/// modules (<c>Basics.elm</c>, <c>List.elm</c>, <c>Maybe.elm</c>, <c>Char.elm</c>) — is
/// loaded verbatim from <see cref="BundledFiles.CompilerSourceContainerFilesDefault"/>
/// and dispatched through the multi-module
/// <see cref="ElmInterpreter.ParseAndInterpret(string, IReadOnlyList{string})"/>
/// overload, which wraps the test expression in a synthetic root module with no
/// explicit imports (only the implicit imports from
/// <see cref="Core.Elm.ElmCompilerInDotnet.ImplicitImportConfig.Default"/> are in
/// scope) and runs the Elm compiler's canonicalization pass before the interpreter
/// dispatches the call. References into the kernel modules under test are therefore
/// always written with their fully-qualified canonical names (e.g. <c>String.toList</c>,
/// <c>Char.toCode</c>).
/// </summary>
public class CoreStringTests
{
    private static readonly Lazy<IReadOnlyList<string>> s_modules = new(LoadModules);

    private static IReadOnlyList<string> LoadModules()
    {
        // Load the kernel modules under test as dependency modules. The root
        // expression's lexical scope is provided by the synthetic root module
        // wrapped around it by ParseAndInterpret; user-supplied modules
        // contribute their declarations under their fully-qualified names but
        // do not contribute exposings to the root expression. Tests therefore
        // address kernel functions by their fully-qualified canonical names
        // (e.g. `String.toList`, `Char.toCode`). The implicit imports applied
        // by canonicalization (Maybe(Just,Nothing), Result(Ok,Err), Basics
        // operators, etc.) are also available because the dependency set
        // includes the corresponding kernel modules.
        return
            [
                LoadKernelModuleSource("String.elm"),
                LoadKernelModuleSource("List.elm"),
                LoadKernelModuleSource("Basics.elm"),
                LoadKernelModuleSource("Maybe.elm"),
                LoadKernelModuleSource("Char.elm"),
            ];
    }

    private static string LoadKernelModuleSource(string fileName)
    {
        var node =
            BundledFiles.ElmKernelModulesDefault.Value
            .GetNodeAtPath([fileName])
            ?? throw new Exception("Did not find elm-kernel-modules/" + fileName + " in bundled files.");

        if (node is not Files.FileTree.FileNode fileNode)
        {
            throw new Exception(
                "Expected elm-kernel-modules/" + fileName + " to be a file node, but got: " + node.GetType());
        }

        return Encoding.UTF8.GetString(fileNode.Bytes.Span);
    }

    /// <summary>
    /// Evaluates the supplied Elm expression against the kernel String / List / Basics /
    /// Maybe / Char modules and returns the rendered Elm-expression form of the result.
    /// </summary>
    private static string Evaluate(string expression) =>
        ElmValue.RenderAsElmExpression(
            ElmInterpreter.ParseAndInterpret(expression, s_modules.Value)
            .Extract(err => throw new Exception(err.ToString())))
        .expressionString;

    // ============================================================
    // toList
    // ============================================================

    [Fact]
    public void ToList_empty_string_is_empty_list()
    {
        Evaluate("String.toList \"\"")
            .Should().Be("[]");
    }

    [Fact]
    public void ToList_ascii_string_returns_chars()
    {
        Evaluate("String.toList \"abc\"")
            .Should().Be("[ 'a', 'b', 'c' ]");
    }

    [Fact]
    public void ToList_single_char_string()
    {
        Evaluate("String.toList \"x\"")
            .Should().Be("[ 'x' ]");
    }

    [Fact]
    public void ToList_string_with_spaces_and_punctuation()
    {
        Evaluate("String.toList \"a b!\"")
            .Should().Be("[ 'a', ' ', 'b', '!' ]");
    }

    // ============================================================
    // fromList
    // ============================================================

    [Fact]
    public void FromList_empty_list_is_empty_string()
    {
        Evaluate("String.fromList []")
            .Should().Be("\"\"");
    }

    [Fact]
    public void FromList_single_char()
    {
        Evaluate("String.fromList [ 'x' ]")
            .Should().Be("\"x\"");
    }

    [Fact]
    public void FromList_multiple_chars()
    {
        Evaluate("String.fromList [ 'a', 'b', 'c' ]")
            .Should().Be("\"abc\"");
    }

    [Fact]
    public void FromList_round_trips_with_toList()
    {
        Evaluate("String.fromList (String.toList \"hello\")")
            .Should().Be("\"hello\"");
    }

    // ============================================================
    // uncons
    // ============================================================

    [Fact]
    public void Uncons_empty_string_is_nothing()
    {
        Evaluate("String.uncons \"\"")
            .Should().Be("Nothing");
    }

    [Fact]
    public void Uncons_single_char_string_returns_char_and_empty_tail()
    {
        Evaluate("String.uncons \"x\"")
            .Should().Be("Just ('x', \"\")");
    }

    [Fact]
    public void Uncons_multi_char_string_returns_head_and_tail()
    {
        Evaluate("String.uncons \"abc\"")
            .Should().Be("Just ('a', \"bc\")");
    }

    // ============================================================
    // foldl
    // ============================================================

    [Fact]
    public void Foldl_empty_string_returns_initial_accumulator()
    {
        Evaluate("String.foldl (\\c acc -> acc ++ String.fromChar c) \"X\" \"\"")
            .Should().Be("\"X\"");
    }

    [Fact]
    public void Foldl_concatenates_chars_in_left_to_right_order()
    {
        // Folding from the left and prepending each new character produces the reverse.
        Evaluate("String.foldl (\\c acc -> String.fromChar c ++ acc) \"\" \"abc\"")
            .Should().Be("\"cba\"");
    }

    [Fact]
    public void Foldl_appends_chars_in_visit_order()
    {
        Evaluate("String.foldl (\\c acc -> acc ++ String.fromChar c) \"\" \"xyz\"")
            .Should().Be("\"xyz\"");
    }

    [Fact]
    public void Foldl_can_count_characters()
    {
        Evaluate("String.foldl (\\_ acc -> acc + 1) 0 \"hello\"")
            .Should().Be("5");
    }

    // ============================================================
    // map
    // ============================================================

    [Fact]
    public void Map_empty_string_returns_empty_string()
    {
        Evaluate("String.map (\\c -> c) \"\"")
            .Should().Be("\"\"");
    }

    [Fact]
    public void Map_identity_returns_original_string()
    {
        Evaluate("String.map (\\c -> c) \"abc\"")
            .Should().Be("\"abc\"");
    }

    [Fact]
    public void Map_replaces_every_character_with_constant()
    {
        Evaluate("String.map (\\c -> 'A') \"hi!\"")
            .Should().Be("\"AAA\"");
    }

    [Fact]
    public void Map_to_upper_via_char_arithmetic()
    {
        // 'a' to 'A': subtract 32 from the code point.
        Evaluate("String.map (\\c -> Char.fromCode (Char.toCode c - 32)) \"abc\"")
            .Should().Be("\"ABC\"");
    }

    // ============================================================
    // split
    // ============================================================

    [Fact]
    public void Split_with_non_matching_separator_returns_singleton_list()
    {
        Evaluate("String.split \",\" \"abc\"")
            .Should().Be("[ \"abc\" ]");
    }

    [Fact]
    public void Split_with_matching_separator_returns_segments()
    {
        Evaluate("String.split \",\" \"a,b,c\"")
            .Should().Be("[ \"a\", \"b\", \"c\" ]");
    }

    [Fact]
    public void Split_keeps_empty_segments_at_the_edges()
    {
        Evaluate("String.split \",\" \",a,\"")
            .Should().Be("[ \"\", \"a\", \"\" ]");
    }

    [Fact]
    public void Split_with_multi_character_separator()
    {
        Evaluate("String.split \"::\" \"a::b::c\"")
            .Should().Be("[ \"a\", \"b\", \"c\" ]");
    }

    [Fact]
    public void Split_with_empty_separator_returns_each_character_as_string()
    {
        Evaluate("String.split \"\" \"abc\"")
            .Should().Be("[ \"a\", \"b\", \"c\" ]");
    }

    [Fact]
    public void Split_with_empty_string_returns_singleton_empty_string()
    {
        Evaluate("String.split \",\" \"\"")
            .Should().Be("[ \"\" ]");
    }

    // ============================================================
    // replace
    // ============================================================

    [Fact]
    public void Replace_with_no_match_returns_original_string()
    {
        Evaluate("String.replace \"x\" \"y\" \"abc\"")
            .Should().Be("\"abc\"");
    }

    [Fact]
    public void Replace_replaces_every_occurrence()
    {
        Evaluate("String.replace \"x\" \"y\" \"axbxc\"")
            .Should().Be("\"aybyc\"");
    }

    [Fact]
    public void Replace_supports_multi_character_pattern()
    {
        Evaluate("String.replace \"ab\" \"XX\" \"abcab\"")
            .Should().Be("\"XXcXX\"");
    }

    [Fact]
    public void Replace_with_longer_replacement()
    {
        Evaluate("String.replace \".\" \" dot \" \"a.b\"")
            .Should().Be("\"a dot b\"");
    }

    [Fact]
    public void Replace_with_empty_replacement_deletes_pattern()
    {
        Evaluate("String.replace \"x\" \"\" \"axbxc\"")
            .Should().Be("\"abc\"");
    }

    // ============================================================
    // fromInt
    // ============================================================

    [Fact]
    public void FromInt_zero_returns_zero_string()
    {
        Evaluate("String.fromInt 0")
            .Should().Be("\"0\"");
    }

    [Fact]
    public void FromInt_positive_single_digit()
    {
        Evaluate("String.fromInt 7")
            .Should().Be("\"7\"");
    }

    [Fact]
    public void FromInt_positive_multi_digit()
    {
        Evaluate("String.fromInt 12345")
            .Should().Be("\"12345\"");
    }

    [Fact]
    public void FromInt_negative_value_includes_minus_sign()
    {
        Evaluate("String.fromInt -42")
            .Should().Be("\"-42\"");
    }

    [Fact]
    public void FromInt_negative_one()
    {
        Evaluate("String.fromInt -1")
            .Should().Be("\"-1\"");
    }

    // ============================================================
    // toInt
    // ============================================================

    [Fact]
    public void ToInt_empty_string_is_nothing()
    {
        Evaluate("String.toInt \"\"")
            .Should().Be("Nothing");
    }

    [Fact]
    public void ToInt_valid_unsigned_integer()
    {
        Evaluate("String.toInt \"42\"")
            .Should().Be("Just 42");
    }

    [Fact]
    public void ToInt_valid_negative_integer()
    {
        Evaluate("String.toInt \"-42\"")
            .Should().Be("Just -42");
    }

    [Fact]
    public void ToInt_valid_explicit_positive_integer()
    {
        Evaluate("String.toInt \"+42\"")
            .Should().Be("Just 42");
    }

    [Fact]
    public void ToInt_zero()
    {
        Evaluate("String.toInt \"0\"")
            .Should().Be("Just 0");
    }

    [Fact]
    public void ToInt_invalid_string_is_nothing()
    {
        Evaluate("String.toInt \"abc\"")
            .Should().Be("Nothing");
    }

    [Fact]
    public void ToInt_string_with_trailing_garbage_is_nothing()
    {
        Evaluate("String.toInt \"42x\"")
            .Should().Be("Nothing");
    }

    [Fact]
    public void ToInt_round_trip_with_fromInt()
    {
        Evaluate("String.toInt (String.fromInt 12345)")
            .Should().Be("Just 12345");
    }

    [Fact]
    public void ToInt_round_trip_with_fromInt_negative()
    {
        Evaluate("String.toInt (String.fromInt -98765)")
            .Should().Be("Just -98765");
    }

    // ============================================================
    // trim
    // ============================================================

    [Fact]
    public void Trim_string_with_no_whitespace_is_identity()
    {
        Evaluate("String.trim \"abc\"")
            .Should().Be("\"abc\"");
    }

    [Fact]
    public void Trim_removes_leading_spaces()
    {
        Evaluate("String.trim \"   abc\"")
            .Should().Be("\"abc\"");
    }

    [Fact]
    public void Trim_removes_trailing_spaces()
    {
        Evaluate("String.trim \"abc   \"")
            .Should().Be("\"abc\"");
    }

    [Fact]
    public void Trim_removes_leading_and_trailing_spaces()
    {
        Evaluate("String.trim \"   abc   \"")
            .Should().Be("\"abc\"");
    }

    [Fact]
    public void Trim_preserves_internal_spaces()
    {
        Evaluate("String.trim \"   a b c   \"")
            .Should().Be("\"a b c\"");
    }

    [Fact]
    public void Trim_string_of_only_whitespace_returns_empty_string()
    {
        Evaluate("String.trim \"     \"")
            .Should().Be("\"\"");
    }

    [Fact]
    public void Trim_empty_string_returns_empty_string()
    {
        Evaluate("String.trim \"\"")
            .Should().Be("\"\"");
    }

    [Fact]
    public void Trim_removes_tabs_and_newlines()
    {
        Evaluate("String.trim \"\\t\\nabc\\n\\t\"")
            .Should().Be("\"abc\"");
    }
}
