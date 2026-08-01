using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Executes the actual <c>ElmSyntax.Concrete.Parser.TokensFromString</c> and
/// <c>ElmSyntax.Concrete.Parser.FromString</c> modules from
/// <c>implement/Pine.Core/Elm/elm-in-elm/pine-elm-syntax/src</c> (bundled with the assembly)
/// through the from-scratch, offline Elm-in-.NET compiler and virtual machine.
/// <para>
/// These tests exist to verify, by actually running the tokenizer, the behavior of the
/// offset/String-based rewrite of the tokenizer: recognizing LF, CRLF, and a lone CR as line
/// breaks with correct row/column bookkeeping, and preserving lexeme/raw-text/decoded-literal
/// content for string, character, and comment tokens.
/// </para>
/// </summary>
public class TokensFromStringConcreteTests
{
    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.ElmKernelModulesDefault.Value;

                var bundledTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value;

                var pineElmSyntaxSrcTree =
                    bundledTree.GetNodeAtPath(["pine-elm-syntax", "src"])
                    ?? throw new Exception("Did not find pine-elm-syntax/src");

                var mergedTree = kernelModulesTree;

                foreach (var (path, file) in pineElmSyntaxSrcTree.EnumerateFilesTransitive())
                {
                    mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
                }

                var rootFilePaths =
                    mergedTree.EnumerateFilesTransitive()
                    .Where(
                        file =>
                        file.path[^1].Equals("TokensFromString.elm", StringComparison.OrdinalIgnoreCase) ||
                        file.path[^1].Equals("FromString.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(file => (IReadOnlyList<string>)file.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        mergedTree,
                        rootFilePaths: rootFilePaths)
                    .Map(r => r.compiledEnvValue)
                    .Extract(err => throw new Exception("Failed compiling: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing: " + err));
            });

    private static PineValue GetModuleFunction(string moduleName, string functionName) =>
        s_env.Value.Modules
        .First(module => module.moduleName == moduleName)
        .moduleContent.FunctionDeclarations[functionName];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(
            reportFunctionApplication: _ => { },
            enableTailRecursionOptimization: true);

    private static ElmValue ElmString(string text) =>
        ElmValue.StringInstance(text);

    private static string TokenizeAndRender(string input)
    {
        var function =
            GetModuleFunction("ElmSyntax.Concrete.Parser.TokensFromString", "parseExpression");

        var (value, _) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                function,
                ElmString(input),
                s_vm);

        return ElmValue.RenderAsElmExpression(value).expressionString;
    }

    private static string ParseExpressionAndRender(string input)
    {
        var function =
            GetModuleFunction("ElmSyntax.Concrete.Parser.FromString", "parseExpression");

        var (value, _) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                function,
                ElmString(input),
                s_vm);

        return ElmValue.RenderAsElmExpression(value).expressionString;
    }

    private static string ParseFileAndRender(string input)
    {
        var function =
            GetModuleFunction("ElmSyntax.Concrete.Parser.FromString", "parseFile");

        var (value, _) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                function,
                ElmString(input),
                s_vm);

        return ElmValue.RenderAsElmExpression(value).expressionString;
    }

    [Fact]
    public void Parse_file_with_import_comment_and_multiple_declarations()
    {
        var source =
            """
            module Main exposing (..)

            -- file comment
            import Html as H exposing (Html)

            first = 1

            second = first
            """;

        var rendered = ParseFileAndRender(source);

        rendered.Should().Be(
            """
            Ok { comments = [ Node { end = { column = 16, row = 3 }, start = { column = 1, row = 3 } } "-- file comment" ], declarations = [ Node { end = { column = 10, row = 6 }, start = { column = 1, row = 6 } } (FunctionDeclaration (Node { end = { column = 10, row = 6 }, start = { column = 1, row = 6 } } { declaration = Node { end = { column = 10, row = 6 }, start = { column = 1, row = 6 } } { arguments = [], equalsTokenLocation = { column = 7, row = 6 }, expression = Node { end = { column = 10, row = 6 }, start = { column = 9, row = 6 } } (IntegerLiteral "1"), name = Node { end = { column = 6, row = 6 }, start = { column = 1, row = 6 } } "first" }, documentation = Nothing, signature = Nothing })), Node { end = { column = 15, row = 8 }, start = { column = 1, row = 8 } } (FunctionDeclaration (Node { end = { column = 15, row = 8 }, start = { column = 1, row = 8 } } { declaration = Node { end = { column = 15, row = 8 }, start = { column = 1, row = 8 } } { arguments = [], equalsTokenLocation = { column = 8, row = 8 }, expression = Node { end = { column = 15, row = 8 }, start = { column = 10, row = 8 } } (Identifier [] "first"), name = Node { end = { column = 7, row = 8 }, start = { column = 1, row = 8 } } "second" }, documentation = Nothing, signature = Nothing })) ], imports = [ Node { end = { column = 33, row = 4 }, start = { column = 1, row = 4 } } { exposingList = Just ({ column = 18, row = 4 }, Node { end = { column = 33, row = 4 }, start = { column = 18, row = 4 } } (Explicit { column = 27, row = 4 } (NonEmpty (Node { end = { column = 32, row = 4 }, start = { column = 28, row = 4 } } (TypeOrAliasExpose "Html")) []) { column = 32, row = 4 })), importTokenLocation = { column = 1, row = 4 }, moduleAlias = Just ({ column = 13, row = 4 }, Node { end = { column = 17, row = 4 }, start = { column = 16, row = 4 } } [ "H" ]), moduleName = Node { end = { column = 12, row = 4 }, start = { column = 8, row = 4 } } [ "Html" ] } ], incompleteDeclarations = [], moduleDefinition = Node { end = { column = 26, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 26, row = 1 }, start = { column = 13, row = 1 } } (All { end = { column = 25, row = 1 }, start = { column = 23, row = 1 } }), moduleName = Node { end = { column = 12, row = 1 }, start = { column = 8, row = 1 } } [ "Main" ] }) }
            """.Trim());
    }

    [Theory]
    [InlineData(
        "LF only",
        "alpha\nbeta",
        """Ok [ { end = { column = 6, row = 1 }, lexeme = "alpha", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = Identifier }, { end = { column = 1, row = 2 }, lexeme = "\n", rawText = Nothing, start = { column = 6, row = 1 }, tokenType = Newline }, { end = { column = 5, row = 2 }, lexeme = "beta", rawText = Nothing, start = { column = 1, row = 2 }, tokenType = Identifier } ]""")]
    [InlineData(
        "CRLF only",
        "alpha\u000D\nbeta",
        """Ok [ { end = { column = 6, row = 1 }, lexeme = "alpha", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = Identifier }, { end = { column = 1, row = 2 }, lexeme = "\n", rawText = Nothing, start = { column = 6, row = 1 }, tokenType = Newline }, { end = { column = 5, row = 2 }, lexeme = "beta", rawText = Nothing, start = { column = 1, row = 2 }, tokenType = Identifier } ]""")]
    [InlineData(
        "lone CR only",
        "alpha\u000Dbeta",
        """Ok [ { end = { column = 6, row = 1 }, lexeme = "alpha", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = Identifier }, { end = { column = 1, row = 2 }, lexeme = "\n", rawText = Nothing, start = { column = 6, row = 1 }, tokenType = Newline }, { end = { column = 5, row = 2 }, lexeme = "beta", rawText = Nothing, start = { column = 1, row = 2 }, tokenType = Identifier } ]""")]
    [InlineData(
        "LF followed by CRLF followed by lone CR",
        "a\nb\u000D\nc\u000Dd",
        """Ok [ { end = { column = 2, row = 1 }, lexeme = "a", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = Identifier }, { end = { column = 1, row = 2 }, lexeme = "\n", rawText = Nothing, start = { column = 2, row = 1 }, tokenType = Newline }, { end = { column = 2, row = 2 }, lexeme = "b", rawText = Nothing, start = { column = 1, row = 2 }, tokenType = Identifier }, { end = { column = 1, row = 3 }, lexeme = "\n", rawText = Nothing, start = { column = 2, row = 2 }, tokenType = Newline }, { end = { column = 2, row = 3 }, lexeme = "c", rawText = Nothing, start = { column = 1, row = 3 }, tokenType = Identifier }, { end = { column = 1, row = 4 }, lexeme = "\n", rawText = Nothing, start = { column = 2, row = 3 }, tokenType = Newline }, { end = { column = 2, row = 4 }, lexeme = "d", rawText = Nothing, start = { column = 1, row = 4 }, tokenType = Identifier } ]""")]
    public void Mixed_line_break_tokenization(string description, string input, string expected)
    {
        _ = description;

        TokenizeAndRender(input).Should().Be(expected);
    }

    [Theory]
    [InlineData(
        "simple operator expression",
        "1 + 2",
        """Ok [ { end = { column = 2, row = 1 }, lexeme = "1", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = NumberLiteral }, { end = { column = 3, row = 1 }, lexeme = " ", rawText = Nothing, start = { column = 2, row = 1 }, tokenType = Whitespace }, { end = { column = 4, row = 1 }, lexeme = "+", rawText = Nothing, start = { column = 3, row = 1 }, tokenType = Operator }, { end = { column = 5, row = 1 }, lexeme = " ", rawText = Nothing, start = { column = 4, row = 1 }, tokenType = Whitespace }, { end = { column = 6, row = 1 }, lexeme = "2", rawText = Nothing, start = { column = 5, row = 1 }, tokenType = NumberLiteral } ]""")]
    [InlineData(
        "hexadecimal integer literal",
        "0xFF",
        """Ok [ { end = { column = 5, row = 1 }, lexeme = "0xFF", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = NumberLiteral } ]""")]
    [InlineData(
        "float literal with exponent",
        "6.5e2",
        """Ok [ { end = { column = 6, row = 1 }, lexeme = "6.5e2", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = NumberLiteral } ]""")]
    [InlineData(
        "negation before identifier",
        "-x",
        """Ok [ { end = { column = 2, row = 1 }, lexeme = "-", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = Negation }, { end = { column = 3, row = 1 }, lexeme = "x", rawText = Nothing, start = { column = 2, row = 1 }, tokenType = Identifier } ]""")]
    [InlineData(
        "minus as operator immediately after identifier",
        "x-y",
        """Ok [ { end = { column = 2, row = 1 }, lexeme = "x", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = Identifier }, { end = { column = 3, row = 1 }, lexeme = "-", rawText = Nothing, start = { column = 2, row = 1 }, tokenType = Operator }, { end = { column = 4, row = 1 }, lexeme = "y", rawText = Nothing, start = { column = 3, row = 1 }, tokenType = Identifier } ]""")]
    public void Basic_token_shapes(string description, string input, string expected)
    {
        _ = description;

        TokenizeAndRender(input).Should().Be(expected);
    }

    [Theory]
    [InlineData(
        "string literal with common escapes",
        "\"a\\nb\\tc\"",
        """Ok [ { end = { column = 10, row = 1 }, lexeme = "a\nb\tc", rawText = Just "a\\nb\\tc", start = { column = 1, row = 1 }, tokenType = StringLiteral } ]""")]
    [InlineData(
        "string literal with unicode escape",
        "\"\\u{1F600}\"",
        "Ok [ { end = { column = 12, row = 1 }, lexeme = \"\uD83D\uDE00\", rawText = Just \"\\\\u{1F600}\", start = { column = 1, row = 1 }, tokenType = StringLiteral } ]")]
    [InlineData(
        "char literal",
        "'x'",
        """Ok [ { end = { column = 4, row = 1 }, lexeme = "x", rawText = Just "x", start = { column = 1, row = 1 }, tokenType = CharLiteral } ]""")]
    public void Literal_lexeme_and_raw_text(string description, string input, string expected)
    {
        _ = description;

        TokenizeAndRender(input).Should().Be(expected);
    }

    [Fact]
    public void Triple_quoted_string_spanning_mixed_line_breaks()
    {
        // Contains a LF and a CRLF inside the triple-quoted string content; both should be
        // normalized to a single '\n' in the lexeme/raw text, each advancing the row by one.
        var input = "\"\"\"a\nb\u000D\nc\"\"\"";

        var expected =
            """Ok [ { end = { column = 5, row = 3 }, lexeme = "a\nb\nc", rawText = Just "a\nb\nc", start = { column = 1, row = 1 }, tokenType = TripleQuotedStringLiteral } ]""";

        TokenizeAndRender(input).Should().Be(expected);
    }

    [Fact]
    public void Nested_multiline_comment_with_crlf_inside()
    {
        var input = "{- outer\u000D\n {- inner -} end -}";

        var expected =
            """Ok [ { end = { column = 20, row = 2 }, lexeme = "{- outer\n {- inner -} end -}", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = Comment } ]""";

        TokenizeAndRender(input).Should().Be(expected);
    }

    [Fact]
    public void Line_comment_stops_before_lone_cr_and_next_token_advances_row()
    {
        var input = "-- comment\u000Dx";

        var expected =
            """Ok [ { end = { column = 11, row = 1 }, lexeme = "-- comment", rawText = Nothing, start = { column = 1, row = 1 }, tokenType = Comment }, { end = { column = 1, row = 2 }, lexeme = "\n", rawText = Nothing, start = { column = 11, row = 1 }, tokenType = Newline }, { end = { column = 2, row = 2 }, lexeme = "x", rawText = Nothing, start = { column = 1, row = 2 }, tokenType = Identifier } ]""";

        TokenizeAndRender(input).Should().Be(expected);
    }

    [Theory]
    [InlineData("integer", "42", "IntegerLiteral")]
    [InlineData("hex integer", "0xFF", "IntegerLiteral")]
    [InlineData("float", "3.14", "FloatLiteral")]
    [InlineData("string", "\"hello\"", "StringLiteral")]
    [InlineData("operator application", "1 + 2 * 3", "OperatorApplication")]
    public void FromString_still_parses_expressions_after_refactor(
        string description,
        string input,
        string expectedTagName)
    {
        _ = description;

        var rendered = ParseExpressionAndRender(input);

        rendered.Should().StartWith("Ok").And.Contain(expectedTagName);
    }
}
