using AwesomeAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;
using Xunit;

using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Tests.Elm.ElmSyntax;

public class ModuleParseErrorTests
{
    [Fact]
    public void SyntaxError_inventory_matches_Elm_reference_JSON()
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);

        var results =
            TestResultSummary.RunFileBasedTestCases(
                Path.Combine("Elm", "ModuleParseError"),
                directory =>
                {
                    var name = Path.GetFileName(directory);
                    seen.Add(name);

                    var source = System.IO.File.ReadAllText(Path.Combine(directory, "Input.elm"));

                    var expected =
                        Canonical(System.IO.File.ReadAllText(Path.Combine(directory, "expected-elm-0-19-2.json")));

                    var errors = ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(source));
                    errors.Should().NotBeEmpty(name);
                    var stem = name[..name.LastIndexOf('-')];

                    var expectedBranch =
                        string.Concat(stem.Split('-').Select(word => char.ToUpperInvariant(word[0]) + word[1..]));

                    if (stem is "list-expr")
                        expectedBranch = nameof(SyntaxErrorBranch.IfCondition);

                    DescribeBranch(errors[0].Kind).Should().Be(expectedBranch, name);
                    var actual = Canonical(ElmSyntaxErrorRenderer.RenderJson(source, errors, new("src/Input.elm")));

                    if (expected != actual)
                    {
                        var artifacts = Path.Combine("artifacts", "test-logs", "SyntaxError");
                        Directory.CreateDirectory(artifacts);
                        System.IO.File.WriteAllText(Path.Combine(artifacts, name + ".json"), actual);

                        System.IO.File.WriteAllText(
                            Path.Combine(artifacts, name + ".error.txt"),
                            string.Join("\n", errors.Select(e => e.Kind.ToString())));
                    }

                    return (expected, actual);
                });

        results.Should().HaveCount(164);
        results.Where(result => !result.Passed).Should().BeEmpty(TestResultSummary.RenderSummary(results));
    }

    [Fact]
    public void SyntaxError_plain_text_composes_the_same_explanation_as_JSON()
    {
        var results =
            TestResultSummary.RunFileBasedTestCases(
                Path.Combine("Elm", "ModuleParseError"),
                directory =>
                {
                    var source = System.IO.File.ReadAllText(Path.Combine(directory, "Input.elm"));
                    var errors = ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(source));

                    foreach (var error in errors)
                    {
                        foreach (var includeSource in new[] { false, true })
                        {
                            var original = error;

                            var json =
                                JsonNode.Parse(
                                ElmSyntaxErrorRenderer.RenderJson(source, error,
                                    new("Input.elm", IncludeSource: includeSource)))!;

                            var message = json["errors"]![0]!["problems"]![0]!["message"]!.AsArray();

                            ElmSyntaxErrorRenderer.RenderPlainText(source, error, includeSource)
                                .Should().Be(string.Concat(message.Select(part => MessageText(part!))));

                            error.Should().Be(original);
                        }
                    }

                    return ("", "");
                });

        results.Should().HaveCount(164);
        results.Where(result => !result.Passed).Should().BeEmpty(TestResultSummary.RenderSummary(results));
    }

    [Fact]
    public void SyntaxError_JSON_options_compose_without_changing_diagnostics()
    {
        const string Source = "module Input exposing (..)\n\nvalue =\n";
        var error = ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(Source)).Single();
        var original = error;
        var options = new ElmSyntaxErrorJsonOptions("src/Input.elm", ModuleName: "Input");

        foreach (var verbosity in Enum.GetValues<SyntaxErrorVerbosity>())
        {
            var json =
                JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson(Source, error,
                options with { Path = "other/Input.elm", Verbosity = verbosity, WriteIndented = true }))!;

            json["errors"]![0]!["path"]!.GetValue<string>().Should().Be("other/Input.elm");
            error.Should().Be(original);
        }

        var unstyled =
            JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson(Source, error,
            options with { IncludeSource = false, IncludeStyling = false }))!;

        unstyled["errors"]![0]!["problems"]![0]!["message"]!.AsArray()
            .Should().OnlyContain(part => part is JsonValue);

        unstyled.ToJsonString().Should().NotContain("3|").And.NotContain("value =");

    }

    [Fact]
    public void SyntaxError_inventory_generalizes_to_different_source_locations()
    {
        var results =
            TestResultSummary.RunFileBasedTestCases(
                Path.Combine("Elm", "ModuleParseError"),
                directory =>
                {
                    var source = System.IO.File.ReadAllText(Path.Combine(directory, "Input.elm"));
                    var originalErrors = ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(source));
                    var shiftedSource = "-- preceding source comment\n\n" + source;

                    var shiftedErrors =
                        ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(shiftedSource));

                    var options = new ElmSyntaxErrorJsonOptions("a/different/location/Input.elm");
                    var expected = JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson(source, originalErrors, options))!;

                    foreach (var problem in expected["errors"]![0]!["problems"]!.AsArray())
                    {
                        foreach (var end in new[] { "start", "end" })
                        {
                            var position = problem!["region"]![end]!;
                            position["line"] = position["line"]!.GetValue<int>() + 2;
                        }

                        var message = problem!["message"]!.AsArray();

                        for (var i = 0; i < message.Count; i++)
                        {
                            if (message[i] is not JsonValue text)
                                continue;

                            message[i] =
                                Regex.Replace(
                                    text.GetValue<string>(),
                                    @"(?m)^(\d+)(\| )",
                                    match =>
                                    (int.Parse(match.Groups[1].Value, CultureInfo.InvariantCulture) + 2).ToString(
                                        CultureInfo.InvariantCulture) +
                                    match.Groups[2].Value);
                        }
                    }

                    var actual = ElmSyntaxErrorRenderer.RenderJson(shiftedSource, shiftedErrors, options);
                    return (Canonical(expected.ToJsonString()), Canonical(actual));
                });

        results.Where(result => !result.Passed).Should().BeEmpty(TestResultSummary.RenderSummary(results));
    }

    [Fact]
    public void SyntaxError_JSON_uses_supplied_source_and_zero_based_ranges()
    {
        var error =
            new ElmSyntaxParseError(
                new Range(new Location(12, 4), new Location(12, 6)),
                null,
                new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Escape(EscapeProblem.Unknown)));

        var source = new string('\n', 11) + "   '\\q'";
        var json = JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson(source, error, new("a\"b.elm")))!;
        var problem = json["errors"]![0]!["problems"]![0]!;
        problem["region"]!["start"]!["line"]!.GetValue<int>().Should().Be(11);
        problem["region"]!["start"]!["column"]!.GetValue<int>().Should().Be(3);
        problem["message"]![0]!.GetValue<string>().Should().Contain("12|    '\\q'\n       ");
        problem["message"]![1]!["string"]!.GetValue<string>().Should().Be("^^");
        json["errors"]![0]!["path"]!.GetValue<string>().Should().Be("a\"b.elm");
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r\n")]
    [InlineData("\r")]
    public void SyntaxError_JSON_keeps_source_markers_literal(string newline)
    {
        const string SourceLine =
            "    \"{{snippet}} {{carets}} {{definition}} {{found}} {{unknown}}\" \\ \t <&> é 🐟";

        var source = "-- prelude" + newline + "contextValue =" + newline + SourceLine;

        var error =
            new ElmSyntaxParseError(
                new Range(new Location(3, 5), new Location(3, 7)),
                new Range(new Location(2, 1), new Location(3, 7)),
                new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Grammar(SyntaxErrorBranch.DefBody)));

        foreach (var includeSource in new[] { false, true })
        {
            foreach (var includeStyling in new[] { false, true })
            {
                var json =
                    JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson(source, error,
                    new("src/Input.elm", IncludeSource: includeSource, IncludeStyling: includeStyling)))!;

                var message = json["errors"]![0]!["problems"]![0]!["message"]!.AsArray();

                message[0]!.GetValue<string>().Should().Be(
                    "I got stuck while parsing the `contextValue` definition:\n\n" +
                    (includeSource ? "2| contextValue =\n3| " + SourceLine + "\n       " : ""));

                MessageText(message[1]).Should().Be(includeSource ? "^^" : "");
            }
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SyntaxError_JSON_composes_annotation_fields_without_interpreting_markers(bool includeStyling)
    {
        const string Annotated = "{{defined}}\"\\é";
        const string Defined = "{{annotated}}{{snippet}}{{unknown}}";
        var range = new Range(new Location(1, 1), new Location(1, 2));

        var error =
            new ElmSyntaxParseError(
                range,
                null,
                new ElmSyntaxErrorKind.Parse(
                    new ElmSyntaxProblem.AnnotationNameMismatch(new LocatedIdentifier(Annotated, range), Defined)));

        var options =
            new ElmSyntaxErrorJsonOptions(
                "Input.elm",
                IncludeSource: false,
                IncludeStyling: includeStyling);

        var json = JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson("", error, options))!;
        var message = json["errors"]![0]!["problems"]![0]!["message"]!.AsArray();

        message.Select(MessageText).Should().Equal(
            $"I just saw a type annotation for `{Annotated}`, but it is followed by a definition for\n`{Defined}`:\n\n",
            "",
            "\nThese names do not match! Is there a typo?\n\n    ",
            Defined,
            " -> ",
            Annotated,
            "");
    }

    [Fact]
    public void SyntaxError_JSON_keeps_found_and_expected_spelling_literal()
    {
        const string Spelling = "{{found}}{{snippet}}{{carets}}{{unknown}}\"\\é";
        var found = new FoundSyntax(FoundSyntaxKind.Keyword, Spelling);

        var error =
            new ElmSyntaxParseError(
                new Range(new Location(1, 1), new Location(1, 2)),
                null,
                new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Grammar(SyntaxErrorBranch.DeclReserved, found)));

        var options = new ElmSyntaxErrorJsonOptions("Input.elm", IncludeSource: false);
        var json = JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson("", error, options))!;
        var message = json["errors"]![0]!["problems"]![0]!["message"]!.AsArray();

        message[0]!.GetValue<string>().Should().Be(
            "I was not expecting to run into the `" + Spelling + "` keyword here:\n\n");

        message[2]!.GetValue<string>().Should().Be(
            "\nIt is reserved for writing `" + Spelling + "` expressions. Try using a different name?\n\n");

        message[4]!.GetValue<string>().Should().StartWith(
            ": If you are trying to write an `" + Spelling + "` expression");

        var expected =
            error with
            {
                Kind =
                new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Expected(new ExpectedSyntax.Keyword(Spelling), found))
            };

        var expectedJson = JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson("", expected, options))!;

        expectedJson["errors"]![0]!["problems"]![0]!["message"]!.AsArray().Select(MessageText)
            .Should().Equal("Expected `" + Spelling + "`.");
    }

    [Theory]
    [InlineData("amount = 903.", "903", "903.0")]
    [InlineData("lookup ___customer = 1", "customer", "customer")]
    [InlineData("Camera = 1", "camera", "camera")]
    [InlineData("text = \"\\u{7e}\"", "\\u{007e}", "\\u{007e}")]
    public void SyntaxError_JSON_composes_dynamic_suggestions_without_source(
        string declaration, string firstSuggestion, string secondSuggestion)
    {
        var source = "module Input exposing (..)\n\n" + declaration + "\n";
        var error = ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(source)).Single();

        var json =
            JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson(source, error,
            new("Input.elm", IncludeSource: false)))!;

        var styledText =
            json["errors"]![0]!["problems"]![0]!["message"]!.AsArray()
            .OfType<JsonObject>().Select(part => part["string"]!.GetValue<string>());

        styledText.Should().Contain(firstSuggestion).And.Contain(secondSuggestion);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SyntaxError_JSON_preserves_protocol_shape_order_and_escaping(bool writeIndented)
    {
        const string Path = "folder\\{{path}}\"é.elm";
        const string Expected = "{{declaredModule}}\"\\\n<&>é";
        const string Declared = "{{expectedModule}}{{snippet}}";

        var error =
            new ElmSyntaxParseError(
                new Range(new Location(1, 2), new Location(1, 4)),
                null,
                new ElmSyntaxErrorKind.ModuleValidation(ModuleValidationProblem.ModuleNameMismatch, Expected, Declared));

        var options = new ElmSyntaxErrorJsonOptions(Path, IncludeSource: false, WriteIndented: writeIndented);
        var text = ElmSyntaxErrorRenderer.RenderJson("", error, options);
        using var document = JsonDocument.Parse(text);
        var root = document.RootElement;
        var file = root.GetProperty("errors")[0];
        var problem = file.GetProperty("problems")[0];
        var region = problem.GetProperty("region");
        var message = problem.GetProperty("message");

        root.EnumerateObject().Select(property => property.Name).Should().Equal("type", "errors");
        root.GetProperty("type").GetString().Should().Be("compile-errors");
        file.EnumerateObject().Select(property => property.Name).Should().Equal("path", "name", "problems");
        file.GetProperty("path").GetString().Should().Be(Path);
        file.GetProperty("name").GetString().Should().Be("{{path}}\"é");
        problem.EnumerateObject().Select(property => property.Name).Should().Equal("title", "region", "message");
        region.EnumerateObject().Select(property => property.Name).Should().Equal("start", "end");
        region.GetProperty("start").EnumerateObject().Select(property => property.Name).Should().Equal("line", "column");
        region.GetProperty("start").GetProperty("line").GetInt32().Should().Be(0);
        region.GetProperty("start").GetProperty("column").GetInt32().Should().Be(1);
        region.GetProperty("end").GetProperty("column").GetInt32().Should().Be(3);

        message[1].EnumerateObject().Select(property => property.Name).Should().Equal(
            "bold",
            "underline",
            "color",
            "string");

        message[1].GetProperty("bold").GetBoolean().Should().BeFalse();
        message[1].GetProperty("underline").GetBoolean().Should().BeFalse();
        message[1].GetProperty("color").GetString().Should().Be("RED");
        message[2].GetString().Should().Contain("`" + Expected + "`");
        message[3].GetProperty("string").GetString().Should().Be(Declared);
        message[5].GetProperty("string").GetString().Should().Be(Expected);
        message[7].GetProperty("color").ValueKind.Should().Be(JsonValueKind.Null);
        message[7].GetProperty("underline").GetBoolean().Should().BeTrue();
        text.Contains('\n').Should().Be(writeIndented);

        var missing =
            error with
            {
                Kind = new ElmSyntaxErrorKind.ModuleValidation(ModuleValidationProblem.ModuleNameMissing, Expected)
            };

        var missingJson = JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson("", missing, options))!;
        missingJson["errors"]![0]!["problems"]![0]!["message"]![2]!.GetValue<string>().Should().Be(" " + Expected + " ");
    }

    [Theory]
    [InlineData("", "    ")]
    [InlineData("\n", "    \n    ")]
    [InlineData("{{snippet}}\n\n\"\\<&>é\r\n{{shaderDetail}}\n", "    {{snippet}}\n    \n    \"\\<&>é\r\n    {{shaderDetail}}\n    ")]
    public void SyntaxError_JSON_preserves_opaque_shader_detail(string detail, string indented)
    {
        var error =
            new ElmSyntaxParseError(
                new Range(new Location(1, 1), new Location(1, 1)),
                null,
                new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Shader(ShaderProblem.Invalid, UpstreamDetail: detail)));

        var json =
            JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson("", error,
            new("Input.elm", IncludeSource: false, IncludeStyling: false)))!;

        json["errors"]![0]!["problems"]![0]!["message"]![2]!.GetValue<string>().Should().Be(
            "\nI use a 3rd party GLSL parser for now, and I did my best to extract their error\nmessage:\n\n" +
            indented);
    }

    [Fact]
    public void SyntaxError_JSON_orders_recovered_errors_and_limits_output()
    {
        const string source = "module Input exposing (..)\n\nfirst =\n\nsecond =\n";
        var result = ElmSyntaxParser.ParseModuleText(source);
        result.IsOkOrNull().Should().NotBeNull();
        var errors = ElmSyntaxErrorRenderer.CollectErrors(result);
        errors.Should().HaveCount(2);

        var json =
            JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson(source, errors.Reverse(),
            new("src/Input.elm", MaximumProblems: 1)))!;

        json["errors"]![0]!["problems"]!.AsArray().Should().HaveCount(1);
        json["errors"]![0]!["problems"]![0]!["region"]!["start"]!["line"]!.GetValue<int>().Should().Be(2);

        var empty =
            JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson(source, errors, new("src/Input.elm", MaximumProblems: 0)))!;

        empty["errors"]!.AsArray().Should().BeEmpty();

        var formatted =
            ElmFormat.FormatModuleTextReportingSyntaxErrors(source)
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        formatted.ParseErrors.Should().Equal(errors);
    }

    [Fact]
    public void SyntaxError_preserves_documentation_and_its_file_level_diagnostic()
    {
        const string Source = "module Input exposing (..)\n\n{-| documentation -}\n";

        var file =
            ElmSyntaxParser.ParseModuleText(Source).Extract(
                error => throw new InvalidOperationException(error.ToString()));

        file.IncompleteDeclarations.Should().BeEmpty();
        file.Comments.Should().Contain(comment => comment.Value.Contains("documentation", StringComparison.Ordinal));
        file.AdditionalParseErrors.Should().HaveCount(1);
        Pine.Core.Elm.ElmSyntax.Avh4Format.Format(file).AdditionalParseErrors.Should().Equal(file.AdditionalParseErrors);
        SnapshotTestFormat.Format(file).AdditionalParseErrors.Should().Equal(file.AdditionalParseErrors);
    }

    [Fact]
    public void SyntaxError_parser_preserves_record_separator_canonicalization()
    {
        foreach (var source in new[]
        {
            "module Input exposing (..)\n\nvalue = { x : 1 }\n",
            "module Input exposing (..)\n\nvalue = { x = 1 }\n"
        })
            ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(source)).Should().BeEmpty();
    }

    [Theory]
    [InlineData("amount = 903.", "903.0", "NumberDot")]
    [InlineData("lookup _customer = 1", "customer", "PatternWildcard")]
    [InlineData("Camera = 1", "camera", "DeclUpper")]
    [InlineData("size : Int\ncount = 1", "size", "DefNameMatch")]
    [InlineData("text = \"\\u{7e}\"", "\\u{007e}", "StringUnicodeShort")]
    public void SyntaxError_renderer_generalizes_source_values(string declaration, string renderedValue, string branch)
    {
        var source = "-- an arbitrary prelude\r\nmodule Other exposing (..)\r\n\r\n" + declaration + "\r\n";
        var error = ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(source)).Single();
        DescribeBranch(error.Kind).Should().Be(branch);
        var json = JsonNode.Parse(ElmSyntaxErrorRenderer.RenderJson(source, error, new("project/Other.elm")))!;
        var message = json["errors"]![0]!["problems"]![0]!["message"]!.AsArray();

        message.Select(part => part is JsonObject ? part["string"]!.GetValue<string>() : part!.GetValue<string>())
            .Should().Contain(part => part.Contains(renderedValue, StringComparison.Ordinal));

        error.Region.Start.Row.Should().BeGreaterThanOrEqualTo(4);
    }

    [Fact]
    public void SyntaxError_payloads_retain_parser_interpretation()
    {
        var mismatch =
            ElmSyntaxErrorRenderer.CollectErrors(
                ElmSyntaxParser.ParseModuleText(
                    "module Input exposing (..)\n\nfirst : Int\nsecond = 1\n")).Single();

        mismatch.Kind.Should().Be(
            new ElmSyntaxErrorKind.Parse(
                new ElmSyntaxProblem.AnnotationNameMismatch(
                    new LocatedIdentifier("first", new Range(new Location(3, 1), new Location(3, 6))),
                    "second")));

        var escaped =
            ElmSyntaxErrorRenderer.CollectErrors(
                ElmSyntaxParser.ParseModuleText(
                    "module Input exposing (..)\n\nx = \"\\u{FFFFFF}\"\n")).Single();

        escaped.Kind.Should().Be(
            new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Escape(EscapeProblem.UnicodeCode, 6, 0xFFFFFF)));

        var pattern =
            ElmSyntaxErrorRenderer.CollectErrors(
                ElmSyntaxParser.ParseModuleText(
                    "module Input exposing (..)\n\nf __argument = 1\n")).Single();

        pattern.Kind.Should().Be(new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Wildcard("__argument", 2)));

        var indentation =
            ElmSyntaxErrorRenderer.CollectErrors(
                ElmSyntaxParser.ParseModuleText(
                    "module Input exposing (..)\n\nvalue =\n1\n")).Single();

        indentation.Kind.Should().Be(
            new ElmSyntaxErrorKind.Parse(
                new ElmSyntaxProblem.Grammar(
                    SyntaxErrorBranch.DefIndentBody,
                    new FoundSyntax(FoundSyntaxKind.Literal, "1"),
                    SyntaxErrorSite.DefinitionBody,
                    RequiredColumn: 2,
                    ActualColumn: 1)));
    }

    [Fact]
    public void SyntaxError_diagnostic_grammar_accepts_qualified_constructor_patterns()
    {
        const string Source =
            """"
            module Test exposing (..)

            lastDeclarationDocumentation : List (Node Declaration.Declaration) -> Maybe String
            lastDeclarationDocumentation declarations =
                case List.reverse declarations of
                    Node _ (Declaration.FunctionDeclaration function) :: _ ->
                        Nothing

                    _ ->
                        Nothing

            nodeStrings : List (Node String) -> List String
            nodeStrings nodes =
                case nodes of
                    Node _ value :: rest ->
                        value :: nodeStrings rest

                    [] ->
                        []
            """";

        ElmSyntaxDiagnostics.Parse(Source).Should().BeEmpty();
    }

    [Fact]
    public void SyntaxError_diagnostic_grammar_accepts_consecutive_documentation_comments()
    {
        const string Source =
            """"
            module Test exposing (..)

            {-| Module documentation without an @docs section. -}

            {-| Declaration documentation. -}
            type alias Path =
                List Int
            """";

        ElmSyntaxDiagnostics.Parse(Source).Should().BeEmpty();
        ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(Source)).Should().BeEmpty();
    }

    [Fact]
    public void SyntaxError_diagnostic_grammar_accepts_exposed_operator_ending_in_dot()
    {
        const string Source = "module Test exposing ((|.))\n\nvalue = 1\n";

        ElmSyntaxDiagnostics.Parse(Source).Should().BeEmpty();
        ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(Source)).Should().BeEmpty();
    }

    [Fact]
    public void SyntaxError_diagnostic_reader_has_no_exception_or_mutation_control_flow()
    {
        var source = System.IO.File.ReadAllText(DiagnosticsSourcePath());
        var nodes = CSharpSyntaxTree.ParseText(source).GetRoot().DescendantNodes().ToArray();

        nodes.Where(
            node => node is
            ThrowStatementSyntax or ThrowExpressionSyntax or TryStatementSyntax or
            ForStatementSyntax or ForEachStatementSyntax or WhileStatementSyntax or DoStatementSyntax)
            .Should().BeEmpty();

        nodes.OfType<AssignmentExpressionSyntax>().Should().OnlyContain(
            assignment =>
            assignment.Parent is InitializerExpressionSyntax &&
            assignment.Parent.RawKind == (int)SyntaxKind.WithInitializerExpression);

        nodes.OfType<PrefixUnaryExpressionSyntax>().Should().NotContain(
            expression =>
            expression.RawKind == (int)SyntaxKind.PreIncrementExpression ||
            expression.RawKind == (int)SyntaxKind.PreDecrementExpression);

        nodes.OfType<PostfixUnaryExpressionSyntax>().Should().NotContain(
            expression =>
            expression.RawKind == (int)SyntaxKind.PostIncrementExpression ||
            expression.RawKind == (int)SyntaxKind.PostDecrementExpression);

        nodes.OfType<ParameterSyntax>().Should().OnlyContain(
            parameter =>
            !parameter.Modifiers.Any(token => token.RawKind == (int)SyntaxKind.RefKeyword ||
                token.RawKind == (int)SyntaxKind.OutKeyword));

        nodes.OfType<ArgumentSyntax>().Should().OnlyContain(argument => argument.RefKindKeyword.RawKind == 0);

        nodes.OfType<FieldDeclarationSyntax>().Should().OnlyContain(
            field =>
            field.Modifiers.Any(
                token => token.RawKind == (int)SyntaxKind.ReadOnlyKeyword ||
                    token.RawKind == (int)SyntaxKind.ConstKeyword));

        nodes.OfType<AccessorDeclarationSyntax>().Should().NotContain(
            accessor =>
            accessor.RawKind == (int)SyntaxKind.SetAccessorDeclaration);

        nodes.OfType<GenericNameSyntax>().Should().NotContain(
            name => name.Identifier.ValueText == "List" ||
                name.Identifier.ValueText == "Stack");

        nodes.OfType<IdentifierNameSyntax>().Should().NotContain(name => name.Identifier.ValueText == "StringBuilder");
    }

    private static string DiagnosticsSourcePath([CallerFilePath] string testPath = "") =>
        Path.GetFullPath(
            Path.Combine(
                Path.GetDirectoryName(testPath)!,
                "../../../Pine.Core/Elm/ElmSyntax/ElmSyntaxDiagnostics.cs"));

    [Theory]
    [InlineData("", EscapeProblem.UnicodeShort, null)]
    [InlineData("7e", EscapeProblem.UnicodeShort, 0x7e)]
    [InlineData("D800", EscapeProblem.UnicodeCode, 0xd800)]
    [InlineData("110000", EscapeProblem.UnicodeCode, 0x110000)]
    [InlineData("FFFFFF", EscapeProblem.UnicodeCode, 0xffffff)]
    [InlineData("0000041", EscapeProblem.UnicodeLong, 0x41)]
    [InlineData("0000000000000041", EscapeProblem.UnicodeLong, 0x41)]
    [InlineData("FFFFFFFF", EscapeProblem.UnicodeLong, -1)]
    [InlineData("80000000", EscapeProblem.UnicodeLong, int.MinValue)]
    [InlineData("100000000", EscapeProblem.UnicodeLong, null)]
    public void SyntaxError_unicode_payload_preserves_hex_width_and_overflow(
        string digits, EscapeProblem problem, int? code)
    {
        var source = "module Test exposing (..)\nvalue = \"\\u{" + digits + "}\"";
        var diagnostic = ElmSyntaxDiagnostics.Parse(source).Single();

        diagnostic.Error.Kind.Should().Be(
            new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Escape(problem, digits.Length, code)));

        diagnostic.Error.Region.Should().Be(new Range(new Location(2, 10), new Location(2, 14 + digits.Length)));
        diagnostic.Error.ContextRange.Should().BeNull();
    }

    [Theory]
    [InlineData("value = café")]
    [InlineData("value = абв")]
    [InlineData("value = 12٣4")]
    [InlineData("value = '😀'")]
    [InlineData("value = \"\\u{0000}\\u{10ffff}\\u{00e9}\"")]
    [InlineData("value = \"\"\"one\r\ntwo\rthree\n😀\"\"\"")]
    public void SyntaxError_lexer_fast_paths_preserve_unicode_and_newlines(string declaration)
    {
        ElmSyntaxDiagnostics.Parse("module Test exposing (..)\n\n" + declaration).Should().BeEmpty();
    }

    [Fact]
    public void SyntaxError_diagnostic_reader_is_reentrant()
    {
        var sources =
            new[]
            {
                "module Test exposing (..)\nvalue = 1",
                "module Test exposing (..)\nfirst =\n\nsecond = \"\\q\"",
                "port module Test exposing (..)\nport send : String -> Cmd msg",
                "module Test exposing (..)\n{-| documentation -}"
            };

        var expected = sources.Select(ElmSyntaxDiagnostics.Parse).ToArray();

        var results =
            Enumerable.Range(0, 64).AsParallel()
            .Select(
                index =>
                (Index: index % sources.Length, Diagnostics: ElmSyntaxDiagnostics.Parse(sources[index % sources.Length])))
            .ToArray();

        foreach (var result in results)
            result.Diagnostics.Should().Equal(expected[result.Index]);
    }

    [Theory]
    [InlineData("\n")]
    [InlineData("\r\n")]
    [InlineData("\r")]
    public void SyntaxError_lexical_failure_preserves_first_error_and_recovery_cursor(string newline)
    {
        var source =
            string.Join(
                newline,
                "module Test exposing (..)",
                "first = \"\\q\\u{}\"",
                "",
                "second = 01.2e+",
                "",
                "third = '😀'",
                "");

        var diagnostics = ElmSyntaxDiagnostics.Parse(source);

        diagnostics.Select(d => d.Error.Kind).Should().Equal(
            new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Escape(EscapeProblem.Unknown)),
            new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Number(NumberProblem.End, "01")));

        diagnostics.Select(d => d.Declaration).Should().Equal(
            new Range(new Location(2, 1), new Location(2, 17)),
            new Range(new Location(4, 1), new Location(4, 16)));

        diagnostics.Select(d => d.Error.Region).Should().Equal(
            new Range(new Location(2, 10), new Location(2, 12)),
            new Range(new Location(4, 16), new Location(4, 16)));

        ElmSyntaxDiagnostics.Parse(source).Should().Equal(diagnostics);
    }

    [Theory]
    [InlineData("void main() { gl_FragColor = vec4(1.0); }", null)]
    [InlineData("#version 100\n/* () */ void main() { // }\n gl_FragColor = vec4(1.0); }", null)]
    [InlineData("void main() {", ShaderSyntaxProblem.UnclosedDelimiter)]
    [InlineData("void main(]", ShaderSyntaxProblem.UnexpectedDelimiter)]
    [InlineData("uniform", ShaderSyntaxProblem.ExpectedDeclarator)]
    [InlineData("uniform float x", ShaderSyntaxProblem.UnfinishedDeclaration)]
    [InlineData("/* unclosed", ShaderSyntaxProblem.UnclosedDelimiter)]
    public void SyntaxError_shader_structure_uses_balanced_immutable_delimiters(
        string shader, ShaderSyntaxProblem? problem)
    {
        var source = "module Test exposing (..)\nvalue = [glsl|" + shader + "|]";
        var diagnostics = ElmSyntaxDiagnostics.Parse(source);

        if (problem is null)
            diagnostics.Should().BeEmpty();

        else
        {
            diagnostics.Single().Error.Kind.Should().BeOfType<ElmSyntaxErrorKind.Parse>()
                .Which.Problem.Should().BeOfType<ElmSyntaxProblem.Shader>()
                .Which.SyntaxProblem.Should().Be(problem);
        }
    }

    [Theory]
    [InlineData("whitespace")]
    [InlineData("string")]
    [InlineData("identifier")]
    [InlineData("comments")]
    [InlineData("list")]
    [InlineData("application")]
    [InlineData("type")]
    [InlineData("record")]
    [InlineData("declarations")]
    [InlineData("recovery")]
    public void SyntaxError_diagnostic_reader_handles_long_flat_inputs(string kind)
    {
        const int Count = 8192;
        static string Repeated(string text) => string.Concat(Enumerable.Repeat(text, Count));

        var body =
            kind switch
            {
                "whitespace" => Repeated(" \r\n") + "value = 1",
                "string" => "value = \"" + Repeated("😀x") + "\"",
                "identifier" => "value = a" + Repeated("b"),
                "comments" => Repeated("{-") + "nested" + Repeated("-}") + "\nvalue = 1",
                "list" => "value = [" + Repeated("1,") + "2]",
                "application" => "value = f " + Repeated("x "),
                "type" => "type alias Long = " + Repeated("Int -> ") + "Int",
                "record" => "value = {" + Repeated("field = 1,") + "last = 2}",
                "declarations" => Repeated("value = 1\n"),
                "recovery" => Repeated("value =\n\n"),

                _ =>
                throw new ArgumentOutOfRangeException(nameof(kind))
            };

        var diagnostics = ElmSyntaxDiagnostics.Parse("module Test exposing (..)\n\n" + body);

        diagnostics.Should().HaveCount(kind == "recovery" ? Count : 0);

        if (kind == "recovery")
        {
            diagnostics.Select(d => d.Declaration!.Start.Row).Should().Equal(
                Enumerable.Range(0, Count).Select(index => 3 + index * 2));
        }
    }

    private static string DescribeBranch(ElmSyntaxErrorKind kind)
    {
        switch (kind)
        {
            case ElmSyntaxErrorKind.Parse parse:
                return parse.Problem switch
                {
                    ElmSyntaxProblem.Expected => "Expected",
                    ElmSyntaxProblem.Grammar grammar => grammar.Branch.ToString(),
                    ElmSyntaxProblem.AnnotationNameMismatch => "DefNameMatch",
                    ElmSyntaxProblem.AnnotationNameRepeated => "DefNameRepeat",

                    ElmSyntaxProblem.Escape escape =>
                    escape.Problem is EscapeProblem.Unknown ? "CharEscape" : "String" + escape.Problem,

                    ElmSyntaxProblem.Number number =>
                    "Number" +
                    (number.Problem is NumberProblem.LeadingZero ? "LeadingZero" : number.Problem.ToString()),

                    ElmSyntaxProblem.Wildcard => "PatternWildcard",

                    ElmSyntaxProblem.Shader shader =>
                    shader.Problem is ShaderProblem.Endless ? "ShaderEndless" : "ShaderProblem",

                    _ =>
                    throw new NotImplementedException(
                        "DescribeBranch does not handle problem: " + parse.Problem.GetType().Name)
                };

            case ElmSyntaxErrorKind.ModuleValidation:
                throw new InvalidOperationException("Text parsing must not produce project policy diagnostics.");

            default:
                throw new NotImplementedException("DescribeBranch does not handle kind: " + kind.GetType().Name);
        }
    }

    private static string Canonical(string json) =>
        JsonNode.Parse(json)!.ToJsonString(new JsonSerializerOptions { WriteIndented = true });

    private static string MessageText(JsonNode? part) =>
        part is JsonObject ? part["string"]!.GetValue<string>() : part!.GetValue<string>();
}
