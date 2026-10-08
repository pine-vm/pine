using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.LanguageServer;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.LanguageServer;

public class ElmCompilerDiagnosticsProviderTests
{
    [Fact]
    public async Task Declaration_diagnostics_show_resolution_details_and_link_real_related_sources()
    {
        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["elm.json"], Manifest),
                    (["src", "Main.elm"], "module Main exposing (..)\nimport Supporting as S\nroot = S.hidden"),
                    (["src", "Supporting.elm"], "module Supporting exposing (visible)\nvisible = 1\nhidden = 42")
                ]);

        var provider =
            new ElmCompilerDiagnosticsProvider(
                workspace,
                new DocumentTexts(),
                (sources, manifestPath, token) => ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                    sources,
                    manifestPath,
                    [],
                    ElmPackageSubstitutions.DefaultBuild.Value,
                    cancellationToken: token));

        var diagnostics =
            (await provider.GetDiagnosticsAsync(
                VirtualWorkspace.DocumentUri("src", "Main.elm"),
                CancellationToken.None))
            .Extract(error => throw new System.Exception(error.Message));

        var main =
            diagnostics.Should().ContainSingle(
                document =>
                document.DocumentUri == VirtualWorkspace.DocumentUri("src", "Main.elm")).Which;

        var diagnostic = main.Diagnostics.Should().ContainSingle().Which;

        diagnostic.Message.Should().Contain("in declaration 'Main.root'")
            .And.Contain("declares value 'hidden', but does not expose it")
            .And.Contain("Reference scope: import at src/Main.elm:2:1")
            .And.NotContain("compilation root");

        diagnostic.RelatedInformation.Should().NotBeNull();

        diagnostic.RelatedInformation!.Should().Contain(
            location =>
            location.Location.Uri == VirtualWorkspace.DocumentUri("src", "Supporting.elm") &&
            location.Location.Range.Start.Line == 2);
    }

    private const string Manifest =
        """
        {
            "type": "application",
            "source-directories": ["src"],
            "elm-version": "0.19.1",
            "dependencies": { "direct": { "elm/core": "1.0.5" }, "indirect": {} },
            "test-dependencies": { "direct": {}, "indirect": {} }
        }
        """;

    private sealed class DocumentTexts : IDocumentTextSource
    {
        public Dictionary<string, string> Texts { get; } = [];

        public string? TryGetDocumentText(string documentUri) => Texts.GetValueOrDefault(documentUri);
    }

    [Fact]
    public async Task Declaration_demand_compiler_diagnostics_cover_disconnected_files_without_roots()
    {
        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["elm.json"], Manifest),
                    (["src", "Main.elm"], "module Main exposing (..)\nroot = missingMain"),
                    (["src", "Other.elm"], "module Other exposing (..)\nwrong : String\nwrong = 42")
                ]);

        var provider =
            new ElmCompilerDiagnosticsProvider(
                workspace,
                new DocumentTexts(),
                (sources, manifestPath, token) => ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                    sources,
                    manifestPath,
                    [],
                    ElmPackageSubstitutions.DefaultBuild.Value,
                    cancellationToken: token));

        var result =
            (await provider.GetDiagnosticsAsync(
                VirtualWorkspace.DocumentUri("src", "Main.elm"),
                CancellationToken.None))
            .Extract(error => throw new System.Exception(error.Message));

        result.Should().HaveCount(2);

        result.Should().Contain(
            document =>
            document.DocumentUri == VirtualWorkspace.DocumentUri("src", "Main.elm") &&
            document.Diagnostics.Count > 0);

        result.Should().Contain(
            document =>
            document.DocumentUri == VirtualWorkspace.DocumentUri("src", "Other.elm") &&
            document.Diagnostics.Count > 0);
    }

    [Fact]
    public async Task Declaration_demand_compiler_diagnostics_use_unsaved_sources_and_clear_fixed_errors()
    {
        var otherUri = VirtualWorkspace.DocumentUri("src", "Other.elm");

        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["elm.json"], Manifest),
                    (["src", "Main.elm"], "module Main exposing (..)\nroot = 42"),
                    (["src", "Other.elm"], "module Other exposing (..)\nroot = missing")
                ]);

        var texts = new DocumentTexts();

        var provider =
            new ElmCompilerDiagnosticsProvider(
                workspace,
                texts,
                (sources, manifestPath, token) => ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                    sources,
                    manifestPath,
                    [],
                    ElmPackageSubstitutions.DefaultBuild.Value,
                    cancellationToken: token));

        var before =
            (await provider.GetDiagnosticsAsync(otherUri, CancellationToken.None))
            .Extract(error => throw new System.Exception(error.Message));

        before.Should().Contain(document => document.DocumentUri == otherUri && document.Diagnostics.Count > 0);

        texts.Texts[otherUri] = "module Other exposing (..)\nroot = 42";

        var after =
            (await provider.GetDiagnosticsAsync(
                VirtualWorkspace.DocumentUri("src", "Main.elm"),
                CancellationToken.None))
            .Extract(error => throw new System.Exception(error.Message));

        after.Should().Contain(document => document.DocumentUri == otherUri && document.Diagnostics.Count == 0);

        var newUri = VirtualWorkspace.DocumentUri("src", "Unsaved.elm");
        texts.Texts[newUri] = "module Unsaved exposing (..)\nroot = notDefined";

        var unsaved =
            (await provider.GetDiagnosticsAsync(newUri, CancellationToken.None))
            .Extract(error => throw new System.Exception(error.Message));

        unsaved.Should().Contain(document => document.DocumentUri == newUri && document.Diagnostics.Count > 0);
    }
}
