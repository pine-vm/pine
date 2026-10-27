using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.Files;
using Pine.Core.Functional;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
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

    [Fact]
    public async Task Declaration_demand_compiler_diagnostics_accept_effect_types_without_executable_stubs()
    {
        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["elm.json"],
                    Manifest.Replace(
                        "\"elm/core\": \"1.0.5\"",
                        "\"elm/core\": \"1.0.5\", \"elm/time\": \"1.0.0\"")),
                    (["src", "Main.elm"],
                    """
                    module Main exposing (..)
                    import Task
                    import Time

                    type Event = TimeArrived Time.Posix

                    subscriptions : model -> Sub Event
                    subscriptions _ = Sub.batch [ Time.every 200 TimeArrived, Sub.none ]

                    update : Event -> model -> (model, Cmd Event)
                    update _ state = (state, Cmd.none)

                    now : Cmd Event
                    now = Task.perform TimeArrived Time.now
                    """)
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

        result.Should().ContainSingle().Which.Diagnostics.Should().BeEmpty();
    }

    [Fact]
    public async Task Declaration_demand_compiler_diagnostics_resolve_encoder_source_directories()
    {
        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["elm.json"],
                    Manifest.Replace("[\"src\"]", "[\"src\", \"encoders/src\"]").Replace(
                        "\"elm/core\": \"1.0.5\"",
                        "\"elm/core\": \"1.0.5\", \"elm/json\": \"1.1.3\"")),
                    (["src", "CompileElmApp.elm"],
                    """
                    module CompileElmApp exposing (..)
                    import Elm.Syntax.Encode.TypeAnnotation
                    import Json.Encode

                    parseJsonConverterDeclarationType annotation =
                        Json.Encode.encode 0 (Elm.Syntax.Encode.TypeAnnotation.encode annotation)

                    asCompletelyLoweredElmApp annotation = parseJsonConverterDeclarationType annotation
                    mapJsonConvertersModuleText annotation = parseJsonConverterDeclarationType annotation
                    """),
                    (["encoders", "src", "Elm", "Syntax", "Encode", "TypeAnnotation.elm"],
                    """
                    module Elm.Syntax.Encode.TypeAnnotation exposing (encode)
                    import Json.Encode

                    encode : String -> Json.Encode.Value
                    encode = Json.Encode.string
                    """)
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
                VirtualWorkspace.DocumentUri("src", "CompileElmApp.elm"),
                CancellationToken.None))
            .Extract(error => throw new System.Exception(error.Message));

        result.Should().HaveCount(2);
        result.Should().OnlyContain(document => document.Diagnostics.Count == 0);
    }

    private sealed class DocumentTexts : IDocumentTextSource
    {
        public Dictionary<string, string> Texts { get; } = [];

        public string? TryGetDocumentText(string documentUri) => Texts.GetValueOrDefault(documentUri);
    }

    [Theory]
    [InlineData("Decode.Value", "Encode.string \"hello\"", false)]
    [InlineData("Decode.Decoder Decode.Value", "Decode.value", false)]
    [InlineData("Decode.Value -> Encode.Value", "identity", false)]
    [InlineData("Encode.Value -> Decode.Value", "identity", false)]
    [InlineData("Value -> Encode.Value", "identity", false)]
    [InlineData("Encode.Value -> Value", "identity", false)]
    [InlineData("Encode.Value", "Decode.decodeString Decode.value \"null\" |> Result.withDefault Encode.null", false)]
    [InlineData("Value", "42", true)]
    [InlineData("Decode.Value", "\"hello\"", true)]
    [InlineData("Decode.Decoder Value", "42", true)]
    public async Task Declaration_demand_compiler_diagnostics_share_json_decode_and_encode_values(
        string type,
        string expression,
        bool expectTypeError)
    {
        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["elm.json"],
                    Manifest.Replace(
                        "\"elm/core\": \"1.0.5\"",
                        "\"elm/core\": \"1.0.5\", \"elm/json\": \"1.1.3\"")),
                    (["src", "Main.elm"],
                    $$"""
                    module Main exposing (..)
                    import Json.Decode as Decode exposing (Value)
                    import Json.Encode as Encode

                    value : {{type}}
                    value = {{expression}}
                    """)
                ]);

        ElmResolvedBuild? build = null;

        var provider =
            new ElmCompilerDiagnosticsProvider(
                workspace,
                new DocumentTexts(),
                async (sources, manifestPath, token) => build =
                    await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                        sources,
                        manifestPath,
                        [],
                        ElmPackageSubstitutions.DefaultBuild.Value,
                        cancellationToken: token));

        var result =
            (await provider.GetDiagnosticsAsync(
                VirtualWorkspace.DocumentUri("src", "Main.elm"),
                CancellationToken.None))
            .Extract(error => throw new Exception(error.Message));

        string[] expectedCodes = expectTypeError ? ["elm-type"] : [];

        result.Should().ContainSingle().Which.Diagnostics.Select(diagnostic => diagnostic.Code)
            .Should().Equal(expectedCodes);

        var name = DeclQualifiedName.Create(["Main"], "value");

        ElmCompiler.QueryDeclaration(build!, name).Extract(error => throw new Exception(error))
            .Diagnostics.Select(diagnostic => diagnostic.Code).Should().Equal(expectedCodes);

        if (!expectTypeError)
        {
            ElmCompiler.CompileResolvedEnvironment(build!, [name]).IsErrOrNull().Should().BeNull();
        }
    }

    [Fact]
    public async Task Declaration_demand_compiler_diagnostics_accept_virtual_dom_json_properties()
    {
        var virtualDom =
            ElmPackageSubstitution.Create(
                "elm/virtual-dom",
                "virtual-dom-type-interface-fixture",
                ["1.0.3"],
                FileTree.FromSetOfFilesWithStringPath(
                    [
                        (new[] { "src", "VirtualDom.elm" },
                        (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(
                            """
                            module VirtualDom exposing (Attribute, Node, node, property)
                            import Elm.Kernel.VirtualDom
                            import Json.Decode as Json

                            type Attribute msg = Attribute
                            type Node msg = Node

                            property : String -> Json.Value -> Attribute msg
                            property = Elm.Kernel.VirtualDom.property

                            node : String -> List (Attribute msg) -> List (Node msg) -> Node msg
                            node = Elm.Kernel.VirtualDom.node
                            """))
                    ]),
                ["VirtualDom"]) with
            {
                ImplementationDependencies =
                ImmutableDictionary<string, string>.Empty
                .Add("elm/core", "1.0.0 <= v < 2.0.0")
                .Add("elm/json", "1.0.0 <= v < 2.0.0")
            };

        var defaults = ElmPackageSubstitutions.DefaultBuild.Value;
        var configuration = defaults with { Substitutions = [.. defaults.Substitutions, virtualDom] };

        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["elm.json"],
                    Manifest.Replace(
                        "\"elm/core\": \"1.0.5\"",
                        "\"elm/core\": \"1.0.5\", \"elm/json\": \"1.1.3\", \"elm/virtual-dom\": \"1.0.3\"")),
                    (["src", "Main.elm"],
                    """
                    module Main exposing (..)
                    import Json.Encode as Encode
                    import Platform
                    import VirtualDom

                    type Event = Tick

                    main : Program () () Event
                    main = Platform.worker
                        { init = \_ -> ( (), Cmd.none )
                        , update = \_ state -> ( state, Cmd.none )
                        , subscriptions = subscriptions
                        }

                    subscriptions : () -> Sub Event
                    subscriptions state =
                        case view state of
                            _ -> Sub.none

                    view : () -> VirtualDom.Node Event
                    view _ = VirtualDom.node "input" [ VirtualDom.property "value" (Encode.string "hello") ] []
                    """)
                ]);

        ElmResolvedBuild? build = null;

        var provider =
            new ElmCompilerDiagnosticsProvider(
                workspace,
                new DocumentTexts(),
                async (sources, manifestPath, token) => build =
                    await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                        sources,
                        manifestPath,
                        [],
                        configuration,
                        cancellationToken: token));

        var result =
            (await provider.GetDiagnosticsAsync(
                VirtualWorkspace.DocumentUri("src", "Main.elm"),
                CancellationToken.None))
            .Extract(error => throw new Exception(error.Message));

        result.Should().ContainSingle().Which.Diagnostics.Should().BeEmpty();

        foreach (var name in new[] { "main", "subscriptions", "view" })
        {
            ElmCompiler.QueryDeclaration(build!, DeclQualifiedName.Create(["Main"], name))
                .Extract(error => throw new Exception(error)).Diagnostics.Should().BeEmpty();
        }

        ElmCompiler.CompileResolvedEnvironment(build!, [DeclQualifiedName.Create(["Main"], "view")])
            .IsErrOrNull().Should().Contain("Elm.Kernel.VirtualDom");
    }

    [Theory]
    [InlineData("List String -> Value", "JE.list JE.string")]
    [InlineData("String -> Value", "JE.string")]
    [InlineData("Value", "JE.string \"hello\"")]
    [InlineData("Value", "JE.object [ ( \"name\", JE.string \"hello\" ) ]")]
    public async Task Declaration_demand_compiler_diagnostics_accept_substituted_json_encoders(
        string type,
        string expression)
    {
        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["elm.json"],
                    Manifest.Replace(
                        "\"elm/core\": \"1.0.5\"",
                        "\"elm/core\": \"1.0.5\", \"elm/json\": \"1.1.3\"")),
                    (["src", "Main.elm"],
                    $$"""
                    module Main exposing (..)
                    import Json.Encode as JE exposing (Value)

                    encode : {{type}}
                    encode = {{expression}}
                    """)
                ]);

        ElmResolvedBuild? preparedBuild = null;

        var provider =
            new ElmCompilerDiagnosticsProvider(
                workspace,
                new DocumentTexts(),
                async (sources, manifestPath, token) => preparedBuild =
                    await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
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

        diagnostics.Should().ContainSingle().Which.Diagnostics.Should().BeEmpty();

        ElmCompiler.QueryDeclaration(preparedBuild!, DeclQualifiedName.Create(["Main"], "encode"))
            .Extract(error => throw new System.Exception(error))
            .Diagnostics.Should().BeEmpty();
    }

    [Theory]
    [InlineData("ParserFast")]
    [InlineData("Elm.Parser.Tokens")]
    public async Task Declaration_demand_compiler_diagnostics_normalize_bootstrap_runtime_strings(string moduleName)
    {
        var (workspace, _) =
            VirtualWorkspace.Create(
                [
                    (["elm.json"], Manifest),
                    (["src", "Main.elm"],
                    $$"""
                    module {{moduleName}} exposing (..)
                    type String = String Int

                    identity : String -> String
                    identity input = input

                    value : String
                    value = identity "hello"
                    """)
                ]);

        ElmResolvedBuild? preparedBuild = null;

        var provider =
            new ElmCompilerDiagnosticsProvider(
                workspace,
                new DocumentTexts(),
                async (sources, manifestPath, token) => preparedBuild =
                    await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
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

        diagnostics.Should().ContainSingle().Which.Diagnostics.Should().BeEmpty();

        ElmCompiler.QueryDeclaration(preparedBuild!, DeclQualifiedName.Create(moduleName.Split('.'), "value"))
            .Extract(error => throw new System.Exception(error))
            .Diagnostics.Should().BeEmpty();
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
