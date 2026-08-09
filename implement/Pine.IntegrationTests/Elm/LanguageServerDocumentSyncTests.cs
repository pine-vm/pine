using AwesomeAssertions;
using Pine.Core.LanguageServerProtocol;
using Pine.Elm;
using System;
using System.IO;
using Xunit;

using ElmLanguageServer = Pine.Elm.LanguageServer;

namespace Pine.IntegrationTests.Elm;

public class LanguageServerDocumentSyncTests
{
    [Fact]
    public void DidChange_updates_hover_to_use_the_latest_valid_document()
    {
        const string OriginalContent =
            """
            module Main exposing (init, name)


            name = init


            init : Int
            init =
                0

            """;

        var tempDirectory =
            Path.Combine(Path.GetTempPath(), "pine-language-server-tests", Guid.NewGuid().ToString("N"));

        var filePath = Path.Combine(tempDirectory, "Main.elm");

        Directory.CreateDirectory(tempDirectory);
        File.WriteAllText(filePath, OriginalContent);

        try
        {
            var documentUri = new Uri(filePath).AbsoluteUri;
            var server = new ElmLanguageServer(logDelegate: null, elmPackagesSearchDirectories: []);
            var rpcTarget = new LanguageServerRpcTarget(server, LogDelegate: null);

            rpcTarget.Initialize(
                new InitializeParams(
                    ProcessId: Environment.ProcessId,
                    Capabilities: new ClientCapabilities(Workspace: null, TextDocument: null),
                    RootPath: tempDirectory,
                    RootUri: new Uri(tempDirectory).AbsoluteUri,
                    WorkspaceFolders: [],
                    ClientInfo: null));

            rpcTarget.TextDocument_didOpen(
                new TextDocumentItem(
                    Uri: documentUri,
                    LanguageId: "elm",
                    Version: 1,
                    Text: OriginalContent));

            HoverAtLine(rpcTarget, documentUri, line: 3).Contents
                .Should().Equal("    init : Int");

            var changedContent = "\n" + OriginalContent;

            rpcTarget.TextDocument_didChange(
                new VersionedTextDocumentIdentifier(documentUri, Version: 2),
                [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: changedContent)]);

            rpcTarget.TextDocument_documentSymbol(new TextDocumentIdentifier(documentUri))
                .Should().NotBeEmpty();

            rpcTarget.Workspace_didChangeWatchedFiles(
                [new FileEvent(documentUri, FileChangeType.Changed)]);

            rpcTarget.TextDocument_didChange(
                new VersionedTextDocumentIdentifier(documentUri, Version: 1),
                [new TextDocumentContentChangeEvent(Range: null, RangeLength: null, Text: OriginalContent)]);

            HoverAtLine(rpcTarget, documentUri, line: 4).Contents
                .Should().Equal("    init : Int");

            HoverAtLine(rpcTarget, documentUri, line: 3).Contents
                .Should().BeEmpty();

            rpcTarget.TextDocument_didClose(new TextDocumentIdentifier(documentUri));

            HoverAtLine(rpcTarget, documentUri, line: 3).Contents
                .Should().Equal("    init : Int");

            HoverAtLine(rpcTarget, documentUri, line: 4).Contents
                .Should().BeEmpty();
        }
        finally
        {
            Directory.Delete(tempDirectory, recursive: true);
        }
    }

    private static Hover HoverAtLine(LanguageServerRpcTarget rpcTarget, string documentUri, uint line)
    {
        return
            rpcTarget.TextDocument_hover(
                new TextDocumentPositionParams(
                    new TextDocumentIdentifier(documentUri),
                    new Position(Line: line, Character: 8)))
            ?? throw new InvalidOperationException("Expected a hover response");
    }
}
