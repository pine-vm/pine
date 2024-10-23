using ElmTime.ElmSyntax;
using Pine.Core.LanguageServerProtocol;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Elm;

public class LanguageServer(System.Action<string>? logDelegate)
{
    private readonly ConcurrentDictionary<string, string> clientTextDocumentContents = new();

    private InitializeParams? initializeParams;

    private readonly System.Action<string>? logDelegate = logDelegate;

    private void Log(string message)
    {
        logDelegate?.Invoke(message);
    }

    public InitializeResult Initialize(InitializeParams initializeParams)
    {
        Log("Initialize: " + System.Text.Json.JsonSerializer.Serialize(initializeParams));

        this.initializeParams = initializeParams;

        return new InitializeResult
        (
            Capabilities: new ServerCapabilities
            {
                TextDocumentSync = TextDocumentSyncKind.Full,

                DocumentFormattingProvider = true,

                Workspace = new ServerCapabilitiesWorkspace
                {
                    WorkspaceFolders = new WorkspaceFoldersServerCapabilities(Supported: true, ChangeNotifications: true)
                }
            },
            ServerInfo: new ParticipentInfo(
                Name: "Pine language server",
                Version: ElmTime.Program.AppVersionId)
        );
    }

    public void Workspace_didChangeWorkspaceFolders(WorkspaceFoldersChangeEvent workspaceFoldersChangeEvent)
    {
        Log("Workspace_didChangeWorkspaceFolders (added " +
            workspaceFoldersChangeEvent.Added.Count + " and removed " + workspaceFoldersChangeEvent.Removed.Count + ")");
    }

    public void TextDocument_didOpen(TextDocumentItem textDocument)
    {
        Log("TextDocument_didOpen: " + textDocument.Uri);

        clientTextDocumentContents[textDocument.Uri] = textDocument.Text;
    }

    public void TextDocument_didChange(
        VersionedTextDocumentIdentifier textDocument,
        IReadOnlyList<TextDocumentContentChangeEvent> contentChanges)
    {
        Log(
            "TextDocument_didChange: " + textDocument.Uri +
            " (" + contentChanges.Count + " content changes)");

        for (var i = 0; i < contentChanges.Count; ++i)
        {
            var contentChange = contentChanges[i];

            Log("Content change " + i + ".range: " + contentChange.Range?.ToString() ?? "null");

            if (contentChange.Range is null)
            {
                clientTextDocumentContents[textDocument.Uri] = contentChange.Text;

                var linesCount = ElmModule.ModuleLines(contentChange.Text).Count();

                Log(
                    "Replaced all of " + textDocument.Uri + " with " + contentChange.Text.Length +
                    " chars distributed over " + linesCount + " lines");
            }
            else
            {
                // TODO

                Log("Failed to apply changes: not implemented");
            }
        }
    }

    public void TextDocument_didClose(TextDocumentIdentifier textDocument)
    {
        Log("TextDocument_didClose: " + textDocument.Uri);

        clientTextDocumentContents.TryRemove(textDocument.Uri, out var _);
    }

    public IReadOnlyList<TextEdit> TextDocument_formatting(
        TextDocumentIdentifier textDocument,
        FormattingOptions options)
    {
        Log("TextDocument_formatting: " + textDocument.Uri);

        clientTextDocumentContents.TryGetValue(textDocument.Uri, out var textDocumentContentBefore);

        if (textDocumentContentBefore is not null)
        {
            Log("Found document " + textDocument.Uri + " in client-managed state");
        }
        else
        {
            try
            {
                textDocumentContentBefore ??= System.IO.File.ReadAllText(textDocument.Uri);
            }
            catch (System.Exception e)
            {
                Log("Failed reading file: " + e);
            }
        }

        if (textDocumentContentBefore is null)
        {
            return [];
        }

        IReadOnlyList<string> linesBefore =
            [.. textDocumentContentBefore.ModuleLines()];

        TextEdit replaceWholeDocument(string newContent)
        {
            return new TextEdit(
                Range: new Range(
                    Start: new Position(Line: 0, Character: 0),
                    End: new Position(Line: (uint)linesBefore.Count, Character: 0)),
                NewText: newContent);
        }

        Log(
            "Document " + textDocument.Uri + " had " +
            linesBefore.Count + " lines and " + textDocumentContentBefore.Length + " chars before");

        var newContent =
            TextDocument_formatting_lessStore(
                textDocument,
                textDocumentContentBefore);

        if (newContent is null)
            return [];

        if (clientTextDocumentContents.ContainsKey(textDocument.Uri))
        {
            clientTextDocumentContents[textDocument.Uri] = newContent;
        }

        return
            [replaceWholeDocument(newContent)
            ];
    }

    public string? TextDocument_formatting_lessStore(
        TextDocumentIdentifier textDocument,
        string textDocumentContentBefore)
    {
        if (textDocument.Uri.EndsWith(".elm"))
        {
            try
            {
                var binaryClock = System.Diagnostics.Stopwatch.StartNew();

                var elmFormatted =
                    AVH4ElmFormatBinaries.RunElmFormat(textDocumentContentBefore);

                binaryClock.Stop();

                Log("Completed elm-format on " + textDocument.Uri +
                    " in " + (int)binaryClock.Elapsed.TotalMilliseconds + " ms");

                return elmFormatted;
            }
            catch (System.Exception e)
            {
                Log("Error: Failed running elm-format: " + e);
            }
        }

        return null;
    }
}
