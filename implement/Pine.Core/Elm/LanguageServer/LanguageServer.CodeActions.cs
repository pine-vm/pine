using Pine.Core.Elm.Syntax;
using Pine.Core.LanguageServerProtocol;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

namespace Pine.Core.Elm.LanguageServer;

public partial class LanguageServer
{
    /// <summary>Offers syntax fixes against the current editor document version.</summary>
    public IReadOnlyList<CodeAction> TextDocument_codeAction(
        CodeActionParams parameters,
        CancellationToken cancellationToken = default)
    {
        const string Kind = "quickfix";
        cancellationToken.ThrowIfCancellationRequested();

        if (parameters.Context.Only is { Count: > 0 } only && !only.Contains(Kind))
            return [];

        var uri = DocumentUriCleaned(parameters.TextDocument.Uri);

        if (!uri.EndsWith(".elm", System.StringComparison.OrdinalIgnoreCase))
            return [];

        string text;
        int version;

        lock (_documentStateLock)
        {
            if (!_clientTextDocumentContents.TryGetValue(uri, out var openText) ||
                !_clientTextDocumentVersions.TryGetValue(uri, out version))
            {
                return [];
            }

            text = openText;
        }

        var errors = ElmSyntaxErrorRenderer.CollectErrors(ElmSyntaxParser.ParseModuleText(text));
        cancellationToken.ThrowIfCancellationRequested();

        var actions = new List<CodeAction>();

        foreach (var error in errors)
        {
            if (ElmSyntaxErrorRenderer.UnicodeEscapeReplacement(error) is not { } replacement)
                continue;

            var range = SyntaxDiagnosticRange(error);
            var endComparedToStart = ComparePositions(parameters.Range.End, range.Start);

            if (ComparePositions(parameters.Range.Start, range.End) >= 0 ||
                endComparedToStart < 0 ||
                endComparedToStart is 0 && parameters.Range.Start != parameters.Range.End)
            {
                continue;
            }

            var edit = new TextDocumentEdit(new(parameters.TextDocument.Uri, version), [new(range, replacement)]);

            actions.Add(
                new CodeAction(
                    "Replace with " + replacement,
                    Kind,
                    new WorkspaceEdit([edit])));
        }

        return actions;
    }
}
