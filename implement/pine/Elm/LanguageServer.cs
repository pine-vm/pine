using Pine.Core.LanguageServerProtocol;

namespace Pine.Elm;

public class LanguageServer
{
    public InitializeResult Initialize(InitializeParams initializeParams)
    {
        return new InitializeResult
        (
            Capabilities: new ServerCapabilities
            {
                TextDocumentSync = TextDocumentSyncKind.Full,

                DocumentFormattingProvider = true
            }
        );
    }

}
