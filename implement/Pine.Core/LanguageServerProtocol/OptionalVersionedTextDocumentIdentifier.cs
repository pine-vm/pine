namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#optionalVersionedTextDocumentIdentifier
/// </summary>
public record OptionalVersionedTextDocumentIdentifier(
    string Uri,
    /*
     * We need to explicitly specify instruct the JSON serializer to keep this,
     * because we configured the default like this:
     * DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
     * */
    [property: System.Text.Json.Serialization.JsonIgnore(
        Condition = System.Text.Json.Serialization.JsonIgnoreCondition.Never)]
    int? Version);

