using Pine.Core.LanguageServerProtocol;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Identifies the document revision and declaration position represented by an unresolved CodeLens.
/// </summary>
public record CodeLensResolveData(
    string DocumentUri,
    Position Position,
    int? ClientVersion,
    long DocumentGeneration);
