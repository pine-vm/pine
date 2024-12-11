namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
/// </summary>
public record Diagnostic(
    Range Range,
    DiagnosticSeverity? Severity,
    string? Code,
    CodeDescription? CodeDescription,
    string? Source,
    string Message,
    int[]? Tags,
    DiagnosticRelatedInformation[]? RelatedInformation
    );

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeDescription
/// </summary>
public record CodeDescription(
    string Href);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticRelatedInformation
/// </summary>
public record DiagnosticRelatedInformation(
    Location Location,
    string Message);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticSeverity
/// </summary>
public enum DiagnosticSeverity
{
    /// <summary>
    /// Reports an error.
    /// </summary>
    Error = 1,

    /// <summary>
    /// Reports a warning.
    /// </summary>
    Warning = 2,

    /// <summary>
    /// Reports an information.
    /// </summary>
    Information = 3,

    /// <summary>
    /// Reports a hint.
    /// </summary>
    Hint = 4
}
