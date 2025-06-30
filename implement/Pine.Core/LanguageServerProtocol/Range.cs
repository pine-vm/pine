namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range
/// 
/// A range in a text document expressed as (zero-based) start and end positions.
/// A range is comparable to a selection in an editor. Therefore, the end position is exclusive.
/// If you want to specify a range that contains a line including the line ending character(s) then
/// use an end position denoting the start of the next line.
/// </summary>
public record Range(
    Position Start,
    Position End);

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#position
/// 
/// Position in a text document expressed as zero-based line and zero-based character offset.
/// A position is between two characters like an ‘insert’ cursor in an editor.
/// Special values like for example -1 to denote the end of a line are not supported.
/// </summary>
public record Position(
    uint Line,
    uint Character);

