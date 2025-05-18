using System.Collections.Generic;

namespace Pine.Core.LanguageServerProtocol;

/// <summary>
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#documentSymbol
/// </summary>
public record DocumentSymbol(
    string Name,
    string? Detail,
    SymbolKind Kind,
    Range Range,
    Range SelectionRange,
    IReadOnlyList<DocumentSymbol>? Children);

#pragma warning disable CS1591

/// <summary>
/// The various kinds of symbols in a document (as per LSP).
/// 
/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind
/// </summary>
public enum SymbolKind
{
    File = 1,

    Module = 2,

    Namespace = 3,

    Package = 4,

    Class = 5,

    /// <summary>
    /// Methods are often found in object-oriented or imperative programming languages coupling data and behavior.
    /// </summary>
    Method = 6,

    /// <summary>
    /// A property of a class or object (usually with getter/setter semantics).
    /// </summary>
    Property = 7,

    /// <summary>
    /// A field of a class or struct (data member).
    /// </summary>
    Field = 8,

    /// <summary>
    /// A special kind of method found in imperative programming languages.
    /// </summary>
    Constructor = 9,

    Enum = 10,

    Interface = 11,

    Function = 12,

    Variable = 13,

    Constant = 14,

    String = 15,

    Number = 16,

    Boolean = 17,

    Array = 18,

    Object = 19,

    Key = 20,

    Null = 21,

    EnumMember = 22,

    Struct = 23,

    Event = 24,

    Operator = 25,

    TypeParameter = 26
}

#pragma warning restore CS1591
