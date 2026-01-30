namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// High-level API for formatting Elm module source code.
/// This provides the shared formatting logic used by both the elm-format CLI command and the language server.
/// </summary>
public static class ElmFormat
{
    /// <summary>
    /// Formats Elm module source code, preserving the original linebreak style (LF or CRLF).
    /// </summary>
    /// <param name="moduleText">The Elm module source code to format.</param>
    /// <returns>
    /// A result containing either the formatted source code on success,
    /// or an error message describing why parsing/formatting failed.
    /// </returns>
    public static Result<string, string> FormatModuleText(string moduleText)
    {
        // Detect linebreak style from original content and use it for rendering
        var linebreakStyle = Rendering.DetectLinebreakStyle(moduleText);

        return FormatModuleText(moduleText, linebreakStyle);
    }

    /// <summary>
    /// Formats Elm module source code using the specified linebreak style.
    /// </summary>
    /// <param name="moduleText">The Elm module source code to format.</param>
    /// <param name="linebreakStyle">The linebreak style to use in the output (LF or CRLF).</param>
    /// <returns>
    /// A result containing either the formatted source code on success,
    /// or an error message describing why parsing/formatting failed.
    /// </returns>
    public static Result<string, string> FormatModuleText(string moduleText, LinebreakStyle linebreakStyle)
    {
        var parseResult = ElmSyntaxParser.ParseModuleText(moduleText);

        if (parseResult.IsErrOrNull() is { } parseErr)
        {
            return Result<string, string>.err(parseErr);
        }

        if (parseResult.IsOkOrNull() is not { } parsed)
        {
            return Result<string, string>.err("Unexpected parse result type");
        }

        var formatted = Avh4Format.Format(parsed);

        var rendered = Rendering.ToString(formatted, linebreakStyle);

        return Result<string, string>.ok(rendered);
    }
}
