using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// High-level API for formatting Elm module source code.
/// This provides the shared formatting logic used by both the elm-format CLI command and the language server.
/// </summary>
public static class ElmFormat
{
    /// <summary>
    /// A syntax error encountered while parsing an Elm module that could otherwise still be
    /// formatted (for example an incomplete declaration in an otherwise parseable module).
    /// </summary>
    /// <param name="Location">The precise location (1-based row/column) where parsing failed.</param>
    /// <param name="Range">The range of the incomplete declaration the error belongs to.</param>
    /// <param name="Message">The error message describing why parsing failed.</param>
    public record ModuleSyntaxError(
        SyntaxModel.Location Location,
        SyntaxModel.Range Range,
        string Message);

    /// <summary>
    /// The result of formatting an Elm module, including any syntax errors that were
    /// recovered from while still producing a formatted rendering.
    /// </summary>
    /// <param name="FormattedText">The formatted source code.</param>
    /// <param name="SyntaxErrors">
    /// Syntax errors recovered from during parsing. Empty when the module parsed cleanly.
    /// </param>
    public record ModuleFormatResult(
        string FormattedText,
        IReadOnlyList<ModuleSyntaxError> SyntaxErrors);

    /// <summary>
    /// Formats Elm module source code, preserving the original linebreak style (LF or CRLF).
    /// </summary>
    /// <param name="moduleText">The Elm module source code to format.</param>
    /// <returns>
    /// A result containing either the formatted source code on success,
    /// or an error message describing why parsing/formatting failed.
    /// </returns>
    public static Result<ElmSyntaxParseError, string> FormatModuleText(string moduleText)
    {
        // Detect linebreak style from original content and use it for rendering
        var linebreakStyle =
            Rendering.DetectLinebreakStyle(moduleText) ?? LinebreakStyle.LF;

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
    public static Result<ElmSyntaxParseError, string> FormatModuleText(
        string moduleText,
        LinebreakStyle linebreakStyle)
    {
        var result = FormatModuleTextReportingSyntaxErrors(moduleText, linebreakStyle);

        if (result.IsErrOrNullable() is { } err)
        {
            return err;
        }

        if (result.IsOkOrNull() is not { } ok)
        {
            throw new System.NotImplementedException(
                "Unexpected null result from FormatModuleTextReportingSyntaxErrors: " + result.ToString());
        }

        return ok.FormattedText;
    }

    /// <summary>
    /// Formats Elm module source code, preserving the original linebreak style (LF or CRLF),
    /// and reports any syntax errors that were recovered from while still producing a formatted rendering.
    /// </summary>
    /// <param name="moduleText">The Elm module source code to format.</param>
    /// <returns>
    /// A result containing either the formatted source code together with the list of recovered
    /// syntax errors, or an error message describing why parsing/formatting failed entirely.
    /// </returns>
    public static Result<ElmSyntaxParseError, ModuleFormatResult> FormatModuleTextReportingSyntaxErrors(
        string moduleText)
    {
        var linebreakStyle =
            Rendering.DetectLinebreakStyle(moduleText) ?? LinebreakStyle.LF;

        return FormatModuleTextReportingSyntaxErrors(moduleText, linebreakStyle);
    }

    /// <summary>
    /// Formats Elm module source code using the specified linebreak style and reports any syntax
    /// errors that were recovered from while still producing a formatted rendering.
    /// </summary>
    /// <param name="moduleText">The Elm module source code to format.</param>
    /// <param name="linebreakStyle">The linebreak style to use in the output (LF or CRLF).</param>
    /// <returns>
    /// A result containing either the formatted source code together with the list of recovered
    /// syntax errors, or an error message describing why parsing/formatting failed entirely.
    /// </returns>
    public static Result<ElmSyntaxParseError, ModuleFormatResult> FormatModuleTextReportingSyntaxErrors(
        string moduleText,
        LinebreakStyle linebreakStyle)
    {
        var parseResult = ElmSyntaxParser.ParseModuleText(moduleText);

        if (parseResult.IsErrOrNullable() is { } parseErr)
        {
            return parseErr;
        }

        if (parseResult.IsOkOrNull() is not { } parsed)
        {
            throw new System.NotImplementedException(
                "Unexpected result type from ElmSyntaxParser.ParseModuleText: " + parseResult.ToString());
        }

        var formatted = Avh4Format.Format(parsed);

        var rendered = Rendering.ToString(formatted, linebreakStyle);

        IReadOnlyList<ModuleSyntaxError> syntaxErrors =
            [
            .. parsed.IncompleteDeclarations
            .Select(
                node =>
                new ModuleSyntaxError(
                    Location: node.Value.ParseError.Location,
                    Range: node.Range,
                    Message: node.Value.ParseError.Message))
            ];

        return new ModuleFormatResult(rendered, syntaxErrors);
    }
}
