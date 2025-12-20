using System;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Exception thrown when parsing Elm syntax fails.
/// Used by <see cref="ElmSyntaxParser"/> to signal parsing errors with optional location information.
/// </summary>
public class ParserException : Exception
{
    /// <summary>
    /// Gets the line number where the parsing error occurred, or null if not available.
    /// </summary>
    public int? LineNumber { get; }

    /// <summary>
    /// Gets the column number where the parsing error occurred, or null if not available.
    /// </summary>
    public int? ColumnNumber { get; }

    /// <summary>
    /// Initializes a new instance of the <see cref="ParserException"/> class with a specified error message.
    /// </summary>
    /// <param name="message">The message that describes the error.</param>
    public ParserException(
        string message) : base(message)
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="ParserException"/> class with a specified error message
    /// and a reference to the inner exception that is the cause of this exception.
    /// </summary>
    /// <param name="message">The error message that explains the reason for the exception.</param>
    /// <param name="innerException">The exception that is the cause of the current exception.</param>
    public ParserException(
        string message, Exception innerException)
        : base(message, innerException)
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="ParserException"/> class with a specified error message
    /// and location information (line and column numbers).
    /// </summary>
    /// <param name="message">The error message that explains the reason for the exception.</param>
    /// <param name="lineNumber">The line number in the source text where the parsing error occurred.</param>
    /// <param name="columnNumber">The column number in the source text where the parsing error occurred.</param>
    public ParserException(
        string message,
        int lineNumber,
        int columnNumber)
        : base($"{message} at {lineNumber}:{columnNumber}")
    {
        LineNumber = lineNumber;
        ColumnNumber = columnNumber;
    }
}
