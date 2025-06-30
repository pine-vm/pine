using System;

namespace Pine.ElmSyntax;

public class ElmSyntaxParserException : Exception
{
    public ElmSyntaxParserException(
        string message) : base(message)
    {
    }

    public ElmSyntaxParserException(
        string message, Exception innerException)
        : base(message, innerException)
    {
    }

    public ElmSyntaxParserException(
        string message,
        int lineNumber,
        int columnNumber)
        : base($"{message} at {lineNumber}:{columnNumber}")
    {
    }
}
