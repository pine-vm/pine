using System;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Exception thrown when parsing a Pine expression fails.
/// </summary>
/// <param name="message">A message that describes the parse error.</param>
public class ParseExpressionException(string message)
    : Exception(message)
{
}
