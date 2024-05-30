using System;

namespace Pine.PineVM;

public class ParseExpressionException(string message)
    : Exception(message)
{
}
