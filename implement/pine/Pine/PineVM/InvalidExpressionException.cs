using System;

namespace Pine.PineVM;

public class InvalidExpressionException(string message)
    : Exception(message)
{
}
