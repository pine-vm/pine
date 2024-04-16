using System;

namespace Pine.PineVM;

public class GenericEvalException(string message)
    : Exception(message)
{
}
