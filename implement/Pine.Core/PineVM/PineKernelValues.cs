namespace Pine.Core.PineVM;

/// <summary>
/// Reused instances of values often used by Pine kernel functions.
/// </summary>
public static class PineKernelValues
{
    /// <summary>
    /// Value returned by Pine kernel functions signalling 'true'.
    /// </summary>
    public static readonly PineValue TrueValue = PineValue.Blob([4]);

    /// <summary>
    /// Value returned by Pine kernel functions signalling 'false'.
    /// </summary>
    public static readonly PineValue FalseValue = PineValue.Blob([2]);
}
