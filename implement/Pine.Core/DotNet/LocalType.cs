namespace Pine.Core.DotNet;

/// <summary>
/// Locals can originate for example from common-subexpression elimination or mapping recursion to a loop.
/// Each local type implies a CLR type we used for the declaration of the local.
/// </summary>
public enum LocalType
{
    /// <summary>
    /// Completely evaluated concrete <see cref="PineValue"/>.
    /// </summary>
    Evaluated = 10,

    /// <summary>
    /// A deferred builder to track nested concatenations of lists and blobs.
    /// See <see cref="Builtins.ImmutableConcatBuilder"/>; caller should evaluate when a <see cref="PineValue"/> is required.
    /// </summary>
    ImmutableConcatBuilder = 20,

    /// <summary>
    /// A slice builder for efficient chained skip/take/head operations over lists and blobs.
    /// See <see cref="Builtins.ImmutableSliceBuilder"/>; caller should evaluate when a <see cref="PineValue"/> is required.
    /// </summary>
    ImmutableSliceBuilder = 30,
}
