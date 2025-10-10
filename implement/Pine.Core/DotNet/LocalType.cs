namespace Pine.Core.DotNet;

public enum LocalType
{
    /// <summary>
    /// Completely evaluated concrete <see cref="PineValue"/>
    /// </summary>
    Evaluated = 10,

    /// <summary>
    /// <see cref="Builtins.MutatingConcatBuilder"/>
    /// </summary>
    MutatingConcatBuilder = 20,
}
