namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Configures the Elm syntax optimization stage of the compiler pipeline.
/// </summary>
public abstract record ElmSyntaxOptimizationConfig
{
    private ElmSyntaxOptimizationConfig()
    {
    }

    /// <summary>
    /// The optimization pipeline (specialization, inlining, operator lowering) is
    /// skipped entirely. The compiled environment is produced directly from the
    /// lambda-lifted output.
    /// </summary>
    public sealed record SyntaxOptimizationDisabled
        : ElmSyntaxOptimizationConfig;

    /// <summary>
    /// The optimization pipeline runs.
    /// </summary>
    public sealed record SyntaxOptimizationEnabled
        : ElmSyntaxOptimizationConfig;
}
