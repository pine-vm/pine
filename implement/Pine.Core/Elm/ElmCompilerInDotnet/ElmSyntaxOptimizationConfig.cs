namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Configures the Elm syntax optimization stage of the compiler pipeline.
/// <para>
/// Modelled as a choice type so that mutually-exclusive configurations are
/// unrepresentable: when optimization is disabled there are no further knobs,
/// so combinations like <c>runSpecializationBeforeLambdaLifting</c> together
/// with a disabled optimization pipeline (which made no sense) can no longer be
/// expressed. The optimization-specific flags live on
/// <see cref="SyntaxOptimizationEnabled"/> only.
/// </para>
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
    /// The optimization pipeline runs. The carried flags configure the individual
    /// stages; see the documentation on
    /// <see cref="ElmCompiler.CompileInteractiveEnvironment"/> for the meaning of
    /// each one.
    /// </summary>
    /// <param name="MaxOptimizationRounds">
    /// Maximum number of optimization-pipeline iterations to run. The pipeline also
    /// stops early once a round produces no further changes.
    /// </param>
    /// <param name="SizeBasedInliningConfigOverride">
    /// Optional override for the size-based inlining configuration used in Phase 3
    /// of the optimization pipeline.
    /// </param>
    /// <param name="MaxSizeBasedInliningRounds">
    /// Maximum number of trailing size-based inlining rounds to run after the main
    /// convergence loop terminates.
    /// </param>
    /// <param name="RunSpecializationBeforeLambdaLifting">
    /// Experimental knob: when <see langword="true"/>, run up to
    /// <paramref name="MaxPreLiftingSpecializationRounds"/> combined
    /// specialization+inlining rounds on the canonicalized (not-yet-lambda-lifted)
    /// declaration dictionary before the standard initial lambda-lifting pass.
    /// </param>
    /// <param name="MaxPreLiftingSpecializationRounds">
    /// When <paramref name="RunSpecializationBeforeLambdaLifting"/> is
    /// <see langword="true"/>, the upper bound on the number of pre-lifting
    /// specialization rounds before lambda lifting runs.
    /// </param>
    /// <param name="InlineLetDestructureThunks">
    /// Experimental knob: when <see langword="true"/>, after the standard
    /// optimization pipeline finishes, run an additional let-destructure-thunk
    /// inlining + wrap/unwrap cancellation cleanup to a fixed point.
    /// </param>
    public sealed record SyntaxOptimizationEnabled(
        int MaxOptimizationRounds = ElmCompiler.OptimizationRoundsDefault,
        ElmSyntaxOptimization.SmallFunctionsConfig? SizeBasedInliningConfigOverride = null,
        int MaxSizeBasedInliningRounds = ElmCompiler.OptimizationRoundsDefault,
        bool RunSpecializationBeforeLambdaLifting = false,
        int MaxPreLiftingSpecializationRounds = ElmCompiler.OptimizationRoundsDefault,
        bool InlineLetDestructureThunks = false)
        : ElmSyntaxOptimizationConfig;
}
