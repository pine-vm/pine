using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Holds the intermediate results of each stage in the optimization pipeline
/// (specialization, inlining, lambda re-lifting, operator lowering).
/// Each stage result is an <see cref="OptimizedElmSyntaxDeclarations"/> snapshot.
/// The <see cref="Iterations"/> list contains per-iteration intermediate results
/// when the pipeline runs more than one round.
/// </summary>
internal record OptimizationPipelineStageResults(
    OptimizedElmSyntaxDeclarations AfterSpecialization,
    OptimizedElmSyntaxDeclarations AfterLowering,
    ImmutableList<OptimizationIterationStageResults> Iterations);
