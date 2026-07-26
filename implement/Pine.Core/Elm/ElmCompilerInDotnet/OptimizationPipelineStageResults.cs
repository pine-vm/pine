namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Holds the intermediate results of each stage in the optimization pipeline
/// (specialization, inlining, lambda re-lifting, operator lowering).
/// Each stage result is an <see cref="OptimizedElmSyntaxDeclarations"/> snapshot.
/// </summary>
internal record OptimizationPipelineStageResults(
    OptimizedElmSyntaxDeclarations AfterLowering);
