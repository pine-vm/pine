namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Public configuration records for <see cref="ElmSyntaxOptimization"/>: <see cref="Config"/>
/// (the main toggle bag for the inlining/specialization pipeline), <see cref="SmallFunctionsConfig"/>
/// (size-based inlining thresholds), and <see cref="RewriteConfig"/> (independent toggles for the
/// specialization and classic-inlining rewrite passes).
/// Lives in a sibling partial-class file so the public configuration surface is discoverable
/// without scrolling through the multi-thousand-line driver in <c>ElmSyntaxOptimization.cs</c>.
/// </summary>
public partial class ElmSyntaxOptimization
{
    /// <summary>
    /// Configuration for the inlining pass, modelled as a product type with three
    /// independent properties:
    /// <list type="bullet">
    ///   <item><description><see cref="IncludeHigherOrder"/> — enable higher-order specialization
    ///     (inline applications that supply functions as arguments).</description></item>
    ///   <item><description><see cref="IncludePlainValues"/> — enable plain-value inlining
    ///     (substitute references to zero-parameter declarations whose body is safe to inline).</description></item>
    ///   <item><description><see cref="SmallFunctions"/> — when non-<see langword="null"/>, enable
    ///     size-based inlining of small non-recursive functions, parameterised by the thresholds
    ///     in <see cref="SmallFunctionsConfig"/>. <see langword="null"/> disables small-function inlining.</description></item>
    /// </list>
    /// The named static instances <see cref="OnlyFunctions"/>, <see cref="SmallFunctionsAndPlainValues"/>
    /// and <see cref="SpecializationAndSmallFunctions"/> map the previous
    /// closed-hierarchy variants onto this product type so call sites and tests
    /// can remain unchanged.
    /// </summary>
    public sealed record Config(
        bool IncludeHigherOrder,
        bool IncludePlainValues,
        SmallFunctionsConfig? SmallFunctions,
        bool WrapUnwrapCancellationEnabled = true)
    {
        /// <summary>
        /// Attempt inlining only for function applications which supply functions as arguments.
        /// </summary>
        public static readonly Config OnlyFunctions =
            new(IncludeHigherOrder: true, IncludePlainValues: false, SmallFunctions: null);

        /// <summary>
        /// Config that runs only the wrap/unwrap cancellation rewrite (Shapes A/B literal
        /// cancellation; no specialization, no inlining, no size-based passes). Intended
        /// primarily for unit tests that want to exercise wrap/unwrap cancellation in
        /// isolation via <see cref="ApplyWrapUnwrapCancellation(OptimizedElmSyntaxDeclarations, Config)"/>.
        /// </summary>
        public static readonly Config WrapUnwrapCancellationOnly =
            new(
                IncludeHigherOrder: false,
                IncludePlainValues: false,
                SmallFunctions: null,
                WrapUnwrapCancellationEnabled: true);

        /// <summary>
        /// Inline small non-recursive functions AND plain values (zero-parameter declarations
        /// with simple bodies). Scheduled as a single trailing round AFTER the
        /// { specialization ; higher-order inlining } convergence loop terminates (see
        /// <c>ElmCompiler.ApplyOptimizationPipelineWithStageResults</c> and the design note
        /// "Should size-based inlining be moved to the very end of lowering?" in
        /// <c>explore/elm-compiler-specializing-function-declarations.md</c>). At this point
        /// the AST is stable and size-based substitution cannot expose new higher-order
        /// patterns that the now-terminated loop would have matched on. See also
        /// <c>docs/2026-04-10-cascading-inlining-bug.md</c>.
        /// </summary>
        public static readonly Config SmallFunctionsAndPlainValues =
            new(IncludeHigherOrder: false, IncludePlainValues: true, SmallFunctions: SmallFunctionsConfig.Default);

        /// <summary>
        /// Combined config: perform higher-order specialization AND size-based inlining of
        /// small non-recursive functions / plain values in a single pass. This is useful
        /// when the AST is processed in one shot (e.g. inside a single optimization phase)
        /// and ensures that small predicate-like functions passed as arguments to recursive
        /// higher-order functions get both eliminated as higher-order parameters
        /// (via specialization) and inlined into the resulting first-order body.
        /// </summary>
        public static readonly Config SpecializationAndSmallFunctions =
            new(IncludeHigherOrder: true, IncludePlainValues: true, SmallFunctions: SmallFunctionsConfig.Default);
    }

    /// <summary>
    /// Thresholds and per-call-site policy for size-based inlining of small non-recursive functions.
    /// A function body is eligible for unconditional inlining only if its expression-node count
    /// is less than or equal to <see cref="MaxBodyNodeCount"/> AND it satisfies the additional
    /// hardcoded restrictions enforced inside the small-function inlining check (see
    /// <see cref="ElmSyntaxOptimization"/> for details).
    /// </summary>
    /// <param name="MaxBodyNodeCount">
    /// Maximum number of expression nodes in a function body for it to be considered "small" and
    /// eligible for unconditional inlining at every call site.
    /// </param>
    public sealed record SmallFunctionsConfig(int MaxBodyNodeCount)
    {
        /// <summary>
        /// Default thresholds used by <see cref="Config.SmallFunctionsAndPlainValues"/> and
        /// <see cref="Config.SpecializationAndSmallFunctions"/>. Mirrors the previous file-scoped
        /// <c>MaxSmallFunctionBodySize</c> constant.
        /// </summary>
        public static readonly SmallFunctionsConfig Default = new(MaxBodyNodeCount: 24);
    }

    /// <summary>
    /// Independent toggles for the post-passes that run inside
    /// <see cref="SpecializeAndInlineDeclarations(OptimizedElmSyntaxDeclarations, Config, RewriteConfig, StageToggles)"/>
    /// after the specialization+inlining rewrite has finished and before the result
    /// is returned. Production callers should use <see cref="Default"/>, which keeps
    /// every post-pass enabled (matching the legacy three-argument overload).
    ///
    /// <para>
    /// Intended for debugging investigations of optimization-stage bugs: by
    /// disabling individual post-passes a test can observe the intermediate
    /// declaration dictionary that would otherwise be hidden inside the
    /// stage. For example, disabling <see cref="LambdaLiftingEnabled"/> lets
    /// callers inspect (and re-execute through the Elm syntax interpreter)
    /// the raw inliner output containing the residual lambdas/local
    /// functions, which is useful for bisecting which sub-pass introduces
    /// an incorrect rewrite. Keep this knob available — future
    /// investigations of similar lowering-stage defects benefit from being
    /// able to single-step the stage.
    /// </para>
    /// </summary>
    /// <param name="LambdaLiftingEnabled">
    /// When <see langword="true"/>, run
    /// <see cref="LambdaLifting.LiftLambdas(OptimizedElmSyntaxDeclarations)"/>
    /// after the inliner. Disable to inspect the raw inliner output that still
    /// carries lambda/let-function nodes.
    /// </param>
    /// <param name="NormalizeApplicationsEnabled">
    /// When <see langword="true"/>, run
    /// <see cref="NormalizeApplicationsInDeclarationDictionary(OptimizedElmSyntaxDeclarations)"/>
    /// after lambda lifting. Disable to inspect nested-curried application chains
    /// produced by substitution-based rewrites.
    /// </param>
    /// <param name="WrapperReturnStrippingEnabled">
    /// When <see langword="true"/> AND <c>Config.SmallFunctions is null</c>, run the
    /// wrapper-return-stripping + wrap/unwrap-cancellation block. Disabling lets
    /// callers observe the pre-WRS shape of the dictionary, useful for confirming
    /// that a residual newtype wrap/unwrap pair survives the inliner unchanged.
    /// </param>
    public sealed record StageToggles(
        bool LambdaLiftingEnabled = true,
        bool NormalizeApplicationsEnabled = true,
        bool WrapperReturnStrippingEnabled = true)
    {
        /// <summary>
        /// Default toggles, matching production behaviour: every post-pass enabled.
        /// </summary>
        public static readonly StageToggles Default = new();

        /// <summary>
        /// Disable all post-passes: returns the raw inliner output without
        /// lambda lifting, application normalization, or wrapper-return stripping.
        /// </summary>
        public static readonly StageToggles InlinerOnly =
            new(
                LambdaLiftingEnabled: false,
                NormalizeApplicationsEnabled: false,
                WrapperReturnStrippingEnabled: false);
    }

    /// <summary>
    /// Independent toggles for the two rewrite passes performed by
    /// <see cref="SpecializeAndInlineDeclarationsCombined(OptimizedElmSyntaxDeclarations, Config)"/>: specialization (collecting and applying
    /// __specialized__N siblings) and classic inlining (substituting callee bodies
    /// at call sites). Either or both can be enabled.
    /// </summary>
    public record RewriteConfig(
        bool SpecializationEnabled,
        bool InliningEnabled)
    {
        /// <summary>
        /// Both specialization collection/emission and classic inlining are enabled.
        /// </summary>
        public static readonly RewriteConfig Combined = new(SpecializationEnabled: true, InliningEnabled: true);

        /// <summary>
        /// Only specialization (collect opportunities and emit <c>__specialized__N</c> siblings); no inlining.
        /// </summary>
        public static readonly RewriteConfig SpecializationOnly =
            new(SpecializationEnabled: true, InliningEnabled: false);

        /// <summary>
        /// Only classic inlining (substitute callee bodies at call sites); no specialization collection.
        /// </summary>
        public static readonly RewriteConfig InliningOnly = new(SpecializationEnabled: false, InliningEnabled: true);
    }
}
