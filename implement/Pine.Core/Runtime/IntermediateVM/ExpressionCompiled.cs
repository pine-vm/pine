using Pine.Core.PineLanguage;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Delegate for observing each expression-cache entry that <see cref="PineVM"/>
/// compiles to IR (stack-frame) instructions.
/// <para />
/// Conceptually this is the per-entry signal of a "dispatch table snapshot": the
/// VM compiles each expression to IR the first time it is evaluated, then caches
/// the result. By recording every notification, an external tool can build (and
/// keep up to date) the set of post-reduction, post-env-class-specialization
/// expressions currently cached for execution.
/// Statically linked callees are reachable through
/// <see cref="DirectInvocation.LinkedStackFrameInstructions"/> and can compile
/// lazily without a separate notification.
/// </summary>
/// <param name="expressionCompiled">
/// A value-type snapshot describing the expression that was just compiled.
/// </param>
public delegate void ReportExpressionCompiled(
    in ExpressionCompiled expressionCompiled);

/// <summary>
/// Information about an <see cref="Expression"/> that has just been compiled to
/// IR instructions by <see cref="PineVM"/>.
/// </summary>
/// <param name="Identity">Runtime syntax with demand-driven canonical views and content hash.</param>
/// <param name="Compilation">
/// The compiled IR for the expression, including the generic body and any
/// env-class-specialized bodies.
/// </param>
public readonly record struct ExpressionCompiled(
    ExpressionIdentity Identity,
    ExpressionCompilation Compilation)
{
    /// <summary>Materializes the compiled root's canonical syntax only when read.</summary>
    public Expression Expression => Identity.CanonicalExpression;

    /// <summary>Computes the canonical value content hash only when read.</summary>
    public string ExpressionHashBase16 => Identity.HashBase16;
}
