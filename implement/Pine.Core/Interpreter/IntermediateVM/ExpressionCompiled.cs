namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Delegate for observing every <see cref="Expression"/> that <see cref="PineVM"/>
/// compiles to IR (stack-frame) instructions.
/// <para />
/// Conceptually this is the per-entry signal of a "dispatch table snapshot": the
/// VM compiles each expression to IR the first time it is evaluated, then caches
/// the result. By recording every notification, an external tool can build (and
/// keep up to date) the set of post-reduction, post-env-class-specialization
/// expressions currently cached for execution.
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
/// <param name="Expression">
/// The (post-reduction, post-env-class-specialization) expression that was
/// compiled to IR instructions.
/// </param>
/// <param name="ExpressionHashBase16">
/// Lower-case hex content hash of the expression value (as produced by the same
/// hashing pipeline the VM uses internally for cache-file naming).
/// </param>
/// <param name="Compilation">
/// The compiled IR for the expression, including the generic body and any
/// env-class-specialized bodies.
/// </param>
public readonly record struct ExpressionCompiled(
    Expression Expression,
    string ExpressionHashBase16,
    ExpressionCompilation Compilation);
