namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Central catalogue of the synthetic name infixes / suffixes that
/// the optimization pipeline appends to original-source declaration
/// names when it emits derived siblings.
/// </summary>
internal static class GeneratedNameSuffixes
{
    /// <summary>Prefix for collision-free IR bindings replacing record patterns.</summary>
    public const string RecordProjectionBindingPrefix = "record__projection_";

    /// <summary>
    /// Infix used by <see cref="LambdaLifting"/> when promoting a
    /// closure to a top-level decl. The full pattern is
    /// <c>&lt;containingFunctionName&gt;__lifted__lambdaN</c> for anonymous
    /// lambdas and
    /// <c>&lt;containingFunctionName&gt;__lifted__&lt;bindingName&gt;_N</c>
    /// for let-bound local functions.
    /// </summary>
    public const string Lifted = "__lifted__";

    /// <summary>
    /// Sub-suffix used by <see cref="LambdaLifting"/> following the
    /// <see cref="Lifted"/> infix when promoting an anonymous lambda
    /// (<c>__lifted__lambdaN</c>). Surfaced as a separate constant so
    /// the lifted-name parser can recover the index without
    /// re-embedding the literal.
    /// </summary>
    public const string LiftedLambdaPrefix = "lambda";
}
