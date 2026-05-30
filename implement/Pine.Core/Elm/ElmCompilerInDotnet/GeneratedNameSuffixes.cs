namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Central catalogue of the synthetic name infixes / suffixes that
/// the optimization pipeline appends to original-source declaration
/// names when it emits derived siblings.
///
/// <para>
/// Centralising these strings serves two purposes:
/// </para>
///
/// <list type="number">
///   <item>
///     A single point of truth for the suffix contract: any pass that
///     needs to reason about "is this a generated decl?" or "what is
///     the original decl this derives from?" should consult these
///     constants rather than embed a literal that can silently
///     diverge from the producer's literal.
///   </item>
///   <item>
///     Searchability: <c>grep</c>-ing for one symbol surfaces the
///     full catalogue of synthetic naming conventions used across
///     <see cref="LambdaLifting"/>, <see cref="WrapperReturnStripping"/>,
///     <see cref="InliningFunctionSpecialization"/>, and
///     <see cref="DeclarationDeduplication"/>.
///   </item>
/// </list>
///
/// <para>
/// Plan reference:
/// <c>explore/internal-analysis/2026-05-18-eliminate-higher-order-parameters-in-focused-tests.md</c>
/// section 11.6.
/// </para>
/// </summary>
internal static class GeneratedNameSuffixes
{
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
    /// Suffix used by <see cref="WrapperReturnStripping"/> when
    /// emitting the sibling decl whose body is the original decl with
    /// the outermost newtype-wrapper constructor peeled off (the
    /// constructor is re-applied at every fully-saturated call site).
    /// </summary>
    public const string Stripped = "__stripped";

    /// <summary>
    /// Infix used by
    /// <see cref="InliningFunctionSpecialization.NameSpecializations"/>
    /// when emitting per-call-site specialised siblings. The full
    /// pattern is <c>&lt;originalDeclName&gt;__specialized__N</c> where
    /// <c>N</c> is a 1-based counter.
    /// </summary>
    public const string Specialized = "__specialized__";

    /// <summary>
    /// Sub-suffix used by <see cref="LambdaLifting"/> following the
    /// <see cref="Lifted"/> infix when promoting an anonymous lambda
    /// (<c>__lifted__lambdaN</c>). Surfaced as a separate constant so
    /// the lifted-name parser can recover the index without
    /// re-embedding the literal.
    /// </summary>
    public const string LiftedLambdaPrefix = "lambda";
}
