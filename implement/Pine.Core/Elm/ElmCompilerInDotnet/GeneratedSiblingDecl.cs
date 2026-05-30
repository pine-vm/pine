using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Metadata describing a generated sibling function declaration and its
/// relationship to the original declaration it was derived from.
/// <para>
/// Captured shape:
/// </para>
/// <list type="bullet">
///   <item><description>
///     Per-parameter mapping (<see cref="ParameterOrigins"/>) — how each
///     sibling parameter relates to the original's parameters or to
///     pieces of them (e.g. inner field of a newtype-wrapped argument).
///   </description></item>
///   <item><description>
///     Result-transform (<see cref="ResultTransform"/>) — what the
///     caller must do to the sibling's result to obtain a value
///     equivalent to the original's result (e.g. re-wrap with the
///     original's outer constructor).
///   </description></item>
/// </list>
/// <para>
/// Producers / consumers:
/// </para>
/// <list type="bullet">
///   <item><description>
///     <see cref="WrapperReturnStripping"/> (D2 Step 2) — produces
///     <c>&lt;f&gt;__stripped</c> siblings whose parameter origins are
///     all <see cref="SiblingParameterOrigin.Identity"/> and whose
///     result transform is
///     <see cref="SiblingResultTransform.WrapWithConstructor"/>.
///   </description></item>
///   <item><description>
///     <see cref="WrapUnwrapCancellation"/> (S1) — consumes this
///     metadata to recognize cancellable wrap/unwrap pairs at
///     fully-saturated call sites: when the original's
///     <see cref="ResultTransform"/> wraps with constructor <c>C</c>
///     and the call site immediately destructures the result on the
///     same <c>C</c>, the wrap and the unwrap cancel and the call is
///     forwarded to the sibling directly.
///   </description></item>
///   <item><description>
///     Future Shape D / S2 / S3 passes — emit siblings with non-trivial
///     parameter mappings (e.g. <see cref="SiblingParameterOrigin.InnerOfWrapper"/>
///     for function-arg destructure rewrites, or tuple-element origins
///     for tuple-arg flattening).
///   </description></item>
/// </list>
/// <para>
/// See <c>explore/internal-analysis/2026-05-15-s1-cancel-parser-newtype-wrap-unwrap.md</c>
/// section "API sketch" for the design rationale.
/// </para>
/// </summary>
internal sealed record GeneratedSiblingDecl(
    DeclQualifiedName OriginalDeclName,
    DeclQualifiedName SiblingDeclName,
    int OriginalArity,
    IReadOnlyList<SiblingParameterOrigin> ParameterOrigins,
    SiblingResultTransform ResultTransform)
{
    /// <summary>
    /// Number of parameters the generated sibling declares — equal to
    /// <see cref="ParameterOrigins"/>'s length by construction.
    /// </summary>
    public int SiblingArity => ParameterOrigins.Count;

    /// <inheritdoc/>
    public bool Equals(GeneratedSiblingDecl? other)
    {
        if (other is null)
            return false;

        return
            OriginalDeclName.Equals(other.OriginalDeclName) &&
            SiblingDeclName.Equals(other.SiblingDeclName) &&
            OriginalArity == other.OriginalArity &&
            ParameterOrigins.SequenceEqual(other.ParameterOrigins) &&
            ResultTransform.Equals(other.ResultTransform);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();
        hashCode.Add(OriginalDeclName);
        hashCode.Add(SiblingDeclName);
        hashCode.Add(OriginalArity);

        foreach (var origin in ParameterOrigins)
            hashCode.Add(origin);

        hashCode.Add(ResultTransform);
        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Where one sibling parameter (at <see cref="SiblingIndex"/>) "comes
/// from" in the original declaration's parameter list. Drives call-site
/// rewriting: at a fully-saturated call site of the original,
/// <see cref="SiblingParameterOrigin"/> says how to derive the
/// corresponding argument for the sibling.
/// </summary>
internal abstract record SiblingParameterOrigin
{
    /// <summary>The sibling's parameter index this origin describes.</summary>
    public abstract int SiblingIndex { get; init; }

    /// <summary>
    /// The original's parameter index this origin draws from. For
    /// origins that draw from a sub-piece of the original's parameter
    /// (e.g. <see cref="InnerOfWrapper"/>), this is the index of the
    /// outer parameter, not the inner field.
    /// </summary>
    public abstract int OriginalIndex { get; init; }

    /// <summary>
    /// The sibling parameter at <see cref="SiblingIndex"/> is
    /// positionally identical to the original's parameter at
    /// <see cref="OriginalIndex"/> — same value, no transformation
    /// applied at the call site.
    /// </summary>
    public sealed record Identity(
        int SiblingIndex,
        int OriginalIndex)
        : SiblingParameterOrigin;

    /// <summary>
    /// The sibling parameter at <see cref="SiblingIndex"/> is the inner
    /// field bound by a single-arg
    /// <see cref="ElmSyntax.Stil4mElmSyntax7.Pattern.NamedPattern"/>
    /// that destructures the original's parameter at
    /// <see cref="OriginalIndex"/> on
    /// <see cref="WrapperConstructor"/> (a registered newtype-shaped
    /// constructor — single ctor, single arg).
    /// <para>
    /// At a fully-saturated call site, the corresponding original
    /// argument must be of the form
    /// <c>WrapperConstructor inner</c> for the rewrite to be sound;
    /// the sibling then receives <c>inner</c>. Mixed call sites
    /// (some wrapped, some not) keep the original alive and only
    /// rewrite the wrap-rooted sites.
    /// </para>
    /// <para>
    /// Reserved for the future Shape D follow-up
    /// (function-arg destructure pattern); not yet emitted by any
    /// current pass.
    /// </para>
    /// </summary>
    public sealed record InnerOfWrapper(
        int SiblingIndex,
        int OriginalIndex,
        DeclQualifiedName WrapperConstructor)
        : SiblingParameterOrigin;
}

/// <summary>
/// What transformation a caller must apply to the sibling's result to
/// obtain a value equivalent to the original's result.
/// </summary>
internal abstract record SiblingResultTransform
{
    /// <summary>
    /// Sibling and original return the same value — the caller uses
    /// the sibling's result verbatim.
    /// </summary>
    public sealed record Identity : SiblingResultTransform;

    /// <summary>
    /// Caller must wrap the sibling's result with
    /// <see cref="Constructor"/> to obtain a value equivalent to the
    /// original's result. This is the shape produced by
    /// <see cref="WrapperReturnStripping"/>: the sibling returns the
    /// inner expression and the caller re-introduces the original's
    /// outer constructor.
    /// </summary>
    public sealed record WrapWithConstructor(
        DeclQualifiedName Constructor)
        : SiblingResultTransform;
}
