using Pine.Core.CodeAnalysis;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Describes how the return value of a specialized function relates to
/// the return value of the original (un-specialized) function. This
/// drives caller-side rewriting: at a fully-saturated call site of the
/// original function, callers must apply this transform to the
/// specialized function's result to obtain a value equivalent to the
/// original's result.
/// <para>
/// Currently used to model the wrapper-return stripping shape: the
/// generated sibling decl returns the inner (unwrapped) expression and
/// callers must re-introduce the original's outer single-tag
/// constructor — see <see cref="WrapWithConstructor"/>.
/// </para>
/// <para>
/// Mirrors <see cref="SiblingResultTransform"/>; the two will be
/// unified as part of the ongoing consolidation that derives
/// <see cref="GeneratedSiblingDecl"/> from
/// <see cref="FunctionSpecialization"/>.
/// </para>
/// </summary>
public abstract record ReturnValueSpecialization
{
    private ReturnValueSpecialization() { }

    /// <summary>
    /// The specialized function returns the same value as the original
    /// would have returned for the same arguments — callers use the
    /// result verbatim, no transformation required. This is the default
    /// for higher-order parameter specializations and tag-unwrap
    /// specializations, which never change the shape of the return
    /// value.
    /// </summary>
    public sealed record Identity : ReturnValueSpecialization
    {
        /// <summary>Singleton instance.</summary>
        public static readonly Identity Instance = new();
    }

    /// <summary>
    /// The specialized function returns the inner (unwrapped) value;
    /// callers must wrap the result with <see cref="Constructor"/> to
    /// obtain a value equivalent to the original's result.
    /// <para>
    /// This is the shape produced by
    /// <see cref="WrapperReturnStripping"/>: the sibling decl peels
    /// the original body's outer single-tag constructor application
    /// and the caller re-introduces it at every fully-saturated call
    /// site.
    /// </para>
    /// </summary>
    /// <param name="Constructor">
    /// Fully-qualified name of the single-tag constructor the caller
    /// must wrap the specialized function's result with.
    /// </param>
    public sealed record WrapWithConstructor(
        DeclQualifiedName Constructor)
        : ReturnValueSpecialization;
}
