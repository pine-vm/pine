using Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxAbstract;

/// <summary>
/// Helpers rendering the abstract Elm syntax model into a canonical textual form
/// used by snapshot-style tests (e.g. operator lowering).
/// </summary>
public static class ElmSyntaxAbstractTestHelper
{
    /// <summary>
    /// Renders the given abstract syntax <see cref="File"/> into a canonical,
    /// elm-format-inspired textual representation suitable for snapshot assertions.
    /// </summary>
    public static string RenderModuleForSnapshotTests(File module)
    {
        var moduleStil4mElmSyntax7 =
            Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(
                ConvertToConcrete.FromFile(module));

        return InliningTestHelper.RenderModuleForSnapshotTests(moduleStil4mElmSyntax7);
    }
}
