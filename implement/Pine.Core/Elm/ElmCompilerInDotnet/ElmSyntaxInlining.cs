using System.Collections.Generic;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public static class ElmSyntaxInlining
{
    public static Result<string, IReadOnlyDictionary<ModuleName, SyntaxTypes.File>> Apply(
        IReadOnlyList<SyntaxTypes.File> modules,
        Inlining.Config config) =>
        Inlining.RunInliningStage(modules, config);
}
