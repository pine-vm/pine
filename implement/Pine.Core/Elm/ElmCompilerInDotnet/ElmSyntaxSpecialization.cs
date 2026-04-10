using System.Collections.Generic;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public static class ElmSyntaxSpecialization
{
    public static Result<string, IReadOnlyDictionary<ModuleName, SyntaxTypes.File>> Apply(
        IReadOnlyList<SyntaxTypes.File> modules,
        Inlining.Config config) =>
        Inlining.RunSpecializationStage(modules, config);
}
