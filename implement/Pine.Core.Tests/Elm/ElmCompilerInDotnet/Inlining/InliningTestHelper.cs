using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

using Inlining = Core.Elm.ElmCompilerInDotnet.Inlining;
using SyntaxV7 = Core.Elm.ElmSyntax.Stil4mElmSyntax7;

public class InliningTestHelper
{
    private static readonly ImmutableDictionary<QualifiedNameRef, QualifiedNameRef> s_renderingNameMap =
        ImmutableDictionary<QualifiedNameRef, QualifiedNameRef>.Empty
        .SetItem(QualifiedNameRef.FromFullName("Basics.Int"), QualifiedNameRef.FromFullName("Int"))
        .SetItem(QualifiedNameRef.FromFullName("Basics.Bool"), QualifiedNameRef.FromFullName("Bool"))
        .SetItem(QualifiedNameRef.FromFullName("String.String"), QualifiedNameRef.FromFullName("String"))
        .SetItem(QualifiedNameRef.FromFullName("Char.Char"), QualifiedNameRef.FromFullName("Char"));

    public static string RenderModuleForSnapshotTests(SyntaxV7.File module)
    {
        var mapped =
            NameMapper.MapNames(SyntaxV7.ToStil4mConcretized.ToConcretized(module), s_renderingNameMap);

        var formatted =
            SnapshotTestFormat.Format(mapped);

        var rendered =
            Rendering.ToString(formatted);

        return rendered;
    }

    public static SyntaxV7.File CanonicalizeAndInlineAndGetSingleModule(
        IReadOnlyList<string> elmModulesTexts,
        IReadOnlyList<string> moduleName,
        Inlining.Config config)
    {
        var parsedModules =
            elmModulesTexts
            .Select(text =>
                ElmSyntaxParser.ParseModuleText(text, enableMaxPreservation: true)
                .Extract(err => throw new System.Exception("Failed parsing: " + err)))
            .Select(SyntaxV7.FromStil4mConcretized.fromStil4mConcretized)
            .ToList();

        var canonicalizeResult =
            Canonicalization.Canonicalize(parsedModules);

        var modulesDict =
            canonicalizeResult
            .Extract(err => throw new System.Exception("Failed canonicalization: " + err));

        var allCanonicalizedModules =
            modulesDict
            .ToDictionary(
                kvp => kvp.Key,
                kvp => kvp.Value
                .Extract(err => throw new System.Exception($"Module {string.Join(".", kvp.Key)} has errors: " + err)));

        var allInlinedModules =
            Inlining.Inline(
                [.. allCanonicalizedModules.Values],
                config)
            .Extract(err => throw new System.Exception("Failed inlining: " + err));

        return allInlinedModules[moduleName];
    }
}
