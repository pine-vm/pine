using Pine.Core.DotNet;
using Pine.Core.Files;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.Testing;

internal static class ElmFuzzPackageSources
{
    public static ElmPackageSubstitution Test(bool legacy = false) =>
        ElmPackageSubstitution.Create(
            "elm-explorations/test",
            legacy ? "pine-elm-test-1.2.2-compat-v1" : "pine-elm-test-2.2.1-fuzz-v3",
            legacy ? ["1.2.2"] : ["2.2.0", "2.2.1"],
            TestSources(legacy),
            [
            "Test",
            "Expect",
            "Fuzz",
            "Test.Runner",
            "Test.Runner.Failure",
            "Test.Distribution",
            .. legacy ? new[] { "Test.Html.Event", "Test.Html.Query", "Test.Html.Selector" } : []
            ]) with
        {
            ImplementationDependencies =
            (legacy
            ?
            ImmutableDictionary<string, string>.Empty
            .Add("elm/html", "1.0.0 <= v < 2.0.0")
            .Add("elm/json", "1.0.0 <= v < 2.0.0")
            .Add("elm/virtual-dom", "1.0.0 <= v < 2.0.0")
            :
            [])
            .Add("elm/core", "1.0.0 <= v < 2.0.0")
            .Add("elm/random", "1.0.0 <= v < 2.0.0"),
        };

    private static FileTree TestSources(bool includeHtml) =>
        FileTree.FromSetOfFilesWithStringPath(
            Load("elm-test").EnumerateFilesTransitive()
            .Where(
                file =>
                includeHtml || file.path.Count < 3 ||
                file.path[0] != "src" || file.path[1] != "Test" ||
                file.path[2] is not ("Html" or "Internal"))
            .Select(file => (file.path, file.fileContent)));

    public static ElmPackageSubstitution VirtualDom() =>
        ElmPackageSubstitution.Create(
            "elm/virtual-dom",
            "pine-pure-virtual-dom-v1",
            ["1.0.3", "1.0.5"],
            Load("elm-virtual-dom"),
            ["VirtualDom", "VirtualDom.Pine"]) with
        {
            ImplementationDependencies =
            ImmutableDictionary<string, string>.Empty
            .Add("elm/core", "1.0.0 <= v < 2.0.0")
            .Add("elm/json", "1.0.0 <= v < 2.0.0"),
        };

    public static ElmPackageSubstitution Html() =>
        ElmPackageSubstitution.Create(
            "elm/html",
            "pine-pure-html-1.0.0-v1",
            ["1.0.0"],
            Load("elm-html"),
            ["Html", "Html.Attributes", "Html.Events", "Html.Keyed", "Html.Lazy"]) with
        {
            ImplementationDependencies =
            ImmutableDictionary<string, string>.Empty
            .Add("elm/core", "1.0.0 <= v < 2.0.0")
            .Add("elm/json", "1.0.0 <= v < 2.0.0")
            .Add("elm/virtual-dom", "1.0.0 <= v < 2.0.0"),
        };

    public static ElmPackageSubstitution Random() =>
        ElmPackageSubstitution.Create(
            "elm/random",
            "pine-pure-random-1.0.0-v1",
            [
            "1.0.0"
            ],
            Load("elm-random"),
            [
            "Random"
            ]) with
        {
            ImplementationDependencies =
            ImmutableDictionary<string, string>.Empty.Add("elm/core", "1.0.0 <= v < 2.0.0"),
        };

    private static FileTree Load(string directory) =>
        FileTree.FromSetOfFilesWithStringPath(
            DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
                ["Elm", "Testing", directory],
                typeof(ElmFuzzPackageSources).Assembly)
            .Extract(
                error => throw new InvalidOperationException("Cannot load bundled fuzz implementation: " + error)));
}
