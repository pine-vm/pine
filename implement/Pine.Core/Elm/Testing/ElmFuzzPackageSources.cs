using Pine.Core.DotNet;
using Pine.Core.Files;
using System;
using System.Collections.Immutable;

namespace Pine.Core.Elm.Testing;

internal static class ElmFuzzPackageSources
{
    public static ElmPackageSubstitution Test() =>
        ElmPackageSubstitution.Create(
            "elm-explorations/test",
            "pine-elm-test-2.2.1-fuzz-v2",
            ["2.2.0", "2.2.1"],
            Load("elm-test"),
            [
            "Test",
            "Expect",
            "Fuzz",
            "Test.Runner",
            "Test.Runner.Failure",
            "Test.Distribution"
            ]) with
        {
            ImplementationDependencies =
            ImmutableDictionary<string, string>.Empty
            .Add("elm/core", "1.0.0 <= v < 2.0.0")
            .Add("elm/random", "1.0.0 <= v < 2.0.0"),
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
