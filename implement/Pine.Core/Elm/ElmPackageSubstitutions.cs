using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm;

/// <summary>Explicit version aliases for Pine's bundled standard-library implementations.</summary>
public static class ElmPackageSubstitutions
{
    /// <summary>Conservative, explicit version aliases for the bundled standard-library sources.</summary>
    public static readonly Lazy<ElmDependencyResolutionConfiguration> DefaultBuild =
        new(() => new() { Substitutions = CreateBundledSubstitutions() });

    private static ImmutableArray<ElmPackageSubstitution> CreateBundledSubstitutions()
    {
        var bundled = BundledFiles.ElmKernelModulesDefault.Value;

        var coreModules =
            new[]
            {
                "Basics", "List", "Maybe", "Result", "String", "Char", "Tuple",
                "Array", "Dict", "Set", "Bitwise",
            };

        ElmPackageSubstitution Package(
            string name,
            string[] versions,
            string[] modules,
            params string[] privateModules)
        {
            var paths =
                modules.Concat(privateModules)
                .Select(
                    module =>
                    {
                        var segments = module.Split('.');
                        return segments.Take(segments.Length - 1).Append(segments[^1] + ".elm").ToArray();
                    })
                .ToArray();

            var sources =
                FileTree.FromSetOfFilesWithStringPath(
                    bundled.EnumerateFilesTransitive()
                    .Where(file => paths.Any(path => path.SequenceEqual(file.path)))
                    .Select(
                        file => ((IReadOnlyList<string>)["src", .. file.path], file.fileContent)));

            return
                ElmPackageSubstitution.Create(name, "pine-bundled:" + name, versions, sources, modules) with
                {
                    ImplementationDependencies =
                    name == "elm/core"
                    ?
                    []
                    :
                    ImmutableDictionary<string, string>.Empty.Add("elm/core", "1.0.0 <= v < 2.0.0"),
                };
        }

        return
            [
                Package("elm/core", ["1.0.5"], [.. coreModules, "Debug"]),
                Package("elm/bytes", ["1.0.8"], ["Bytes", "Bytes.Encode", "Bytes.Decode"]),
                Package("elm/json", ["1.1.3", "1.1.4"], ["Json.Encode", "Json.Decode"]),
                Package("elm/parser", ["1.1.0"], ["Parser", "Parser.Advanced"], "Elm.Kernel.Parser"),
                Package("elm/regex", ["1.0.0"], ["Regex"]),
                Package("elm/time", ["1.0.0"], ["Time"]),
                Package("elm/url", ["1.0.0"], ["Url", "Url.Parser", "Url.Parser.Query"], "Url.Parser.Internal"),
            ];
    }
}
