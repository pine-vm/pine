using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Functional;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;

using SyntaxTypes = Pine.Core.Elm.Syntax.Concrete;

namespace Pine.Core.Elm.Syntax.Interpreter;

/// <summary>
/// Loads interpreter sources without compiling them to Pine or acquiring packages. Callers supply
/// already-lowered sources; app compilation interfaces are handled by the app-lowering layer.
/// </summary>
public static class ElmInterpreterSourcePreparation
{
    /// <summary>
    /// Selects source modules and prepares an immutable interpreter environment. Null roots select
    /// all app and bundled modules; explicit roots select their import closure and implicit core imports.
    /// No app sources is a supported library-only environment.
    /// </summary>
    public static Result<string, ElmInterpreterEnvironment> Prepare(
        FileTree? sources,
        IReadOnlyList<IReadOnlyList<string>>? rootFilePaths = null,
        bool includeBundledKernelModules = true,
        bool includeTests = false) =>
        LoadModules(sources, rootFilePaths, includeBundledKernelModules, includeTests)
        .AndThen(modules => ElmInterpreterEnvironment.Create(modules));

    /// <summary>
    /// Prepares an already-resolved build without flattening package ownership or reselecting
    /// source directories. Namespace-rewritten syntax and source ranges are reused when available.
    /// Apply app lowering before dependency preparation when compilation interfaces are needed.
    /// </summary>
    public static Result<string, ElmInterpreterEnvironment> PrepareResolved(ElmResolvedBuild build)
    {
        if (!build.Resolution.Succeeded)
        {
            return
                "Cannot prepare an unsuccessful dependency resolution: " +
                string.Join("\n", build.Resolution.Failures.Select(failure => failure.Message));
        }

        return
            build.CompilerModuleSyntax.Count > 0
            ?
            ElmInterpreterEnvironment.Create(
                [
                .. build.CompilerModuleSyntax.OrderBy(item => item.Key, StringComparer.Ordinal).Select(item => item.Value)
                ])
            :
            ElmInterpreterEnvironment.Create(
                [
                    .. build.Sources.EnumerateFilesTransitive()
                    .Where(
                        file =>
                        file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) &&
                        !file.path.Contains("elm-stuff"))
                    .Select(file => Encoding.UTF8.GetString(file.fileContent.Span))
                ]);
    }

    /// <summary>
    /// Selects an application's declared source directories without parsing Elm bodies or loading
    /// packages. Validates the root manifest, including duplicate JSON properties and source paths.
    /// A tree without elm.json is treated as an explicit collection of source files.
    /// </summary>
    public static Result<string, FileTree> SelectAppSources(FileTree sources, bool includeTests = false)
    {
        if (sources.GetNodeAtPath(["elm.json"]) is null)
            return sources;

        try
        {
            var manifest = ElmDependencyResolver.ReadManifest(sources, ["elm.json"]);
            ElmDependencyResolver.ValidateManifestShape(manifest, "elm.json");
            return ElmResolvedBuildPreparation.SelectProjectSources(sources, ["elm.json"], manifest, includeTests);
        }
        catch (Exception exception) when (exception is JsonException or FormatException)
        {
            return "Invalid elm.json: " + exception.Message;
        }
    }

    /// <summary>
    /// Selects and parses sources for reuse by consumers that want their own environment policy.
    /// Root elm.json source directories are honored; elm-stuff and unrelated nested projects are excluded.
    /// App modules override bundled modules by declared module name, not by their filesystem path.
    /// Missing roots, duplicate app modules, and syntax errors are explicit failures.
    /// </summary>
    public static Result<string, IReadOnlyList<SyntaxTypes.File>> LoadModules(
        FileTree? sources,
        IReadOnlyList<IReadOnlyList<string>>? rootFilePaths = null,
        bool includeBundledKernelModules = true,
        bool includeTests = false)
    {
        sources ??= FileTree.EmptyTree;

        var selection = SelectAppSources(sources, includeTests);

        if (selection.IsErrOrNull() is { } selectionError)
            return selectionError;

        sources = selection.Extract(error => throw new InvalidOperationException(error));

        var modules = new Dictionary<string, (IReadOnlyList<string> path, string text)>(StringComparer.Ordinal);

        foreach (var file in sources.EnumerateFilesTransitive().Where(
            file =>
            file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) && !file.path.Contains("elm-stuff")))
        {
            var text = Encoding.UTF8.GetString(file.fileContent.Span);
            var header = ElmSyntaxParser.ParseModuleHeader(text);

            if (header.IsErrOrNull() is { } failure)
                return "Failed parsing module header at " + string.Join("/", file.path) + ": " + failure;

            var name =
                string.Join(
                    ".",
                    header.Extract(error => throw new InvalidOperationException(error.ToString())).ModuleName);

            if (!modules.TryAdd(name, (file.path, text)))
            {
                return
                    "Duplicate app module " + name + " at " + string.Join("/", file.path) +
                    " and " + string.Join("/", modules[name].path) + ".";
            }
        }

        var roots = new List<string>();

        if (rootFilePaths is not null)
        {
            foreach (var path in rootFilePaths)
            {
                var root = modules.Values.FirstOrDefault(module => module.path.SequenceEqual(path));

                if (root.text is null)
                    return "Selected Elm source file not found in the app source directories: " + string.Join("/", path);

                roots.Add(root.text);
            }
        }

        if (includeBundledKernelModules)
        {
            foreach (var file in BundledFiles.ElmKernelModulesDefault.Value.EnumerateFilesTransitive()
                .Where(file => file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase)))
            {
                var text = Encoding.UTF8.GetString(file.fileContent.Span);
                var name = ElmModule.ParseModuleName(text).Extract(error => throw new InvalidOperationException(error));
                modules.TryAdd(string.Join(".", name), (file.path, text));
            }
        }

        var texts =
            modules.OrderBy(module => module.Key, StringComparer.Ordinal).Select(module => module.Value.text).ToArray();

        var selected =
            rootFilePaths is null
            ?
            texts
            :
            ElmModule.ModulesTextOrderedForCompilationByDependencies(roots, texts);

        var pathsByText = modules.Values.ToDictionary(module => module.text, module => string.Join("/", module.path));
        var parsed = new List<SyntaxTypes.File>();

        foreach (var text in selected)
        {
            var result = ElmSyntaxParser.ParseModuleText(text);

            if (result.IsErrOrNullable() is { } failure)
                return "Failed parsing " + pathsByText[text] + ": " + failure;

            var file = result.Extract(error => throw new InvalidOperationException(error.ToString()));

            var errors =
                file.AdditionalParseErrors
                .Concat(file.IncompleteDeclarations.Select(node => node.Value.ParseError)).ToArray();

            if (errors.Length > 0)
            {
                return
                    "Invalid syntax in " + pathsByText[text] + ":\n" +
                    string.Join("\n", errors.Select(error => error.ToString()));
            }

            parsed.Add(file);
        }

        return parsed.ToImmutableArray();
    }
}
