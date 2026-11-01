using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Files;
using Pine.Core.Functional;
using System;
using System.Collections.Generic;
using System.Linq;

namespace ElmTime.ElmInteractive;

/// <summary>App-source lowering and preparation shared by interpreter-backed apps and interactive scenarios.</summary>
public static class ElmInterpreterAppPreparation
{
    /// <summary>
    /// Lowers compilation interfaces before selecting and preparing interpreter modules.
    /// No Elm-to-Pine compiler environment is built. Supply package sources explicitly, or use
    /// the dependency resolver before this step. Lowering may invoke external build dependencies
    /// requested by compilation interfaces.
    /// </summary>
    public static Result<string, ElmInterpreterEnvironment> Prepare(
        FileTree? appSources,
        IReadOnlyList<IReadOnlyList<string>>? rootFilePaths = null,
        bool skipLowering = false,
        bool includeBundledKernelModules = true,
        bool includeTests = false) =>
        (skipLowering || appSources is null || rootFilePaths is { Count: 0 }
        ?
        Result<string, FileTree>.ok(appSources ?? FileTree.EmptyTree)
        :
        LowerAppSourceTree(appSources, rootFilePaths?.FirstOrDefault(), includeTests))
        .AndThen(
            sources =>
            ElmInterpreterSourcePreparation.Prepare(sources, rootFilePaths, includeBundledKernelModules, includeTests));

    /// <summary>
    /// Applies app compilation interfaces while retaining non-Elm assets needed by SourceFiles.
    /// Trees without a root manifest or without Elm sources need no lowering. Failures are returned
    /// to the consumer rather than printed or converted to an empty/successful environment.
    /// </summary>
    public static Result<string, FileTree> LowerAppSourceTree(
        FileTree sourceTree,
        IReadOnlyList<string>? compilationRootFilePath = null,
        bool includeTests = false)
    {
        if (sourceTree.GetNodeAtPath(["elm.json"]) is null)
            return sourceTree;

        var selection = ElmInterpreterSourcePreparation.SelectAppSources(sourceTree, includeTests);

        if (selection.IsErrOrNull() is { } selectionError)
            return selectionError;

        var selected = selection.Extract(error => throw new InvalidOperationException(error));

        var root =
            compilationRootFilePath ??
            selected.EnumerateFilesTransitive()
            .Where(
                file =>
                file.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) &&
                !file.path.Contains("elm-stuff"))
            .OrderBy(file => file.path.Count)
            .ThenBy(file => string.Join("/", file.path), StringComparer.Ordinal)
            .Select(file => file.path)
            .FirstOrDefault();

        if (root is null)
            return sourceTree;

        if (selected.GetNodeAtPath(root) is not FileTree.FileNode ||
            root.Count is 0 ||
            !root[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase) ||
            root.Contains("elm-stuff"))
        {
            return "Selected lowering root is not an app source file: " + string.Join("/", root);
        }

        return
            ElmAppCompilation.AsCompletelyLoweredElmApp(
                FileTreeExtensions.ToFlatDictionaryWithPathComparer(sourceTree),
                workingDirectoryRelative: [],
                ElmAppInterfaceConfig.Default with { CompilationRootFilePath = root })
            .MapError(ElmAppCompilation.CompileCompilationErrorsDisplayText)
            .Map(success => FileTree.FromSetOfFilesWithStringPath(success.Result.CompiledFiles));
    }
}
