using Pine.Core.DotNet;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmInElm;

/// <summary>
/// Provides access to the Elm source files bundled with the Pine.Core assembly.
/// These include the elm-in-elm compiler source and the elm-kernel-modules used as
/// the standard library source for the Pine Elm compiler.
/// </summary>
public class BundledFiles
{
    private const string ElmKernelModulesDirectoryName = "elm-kernel-modules";

    /// <summary>
    /// The default compiler source file tree, containing both the elm-in-elm compiler
    /// implementation and the elm-kernel-modules standard library source files.
    /// Loaded lazily from the embedded manifest resources of the Pine.Core assembly.
    /// </summary>
    public static readonly Lazy<FileTree> CompilerSourceContainerFilesDefault =
        new(
            () => FileTree.FromSetOfFilesWithStringPath(
                LoadElmCompilerSourceCodeFiles()
                .Extract(
                    error => throw new NotImplementedException(nameof(LoadElmCompilerSourceCodeFiles) + ": " + error))));

    /// <summary>
    /// The bundled <c>elm-kernel-modules</c> source files used as the standard library source
    /// for the Pine Elm compiler implementations.
    /// </summary>
    public static readonly Lazy<FileTree> ElmKernelModulesDefault =
        new(
            () => FileTree.FromSetOfFilesWithStringPath(
                LoadElmKernelModuleFiles()
                .Extract(
                    error => throw new NotImplementedException(nameof(LoadElmKernelModuleFiles) + ": " + error))));

    /// <summary>
    /// Loads all Elm compiler source code files from the manifest embedded file provider
    /// of the Pine.Core assembly, returning them as a dictionary from path to file content.
    /// The files reside under the <c>Elm/elm-in-elm</c> directory of the assembly.
    /// </summary>
    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadElmCompilerSourceCodeFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["Elm", "elm-in-elm"],
            assembly: typeof(BundledFiles).Assembly)
        .Map(files =>
            (IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>)files
            .Where(file => file.Key.Count > 0 && file.Key[0] != ElmKernelModulesDirectoryName)
            .ToImmutableDictionary(
                file => file.Key,
                file => file.Value,
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>()));

    /// <summary>
    /// Loads the bundled <c>elm-kernel-modules</c> source files from the manifest embedded file provider
    /// of the Pine.Core assembly.
    /// </summary>
    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadElmKernelModuleFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["Elm", "elm-in-elm", ElmKernelModulesDirectoryName],
            assembly: typeof(BundledFiles).Assembly);

}
