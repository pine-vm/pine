using Pine.Core.DotNet;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmInElm;

public class BundledFiles
{
    static public readonly Lazy<FileTree> CompilerSourceContainerFilesDefault =
        new(() => FileTree.FromSetOfFilesWithStringPath(
            LoadElmCompilerSourceCodeFiles()
            .Extract(error => throw new NotImplementedException(nameof(LoadElmCompilerSourceCodeFiles) + ": " + error))));

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadElmCompilerSourceCodeFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["Elm", "elm-in-elm"],
            assembly: typeof(BundledFiles).Assembly);

}
