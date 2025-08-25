using ElmTime.Platform.WebService;
using Pine.Core;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;
using System.Threading.Tasks;

namespace ElmTime.Gui;

public class MakeGuiCache
{
    public static readonly Lazy<Task<Result<string, string>>> MakeGuiHtmlTask = new(() => Task.Run(MakeGuiHtml));

    public static Result<string, string> MakeGuiHtml()
    {
        var makeResult =
            LoadGuiElmProgramCodeFiles()
            .AndThen(elmProgramCodeFiles =>
            Program.Make(
                sourceFiles: elmProgramCodeFiles,
                workingDirectoryRelative: null,
                pathToFileWithElmEntryPoint: ["src", "Frontend", "Main.elm"],
                outputFileName: "index.html",
                elmMakeCommandAppendix: null));

        if (makeResult.IsErrOrNull() is { } err)
        {
            return Result<string, string>.err(err);
        }

        if (makeResult.IsOkOrNull() is not { } makeOk)
        {
            throw new NotImplementedException(
                "MakeGuiHtml: Unexpected result type: " + makeResult.GetType());
        }

        var producedFiles = makeOk.ProducedFiles;

        if (producedFiles is not BlobTreeWithStringPath.BlobNode blobNode)
        {
            throw new NotImplementedException(
                "MakeGuiHtml: Unexpected content in files produced by make: " + producedFiles.GetType());
        }

        return
            Result<string, string>.ok(Encoding.UTF8.GetString(blobNode.Bytes.Span));
    }

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadGuiElmProgramCodeFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["Gui", "elm"],
            assembly: typeof(StartupAdminInterface).Assembly);
}
