using ElmTime.Platform.WebService;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;
using System.Threading.Tasks;

namespace ElmTime.Gui;

public class MakeGuiCache
{
    public static readonly Lazy<Task<Result<string, string>>> MakeGuiHtmlTask = new(() => Task.Run(MakeGuiHtml));

    public static Result<string, string> MakeGuiHtml() =>
        LoadGuiElmProgramCodeFiles()
        .AndThen(elmProgramCodeFiles =>
        Program.Make(
            sourceFiles: elmProgramCodeFiles,
            workingDirectoryRelative: null,
            pathToFileWithElmEntryPoint: ["src", "Frontend", "Main.elm"],
            outputFileName: "index.html",
            elmMakeCommandAppendix: null))
        .Map(makeOk => Encoding.UTF8.GetString(makeOk.producedFile.Span));

    public static Result<string, IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>> LoadGuiElmProgramCodeFiles() =>
        DotNetAssembly.LoadDirectoryFilesFromManifestEmbeddedFileProviderAsDictionary(
            directoryPath: ["Gui", "elm"],
            assembly: typeof(StartupAdminInterface).Assembly);
}
