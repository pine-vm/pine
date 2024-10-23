using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace Pine.Elm;

public class AVH4ElmFormatBinaries
{
    public static readonly IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> ExecutableFileByOs =
        ImmutableDictionary<OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            OSPlatform.Linux,
            ("9acdd1006b9e4720f48cdbbb12f16262625c2a56145d56239f9c7b9a50ed0db4",
            @"https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-linux-x64.tgz"))
        .Add(
            OSPlatform.Windows,
            ("57b8f899f16e879338a6f0d898a13e28c440ab3947b173197de82048e3b520d6",
            @"https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-win-x64.zip"))
        .Add(
            OSPlatform.OSX,
            ("364469d9b64866e0595c9c2837eb330eeb1c58269d31567085fa24886b5a46d7",
            @"https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-mac-x64.tgz"));

    public static ReadOnlyMemory<byte>? ExecutableFileForCurrentOs() =>
        BlobLibrary.LoadFileForCurrentOs(ExecutableFileByOs);

    public static string RunElmFormat(string moduleTextBefore)
    {
        var executableFile = ExecutableFileForCurrentOs()!;

        var executableFileName = "elm-format" + ExecutableFile.ExecutableFileNameAppendix;

        var inputElmModuleFileName = "ElmModuleToFormat.elm";

        var outputElmModuleFileName = "ElmModuleFormatted.elm";

        var environmentFilesNotExecutable =
            ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty
            .SetItem([inputElmModuleFileName], Encoding.UTF8.GetBytes(moduleTextBefore));

        var executeBinaryResult =
            ExecutableFile.ExecuteFileWithArguments(
            environmentFilesNotExecutable: environmentFilesNotExecutable,
            executableFile: executableFile.Value,
            arguments: "--yes  --output \"" + outputElmModuleFileName + "\"  " + inputElmModuleFileName,
            environmentStrings: null,
            workingDirectoryRelative: null,
            environmentFilesExecutable: null,
            environmentPathExecutableFiles: null);

        if (executeBinaryResult.processOutput.ExitCode is not 0)
        {
            throw new Exception(
                string.Join(
                    "\n",
                    "Exit code " + executeBinaryResult.processOutput.ExitCode + " indicates failure.",
                    "Standard Output:",
                    executeBinaryResult.processOutput.StandardOutput,
                    "Standard Error:",
                    executeBinaryResult.processOutput.StandardError));
        }

        var resultFile =
            executeBinaryResult.resultingFiles
            .FirstOrDefault(c => c.path.SequenceEqual([outputElmModuleFileName]));

        if (resultFile.path is null)
        {
            throw new Exception("Did not find file " + outputElmModuleFileName);
        }

        return Encoding.UTF8.GetString(resultFile.content.Span);
    }
}
