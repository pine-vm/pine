using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
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

    public static string RunElmFormat(string moduleTextBefore)
    {
        var executableFile =
            BlobLibrary.LoadFileForCurrentOs(ExecutableFileByOs)
            ??
            throw new Exception("Failed to load elm-format executable file");

        if (!RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            System.IO.File.SetUnixFileMode(
                executableFile.cacheFilePath,
                ExecutableFile.UnixFileModeForExecutableFile);
        }

        var process = new Process
        {
            StartInfo = new ProcessStartInfo
            {
                FileName = executableFile.cacheFilePath,
                Arguments = "--stdin",
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true,
                StandardOutputEncoding = Encoding.UTF8,
                StandardErrorEncoding = Encoding.UTF8
            }
        };

        process.Start();

        using (var writer = new System.IO.StreamWriter(
            process.StandardInput.BaseStream,
            new UTF8Encoding(false),
            1024,
            leaveOpen: false))
        {
            // Ensure input ends with newline
            if (!moduleTextBefore.EndsWith("\n"))
            {
                moduleTextBefore += "\n";
            }

            writer.Write(moduleTextBefore);
            writer.Flush();
        }

        string output = process.StandardOutput.ReadToEnd();
        string error = process.StandardError.ReadToEnd();

        process.WaitForExit();

        if (process.ExitCode is not 0)
        {
            throw new Exception(
                string.Join(
                    "\n",
                    "Exit code " + process.ExitCode + " indicates failure.",
                    "Standard Output:",
                    output,
                    "Standard Error:",
                    error));
        }

        return output;
    }
}
