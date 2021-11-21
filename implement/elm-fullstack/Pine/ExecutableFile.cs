using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using Mono.Unix;

namespace Pine;

public class ExecutableFile
{
    public struct ProcessOutput
    {
        public string StandardError;

        public string StandardOutput;

        public int ExitCode;
    }

    static public (ProcessOutput processOutput, IReadOnlyCollection<(IImmutableList<string> path, IReadOnlyList<byte> content)> resultingFiles) ExecuteFileWithArguments(
        IReadOnlyDictionary<IImmutableList<string>, IReadOnlyList<byte>> environmentFilesNotExecutable,
        byte[] executableFile,
        string arguments,
        IDictionary<string, string> environmentStrings,
        IImmutableList<string> workingDirectory = null,
        IReadOnlyDictionary<IImmutableList<string>, IReadOnlyList<byte>> environmentFilesExecutable = null,
        IReadOnlyDictionary<string, IReadOnlyList<byte>> environmentPathExecutableFiles = null)
    {
        var environmentStringsDict =
            environmentStrings?.ToImmutableDictionary() ?? ImmutableDictionary<string, string>.Empty;

        var environmentPathContainerDirectoryName = "environment-path-cont";

        var containerDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

        string writeEnvironmentFile(KeyValuePair<IImmutableList<string>, IReadOnlyList<byte>> environmentFile)
        {
            var environmentFilePath = Path.Combine(containerDirectory, Filesystem.MakePlatformSpecificPath(environmentFile.Key));
            var environmentFileDirectory = Path.GetDirectoryName(environmentFilePath);

            Directory.CreateDirectory(environmentFileDirectory);

            File.WriteAllBytes(environmentFilePath, (environmentFile.Value as byte[]) ?? environmentFile.Value.ToArray());

            return environmentFilePath;
        }

        foreach (var environmentFile in environmentFilesNotExecutable)
            writeEnvironmentFile(environmentFile);

        var mainExecutableFileName = "name-used-to-execute-file.exe";
        var mainExecutableFilePathRelative = ImmutableList.Create(mainExecutableFileName);

        var executableFileNameAppendix =
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? ".exe" : "";

        var allExecutableFiles =
            (environmentFilesExecutable ?? ImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>>.Empty)
            .ToImmutableDictionary()
            .SetItems(
                (environmentPathExecutableFiles ?? ImmutableDictionary<string, IReadOnlyList<byte>>.Empty)
                .Select(execFile => new KeyValuePair<IImmutableList<string>, IReadOnlyList<byte>>(
                    ImmutableList.Create(environmentPathContainerDirectoryName, execFile.Key + executableFileNameAppendix), execFile.Value)))
            .SetItem(mainExecutableFilePathRelative, executableFile);

        foreach (var environmentFile in allExecutableFiles)
        {
            var fileAbsolutePath = writeEnvironmentFile(environmentFile);

            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            {
                var unixFileInfo = new UnixFileInfo(fileAbsolutePath);

                unixFileInfo.FileAccessPermissions |=
                    FileAccessPermissions.GroupExecute | FileAccessPermissions.UserExecute | FileAccessPermissions.OtherExecute |
                    FileAccessPermissions.GroupRead | FileAccessPermissions.UserRead | FileAccessPermissions.OtherRead;
            }
        }

        var workingDirectoryAbsolute =
            Path.Combine(
                containerDirectory,
                Filesystem.MakePlatformSpecificPath(workingDirectory ?? ImmutableList<string>.Empty));

        var mainExecutableFilePathAbsolute = Path.Combine(containerDirectory, mainExecutableFileName);

        var environmentPathExecutableFilesPathAbsolute = Path.Combine(containerDirectory, environmentPathContainerDirectoryName);

        var pathEnvironmentVarSeparator =
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? ";" : ":";

        var environmentPathEntryBefore =
            environmentStringsDict?.FirstOrDefault(c => c.Key.Equals("PATH", StringComparison.InvariantCultureIgnoreCase));

        var environmentPath = environmentPathExecutableFilesPathAbsolute + pathEnvironmentVarSeparator + environmentPathEntryBefore?.Value;

        var environmentStringsWithExecutableFiles =
            environmentStringsDict
            .SetItem(environmentPathEntryBefore?.Key ?? "PATH", environmentPath);

        var process = new Process
        {
            StartInfo = new ProcessStartInfo
            {
                WorkingDirectory = workingDirectoryAbsolute,
                FileName = mainExecutableFilePathAbsolute,
                Arguments = arguments,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
            },
        };

        foreach (var envString in environmentStringsWithExecutableFiles.EmptyIfNull())
            process.StartInfo.Environment[envString.Key] = envString.Value;

        process.Start();
        var standardOutput = process.StandardOutput.ReadToEnd();
        var standardError = process.StandardError.ReadToEnd();

        process.WaitForExit();
        var exitCode = process.ExitCode;
        process.Close();

        var createdFiles =
            Filesystem.GetFilesFromDirectory(
                directoryPath: containerDirectory,
                filterByRelativeName: path => !path.SequenceEqual(mainExecutableFilePathRelative));

        try
        {
            Directory.Delete(path: containerDirectory, recursive: true);
        }
        // Avoid crash in scenario like https://forum.botengine.org/t/farm-manager-tribal-wars-2-farmbot/3038/170
        catch (UnauthorizedAccessException)
        {
        }

        return (new ProcessOutput
        {
            ExitCode = exitCode,
            StandardError = standardError,
            StandardOutput = standardOutput,
        }, createdFiles);
    }

    /// <summary>
    /// Offer method with old interface for backwards-compatibility with assembly consumers.
    /// </summary>
    static public (ProcessOutput processOutput, IReadOnlyCollection<(IImmutableList<string> path, IReadOnlyList<byte> content)> resultingFiles) ExecuteFileWithArguments(
        IImmutableList<(IImmutableList<string> path, IReadOnlyList<byte> content)> environmentFiles,
        byte[] executableFile,
        string arguments,
        IDictionary<string, string> environmentStrings,
        IImmutableList<string> workingDirectory = null) =>
        ExecuteFileWithArguments(
            environmentFilesNotExecutable: Composition.ToFlatDictionaryWithPathComparer(environmentFiles),
            executableFile: executableFile,
            arguments: arguments,
            environmentStrings: environmentStrings,
            workingDirectory: workingDirectory);


    //  Helper for Linux platform. Thank you Kyle Spearrin!
    //  https://stackoverflow.com/questions/45132081/file-permissions-on-linux-unix-with-net-core/47918132#47918132
    static public void LinuxExecWithBash(string cmd)
    {
        var escapedArgs = cmd.Replace("\"", "\\\"");

        var process = new Process
        {
            StartInfo = new ProcessStartInfo
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true,
                WindowStyle = ProcessWindowStyle.Hidden,
                FileName = "/bin/bash",
                Arguments = $"-c \"{escapedArgs}\""
            }
        };

        process.Start();
        process.WaitForExit();

        Console.WriteLine(
            "Executed command with bash: " +
            Newtonsoft.Json.JsonConvert.SerializeObject(
                new
                {
                    cmd = cmd,
                    exitCode = process.ExitCode,
                    standardOut = process.StandardOutput.ReadToEnd(),
                    standardError = process.StandardError.ReadToEnd(),
                }));
    }
}
