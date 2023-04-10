using Mono.Unix;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;

namespace Pine;

public class ExecutableFile
{
    public record ProcessOutput(
        string StandardError,
        string StandardOutput,
        int ExitCode);

    public static (ProcessOutput processOutput, IReadOnlyCollection<(IReadOnlyList<string> path, ReadOnlyMemory<byte> content)> resultingFiles) ExecuteFileWithArguments(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> environmentFilesNotExecutable,
        ReadOnlyMemory<byte> executableFile,
        string arguments,
        IDictionary<string, string>? environmentStrings,
        IReadOnlyList<string>? workingDirectoryRelative = null,
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? environmentFilesExecutable = null,
        IReadOnlyDictionary<string, ReadOnlyMemory<byte>>? environmentPathExecutableFiles = null)
    {
        var environmentStringsDict =
            environmentStrings?.ToImmutableDictionary() ?? ImmutableDictionary<string, string>.Empty;

        const string environmentPathContainerDirectoryName = "environment-path-cont";

        var containerDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

        string writeEnvironmentFile(KeyValuePair<IReadOnlyList<string>, ReadOnlyMemory<byte>> environmentFile)
        {
            var environmentFilePath = Path.Combine(containerDirectory, Filesystem.MakePlatformSpecificPath(environmentFile.Key));
            var environmentFileDirectory = Path.GetDirectoryName(environmentFilePath)!;

            Directory.CreateDirectory(environmentFileDirectory);

            File.WriteAllBytes(environmentFilePath, environmentFile.Value.ToArray());

            return environmentFilePath;
        }

        foreach (var environmentFile in environmentFilesNotExecutable)
            writeEnvironmentFile(environmentFile);

        const string mainExecutableFileName = "name-used-to-execute-file.exe";
        var mainExecutableFilePathRelative = ImmutableList.Create(mainExecutableFileName);

        var executableFileNameAppendix =
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? ".exe" : "";

        var allExecutableFiles =
            (environmentFilesExecutable ?? ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty)
            .ToImmutableDictionary()
            .SetItems(
                (environmentPathExecutableFiles ?? ImmutableDictionary<string, ReadOnlyMemory<byte>>.Empty)
                .Select(execFile => new KeyValuePair<IReadOnlyList<string>, ReadOnlyMemory<byte>>(
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
                Filesystem.MakePlatformSpecificPath(workingDirectoryRelative ?? ImmutableList<string>.Empty));

        var mainExecutableFilePathAbsolute = Path.Combine(containerDirectory, mainExecutableFileName);

        var environmentPathExecutableFilesPathAbsolute = Path.Combine(containerDirectory, environmentPathContainerDirectoryName);

        var pathEnvironmentVarSeparator =
            RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? ";" : ":";

        var environmentPathEntryBefore =
            environmentStringsDict.FirstOrDefault(c => c.Key.Equals("PATH", StringComparison.InvariantCultureIgnoreCase));

        var environmentPath = environmentPathExecutableFilesPathAbsolute + pathEnvironmentVarSeparator + environmentPathEntryBefore.Value;

        var environmentStringsWithExecutableFiles =
            environmentStringsDict
            .SetItem(environmentPathEntryBefore.Key ?? "PATH", environmentPath);

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
                CreateNoWindow = true,
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
        // Avoid crash in scenario like https://forum.botlab.org/t/farm-manager-tribal-wars-2-farmbot/3038/170
        catch (UnauthorizedAccessException)
        {
        }

        return (new ProcessOutput
        (
            ExitCode: exitCode,
            StandardError: standardError,
            StandardOutput: standardOutput
        ), createdFiles);
    }
}
