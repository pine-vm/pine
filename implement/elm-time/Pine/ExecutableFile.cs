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
        IImmutableList<string>? workingDirectory = null,
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>? environmentFilesExecutable = null,
        IReadOnlyDictionary<string, ReadOnlyMemory<byte>>? environmentPathExecutableFiles = null)
    {
        var environmentStringsDict =
            environmentStrings?.ToImmutableDictionary() ?? ImmutableDictionary<string, string>.Empty;

        var environmentPathContainerDirectoryName = "environment-path-cont";

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

        var mainExecutableFileName = "name-used-to-execute-file.exe";
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
                Filesystem.MakePlatformSpecificPath(workingDirectory ?? ImmutableList<string>.Empty));

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

    /// <summary>
    /// Offer method with old interface for backwards-compatibility with assembly consumers.
    /// </summary>
    [Obsolete("Use the new " + nameof(ExecuteFileWithArguments) + " instead")]
    public static (ProcessOutput processOutput, IReadOnlyCollection<(IImmutableList<string> path, ReadOnlyMemory<byte> content)> resultingFiles) ExecuteFileWithArguments(
    IReadOnlyDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> environmentFilesNotExecutable,
    ReadOnlyMemory<byte> executableFile,
    string arguments,
    IDictionary<string, string>? environmentStrings,
    IImmutableList<string>? workingDirectory = null,
    IReadOnlyDictionary<IImmutableList<string>, ReadOnlyMemory<byte>>? environmentFilesExecutable = null,
    IReadOnlyDictionary<string, ReadOnlyMemory<byte>>? environmentPathExecutableFiles = null)
    {
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> toNewDictType(IReadOnlyDictionary<IImmutableList<string>, ReadOnlyMemory<byte>> oldDict) =>
            oldDict
            .ToImmutableDictionary(
                keySelector: e => (IReadOnlyList<string>)e.Key,
                elementSelector: e => e.Value,
                keyComparer: EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

        var (processOutput, resultingFilesOld) =
            ExecuteFileWithArguments(
                environmentFilesNotExecutable: toNewDictType(environmentFilesNotExecutable),
                executableFile: executableFile,
                arguments: arguments,
                environmentStrings: environmentStrings,
                workingDirectory: workingDirectory,
                environmentFilesExecutable: environmentFilesExecutable is null ? null : toNewDictType(environmentFilesExecutable),
                environmentPathExecutableFiles: environmentPathExecutableFiles);

        var resultingFiles =
            resultingFilesOld.Select(file => ((IImmutableList<string>)file.path.ToImmutableList(), file.content)).ToImmutableList();

        return (processOutput, resultingFiles);
    }


    //  Helper for Linux platform. Thank you Kyle Spearrin!
    //  https://stackoverflow.com/questions/45132081/file-permissions-on-linux-unix-with-net-core/47918132#47918132
    public static void LinuxExecWithBash(string cmd)
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
            System.Text.Json.JsonSerializer.Serialize(
                new
                {
                    cmd = cmd,
                    exitCode = process.ExitCode,
                    standardOut = process.StandardOutput.ReadToEnd(),
                    standardError = process.StandardError.ReadToEnd(),
                }));
    }
}
