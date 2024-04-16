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

        void writeEnvironmentFile(
            KeyValuePair<IReadOnlyList<string>, ReadOnlyMemory<byte>> environmentFile,
            bool executable)
        {
            var environmentFilePath = Path.Combine(containerDirectory, Filesystem.MakePlatformSpecificPath(environmentFile.Key));

            CreateAndWriteFileToPath(environmentFilePath, environmentFile.Value, executable);
        }

        foreach (var environmentFile in environmentFilesNotExecutable)
            writeEnvironmentFile(environmentFile, executable: false);

        var executableFileNameAppendix = ExecutableFileNameAppendix;

        var mainExecutableFileName = "name-used-to-execute-file" + executableFileNameAppendix;
        var mainExecutableFilePathRelative = ImmutableList.Create(mainExecutableFileName);

        var allExecutableFiles =
            (environmentFilesExecutable ?? ImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>.Empty)
            .ToImmutableDictionary()
            .SetItems(
                (environmentPathExecutableFiles ?? ImmutableDictionary<string, ReadOnlyMemory<byte>>.Empty)
                .Select(execFile => new KeyValuePair<IReadOnlyList<string>, ReadOnlyMemory<byte>>(
                    [environmentPathContainerDirectoryName, execFile.Key + executableFileNameAppendix], execFile.Value)))
            .SetItem(mainExecutableFilePathRelative, executableFile);

        foreach (var environmentFile in allExecutableFiles)
        {
            writeEnvironmentFile(environmentFile, executable: true);
        }

        var workingDirectoryAbsolute =
            Path.Combine(
                containerDirectory,
                Filesystem.MakePlatformSpecificPath(workingDirectoryRelative ?? []))
            .TrimEnd(Path.DirectorySeparatorChar)
            + Path.DirectorySeparatorChar.ToString();

        var mainExecutableFilePathAbsolute = Path.Combine(containerDirectory, mainExecutableFileName);

        var environmentPathExecutableFilesPathAbsolute = Path.Combine(containerDirectory, environmentPathContainerDirectoryName);

        var pathEnvironmentVarSeparator = PathEnvironmentVarSeparator.ToString();

        var environmentPathEntryBefore =
            environmentStringsDict.FirstOrDefault(c => c.Key.Equals("PATH", StringComparison.InvariantCultureIgnoreCase));

        var environmentVarsForNewPath =
            /*
             * Branch to account for failure on OSX/MacOS observed 2023-05-20:
             * When we added an environment variable 'PATH' with empty value, `elm  make` crashed with an error message like this:
             * 
             * security: createProcess: runInteractiveProcess: exec: does not exist (No such file or directory)
             * 
             * -- ERROR -----------------------------------------------------------------------
             * 
             * I ran into something that bypassed the normal error reporting process! I
             * extracted whatever information I could from the internal error:
             * 
             * >   thread blocked indefinitely in an MVar operation
             * */
            Directory.Exists(environmentPathExecutableFilesPathAbsolute)
            ?
            ImmutableList.Create(
                new KeyValuePair<string, string>(
                    "PATH",
                    string.Join(
                        pathEnvironmentVarSeparator,
                        new[]
                        {
                            environmentPathExecutableFilesPathAbsolute,
                            environmentPathEntryBefore.Value
                        }
                        .Where(c => 0 < c?.Length))))
            :
            [];

        var environmentStringsWithExecutableFiles =
            environmentStringsDict
            .SetItems(environmentVarsForNewPath);

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

    public static void CreateAndWriteFileToPath(
        string filePath,
        ReadOnlyMemory<byte> fileContent,
        bool makeExecutable)
    {
        var directory = Path.GetDirectoryName(filePath);

        if (directory is not null)
            Directory.CreateDirectory(directory);

        var unixCreateMode =
            makeExecutable
            ?
            (UnixFileMode?)UnixFileModeForExecutableFile
            :
            null;

        var fileStreamOptions = new FileStreamOptions
        {
            Mode = FileMode.Create,
            Access = FileAccess.Write
        };

        if (!RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            fileStreamOptions.UnixCreateMode = unixCreateMode;

        using var fileStream = new FileStream(filePath, options: fileStreamOptions);

        fileStream.Write(fileContent.Span);

        fileStream.Flush(flushToDisk: true);

        fileStream.Close();
    }

    public static UnixFileMode UnixFileModeForExecutableFile =>
        UnixFileMode.GroupExecute | UnixFileMode.UserExecute | UnixFileMode.OtherExecute |
        UnixFileMode.GroupRead | UnixFileMode.UserRead | UnixFileMode.OtherRead;

    public static IEnumerable<string> GetExecutablePath(string executableFileName) =>
        Environment.GetEnvironmentVariable("PATH")
        ?.Split(PathEnvironmentVarSeparator)
        .Select(s => Path.Combine(s, executableFileName))
        .Where(File.Exists)
        ??
        [];

    public static char PathEnvironmentVarSeparator =>
        RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? ';' : ':';

    public static string ExecutableFileNameAppendix =>
        RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? ".exe" : "";
}
