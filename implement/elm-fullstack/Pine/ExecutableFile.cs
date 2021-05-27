using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using Mono.Unix;

namespace Pine
{
    public class ExecutableFile
    {
        public struct ProcessOutput
        {
            public string StandardError;

            public string StandardOutput;

            public int ExitCode;
        }

        static public (ProcessOutput processOutput, IReadOnlyCollection<(IImmutableList<string> path, IReadOnlyList<byte> content)> resultingFiles) ExecuteFileWithArguments(
            IImmutableList<(IImmutableList<string> path, IReadOnlyList<byte> content)> environmentFiles,
            byte[] executableFile,
            string arguments,
            IDictionary<string, string> environmentStrings,
            IImmutableList<string> workingDirectory = null)
        {
            var containerDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var executableFileName = "name-used-to-execute-file.exe";

            var executableFilePathRelative = ImmutableList.Create(executableFileName);

            foreach (var environmentFile in environmentFiles)
            {
                var environmentFilePath = Path.Combine(containerDirectory, Filesystem.MakePlatformSpecificPath(environmentFile.path));
                var environmentFileDirectory = Path.GetDirectoryName(environmentFilePath);

                Directory.CreateDirectory(environmentFileDirectory);

                File.WriteAllBytes(environmentFilePath, (environmentFile.content as byte[]) ?? environmentFile.content.ToArray());
            }

            var executableFilePathAbsolute = Path.Combine(containerDirectory, executableFileName);

            File.WriteAllBytes(executableFilePathAbsolute, executableFile);

            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            {
                var unixFileInfo = new Mono.Unix.UnixFileInfo(executableFilePathAbsolute);

                unixFileInfo.FileAccessPermissions |=
                    FileAccessPermissions.GroupExecute | FileAccessPermissions.UserExecute | FileAccessPermissions.OtherExecute |
                    FileAccessPermissions.GroupRead | FileAccessPermissions.UserRead | FileAccessPermissions.OtherRead;
            }

            var workingDirectoryAbsolute =
                Path.Combine(
                    containerDirectory,
                    Filesystem.MakePlatformSpecificPath(workingDirectory ?? ImmutableList<string>.Empty));

            var process = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    WorkingDirectory = workingDirectoryAbsolute,
                    FileName = executableFilePathAbsolute,
                    Arguments = arguments,
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                },
            };

            foreach (var envString in environmentStrings.EmptyIfNull())
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
                    filterByRelativeName: path => !path.SequenceEqual(executableFilePathRelative));

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
}
