using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using Mono.Unix;

namespace Kalmit
{
    public class ExecutableFile
    {
        public struct ProcessOutput
        {
            public string StandardError;

            public string StandardOutput;

            public int ExitCode;
        }

        static public (ProcessOutput processOutput, IReadOnlyCollection<(string name, IImmutableList<byte> content)> resultingFiles) ExecuteFileWithArguments(
            IImmutableList<(string name, IImmutableList<byte> content)> environmentFiles,
            byte[] executableFile,
            string arguments,
            IDictionary<string, string> environmentStrings)
        {
            var workingDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

            var executableFileName = "name-used-to-execute-file.exe";

            foreach (var environmentFile in environmentFiles)
            {
                var environmentFilePath = Path.Combine(workingDirectory, environmentFile.name);
                var environmentFileDirectory = Path.GetDirectoryName(environmentFilePath);

                Directory.CreateDirectory(environmentFileDirectory);

                File.WriteAllBytes(environmentFilePath, environmentFile.content.ToArray());
            }

            var executableFilePath = Path.Combine(workingDirectory, executableFileName);

            File.WriteAllBytes(executableFilePath, executableFile);

            if (System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            {
                var unixFileInfo = new Mono.Unix.UnixFileInfo(executableFilePath);

                unixFileInfo.FileAccessPermissions |=
                    FileAccessPermissions.GroupExecute | FileAccessPermissions.UserExecute | FileAccessPermissions.OtherExecute |
                    FileAccessPermissions.GroupRead | FileAccessPermissions.UserRead | FileAccessPermissions.OtherRead;
            }

            var process = new System.Diagnostics.Process
            {
                StartInfo = new ProcessStartInfo
                {
                    WorkingDirectory = workingDirectory,
                    FileName = executableFilePath,
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

            File.Delete(executableFilePath);

            var resultFiles =
                Filesystem.GetAllFilesFromDirectory(workingDirectory);

            Directory.Delete(workingDirectory, true);

            return (new ProcessOutput
            {
                ExitCode = exitCode,
                StandardError = standardError,
                StandardOutput = standardOutput,
            }, resultFiles);
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
