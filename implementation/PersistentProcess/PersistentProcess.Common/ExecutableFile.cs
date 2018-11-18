using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

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

        static public (ProcessOutput processOutput, IReadOnlyCollection<(string name, byte[] content)> resultingFiles) ExecuteFileWithArguments(
            IReadOnlyCollection<(string name, byte[] content)> environmentFiles,
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

                File.WriteAllBytes(environmentFilePath, environmentFile.content);
            }

            var executableFilePath = Path.Combine(workingDirectory, executableFileName);

            File.WriteAllBytes(executableFilePath, executableFile);

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
    }
}
