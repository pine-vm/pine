using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace Kalmit
{
    public class ExecutableFile
    {
        static public (int exitCode, string standardOutput, IReadOnlyCollection<(string name, byte[] content)> resultingFiles) ExecuteFileWithArguments(
            IReadOnlyCollection<(string name, byte[] content)> environmentFiles,
            byte[] executableFile,
            string arguments)
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
                },
            };

            process.Start();
            var standardOutput = process.StandardOutput.ReadToEnd();

            process.WaitForExit();
            var exitCode = process.ExitCode;
            process.Close();

            File.Delete(executableFilePath);

            var resultFiles =
                Filesystem.GetAllFilesFromDirectory(workingDirectory);

            Directory.Delete(workingDirectory, true);

            return (exitCode, standardOutput, resultFiles);
        }
    }
}
