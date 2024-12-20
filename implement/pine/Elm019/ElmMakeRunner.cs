using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace Pine.Elm019;

public static class ElmMakeRunner
{
    public static async Task<ExecutableFile.ProcessOutput> ElmMakeAsync(
        string workingDirectoryAbsolute,
        string pathToFileWithElmEntryPoint)
    {
        var arguments =
            string.Join(" ", ["make", pathToFileWithElmEntryPoint, "--report=json  --output=/dev/null"]);

        var executableFile =
            BlobLibrary.LoadFileForCurrentOs(ElmTime.Elm019.Elm019Binaries.ElmExecutableFileByOs)
            ??
            throw new System.Exception("Failed to load elm-format executable file");

        if (!RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            System.IO.File.SetUnixFileMode(
                executableFile.cacheFilePath,
                ExecutableFile.UnixFileModeForExecutableFile);
        }

        var process = new System.Diagnostics.Process
        {
            StartInfo = new System.Diagnostics.ProcessStartInfo
            {
                WorkingDirectory = workingDirectoryAbsolute,
                FileName = executableFile.cacheFilePath,
                Arguments = arguments,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                CreateNoWindow = true,
            },
        };


        //  Avoid elm make failing on `getAppUserDataDirectory`.
        /* Also, work around problems with elm make like this:
        -- HTTP PROBLEM ----------------------------------------------------------------

        The following HTTP request failed:
            <https://github.com/elm/core/zipball/1.0.0/>

        Here is the error message I was able to extract:

        HttpExceptionRequest Request { host = "github.com" port = 443 secure = True
        requestHeaders = [("User-Agent","elm/0.19.0"),("Accept-Encoding","gzip")]
        path = "/elm/core/zipball/1.0.0/" queryString = "" method = "GET" proxy =
        Nothing rawBody = False redirectCount = 10 responseTimeout =
        ResponseTimeoutDefault requestVersion = HTTP/1.1 } (StatusCodeException
        (Response {responseStatus = Status {statusCode = 429, statusMessage = "Too
        Many Requests"}, responseVersion = HTTP/1.1, responseHeaders =
        [("Server","GitHub.com"),("Date","Sun, 18 Nov 2018 16:53:18
        GMT"),("Content-Type","text/html"),("Transfer-Encoding","chunked"),("Status","429
        Too Many
        Requests"),("Retry-After","120")

        To avoid elm make failing with this error, break isolation here and reuse elm home directory.
        An alternative would be retrying when this error is parsed from `commandResults.processOutput.StandardError`.
        */
        process.StartInfo.Environment["ELM_HOME"] = ElmTime.Elm019.Elm019Binaries.GetElmHomeDirectory();

        var standardOutputBuilder = new StringBuilder();
        var standardErrorBuilder = new StringBuilder();

        // We'll use TaskCompletionSource to wait until all output has been read
        var stdoutTcs = new TaskCompletionSource<bool>();
        var stderrTcs = new TaskCompletionSource<bool>();

        // Event handler for standard output
        process.OutputDataReceived += (sender, e) =>
        {
            if (e.Data is null)
            {
                // No more output
                stdoutTcs.TrySetResult(true);
            }
            else
            {
                standardOutputBuilder.AppendLine(e.Data);
            }
        };

        // Event handler for standard error
        process.ErrorDataReceived += (sender, e) =>
        {
            if (e.Data is null)
            {
                // No more error output
                stderrTcs.TrySetResult(true);
            }
            else
            {
                standardErrorBuilder.AppendLine(e.Data);
            }
        };

        // Start the process and begin asynchronous reads
        if (!process.Start())
        {
            throw new System.Exception("Failed to start elm make process.");
        }

        process.BeginOutputReadLine();
        process.BeginErrorReadLine();

        // Wait for the process to exit, then for all output to be collected
        await process.WaitForExitAsync();

        // At this point, the process has exited, but we need to ensure we collected all output lines.
        // The event handlers complete when they receive a null Data line.
        await Task.WhenAll(stdoutTcs.Task, stderrTcs.Task);

        var exitCode = process.ExitCode;
        process.Close();

        return new ExecutableFile.ProcessOutput(
            StandardError: standardErrorBuilder.ToString(),
            StandardOutput: standardOutputBuilder.ToString(),
            ExitCode: exitCode);
    }
}
