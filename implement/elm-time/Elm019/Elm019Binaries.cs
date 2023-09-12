using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;

namespace ElmTime.Elm019;

/// <summary>
/// Functions to work with the Elm 0.19 binaries from https://github.com/elm/compiler/releases
/// This class loads the Elm binary matching the current operating system and uses the command-line interface by spawning a new process for commands like 'elm make'
/// </summary>
public static class Elm019Binaries
{
    public static string? OverrideElmMakeHomeDirectory = null;

    public static IFileStore? ElmMakeResultCacheFileStoreDefault = null;

    private static string? _elmHomeDirectory;

    public record ElmMakeOk(ReadOnlyMemory<byte> producedFile);

    /// <inheritdoc cref="ElmMake"/>
    public static Result<string, ElmMakeOk> ElmMakeToJavascript(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmCodeFiles,
        IReadOnlyList<string>? workingDirectoryRelative,
        IReadOnlyList<string> pathToFileWithElmEntryPoint,
        string? elmMakeCommandAppendix = null) =>
        ElmMake(
            elmCodeFiles,
            workingDirectoryRelative: workingDirectoryRelative,
            pathToFileWithElmEntryPoint,
            "file-for-elm-make-output.js",
            elmMakeCommandAppendix);

    /// <inheritdoc cref="ElmMake"/>
    public static Result<string, ElmMakeOk> ElmMakeToHtml(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmCodeFiles,
        IReadOnlyList<string>? workingDirectoryRelative,
        IImmutableList<string> pathToFileWithElmEntryPoint,
        string? elmMakeCommandAppendix = null) =>
        ElmMake(
            elmCodeFiles,
            workingDirectoryRelative: workingDirectoryRelative,
            pathToFileWithElmEntryPoint,
            "file-for-elm-make-output.html",
            elmMakeCommandAppendix);

    /// <inheritdoc cref="ElmMakeIgnoringCachedResults"/>
    public static Result<string, ElmMakeOk> ElmMake(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmCodeFiles,
        IReadOnlyList<string>? workingDirectoryRelative,
        IReadOnlyList<string> pathToFileWithElmEntryPoint,
        string outputFileName,
        string? elmMakeCommandAppendix = null) =>
        ElmMake(
            elmCodeFiles: elmCodeFiles,
            workingDirectoryRelative: workingDirectoryRelative,
            pathToFileWithElmEntryPoint: pathToFileWithElmEntryPoint,
            outputFileName: outputFileName,
            elmMakeCommandAppendix: elmMakeCommandAppendix,
            resultCacheFileStore: ElmMakeResultCacheFileStoreDefault);

    /// <inheritdoc cref="ElmMakeIgnoringCachedResults"/>
    public static Result<string, ElmMakeOk> ElmMake(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmCodeFiles,
        IReadOnlyList<string>? workingDirectoryRelative,
        IReadOnlyList<string> pathToFileWithElmEntryPoint,
        string outputFileName,
        string? elmMakeCommandAppendix,
        IFileStore? resultCacheFileStore)
    {
        var elmCodeFilesHash =
            CommonConversion.StringBase16(
                PineValueHashTree.ComputeHashSorted(PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(elmCodeFiles)));

        var requestIdentifier = new ElmMakeRequestIdentifier(
            elmCodeFilesHash: elmCodeFilesHash,
            pathToFileWithElmEntryPoint: pathToFileWithElmEntryPoint,
            outputFileName: outputFileName,
            elmMakeCommandAppendix: elmMakeCommandAppendix);

        var requestHash =
            CommonConversion.StringBase16(
                CommonConversion.HashSHA256(System.Text.Encoding.UTF8.GetBytes(System.Text.Json.JsonSerializer.Serialize(requestIdentifier))));

        var cacheEntryPath = ImmutableList.Create(requestHash);

        try
        {
            var cacheEntryFile =
                resultCacheFileStore?.GetFileContent(cacheEntryPath);

            if (cacheEntryFile is not null)
            {
                var resultFromCache =
                    System.Text.Json.JsonSerializer.Deserialize<Result<string, ElmMakeOkJsonStructure>>(cacheEntryFile.Value.Span)
                    ?.Map(AsElmMakeOk);

                if (resultFromCache is Result<string, ElmMakeOk>.Ok resultOk)
                    return resultFromCache;
            }
        }
        catch { }

        var result =
            ElmMakeIgnoringCachedResults(
                elmCodeFiles,
                workingDirectoryRelative: workingDirectoryRelative,
                pathToFileWithElmEntryPoint,
                outputFileName,
                elmMakeCommandAppendix);

        if (resultCacheFileStore is not null)
        {
            try
            {
                resultCacheFileStore.SetFileContent(
                    cacheEntryPath,
                    System.Text.Json.JsonSerializer.SerializeToUtf8Bytes(result.Map(AsJsonStructure)));
            }
            catch { }
        }

        return result;
    }

    /*
    2019-12-14: Switch to modeling file paths as a list of string instead of a string, to avoid that problem reported earlier and described below:

    Unify directory separator symbols in file names to avoid this problem observed 2019-07-31:
    I had built web-app-config.zip on a Windows system. Starting the webserver with this worked as expected in Windows. But in a Docker container it failed, with an error as below:
    ----
    Output file not found. Maybe the output from the Elm make process helps to find the cause:
    Exit Code: 1
    Standard Output:
    ''
    Standard Error:
    '-- BAD JSON ----------------------------------------------------------- elm.json

    The "source-directories" in your elm.json lists the following directory:

        src

    I cannot find that directory though! Is it missing? Is there a typo?
    [...]
    */
    /// <summary>
    /// Use the 'elm make' command on the Elm executable file.
    /// </summary>
    public static Result<string, ElmMakeOk> ElmMakeIgnoringCachedResults(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmCodeFiles,
        IReadOnlyList<string>? workingDirectoryRelative,
        IReadOnlyList<string> pathToFileWithElmEntryPoint,
        string outputFileName,
        string? elmMakeCommandAppendix = null)
    {
        /*
        2020-04-01: Avoid the sporadic failures as reported at
        https://github.com/elm-time/elm-time/blob/a206b8095e9f2300f413ef381342db1dca790542/explore/2020-04-01.automate-testing/2020-04-01.automate-testing.md
        Retry for these class of errors.
        */
        const int maxRetryCount = 2;

        var command = "make " + Filesystem.MakePlatformSpecificPath(pathToFileWithElmEntryPoint) + " --output=\"" + outputFileName + "\" " + elmMakeCommandAppendix;

        var attemptsResults = new List<(ExecutableFile.ProcessOutput processOutput, IReadOnlyCollection<(IReadOnlyList<string> path, ReadOnlyMemory<byte> content)> resultingFiles)>();

        do
        {
            var commandResults = ExecutableFile.ExecuteFileWithArguments(
                environmentFilesNotExecutable: elmCodeFiles,
                GetElmExecutableFile,
                command,
                new Dictionary<string, string>
                {
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
                    {"ELM_HOME", GetElmHomeDirectory()},
                },
                workingDirectoryRelative: workingDirectoryRelative);

            attemptsResults.Add(commandResults);

            var newFiles =
                commandResults.resultingFiles
                .Where(file => !elmCodeFiles.ContainsKey(file.path))
                .ToImmutableList();

            var outputFileExpectedPath =
                (workingDirectoryRelative ?? []).ToImmutableList().Add(outputFileName);

            var outputFiles =
                newFiles
                .Where(resultFile => resultFile.path.SequenceEqual(outputFileExpectedPath))
                .ToImmutableList();

            if (1 <= outputFiles.Count)
            {
                return Result<string, ElmMakeOk>.ok(new ElmMakeOk(outputFiles.First().content));
            }

            if (!ElmMakeOutputQualifiesForRetry(standardErrorText: commandResults.processOutput.StandardError))
                break;

        } while (attemptsResults.Count <= maxRetryCount);

        var lastAttemptResults = attemptsResults.Last();

        return Result<string, ElmMakeOk>.err(
            "Failed for " + attemptsResults.Count + " attempts. Output file not found. Maybe the output from the Elm make process from the last attempt helps to find the cause:" +
            "\nExit Code: " + lastAttemptResults.processOutput.ExitCode +
            "\nStandard Output:\n'" + lastAttemptResults.processOutput.StandardOutput + "'" +
            "\nStandard Error:\n'" + lastAttemptResults.processOutput.StandardError + "'");
    }

    public static bool ElmMakeOutputQualifiesForRetry(string standardErrorText)
    {
        if (standardErrorText.Contains("openBinaryFile: resource busy (file is locked)"))
            return true;

        /*
         * Elm make sporadically fails with this error:
         * 
            -- PROBLEM BUILDING DEPENDENCIES -----------------------------------------------

             I ran into a compilation error when trying to build the following package:

                 elm/core 1.0.5

             This probably means it has package constraints that are too wide. It may be
             possible to tweak your elm.json to avoid the root problem as a stopgap. Head
             over to https://elm-lang.org/community to get help figuring out how to take this
             path!
         * */

        if (standardErrorText.Contains("I ran into a compilation error when trying to build the following package:") &&
            standardErrorText.Contains("This probably means it has package constraints that are too wide."))
            return true;

        return false;
    }

    public static ReadOnlyMemory<byte> GetElmExecutableFile =>
        BlobLibrary.LoadFileForCurrentOs(ElmExecutableFileByOs)!.Value;

    public static IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> ElmExecutableFileByOs =
        ImmutableDictionary<OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            /*
            Loaded 2022-02-01 🐯 from
            https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
            */
            OSPlatform.Linux,
            ("f8f12a61a61f64ac71a85d57284cc4d14fb81f1cbebb8b150839d9731034092e",
            @"https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz"))
        .Add(
            /*
            Loaded 2022-02-01 🐯 from
            https://github.com/elm/compiler/releases/download/0.19.1/binary-for-windows-64-bit.gz
            */
            OSPlatform.Windows,
            ("821e61ee150b660ca173584b66d1784b7be08b7107e7aa4977135686dc9d2fb2",
            @"https://github.com/elm/compiler/releases/download/0.19.1/binary-for-windows-64-bit.gz"))
        .Add(
            /*
            Loaded 2023-05-20 from
            https://github.com/elm/compiler/releases/download/0.19.1/binary-for-mac-64-bit.gz
            */
            OSPlatform.OSX,
            ("ae645d3ff3d2fbe87460c077f644d3d64de87b5c40e5dbadd8bb08978148a117",
            @"https://github.com/elm/compiler/releases/download/0.19.1/binary-for-mac-64-bit.gz"));

    public static string GetElmHomeDirectory()
    {
        _elmHomeDirectory =
            OverrideElmMakeHomeDirectory ??
            _elmHomeDirectory ??
            Path.Combine(Filesystem.CreateRandomDirectoryInTempDirectory(), "elm-home");

        Directory.CreateDirectory(_elmHomeDirectory);
        return _elmHomeDirectory;
    }

    private record ElmMakeRequestIdentifier(
        string elmCodeFilesHash,
        IReadOnlyList<string> pathToFileWithElmEntryPoint,
        string outputFileName,
        string? elmMakeCommandAppendix);

    private record ElmMakeOkJsonStructure(
        string producedFileBase64);

    private static ElmMakeOk AsElmMakeOk(ElmMakeOkJsonStructure cacheEntry) =>
        new(producedFile: Convert.FromBase64String(cacheEntry.producedFileBase64));

    private static ElmMakeOkJsonStructure AsJsonStructure(ElmMakeOk cacheEntry) =>
        new(producedFileBase64: Convert.ToBase64String(cacheEntry.producedFile!.ToArray())!);
}
