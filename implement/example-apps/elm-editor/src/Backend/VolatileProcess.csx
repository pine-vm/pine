#r "netstandard"
#r "System"
#r "System.Collections.Immutable"
#r "System.Net"
#r "System.Net.Http"
#r "System.Net.Primitives"
#r "System.Private.Uri"
#r "System.Linq"
#r "System.Runtime.InteropServices.RuntimeInformation"
#r "System.Text.Json"

#r "elm-time"

using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;


int loadCompositionLimitFileCount = 110;
int loadCompositionLimitAggregateFileSize = 900_000;
int loadCompositionLimitMaximumPathLength = 200;


public class RequestStructure
{
    public IReadOnlyList<ElmMakeRequestStructure> ElmMakeRequest { set; get; }

    public IReadOnlyList<string> FormatElmModuleTextRequest { set; get; }

    public IReadOnlyList<string> LoadCompositionRequest { set; get; }
}

public class ResponseStructure
{
    [System.Text.Json.Serialization.JsonIgnore(Condition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull)]
    public IReadOnlyList<ElmMakeResponseStructure> ElmMakeResponse { set; get; }

    [System.Text.Json.Serialization.JsonIgnore(Condition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull)]
    public IReadOnlyList<FormatElmModuleTextResponseStructure> FormatElmModuleTextResponse { set; get; }

    [System.Text.Json.Serialization.JsonIgnore(Condition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull)]
    public IReadOnlyList<LoadCompositionResponseStructure> LoadCompositionResponse { set; get; }

    [System.Text.Json.Serialization.JsonIgnore(Condition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull)]
    public IReadOnlyList<string> ErrorResponse { set; get; }
}

public class ElmMakeRequestStructure
{
    public IReadOnlyList<FileWithPath> files { set; get; }

    public IReadOnlyList<string> entryPointFilePathFromWorkingDirectory { set; get; }

    public IReadOnlyList<string> workingDirectoryPath { set; get; }

    public bool makeOptionDebug { set; get; }

    public ElmMakeOutputType outputType { set; get; }
}

public class ElmMakeOutputType
{
    public object ElmMakeOutputTypeHtml { set; get; }
    public object ElmMakeOutputTypeJs { set; get; }
}

public class FormatElmModuleTextResponseStructure
{
    public Maybe<string> formattedText { set; get; }

    public ProcessOutput processOutput { set; get; }
}

public class FileWithPath
{
    public IReadOnlyList<string> path { set; get; }

    public string contentBase64 { set; get; }
}

public class ElmMakeResponseStructure
{
    public ProcessOutput processOutput { set; get; }

    public Maybe<string> outputFileContentBase64 { set; get; }

    public ProcessOutput reportJsonProcessOutput { set; get; }
}

public struct ProcessOutput
{
    public string standardError { set; get; }

    public string standardOutput { set; get; }

    public int exitCode { set; get; }
}

public class LoadCompositionResponseStructure
{
    public string compositionId { set; get; }

    public IReadOnlyList<FileWithPath> filesAsFlatList { set; get; }

    public string urlInCommit { set; get; }
}


string GetSerialResponseFromSerialRequest(string serializedRequest)
{
    var request = System.Text.Json.JsonSerializer.Deserialize<RequestStructure>(serializedRequest);

    var response = GetResponseFromRequest(request);

    return System.Text.Json.JsonSerializer.Serialize(response);
}

ResponseStructure GetResponseFromRequest(RequestStructure request)
{
    var elmMakeRequest =
        request.ElmMakeRequest?.FirstOrDefault();

    if (elmMakeRequest != null)
    {
        return new ResponseStructure
        {
            ElmMakeResponse = ImmutableList.Create(ElmMake(elmMakeRequest))
        };
    }

    var formatElmModuleTextRequest =
        request.FormatElmModuleTextRequest?.FirstOrDefault();

    if (formatElmModuleTextRequest != null)
    {
        return new ResponseStructure
        {
            FormatElmModuleTextResponse = ImmutableList.Create(ElmFormat.FormatElmModuleText(formatElmModuleTextRequest))
        };
    }

    var loadCompositionRequest =
        request.LoadCompositionRequest?.FirstOrDefault();

    if (loadCompositionRequest != null)
    {
        var sourcePath = loadCompositionRequest;

        if (!(Uri.TryCreate(sourcePath, UriKind.Absolute, out var uriResult)
            && (uriResult.Scheme == Uri.UriSchemeHttp || uriResult.Scheme == Uri.UriSchemeHttps)))
        {
            return new ResponseStructure
            {
                ErrorResponse = ImmutableList.Create("This string is not a supported URL: '" + sourcePath + "'")
            };
        }

        var loadFromGitResult = LoadFromGitHubOrGitLab.LoadFromUrl(sourcePath);

        return
            loadFromGitResult
            .Unpack(
                fromErr:
                err => new ResponseStructure
                {
                    ErrorResponse = ImmutableList.Create("Failed to load from path '" + sourcePath + "': " + err)
                },
                fromOk:
                loadFromGitOk =>
                {
                    if (loadFromGitOk?.tree == null)
                    {
                        return new ResponseStructure
                        {
                            ErrorResponse = ImmutableList.Create("Did not find a tree object at '" + sourcePath + "'")
                        };
                    }

                    var composition = Pine.PineValueComposition.FromTreeWithStringPath(loadFromGitOk.tree);

                    var compositionId = Pine.CommonConversion.StringBase16(Pine.PineValueHashTree.ComputeHash(composition));

                    var blobs =
                        loadFromGitOk.tree.EnumerateBlobsTransitive()
                        .ToImmutableList();

                    var urlInCommit = loadFromGitOk.urlInCommit;

                    ResponseStructure responseErrorExceedingLimit(string limitName)
                    {
                        return new ResponseStructure
                        {
                            ErrorResponse = ImmutableList.Create("Composition " + compositionId + " from " + urlInCommit + " exceeds supported limits: " + limitName)
                        };
                    }

                    var fileCount = blobs.Count();

                    if (loadCompositionLimitFileCount < fileCount)
                    {
                        return responseErrorExceedingLimit("File count: " + fileCount);
                    }

                    var aggregateFileSize =
                        blobs.Sum(file => file.blobContent.Length);

                    var filesBySize =
                        blobs.OrderByDescending(file => file.blobContent.Length).ToImmutableList();

                    var largestFilesToDisplay =
                        filesBySize.Take(3).ToImmutableList();

                    if (loadCompositionLimitAggregateFileSize < aggregateFileSize)
                    {
                        var largestFilesDescriptions =
                            largestFilesToDisplay.Select(file => string.Join("/", file.path) + " (" + file.blobContent.Length + " bytes)");

                        return responseErrorExceedingLimit(
                            "Aggregate file size: " + aggregateFileSize +
                            " bytes. Following are the largest " + largestFilesToDisplay.Count +
                            " files:\n" + string.Join("\n", largestFilesDescriptions));
                    }

                    var maximumPathLength =
                        blobs.Max(file => file.path.Sum(pathElement => pathElement.Length));

                    if (loadCompositionLimitMaximumPathLength < maximumPathLength)
                    {
                        return responseErrorExceedingLimit("Maximum path length: " + maximumPathLength);
                    }

                    var filesAsFlatList =
                        blobs
                        .Select(file => new FileWithPath
                        {
                            path = file.path,
                            contentBase64 = Convert.ToBase64String(file.blobContent.ToArray()),
                        })
                        .ToImmutableList();

                    return new ResponseStructure
                    {
                        LoadCompositionResponse = ImmutableList.Create(
                            new LoadCompositionResponseStructure
                            {
                                compositionId = compositionId,
                                filesAsFlatList = filesAsFlatList,
                                urlInCommit = urlInCommit,
                            })
                    };
                });
    }

    return new ResponseStructure
    {
        ErrorResponse = ImmutableList.Create("This request does not encode any supported case.")
    };
}

ElmMakeResponseStructure ElmMake(ElmMakeRequestStructure elmMakeRequest)
{
    var elmCodeFiles =
        elmMakeRequest.files
        .ToImmutableDictionary(
            file => file.path,
            file => (ReadOnlyMemory<byte>)Convert.FromBase64String(file.contentBase64));

    var environmentFiles = elmCodeFiles;

    var entryPointFilePathFromWorkingDirectory =
        MakePlatformSpecificPath(elmMakeRequest.entryPointFilePathFromWorkingDirectory);

    var elmMakeOutputFileName = "elm-make-output." + (elmMakeRequest.outputType.ElmMakeOutputTypeJs != null ? "js" : "html");

    var commandLineCommonArguments = "make " + entryPointFilePathFromWorkingDirectory + " " + (elmMakeRequest.makeOptionDebug ? "--debug" : "");

    var commandLineArguments = commandLineCommonArguments + " --output=" + elmMakeOutputFileName;
    var reportJsonCommandLineArguments = commandLineCommonArguments + " --report=json";

    (Pine.ExecutableFile.ProcessOutput processOutput, IReadOnlyCollection<(IReadOnlyList<string> path, ReadOnlyMemory<byte> content)> resultingFiles) commandResultsFromArguments(string arguments)
    {
        return
            Pine.ExecutableFile.ExecuteFileWithArguments(
                environmentFiles,
                GetElmExecutableFile,
                arguments,
                new Dictionary<string, string>()
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
                workingDirectoryRelative: elmMakeRequest.workingDirectoryPath.ToImmutableList());
    }

    var commandResults = commandResultsFromArguments(commandLineArguments);

    var newFiles =
        commandResults.resultingFiles
        .Where(file => !elmCodeFiles.ContainsKey(file.path))
        .Select(file => new FileWithPath
        {
            path = file.path,
            contentBase64 = Convert.ToBase64String(file.content.ToArray()),
        })
        .ToImmutableList();

    var outputFile =
        newFiles
        .Where(file => file.path.LastOrDefault() == elmMakeOutputFileName)
        .FirstOrDefault();

    var outputFileContentBase64 = outputFile?.contentBase64;

    var processOutput = new ProcessOutput
    {
        standardOutput = commandResults.processOutput.StandardOutput,
        standardError = commandResults.processOutput.StandardError,
        exitCode = commandResults.processOutput.ExitCode,
    };

    var reportJsonCommandResults = commandResultsFromArguments(reportJsonCommandLineArguments);

    var reportJsonProcessOutput = new ProcessOutput
    {
        standardOutput = reportJsonCommandResults.processOutput.StandardOutput,
        standardError = reportJsonCommandResults.processOutput.StandardError,
        exitCode = reportJsonCommandResults.processOutput.ExitCode,
    };

    var responseStructure = new ElmMakeResponseStructure
    {
        processOutput = processOutput,
        outputFileContentBase64 = Maybe.NothingFromNull(outputFileContentBase64),
        reportJsonProcessOutput = reportJsonProcessOutput,
    };

    return responseStructure;
}

string MakePlatformSpecificPath(IReadOnlyList<string> path) =>
    string.Join(System.IO.Path.DirectorySeparatorChar.ToString(), path);

static public byte[] GetElmExecutableFile =>
    Pine.CommonConversion.DecompressGzip(GetElmExecutableFileCompressedGzip).ToArray();

static public byte[] GetElmExecutableFileCompressedGzip =>
    Pine.BlobLibrary.GetBlobWithSHA256(Pine.CommonConversion.ByteArrayFromStringBase16(
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Linux)
        ?
        /*
        Loaded 2019-10-29 from
        https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
        */
        "e44af52bb27f725a973478e589d990a6428e115fe1bb14f03833134d6c0f155c"
        :
        /*
        Loaded 2019-10-29 from
        https://github.com/elm/compiler/releases/download/0.19.1/binary-for-windows-64-bit.gz
        */
        "d1bf666298cbe3c5447b9ca0ea608552d750e5d232f9845c2af11907b654903b"))
    ?.ToArray();

static public string overrideElmMakeHomeDirectory = null;

static string elmHomeDirectory;

static public string GetElmHomeDirectory()
{
    elmHomeDirectory =
        overrideElmMakeHomeDirectory ??
        elmHomeDirectory ??
        System.IO.Path.Combine(Pine.Filesystem.CreateRandomDirectoryInTempDirectory(), "elm-home");

    System.IO.Directory.CreateDirectory(elmHomeDirectory);
    return elmHomeDirectory;
}

static public class ElmFormat
{
    static public FormatElmModuleTextResponseStructure FormatElmModuleText(string originalModuleText)
    {
        var elmModuleFileName = "ElmModuleToFormat.elm";

        var elmModuleFilePath = ImmutableList.Create(elmModuleFileName);

        var elmFormatResult =
            Pine.ExecutableFile.ExecuteFileWithArguments(
                ImmutableDictionary.Create<IReadOnlyList<string>, ReadOnlyMemory<byte>>()
                .SetItem(elmModuleFilePath, System.Text.Encoding.UTF8.GetBytes(originalModuleText)),
                GetElmFormatExecutableFile,
                " " + elmModuleFileName + " --yes",
                environmentStrings: null);

        var resultingFiles =
            elmFormatResult.resultingFiles
            .Where(file => file.path.SequenceEqual(elmModuleFilePath))
            .ToImmutableList();

        var formattedText =
            resultingFiles.Any() ?
            System.Text.Encoding.UTF8.GetString(resultingFiles.First().content.Span)
            :
            null;

        var processOutput = new ProcessOutput
        {
            standardOutput = elmFormatResult.processOutput.StandardOutput,
            standardError = elmFormatResult.processOutput.StandardError,
            exitCode = elmFormatResult.processOutput.ExitCode,
        };

        return new FormatElmModuleTextResponseStructure
        {
            processOutput = processOutput,
            formattedText = Maybe.NothingFromNull(formattedText),
        };
    }

    static public byte[] GetElmFormatExecutableFile =>
        Pine.BlobLibrary.LoadFileForCurrentOs(ElmFormatExecutableFileByOs)
        ?.ToArray();

    static public IReadOnlyDictionary<System.Runtime.InteropServices.OSPlatform, (string hash, string remoteSource)> ElmFormatExecutableFileByOs =
        ImmutableDictionary<System.Runtime.InteropServices.OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            System.Runtime.InteropServices.OSPlatform.Linux,
            ("9acdd1006b9e4720f48cdbbb12f16262625c2a56145d56239f9c7b9a50ed0db4",
            @"https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-linux-x64.tgz"))
        .Add(
            System.Runtime.InteropServices.OSPlatform.Windows,
            ("57b8f899f16e879338a6f0d898a13e28c440ab3947b173197de82048e3b520d6",
            @"https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-win-x64.zip"))
        .Add(
            System.Runtime.InteropServices.OSPlatform.OSX,
            ("364469d9b64866e0595c9c2837eb330eeb1c58269d31567085fa24886b5a46d7",
            @"https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-mac-x64.tgz"));

}

string InterfaceToHost_Request(string request)
{
    return GetSerialResponseFromSerialRequest(request);
}
