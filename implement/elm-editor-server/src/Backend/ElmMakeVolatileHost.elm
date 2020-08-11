module Backend.ElmMakeVolatileHost exposing
    ( jsonDecodeElmMakeResponseStructure
    , requestToVolatileHost
    , volatileHostScript
    )

import ElmFullstackCompilerInterface.GenerateJsonCoders
import FrontendBackendInterface
import Json.Decode
import Json.Encode


type alias ElmMakeRequestStructure =
    FrontendBackendInterface.ElmMakeRequestStructure


type alias ElmMakeResponseStructure =
    FrontendBackendInterface.ElmMakeResponseStructure


jsonDecodeElmMakeResponseStructure : Json.Decode.Decoder ElmMakeResponseStructure
jsonDecodeElmMakeResponseStructure =
    ElmFullstackCompilerInterface.GenerateJsonCoders.jsonDecodeElmMakeResponseStructure


jsonEncodeElmMakeRequestStructure : ElmMakeRequestStructure -> Json.Encode.Value
jsonEncodeElmMakeRequestStructure =
    ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeElmMakeRequestStructure


requestToVolatileHost : ElmMakeRequestStructure -> String
requestToVolatileHost =
    jsonEncodeElmMakeRequestStructure >> Json.Encode.encode 0


volatileHostScript : String
volatileHostScript =
    """
#r "netstandard"
#r "System"
#r "System.Collections.Immutable"
#r "System.Net"
#r "System.Net.Http"
#r "System.Net.Primitives"
#r "System.Private.Uri"
#r "System.Linq"
#r "System.Runtime.InteropServices.RuntimeInformation"

//  https://www.nuget.org/api/v2/package/Newtonsoft.Json/12.0.2
#r "sha256:b9b4e633ea6c728bad5f7cbbef7f8b842f7e10181731dbe5ec3cd995a6f60287"

// from elm-fullstack-separate-assemblies-24e368044d27f7101ba4e42a0a2f9e8a85f2a170-linux-x64
#r "sha256:52a3e6e6e01416baafbe00f2cffd8d159c36478aa6e571dac21a317f39f30007"

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;


public class ElmMakeRequestStructure
{
    public IReadOnlyList<FileWithPath> files;

    public string commandLineArguments;
}

public class FileWithPath
{
    public IReadOnlyList<string> path;

    public string contentBase64;
}

public class ElmMakeResultStructure
{
    public ProcessOutput processOutput;

    public IReadOnlyList<FileWithPath> files;
}

public struct ProcessOutput
{
    public string standardError;

    public string standardOutput;

    public int exitCode;
}


string GetResponseFromRequestSerial(string serializedRequest)
{
    var elmMakeRequest = Newtonsoft.Json.JsonConvert.DeserializeObject<ElmMakeRequestStructure>(serializedRequest);

    var elmCodeFiles =
        elmMakeRequest.files
        .ToImmutableDictionary(
            file => (IImmutableList<string>)file.path.ToImmutableList(),
            file => (IImmutableList<byte>)Convert.FromBase64String(file.contentBase64).ToImmutableList());

    var platformSpecificFiles =
        elmCodeFiles
        .Select(elmCodeFile => (MakePlatformSpecificPath(elmCodeFile.Key), elmCodeFile.Value))
        .ToImmutableList();

    var commandResults = Kalmit.ExecutableFile.ExecuteFileWithArguments(
        platformSpecificFiles,
        GetElmExecutableFile,
        elmMakeRequest.commandLineArguments,
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
        });

    var platformSpecificNewFiles =
        commandResults.resultingFiles
        .Where(file => !platformSpecificFiles.Any(inputFile => inputFile.Item1 == file.name))
        .ToImmutableList();

    var newFiles =
        platformSpecificNewFiles
        .Select(file => new FileWithPath
        {
            path = file.name.Split('/', '\\\\'),
            contentBase64 = Convert.ToBase64String(file.content.ToArray()),
        })
        .ToImmutableList();

    var processOutput = new ProcessOutput
    {
        standardOutput = commandResults.processOutput.StandardOutput,
        standardError = commandResults.processOutput.StandardError,
        exitCode = commandResults.processOutput.ExitCode,
    };

    var responseStructure = new ElmMakeResultStructure
    {
        processOutput = processOutput,
        files = newFiles,
    };

    return Newtonsoft.Json.JsonConvert.SerializeObject(responseStructure);
}

string MakePlatformSpecificPath(IImmutableList<string> path) =>
    string.Join(System.IO.Path.DirectorySeparatorChar.ToString(), path);

static public byte[] GetElmExecutableFile =>
    Kalmit.CommonConversion.DecompressGzip(GetElmExecutableFileCompressedGzip);

static public byte[] GetElmExecutableFileCompressedGzip =>
    Kalmit.BlobLibrary.GetBlobWithSHA256(Kalmit.CommonConversion.ByteArrayFromStringBase16(
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
        "d1bf666298cbe3c5447b9ca0ea608552d750e5d232f9845c2af11907b654903b"));

static public string overrideElmMakeHomeDirectory = null;

static string elmHomeDirectory;

static public string GetElmHomeDirectory()
{
    elmHomeDirectory =
        overrideElmMakeHomeDirectory ??
        elmHomeDirectory ??
        System.IO.Path.Combine(Kalmit.Filesystem.CreateRandomDirectoryInTempDirectory(), "elm-home");

    System.IO.Directory.CreateDirectory(elmHomeDirectory);
    return elmHomeDirectory;
}


string InterfaceToHost_Request(string request)
{
    return GetResponseFromRequestSerial(request);
}

"""
