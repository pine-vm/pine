using Pine;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.InteropServices;

namespace elm_fullstack;

public record ElmTestRsReportJsonEntry(
    string @event,
    int? testCount,
    int? passed,
    int? failed,
    string status,
    string[] labels,
    double? duration,
    string initialSeed);

public class ElmTestRs
{
    static public IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> ElmTestRsExecutableFileByOs =
        ImmutableDictionary<OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            OSPlatform.Linux,
            ("889e609c85906335ea52cd01519814d528b06d8028e7695a2c8fcaf0d71e7408",
            @"https://github.com/mpizenberg/elm-test-rs/releases/download/v1.2.2/elm-test-rs_linux.tar.gz"))
        .Add(
            OSPlatform.Windows,
            ("98708c4ecd1a34b61545f0faca7ac3c46b5bb3d0bf6b4719b16eb4cffe0a82ac",
            @"https://github.com/mpizenberg/elm-test-rs/releases/download/v1.2.2/elm-test-rs_windows.zip"));

    static public byte[] LoadElmTestRsExecutableFileForCurrentOs()
    {
        var hashAndRemoteSource =
            ElmTestRsExecutableFileByOs
            .FirstOrDefault(c => RuntimeInformation.IsOSPlatform(c.Key)).Value;

        if (hashAndRemoteSource.hash == null)
            throw new System.Exception("Unknown OS: " + RuntimeInformation.OSDescription);

        var hash = CommonConversion.ByteArrayFromStringBase16(hashAndRemoteSource.hash);

        return BlobLibrary.GetBlobWithSHA256Cached(
            hash,
            getIfNotCached: () => BlobLibrary.DownloadFromUrlAndExtractBlobWithMatchingHash(hashAndRemoteSource.remoteSource, hash));
    }

    static public (string stdout, string stderr, IReadOnlyList<(string rawLine, ElmTestRsReportJsonEntry parsedLine)> stdoutLines) Run(
        IImmutableDictionary<IImmutableList<string>, IReadOnlyList<byte>> elmProjectFiles)
    {
        var elmTestExecutableFile = LoadElmTestRsExecutableFileForCurrentOs();

        var elmExecutableFileName = "elm.exe";

        var environmentFilesExecutable =
            ImmutableDictionary.Create<IImmutableList<string>, IReadOnlyList<byte>>().SetItem(
                ImmutableList.Create(elmExecutableFileName), ElmFullstack.ProcessFromElm019Code.GetElmExecutableFile);

        var executeElmTestResult =
            ExecutableFile.ExecuteFileWithArguments(
            environmentFilesNotExecutable: elmProjectFiles,
            executableFile: elmTestExecutableFile,
            arguments: "--compiler=./" + elmExecutableFileName + " --report=json",
            environmentStrings: null,
            workingDirectory: null,
            environmentFilesExecutable: environmentFilesExecutable);

        var stdoutLines =
            executeElmTestResult.processOutput.StandardOutput
            .Split(new char[] { (char)10, (char)13 })
            .Where(l => 0 < l?.Length)
            .ToImmutableList();

        var parsedLines =
            stdoutLines.Select(line => (line, Newtonsoft.Json.JsonConvert.DeserializeObject<ElmTestRsReportJsonEntry>(line)))
            .ToImmutableList();

        return (executeElmTestResult.processOutput.StandardOutput, executeElmTestResult.processOutput.StandardError, parsedLines);
    }
}
