using Mono.Unix;
using Pine;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
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

    static public IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> DenoExecutableFileByOs =
        ImmutableDictionary<OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            OSPlatform.Linux,
            ("34374d77515b093028df6605b6e74985aa6cd7a022c5e8310a14d65daecbd91d",
            @"https://github.com/denoland/deno/releases/download/v1.16.2/deno-x86_64-unknown-linux-gnu.zip"))
        .Add(
            OSPlatform.Windows,
            ("3a1611329306d24b24fd6b98418ddec9ee79c044623e712093db24eba6a573df",
            @"https://github.com/denoland/deno/releases/download/v1.16.2/deno-x86_64-pc-windows-msvc.zip"));

    static public byte[] ElmTestRsExecutableFileForCurrentOs() => LoadFileForCurrentOs(ElmTestRsExecutableFileByOs);

    static public byte[] DenoExecutableFileForCurrentOs() => LoadFileForCurrentOs(DenoExecutableFileByOs);

    static public byte[] LoadFileForCurrentOs(IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> dict)
    {
        var hashAndRemoteSource =
            dict.FirstOrDefault(c => RuntimeInformation.IsOSPlatform(c.Key)).Value;

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
        var elmTestExecutableFile = ElmTestRsExecutableFileForCurrentOs();

        var elmExecutableFileName = "elm.exe";

        string elmTestEnvironmentPath = null;

        {
            /*
             * We found no way yet to point elm-test-rs to the deno executable file.
             * As a temporary solution, adapt the file path and environment variables to help elm-test-rs find it.
             * */

            var (denoExecutableFileName, pathEnvironmentVarSeparator) =
                RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? ("deno.exe", ";") : ("deno", ":");

            var denoExecutableFileContent = DenoExecutableFileForCurrentOs();

            var denoExecutableFileDirectory =
                Path.Combine(Filesystem.CacheDirectory,
                "bin-by-sha256",
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(denoExecutableFileContent)));

            var denoExecutableFilePath = Path.Combine(denoExecutableFileDirectory, denoExecutableFileName);

            Directory.CreateDirectory(denoExecutableFileDirectory);

            File.WriteAllBytes(denoExecutableFilePath, denoExecutableFileContent);

            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            {
                var unixFileInfo = new UnixFileInfo(denoExecutableFilePath);

                unixFileInfo.FileAccessPermissions |=
                    FileAccessPermissions.GroupExecute | FileAccessPermissions.UserExecute | FileAccessPermissions.OtherExecute |
                    FileAccessPermissions.GroupRead | FileAccessPermissions.UserRead | FileAccessPermissions.OtherRead;
            }

            var environmentPath =
                System.Environment.GetEnvironmentVariable("PATH") ?? System.Environment.GetEnvironmentVariable("path");

            elmTestEnvironmentPath = environmentPath + pathEnvironmentVarSeparator + denoExecutableFileDirectory;
        }

        var environmentFilesExecutable =
            ImmutableDictionary.Create<IImmutableList<string>, IReadOnlyList<byte>>()
            .SetItem(ImmutableList.Create(elmExecutableFileName), ElmFullstack.ProcessFromElm019Code.GetElmExecutableFile);

        var executeElmTestResult =
            ExecutableFile.ExecuteFileWithArguments(
            environmentFilesNotExecutable: elmProjectFiles,
            executableFile: elmTestExecutableFile,
            arguments: "--compiler=./" + elmExecutableFileName + "  --deno  --report=json",
            environmentStrings: ImmutableDictionary<string, string>.Empty.SetItem("PATH", elmTestEnvironmentPath),
            workingDirectory: null,
            environmentFilesExecutable: environmentFilesExecutable); ;

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
