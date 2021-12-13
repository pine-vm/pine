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
    ElmTestRsReportJsonEntryFailure[] failures,
    double? duration,
    string initialSeed,
    int? fuzzRuns);

public record ElmTestRsReportJsonEntryFailure(
    string message,
    ElmTestRsReportJsonEntryFailureReason reason);

public record ElmTestRsReportJsonEntryFailureReason(
    string type,
    ElmTestRsReportJsonEntryFailureReasonData data);

public record ElmTestRsReportJsonEntryFailureReasonData(
    string? @String = null,
    ElmTestRsReportJsonEntryFailureReasonDataEquality? Equality = null);

public record ElmTestRsReportJsonEntryFailureReasonDataEquality(
    string expected,
    string actual,
    string comparison);

public class ElmTestRsReportJsonEntryFailureReasonDataJsonConverter : System.Text.Json.Serialization.JsonConverter<ElmTestRsReportJsonEntryFailureReasonData>
{
    public override ElmTestRsReportJsonEntryFailureReasonData Read(
        ref System.Text.Json.Utf8JsonReader reader,
        System.Type typeToConvert,
        System.Text.Json.JsonSerializerOptions options)
    {
        try
        {
            var equalityReader = reader;

            var asEquality = new ElmTestRsReportJsonEntryFailureReasonData(
                Equality: System.Text.Json.JsonSerializer.Deserialize<ElmTestRsReportJsonEntryFailureReasonDataEquality>(ref equalityReader));

            reader = equalityReader;

            return asEquality;
        }
        catch { }

        return new ElmTestRsReportJsonEntryFailureReasonData(
            String: System.Text.Json.JsonSerializer.Deserialize<string>(ref reader));
    }

    public override void Write(
        System.Text.Json.Utf8JsonWriter writer,
        ElmTestRsReportJsonEntryFailureReasonData value,
        System.Text.Json.JsonSerializerOptions options)
    {
        throw new System.NotImplementedException();
    }
}

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

    static public byte[]? ElmTestRsExecutableFileForCurrentOs() => LoadFileForCurrentOs(ElmTestRsExecutableFileByOs);

    static public byte[]? DenoExecutableFileForCurrentOs() => LoadFileForCurrentOs(DenoExecutableFileByOs);

    static public byte[]? LoadFileForCurrentOs(IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> dict)
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
        var elmTestExecutableFile = ElmTestRsExecutableFileForCurrentOs()!;

        var elmExecutableFileName = "elm.exe";

        /*
         * We found no way yet to point elm-test-rs to the deno executable file.
         * As a temporary solution, use the path environment variable to help elm-test-rs find it.
         * */

        var environmentPathExecutableFiles =
            ImmutableDictionary<string, IReadOnlyList<byte>>.Empty
            .SetItem("deno", DenoExecutableFileForCurrentOs()!);

        var environmentFilesExecutable =
            ImmutableDictionary.Create<IImmutableList<string>, IReadOnlyList<byte>>()
            .SetItem(ImmutableList.Create(elmExecutableFileName), ElmFullstack.ProcessFromElm019Code.GetElmExecutableFile);

        var executeElmTestResult =
            ExecutableFile.ExecuteFileWithArguments(
            environmentFilesNotExecutable: elmProjectFiles,
            executableFile: elmTestExecutableFile,
            arguments: "--compiler=./" + elmExecutableFileName + "  --deno  --report=json",
            environmentStrings: null,
            workingDirectory: null,
            environmentFilesExecutable: environmentFilesExecutable,
            environmentPathExecutableFiles: environmentPathExecutableFiles);

        var stdout = executeElmTestResult.processOutput.StandardOutput;

        var stdoutLines =
            stdout
            .Split(new char[] { (char)10, (char)13 })
            .Where(l => 0 < l?.Length)
            .ToImmutableList();

        var parsedLines =
            stdoutLines.Select(line => (line, DeserializeElmTestRsReportJsonEntry(line)))
            .ToImmutableList();

        return (stdout, executeElmTestResult.processOutput.StandardError, parsedLines);
    }

    static public ElmTestRsReportJsonEntry DeserializeElmTestRsReportJsonEntry(string json)
    {
        var serializeOptions = new System.Text.Json.JsonSerializerOptions
        {
            NumberHandling = System.Text.Json.Serialization.JsonNumberHandling.AllowReadingFromString
        };

        serializeOptions.Converters.Add(new ElmTestRsReportJsonEntryFailureReasonDataJsonConverter());

        return System.Text.Json.JsonSerializer.Deserialize<ElmTestRsReportJsonEntry>(json, serializeOptions)!;
    }

    static public (IReadOnlyList<(string text, ElmTestRsConsoleOutputColor color)> text, bool? overallSuccess) OutputFromEvent(
        ElmTestRsReportJsonEntry @event)
    {
        if (@event.@event == "runStart")
        {
            return
                (ImmutableList.Create(
                    (string.Join("\n",
                    "Running " + @event.testCount + " tests. To reproduce these results later,"
                    , "run elm-test-rs with --seed " + @event.initialSeed + " and --fuzz " + @event.fuzzRuns),
                    ElmTestRsConsoleOutputColor.DefaultColor)
                    ),
                overallSuccess: null);
        }

        if (@event.@event == "runComplete")
        {
            var overallSuccess = @event.failed == 0;

            var overallSuccessText =
                overallSuccess ?
                ("\nTEST RUN PASSED\n\n", ElmTestRsConsoleOutputColor.GreenColor)
                :
                ("\nTEST RUN FAILED\n\n", ElmTestRsConsoleOutputColor.RedColor);

            return
                (ImmutableList.Create(
                    overallSuccessText,
                    (string.Join("\n",
                    "Duration: " + string.Format("{0:#,##0}", @event.duration) + " ms",
                    "Passed:   " + @event.passed,
                    "Failed:   " + @event.failed),
                    ElmTestRsConsoleOutputColor.DefaultColor)),
                overallSuccess: overallSuccess);
        }

        if (@event.@event == "testsCompleted" && @event.status != "pass")
        {
            var textsFromLabels =
                @event.labels.EmptyIfNull().SkipLast(1).Select(label => ("\n↓ " + label, ElmTestRsConsoleOutputColor.DefaultColor))
                .Concat(@event.labels.EmptyIfNull().TakeLast(1).Select(label => ("\n✗ " + label, ElmTestRsConsoleOutputColor.RedColor)))
                .ToImmutableList();

            static IReadOnlyList<string> renderFailureReasonData(ElmTestRsReportJsonEntryFailureReasonData failureReasonData)
            {
                if (failureReasonData.Equality != null)
                {
                    return ImmutableList.Create(
                        "",
                        failureReasonData.Equality.actual,
                        "╷",
                        "│ " + failureReasonData.Equality.comparison,
                        "╵",
                        failureReasonData.Equality.expected,
                        ""
                    );
                }

                if (failureReasonData.String != null)
                    return ImmutableList.Create("", failureReasonData.String, "");

                throw new System.Exception("Incomplete match on sum type.");
            }

            var textsFromFailures =
                @event.failures.EmptyIfNull()
                .Select(failure => failure.reason?.data)
                .WhereNotNull()!
                .SelectMany(renderFailureReasonData)
                .ToImmutableList();

            return
                (textsFromLabels.Concat(
                    textsFromFailures
                    .Select(textFromFailure => ("\n    " + textFromFailure, ElmTestRsConsoleOutputColor.DefaultColor))).ToImmutableList(),
                    null);
        }

        return (ImmutableList<(string text, ElmTestRsConsoleOutputColor color)>.Empty, null);
    }
}

public record ElmTestRsConsoleOutputColor(object? Default = null, object? Red = null, object? Green = null)
{
    static public ElmTestRsConsoleOutputColor DefaultColor => new ElmTestRsConsoleOutputColor(Default: new object());
    static public ElmTestRsConsoleOutputColor RedColor => new ElmTestRsConsoleOutputColor(Red: new object());
    static public ElmTestRsConsoleOutputColor GreenColor => new ElmTestRsConsoleOutputColor(Green: new object());
}
