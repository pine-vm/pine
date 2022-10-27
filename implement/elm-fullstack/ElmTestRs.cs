using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.InteropServices;

namespace ElmFullstack;

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
        Type typeToConvert,
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
        throw new NotImplementedException();
    }
}

public class ElmTestRs
{
    static public IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> ElmTestRsExecutableFileByOs =
        ImmutableDictionary<OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            OSPlatform.Linux,
            ("470e7fcf96cafe569455df522c03f577ab317a6fd758968feb208e751627fbf5",
            @"https://github.com/mpizenberg/elm-test-rs/releases/download/v2.0.1/elm-test-rs_linux.tar.gz"))
        .Add(
            OSPlatform.Windows,
            ("a8ef939a9a7b91361745c2ce188c23ef246031693d4770ee3b0f82775b71f242",
            @"https://github.com/mpizenberg/elm-test-rs/releases/download/v2.0.1/elm-test-rs_windows.zip"));

    static public IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> DenoExecutableFileByOs =
        ImmutableDictionary<OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            OSPlatform.Linux,
            ("227d743e45ebac69c3dccb406929ac671ad41d9e6dc22621c6111ab88c6c287f",
            @"https://github.com/denoland/deno/releases/download/v1.20.6/deno-x86_64-unknown-linux-gnu.zip"))
        .Add(
            OSPlatform.Windows,
            ("c531a3fb716175599033a5b71b150c14cc260c79a5c2f180927c840ac5d470c4",
            @"https://github.com/denoland/deno/releases/download/v1.20.6/deno-x86_64-pc-windows-msvc.zip"));

    static public ReadOnlyMemory<byte>? ElmTestRsExecutableFileForCurrentOs() => BlobLibrary.LoadFileForCurrentOs(ElmTestRsExecutableFileByOs);

    static public ReadOnlyMemory<byte>? DenoExecutableFileForCurrentOs() => BlobLibrary.LoadFileForCurrentOs(DenoExecutableFileByOs);

    static public (ExecutableFile.ProcessOutput processOutput, IReadOnlyList<(string rawLine, ElmTestRsReportJsonEntry parsedLine)> stdoutLines) Run(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmProjectFiles)
    {
        var elmTestExecutableFile = ElmTestRsExecutableFileForCurrentOs()!;

        var elmExecutableFileName = "elm.exe";

        /*
         * We found no way yet to point elm-test-rs to the deno executable file.
         * As a temporary solution, use the path environment variable to help elm-test-rs find it.
         * */

        var environmentPathExecutableFiles =
            ImmutableDictionary<string, ReadOnlyMemory<byte>>.Empty
            .SetItem("deno", DenoExecutableFileForCurrentOs()!.Value);

        var environmentFilesExecutable =
            ImmutableDictionary.Create<IReadOnlyList<string>, ReadOnlyMemory<byte>>()
            .SetItem(ImmutableList.Create(elmExecutableFileName), Elm019Binaries.GetElmExecutableFile);

        var executeElmTestResult =
            ExecutableFile.ExecuteFileWithArguments(
            environmentFilesNotExecutable: elmProjectFiles,
            executableFile: elmTestExecutableFile.Value,
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

        return (executeElmTestResult.processOutput, parsedLines);
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
                overallSuccess);
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

                throw new Exception("Incomplete match on sum type.");
            }

            var textsFromFailures =
                @event.failures.EmptyIfNull()
                .Select(failure => failure.reason?.data)
                .WhereNotNull()
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
    static public ElmTestRsConsoleOutputColor DefaultColor => new(Default: new object());
    static public ElmTestRsConsoleOutputColor RedColor => new(Red: new object());
    static public ElmTestRsConsoleOutputColor GreenColor => new(Green: new object());
}
