using ElmTime.Elm019;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.InteropServices;

namespace ElmTime;

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
    public static IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> ElmTestRsExecutableFileByOs =
        ImmutableDictionary<OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            OSPlatform.Linux,
            ("15083a48b6818bc9e247e8bd3b198907a1e9c782270a662ddd560dd6f9d71ea8",
            @"https://github.com/mpizenberg/elm-test-rs/releases/download/v3.0/elm-test-rs_linux.tar.gz"))
        .Add(
            OSPlatform.Windows,
            ("b9a2d5d68307426e0b35f8030efbac940a3fa621a33ad2d84334a313cac33090",
            @"https://github.com/mpizenberg/elm-test-rs/releases/download/v3.0/elm-test-rs_windows.zip"))
        .Add(
            OSPlatform.OSX,
            ("bfe56245ac8648a705366defb8c6cf5d95c721527fcf318f87200c608354e61e",
            @"https://github.com/mpizenberg/elm-test-rs/releases/download/v3.0/elm-test-rs_macos.tar.gz"));

    public static IReadOnlyDictionary<OSPlatform, (string hash, string remoteSource)> DenoExecutableFileByOs =
        ImmutableDictionary<OSPlatform, (string hash, string remoteSource)>.Empty
        .Add(
            OSPlatform.Linux,
            ("29ac935066d598d98980107c924ed6e9511d3a9e0cb5ab18711ae735e7c8c257",
            @"https://github.com/denoland/deno/releases/download/v1.33.4/deno-x86_64-unknown-linux-gnu.zip"))
        .Add(
            OSPlatform.Windows,
            ("9e52db0ccc7619f1d1b6934122f1cfd01836bf579888ed4aee2c5ccad1c7eb64",
            @"https://github.com/denoland/deno/releases/download/v1.33.4/deno-x86_64-pc-windows-msvc.zip"))
        .Add(
            OSPlatform.OSX,
            ("e2522d5217a8a3d78e1876e184eae7b68f7d6498eb40765c1f6331b4756b4d6e",
            @"https://github.com/denoland/deno/releases/download/v1.33.4/deno-x86_64-apple-darwin.zip"));

    public static ReadOnlyMemory<byte>? ElmTestRsExecutableFileForCurrentOs() => BlobLibrary.LoadFileForCurrentOs(ElmTestRsExecutableFileByOs);

    public static ReadOnlyMemory<byte>? DenoExecutableFileForCurrentOs() => BlobLibrary.LoadFileForCurrentOs(DenoExecutableFileByOs);

    public static (ExecutableFile.ProcessOutput processOutput, IReadOnlyList<(string rawLine, ElmTestRsReportJsonEntry parsedLine)> stdoutLines) Run(
        IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> elmProjectFiles)
    {
        var elmTestExecutableFile = ElmTestRsExecutableFileForCurrentOs()!;

        var elmExecutableFileName = "elm" + ExecutableFile.ExecutableFileNameAppendix;

        /*
         * We found no way yet to point elm-test-rs to the deno executable file.
         * As a temporary solution, use the PATH environment variable to help elm-test-rs find it.
         * */

        var environmentPathExecutableFiles =
            ImmutableDictionary<string, ReadOnlyMemory<byte>>.Empty
            .SetItem("deno", DenoExecutableFileForCurrentOs()!.Value);

        var environmentFilesExecutable =
            ImmutableDictionary.Create<IReadOnlyList<string>, ReadOnlyMemory<byte>>()
            .SetItem([elmExecutableFileName], Elm019Binaries.GetElmExecutableFile);

        var executeElmTestResult =
            ExecutableFile.ExecuteFileWithArguments(
            environmentFilesNotExecutable: elmProjectFiles,
            executableFile: elmTestExecutableFile.Value,
            arguments: "--compiler=./" + elmExecutableFileName + "  --deno  --report=json",
            environmentStrings: null,
            workingDirectoryRelative: null,
            environmentFilesExecutable: environmentFilesExecutable,
            environmentPathExecutableFiles: environmentPathExecutableFiles);

        var stdout = executeElmTestResult.processOutput.StandardOutput;

        var stdoutLines =
            stdout
            .Split((char)10, (char)13)
            .Where(l => 0 < l?.Length)
            .ToImmutableList();

        var parsedLines =
            stdoutLines.Select(line => (line, DeserializeElmTestRsReportJsonEntry(line)))
            .ToImmutableList();

        return (executeElmTestResult.processOutput, parsedLines);
    }

    public static ElmTestRsReportJsonEntry DeserializeElmTestRsReportJsonEntry(string json)
    {
        var serializeOptions = new System.Text.Json.JsonSerializerOptions
        {
            NumberHandling = System.Text.Json.Serialization.JsonNumberHandling.AllowReadingFromString
        };

        serializeOptions.Converters.Add(new ElmTestRsReportJsonEntryFailureReasonDataJsonConverter());

        return System.Text.Json.JsonSerializer.Deserialize<ElmTestRsReportJsonEntry>(json, serializeOptions)!;
    }

    public static (IReadOnlyList<(string text, ElmTestRsConsoleOutputColor color)> text, bool? overallSuccess) OutputFromEvent(
        ElmTestRsReportJsonEntry @event)
    {
        if (@event.@event == "runStart")
        {
            return
                ([
                    (string.Join("\n",
                    "Running " + @event.testCount + " tests. To reproduce these results later,"
                    , "run elm-test-rs with --seed " + @event.initialSeed + " and --fuzz " + @event.fuzzRuns),
                    ElmTestRsConsoleOutputColor.DefaultColor)
                    ],
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
                ([
                    overallSuccessText,
                    (string.Join("\n",
                    "Duration: " + string.Format("{0:#,##0}", @event.duration) + " ms",
                    "Passed:   " + @event.passed,
                    "Failed:   " + @event.failed),
                    ElmTestRsConsoleOutputColor.DefaultColor)],
                overallSuccess);
        }

        if (@event.@event == "testsCompleted" && @event.status != "pass")
        {
            var textsFromLabels =
                (@event.labels ?? []).SkipLast(1).Select(label => ("\n↓ " + label, ElmTestRsConsoleOutputColor.DefaultColor))
                .Concat((@event.labels ?? []).TakeLast(1).Select(label => ("\n✗ " + label, ElmTestRsConsoleOutputColor.RedColor)))
                .ToImmutableList();

            static IReadOnlyList<string> renderFailureReasonData(ElmTestRsReportJsonEntryFailureReasonData failureReasonData)
            {
                if (failureReasonData.Equality != null)
                {
                    return [
                        "",
                        failureReasonData.Equality.actual,
                        "╷",
                        "│ " + failureReasonData.Equality.comparison,
                        "╵",
                        failureReasonData.Equality.expected,
                        ""
                    ];
                }

                if (failureReasonData.String != null)
                    return ["", failureReasonData.String, ""];

                throw new Exception("Incomplete match on sum type.");
            }

            var textsFromFailures =
                (@event.failures ?? [])
                .Select(failure => failure.reason?.data)
                .WhereNotNull()
                .SelectMany(renderFailureReasonData)
                .ToImmutableList();

            return
                ([.. textsFromLabels
                ,
                    .. textsFromFailures.Select(textFromFailure => ("\n    " + textFromFailure, ElmTestRsConsoleOutputColor.DefaultColor))],
                    null);
        }

        return ([], null);
    }
}

public enum ElmTestRsConsoleOutputColor
{
    DefaultColor = 0,
    RedColor = 2,
    GreenColor = 4,
}
