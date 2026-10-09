using AwesomeAssertions;
using Pine.CLI;
using Pine.CLI.Elm;
using Pine.Core;
using Pine.Core.CLI;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.Testing;
using Pine.Core.Interpreter.IntermediateVM;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using Xunit;

namespace Pine.IntegrationTests.CLI.Elm;

public class TestCommandTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Numeric_help_documents_count_and_time_input_syntax(bool profile)
    {
        var output = new StringWriter();

        TestCommand.Create().Parse(profile ? ["profile", "--help"] : ["--help"])
            .Invoke(new InvocationConfiguration { Output = output }).Should().Be(0);

        output.ToString().Should().Contain("underscores").And.Contain("k/M/G").And.Contain("ms/s/min/h");

        if (profile)
        {
            output.ToString().Should().Contain("backward-jump destinations").And.Contain("--no-inlining")
                .And.Contain("--no-application-fast-paths");
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Numeric_count_options_accept_underscores_and_decimal_SI_units(bool profile)
    {
        var command = TestCommand.Create();
        var commonCommand = profile ? command.Subcommands.Single(child => child.Name == "profile") : command;

        string[] values =
            [
            ".", "--workers", "1__000", "--seed", "+4_294_967_295", "--fuzz", "4 G",
            "--budget", "2G", "--invocation-budget", "1 k", "--loop-budget", "1 M",
            "--max-stack-depth", "100 k",
            ];

        var parsed = command.Parse(profile ? ["profile", .. values] : values);
        parsed.Errors.Should().BeEmpty();

        parsed.GetValue(commonCommand.Options.OfType<Option<int?>>().Single(option => option.Name == "--workers"))
            .Should().Be(1_000);

        parsed.GetValue(commonCommand.Options.OfType<Option<uint?>>().Single(option => option.Name == "--seed"))
            .Should().Be(uint.MaxValue);

        parsed.GetValue(commonCommand.Options.OfType<Option<uint>>().Single(option => option.Name == "--fuzz"))
            .Should().Be(4_000_000_000);

        parsed.GetValue(command.Options.OfType<Option<int?>>().Single(option => option.Name == "--budget"))
            .Should().Be(2_000_000_000);

        parsed.GetValue(command.Options.OfType<Option<int?>>().Single(option => option.Name == "--invocation-budget"))
            .Should().Be(1_000);

        parsed.GetValue(command.Options.OfType<Option<int?>>().Single(option => option.Name == "--loop-budget"))
            .Should().Be(1_000_000);

        parsed.GetValue(command.Options.OfType<Option<int>>().Single(option => option.Name == "--max-stack-depth"))
            .Should().Be(100_000);

        command.Parse(["--budget", "1 k", "profile", "."]).Errors.Should().BeEmpty();
    }

    [Fact]
    public void Profile_small_count_options_accept_underscores()
    {
        var command = TestCommand.Create();
        var profile = command.Subcommands.Single(child => child.Name == "profile");

        var parsed =
            command.Parse(["profile", ".", "--stack-depth", "1_000", "--top", "2__000", "--max-samples", "3__000"]);

        parsed.Errors.Should().BeEmpty();

        parsed.GetValue(profile.Options.OfType<Option<int>>().Single(option => option.Name == "--stack-depth"))
            .Should().Be(1_000);

        parsed.GetValue(profile.Options.OfType<Option<int>>().Single(option => option.Name == "--top"))
            .Should().Be(2_000);

        parsed.GetValue(profile.Options.OfType<Option<int>>().Single(option => option.Name == "--max-samples"))
            .Should().Be(3_000);
    }

    [Fact]
    public void Profile_sample_limit_and_no_stacks_are_forwarded_to_settings()
    {
        var command = new Command("profile");
        var options = TestProfileCommand.AddOptions(command, TestBudgetOptions.AddTo(command));
        var parsed = command.Parse(["--max-samples", "3__000", "--no-stacks"]);
        parsed.Errors.Should().BeEmpty();
        var settings = options.Read(parsed);
        settings.Instrumentation.MaxSamples.Should().Be(3_000);
        settings.ShowStackTraces.Should().BeFalse();
        settings.Sort.Should().Be(TestProfileSort.Loops);
    }

    [Theory]
    [InlineData("--no-inlining")]
    [InlineData("--no-application-fast-paths")]
    [InlineData("--no-tail-recursion")]
    [InlineData("--no-reduction")]
    public void Profile_compiler_controls_are_independent_and_preserve_call_boundary_guidance(string optionName)
    {
        var command = new Command("profile");
        var options = TestProfileCommand.AddOptions(command, TestBudgetOptions.AddTo(command));
        var defaults = options.Read(command.Parse([])).Instrumentation;
        defaults.DisableInlining.Should().BeFalse();
        defaults.DisableApplicationFastPaths.Should().BeFalse();
        defaults.DisableTailRecursion.Should().BeFalse();
        defaults.DisableReduction.Should().BeFalse();

        var parsed = command.Parse([optionName]);
        parsed.Errors.Should().BeEmpty();
        var settings = options.Read(parsed).Instrumentation;
        settings.DisableInlining.Should().Be(optionName is "--no-inlining");
        settings.DisableApplicationFastPaths.Should().Be(optionName is "--no-application-fast-paths");
        settings.DisableTailRecursion.Should().Be(optionName is "--no-tail-recursion");
        settings.DisableReduction.Should().Be(optionName is "--no-reduction");

        options.NoInlining.Description.Should().Contain("call boundaries")
            .And.Contain("separate from expression reduction and tail-call replacement")
            .And.Contain("Backward jumps can remain");

        options.NoApplicationFastPaths.Description.Should().Contain("generic-application chain consolidation")
            .And.Contain("eval/template continuation shortcuts").And.Contain("preserving ordinary nested eval")
            .And.Contain("direct-call instruction lowering").And.Contain("runtime curried-application shortcuts")
            .And.NotContain("may remain");

        TestCommand.Create().Parse(["profile", ".", optionName]).Errors.Should().BeEmpty();
    }

    [Theory]
    [InlineData("--workers")]
    [InlineData("--stack-depth")]
    [InlineData("--top")]
    [InlineData("--max-samples")]
    public void Small_count_options_reject_SI_units_and_keep_simple_help(string optionName)
    {
        var command = TestCommand.Create();
        var profile = command.Subcommands.Single(child => child.Name == "profile");
        var option = profile.Options.Single(option => option.Name == optionName);

        option.Description.Should().NotContain("units").And.NotContain("underscores").And.NotContain("k/M/G");

        foreach (var suffix in new[] { "k", "M", "G", " k", " M", " G" })
        {
            command.Parse(["profile", ".", optionName, "1" + suffix]).Errors.Should().NotBeEmpty();

            if (optionName is "--workers")
                command.Parse([".", optionName, "1" + suffix]).Errors.Should().NotBeEmpty();
        }

        foreach (var input in new[] { "20", "2_0", "2__0" })
        {
            var parsed = command.Parse(["profile", ".", optionName, input]);
            parsed.Errors.Should().BeEmpty();

            if (option is Option<int?> nullableOption)
                parsed.GetValue(nullableOption).Should().Be(20);

            else if (option is Option<int> integerOption)
                parsed.GetValue(integerOption).Should().Be(20);
        }

        foreach (var input in new[] { "0", "-1", "_20", "20_", "2147483648" })
            command.Parse(["profile", ".", optionName, input]).Errors.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("--workers", "3G")]
    [InlineData("--workers", "0k")]
    [InlineData("--fuzz", "5G")]
    [InlineData("--fuzz", "0k")]
    [InlineData("--fuzz", "-1k")]
    [InlineData("--seed", "1k")]
    [InlineData("--seed", "4_294_967_296")]
    [InlineData("--budget", "3G")]
    [InlineData("--budget", "9223372036854775807G")]
    [InlineData("--budget", "0 M")]
    [InlineData("--invocation-budget", "-1k")]
    [InlineData("--loop-budget", "1K")]
    [InlineData("--max-stack-depth", "0k")]
    [InlineData("--max-stack-depth", "2_147_483_648")]
    public void Both_commands_reject_invalid_or_out_of_range_numeric_counts(string option, string value)
    {
        var command = TestCommand.Create();
        command.Parse([".", option, value]).Errors.Should().NotBeEmpty();
        command.Parse(["profile", ".", option, value]).Errors.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("--stack-depth", "0")]
    [InlineData("--stack-depth", "-1")]
    [InlineData("--stack-depth", "0k")]
    [InlineData("--stack-depth", "3G")]
    [InlineData("--top", "0")]
    [InlineData("--top", "-1")]
    [InlineData("--top", "-1k")]
    [InlineData("--top", "3G")]
    [InlineData("--max-samples", "0")]
    [InlineData("--max-samples", "-1")]
    [InlineData("--max-samples", "3G")]
    [InlineData("--max-samples", "2147483648")]
    public void Profile_rejects_nonpositive_and_out_of_range_numeric_counts(string option, string value) =>
        TestCommand.Create().Parse(["profile", ".", option, value]).Errors.Should().NotBeEmpty();

    [Theory]
    [InlineData("1__250ms", 1.25)]
    [InlineData("2 seconds", 2)]
    [InlineData("3min", 180)]
    [InlineData("4 m", 240)]
    [InlineData("1h", 3600)]
    [InlineData("1 hours", 3600)]
    [InlineData("0.001", 0.001)]
    [InlineData("1.25", 1.25)]
    [InlineData("+.5", 0.5)]
    [InlineData("1e-3", 0.001)]
    [InlineData("2E1", 20)]
    public void Clock_options_accept_integer_units_and_legacy_fractional_seconds(string value, double seconds)
    {
        var command = TestCommand.Create();
        var profile = command.Subcommands.Single(child => child.Name == "profile");
        var timeout = command.Options.OfType<Option<double?>>().Single(option => option.Name == "--timeout");
        var interval = profile.Options.OfType<Option<double>>().Single(option => option.Name == "--interval");
        var ordinary = command.Parse([".", "--timeout", value]);
        ordinary.Errors.Should().BeEmpty();
        ordinary.GetValue(timeout).Should().Be(seconds);

        var parsed = command.Parse(["profile", ".", "--timeout", value, "--interval", value]);
        parsed.Errors.Should().BeEmpty();
        parsed.GetValue(timeout).Should().Be(seconds);
        parsed.GetValue(interval).Should().Be(seconds);
    }

    [Theory]
    [InlineData("0")]
    [InlineData("0 ms")]
    [InlineData("0.0")]
    [InlineData("0e1")]
    public void Zero_sampling_interval_remains_supported_but_timeout_must_be_positive(string value)
    {
        var command = TestCommand.Create();
        var profile = command.Subcommands.Single(child => child.Name == "profile");
        var interval = profile.Options.OfType<Option<double>>().Single(option => option.Name == "--interval");
        var parsed = command.Parse(["profile", ".", "--interval", value]);
        parsed.Errors.Should().BeEmpty();
        parsed.GetValue(interval).Should().Be(0);
        command.Parse(["profile", ".", "--timeout", value]).Errors.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("_1")]
    [InlineData("1_")]
    [InlineData("1__")]
    [InlineData("1_0.5")]
    [InlineData("1s_")]
    [InlineData("1K")]
    [InlineData("0.5s")]
    [InlineData("9223372036854775808")]
    [InlineData("1e999")]
    [InlineData("-Infinity")]
    [InlineData("NaN")]
    public void Clock_options_do_not_fall_back_for_invalid_integer_syntax_or_nonfinite_values(string value)
    {
        var command = TestCommand.Create();
        command.Parse([".", "--timeout", value]).Errors.Should().NotBeEmpty();
        command.Parse(["profile", ".", "--interval", value]).Errors.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("--timeout", "4_294_967_295ms")]
    [InlineData("--timeout", "1194h")]
    [InlineData("--timeout", "1e308")]
    [InlineData("--interval", "922_337_203_685_478ms")]
    [InlineData("--interval", "9223372036854775807 hours")]
    [InlineData("--interval", "1e308")]
    public void Clock_option_range_checks_apply_after_unit_conversion(string option, string value) =>
        TestCommand.Create().Parse(["profile", ".", option, value]).Errors.Should().NotBeEmpty();

    [Fact]
    public void Timeout_accepts_the_timer_limit_in_milliseconds()
    {
        var command = TestCommand.Create();
        var timeout = command.Options.OfType<Option<double?>>().Single(option => option.Name == "--timeout");
        var parsed = command.Parse([".", "--timeout", "4_294_967_294ms"]);
        parsed.Errors.Should().BeEmpty();
        parsed.GetValue(timeout).Should().Be(4_294_967.294);
    }

    [Theory]
    [InlineData("--workers")]
    [InlineData("--seed")]
    [InlineData("--fuzz")]
    [InlineData("--budget")]
    [InlineData("--invocation-budget")]
    [InlineData("--loop-budget")]
    [InlineData("--max-stack-depth")]
    [InlineData("--timeout")]
    [InlineData("--interval")]
    [InlineData("--stack-depth")]
    [InlineData("--top")]
    [InlineData("--max-samples")]
    public void Explicit_numeric_options_require_a_value_instead_of_using_defaults(string option) =>
        TestCommand.Create().Parse(["profile", ".", option]).Errors.Should().NotBeEmpty();

    [Fact]
    public void Shared_numeric_parsers_preserve_absent_nullable_options_and_defaults()
    {
        var command = TestCommand.Create();
        var profile = command.Subcommands.Single(child => child.Name == "profile");
        var ordinary = command.Parse([]);
        ordinary.Errors.Should().BeEmpty();

        foreach (var option in command.Options.OfType<Option<int?>>())
            ordinary.GetValue(option).Should().BeNull();

        ordinary.GetValue(command.Options.OfType<Option<uint?>>().Single(option => option.Name == "--seed"))
            .Should().BeNull();

        ordinary.GetValue(command.Options.OfType<Option<double?>>().Single(option => option.Name == "--timeout"))
            .Should().BeNull();

        ordinary.GetValue(command.Options.OfType<Option<uint>>().Single(option => option.Name == "--fuzz"))
            .Should().Be(100);

        ordinary.GetValue(command.Options.OfType<Option<int>>().Single(option => option.Name == "--max-stack-depth"))
            .Should().Be(100_000);

        var parsed = command.Parse(["profile", "."]);
        parsed.Errors.Should().BeEmpty();

        parsed.GetValue(profile.Options.OfType<Option<int?>>().Single(option => option.Name == "--workers"))
            .Should().BeNull();

        parsed.GetValue(profile.Options.OfType<Option<double>>().Single(option => option.Name == "--interval"))
            .Should().Be(5);

        foreach (var option in profile.Options.OfType<Option<int>>())
            parsed.GetValue(option).Should().Be(option.Name is "--max-samples" ? 200 : 20);

        parsed.GetValue(profile.Options.OfType<Option<TestProfileSort>>().Single(option => option.Name == "--sort"))
            .Should().Be(TestProfileSort.Loops);

        new TestProfileSettings().Sort.Should().Be(TestProfileSort.Loops);
    }

    [Theory]
    [InlineData("-2_147_483_648", int.MinValue)]
    [InlineData("+2_147_483_647", int.MaxValue)]
    public void Integer_option_helper_accepts_signed_boundaries_for_nullable_and_required_types(
        string value,
        int expected)
    {
        var required = new Option<int>("--required");
        var nullable = new Option<int?>("--nullable");
        NumericOptionParsing.SetIntegerParser(required);
        NumericOptionParsing.SetIntegerParser(nullable);
        var command = new Command("numbers") { required, nullable };
        var parsed = command.Parse(["--required", value, "--nullable", value]);
        parsed.Errors.Should().BeEmpty();
        parsed.GetValue(required).Should().Be(expected);
        parsed.GetValue(nullable).Should().Be(expected);
    }

    [Theory]
    [InlineData("2_147_483_648")]
    [InlineData("-2_147_483_649")]
    [InlineData("1k")]
    [InlineData("1_")]
    public void Integer_option_helper_reports_errors_without_replacing_explicit_values_with_defaults(string value)
    {
        var option = new Option<int>("--number") { DefaultValueFactory = _ => 42 };
        NumericOptionParsing.SetIntegerParser(option);
        var command = new Command("numbers") { option };
        command.Parse([]).GetValue(option).Should().Be(42);
        var parsed = command.Parse(["--number", value]);
        parsed.Errors.Should().NotBeEmpty();
        var ran = false;
        command.SetAction(_ => ran = true);

        command.Parse(["--number", value]).Invoke(new InvocationConfiguration { Error = new StringWriter() })
            .Should().NotBe(0);

        ran.Should().BeFalse();
    }

    [Fact]
    public void Time_option_helper_converts_legacy_fractional_default_units_to_seconds()
    {
        var option = new Option<double?>("--time");
        NumericOptionParsing.SetTimeParser(option, TimeUnit.Milliseconds);
        var command = new Command("numbers") { option };
        var parsed = command.Parse(["--time", "1.5"]);
        parsed.Errors.Should().BeEmpty();
        parsed.GetValue(option).Should().Be(0.0015);
        command.Parse([]).GetValue(option).Should().BeNull();
    }

    [Fact]
    public void Profile_rejects_sampling_intervals_that_overflow_TimeSpan_before_execution()
    {
        TestCommand.Create().Parse(
            [
            "profile", ".", "--interval",
            TimeSpan.MaxValue.TotalSeconds.ToString("R", CultureInfo.InvariantCulture)
            ])
            .Errors.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("")]
    [InlineData("\0")]
    public void Profile_invalid_output_paths_are_reported_before_execution(string path)
    {
        var ran = false;
        var (console, output) = CreateConsole(AnsiSupport.No);

        TestProfileCommand.Execute(
            ".",
            null,
            new() { OutputPath = path },
            _ =>
            {
                ran = true;
                return 0;
            },
            console).Should().Be(1);

        ran.Should().BeFalse();
        output.ToString().Should().Contain("Error:").And.NotContain("Effective execution limits:");
    }

    [Fact]
    public void Profile_report_write_errors_are_not_reported_as_success()
    {
        var directory = Path.Combine(Path.GetTempPath(), "pine-profile-write-error-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestProfileCommand.Execute(".", null, new() { OutputPath = directory }, _ => 0, console)
                .Should().Be(1);

            output.ToString().Should().Contain("Error saving instrumentation report:")
                .And.NotContain("JSON SHA256:").And.NotContain("Saved JSON report:");
        }
        finally
        {
            Directory.Delete(directory);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Profile_ranking_columns_align_headers_and_rows_for_large_formatted_counters(bool useLargestCounters)
    {
        ElmTestExpressionProfile[] expressions =
            [
                new(new string('a', 64), ["First"], 232_924, 0, 0, ""),
                new(new string('b', 64), ["Second"], 100_590, 2_765_963, 168_829, ""),
                new(
                    new string('c', 64),
                    ["Third"],
                    useLargestCounters ? long.MaxValue : 1,
                    useLargestCounters ? long.MaxValue - 1 : 0,
                    useLargestCounters ? long.MaxValue - 2 : 0,
                    ""),
            ];

        var report =
            new ElmTestProfileReport(
                1,
                new(),
                null,
                new("completed", null, "execution", "one test", 0, default),
                [],
                expressions,
                new Dictionary<string, ElmTestProfileValueNode>());

        var lines = new List<string>();
        TestProfileCommand.Render(report, new(), (line, _) => lines.Add(line));

        var header = lines[1];
        var invocationEnd = header.IndexOf("Invocations", StringComparison.Ordinal) + "Invocations".Length;
        var loopEnd = header.IndexOf("Loops", StringComparison.Ordinal) + "Loops".Length;
        var instructionEnd = header.IndexOf("Instructions", StringComparison.Ordinal) + "Instructions".Length;
        var declarationStart = header.IndexOf("Declarations", StringComparison.Ordinal);

        var sorted =
            expressions.OrderByDescending(row => row.LoopIterations)
            .ThenBy(row => row.Hash, StringComparer.Ordinal).ToArray();

        lines.Should().HaveCount(2 + sorted.Length);

        for (var index = 0; index < sorted.Length; index++)
        {
            var row = lines[index + 2];
            var expression = sorted[index];
            var invocations = CommandLineInterface.FormatIntegerForDisplay(expression.Invocations);
            var loops = CommandLineInterface.FormatIntegerForDisplay(expression.LoopIterations);
            var instructions = CommandLineInterface.FormatIntegerForDisplay(expression.Instructions);
            row[..16].Should().Be(expression.Hash[..16]);
            row.Substring(invocationEnd - invocations.Length, invocations.Length).Should().Be(invocations);
            row.Substring(loopEnd - loops.Length, loops.Length).Should().Be(loops);
            row.Substring(instructionEnd - instructions.Length, instructions.Length).Should().Be(instructions);
            row.Substring(invocationEnd, 2).Should().Be("  ");
            row.Substring(loopEnd, 2).Should().Be("  ");
            row.Substring(instructionEnd, 2).Should().Be("  ");
            row[declarationStart..].Should().Be(string.Join(", ", expression.Declarations));
        }
    }

    [Fact]
    public void Profile_ranking_with_no_expressions_still_renders_its_headers()
    {
        var report =
            new ElmTestProfileReport(
                1,
                new(),
                null,
                new("completed", null, "execution", "one test", 0, default),
                [],
                [],
                new Dictionary<string, ElmTestProfileValueNode>());

        var lines = new List<string>();
        TestProfileCommand.Render(report, new(), (line, _) => lines.Add(line));
        lines.Should().HaveCount(2);
        lines[1].Should().Contain("Invocations  Loops  Instructions  Declarations");
    }

    [Theory]
    [InlineData(true, true)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(false, false)]
    public void Profile_live_samples_display_current_first_declarations_and_frame_counters(
        bool showStacks,
        bool includeValues)
    {
        var directory =
            Path.Combine(Environment.CurrentDirectory, "artifacts", "profile-live-" + Guid.NewGuid().ToString("N"));

        var path = Path.Combine(directory, "profile.json");
        var (console, output) = CreateConsole(AnsiSupport.No);
        console.Profile.Width = 240;

        var sample =
            new ElmTestProfileSample(
                new("running", null, "execution", "one test", 12, default),
                [
                new(
                    new string('a', 64),
                    7,
                    includeValues ? [new[] { 0, 2 }] : null,
                    includeValues ? [new(null, "input preview")] : null,
                    includeValues ? [new(null, "local preview")] : null)
                {
                    Declarations = ["Tests.spin"],
                    FrameIndex = 3,
                    InstructionCount = 12_345,
                    LoopIterationCount = 2_345,
                    CompiledFrameId = "body-current",
                },
                new(new string('b', 64), 1, null, null, null)
                {
                    Declarations = ["Tests.caller"],
                    FrameIndex = 2,
                    CompiledFrameId = "body-caller",
                },
                ]);

        void AssertStack(string text)
        {
            if (!showStacks)
            {
                text.Should().NotContain("stack trace").And.NotContain("Tests.spin")
                    .And.NotContain("body-current").And.NotContain("input preview").And.NotContain("local preview");

                return;
            }

            text.Should().Contain("current frame first").And.Contain("Tests.spin").And.Contain("frame: 3")
                .And.Contain("body: body-current").And.Contain("instruction pointer: 7")
                .And.Contain("instructions: 12_345").And.Contain("loops: 2_345").And.Contain("Tests.caller");

            text.IndexOf("Tests.spin", StringComparison.Ordinal)
                .Should().BeLessThan(text.IndexOf("Tests.caller", StringComparison.Ordinal));

            if (includeValues)
                text.Should().Contain("input [0,2]: input preview").And.Contain("local 0: local preview");

            else
                text.Should().NotContain("input preview").And.NotContain("local preview");
        }

        var settings =
            new TestProfileSettings
            {
                OutputPath = path,
                ShowStackTraces = showStacks,
                Instrumentation = new() { IncludeInputs = includeValues, IncludeLocals = includeValues },
            };

        try
        {
            TestProfileCommand.Execute(
                ".",
                null,
                settings,
                instrumentation =>
                {
                    instrumentation.OnSample.Should().NotBeNull();
                    instrumentation.OnSample!(sample);

                    output.ToString().Should().Contain("Live instrumentation sample.").And.NotContain(
                        "Saved JSON report:");

                    AssertStack(output.ToString());
                    File.Exists(path).Should().BeFalse();
                    return 0;
                },
                console,
                FormatCommandColorMode.Never).Should().Be(0);

            var finalLines = new List<string>();

            TestProfileCommand.Render(
                new(
                    2,
                    settings.Instrumentation,
                    null,
                    sample.Summary,
                    [sample],
                    [],
                    new Dictionary<string, ElmTestProfileValueNode>()),
                settings,
                (line, _) => finalLines.Add(line));

            AssertStack(string.Join("\n", finalLines));
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void Profile_reports_hot_loop_sites_and_explains_dropped_samples()
    {
        var report =
            new ElmTestProfileReport(
                2,
                new() { MaxSamples = 2 },
                null,
                new("stopped", "budget", "execution", "one test", 0, default),
                [],
                [],
                new Dictionary<string, ElmTestProfileValueNode>())
            {
                DroppedSamples = 1_234,
                LoopSites =
                [
                new(new string('a', 64), "body-cold", 3, 5),
                new(new string('b', 64), "body-hot", 7, 9_876),
                ],
            };

        var lines = new List<string>();
        TestProfileCommand.Render(report, new() { Top = 1 }, (line, _) => lines.Add(line));
        var text = string.Join("\n", lines);

        text.Should().Contain("Hot loop sites (by iterations):").And.Contain("body: body-hot")
            .And.Contain("backward-jump destination: 7").And.Contain("iterations: 9_876").And.NotContain("body-cold")
            .And.Contain("Dropped samples: 1_234").And.Contain("most recent 2")
            .And.Contain("--max-samples").And.Contain("aggregate counters still cover the entire run");
    }

    [Fact]
    public void Profile_direct_budget_stop_saves_a_bounded_partial_report()
    {
        var directory =
            Path.Combine(Environment.CurrentDirectory, "artifacts", "profile-partial-" + Guid.NewGuid().ToString("N"));

        var path = Path.Combine(directory, "profile.json");
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestProfileCommand.Execute(
                ".",
                null,
                new()
                {
                    OutputPath = path,
                    Instrumentation =
                    new()
                    {
                        InvocationBudget = 10,
                        MaxSamples = 2,
                        SnapshotInterval = TimeSpan.FromTicks(1),
                        DisablePrecompiledLeaves = true,
                    },
                },
                instrumentation =>
                {
                    using var scope = instrumentation.EnterScope("execution", "recursive eval");

                    var recursive =
                        Expression.ListInst(
                            [
                            new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance),
                            Expression.EmptyList,
                            ]);

                    instrumentation.CreateVm(
                        new ConcurrentInvocationCache(),
                        new PineVMSharedCaches())
                        .EvaluateExpression(recursive, ExpressionEncoding.EncodeExpressionAsValue(recursive));

                    return 0;
                },
                console,
                FormatCommandColorMode.Never).Should().Be(2);

            using var json = JsonDocument.Parse(File.ReadAllText(path));
            json.RootElement.GetProperty("SchemaVersion").GetInt32().Should().Be(2);
            json.RootElement.GetProperty("Options").GetProperty("MaxSamples").GetInt32().Should().Be(2);
            json.RootElement.GetProperty("Samples").GetArrayLength().Should().BeInRange(1, 2);
            json.RootElement.GetProperty("DroppedSamples").GetInt64().Should().BeGreaterThan(0);
            json.RootElement.GetProperty("Expressions").GetArrayLength().Should().BeGreaterThan(0);
            json.RootElement.GetProperty("Summary").GetProperty("Outcome").GetString().Should().Be("stopped");

            output.ToString().Should().Contain("Live instrumentation sample.").And.Contain("Execution stopped.")
                .And.Contain("Dropped samples:").And.Contain("Saved JSON report:");
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Theory]
    [InlineData(false, false, 2, "stopped")]
    [InlineData(false, true, 2, "stopped")]
    [InlineData(true, false, 130, "cancelled")]
    [InlineData(true, true, 130, "cancelled")]
    public void Profile_direct_stop_exceptions_save_partial_reports_with_the_correct_exit_code(
        bool userCancellation,
        bool operationCanceled,
        int expectedExitCode,
        string expectedOutcome)
    {
        var directory =
            Path.Combine(Environment.CurrentDirectory, "artifacts", "profile-stopped-" + Guid.NewGuid().ToString("N"));

        var path = Path.Combine(directory, "profile.json");
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestProfileCommand.Execute(
                ".",
                null,
                new() { OutputPath = path },
                instrumentation =>
                {
                    if (userCancellation)
                        instrumentation.Cancel();

                    if (operationCanceled)
                        throw new OperationCanceledException();

                    throw new ElmTestInstrumentationStoppedException("Direct execution stop.");
                },
                console,
                FormatCommandColorMode.Never).Should().Be(expectedExitCode);

            using var json = JsonDocument.Parse(File.ReadAllText(path));
            json.RootElement.GetProperty("Summary").GetProperty("Outcome").GetString().Should().Be(expectedOutcome);
            output.ToString().Should().Contain("Saved JSON report:").And.Contain("JSON SHA256:");
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void Profile_run_exceptions_save_failed_reports_with_exception_details()
    {
        var directory =
            Path.Combine(Environment.CurrentDirectory, "artifacts", "profile-failed-" + Guid.NewGuid().ToString("N"));

        var path = Path.Combine(directory, "profile.json");
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestProfileCommand.Execute(
                ".",
                null,
                new() { OutputPath = path },
                _ => throw new InvalidOperationException("Unexpected bug."),
                console,
                FormatCommandColorMode.Never).Should().Be(1);

            using var json = JsonDocument.Parse(File.ReadAllText(path));
            json.RootElement.GetProperty("Summary").GetProperty("Outcome").GetString().Should().Be("failed");

            json.RootElement.GetProperty("Metadata").GetProperty("ExceptionDetails").GetString().Should()
                .Contain("System.InvalidOperationException: Unexpected bug.");

            output.ToString().Should().Contain("Execution failed.").And.Contain("System.InvalidOperationException")
                .And.Contain("Unexpected bug.").And.Contain("Saved JSON report:");
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Profile_announces_report_path_stop_tips_and_preparation_before_execution(bool invocationOnly)
    {
        var directory =
            Path.Combine(Environment.CurrentDirectory, "artifacts", "profile-tips-" + Guid.NewGuid().ToString("N"));

        var path = Path.Combine(directory, "profile.json");
        var (console, output) = CreateConsole(AnsiSupport.No);
        console.Profile.Width = 240;

        try
        {
            TestProfileCommand.Execute(
                ".",
                "one",
                new() { OutputPath = path, Instrumentation = new() { InvocationBudget = invocationOnly ? 20 : null } },
                _ =>
                {
                    var before = output.ToString();

                    before.Should().Contain("Profile report path: " + path).And.Contain("Ctrl+C")
                        .And.Contain("--budget").And.Contain("--loop-budget").And.Contain("--timeout")
                        .And.Contain("prepares all test declarations before filtering")
                        .And.Contain("no --filter guarantees bypassing preparation")
                        .And.Contain("Warning:").And.NotContain("Saved JSON report:");

                    if (invocationOnly)
                        before.Should().Contain("invocation budget alone does not bound backward-jump loops");

                    else
                        before.Should().Contain("limits are all unbounded").And.Contain("will not catch compiled loops");

                    return 0;
                },
                console,
                FormatCommandColorMode.Never).Should().Be(0);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void Elm_test_help_places_execution_limits_after_dependency_report_and_before_help()
    {
        var output = new StringWriter();
        TestCommand.Create().Parse(["--help"]).Invoke(new InvocationConfiguration { Output = output }).Should().Be(0);
        var text = output.ToString();
        var dependency = text.IndexOf("--dependency-report <", StringComparison.Ordinal);
        var budget = text.IndexOf("--budget <", StringComparison.Ordinal);
        var help = text.LastIndexOf("--help", StringComparison.Ordinal);
        dependency.Should().BeGreaterThan(0);
        budget.Should().BeGreaterThan(dependency);
        var previous = dependency;

        foreach (var option in new[] { "--budget <", "--invocation-budget <", "--loop-budget <", "--timeout <", "--max-stack-depth <" })
        {
            var index = text.IndexOf(option, StringComparison.Ordinal);
            index.Should().BeGreaterThan(previous).And.BeLessThan(help);
            previous = index;
        }
    }

    [Fact]
    public void Profile_selects_a_single_project_test_without_a_filter()
    {
        var project = CreateTestProject(PassingTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestProfileCommand.Execute(
                project,
                null,
                new() { OutputPath = Path.Combine(project, "profile.json") },
                profile => TestCommand.Execute(
                    project,
                    console: console,
                    errorConsole: console,
                    offline: true,
                    instrumentation: profile),
                console).Should().Be(0, output.ToString());

            output.ToString().Should().Contain("Running 1 test").And.Contain("Effective execution limits:");
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Fact]
    public void Profile_reports_zero_counts_when_the_project_has_no_test_modules()
    {
        var project = CreateTestProject(PassingTestsModule);
        File.Delete(Path.Combine(project, "tests", "Tests.elm"));
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestProfileCommand.Execute(
                project,
                null,
                new() { OutputPath = Path.Combine(project, "profile.json") },
                profile => TestCommand.Execute(
                    project,
                    console: console,
                    errorConsole: console,
                    offline: true,
                    instrumentation: profile),
                console).Should().Be(1);

            output.ToString().Should().Contain("Tests found: 0").And.Contain("Tests remaining after filter: 0")
                .And.Contain("Add a runnable Elm test");
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Theory]
    [InlineData(null, 3, "first")]
    [InlineData("chosen", 2, "first")]
    [InlineData("missing", 0, "first")]
    public void Profile_selection_rejection_explains_counts_and_offers_an_exact_followup(
        string? filter, int remaining, string selectedName)
    {
        var project =
            CreateTestProject(
                """
                module Tests exposing (suite)
                import Test
                import Expect
                suite =
                    Test.describe "root"
                        [ Test.describe "chosen" [ Test.test "first" (\_ -> Expect.pass), Test.test "second" (\_ -> Expect.pass) ]
                        , Test.test "other" (\_ -> Expect.pass)
                        ]
                """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestProfileCommand.Execute(
                project,
                filter,
                new() { OutputPath = Path.Combine(project, "profile.json") },
                profile => TestCommand.Execute(
                    project,
                    console: console,
                    errorConsole: console,
                    offline: true,
                    filter: filter,
                    instrumentation: profile),
                console).Should().Be(1);

            var text = output.ToString();

            text.Should().Contain("Tests found: 3").And.Contain("Tests remaining after filter: " + remaining)
                .And.Contain("--filter '=tests/Tests.elm/root/chosen/" + selectedName + "'");

            if (filter == "chosen")
                text.Should().NotContain("--filter '=tests/Tests.elm/root/other'");

            if (remaining == 0)
                text.Should().Contain("No tests remain");

            var (nextConsole, nextOutput) = CreateConsole(AnsiSupport.No);

            TestProfileCommand.Execute(
                project,
                "=tests/Tests.elm/root/chosen/first",
                new() { OutputPath = Path.Combine(project, "next.json") },
                profile => TestCommand.Execute(
                    project,
                    console: nextConsole,
                    errorConsole: nextConsole,
                    offline: true,
                    filter: "=tests/Tests.elm/root/chosen/first",
                    instrumentation: profile),
                nextConsole)
                .Should().Be(0, nextOutput.ToString());
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Fact]
    public void Duplicate_test_paths_offer_and_accept_a_discovery_ordinal()
    {
        var project =
            CreateTestProject(
                """
                module Tests exposing (first, second)
                import Test
                import Expect
                first = Test.test "same" (\_ -> Expect.pass)
                second = Test.test "same" (\_ -> Expect.fail "not selected")
                """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestProfileCommand.Execute(
                project,
                null,
                new() { OutputPath = Path.Combine(project, "profile.json") },
                profile => TestCommand.Execute(
                    project,
                    console: console,
                    errorConsole: console,
                    offline: true,
                    instrumentation: profile),
                console).Should().Be(1);

            output.ToString().Should().Contain("--filter '#1'").And.Contain("--filter '#2'");
            var (selectedConsole, selectedOutput) = CreateConsole(AnsiSupport.No);

            TestProfileCommand.Execute(
                project,
                "#1",
                new() { OutputPath = Path.Combine(project, "selected.json") },
                profile =>
                TestCommand.Execute(
                    project,
                    console: selectedConsole,
                    errorConsole: selectedConsole,
                    offline: true,
                    filter: "#1",
                    instrumentation: profile),
                selectedConsole).Should().Be(0, selectedOutput.ToString());
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Theory]
    [InlineData(FormatCommandColorMode.Always, true)]
    [InlineData(FormatCommandColorMode.Never, false)]
    public void Profile_report_colors_respect_the_explicit_mode(FormatCommandColorMode mode, bool expectColor)
    {
        var directory = Path.Combine(Path.GetTempPath(), "pine-profile-colors-" + Guid.NewGuid().ToString("N"));
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        try
        {
            TestProfileCommand.Execute(
                directory,
                null,
                new() { OutputPath = Path.Combine(directory, "profile.json") },
                _ => 0,
                console,
                colorMode: mode).Should().Be(0);

            output.ToString().Contains("\u001b[", StringComparison.Ordinal).Should().Be(expectColor);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void Execution_budget_failure_identifies_the_test_and_provides_a_profile_filter()
    {
        var project =
            CreateTestProject(
                """
                module Tests exposing (suite)
                import Test
                import Expect
                spin n = spin (n + 1)
                suite = Test.test "looping test" (\_ -> spin 0)
                """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestCommand.Execute(
                project,
                console: console,
                errorConsole: console,
                offline: true,
                evaluationOptions: new() { LoopBudget = 10000 }).Should().Be(2);

            var text = output.ToString();

            text.Should().Contain("Elm test: tests/Tests.elm/looping test")
                .And.Contain("pine elm test profile").And.Contain("--filter '=tests/Tests.elm/looping test'");

            text.IndexOf("Invocations:", StringComparison.Ordinal).Should()
                .BeLessThan(text.IndexOf("; loops:", StringComparison.Ordinal));

            text.IndexOf("; loops:", StringComparison.Ordinal).Should()
                .BeLessThan(text.IndexOf("; instructions:", StringComparison.Ordinal));
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Numeric_help_defaults_use_integer_display_format_without_changing_parsed_values(bool profile)
    {
        var command = TestCommand.Create();
        var output = new StringWriter();
        var errors = new StringWriter();
        var arguments = profile ? new[] { "profile", "--help" } : new[] { "--help" };
        command.Parse(arguments).Invoke(new InvocationConfiguration { Output = output, Error = errors }).Should().Be(0);
        output.ToString().Should().Contain("[default: 100_000]").And.NotContain("[default: 100000]");
        errors.ToString().Should().BeEmpty();

        var parsed =
            command.Parse(
                profile
                ?
                ["profile", ".", "--filter", "one"]
                :
                ["."]);

        parsed.GetValue(command.Options.OfType<Option<int>>().Single(option => option.Name == "--max-stack-depth"))
            .Should().Be(100_000);
    }

    [Fact]
    public void Effective_limit_display_groups_large_numbers_and_preserves_fractional_timeouts()
    {
        var (console, output) = CreateConsole(AnsiSupport.No);

        TestCommand.Execute(
            "does-not-exist",
            console: console,
            errorConsole: console,
            evaluationOptions: new()
            {
                InvocationBudget = 1_234_567,
                LoopBudget = 2_345_678,
                StackDepthLimit = 100_000,
                Timeout = TimeSpan.FromSeconds(1234.125),
            }).Should().Be(1);

        output.ToString().Should().Contain("Invocation budget: 1_234_567")
            .And.Contain("Loop budget: 2_345_678").And.Contain("Stack-depth limit: 100_000")
            .And.Contain("Timeout: 1_234.125 seconds");
    }

    [Fact]
    public void Profile_command_is_directly_discoverable_and_accepts_optional_filters()
    {
        var command = TestCommand.Create();
        command.Description.Should().Contain("elm test profile").And.Contain("--budget");
        command.Subcommands.Single(child => child.Name == "profile").Subcommands.Should().BeEmpty();
        command.Parse(["profile", "."]).Errors.Should().BeEmpty();

        command.Parse(
            [
            "profile", ".", "--filter", "one", "--loop-budget", "10",
            "--seed", "0", "--fuzz", "1", "--offline", "--sort", "Loops", "--include-locals"
            ])
            .Errors.Should().BeEmpty();
    }

    [Theory]
    [InlineData("--instruction-budget")]
    public void Instrument_does_not_accept_removed_instruction_budget_options(string option) =>
        TestCommand.Create().Parse(["profile", ".", "--filter", "one", option, "10"])
        .Errors.Should().NotBeEmpty();

    [Theory]
    [InlineData("--timeout", "NaN")]
    [InlineData("--timeout", "Infinity")]
    [InlineData("--timeout", "-1")]
    [InlineData("--interval", "NaN")]
    [InlineData("--interval", "-1")]
    public void Instrument_rejects_invalid_clock_options_before_execution(string option, string value) =>
        TestCommand.Create().Parse(["profile", ".", "--filter", "one", option, value])
        .Errors.Should().NotBeEmpty();

    [Fact]
    public void Instrument_stops_preparation_prints_immediate_stats_then_saves_json_and_its_sha256()
    {
        var project =
            CreateTestProject(
                """
                module Tests exposing (suite)
                import Test
                import Expect
                suite = Test.test "one" (\_ -> Expect.pass)
                """);

        var (console, output) = CreateConsole(AnsiSupport.No);
        var path = Path.Combine(project, "profile.json");

        try
        {
            var result =
                TestProfileCommand.Execute(
                    project,
                    "one",
                    new()
                    {
                        OutputPath = path,
                        Top = 1,
                        ShowExpressions = true,
                        Instrumentation = new() { InvocationBudget = 1, IncludeInputs = true, IncludeLocals = true },
                    },
                    instrumentation => TestCommand.Execute(
                        project,
                        console: console,
                        errorConsole: console,
                        filter: "one",
                        offline: true,
                        seed: 0,
                        instrumentation: instrumentation),
                    console);

            result.Should().Be(2);
            var text = output.ToString();

            text.Should().Contain("Execution stopped.").And.Contain("invocations: 2")
                .And.Contain("Saving all recorded").And.Contain("Overall instrumentation stats.")
                .And.Contain("Pine expression ranking").And.Contain("Last recorded stack trace");

            text.IndexOf("Execution stopped.", StringComparison.Ordinal)
                .Should().BeLessThan(text.IndexOf("Saving all recorded", StringComparison.Ordinal));

            text.IndexOf("Saving all recorded", StringComparison.Ordinal)
                .Should().BeLessThan(text.IndexOf("JSON SHA256:", StringComparison.Ordinal));

            using var file = File.OpenRead(path);
            text.Should().Contain(Convert.ToHexStringLower(SHA256.HashData(file)));
            using var json = JsonDocument.Parse(File.ReadAllText(path));
            json.RootElement.GetProperty("SchemaVersion").GetInt32().Should().Be(2);
            json.RootElement.GetProperty("Summary").GetProperty("Phase").GetString().Should().Be("preparation");
            json.RootElement.GetProperty("Expressions").GetArrayLength().Should().BeGreaterThan(0);
            json.RootElement.GetProperty("Values").EnumerateObject().Should().NotBeEmpty();
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Fact]
    public void Instrument_requires_exactly_one_runnable_test_before_execution()
    {
        var project =
            CreateTestProject(
                """
                module Tests exposing (suite)
                import Test
                import Expect
                suite = Test.describe "group" [ Test.test "first" (\_ -> Expect.pass), Test.test "second" (\_ -> Expect.pass) ]
                """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var result =
                TestProfileCommand.Execute(
                    project,
                    "group",
                    new()
                    {
                        OutputPath = Path.Combine(project, "profile.json"),
                    },
                    profile => TestCommand.Execute(
                        project,
                        console: console,
                        errorConsole: console,
                        filter: "group",
                        offline: true,
                        instrumentation: profile),
                    console);

            result.Should().Be(1);

            output.ToString().Should().Contain("exactly one runnable Elm test")
                .And.Contain("Tests found: 2").And.Contain("Tests remaining after filter: 2")
                .And.Contain("pine elm test profile").And.Contain("--filter '=tests/Tests.elm/group/first'")
                .And.NotContain("Running 2 tests");
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Fact]
    public void Instrument_executes_one_test_and_saves_all_expressions_despite_top_limit()
    {
        var project =
            CreateTestProject(
                """
                module Tests exposing (suite)
                import Test
                import Expect
                suite = Test.describe "group" [ Test.test "first" (\_ -> Expect.equal 42 42), Test.test "second" (\_ -> Expect.fail "not selected") ]
                """);

        var (console, output) = CreateConsole(AnsiSupport.No);
        var path = Path.Combine(project, "profile.json");

        try
        {
            var result =
                TestProfileCommand.Execute(
                    project,
                    "first",
                    new()
                    {
                        OutputPath = path,
                        Top = 1,
                        Sort = TestProfileSort.Instructions,
                    },
                    profile => TestCommand.Execute(
                        project,
                        console: console,
                        errorConsole: console,
                        filter: "first",
                        offline: true,
                        seed: 0,
                        instrumentation: profile),
                    console);

            result.Should().Be(0, output.ToString());
            output.ToString().Should().Contain("Running 1 test").And.Contain("TEST RUN PASSED");
            using var json = JsonDocument.Parse(File.ReadAllText(path));
            json.RootElement.GetProperty("SelectedTest").GetString().Should().EndWith("/first");
            json.RootElement.GetProperty("Expressions").GetArrayLength().Should().BeGreaterThan(1);

            json.RootElement
                .GetProperty("Summary").GetProperty("Counters").GetProperty("InstructionCount").GetInt64().Should()
                .BeGreaterThan(0);

            json.RootElement.GetProperty("Summary").GetProperty("Counters").EnumerateObject()
                .Take(3).Select(property => property.Name)
                .Should().Equal("InvocationCount", "LoopIterationCount", "InstructionCount");

            json.RootElement.GetProperty("Expressions")[0].EnumerateObject()
                .Where(property => property.Name is "Invocations" or "Instructions" or "LoopIterations")
                .Select(property => property.Name)
                .Should().Equal("Invocations", "LoopIterations", "Instructions");

            var text = output.ToString();
            text.Should().Contain("Invocations  Loops  Instructions");

            text.IndexOf("invocations:", StringComparison.Ordinal).Should().BeLessThan(
                text.IndexOf("loops:", StringComparison.Ordinal));

            text.IndexOf("loops:", StringComparison.Ordinal).Should().BeLessThan(
                text.IndexOf("instructions:", StringComparison.Ordinal));

            var topExpression =
                json.RootElement.GetProperty("Expressions").EnumerateArray()
                .OrderByDescending(row => row.GetProperty("Instructions").GetInt64())
                .ThenBy(row => row.GetProperty("Hash").GetString(), StringComparer.Ordinal).First();

            output.ToString().Should().Contain(
                $"{topExpression.GetProperty("Hash").GetString()![..16]}  " +
                $"{CommandLineInterface.FormatIntegerForDisplay(topExpression.GetProperty("Invocations").GetInt64()),11}  " +
                $"{CommandLineInterface.FormatIntegerForDisplay(topExpression.GetProperty("LoopIterations").GetInt64()),5}  " +
                $"{CommandLineInterface.FormatIntegerForDisplay(topExpression.GetProperty("Instructions").GetInt64()),12}");
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Shared_budget_shortcut_and_specific_overrides_are_effective_in_both_commands(bool profile)
    {
        var command = TestCommand.Create();

        var arguments =
            profile
            ?
            new[] { "profile", ".", "--filter", "one", "--budget", "20", "--loop-budget", "30", "--timeout", "2", "--max-stack-depth", "200" }
            :
            new[] { ".", "--budget", "20", "--loop-budget", "30", "--timeout", "2", "--max-stack-depth", "200" };

        var parsed = command.Parse(arguments);
        parsed.Errors.Should().BeEmpty();
        var common = command.Options.OfType<Option<int?>>().Single(option => option.Name == "--budget");
        var invocations = command.Options.OfType<Option<int?>>().Single(option => option.Name == "--invocation-budget");
        var loops = command.Options.OfType<Option<int?>>().Single(option => option.Name == "--loop-budget");
        (parsed.GetValue(invocations) ?? parsed.GetValue(common)).Should().Be(20);
        (parsed.GetValue(loops) ?? parsed.GetValue(common)).Should().Be(30);

        command.Parse(
            profile
            ?
            ["--budget", "20", "profile", ".", "--filter", "one"]
            :
            ["--budget", "20", "."]).Errors.Should().BeEmpty();
    }

    [Theory]
    [InlineData("--budget", "0")]
    [InlineData("--budget", "-1")]
    [InlineData("--invocation-budget", "0")]
    [InlineData("--loop-budget", "-1")]
    [InlineData("--max-stack-depth", "0")]
    [InlineData("--timeout", "Infinity")]
    public void Both_commands_reject_invalid_shared_budget_options(string option, string value)
    {
        var command = TestCommand.Create();
        command.Parse([".", option, value]).Errors.Should().NotBeEmpty();
        command.Parse(["profile", ".", "--filter", "one", option, value]).Errors.Should().NotBeEmpty();
    }

    [Fact]
    public void Ordinary_tests_hide_default_limits_but_show_configured_limits_and_invocation_only_warning()
    {
        var (console, output) = CreateConsole(AnsiSupport.No);

        TestCommand.Execute("does-not-exist", console: console, errorConsole: console)
            .Should().Be(1);

        output.ToString().Should().NotContain("Effective execution limits:");

        var (boundedConsole, boundedOutput) = CreateConsole(AnsiSupport.No);

        TestCommand.Execute(
            "does-not-exist",
            console: boundedConsole,
            errorConsole: boundedConsole,
            evaluationOptions: new() { InvocationBudget = 20 })
            .Should().Be(1);

        boundedOutput.ToString().Should().Contain("Invocation budget: 20").And.Contain("Loop budget: unbounded")
            .And.Contain("Warning: an invocation budget alone does not bound backward-jump loops");
    }

    [Fact]
    public void Profile_prints_effective_limits_before_execution_without_warning_when_both_are_bounded()
    {
        var directory = Path.Combine(Path.GetTempPath(), "pine-profile-limits-" + Guid.NewGuid().ToString("N"));
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestProfileCommand.Execute(
                directory,
                "one",
                new()
                {
                    OutputPath = Path.Combine(directory, "profile.json"),
                    Instrumentation =
                    new() { InvocationBudget = 20, LoopBudget = 30, Timeout = TimeSpan.FromSeconds(2), StackDepthLimit = 200 },
                },
                _ =>
                {
                    var before = output.ToString();

                    before.Should().Contain("Invocation budget: 20").And.Contain("Loop budget: 30")
                        .And.Contain("Timeout: 2 seconds").And.Contain("Stack-depth limit: 200")
                        .And.NotContain("Warning:");

                    return 0;
                },
                console).Should().Be(0);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void Ordinary_command_budget_stops_preparation_without_requiring_a_single_test_filter()
    {
        var project =
            CreateTestProject(
                """
                module Tests exposing (suite)
                import Test
                import Expect
                suite = Test.describe "group" [ Test.test "first" (\_ -> Expect.pass), Test.test "second" (\_ -> Expect.pass) ]
                """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestCommand.Execute(
                project,
                console: console,
                errorConsole: console,
                offline: true,
                evaluationOptions: new() { InvocationBudget = 1 })
                .Should().Be(2);

            output.ToString().Should().Contain("Execution stopped.").And.Contain("InvocationCount")
                .And.Contain("Elm test preparation: tests/Tests.elm (Tests.suite)")
                .And.Contain("The individual test has not been constructed yet")
                .And.Contain("pine elm test profile").And.Contain("--filter 'tests/Tests.elm/**'")
                .And.NotContain("exactly one runnable").And.NotContain("Saving all recorded");
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Fact]
    public void Ordinary_budgeted_execution_preserves_multiple_tests_and_workers_without_recording_profiles()
    {
        var project =
            CreateTestProject(
                """
                module Tests exposing (suite)
                import Test
                import Expect
                suite = Test.describe "group" [ Test.test "first" (\_ -> Expect.pass), Test.test "second" (\_ -> Expect.pass) ]
                """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestCommand.Execute(
                project,
                console: console,
                errorConsole: console,
                offline: true,
                workers: 2,
                evaluationOptions: new() { InvocationBudget = 100000, LoopBudget = 100000 })
                .Should().Be(0, output.ToString());

            output.ToString().Should().Contain("Running 2 tests").And.Contain("TEST RUN PASSED")
                .And.Contain("Invocation budget: 100_000").And.Contain("Loop budget: 100_000")
                .And.NotContain("Saving all recorded");
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Fact]
    public void Compilation_errors_are_rendered_with_declaration_context_without_unhandled_exceptions()
    {
        var project =
            CreateTestProject(
                "module Tests exposing (suite)\nsuite = helper\nhelper = missing");

        var (errorConsole, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    project,
                    FormatCommandColorMode.Never,
                    errorConsole: errorConsole,
                    offline: true);

            exitCode.Should().Be(1);

            output.ToString().Should().Contain("Error:").And.Contain("Failed compiling Elm tests:")
                .And.Contain("Tests.suite (compilation root)")
                .And.Contain("Tests.helper — referenced by Tests.suite")
                .And.Contain("No local binding, value declaration, or exposed import provides 'missing'")
                .And.NotContain("Unhandled exception")
                .And.NotContain("System.InvalidOperationException")
                .And.NotContain(" at Pine.");
        }
        finally
        {
            Directory.Delete(project, recursive: true);
        }
    }

    [Theory]
    [InlineData("--seed", "-1")]
    [InlineData("--seed", "4294967296")]
    [InlineData("--seed", "not-a-number")]
    [InlineData("--fuzz", "0")]
    [InlineData("--fuzz", "-1")]
    [InlineData("--fuzz", "4294967296")]
    [InlineData("--fuzz", "not-a-number")]
    public void Fuzz_options_reject_values_outside_elm_test_rs_ranges(string option, string value) =>
        TestCommand.Create().Parse([option, value]).Errors.Should().NotBeEmpty();

    [Fact]
    public void Fuzz_options_match_elm_test_rs_defaults_and_unsigned_boundaries()
    {
        var command = TestCommand.Create();
        var fuzz = command.Options.OfType<Option<uint>>().Single(option => option.Name is "--fuzz");
        var seed = command.Options.OfType<Option<uint?>>().Single(option => option.Name is "--seed");
        var defaults = command.Parse([]);
        defaults.Errors.Should().BeEmpty();
        defaults.GetValue(fuzz).Should().Be(100);
        defaults.GetValue(seed).Should().BeNull();
        var boundaries = command.Parse(["--seed", "4294967295", "--fuzz", "4294967295"]);
        boundaries.Errors.Should().BeEmpty();
        boundaries.GetValue(fuzz).Should().Be(uint.MaxValue);
        boundaries.GetValue(seed).Should().Be(uint.MaxValue);
    }

    [Theory]
    [InlineData(80, FormatCommandColorMode.Never)]
    [InlineData(120, FormatCommandColorMode.Never)]
    [InlineData(200, FormatCommandColorMode.Never)]
    [InlineData(80, FormatCommandColorMode.Always)]
    [InlineData(120, FormatCommandColorMode.Always)]
    [InlineData(200, FormatCommandColorMode.Always)]
    public void Fuzz_failure_displays_counterexample_and_reproduction_flags(
        int consoleWidth, FormatCommandColorMode colorMode)
    {
        var directory =
            CreateTestProject(
                """
                module Tests exposing (suite)
                import Test
                import Fuzz
                import Expect
                suite = Test.fuzz (Fuzz.intRange 1 100) "failing property" (\n -> Expect.equal 0 n)
                """);

        var (console, output) =
            CreateConsole(colorMode is FormatCommandColorMode.Always ? AnsiSupport.Yes : AnsiSupport.No);

        console.Profile.Width = consoleWidth;

        try
        {
            var result =
                TestCommand.Execute(
                    directory,
                    colorMode,
                    console: console,
                    seed: uint.MaxValue,
                    fuzz: 1,
                    offline: true);

            result.Should().Be(1);
            output.ToString().Should().Contain("Given: 1").And.Contain("--seed 4294967295 --fuzz 1");

            output.ToString().Should().Contain(
                $"To reproduce these results, run pine elm test \"{Path.GetFullPath(directory)}\" --seed 4294967295 --fuzz 1");

            output.ToString().Should().Contain("failing property").And.Contain("TEST RUN FAILED");
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void Default_fuzz_count_executes_string_properties_without_a_cli_work_quota()
    {
        var directory =
            CreateTestProject(
                """
                module Tests exposing (suite)
                import Test
                import Fuzz
                import Expect
                suite = Test.fuzz Fuzz.string "split join" (\s -> Expect.equal s (String.join "." (String.split "." s)))
                """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            TestCommand.Execute(
                directory,
                FormatCommandColorMode.Never,
                console: console,
                seed: 0,
                offline: true)
                .Should().Be(0);

            output.ToString().Should().Contain("TEST RUN PASSED").And.Contain("--seed 0 --fuzz 100");
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public void Direct_execution_rejects_zero_fuzz_count_before_loading_sources()
    {
        var (errorConsole, output) = CreateConsole(AnsiSupport.No);
        TestCommand.Execute("does-not-exist", errorConsole: errorConsole, fuzz: 0).Should().Be(1);
        output.ToString().Should().Contain("--fuzz").And.Contain("positive");
    }

    [Fact]
    public void Declared_parent_source_directories_are_loaded_without_merging_parent_projects()
    {
        var root =
            CreateTestProject(
                PassingTestsModule
                .Replace("import Expect", "import Expect\nimport Shared")
                .Replace("71 |> Expect.equal 71", "Shared.value |> Expect.equal 71"));

        var projectDirectory = Path.Combine(root, "nested");
        var (console, output) = CreateConsole(AnsiSupport.No);
        var (errorConsole, errorOutput) = CreateConsole(AnsiSupport.No);

        try
        {
            Directory.CreateDirectory(projectDirectory);
            Directory.CreateDirectory(Path.Combine(root, "src"));
            File.WriteAllText(Path.Combine(root, "src", "Shared.elm"), "module Shared exposing (value)\nvalue = 71\n");
            File.Move(Path.Combine(root, "elm.json"), Path.Combine(projectDirectory, "elm.json"));
            Directory.Move(Path.Combine(root, "tests"), Path.Combine(projectDirectory, "tests"));
            var manifest = Path.Combine(projectDirectory, "elm.json");
            File.WriteAllText(manifest, File.ReadAllText(manifest).Replace("[\"src\"]", "[\"../src\"]"));
            File.WriteAllText(Path.Combine(root, "elm.json"), "{\"this is not the selected project\": true}");

            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    errorConsole: errorConsole);

            exitCode.Should().Be(0, errorOutput.ToString());
            output.ToString().Should().Contain("TEST RUN PASSED");
        }
        finally
        {
            Directory.Delete(root, recursive: true);
        }
    }

    [Theory]
    [InlineData("2.0.0", "ConstraintConflict", "empty intersection")]
    [InlineData("1.0.4", "UnsupportedSubstitutionVersion", "supports only")]
    public void Dependency_conflicts_are_reported_without_unhandled_exceptions_and_preserve_json_details(
        string version, string failureKind, string message)
    {
        var projectDirectory = CreateTestProject(PassingTestsModule);
        var reportPath = Path.Combine(projectDirectory, "dependencies.json");
        var (errorConsole, errorOutput) = CreateConsole(AnsiSupport.No);

        try
        {
            var manifestPath = Path.Combine(projectDirectory, "elm.json");

            File.WriteAllText(
                manifestPath,
                File.ReadAllText(manifestPath).Replace("\"elm/core\": \"1.0.5\"", $"\"elm/core\": \"{version}\""));

            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    errorConsole: errorConsole,
                    offline: true,
                    dependencyReportPath: reportPath);

            exitCode.Should().Be(1);

            errorOutput.ToString().Should().Contain("elm/core").And.Contain(version).And.Contain(message).And.Contain(
                "elm.json");

            File.ReadAllText(reportPath).Should().Contain(failureKind).And.Contain("Substitutions").And.Contain("Trace");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }

    [Fact]
    public void Success_output_uses_elm_test_rs_colors()
    {
        var projectDirectory = CreateTestProject(PassingTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Always,
                    console: console);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("\u001b[4;32mTEST RUN PASSED");
            output.ToString().Should().Contain("\u001b[2mPassed:   ");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Workers_option_runs_tests_in_parallel()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    workers: 2,
                    reportDurations: true);

            var rendered = output.ToString();
            var runningMessageIndex = rendered.IndexOf("Running 3 tests", StringComparison.Ordinal);

            exitCode.Should().Be(1);
            runningMessageIndex.Should().BeGreaterThanOrEqualTo(0);
            rendered.LastIndexOf("Running 3 tests", StringComparison.Ordinal).Should().Be(runningMessageIndex);
            runningMessageIndex.Should().BeLessThan(rendered.IndexOf("TEST RUN", StringComparison.Ordinal));
            rendered.Should().Contain("Duration:");
            rendered.Should().Contain("Compilation:");
            rendered.Should().Contain("Test execution:");
            rendered.Should().Contain("Passed:   2");
            rendered.Should().Contain("Failed:   1");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Workers_option_rejects_values_smaller_than_one()
    {
        var projectDirectory = CreateTestProject(PassingTestsModule);
        var (errorConsole, errorOutput) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    errorConsole: errorConsole,
                    workers: 0);

            exitCode.Should().Be(1);
            errorOutput.ToString().Should().Contain("The --workers value must be at least 1.");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Failure_output_uses_elm_test_rs_colors()
    {
        var projectDirectory = CreateTestProject(FailingTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Always,
                    console: console);

            var rendered = output.ToString();

            exitCode.Should().Be(1);
            rendered.Should().Contain("\u001b[2m↓ Group Title");
            rendered.Should().Contain("\u001b[91m✗ Another Test Title");
            rendered.Should().Contain("\u001b[7m1\u001b[0m");
            rendered.Should().Contain("\u001b[7m3\u001b[0m");
            rendered.Should().Contain("\u001b[4;91mTEST RUN FAILED");
            rendered.Should().Contain("\u001b[2mPassed:   ");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Color_never_emits_plain_text()
    {
        var projectDirectory = CreateTestProject(FailingTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console);

            exitCode.Should().Be(1);
            output.ToString().Contains('\u001b').Should().BeFalse();
            output.ToString().Should().Contain("TEST RUN FAILED");
            output.ToString().Should().Contain("Passed:   2");
            output.ToString().Should().Contain("Failed:   1");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData("SELECTED GROUP", 0, 2, 2, 0)]
    [InlineData("UNIQUE TEST", 1, 1, 0, 1)]
    public void Filter_is_case_insensitive_and_matches_test_or_group_name(
        string filter,
        int expectedExitCode,
        int expectedTestCount,
        int expectedPassedCount,
        int expectedFailedCount)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: filter);

            exitCode.Should().Be(expectedExitCode);
            output.ToString().Should().Contain($"Running {expectedTestCount} test");
            output.ToString().Should().Contain($"Passed:   {expectedPassedCount}");
            output.ToString().Should().Contain($"Failed:   {expectedFailedCount}");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData(null, 3, "First", "Unique Test")]
    [InlineData("SELECTED GROUP", 2, "First", null)]
    [InlineData("unique test", 1, null, "Unique Test")]
    [InlineData("tests/tests.elm", 3, "First", "Unique Test")]
    [InlineData(@"tests\Tests\Root\Selected Group\First", 1, "First", null)]
    [InlineData("Tests/**/First", 1, "First", null)]
    [InlineData("Tests/*/Selected*/First", 1, "First", null)]
    [InlineData("Root/Selected Group", 2, "First", null)]
    public void List_tests_includes_metadata_and_applies_filter(
        string? filter,
        int expectedTestCount,
        string? expectedFirstName,
        string? expectedUniqueName)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: filter,
                    listTests: true);

            var rendered = output.ToString();

            exitCode.Should().Be(0);

            rendered.Should().Contain(
                filter is null
                ?
                $"Available tests ({expectedTestCount})"
                :
                $"Tests remaining after filtering ({expectedTestCount})");

            rendered.Should().Contain("tests/Tests.elm");
            rendered.Should().Contain("Root");
            rendered.Should().Contain("└──");
            rendered.Contains('\u001b').Should().BeFalse();

            if (expectedFirstName is null)
                rendered.Should().NotContain("First");

            else
                rendered.Should().Contain(expectedFirstName);

            if (expectedUniqueName is null)
                rendered.Should().NotContain("Unique Test");

            else
                rendered.Should().Contain(expectedUniqueName);
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Path_filter_selects_individual_test_for_running_and_listing(bool listTests)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "tests/Tests/Root/Selected Group/*irst",
                    listTests: listTests);

            var rendered = output.ToString();

            exitCode.Should().Be(0);
            rendered.Should().NotContain("Second").And.NotContain("Unique Test");

            if (listTests)
            {
                rendered.Should().Contain("Tests remaining after filtering (1)").And.Contain("First");
                rendered.Should().NotContain("Running").And.NotContain("TEST RUN");
            }
            else
            {
                rendered.Should().Contain("Running 1 test.").And.Contain("Passed:   1");
            }
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void No_matches_prints_closest_paths_and_guidance_without_running(bool listTests)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "tests/Tests/**/Selected Group/Firstt",
                    listTests: listTests);

            var rendered = output.ToString();

            exitCode.Should().Be(1);
            rendered.Should().Contain("No tests matched the filter expression:");
            rendered.Should().Contain("tests/Tests/**/Selected Group/Firstt");
            rendered.Should().Contain("Closest existing test paths (not selected or run):");
            rendered.Should().Contain("tests/Tests.elm/Root/Selected Group/First");
            rendered.Should().Contain("broaden").And.Contain("**").And.Contain("--list-tests").And.Contain("--help");
            rendered.Should().NotContain("Running").And.NotContain("TEST RUN PASSED");
            rendered.Contains('\u001b').Should().BeFalse();

            if (listTests)
            {
                var treeOutput = rendered[..rendered.IndexOf("No tests matched", StringComparison.Ordinal)];
                treeOutput.Should().Contain("Tests remaining after filtering (0)");
                treeOutput.Should().Contain("tests/Tests.elm").And.Contain("Selected Group").And.Contain("Other Group");

                treeOutput.Should().Contain("3 tests filtered out").And.Contain("2 tests filtered out").And.Contain(
                    "1 test filtered out");

                treeOutput.Should().NotContain("First").And.NotContain("Second").And.NotContain("Unique Test");
            }
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void No_match_suggestions_preserve_full_long_paths_in_redirected_output()
    {
        var testName = new string('a', 160);
        var projectDirectory = CreateTestProject(PassingTestsModule.Replace("Test Title", testName));
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "missing test");

            exitCode.Should().Be(1);
            output.ToString().Should().Contain("tests/Tests.elm/Group Title/" + testName);
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Command_accepts_one_filter_expression()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var arguments =
                new[] { projectDirectory, "--color", "never", "--list-tests", "--filter", "Tests/**/First" };

            var command = TestCommand.Create();
            var option = command.Options.OfType<Option<string>>().Single(option => option.Name is "--filter");
            var parseResult = command.Parse(arguments);

            parseResult.Errors.Should().BeEmpty();
            parseResult.GetValue(option).Should().Be("Tests/**/First");

            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: parseResult.GetValue(option),
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("Tests remaining after filtering (1)").And.Contain("First");
            output.ToString().Should().NotContain("Second").And.NotContain("Unique Test");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filter_arity_requires_one_value()
    {
        var command = TestCommand.Create();
        var option = command.Options.OfType<Option<string>>().Single(option => option.Name is "--filter");
        var parseResult = command.Parse(["--filter"]);

        option.Arity.Should().Be(ArgumentArity.ExactlyOne);
        parseResult.Errors.Should().NotBeEmpty();
    }


    [Theory]
    [InlineData("--filter", "First", "--filter", "Second")]
    [InlineData("--filter", "First", "--filter", "First")]
    [InlineData("--filter=First", "--filter=Second")]
    [InlineData("--filter", "First", "--filter")]
    public void Command_rejects_repeated_filter_options(params string[] arguments)
    {
        var parseResult = TestCommand.Create().Parse(arguments);

        parseResult.Errors.Should().NotBeEmpty();
    }


    [Fact]
    public void Command_accepts_no_filter_option()
    {
        var command = TestCommand.Create();
        var option = command.Options.OfType<Option<string>>().Single(option => option.Name is "--filter");
        var parseResult = command.Parse([]);

        parseResult.Errors.Should().BeEmpty();
        parseResult.GetValue(option).Should().BeNull();
    }


    [Fact]
    public void Filter_arity_rejects_multiple_values_per_occurrence()
    {
        TestCommand.Create().Parse(["project", "--filter", "First", "Second"])
            .Errors.Should().NotBeEmpty();
    }


    [Fact]
    public void Help_explains_filter_paths_and_wildcards()
    {
        var output = new StringWriter();

        var root =
            new RootCommand
            {
                TestCommand.Create()
            };

        var exitCode =
            root.Parse(["test", "--help"])
            .Invoke(new InvocationConfiguration { Output = output, Error = output });

        exitCode.Should().Be(0);
        var rendered = output.ToString();
        rendered.Should().Contain("--filter <EXPRESSION>");
        rendered.Should().Contain("Run tests matching the expression.").And.Contain("**").And.Contain("Examples:");
        rendered.Should().Contain("case-insensitive").And.Contain("extension may be omitted");
    }


    [Fact]
    public void Filter_matches_nested_file_paths_relative_to_discovery_root()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var nestedDirectory = Path.Combine(projectDirectory, "tests", "nested");
        Directory.CreateDirectory(nestedDirectory);

        File.WriteAllText(
            Path.Combine(nestedDirectory, "NestedTests.elm"),
            """
            module Nested.NestedTests exposing (suite)
            import Expect
            import Test exposing (Test)
            suite : Test
            suite = Test.test "Nested test" <| \_ -> Expect.pass
            """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: @"tests\nested\NestedTests\Nested test",
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("Tests remaining after filtering (1)");
            output.ToString().Should().Contain("tests/nested/NestedTests.elm").And.Contain("Nested test");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filtered_empty_groups_report_no_individual_tests()
    {
        var projectDirectory = CreateTestProject(EmptyGroupTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "Empty group");

            exitCode.Should().Be(1);
            output.ToString().Should().Contain("No individual tests were discovered.");
            output.ToString().Should().NotContain("Closest existing").And.NotContain("Running");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filter_includes_matches_from_both_file_paths_and_test_descriptions()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var testsDirectory = Path.Combine(projectDirectory, "tests");

        File.WriteAllText(
            Path.Combine(testsDirectory, "SelectedFile.elm"),
            """
            module SelectedFile exposing (suite)

            import Expect
            import Test exposing (Test)

            suite : Test
            suite =
                Test.test "File path match" <|
                    \_ ->
                        Expect.pass
            """,
            new UTF8Encoding(encoderShouldEmitUTF8Identifier: false));

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "selected",
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("Tests remaining after filtering (3)");
            output.ToString().Should().Contain("tests/SelectedFile.elm");
            output.ToString().Should().Contain("Selected Group");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filtered_listing_counts_exclusions_in_every_file_and_group()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);

        File.WriteAllText(
            Path.Combine(projectDirectory, "tests", "ExcludedFile.elm"),
            """
            module ExcludedFile exposing (suite)

            import Expect
            import Test exposing (Test)

            suite : Test
            suite =
                Test.describe "Excluded File Group"
                    [ Test.test "Hidden file test" <| \_ -> Expect.pass ]
            """);

        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: "Tests/**/First",
                    listTests: true);

            exitCode.Should().Be(0);

            output.ToString().Replace("\r\n", "\n").Trim().Should().Be(
                """
                Tests remaining after filtering (1)
                ├── tests/ExcludedFile.elm
                │   ├── Excluded File Group
                │   │   └── 1 test filtered out
                │   └── 1 test filtered out
                └── tests/Tests.elm
                    ├── Root
                    │   ├── Other Group
                    │   │   └── 1 test filtered out
                    │   ├── Selected Group
                    │   │   ├── First
                    │   │   └── 1 test filtered out
                    │   └── 2 tests filtered out
                    └── 2 tests filtered out
                """);
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Theory]
    [InlineData(null, "Available tests (3)")]
    [InlineData("*", "Tests remaining after filtering (3)")]
    public void Listing_does_not_report_exclusions_when_all_tests_are_selected(
        string? filter,
        string expectedHeading)
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    filter: filter,
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain(expectedHeading).And.NotContain("filtered out");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Filtered_listing_styles_exclusion_counts_as_dim_text()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Always,
                    console: console,
                    filter: "Tests/**/First",
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("\u001b[1mTests remaining after filtering (1)");
            output.ToString().Should().Contain("\u001b[2m1 test filtered out");
            output.ToString().Should().Contain("\u001b[2m2 tests filtered out");
            output.ToString().Should().NotContain("Second").And.NotContain("Unique Test");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void List_tests_uses_colors_when_enabled()
    {
        var projectDirectory = CreateTestProject(FilterTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.Yes);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Always,
                    console: console,
                    listTests: true);

            var rendered = output.ToString();

            exitCode.Should().Be(0);
            rendered.Should().Contain("\u001b[1mAvailable tests (3)");
            rendered.Should().Contain("\u001b[93mRoot");
            rendered.Should().Contain("\u001b[32mFirst");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void List_tests_does_not_count_empty_descriptions_as_tests()
    {
        var projectDirectory = CreateTestProject(EmptyGroupTestsModule);
        var (console, output) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    console: console,
                    listTests: true);

            exitCode.Should().Be(0);
            output.ToString().Should().Contain("Available tests (0)");
            output.ToString().Should().NotContain("Empty group");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void No_Elm_test_modules_reports_error()
    {
        var projectDirectory =
            Path.Combine(
                Path.GetTempPath(),
                "pine-elm-test-command-tests",
                Guid.NewGuid().ToString("N") + new string('a', 80));

        Directory.CreateDirectory(projectDirectory);

        var (errorConsole, errorOutput) = CreateConsole(AnsiSupport.No);

        try
        {
            var exitCode =
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never,
                    errorConsole: errorConsole);

            exitCode.Should().Be(1);
            errorOutput.ToString().Should().Contain("Error:");
            errorOutput.ToString().Should().Contain("Did not find Elm test modules");
            errorOutput.ToString().Should().Contain(projectDirectory);
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Unexpected_exception_from_Elm_test_runner_propagates()
    {
        var projectDirectory = CreateTestProject("This is not a valid Elm module.");

        try
        {
            Action execute =
                () =>
                TestCommand.Execute(
                    projectDirectory,
                    colorMode: FormatCommandColorMode.Never);

            execute.Should().Throw<InvalidOperationException>()
                .WithMessage("Failed parsing Elm test module 'tests/Tests.elm':*");
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    private static string CreateTestProject(string testsModule)
    {
        var projectDirectory =
            Path.Combine(
                Path.GetTempPath(),
                "pine-elm-test-command-tests",
                Guid.NewGuid().ToString("N"));

        Directory.CreateDirectory(Path.Combine(projectDirectory, "tests"));

        File.WriteAllText(
            Path.Combine(projectDirectory, "elm.json"),
            ElmJson,
            new UTF8Encoding(encoderShouldEmitUTF8Identifier: false));

        File.WriteAllText(
            Path.Combine(projectDirectory, "tests", "Tests.elm"),
            testsModule,
            new UTF8Encoding(encoderShouldEmitUTF8Identifier: false));

        return projectDirectory;
    }


    private static (IAnsiConsole console, StringWriter output) CreateConsole(
        AnsiSupport ansi)
    {
        var output = new StringWriter();

        var console =
            AnsiConsole.Create(
                new AnsiConsoleSettings
                {
                    Ansi = ansi,
                    ColorSystem = ColorSystemSupport.Standard,
                    Interactive = InteractionSupport.No,
                    Out = new AnsiConsoleOutput(output),
                });

        return (console, output);
    }


    private const string ElmJson =
        """
        {
            "type": "application",
            "source-directories": ["src"],
            "elm-version": "0.19.1",
            "dependencies": {
                "direct": {
                    "elm/core": "1.0.5"
                },
                "indirect": {}
            },
            "test-dependencies": {
                "direct": {
                    "elm-explorations/test": "2.2.0"
                },
                "indirect": {
                    "elm/bytes": "1.0.8",
                    "elm/json": "1.1.4",
                    "elm/random": "1.0.0",
                    "elm/time": "1.0.0"
                }
            }
        }
        """;


    private const string FailingTestsModule =
        """
        module Tests exposing (..)

        import Expect
        import Test exposing (Test)


        suite : Test
        suite =
            Test.describe
                "Group Title"
                [ Test.test "Test Title" <|
                    \_ ->
                        71 |> Expect.equal 71
                , Test.test "Another Test Title" <|
                    \_ ->
                        41 |> Expect.equal 43
                , Test.test "Yet Another Test Title" <|
                    \_ ->
                        21 |> Expect.equal 21
                ]
        """;


    private const string PassingTestsModule =
        """
        module Tests exposing (..)

        import Expect
        import Test exposing (Test)


        suite : Test
        suite =
            Test.describe "Group Title"
                [ Test.test "Test Title" <|
                    \_ ->
                        71 |> Expect.equal 71
                ]
        """;


    private const string EmptyGroupTestsModule =
        """
        module Tests exposing (..)

        import Test exposing (Test)


        suite : Test
        suite =
            Test.describe "Empty group" []
        """;


    private const string FilterTestsModule =
        """
        module Tests exposing (..)

        import Expect
        import Test exposing (Test)


        suite : Test
        suite =
            Test.describe "Root"
                [ Test.describe "Selected Group"
                    [ Test.test "First" <|
                        \_ ->
                            1 |> Expect.equal 1
                    , Test.test "Second" <|
                        \_ ->
                            2 |> Expect.equal 2
                    ]
                , Test.describe "Other Group"
                    [ Test.test "Unique Test" <|
                        \_ ->
                            3 |> Expect.equal 4
                    ]
                ]
        """;
}
