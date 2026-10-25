using AwesomeAssertions;
using Pine.CLI;
using Pine.Core.CLI;
using Pine.Core.Elm;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.CLI;

public sealed class RunCommandTests : IDisposable
{
    private readonly string _directory =
        Path.Combine(Path.GetTempPath(), "pine-run-tests-" + Guid.NewGuid().ToString("N"));

    private string Module => Path.Combine(_directory, "Batch.elm");

    private string Cache => Path.Combine(_directory, "cache");

    public RunCommandTests()
    {
        Directory.CreateDirectory(_directory);

        File.WriteAllText(
            Module,
            """
            module Batch exposing (..)

            import Bytes exposing (Bytes)
            import List
            import String

            type Box a = Box a | Empty

            runRoot : String -> String
            runRoot text = String.toUpper text

            size : String -> Int
            size text = String.length text

            bytes : Bytes -> Bytes
            bytes input = input

            record : { name : String, count : Int } -> String
            record input = String.repeat input.count input.name

            type alias Options = { name : String, count : Int }

            recordPattern : Options -> String
            recordPattern { name, count } = String.repeat count name

            wildcard : String -> String
            wildcard _ = "ignored"

            tuple : ( String, Int ) -> String
            tuple ( text, count ) = String.repeat count text

            choice : Box String -> String
            choice input =
                case input of
                    Box text -> text
                    Empty -> ""

            list : List Int -> Int
            list input = List.sum input

            multiple : String -> String -> String
            multiple a b = a ++ b

            fileTuple : String -> ( String, Int ) -> String
            fileTuple text ( suffix, count ) = String.repeat count text ++ suffix

            tupleFile : ( String, Int ) -> String -> String
            tupleFile ( prefix, count ) text = prefix ++ String.repeat count text

            fileChoice : String -> Box ( String, Int ) -> String
            fileChoice text box =
                case box of
                    Box ( suffix, count ) -> String.repeat count text ++ suffix
                    Empty -> text

            three : String -> String -> String -> String
            three first second third = first ++ second ++ third

            curried : String -> String -> String
            curried first = \second -> first ++ second

            partial : String -> String -> String
            partial = three "prefix:"

            multipleAlias : String -> String -> String
            multipleAlias = multiple

            same : a -> a -> a
            same first second = first

            inferred text = String.length text

            generic : a -> a
            generic value = value

            nested : List (List Int) -> Int
            nested input = List.length input

            higherOrder : (String -> Int) -> Int
            higherOrder callback = callback ""

            constant : Int
            constant = 42

            alias : String -> Int
            alias = String.length

            result : String -> Result String String
            result text = Err text

            {-| Count the characters.
            This documentation is selectable.
            -}
            documented : String -> Int
            documented text =
                String.length text

            loop : Int -> Int
            loop n = loop (n + 1)
            """);
    }

    private (int Exit, string Output, string Error) Run(
        string? declaration = "runRoot",
        string? input = null,
        string? inputFile = null,
        string? outputFile = null,
        string? transformFile = null,
        TimeSpan? timeout = null,
        string? source = null,
        RunSource.Loader? loader = null,
        bool durations = false,
        CancellationToken cancellationToken = default,
        TextReader? inputReader = null,
        bool batchOnly = false,
        Func<IReadOnlyList<RunDeclarationSelection.Candidate>, string>? selectDeclaration = null,
        CommandLineColorMode color = CommandLineColorMode.Never,
        IReadOnlyList<RunCommand.BatchInput>? inputs = null)
    {
        using var output = new StringWriter();
        using var error = new StringWriter();

        var exit =
            RunCommand.Execute(
                source ?? Module + (declaration is null ? "" : "/" + declaration),
                inputs:
                inputs ??
                [
                    .. input is null ? Array.Empty<RunCommand.BatchInput>() : [new(input)],
                    .. inputFile is null ? Array.Empty<RunCommand.BatchInput>() : [new(inputFile, IsFile: true)]
                ],
                outputFile: outputFile,
                transformFile: transformFile,
                color: color,
                timeout: timeout ?? TimeSpan.FromSeconds(45),
                output: output,
                error: error,
                cacheDirectory: Cache,
                sourceLoader: loader,
                reportDurations: durations,
                cancellationToken: cancellationToken,
                inputReader: inputReader ?? TextReader.Null,
                batchOnly: batchOnly,
                selectDeclaration:
                selectDeclaration ??
                (candidates =>
                RunDeclarationSelection.Select(
                    candidates,
                    FormatCommandShared.CreateSystemConsole(error, color),
                    inputRedirected: true,
                    batchOnly,
                    cancellationToken)));

        return (exit, output.ToString(), error.ToString());
    }

    [Theory]
    [InlineData("runRoot", "hello world", "\"HELLO WORLD\"")]
    [InlineData("runRoot", "\"hello\\nworld\"", "\"HELLO\\nWORLD\"")]
    [InlineData("runRoot", "", "\"\"")]
    [InlineData("size", "hello", "5")]
    [InlineData("record", "{ name = \"ab\", count = 2 }", "\"abab\"")]
    [InlineData("tuple", "( \"x\", 3 )", "\"xxx\"")]
    [InlineData("choice", "OtherModule.Box \"hello\"", "\"hello\"")]
    [InlineData("choice", "Empty", "\"\"")]
    [InlineData("list", "[1, 2, -3, 0x04]", "4")]
    [InlineData("inferred", "hello", "5")]
    [InlineData("generic", "( \"x\", 3 )", "(\"x\", 3)")]
    [InlineData("alias", "hello", "5")]
    [InlineData("result", "hello", "Err \"hello\"")]
    public void Run_batch_accepts_supported_inputs(string declaration, string input, string expected)
    {
        foreach (var batchOnly in new[] { false, true })
        {
            using var unusedReader = new StringReader("unused input\n");
            var result = Run(declaration, input, inputReader: unusedReader, batchOnly: batchOnly);
            result.Exit.Should().Be(0, result.Error);
            result.Output.Should().Be(expected + Environment.NewLine);

            result.Error.Should().Contain("Batch mode was selected because")
                .And.Contain(declaration + " : ")
                .And.NotContain("Input> ");

            unusedReader.ReadLine().Should().Be("unused input");

            using var reader = new StringReader(input + "\nunused input\n");
            var prompted = Run(declaration, inputReader: reader, batchOnly: batchOnly);
            prompted.Exit.Should().Be(0, prompted.Error);
            prompted.Output.Should().Be(result.Output);

            prompted.Error.Should().Contain("Batch mode was selected because")
                .And.Contain("Supply batch input using --input, --input-file, or --transform-file")
                .And.Contain("Input> ");

            reader.ReadLine().Should().Be("unused input");
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_module_requires_explicit_selection_even_when_runRoot_exists(bool batchOnly)
    {
        var result = Run(declaration: null, input: "hello", durations: true, batchOnly: batchOnly);
        result.Exit.Should().Be(1, result.Error);
        result.Output.Should().BeEmpty();

        result.Error.Should().Contain("Select a declaration explicitly")
            .And.Contain("runRoot : String -> String")
            .And.Contain("compilation:")
            .And.NotContain("Batch mode was selected");
    }

    [Theory]
    [InlineData("main")]
    [InlineData("runRoot")]
    [InlineData("webServiceMain")]
    public void Run_single_declaration_is_not_an_implicit_default(string name)
    {
        File.WriteAllText(
            Module,
            "module Batch exposing (..)\n\n" + name + " : String -> String\n" + name + " input = input\n");

        foreach (var batchOnly in new[] { false, true })
        {
            var result = Run(declaration: null, input: "hello", batchOnly: batchOnly);
            result.Exit.Should().Be(1);
            result.Error.Should().Contain("Select a declaration explicitly").And.Contain(name + " : String -> String");
            result.Output.Should().BeEmpty();
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_compiles_only_the_explicitly_chosen_declaration(bool batchOnly)
    {
        var selected = false;

        var result =
            Run(
                declaration: null,
                input: "hello",
                batchOnly: batchOnly,
                selectDeclaration:
                candidates =>
                {
                    selected = true;

                    candidates.Should().Contain(
                        item => item.Name == "runRoot" && item.Signature == "runRoot : String -> String");

                    candidates.Should().Contain(item => item.Name == "inferred" && item.Signature == null);
                    return "inferred";
                });

        selected.Should().BeTrue();
        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().Be("5" + Environment.NewLine);
        result.Error.Should().Contain("inferred : String -> Int");
    }

    [Theory]
    [InlineData(CommandLineColorMode.Never, false)]
    [InlineData(CommandLineColorMode.Auto, false)]
    [InlineData(CommandLineColorMode.Always, true)]
    public void Run_colors_inferred_type_signatures_but_not_results(CommandLineColorMode color, bool colored)
    {
        foreach (var batchOnly in new[] { false, true })
        {
            var result = Run("inferred", input: "hello", batchOnly: batchOnly, color: color);
            result.Exit.Should().Be(0, result.Error);
            result.Output.Should().Be("5" + Environment.NewLine);
            Regex.Replace(result.Error, @"\x1b\[[0-9;]*m", "").Should().Contain("inferred : String -> Int");

            var invalid = Run("multiple", input: "hello", batchOnly: batchOnly, color: color);
            invalid.Exit.Should().Be(1);

            Regex.Replace(invalid.Error, @"\x1b\[[0-9;]*m", "").Should().Contain(
                "multiple : String -> String -> String");

            foreach (var diagnostics in new[] { result.Error, invalid.Error })
            {
                if (colored)
                    diagnostics.Should().MatchRegex(@"\x1b\[[0-9;]*mString");

                else
                    diagnostics.Should().NotContain("\u001b[");
            }
        }
    }

    [Fact]
    public void Run_prompts_for_selection_before_dependency_resolution_and_without_evaluating_candidates()
    {
        File.WriteAllText(
            Module,
            """
            module Batch exposing (..)
            import Missing
            candidate : Missing.Value -> Missing.Value
            candidate input = input
            loop value = loop value
            unit : () -> ()
            unit input = input
            """);

        using var cancellation = new CancellationTokenSource();

        var result =
            Run(
                declaration: null,
                cancellationToken: cancellation.Token,
                selectDeclaration:
                candidates =>
                {
                    candidates.Should().Contain(
                        item =>
                        item.Name == "candidate" && item.Signature == "candidate : Missing.Value -> Missing.Value");

                    candidates.Should().Contain(item => item.Name == "loop" && item.Signature == null);
                    candidates.Should().Contain(item => item.Name == "unit" && item.Signature == "unit : () -> ()");
                    cancellation.Cancel();
                    return "candidate";
                });

        result.Exit.Should().Be(130, result.Error);
        result.Output.Should().BeEmpty();
        result.Error.Should().Contain("Run cancelled.").And.NotContain("Count:");
        Directory.Exists(Path.Combine(Cache, "run")).Should().BeFalse();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_invalid_explicit_selection_does_not_prompt(bool batchOnly)
    {
        var result =
            Run(
                "missing",
                input: "hello",
                batchOnly: batchOnly,
                selectDeclaration: _ => throw new InvalidOperationException("The picker must not run."));

        result.Exit.Should().Be(1);
        result.Error.Should().Contain("Declaration 'missing' not found.").And.NotContain("The picker must not run");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Run_cli_requires_explicit_selection_when_stdin_is_redirected_even_in_ci(bool batchOnly)
    {
        var start =
            new ProcessStartInfo("dotnet")
            {
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };

        start.Environment["GITHUB_ACTIONS"] = "true";
        start.Environment["TF_BUILD"] = "True";
        start.ArgumentList.Add(typeof(RunCommand).Assembly.Location);
        start.ArgumentList.Add(batchOnly ? "apply" : "run");
        start.ArgumentList.Add(Module);
        start.ArgumentList.Add("--timeout");
        start.ArgumentList.Add("45");
        using var process = Process.Start(start)!;
        var output = process.StandardOutput.ReadToEndAsync();
        var error = process.StandardError.ReadToEndAsync();

        try
        {
            await process.StandardInput.WriteAsync("hello\n");
            process.StandardInput.Close();
            await process.WaitForExitAsync().WaitAsync(TimeSpan.FromSeconds(60));
            var diagnostics = await error;
            process.ExitCode.Should().Be(1, diagnostics);
            (await output).Should().BeEmpty();

            diagnostics.Should().Contain("Select a declaration explicitly")
                .And.Contain("runRoot : String -> String")
                .And.NotContain("\u001b[")
                .And.NotContain("Input> ")
                .And.NotContain("Batch mode was selected");
        }
        finally
        {
            if (!process.HasExited)
                process.Kill(entireProcessTree: true);
        }
    }

    [Theory]
    [InlineData("multiple", "\"a\"")]
    [InlineData("list", "[1, \"two\"]")]
    [InlineData("list", "List.range 1 3")]
    [InlineData("choice", "Box 42")]
    [InlineData("choice", "Missing")]
    [InlineData("record", "{ name = \"ab\" }")]
    [InlineData("size", "\"unterminated")]
    public void Run_batch_rejects_invalid_inputs_without_stdout(string declaration, string input)
    {
        foreach (var batchOnly in new[] { false, true })
        {
            var result = Run(declaration, input, batchOnly: batchOnly);
            result.Exit.Should().Be(1);
            result.Output.Should().BeEmpty();
            result.Error.Should().Contain("Error:");

            using var reader = new StringReader(input + "\n");
            var prompted = Run(declaration, inputReader: reader, batchOnly: batchOnly);
            prompted.Exit.Should().Be(1, prompted.Error);
            prompted.Output.Should().BeEmpty();
            prompted.Error.Should().Contain("Error:");
        }
    }

    [Fact]
    public void Run_module_without_selection_lists_function_declarations()
    {
        File.WriteAllText(Module, "module Batch exposing (..)\n\nhello : String -> String\nhello text = text\n");
        var result = Run(declaration: null);
        result.Exit.Should().Be(1);
        result.Error.Should().Contain("Select a declaration explicitly").And.Contain("hello : String -> String");
    }

    [Fact]
    public void Run_end_of_input_and_conflicting_options_fail()
    {
        foreach (var batchOnly in new[] { false, true })
        {
            var eof = Run(batchOnly: batchOnly);
            eof.Exit.Should().Be(1);
            eof.Output.Should().BeEmpty();
            eof.Error.Should().Contain("Input> ").And.Contain("No batch input was received (end of input).");
            Run(input: "x", inputFile: "input", batchOnly: batchOnly).Error.Should().Contain("expected 1, received 2");
            Run(input: "x", transformFile: "input", batchOnly: batchOnly).Error.Should().Contain("cannot be combined");

            Run(transformFile: "https://github.com/a/b/blob/main/input", batchOnly: batchOnly).Error.Should().Contain(
                "local file");
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_prompted_input_can_write_an_output_file(bool batchOnly)
    {
        var path = Path.Combine(_directory, "prompted-output");
        using var reader = new StringReader("hello\n");
        var result = Run(outputFile: path, inputReader: reader, batchOnly: batchOnly);
        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().BeEmpty();
        result.Error.Should().Contain("Batch mode was selected because").And.Contain("Input> ");
        File.ReadAllText(path).Should().Be("HELLO");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_cancellation_at_the_input_prompt_does_not_print_counters(bool batchOnly)
    {
        using var cancellation = new CancellationTokenSource();
        using var reader = new CancellingInputReader(cancellation);
        var result = Run(inputReader: reader, cancellationToken: cancellation.Token, batchOnly: batchOnly);
        result.Exit.Should().Be(130, result.Error);
        result.Output.Should().BeEmpty();

        result.Error.Should().Contain("Batch mode was selected because").And.Contain("Input> ")
            .And.Contain("Run cancelled.").And.NotContain("Count:");
    }

    private sealed class CancellingInputReader(CancellationTokenSource cancellation) : TextReader
    {
        public override ValueTask<string?> ReadLineAsync(CancellationToken cancellationToken)
        {
            cancellation.Cancel();
            return ValueTask.FromCanceled<string?>(cancellationToken);
        }
    }

    [Theory]
    [InlineData(false, "hello\n", 0, "\"HELLO\"")]
    [InlineData(true, "hello\n", 0, "\"HELLO\"")]
    [InlineData(false, "\n", 0, "\"\"")]
    [InlineData(true, "\n", 0, "\"\"")]
    [InlineData(false, "", 1, null)]
    [InlineData(true, "", 1, null)]
    [InlineData(false, "\"unterminated\n", 1, null)]
    [InlineData(true, "\"unterminated\n", 1, null)]
    public Task Run_prompt_reads_redirected_stdin_and_preserves_stdout(
        bool batchOnly, string standardInput, int expectedExit, string? expectedOutput) =>
        AssertRedirectedPrompt(standardInput, expectedExit, expectedOutput, batchOnly);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public Task Run_prompt_waiting_for_stdin_observes_timeout(bool batchOnly) =>
        AssertRedirectedPrompt(null, 124, null, batchOnly);

    private async Task AssertRedirectedPrompt(
        string? standardInput,
        int expectedExit,
        string? expectedOutput,
        bool batchOnly)
    {
        var start =
            new ProcessStartInfo("dotnet")
            {
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
            };

        start.ArgumentList.Add(typeof(RunCommand).Assembly.Location);
        start.ArgumentList.Add(batchOnly ? "apply" : "run");
        start.ArgumentList.Add(Module + "/runRoot");
        start.ArgumentList.Add("--timeout");
        start.ArgumentList.Add(standardInput is null ? "30" : "45");
        using var process = Process.Start(start)!;
        var output = process.StandardOutput.ReadToEndAsync();
        var error = process.StandardError.ReadToEndAsync();

        try
        {
            if (standardInput is not null)
            {
                await process.StandardInput.WriteAsync(standardInput);
                process.StandardInput.Close();
            }

            await process.WaitForExitAsync().WaitAsync(TimeSpan.FromSeconds(60));
            var diagnostics = await error;
            process.ExitCode.Should().Be(expectedExit, diagnostics);
            (await output).Should().Be(expectedOutput is null ? "" : expectedOutput + Environment.NewLine);

            diagnostics.Should().Contain("Batch mode was selected because")
                .And.Contain("runRoot : String -> String")
                .And.Contain("Supply batch input using --input, --input-file, or --transform-file")
                .And.Contain("Input> ");

            if (standardInput is "")
                diagnostics.Should().Contain("No batch input was received (end of input).");

            if (standardInput is null)
                diagnostics.Should().Contain("Run timed out").And.NotContain("Count:");
        }
        finally
        {
            if (!process.HasExited)
                process.Kill(entireProcessTree: true);
        }
    }

    [Theory]
    [InlineData("runRoot", "runRoot : String -> String")]
    [InlineData("inferred", "inferred : String -> Int")]
    [InlineData("generic", "generic : a -> a")]
    [InlineData("record", "record : { count : Int, name : String } -> String")]
    [InlineData("tuple", "tuple : (String, Int) -> String")]
    [InlineData("choice", "choice : Box String -> String")]
    [InlineData("list", "list : List Int -> Int")]
    [InlineData("nested", "nested : List (List Int) -> Int")]
    [InlineData("higherOrder", "higherOrder : (String -> Int) -> Int")]
    [InlineData("bytes", "bytes : Bytes -> Bytes")]
    [InlineData("result", "result : String -> Result String String")]
    public void Run_missing_batch_input_prompts_with_the_selected_Elm_signature(string declaration, string signature)
    {
        var result = Run(declaration);
        result.Exit.Should().Be(1);
        result.Output.Should().BeEmpty();

        result.Error.Should().Contain("Supply batch input using --input, --input-file, or --transform-file")
            .And.Contain("Batch mode was selected because")
            .And.Contain("Input> ")
            .And.Contain(Environment.NewLine + "    " + signature + Environment.NewLine);
    }

    [Theory]
    [InlineData("constant", "constant : Int")]
    public void Run_unsupported_declaration_reports_the_selected_Elm_signature(string declaration, string signature)
    {
        foreach (var batchOnly in new[] { false, true })
        {
            var result = Run(declaration, batchOnly: batchOnly);
            result.Exit.Should().Be(1);
            result.Output.Should().BeEmpty();

            result.Error.Should().Contain("Select a function taking one or more inputs")
                .And.Contain(Environment.NewLine + "    " + signature + Environment.NewLine);

            if (batchOnly)
                result.Error.Should().NotContain("or a command-line application");
        }
    }

    [Fact]
    public void Run_requires_selection_and_reports_names_and_types_also_from_cached_compilation()
    {
        var first = Run(declaration: null);
        first.Exit.Should().Be(1);
        first.Error.Should().Contain("Select a declaration explicitly").And.Contain("runRoot : String -> String");

        Run("runRoot").Exit.Should().Be(1);
        var cached = Run("runRoot");
        cached.Exit.Should().Be(1);
        cached.Error.Should().Contain("Using cached run compilation.").And.Contain("runRoot : String -> String");

        var lines = File.ReadAllLines(Module);
        var line = Array.FindIndex(lines, item => item.Contains("inferred text =", StringComparison.Ordinal)) + 1;
        var selected = Run(source: Module + "#L" + line);
        selected.Exit.Should().Be(1);
        selected.Output.Should().BeEmpty();
        selected.Error.Should().Contain("inferred : String -> Int");
    }

    [Theory]
    [InlineData("import Types as T", "T.Box (List Int)")]
    [InlineData("import Types exposing (Box)", "Box (List Int)")]
    [InlineData("import Types exposing (..)", "Box (List Int)")]
    public void Run_missing_batch_input_uses_source_type_names_in_resolved_projects(string import, string inputType)
    {
        File.WriteAllText(
            Path.Combine(_directory, "elm.json"),
            """{"type":"application","source-directories":["."],"elm-version":"0.19.1","dependencies":{"direct":{"elm/core":"1.0.5"},"indirect":{}},"test-dependencies":{"direct":{},"indirect":{}}}""");

        File.WriteAllText(
            Path.Combine(_directory, "Types.elm"),
            "module Types exposing (..)\n\ntype Box a = Box a\n");

        var main = Path.Combine(_directory, "Main.elm");

        File.WriteAllText(
            main,
            "module Main exposing (..)\n\n" + import + "\n\nrunRoot : " + inputType +
            " -> Maybe (List Int)\nrunRoot input = Nothing\n");

        var result = Run(source: main + "/runRoot", timeout: TimeSpan.FromSeconds(45));
        result.Exit.Should().Be(1, result.Error);
        result.Output.Should().BeEmpty();
        result.Error.Should().Contain("runRoot : " + inputType + " -> Maybe (List Int)");
        result.Error.Should().Contain("input : " + inputType);

        var cached = Run(source: main + "/runRoot", timeout: TimeSpan.FromSeconds(45));
        cached.Exit.Should().Be(1, cached.Error);

        cached.Error.Should().Contain("Using cached run compilation.")
            .And.Contain("runRoot : " + inputType + " -> Maybe (List Int)")
            .And.Contain("input : " + inputType);
    }

    [Theory]
    [InlineData("This documentation is selectable.")]
    [InlineData("documented :")]
    [InlineData("documented text =")]
    [InlineData("    String.length text")]
    public void Run_line_selection_includes_documentation_signature_and_body(string marker)
    {
        var lines = File.ReadAllLines(Module);
        var line = Array.FindLastIndex(lines, item => item.Contains(marker, StringComparison.Ordinal)) + 1;

        foreach (var batchOnly in new[] { false, true })
        {
            var result = Run(input: "hello", source: Module + "#L" + line, batchOnly: batchOnly);
            result.Exit.Should().Be(0, result.Error);
            result.Output.Should().Be("5" + Environment.NewLine);
        }
    }

    [Theory]
    [InlineData("utf8", false)]
    [InlineData("utf8", true)]
    [InlineData("utf16le", true)]
    [InlineData("utf16be", true)]
    [InlineData("utf32le", true)]
    [InlineData("utf32be", true)]
    public void Run_transform_preserves_encoding_byte_order_and_bom(string name, bool bom)
    {
        Encoding encoding =
            name switch
            {
                "utf8" => new UTF8Encoding(bom, true),
                "utf16le" => new UnicodeEncoding(false, bom, true),
                "utf16be" => new UnicodeEncoding(true, bom, true),
                "utf32le" => new UTF32Encoding(false, bom, true),
                "utf32be" => new UTF32Encoding(true, bom, true),

                _ =>
                throw new NotImplementedException(name)
            };

        var path = Path.Combine(_directory, "text");

        foreach (var batchOnly in new[] { false, true })
        {
            File.WriteAllBytes(path, [.. encoding.GetPreamble(), .. encoding.GetBytes("h\u00e9llo\n")]);
            var result = Run(transformFile: path, batchOnly: batchOnly);
            result.Exit.Should().Be(0, result.Error);
            result.Output.Should().BeEmpty();
            File.ReadAllBytes(path).Should().Equal([.. encoding.GetPreamble(), .. encoding.GetBytes("H\u00e9LLO\n")]);
        }
    }

    [Fact]
    public void Run_binary_files_are_not_decoded_and_stdout_uses_hex()
    {
        var path = Path.Combine(_directory, "binary");
        byte[] bytes = [0xff, 0xfe, 0, 0, 0xff];

        foreach (var batchOnly in new[] { false, true })
        {
            File.WriteAllBytes(path, bytes);
            var result = Run("bytes", inputFile: path, batchOnly: batchOnly);
            result.Exit.Should().Be(0, result.Error);
            result.Output.Should().Be("Bytes 5 0xfffe0000ff" + Environment.NewLine);
            result.Error.Should().Contain("Batch mode was selected because").And.NotContain("Input> ");
            Run("bytes", transformFile: path, batchOnly: batchOnly).Exit.Should().Be(0);
            File.ReadAllBytes(path).Should().Equal(bytes);
        }
    }

    [Theory]
    [InlineData(new byte[] { 0xff })]
    [InlineData(new byte[] { 0xff, 0xfe, 0x61 })]
    [InlineData(new byte[] { 0, 0, 0xfe, 0xff, 0, 0x11, 0, 0 })]
    public void Run_invalid_encoding_leaves_transform_destination_unchanged(byte[] bytes)
    {
        var path = Path.Combine(_directory, "invalid");

        foreach (var batchOnly in new[] { false, true })
        {
            File.WriteAllBytes(path, bytes);
            var result = Run(transformFile: path, batchOnly: batchOnly);
            result.Exit.Should().Be(1);
            result.Output.Should().BeEmpty();
            File.ReadAllBytes(path).Should().Equal(bytes);
        }
    }

    [Fact]
    public void Run_file_outputs_use_raw_strings_and_render_other_values()
    {
        var path = Path.Combine(_directory, "result");

        foreach (var batchOnly in new[] { false, true })
        {
            Run(input: "hello", outputFile: path, batchOnly: batchOnly).Exit.Should().Be(0);
            File.ReadAllBytes(path).Should().Equal(Encoding.UTF8.GetBytes("HELLO"));
            Run("size", input: "hello", outputFile: path, batchOnly: batchOnly).Exit.Should().Be(0);
            File.ReadAllText(path).Should().Be("5");
        }
    }

    [Fact]
    public void Run_nested_bytes_render_as_hex_and_invalid_lines_are_rejected()
    {
        var bytes = new ElmValue.ElmBytes(new byte[] { 0, 1, 255 });

        RunValues.Render(ElmValue.RecordInstance([("data", bytes)]))
            .Should().Be("{ data = Bytes 3 0x0001ff }");

        var result = Run(input: "hello", source: Module + "#L1");
        result.Exit.Should().Be(1);
        result.Error.Should().Contain("not inside a function");
    }

    [Fact]
    public void Run_compiled_cache_reuses_identical_sources_and_invalidates_source_changes()
    {
        Run(input: "hello").Exit.Should().Be(0);
        var second = Run(input: "other", batchOnly: true);
        second.Exit.Should().Be(0, second.Error);
        second.Error.Should().Contain("Using cached run compilation.");
        var before = Directory.GetFiles(Path.Combine(Cache, "run"));
        File.AppendAllText(Module, "\nnewValue = 42\n");
        var changed = Run(input: "hello");
        changed.Exit.Should().Be(0, changed.Error);
        Directory.GetFiles(Path.Combine(Cache, "run")).Length.Should().Be(before.Length + 1);
    }

    [Fact]
    public void Run_remote_sources_and_inputs_pin_a_branch_once_and_cache_by_contents()
    {
        var resolutions = 0;

        var tree =
            FileTree.FromSetOfFilesWithStringPath(
                new[]
                {
                    ((IReadOnlyList<string>)["src", "Batch.elm"], (ReadOnlyMemory<byte>)File.ReadAllBytes(Module)),
                    ((IReadOnlyList<string>)["input.txt"], (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes("hello"))
                });

        var fixture = new GitRepositoryFixture(tree);
        var commit = fixture.Commit;
        using var diagnostics = new StringWriter();

        var loader =
            new RunSource.Loader(
                Cache,
                diagnostics,
                (_, _) =>
                {
                    ++resolutions;
                    return commit;
                },
                (url, pinned, _) =>
                {
                    url.Should().Be("https://github.com/owner/repo.git");
                    pinned.Should().Be(commit);
                    return fixture.Open();
                });

        var result =
            Run(
                inputFile: "https://github.com/owner/repo/blob/main/input.txt",
                source: "https://github.com/owner/repo/blob/main/src/Batch.elm/runRoot",
                loader: loader);

        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().Be("\"HELLO\"" + Environment.NewLine);
        resolutions.Should().Be(1);
        fixture.OpenCount.Should().Be(1);
        fixture.RequestedPaths.Should().BeEquivalentTo("src/Batch.elm", "input.txt");

        diagnostics.ToString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries).Should().Equal(
            "Resolved Git branch 'main' in https://github.com/owner/repo to commit " + commit + ".",
            "Loading Git source https://github.com/owner/repo at " + commit + ".",
            "Resolved Git branch 'main' in https://github.com/owner/repo to commit " + commit + ".");

        diagnostics.GetStringBuilder().Clear();

        var pinned =
            Run(
                input: "hello",
                source: $"https://github.com/owner/repo/blob/{commit}/src/Batch.elm/runRoot",
                loader: loader,
                batchOnly: true);

        pinned.Exit.Should().Be(0, pinned.Error);
        pinned.Error.Should().Contain("Using cached run compilation.");
        diagnostics.ToString().Should().BeEmpty();
    }

    [Fact]
    public void Run_loads_the_sufficient_ancestor_and_ignores_non_governing_manifests()
    {
        var nested = Path.Combine(_directory, "nested");
        Directory.CreateDirectory(nested);
        Directory.CreateDirectory(Path.Combine(_directory, ".git"));

        File.WriteAllText(
            Path.Combine(_directory, "Helper.elm"),
            "module Helper exposing (size)\n\nsize : String -> Int\nsize text = String.length text\n");

        File.WriteAllText(
            Path.Combine(nested, "Main.elm"),
            "module Main exposing (main)\n\nimport Helper\n\nmain : String -> Int\nmain text = Helper.size text\n");

        File.WriteAllText(
            Path.Combine(_directory, "elm.json"),
            """
            {"type":"application","source-directories":["unrelated"],"elm-version":"0.19.1","dependencies":{"direct":{"elm/core":"1.0.5"},"indirect":{}},"test-dependencies":{"direct":{},"indirect":{}}}
            """);

        var result = Run(input: "hello", source: Path.Combine(nested, "Main.elm") + "/main");
        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().Be("5" + Environment.NewLine);
    }

    [Fact]
    public void Run_resolves_project_sources_using_elm_json()
    {
        var project = Path.Combine(_directory, "project");
        var src = Path.Combine(project, "src");
        Directory.CreateDirectory(src);

        File.WriteAllText(
            Path.Combine(project, "elm.json"),
            """
            {"type":"application","source-directories":["src"],"elm-version":"0.19.1","dependencies":{"direct":{"elm/core":"1.0.5"},"indirect":{}},"test-dependencies":{"direct":{},"indirect":{}}}
            """);

        File.WriteAllText(
            Path.Combine(src, "Main.elm"),
            "module Main exposing (main)\n\nmain : String -> Int\nmain text = String.length text\n");

        var result = Run(input: "hello", source: Path.Combine(src, "Main.elm") + "/main");
        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().Be("5" + Environment.NewLine);
    }

    [Theory]
    [InlineData("https://github.com/owner/repo", "explicit branch")]
    [InlineData("https://github.com/owner/repo/blob/feature%2Fbranch/Batch.elm/size", "slashes")]
    public void Run_rejects_unsupported_remote_references_before_loading(string source, string message)
    {
        var result = Run(input: "hello", source: source);
        result.Exit.Should().Be(1);
        result.Error.Should().Contain(message);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_cancellation_is_observed_after_remote_loading_before_publishing_results(bool batchOnly)
    {
        using var cancelled = new CancellationTokenSource();

        var fixture =
            new GitRepositoryFixture(
                FileTree.FromSetOfFilesWithStringPath(
                    new[] { ((IReadOnlyList<string>)["Batch.elm"], (ReadOnlyMemory<byte>)File.ReadAllBytes(Module)) }));

        var loader =
            new RunSource.Loader(
                Cache,
                TextWriter.Null,
                openRemote:
                (_, _, _) =>
                {
                    cancelled.Cancel();
                    return fixture.Open();
                });

        var result =
            Run(
                input: "hello",
                source: "https://github.com/owner/repo/blob/" + fixture.Commit + "/Batch.elm/runRoot",
                loader: loader,
                cancellationToken: cancelled.Token,
                batchOnly: batchOnly);

        result.Exit.Should().Be(130);
        result.Output.Should().BeEmpty();
        result.Error.Should().Contain("Run cancelled.").And.NotContain("Count:");
        Directory.Exists(Path.Combine(Cache, "run")).Should().BeFalse();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_timeout_and_cancellation_preserve_files_and_print_counters(bool batchOnly)
    {
        // Warm VM setup and compilation so the deadline interrupts actual work.
        var warmup = Run(input: "warmup", batchOnly: batchOnly);
        warmup.Exit.Should().Be(0, warmup.Error);
        var source = new RunSource.Loader(Cache, TextWriter.Null).Read(Module + "/loop", null, default);
        RunCompilation.Compile(source, Cache, TextWriter.Null, default);
        var path = Path.Combine(_directory, "output");
        File.WriteAllText(path, "unchanged");

        var result =
            Run("loop", input: "0", outputFile: path, timeout: TimeSpan.FromSeconds(5), batchOnly: batchOnly);

        result.Exit.Should().Be(124, result.Error);
        result.Error.Should().Contain("timed out").And.MatchRegex(@"(?m)^InstructionCount: [1-9]");
        result.Output.Should().BeEmpty();
        File.ReadAllText(path).Should().Be("unchanged");
        using var cancelled = new CancellationTokenSource();
        cancelled.Cancel();
        var beforeWork = Run(input: "hello", cancellationToken: cancelled.Token, batchOnly: batchOnly);
        beforeWork.Exit.Should().Be(130);
        beforeWork.Output.Should().BeEmpty();
        beforeWork.Error.Should().Contain("Run cancelled.").And.NotContain("Count:");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_cli_help_and_timeout_validation(bool batchOnly)
    {
        using var output = new StringWriter();
        var command = RunCommand.Create(batchOnly);
        command.Name.Should().Be(batchOnly ? "apply" : "run");
        command.Parse(["--help"]).Invoke(new InvocationConfiguration { Output = output }).Should().Be(0);
        output.ToString().Should().Contain("--input-file").And.Contain("--transform-file").And.Contain("--timeout");

        foreach (var invalid in new[] { "0", "-1", "NaN", "Infinity" })
            command.Parse([Module, "--timeout", invalid]).Errors.Should().NotBeEmpty();

        command.Parse([Module, "--input", "one", "--input", "two"]).Errors.Should().BeEmpty();
        command.Parse([Module, "--input-file", "one", "--input-file", "two"]).Errors.Should().BeEmpty();
        command.Parse([Module, "--input", "one", "--input-file", "two"]).Errors.Should().BeEmpty();
        command.Parse([Module, "--input"]).Errors.Should().NotBeEmpty();
        command.Parse([Module, "--input-file"]).Errors.Should().NotBeEmpty();
        command.Parse([Module, "--input", "one", "--input"]).Errors.Should().NotBeEmpty();
        command.Parse([Module, "--input-file", "one", "--input-file"]).Errors.Should().NotBeEmpty();
        command.Parse([Module, "--input", "--input", "two"]).Errors.Should().NotBeEmpty();
        command.Parse([Module, "--input", "one", "two"]).Errors.Should().NotBeEmpty();
        command.Parse([Module, "--output-file", "one", "--output-file", "two"]).Errors.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_cli_batch_options_are_wired_to_the_shared_executor(bool batchOnly)
    {
        using var output = new StringWriter();
        using var error = new StringWriter();

        var exit =
            RunCommand.Create(batchOnly).Parse(
                [Module + "/size", "--input", "hello", "--color", "Never", "--report-durations", "--timeout", "45"])
            .Invoke(new InvocationConfiguration { Output = output, Error = error });

        exit.Should().Be(0, error.ToString());
        output.ToString().Should().Be("5" + Environment.NewLine);

        error.ToString().Should().Contain("Batch mode was selected because")
            .And.Contain("size : String -> Int")
            .And.Contain("compilation:")
            .And.Contain("execution:")
            .And.NotContain("Input> ");
    }

    [Theory]
    [InlineData("multiple", "\"firstsecond\"", "--input", "first", "--input", "second")]
    [InlineData("multiple", "\"samesame\"", "--input", "same", "--input", "same")]
    [InlineData("multiple", "\"second\"", "--input", "", "--input", "second")]
    [InlineData("multiple", "\"--input-filex\"", "--input=--input-file", "--input=x")]
    [InlineData("multipleAlias", "\"firstsecond\"", "--input", "first", "--input=second")]
    [InlineData("curried", "\"firstsecond\"", "--input", "first", "--input", "second")]
    [InlineData("partial", "\"prefix:firstsecond\"", "--input", "first", "--input", "second")]
    [InlineData("three", "\"firstsecondthird\"", "--input", "first", "--input", "second", "--input", "third")]
    [InlineData("fileTuple", "\"ababab!\"", "--input-file", "<file>", "--input", "( \"!\", 3 )")]
    [InlineData("tupleFile", "\"!ababab\"", "--input", "( \"!\", 3 )", "--input-file", "<file>")]
    [InlineData("fileChoice", "\"ababab!\"", "--input-file=<file>", "--input:Box ( \"!\", 3 )")]
    [InlineData("fileChoice", "\"ab\"", "--input-file", "<file>", "--input", "Empty")]
    [InlineData("three", "\"ab|ab\"", "--input-file", "<file>", "--input", "|", "--input-file=<file>")]
    [InlineData("same", "\"first\"", "--input", "\"first\"", "--input", "\"second\"")]
    public void Run_cli_multi_parameter_inputs_preserve_order(
        string declaration,
        string expected,
        params string[] options)
    {
        var path = Path.Combine(_directory, "input");
        File.WriteAllText(path, "ab");

        foreach (var batchOnly in new[] { false, true })
        {
            using var output = new StringWriter();
            using var error = new StringWriter();

            var exit =
                RunCommand.Create(batchOnly).Parse(
                    [
                        Module + "/" + declaration,
                        .. options.Select(option => option.Replace("<file>", path, StringComparison.Ordinal)),
                        "--color", "Never", "--timeout", "45"
                    ])
                .Invoke(new InvocationConfiguration { Output = output, Error = error });

            exit.Should().Be(0, error.ToString());
            output.ToString().Should().Be(expected + Environment.NewLine);
            error.ToString().Should().Contain("Batch mode was selected because").And.NotContain("Input> ");
        }
    }

    [Theory]
    [InlineData("multiple", 3, 2)]
    [InlineData("runRoot", 2, 1)]
    public void Run_batch_rejects_excess_inputs_before_reading_files(
        string declaration, int supplied, int required)
    {
        var path = Path.Combine(_directory, "output");
        File.WriteAllText(path, "unchanged");

        foreach (var batchOnly in new[] { false, true })
        {
            using var reader = new StringReader("unused input\n");

            var result =
                Run(
                    declaration,
                    inputs: [.. Enumerable.Repeat(new RunCommand.BatchInput("missing-file", IsFile: true), supplied)],
                    outputFile: path,
                    inputReader: reader,
                    batchOnly: batchOnly);

            result.Exit.Should().Be(1);
            result.Output.Should().BeEmpty();

            result.Error.Should().Contain($"expected {required}, received {supplied}")
                .And.Contain(declaration + " : ")
                .And.NotContain("Input> ");

            reader.ReadLine().Should().Be("unused input");
            File.ReadAllText(path).Should().Be("unchanged");
        }
    }

    [Theory]
    [InlineData("fileTuple", "( \"!\", \"not an Int\" )", "does not match")]
    [InlineData("fileChoice", "Box ( 42, 3 )", "does not match")]
    [InlineData("multiple", "\"unterminated", "Invalid --input expression")]
    public void Run_batch_validates_each_parameter_before_writing_output(
        string declaration, string expression, string message)
    {
        var inputPath = Path.Combine(_directory, "input");
        var outputPath = Path.Combine(_directory, "output");
        File.WriteAllText(inputPath, "ab");
        File.WriteAllText(outputPath, "unchanged");

        foreach (var batchOnly in new[] { false, true })
        {
            var result =
                Run(
                    declaration,
                    inputs: [new(inputPath, IsFile: true), new(expression)],
                    outputFile: outputPath,
                    batchOnly: batchOnly);

            result.Exit.Should().Be(1);
            result.Output.Should().BeEmpty();
            result.Error.Should().Contain(message);
            File.ReadAllText(outputPath).Should().Be("unchanged");
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_batch_validates_shared_type_variables_across_parameters(bool batchOnly)
    {
        var result = Run("same", inputs: [new("\"text\""), new("42")], batchOnly: batchOnly);
        result.Exit.Should().Be(1);
        result.Output.Should().BeEmpty();
        result.Error.Should().Contain("does not match");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_prompts_once_per_parameter_when_no_inputs_are_supplied(bool batchOnly)
    {
        using var reader = new StringReader("ab\n( \"!\", 3 )\nunused input\n");
        var result = Run("fileTuple", inputReader: reader, batchOnly: batchOnly);
        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().Be("\"ababab!\"" + Environment.NewLine);
        result.Error.Should().Contain("Input 1 of 2> ").And.Contain("Input 2 of 2> ");

        result.Error.Should().Contain("Argument 1/2: text : String")
            .And.Contain("Argument 2/2: ( suffix, count ) : ( String, Int )")
            .And.Contain("Source: stdin")
            .And.Contain("Value: \"ab\"")
            .And.Contain("Value: (\"!\", 3)");

        reader.ReadLine().Should().Be("unused input");

        using var incomplete = new StringReader("ab\n");
        var eof = Run("fileTuple", inputReader: incomplete, batchOnly: batchOnly);
        eof.Exit.Should().Be(1);
        eof.Output.Should().BeEmpty();
        eof.Error.Should().Contain("No batch input was received (end of input).");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_partial_inputs_prompt_for_each_remaining_parameter_and_log_used_inputs(bool batchOnly)
    {
        for (var attempt = 0; attempt < 2; ++attempt)
        {
            using var reader = new StringReader("second\nthird\nunused input\n");
            var result = Run("three", inputs: [new("first")], inputReader: reader, batchOnly: batchOnly);
            result.Exit.Should().Be(0, result.Error);
            result.Output.Should().Be("\"firstsecondthird\"" + Environment.NewLine);

            result.Error.Should().Contain("Argument 1/3: first : String")
                .And.Contain("Source: --input")
                .And.Contain("Value: \"first\"")
                .And.Contain("Argument 2/3: second : String")
                .And.Contain("Input 2 of 3> ")
                .And.Contain("Value: \"second\"")
                .And.Contain("Argument 3/3: third : String")
                .And.Contain("Input 3 of 3> ")
                .And.Contain("Value: \"third\"")
                .And.Contain("Applying three...")
                .And.NotContain("Input 1 of 3> ");

            result.Error.IndexOf("Argument 2/3:", StringComparison.Ordinal).Should().BeLessThan(
                result.Error.IndexOf("Input 2 of 3> ", StringComparison.Ordinal));

            result.Error.IndexOf("Argument 3/3:", StringComparison.Ordinal).Should().BeLessThan(
                result.Error.IndexOf("Input 3 of 3> ", StringComparison.Ordinal));

            reader.ReadLine().Should().Be("unused input");

            if (attempt is 1)
                result.Error.Should().Contain("Using cached run compilation.");
        }
    }

    [Theory]
    [InlineData("inferred", "hello\n", "text : String", "5")]
    [InlineData("recordPattern", "{ name = \"x\", count = 2 }\n", "{ name, count } : Options", "\"xx\"")]
    [InlineData("wildcard", "hello\n", "_ : String", "\"ignored\"")]
    [InlineData("curried", "first\nsecond\n", "second : String", "\"firstsecond\"")]
    [InlineData("multipleAlias", "first\nsecond\n", "argument 2 : String", "\"firstsecond\"")]
    public void Run_parameter_guides_show_source_patterns_and_annotations_or_inferred_types(
        string declaration, string input, string guide, string expected)
    {
        foreach (var batchOnly in new[] { false, true })
        {
            using var reader = new StringReader(input);
            var result = Run(declaration, inputReader: reader, batchOnly: batchOnly);
            result.Exit.Should().Be(0, result.Error);
            result.Output.Should().Be(expected + Environment.NewLine);
            result.Error.Should().Contain(guide).And.Contain("Source: stdin").And.Contain("Value:");

            result.Error.IndexOf(guide, StringComparison.Ordinal).Should().BeLessThan(
                result.Error.LastIndexOf("Input", StringComparison.Ordinal));
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Run_cli_prompts_for_a_missing_tuple_after_file_input_with_parameter_guides(bool batchOnly)
    {
        var path = Path.Combine(_directory, "input");
        File.WriteAllText(path, "ab");

        var start =
            new ProcessStartInfo("dotnet")
            {
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };

        foreach (var argument in new[]
        {
            typeof(RunCommand).Assembly.Location, batchOnly ? "apply" : "run",
            Module + "/fileTuple", "--input-file", path, "--color", "Never", "--timeout", "45"
        })
        {
            start.ArgumentList.Add(argument);
        }

        using var process = Process.Start(start)!;
        var output = process.StandardOutput.ReadToEndAsync();
        var error = process.StandardError.ReadToEndAsync();

        try
        {
            await process.StandardInput.WriteAsync("( \"!\", 3 )\n");
            process.StandardInput.Close();
            await process.WaitForExitAsync().WaitAsync(TimeSpan.FromSeconds(60));
            var diagnostics = await error;
            process.ExitCode.Should().Be(0, diagnostics);
            (await output).Should().Be("\"ababab!\"" + Environment.NewLine);

            diagnostics.Should().Contain("fileTuple : String -> (String, Int) -> String")
                .And.Contain("Argument 1/2: text : String")
                .And.Contain("Source: --input-file")
                .And.Contain(path)
                .And.Contain("(2 bytes)")
                .And.Contain("Value: \"ab\"")
                .And.Contain("Argument 2/2: ( suffix, count ) : ( String, Int )")
                .And.Contain("Input 2 of 2> ")
                .And.Contain("Source: stdin")
                .And.Contain("Value: (\"!\", 3)")
                .And.NotContain("Input 1 of 2> ")
                .And.NotContain("\u001b[");
        }
        finally
        {
            if (!process.HasExited)
                process.Kill(entireProcessTree: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_cancellation_at_a_remaining_parameter_prompt_preserves_output(bool batchOnly)
    {
        var path = Path.Combine(_directory, "output");
        File.WriteAllText(path, "unchanged");
        using var cancellation = new CancellationTokenSource();
        using var reader = new CancellingInputReader(cancellation);

        var result =
            Run(
                "multiple",
                inputs: [new("first")],
                outputFile: path,
                inputReader: reader,
                cancellationToken: cancellation.Token,
                batchOnly: batchOnly);

        result.Exit.Should().Be(130, result.Error);
        result.Output.Should().BeEmpty();
        result.Error.Should().Contain("Argument 2/2: b : String").And.Contain("Run cancelled.");
        File.ReadAllText(path).Should().Be("unchanged");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_transform_prompts_for_remaining_arguments_and_logs_the_destination(bool batchOnly)
    {
        var path = Path.Combine(_directory, "transform");
        File.WriteAllText(path, "ab", new UTF8Encoding(false, true));
        using var reader = new StringReader("( \"!\", 3 )\n");
        var result = Run("fileTuple", transformFile: path, inputReader: reader, batchOnly: batchOnly);
        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().BeEmpty();

        result.Error.Should().Contain("Source: --transform-file")
            .And.Contain("Argument 2/2: ( suffix, count ) : ( String, Int )")
            .And.Contain("Input 2 of 2> ")
            .And.Contain("Result written to")
            .And.Contain(path);

        File.ReadAllText(path).Should().Be("ababab!");
    }

    [Fact]
    public void Run_used_input_log_bounds_previews_without_changing_values()
    {
        var input = "[red]\n" + new string('x', 500);
        var result = Run(input: input, batchOnly: true);
        result.Exit.Should().Be(0, result.Error);

        result.Output.Should().Be(
            RunValues.Render(ElmValue.StringInstance(input.ToUpperInvariant())) + Environment.NewLine);

        result.Error.Should().Contain("Argument 1/1: text : String")
            .And.Contain("Value: \"[red]\\n")
            .And.Contain("(506 characters)")
            .And.NotContain(new string('x', 161));

        RunValues.RenderInputPreview(new ElmValue.ElmBytes(new byte[1000]))
            .Should().Be("Bytes 1000 0x" + new string('0', 64) + "...");

        var prefix = new string('x', 159);

        RunValues.RenderInputPreview(ElmValue.StringInstance(prefix + "\ud83d\ude42tail"))
            .Should().Be("\"" + prefix + "\" ... (165 characters)");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_multiple_file_inputs_preserve_the_first_text_files_encoding(bool batchOnly)
    {
        var first = Path.Combine(_directory, "first");
        var second = Path.Combine(_directory, "second");
        var destination = Path.Combine(_directory, "output");
        var encoding = new UnicodeEncoding(bigEndian: true, byteOrderMark: true, throwOnInvalidBytes: true);
        File.WriteAllText(first, "first", encoding);
        File.WriteAllText(second, "second", new UTF8Encoding(false, true));

        var result =
            Run(
                "multiple",
                inputs: [new(first, IsFile: true), new(second, IsFile: true)],
                outputFile: destination,
                batchOnly: batchOnly);

        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().BeEmpty();

        File.ReadAllBytes(destination).Should().Equal(
            [.. encoding.GetPreamble(), .. encoding.GetBytes("firstsecond")]);
    }

    [Fact]
    public async Task Apply_is_listed_in_top_level_help()
    {
        var start =
            new ProcessStartInfo("dotnet")
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
            };

        start.ArgumentList.Add(typeof(RunCommand).Assembly.Location);
        start.ArgumentList.Add("help");
        using var process = Process.Start(start)!;
        var output = process.StandardOutput.ReadToEndAsync();
        var error = process.StandardError.ReadToEndAsync();

        try
        {
            await process.WaitForExitAsync().WaitAsync(TimeSpan.FromSeconds(30));
            process.ExitCode.Should().Be(0, await error);
            (await output).Should().Contain("Apply an Elm function to batch inputs; reject interactive applications.");
        }
        finally
        {
            if (!process.HasExited)
                process.Kill(entireProcessTree: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Run_app_preserves_stdin_stdout_stderr_and_exit_status(bool customDeclaration)
    {
        var app = Path.Combine(_directory, "App.elm");

        File.WriteAllText(
            app,
            """
            module App exposing (..)

            import Bytes exposing (Bytes)
            import Bytes.Encode

            type Command = SendToStdOutCmd Bytes | SendToStdErrCmd Bytes

            type alias Response = { commands : List Command, exit : Maybe Int }
            type alias Environment = { commandLine : String, environmentVariables : List ( String, String ) }
            type alias Program =
                { init : Environment -> ( (), Response )
                , subscriptions : () -> { stdIn : Maybe (Bytes -> () -> ( (), Response )) }
                }

            runRoot : Program
            runRoot = program

            custom : Program
            custom = program

            program : Program
            program =
                { init = init
                , subscriptions = subscriptions
                }

            init : Environment -> ( (), Response )
            init environment =
                ( ()
                , { commands = [ SendToStdErrCmd (Bytes.Encode.encode (Bytes.Encode.string "app stderr\n")) ]
                  , exit = Nothing
                  }
                )

            subscriptions : () -> { stdIn : Maybe (Bytes -> () -> ( (), Response )) }
            subscriptions state = { stdIn = Just receive }

            receive : Bytes -> () -> ( (), Response )
            receive bytes state = ( (), { commands = [ SendToStdOutCmd bytes ], exit = Just 7 } )
            """);

        var declaration = customDeclaration ? "custom" : "runRoot";
        var entryPoint = app + "/" + declaration;

        var conflict =
            Run(
                source: entryPoint,
                input: "unexpected",
                timeout: TimeSpan.FromSeconds(60));

        conflict.Exit.Should().Be(1, conflict.Error);
        conflict.Output.Should().BeEmpty();

        conflict.Error.Should().Contain("Application mode was selected because")
            .And.Contain("Batch input/output options cannot be used with an application.")
            .And.Contain(declaration + " : { init :")
            .And.Contain("subscriptions :");

        var lines = File.ReadAllLines(app);

        var line =
            Array.FindIndex(lines, text => text.StartsWith(declaration + " : Program", StringComparison.Ordinal)) + 1;

        foreach (var source in new[] { entryPoint, app + "#L" + line })
        {
            using var reader = new StringReader("unused input\n");

            var rejected =
                Run(
                    source: source,
                    inputReader: reader,
                    timeout: TimeSpan.FromSeconds(60),
                    batchOnly: true);

            rejected.Exit.Should().Be(1, rejected.Error);
            rejected.Output.Should().BeEmpty();

            rejected.Error.Should().Contain("'pine apply' only supports batch functions.")
                .And.Contain("Use 'pine run'")
                .And.Contain(declaration + " : { init :")
                .And.NotContain("Application mode was selected")
                .And.NotContain("Starting Elm app")
                .And.NotContain("Input> ");

            reader.ReadLine().Should().Be("unused input");
        }

        foreach (var batchOnly in new[] { false, true })
        {
            var start =
                new ProcessStartInfo("dotnet")
                {
                    RedirectStandardInput = true,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                };

            start.ArgumentList.Add(typeof(RunCommand).Assembly.Location);
            start.ArgumentList.Add(batchOnly ? "apply" : "run");
            start.ArgumentList.Add("App.elm/" + declaration);
            start.ArgumentList.Add("--input-directory");
            start.ArgumentList.Add(_directory);
            start.ArgumentList.Add("--timeout");
            start.ArgumentList.Add("45");

            if (batchOnly)
            {
                start.ArgumentList.Add("--input");
                start.ArgumentList.Add("unexpected");
            }

            using var process = Process.Start(start)!;
            var output = process.StandardOutput.ReadToEndAsync();
            var error = process.StandardError.ReadToEndAsync();

            try
            {
                await process.StandardInput.WriteAsync("hello\n");
                process.StandardInput.Close();
                await process.WaitForExitAsync().WaitAsync(TimeSpan.FromSeconds(60));
                var errors = await error;

                if (batchOnly)
                {
                    process.ExitCode.Should().Be(1, errors);
                    (await output).Should().BeEmpty();

                    errors.Should().Contain("'pine apply' only supports batch functions.")
                        .And.Contain("Use 'pine run'")
                        .And.Contain(declaration + " : { init :")
                        .And.NotContain("app stderr")
                        .And.NotContain("Starting Elm app")
                        .And.NotContain("Input> ");
                }
                else
                {
                    process.ExitCode.Should().Be(7, errors);
                    (await output).Should().Be("hello\n");

                    errors.Should().Contain("app stderr").And.Contain("Starting Elm app")
                        .And.Contain("Application mode was selected because")
                        .And.Contain(declaration + " : { init :")
                        .And.NotContain("Input> ");
                }
            }
            finally
            {
                if (!process.HasExited)
                    process.Kill(entireProcessTree: true);
            }
        }
    }

    public void Dispose() => Directory.Delete(_directory, recursive: true);
}
