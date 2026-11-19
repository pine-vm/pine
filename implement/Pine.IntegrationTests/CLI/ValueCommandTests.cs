using AwesomeAssertions;
using Pine.CLI;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.PineLanguage;
using System;
using System.CommandLine;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using ElmCompileCommand = Pine.CLI.Elm.CompileCommand;

namespace Pine.IntegrationTests.CLI;

public sealed class ValueCommandTests : IDisposable
{
    private readonly string _directory =
        Path.Combine(Path.GetTempPath(), "pine-value-tests-" + Guid.NewGuid().ToString("N"));

    public ValueCommandTests() => Directory.CreateDirectory(_directory);

    private string PathFor(string name) => Path.Combine(_directory, name);

    private string WriteValue(string name, PineValue value)
    {
        var path = PathFor(name);
        using var buffer = new MemoryStream();
        PineTreeBinaryEncoding.Encode(buffer, value);
        File.WriteAllBytes(path, buffer.ToArray());
        return path;
    }

    private (string Path, FileTree Tree) WriteDirectory(bool empty)
    {
        var path = PathFor("input-directory");
        Directory.CreateDirectory(path);

        var files =
            empty
            ?
            []
            :
            new (string[] Path, byte[] Content)[]
            {
                (["z.pine"], [255, 0, 1]),
                (["nested", "empty.txt"], []),
                (["nested", "a.bin"], [2, 3]),
                ([".hidden"], [5])
            };

        foreach (var file in files)
        {
            var filePath = Path.Combine([path, .. file.Path]);
            Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);
            File.WriteAllBytes(filePath, file.Content);
        }

        return
            (path,
            FileTree.FromSetOfFilesWithStringPath(
                files.Select(file => (file.Path, (ReadOnlyMemory<byte>)file.Content))));
    }

    private (int Exit, string Output, string Error) Invoke(params string[] args)
    {
        var command =
            new RootCommand
            {
                ElmCommand.Create(),
                ValueCommand.Create()
            };

        using var output = new StringWriter();
        using var error = new StringWriter();
        var exit = command.Parse(args).Invoke(new InvocationConfiguration { Output = output, Error = error });
        return (exit, output.ToString(), error.ToString());
    }

    private string Compile(string declaration, string? text = null)
    {
        var source = PathFor("Program.elm");

        File.WriteAllText(
            source,
            text ??
            """
            module Program exposing (..)

            import String

            combine : String -> String -> String
            combine first second = first ++ ":" ++ second

            constant : Int
            constant = 42

            curried : String -> String -> String
            curried first = \second -> first ++ second

            partial : String -> String
            partial = combine "prefix"

            higherOrder : (String -> Int) -> String -> Int
            higherOrder function value = function value

            count : String -> Int
            count value = String.length value

            unreachable = Missing.value

            diverge : Int
            diverge = diverge
            """);

        var compiled = PathFor(declaration + ".pine");
        var result = Invoke("elm", "compile", source + "/" + declaration, "--output", compiled);
        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().BeEmpty();
        File.Exists(compiled).Should().BeTrue();
        return compiled;
    }

    [Fact]
    public void Value_commands_compile_inspect_and_apply_after_removing_sources()
    {
        var compiled = Compile("combine");
        var first = WriteValue("first.pine", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("left")));
        var second = WriteValue("second.pine", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("right")));
        var expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("left:right"));
        var inspection = Invoke("value", "inspect", compiled);
        inspection.Exit.Should().Be(0, inspection.Error);
        using var json = JsonDocument.Parse(inspection.Output);
        json.RootElement.GetProperty("arity").GetInt32().Should().Be(2);
        json.RootElement.GetProperty("isExpression").GetBoolean().Should().BeTrue();
        json.RootElement.TryGetProperty("fileTree", out _).Should().BeFalse();

        json.RootElement.GetProperty("hash").GetString().Should().Be(
            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(ValueCommand.Read(compiled)).Span));

        var originalBytes = File.ReadAllBytes(compiled);
        Compile("combine").Should().Be(compiled);
        File.ReadAllBytes(compiled).Should().Equal(originalBytes);
        File.Delete(PathFor("Program.elm"));

        var output = PathFor("result.pine");

        var result =
            Invoke(
                "value",
                "apply",
                compiled,
                "--argument-value",
                first,
                "--argument-value",
                second,
                "--output",
                output);

        result.Exit.Should().Be(0, result.Error);
        result.Output.Should().BeEmpty();
        ValueCommand.Read(output).Should().Be(expected);
    }

    [Theory]
    [InlineData("combine", "a:b")]
    [InlineData("curried", "ab")]
    public void Value_commands_preserve_curried_partial_application(string declaration, string expected)
    {
        var compiled = Compile(declaration);
        var a = WriteValue("a.pine", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("a")));
        var b = WriteValue("b.pine", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("b")));
        var partial = PathFor("partial.pine");
        var first = Invoke("value", "apply", compiled, "--argument-value", a, "--output", partial);
        first.Exit.Should().Be(0, first.Error);

        var information =
            PineValueApplication.Inspect(ValueCommand.Read(partial)).Extract(error => throw new Exception(error));

        information.Arity.Should().Be(1);
        var result = Invoke("value", "apply", partial, "--argument-value", b, "--output", partial);
        result.Exit.Should().Be(0, result.Error);
        ValueCommand.Read(partial).Should().Be(ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(expected)));
    }

    [Fact]
    public void Value_commands_apply_nullary_declarations_and_function_aliases()
    {
        var constant = Compile("constant");
        var output = PathFor("output.pine");
        var result = Invoke("value", "apply", constant, "--output", output);
        result.Exit.Should().Be(0, result.Error);
        ValueCommand.Read(output).Should().Be(IntegerEncoding.EncodeSignedInteger(42));

        var alias = Compile("partial");

        var argument =
            WriteValue("argument.pine", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("suffix")));

        result = Invoke("value", "apply", alias, "--argument-value", argument, "--output", output);
        result.Exit.Should().Be(0, result.Error);

        ValueCommand.Read(output).Should().Be(
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("prefix:suffix")));

        var data = Invoke("value", "inspect", output);
        data.Exit.Should().Be(0, data.Error);
        using var json = JsonDocument.Parse(data.Output);
        json.RootElement.GetProperty("arity").GetInt32().Should().Be(0);
    }

    [Fact]
    public void Value_commands_compile_without_evaluating_divergent_values()
    {
        var compiled = Compile("diverge");
        var result = Invoke("value", "inspect", compiled);
        result.Exit.Should().Be(0, result.Error);
        using var json = JsonDocument.Parse(result.Output);
        json.RootElement.GetProperty("arity").GetInt32().Should().Be(0);
    }

    [Fact]
    public void Value_commands_accept_binary_function_arguments_without_Elm_type_conversion()
    {
        var caller = Compile("higherOrder");
        var callback = Compile("count");
        var text = WriteValue("text.pine", ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("hello")));
        File.Delete(PathFor("Program.elm"));
        var output = PathFor("output.pine");

        var result =
            Invoke(
                "value",
                "apply",
                caller,
                "--argument-value",
                callback,
                "--argument-value",
                text,
                "--output",
                output);

        result.Exit.Should().Be(0, result.Error);
        ValueCommand.Read(output).Should().Be(IntegerEncoding.EncodeSignedInteger(5));
    }

    [Fact]
    public void Value_commands_compile_project_sibling_sources_and_line_selection()
    {
        var project = PathFor("project");
        var shared = PathFor("shared");
        Directory.CreateDirectory(project);
        Directory.CreateDirectory(shared);

        File.WriteAllText(
            Path.Combine(project, "elm.json"),
            """
            {
                "type": "application",
                "source-directories": [".", "../shared"],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": { "elm/core": "1.0.5" },
                    "indirect": {}
                },
                "test-dependencies": { "direct": {}, "indirect": {} }
            }
            """);

        File.WriteAllText(
            Path.Combine(project, "Program.elm"),
            "module Program exposing (selected)\nimport Shared\nselected _ = Shared.answer\n");

        File.WriteAllText(
            Path.Combine(shared, "Shared.elm"),
            "module Shared exposing (answer)\nanswer = 123\n");

        var output = PathFor("compiled.pine");

        var result =
            Invoke(
                "elm",
                "compile",
                "Program.elm#L3",
                "--input-directory",
                project,
                "--output",
                output);

        result.Exit.Should().Be(0, result.Error);
        var unit = WriteValue("unit.pine", PineValue.EmptyList);
        var applied = Invoke("value", "apply", output, "--argument-value", unit, "--output", output);
        applied.Exit.Should().Be(0, applied.Error);
        ValueCommand.Read(output).Should().Be(IntegerEncoding.EncodeSignedInteger(123));
    }

    [Fact]
    public async Task Value_commands_fresh_processes_keep_stdout_clean_and_need_no_sources()
    {
        var source = PathFor("Fresh.elm");
        File.WriteAllText(source, "module Fresh exposing (identity)\nidentity value = value\n");
        var compiled = PathFor("fresh.pine");
        var compilation = await RunProcess("elm", "compile", source + "/identity", "--output", compiled);
        compilation.Exit.Should().Be(0, compilation.Error);
        compilation.Output.Should().BeEmpty();
        File.Delete(source);
        var inspect = await RunProcess("value", "inspect", compiled);
        inspect.Exit.Should().Be(0, inspect.Error);
        using var json = JsonDocument.Parse(inspect.Output);
        json.RootElement.GetProperty("arity").GetInt32().Should().Be(1);
        var argument = WriteValue("input.pine", PineValue.Blob([0, 1, 255]));
        var output = PathFor("output.pine");

        var apply =
            await RunProcess(
                "value",
                "apply",
                compiled,
                "--argument-value",
                argument,
                "--output",
                output);

        apply.Exit.Should().Be(0, apply.Error);
        apply.Output.Should().BeEmpty();
        ValueCommand.Read(output).Should().Be(ValueCommand.Read(argument));
    }

    private async Task<(int Exit, string Output, string Error)> RunProcess(params string[] arguments)
    {
        var start =
            new ProcessStartInfo("dotnet")
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                WorkingDirectory = _directory
            };

        start.ArgumentList.Add(typeof(ValueCommand).Assembly.Location);

        foreach (var argument in arguments)
            start.ArgumentList.Add(argument);

        using var process = Process.Start(start) ?? throw new InvalidOperationException("Failed starting Pine.");
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(60));
        var output = process.StandardOutput.ReadToEndAsync();
        var error = process.StandardError.ReadToEndAsync();

        try
        {
            await process.WaitForExitAsync(cancellation.Token);
            return (process.ExitCode, await output, await error);
        }
        finally
        {
            if (!process.HasExited)
                process.Kill(entireProcessTree: true);
        }
    }

    [Fact]
    public void Value_commands_accept_raw_expressions_and_non_expression_data()
    {
        var identity =
            WriteValue("identity.pine", ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance));

        var data = WriteValue("data.pine", PineValue.Blob([7, 8, 9]));
        var inspection = Invoke("value", "inspect", identity);
        inspection.Exit.Should().Be(0, inspection.Error);
        using var json = JsonDocument.Parse(inspection.Output);
        json.RootElement.GetProperty("arity").GetInt32().Should().Be(1);
        var output = PathFor("output.pine");
        Invoke("value", "apply", identity, "--argument-value", data, "--output", output).Exit.Should().Be(0);
        File.ReadAllBytes(output).Should().Equal(File.ReadAllBytes(data));
        Invoke("value", "apply", data, "--output", output).Exit.Should().Be(0);
        File.ReadAllBytes(output).Should().Equal(File.ReadAllBytes(data));
        var failure = Invoke("value", "apply", data, "--argument-value", data, "--output", output);
        failure.Exit.Should().NotBe(0);
        failure.Error.Should().Contain("not a Pine expression");
        File.ReadAllBytes(output).Should().Equal(File.ReadAllBytes(data));
    }

    [Theory]
    [InlineData(false, 4, 6)]
    [InlineData(true, 0, 0)]
    public void Value_commands_inspect_directory_mapping_with_statistics(bool empty, int fileCount, long totalBytes)
    {
        var input = WriteDirectory(empty);
        var expected = FileTreeEncoding.Encode(input.Tree);
        var inspected = Invoke("value", "inspect", input.Path);
        inspected.Exit.Should().Be(0, inspected.Error);

        inspected.Error.Should().Contain("Mapped directory")
            .And.Contain("FileTreeEncoding")
            .And.Contain($"{fileCount} files, {totalBytes} bytes");

        using var json = JsonDocument.Parse(inspected.Output);
        json.RootElement.GetProperty("arity").GetInt32().Should().Be(0);

        json.RootElement.GetProperty("hash").GetString().Should().Be(
            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(expected).Span));

        var metadata = json.RootElement.GetProperty("fileTree");
        metadata.GetProperty("path").GetString().Should().Be(input.Path);
        metadata.GetProperty("encoding").GetString().Should().Be("FileTreeEncoding");
        metadata.GetProperty("fileCount").GetInt32().Should().Be(fileCount);
        metadata.GetProperty("totalBytes").GetInt64().Should().Be(totalBytes);

        var identity =
            WriteValue("identity.pine", ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance));

        var output = PathFor("tree.pine");
        var applied = Invoke("value", "apply", identity, "--argument-value", input.Path, "--output", output);
        applied.Exit.Should().Be(0, applied.Error);
        applied.Output.Should().BeEmpty();
        applied.Error.Should().Contain("FileTreeEncoding").And.Contain($"{fileCount} files, {totalBytes} bytes");
        ValueCommand.Read(output).Should().Be(expected);

        FileTreeEncoding.Parse(ValueCommand.Read(output)).Extract(_ => throw new Exception("Invalid file tree"))
            .Should().Be(input.Tree);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Value_commands_directory_arguments_preserve_order_with_binary_arguments(bool directoryFirst)
    {
        var input = WriteDirectory(empty: false);
        var tree = FileTreeEncoding.Encode(input.Tree);
        var data = PineValue.Blob([42]);
        var dataPath = WriteValue("data.pine", data);

        var record =
            new FunctionRecord(
                Expression.EnvironmentInstance,
                2,
                ReadOnlyMemory<PineValue>.Empty,
                ReadOnlyMemory<PineValue>.Empty,
                UsesNestedArgFormat: true);

        var function = WriteValue("function.pine", FunctionRecord.EncodeFunctionRecordInValueTagged(record));
        var output = PathFor("result.pine");

        var applied =
            Invoke(
                "value",
                "apply",
                function,
                "--argument-value",
                directoryFirst ? input.Path : dataPath,
                "--argument-value",
                directoryFirst ? dataPath : input.Path,
                "--output",
                output);

        applied.Exit.Should().Be(0, applied.Error);
        applied.Output.Should().BeEmpty();
        applied.Error.Should().Contain("FileTreeEncoding").And.Contain("4 files, 6 bytes");

        ValueCommand.Read(output).Should().Be(
            PineValue.List(
                [
                    PineValue.EmptyList,
                    PineValue.List(directoryFirst ? [tree, data] : [data, tree])
                ]));
    }

    [Fact]
    public void Value_commands_preserve_collected_arguments_in_function_records()
    {
        var data = WriteValue("data.pine", PineValue.Blob([42]));
        var output = PathFor("output.pine");

        var record =
            new FunctionRecord(
                Expression.EnvironmentInstance,
                2,
                ReadOnlyMemory<PineValue>.Empty,
                ReadOnlyMemory<PineValue>.Empty,
                UsesNestedArgFormat: true);

        var function = WriteValue("tagged.pine", FunctionRecord.EncodeFunctionRecordInValueTagged(record));
        var partial = PathFor("partial.pine");
        Invoke("value", "apply", function, "--argument-value", data, "--output", partial).Exit.Should().Be(0);

        PineValueApplication.Inspect(ValueCommand.Read(partial)).Extract(error => throw new Exception(error))
            .Arity.Should().Be(1);

        var applied = Invoke("value", "apply", partial, "--argument-value", data, "--output", output);
        applied.Exit.Should().Be(0, applied.Error);

        ValueCommand.Read(output).Should().Be(
            PineValue.List([PineValue.EmptyList, PineValue.List([ValueCommand.Read(data), ValueCommand.Read(data)])]));
    }

    [Theory]
    [InlineData("truncated")]
    [InlineData("trailing")]
    [InlineData("external")]
    [InlineData("eval")]
    public void Value_commands_reject_invalid_or_non_standalone_encoding_without_writing(string kind)
    {
        var file = WriteValue("invalid.pine", PineValue.EmptyList);
        var bytes = File.ReadAllBytes(file);

        File.WriteAllBytes(
            file,
            kind switch
            {
                "truncated" => bytes[..^1],
                "trailing" => [.. bytes, 0],
                "external" => [0, 0, 0, 5, .. new byte[32]],
                "eval" => [0, 0, 0, 6, .. bytes, .. bytes],

                _ =>
                throw new NotImplementedException(kind)
            });

        var output = PathFor("preserved.pine");
        File.WriteAllText(output, "unchanged");
        var inspect = Invoke("value", "inspect", file);
        inspect.Exit.Should().NotBe(0);
        inspect.Output.Should().BeEmpty();
        inspect.Error.Should().Contain("Error:");
        var apply = Invoke("value", "apply", file, "--output", output);
        apply.Exit.Should().NotBe(0);
        apply.Error.Should().Contain("Error:");
        File.ReadAllText(output).Should().Be("unchanged");
    }

    [Fact]
    public void Value_commands_compilation_failure_preserves_output_and_requires_selection()
    {
        Compile("constant");
        var source = PathFor("Program.elm");
        var output = PathFor("preserved.pine");
        File.WriteAllText(output, "unchanged");

        foreach (var selection in new[] { source, source + "/missing", source + "/unreachable" })
        {
            var result = Invoke("elm", "compile", selection, "--output", output);
            result.Exit.Should().NotBe(0);
            result.Error.Should().Contain("Error:");
            result.Output.Should().BeEmpty();
            File.ReadAllText(output).Should().Be("unchanged");
        }
    }

    [Fact]
    public void Value_commands_timeout_and_cancellation_preserve_output()
    {
        var source = PathFor("Loop.elm");
        File.WriteAllText(source, "module Loop exposing (loop)\nloop value = loop value\n");
        var compiled = PathFor("loop.pine");
        using var error = new StringWriter();
        ElmCompileCommand.Execute(source + "/loop", compiled, error).Should().Be(0, error.ToString());
        var argument = WriteValue("argument.pine", IntegerEncoding.EncodeSignedInteger(1));
        var output = PathFor("output.pine");
        File.WriteAllText(output, "unchanged");

        ValueCommand.Apply(compiled, [argument], output, error, TimeSpan.FromSeconds(1)).Should().Be(
            124,
            error.ToString());

        File.ReadAllText(output).Should().Be("unchanged");
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();
        ValueCommand.Apply(compiled, [argument], output, error, cancellationToken: cancellation.Token).Should().Be(130);

        ElmCompileCommand.Execute(source + "/loop", output, error, cancellationToken: cancellation.Token).Should().Be(
            130);

        File.ReadAllText(output).Should().Be("unchanged");
    }

    [Fact]
    public void Value_commands_cli_requires_binary_output_and_validates_repeated_arguments()
    {
        var value = ValueCommand.Create();
        value.Subcommands.Single(command => command.Name == "inspect").Parse([]).Errors.Should().NotBeEmpty();
        var apply = value.Subcommands.Single(command => command.Name == "apply");
        apply.Parse(["--output", "result"]).Errors.Should().NotBeEmpty();
        apply.Parse(["function.pine"]).Errors.Should().NotBeEmpty();
        apply.Parse(["function.pine", "--argument-value", "a", "b", "--output", "result"]).Errors.Should().NotBeEmpty();

        apply.Parse(["function.pine", "--argument-value", "a", "--argument-value", "b", "--output", "result"])
            .Errors.Should().BeEmpty();

        var compile = ElmCompileCommand.Create();
        compile.Parse(["--output", "result"]).Errors.Should().NotBeEmpty();
        compile.Parse(["Program.elm/value"]).Errors.Should().NotBeEmpty();

        foreach (var timeout in new[] { "0", "-1", "NaN", "Infinity" })
        {
            apply.Parse(["function.pine", "--output", "result", "--timeout", timeout]).Errors.Should().NotBeEmpty();
            compile.Parse(["Program.elm/value", "--output", "result", "--timeout", timeout]).Errors.Should().NotBeEmpty();
        }
    }

    public void Dispose()
    {
        if (Directory.Exists(_directory))
            Directory.Delete(_directory, recursive: true);
    }
}
