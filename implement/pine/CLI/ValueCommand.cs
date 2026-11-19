using Pine.Core.Addressing;
using Pine.Core.CLI;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Testing;
using Pine.Core.Encodings;
using Pine.Core.IO;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;

namespace Pine.CLI;

public static class ValueCommand
{
    private sealed record FileTreeInput(string Path, int FileCount, long TotalBytes)
    {
        public string Encoding => nameof(FileTreeEncoding);
    }

    public static Command Create() =>
        new("value", "Inspect and apply binary Pine values without compiling Elm source.")
        {
            CreateInspect(),
            CreateApply()
        };

    private static Command CreateInspect()
    {
        var command =
            new Command(
                "inspect",
                "Inspect a binary Pine value file or a directory mapped with FileTreeEncoding, without evaluation.");

        var value = PineCliCommand.RequiredArgument("path");
        command.Add(value);

        command.SetAction(
            result => Inspect(
                result.GetRequiredValue(value),
                result.InvocationConfiguration.Output,
                result.InvocationConfiguration.Error));

        return command;
    }

    private static Command CreateApply()
    {
        var command = new Command("apply", "Apply binary Pine arguments and write the resulting raw binary value.");
        var value = PineCliCommand.RequiredArgument("file");

        var arguments =
            new Option<string[]>("--argument-value")
            {
                Description =
                "Binary Pine argument file or directory mapped with FileTreeEncoding. Repeat in parameter order; partial application is supported.",
                AllowMultipleArgumentsPerToken = false,
                Arity = ArgumentArity.OneOrMore
            };

        var output = OutputOption();
        var timeout = TimeoutOption();
        command.Add(value);
        command.Add(arguments);
        command.Add(output);
        command.Add(timeout);

        arguments.Validators.Add(
            result =>
            {
                if (result.Tokens.Count != result.IdentifierTokenCount)
                    result.AddError("--argument-value requires exactly one path per occurrence.");
            });

        command.SetAction(
            result => Apply(
                result.GetRequiredValue(value),
                result.GetValue(arguments) ?? [],
                result.GetRequiredValue(output),
                result.InvocationConfiguration.Error,
                result.GetValue(timeout) is { } seconds ? TimeSpan.FromSeconds(seconds) : null));

        return command;
    }

    internal static Option<string> OutputOption() =>
        new("--output")
        {
            Description = "Destination for one raw binary Pine value; atomically replace an existing file.",
            Required = true
        };

    internal static Option<double?> TimeoutOption()
    {
        var option =
            new Option<double?>("--timeout")
            {
                Description = "Cooperative whole-command timeout; default unit is seconds."
            };

        NumericOptionParsing.SetTimeParser(
            option,
            TimeUnit.Seconds,
            seconds => seconds is { } value &&
                (!double.IsFinite(value) || value <= 0 || value * 1000 > uint.MaxValue - 1)
            ?
            "--timeout must be finite, positive, and fit the timer range."
            :
            null);

        return option;
    }

    internal static PineValue Read(string path)
    {
        using var decoder = new PineTreeBinaryEncoding.TreeDecoder<PineValue>(File.ReadAllBytes(path));
        var value = decoder.Read<PineValue, PineTreeBinaryEncoding.PineDecoding<PineValue>>(default);

        if (decoder.Remaining != 0)
            throw new InvalidDataException("Trailing bytes after the binary Pine value in '" + path + "'.");

        return value;
    }

    private static (PineValue Value, FileTreeInput? FileTree) ReadInput(
        string path, TextWriter diagnostics, CancellationToken token = default)
    {
        token.ThrowIfCancellationRequested();

        if (!Directory.Exists(path))
            return (Read(path), null);

        var tree =
            LoadFromLocalFilesystem.LoadSortedTreeFromPath(path)
            ?? throw new IOException("Cannot load directory '" + path + "'.");

        var fileCount = 0;
        var totalBytes = 0L;

        foreach (var file in tree.EnumerateFilesTransitive())
        {
            fileCount++;
            totalBytes += file.fileContent.Length;
        }

        token.ThrowIfCancellationRequested();
        var value = FileTreeEncoding.Encode(tree);
        token.ThrowIfCancellationRequested();

        diagnostics.WriteLine(
            $"Mapped directory '{path}' with FileTreeEncoding: {fileCount} files, {totalBytes} bytes.");

        return (value, new(path, fileCount, totalBytes));
    }

    internal static void Write(string path, PineValue value, CancellationToken token)
    {
        using var buffer = new MemoryStream();
        PineTreeBinaryEncoding.Encode(buffer, value);
        token.ThrowIfCancellationRequested();
        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath(path))!);
        AtomicFile.WriteAllBytes(path, buffer.ToArray(), token);
    }

    internal static int Inspect(string path, TextWriter output, TextWriter error)
    {
        try
        {
            var (value, fileTree) = ReadInput(path, error);

            var information =
                PineValueApplication.Inspect(value)
                .Extract(message => throw new InvalidDataException(message));

            output.WriteLine(
                JsonSerializer.Serialize(
                    new
                    {
                        arity = information.Arity,
                        isExpression = information.IsExpression,
                        hash = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(value).Span),
                        fileTree
                    },
                    new JsonSerializerOptions
                    {
                        WriteIndented = true,
                        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                        DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull
                    }));

            return 0;
        }
        catch (Exception exception) when (IsInputError(exception))
        {
            error.WriteLine("Error: " + exception.Message);
            return 1;
        }
    }

    internal static int Apply(
        string path,
        IReadOnlyList<string> argumentFiles,
        string output,
        TextWriter error,
        TimeSpan? timeout = null,
        CancellationToken cancellationToken = default)
    {
        using var tracker =
            new ElmTestInstrumentation(
                new() { Timeout = timeout, DisablePrecompiledLeaves = true },
                recordDiagnostics: false);

        using var cancellation = cancellationToken.Register(tracker.Cancel);
        using var consoleCancellation = Elm.TestBudgetOptions.RegisterCancellation(tracker);
        var token = tracker.CancellationToken;

        try
        {
            token.ThrowIfCancellationRequested();
            var value = Read(path);
            var arguments = argumentFiles.Select(argument => ReadInput(argument, error, token).Value).ToArray();
            var caches = new PineVMSharedCaches();

            var vm =
                tracker.CreateVm(
                    new InvocationCacheAccessFromDictionary(new InvocationCache()),
                    caches);

            using var execution = tracker.EnterScope("execution", path);

            var result =
                PineValueApplication.Apply(vm, value, arguments, token, caches.ParsedExpressions)
                .Extract(message => throw new InvalidOperationException(message));

            token.ThrowIfCancellationRequested();
            Write(output, result, token);
            return 0;
        }
        catch (Exception) when (token.IsCancellationRequested)
        {
            error.WriteLine(
                tracker.CancelledByUser ? "Application cancelled." : "Application timed out (cooperative timeout).");

            return tracker.CancelledByUser ? 130 : 124;
        }
        catch (Exception exception) when (IsInputError(exception))
        {
            error.WriteLine("Error: " + exception.Message);
            return 1;
        }
    }

    internal static bool IsInputError(Exception exception) =>
        exception is IOException or InvalidDataException or UnauthorizedAccessException or ArgumentException or
            InvalidOperationException or PineTreeBinaryEncoding.DecodeException;
}
