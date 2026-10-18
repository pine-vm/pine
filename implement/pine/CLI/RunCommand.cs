using Pine.Core;
using Pine.Core.CLI;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.Testing;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.IO;
using Pine.Core.PineVM;
using Pine.Elm.Platform;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.CommandLine;
using System.CommandLine.Help;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;

using Type = Pine.Core.Elm.ElmCompilerInDotnet.TypeInference.InferredType;

namespace Pine.CLI;

public static class RunCommand
{
    public static Command Create(bool batchOnly = false)
    {
        var command =
            new Command(
                batchOnly ? "apply" : "run",
                batchOnly
                ?
                "Apply an Elm function to one input; reject interactive applications."
                :
                "Run an Elm application or apply an Elm function to one input.")
            {
                new HelpOption()
            };

        var entryPointArgument = PineCliCommand.RequiredArgument("entry-point");

        entryPointArgument.Description =
            "Elm declaration (.elm/name or .elm#Lnumber), or a module to choose a declaration interactively. No default names.";

        var inputDirectoryOption =
            new Option<string?>("--input-directory") { Description = "Source directory for a relative local entry point." };

        var inputOption =
            new Option<string?>("--input") { Description = "Input Elm literal, list, tuple, record, or choice constructor. String quotes may be omitted." };

        var inputFileOption =
            new Option<string?>("--input-file") { Description = "Input file path or GitHub/GitLab file URL." };

        var outputFileOption =
            new Option<string?>("--output-file") { Description = "Write the result to this local file instead of stdout; overwrite an existing file." };

        var transformFileOption =
            new Option<string?>("--transform-file") { Description = "Read and replace the same local file." };

        var colorOption = FormatCommandShared.CreateColorOption();

        var durationsOption =
            new Option<bool>("--report-durations") { Description = "Report compilation and execution durations on stderr." };

        var timeoutOption =
            new Option<double?>("--timeout") { Description = "Cooperative whole-command timeout; default unit is seconds." };

        NumericOptionParsing.SetTimeParser(
            timeoutOption,
            TimeUnit.Seconds,
            value =>
            value is { } seconds && (!double.IsFinite(seconds) || seconds <= 0 || seconds * 1000 > uint.MaxValue - 1)
            ?
            "--timeout must be finite, positive, and fit the timer range."
            :
            null);

        command.Add(entryPointArgument);

        foreach (var option in new Option[] { inputDirectoryOption, inputOption, inputFileOption, outputFileOption, transformFileOption, colorOption, durationsOption, timeoutOption })
        {
            command.Add(option);

            option.Validators.Add(
                result =>
                {
                    if (result.IdentifierTokenCount > 1)
                        result.AddError(option.Name + " can only be specified once.");
                });
        }

        command.SetAction(
            result => Execute(
                result.GetRequiredValue(entryPointArgument),
                result.GetValue(inputDirectoryOption),
                result.GetValue(inputOption),
                result.GetValue(inputFileOption),
                result.GetValue(outputFileOption),
                result.GetValue(transformFileOption),
                result.GetValue(colorOption),
                result.GetValue(durationsOption),
                result.GetValue(timeoutOption) is { } seconds ? TimeSpan.FromSeconds(seconds) : null,
                result.InvocationConfiguration.Output,
                result.InvocationConfiguration.Error,
                batchOnly: batchOnly));

        return command;
    }

    internal static int Execute(
        string entryPoint,
        string? inputDirectory = null,
        string? input = null,
        string? inputFile = null,
        string? outputFile = null,
        string? transformFile = null,
        CommandLineColorMode? color = null,
        bool reportDurations = false,
        TimeSpan? timeout = null,
        TextWriter? output = null,
        TextWriter? error = null,
        string? cacheDirectory = null,
        RunSource.Loader? sourceLoader = null,
        CancellationToken cancellationToken = default,
        TextReader? inputReader = null,
        bool batchOnly = false,
        Func<IReadOnlyList<RunDeclarationSelection.Candidate>, string>? selectDeclaration = null)
    {
        output ??= Console.Out;
        error ??= Console.Error;
        cacheDirectory ??= Filesystem.CacheDirectory;

        using var branchResolution =
            GitBranchResolutionCache.BeginScope(
                GitBranchResolutionCache.Active ?? new(cacheDirectory));

        using var tracker =
            new ElmTestInstrumentation(
                new() { Timeout = timeout },
                recordDiagnostics: false);

        using var cancellation = cancellationToken.Register(tracker.Cancel);
        using var consoleCancellation = CLI.Elm.TestBudgetOptions.RegisterCancellation(tracker);
        var token = tracker.CancellationToken;
        var stopwatch = Stopwatch.StartNew();
        var compilationDuration = TimeSpan.Zero;
        var executionStarted = false;

        try
        {
            color =
                FormatCommandShared.ResolveColorMode(
                    color,
                    Environment.GetEnvironmentVariable(FormatCommandShared.ColorEnvironmentVariable));

            var console = FormatCommandShared.CreateSystemConsole(error, color.Value);

            if (transformFile is not null)
            {
                if (input is not null || inputFile is not null || outputFile is not null)
                {
                    throw new ArgumentException(
                        "--transform-file cannot be combined with --input, --input-file, or --output-file.");
                }

                if (RunSource.IsRemote(transformFile))
                    throw new ArgumentException("--transform-file requires a local file.");

                inputFile = outputFile = transformFile;
            }

            if (input is not null && inputFile is not null)
                throw new ArgumentException("--input and --input-file are mutually exclusive.");

            if (outputFile is not null && RunSource.IsRemote(outputFile))
                throw new ArgumentException("--output-file requires a local path.");

            sourceLoader ??= new RunSource.Loader(cacheDirectory, error);
            RunCompilation compiled;

            using (tracker.EnterScope("compilation", entryPoint))
                compiled =
                    RunCompilation.Compile(
                        sourceLoader.Read(entryPoint, inputDirectory, token),
                        cacheDirectory,
                        error,
                        token,
                        selectDeclaration ??
                        (candidates =>
                        RunDeclarationSelection.Select(
                            candidates,
                            RunDeclarationSelection.CreateConsole(error, color.Value),
                            Console.IsInputRedirected,
                            batchOnly,
                            token)));

            compilationDuration = stopwatch.Elapsed;
            executionStarted = true;
            using var execution = tracker.EnterScope("execution", entryPoint);
            var caches = new PineVMSharedCaches();
            var invocations = new InvocationCache();
            var vm = tracker.CreateVm(new InvocationCacheAccessFromDictionary(invocations), caches);

            var environment =
                ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiled.Environment)
                .Extract(message => throw new InvalidDataException(message));

            var wrapper =
                environment.Modules.Single(module => module.moduleName == compiled.ModuleName)
                .moduleContent.FunctionDeclarations[compiled.Declaration];

            if (compiled.Types.Type is Type.RecordType record &&
                record.Fields.Any(field => field is { FieldName: "init", FieldType: Type.FunctionType }) &&
                record.Fields.Any(field => field is { FieldName: "subscriptions", FieldType: Type.FunctionType }))
            {
                if (batchOnly)
                {
                    throw DeclarationError(
                        compiled,
                        "The selected declaration models an interactive application. " +
                        "'pine apply' only supports batch functions. Use 'pine run' to start this application.");
                }

                error.WriteLine(
                    "Application mode was selected because the declaration is a record with init and subscriptions functions.");

                RunDeclarationSelection.WriteInferredSignature(console, compiled.DeclarationSignature);

                if (input is not null || inputFile is not null || outputFile is not null)
                    throw new ArgumentException("Batch input/output options cannot be used with an application.");

                var selectedValue =
                    ElmSourceCompilation.EvaluateZeroParameterRoot(wrapper, vm, caches.ParsedExpressions)
                    .Extract(message => throw new InvalidOperationException(message));

                return
                    RunElmAppOnCommandLine(
                        CommandLineAppConfig.ConfigFromDeclarationValue(selectedValue),
                        vm,
                        invocations,
                        token,
                        error);
            }

            if (compiled.Types.Type is not Type.FunctionType function || function.ReturnType is Type.FunctionType)
            {
                throw DeclarationError(
                    compiled,
                    "Select a function taking one input and returning a non-function value" +
                    (batchOnly ? ". " : ", or a command-line application. ") +
                    "Add an explicit type annotation if inference is insufficient. Web services use 'pine run-server'.");
            }

            error.WriteLine(
                "Batch mode was selected because the declaration takes one input and returns a non-function value.");

            RunDeclarationSelection.WriteInferredSignature(console, compiled.DeclarationSignature);

            if (input is null && inputFile is null)
            {
                error.WriteLine(
                    "Supply batch input using --input, --input-file, or --transform-file, or enter it below.");

                error.Write("Input> ");
                error.Flush();

                using var standardInput =
                    inputReader is null
                    ?
                    new StreamReader(Console.OpenStandardInput(), Console.InputEncoding)
                    :
                    null;

                // Console streams may not interrupt an in-flight read when its token is cancelled.
                input =
                    (inputReader ?? standardInput!).ReadLineAsync(token).AsTask()
                    .WaitAsync(token).GetAwaiter().GetResult();

                error.WriteLine();
                token.ThrowIfCancellationRequested();

                if (input is null)
                {
                    throw new ArgumentException(
                        "No batch input was received (end of input). Supply --input, --input-file, or --transform-file.");
                }
            }

            ElmValue argument;
            RunValues.TextEncoding? encoding = null;

            if (inputFile is not null)
            {
                (argument, encoding) =
                    RunValues.FromFile(sourceLoader.ReadInput(inputFile, token), function.ArgumentType);
            }
            else
                argument = RunValues.ParseInput(input!, function.ArgumentType);

            RunValues.Validate(argument, function.ArgumentType, compiled.Types);
            token.ThrowIfCancellationRequested();

            var callable =
                FunctionRecord.ParseFunctionRecordTagged(wrapper, caches.ParsedExpressions)
                .Extract(message => throw new InvalidOperationException(message));

            if (callable.ParameterCount is 0)
            {
                var functionValue =
                    ElmSourceCompilation.EvaluateZeroParameterRoot(wrapper, vm, caches.ParsedExpressions)
                    .Extract(message => throw new InvalidOperationException(message));

                callable =
                    FunctionRecord.ParseFunctionRecordTagged(functionValue, caches.ParsedExpressions)
                    .Extract(message => throw new InvalidOperationException(message));
            }

            var result =
                ElmInteractiveEnvironment.ApplyFunction(
                    vm,
                    callable,
                    [
                    ElmValueEncoding.ElmValueAsPineValue(argument)
                    ],
                    token)
                .Extract(message => throw new InvalidOperationException(message));

            token.ThrowIfCancellationRequested();

            if (FunctionRecord.ParseFunctionRecordTagged(result, caches.ParsedExpressions).IsOkOrNull() is { ParameterCount: > 0 })
            {
                throw new ArgumentException(
                    "The selected function returned another function; only one-input batch functions are supported.");
            }

            var value =
                ElmValueEncoding.PineValueAsElmValue(result, null, null)
                .Extract(message => throw new InvalidOperationException(message));

            if (outputFile is not null)
                AtomicFile.WriteAllBytes(outputFile, RunValues.ToFile(value, encoding), token);

            else
            {
                var text = RunValues.Render(value);
                token.ThrowIfCancellationRequested();
                output.WriteLine(text);
            }

            return 0;
        }
        catch (Exception) when (token.IsCancellationRequested)
        {
            tracker.NotifyCancellation();
            error.WriteLine(tracker.CancelledByUser ? "Run cancelled." : "Run timed out (cooperative timeout).");
            var counters = tracker.GetSummary().Counters;

            if (counters != default)
                error.WriteLine(EvalWorkCountersFormatting.FormatCounts(counters));

            return tracker.CancelledByUser ? 130 : 124;
        }
        catch (Exception exception) when (exception is ArgumentException or IOException or InvalidDataException or UnauthorizedAccessException or
            ElmDependencyResolutionException or Core.Elm.ElmCompilerInDotnet.ElmCompilationException or InvalidOperationException)
        {
            var console = FormatCommandShared.CreateSystemConsole(error, color ?? CommandLineColorMode.Auto);
            FormatCommandShared.WriteLabelledLine(console, "Error:", FormatCommandTheme.Error, exception.Message);

            if (exception is DeclarationException declaration)
                RunDeclarationSelection.WriteInferredSignature(console, declaration.Signature);

            return 1;
        }
        catch (Exception exception)
        {
            error.WriteLine("Failed to run with runtime exception: " + exception);
            return 1;
        }
        finally
        {
            if (reportDurations)
            {
                error.WriteLine(
                    "compilation: " +
                    (executionStarted ? compilationDuration : stopwatch.Elapsed).TotalSeconds.ToString(
                        "F3",
                        System.Globalization.CultureInfo.InvariantCulture) +
                    " s");

                if (executionStarted)
                {
                    error.WriteLine(
                        "execution: " +
                        (stopwatch.Elapsed - compilationDuration).TotalSeconds.ToString(
                            "F3",
                            System.Globalization.CultureInfo.InvariantCulture) +
                        " s");
                }
            }
        }
    }

    private static DeclarationException DeclarationError(RunCompilation compiled, string message) =>
        new(message, compiled.DeclarationSignature);

    private sealed class DeclarationException(string message, string signature) : ArgumentException(message)
    {
        public string Signature { get; } = signature;
    }

    private static int RunElmAppOnCommandLine(
        CommandLineAppConfig appConfig, IPineVM vm, InvocationCache invocations, CancellationToken token, TextWriter diagnostics)
    {
        var envVarDict = Environment.GetEnvironmentVariables();

        var environmentVariables =
            envVarDict.Keys.OfType<string>()
            .Select(
                envVarKey => new KeyValuePair<string, string>(
                    envVarKey,
                    envVarDict[envVarKey]?.ToString() ?? string.Empty))
            .ToImmutableArray();

        diagnostics.WriteLine("Starting Elm app using runtime version " + PineCliCommand.AppVersionId + " ...");

        var mutatingCliApp =
            new MutatingCommandLineApp(
                appConfig,
                environment:
                new CommandLineAppConfig.CommandLineAppInitEnvironment(
                    CommandLine: Environment.CommandLine,
                    EnvironmentVariables: environmentVariables),
                pineVM: vm);

        // using var standardInput = Console.OpenStandardInput();
        using var standardOutput = Console.OpenStandardOutput();
        using var standardError = Console.OpenStandardError();

        void ProcessStandardInput(ReadOnlyMemory<byte> bytes)
        {
            token.ThrowIfCancellationRequested();

            if (bytes.Span.Length is not 0)
            {
                var appEventResponse = mutatingCliApp.EventStdIn(bytes);
            }

            if (invocations.Count >= 10_000)
                invocations.Clear();

            foreach (var outputItem in mutatingCliApp.DequeueStdOut())
            {
                standardOutput.Write(outputItem.Span);
            }

            standardOutput.Flush();

            foreach (var outputItem in mutatingCliApp.DequeueStdErr())
            {
                standardError.Write(outputItem.Span);
            }

            standardError.Flush();
        }

        ProcessStandardInput(ReadOnlyMemory<byte>.Empty);

        if (mutatingCliApp.ExitCode is { } initExitCode)
        {
            return initExitCode;
        }

        var buffer = new byte[0x100_000];

        /*
         * When the standard input has been redirected (for example when this process
         * is launched as a subprocess by another program or by an integration test),
         * `Console.ReadKey` throws `InvalidOperationException`. In that case we read
         * raw bytes from the standard-input stream instead. The `Console.ReadKey`
         * path is preserved for the interactive terminal case where, on Windows,
         * reading from the standard-input stream blocks until a line break.
         * */

        if (Console.IsInputRedirected)
        {
            using var standardInput = Console.OpenStandardInput();

            while (true)
            {
                var readCount = standardInput.ReadAsync(buffer, token).AsTask().GetAwaiter().GetResult();

                if (readCount < 1)
                {
                    /*
                     * EOF on standard input: forward to the Elm app as a zero-length
                     * event so it can flush any buffered output, then exit if the
                     * app has not requested an exit code on its own.
                     * */

                    ProcessStandardInput(ReadOnlyMemory<byte>.Empty);

                    return mutatingCliApp.ExitCode ?? 0;
                }

                ProcessStandardInput(buffer.AsMemory()[..readCount]);

                if (mutatingCliApp.ExitCode is { } exitCode)
                {
                    return exitCode;
                }
            }
        }

        while (true)
        {
            var keys = new List<ReadOnlyMemory<byte>>(capacity: 100);

            void ReadKey()
            {
                while (!Console.KeyAvailable)
                {
                    token.ThrowIfCancellationRequested();
                    Thread.Sleep(10);
                }

                var keyInfo = Console.ReadKey(intercept: true);

                keys.Add(Encoding.UTF8.GetBytes([keyInfo.KeyChar]));
            }

            ReadKey();

            while (Console.KeyAvailable)
            {
                ReadKey();
            }

            var asStandardInput = BytesConversions.Concat(keys);

            ProcessStandardInput(asStandardInput);

            if (mutatingCliApp.ExitCode is { } exitCode)
            {
                return exitCode;
            }
        }
    }
}
