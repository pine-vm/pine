using Pine.Core.Elm.Testing;
using Spectre.Console;
using System;
using System.CommandLine;
using System.IO;
using System.Linq;

namespace Pine.CLI.Elm;

public static class TestCommand
{
    public static Command Create()
    {
        var command =
            new Command(
                "test",
                "Compile and run Elm tests.");

        var sourceArgument =
            new Argument<string?>("source")
            {
                Arity = ArgumentArity.ZeroOrOne,
                Description = "Path to the Elm project. Defaults to the current directory.",
            };

        var colorOption = FormatCommandShared.CreateColorOption();

        var filterOption =
            new Option<string?>("--filter")
            {
                Description =
                "Only run tests whose test or group name contains this value (case-insensitive)."
            };

        command.Add(sourceArgument);
        command.Add(colorOption);
        command.Add(filterOption);

        command.SetAction(
            parseResult =>
            Execute(
                source: parseResult.GetValue(sourceArgument) ?? Environment.CurrentDirectory,
                colorMode: parseResult.GetValue(colorOption),
                filter: parseResult.GetValue(filterOption)));

        return command;
    }


    public static int Execute(
        string source,
        FormatCommandColorMode? colorMode = null,
        IAnsiConsole? console = null,
        IAnsiConsole? errorConsole = null,
        string? filter = null)
    {
        FormatCommandColorMode resolvedColorMode;

        try
        {
            resolvedColorMode =
                FormatCommandShared.ResolveColorMode(
                    colorMode,
                    Environment.GetEnvironmentVariable(FormatCommandShared.ColorEnvironmentVariable));
        }
        catch (ArgumentException exception)
        {
            errorConsole ??=
                CreateSystemConsole(
                    Console.Error,
                    FormatCommandColorMode.Auto);

            errorConsole.Write(new Text("Error: ", TestCommandTheme.Failure));
            errorConsole.WriteLine(exception.Message);

            return 1;
        }

        console ??= CreateSystemConsole(Console.Out, resolvedColorMode);

        var testRun =
            ElmTestRunner.CompileAndRunTests(
                source,
                pineVm: IntermediateVM.SetupVM.Create(),
                filter: filter);

        if (testRun is ElmTestRun.NoTestModules noTestModules)
        {
            errorConsole ??= CreateSystemConsole(Console.Error, resolvedColorMode);

            errorConsole.Write(new Text("Error: ", TestCommandTheme.Failure));

            var message = "Did not find Elm test modules in " + noTestModules.AppDirectory;

            if (errorConsole.Profile.Out.IsTerminal)
                errorConsole.WriteLine(message);

            else
                errorConsole.Profile.Out.Writer.WriteLine(message);

            return 1;
        }

        if (testRun is not ElmTestRun.Completed completed)
            throw new InvalidOperationException("Unexpected Elm test run type: " + testRun.GetType());

        var output =
            ElmTestRunner.RenderTestResults(
                completed.Tests,
                includeTestDetails: true,
                completed.Duration);

        if (resolvedColorMode is FormatCommandColorMode.Never)
        {
            console.Write(new Text(output.PlainText));
        }
        else
        {
            foreach (var fragment in output.Fragments)
                console.Write(new Text(fragment.Text, StyleFor(fragment.Style)));
        }

        console.WriteLine();

        return
            completed.Tests.All(test => test.Kind is CompletedTestKind.Passed)
            ?
            0
            :
            1;
    }


    private static Style StyleFor(TestOutputStyle style) =>
        style switch
        {
            TestOutputStyle.Default => TestCommandTheme.Default,
            TestOutputStyle.Dark => TestCommandTheme.Dark,
            TestOutputStyle.Success => TestCommandTheme.Success,
            TestOutputStyle.SuccessHeadline => TestCommandTheme.SuccessHeadline,
            TestOutputStyle.Failure => TestCommandTheme.Failure,
            TestOutputStyle.FailureHeadline => TestCommandTheme.FailureHeadline,
            TestOutputStyle.Todo => TestCommandTheme.Todo,
            TestOutputStyle.TodoHeadline => TestCommandTheme.TodoHeadline,
            TestOutputStyle.Highlighted => TestCommandTheme.Highlighted,

            _ =>
            throw new ArgumentOutOfRangeException(nameof(style)),
        };


    private static IAnsiConsole CreateSystemConsole(
        TextWriter writer,
        FormatCommandColorMode colorMode) =>
        AnsiConsole.Create(
            new AnsiConsoleSettings
            {
                Ansi = FormatCommandShared.AnsiSupportForColorMode(colorMode),
                ColorSystem = FormatCommandShared.ColorSystemSupportForColorMode(colorMode),
                Out = new AnsiConsoleOutput(writer),
            });


    private static class TestCommandTheme
    {
        public static Style Default { get; } =
            new(foreground: Color.Default);

        public static Style Dark { get; } =
            new(foreground: Color.Default, decoration: Decoration.Dim);

        public static Style Success { get; } =
            new(foreground: Color.Green);

        public static Style SuccessHeadline { get; } =
            new(foreground: Color.Green, decoration: Decoration.Underline);

        public static Style Failure { get; } =
            new(foreground: Color.Red);

        public static Style FailureHeadline { get; } =
            new(foreground: Color.Red, decoration: Decoration.Underline);

        public static Style Todo { get; } =
            new(foreground: Color.Yellow);

        public static Style TodoHeadline { get; } =
            new(foreground: Color.Yellow, decoration: Decoration.Underline);

        public static Style Highlighted { get; } =
            new(foreground: Color.Default, decoration: Decoration.Invert);
    }
}
