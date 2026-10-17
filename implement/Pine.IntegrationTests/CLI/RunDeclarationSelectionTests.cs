using AwesomeAssertions;
using Pine.CLI;
using Spectre.Console;
using Spectre.Console.Rendering;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.CLI;

public class RunDeclarationSelectionTests
{
    private static readonly RunDeclarationSelection.Candidate[] s_candidates =
        [
        new("alpha", "alpha : String -> Int"),
        new("beta", null),
        new("gamma", "gamma : List (List String) -> String")
        ];

    [Theory]
    [InlineData("2\n", "beta")]
    [InlineData("gamma\n", "gamma")]
    [InlineData("\nunknown\n0\n4\nalpha\n", "alpha")]
    [InlineData("alpx\bha\n", "alpha")]
    public void Plain_selection_accepts_exact_names_or_numbers_and_reprompts_invalid_input(
        string input,
        string expected)
    {
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(5));
        var (console, output, keys) = CreateConsole(Keys(input + "payload"), ansi: false);

        var result = RunDeclarationSelection.Select(s_candidates, console, false, false, cancellation.Token);
        result.Should().Be(expected);
        keys.Remaining.Should().Be(7);

        output.ToString().Should().Contain("1. alpha : String -> Int")
            .And.Contain("2. beta (type not inferred yet)")
            .And.Contain("Selected declaration: " + expected)
            .And.NotContain("\u001b[");
    }

    [Fact]
    public void Searchable_picker_filters_choices_without_consuming_following_input()
    {
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(5));
        var (console, output, keys) = CreateConsole(Keys("gamma\npayload"), ansi: true);
        var result = RunDeclarationSelection.Select(s_candidates, console, false, true, cancellation.Token);
        result.Should().Be("gamma");
        keys.Remaining.Should().Be(7);

        output.ToString().Should().Contain("apply accepts batch functions only")
            .And.Contain("types are checked after selection")
            .And.Contain("Selected declaration: gamma");
    }

    [Fact]
    public void Searchable_picker_supports_arrow_navigation()
    {
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(5));

        var input =
            new[]
            {
                new ConsoleKeyInfo('\0', ConsoleKey.DownArrow, false, false, false),
                new ConsoleKeyInfo('\0', ConsoleKey.DownArrow, false, false, false),
                new ConsoleKeyInfo('\r', ConsoleKey.Enter, false, false, false)
            };

        var (console, _, _) = CreateConsole(input, ansi: true);
        RunDeclarationSelection.Select(s_candidates, console, false, false, cancellation.Token).Should().Be("beta");
    }

    [Fact]
    public void Searchable_picker_can_switch_to_the_plain_list()
    {
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(5));
        var (console, output, _) = CreateConsole(Keys("\n3\n"), ansi: true);
        RunDeclarationSelection.Select(s_candidates, console, false, false, cancellation.Token).Should().Be("gamma");
        output.ToString().Should().Contain("Declaration name or number>");
    }

    [Fact]
    public void Searchable_picker_pages_through_large_lists()
    {
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(5));

        var candidates =
            Enumerable.Range(0, 25)
            .Select(index => new RunDeclarationSelection.Candidate("decl" + index.ToString("D2"), null))
            .ToArray();

        var keys =
            Enumerable.Repeat(new ConsoleKeyInfo('\0', ConsoleKey.DownArrow, false, false, false), 25)
            .Append(new ConsoleKeyInfo('\r', ConsoleKey.Enter, false, false, false));

        var (console, _, _) = CreateConsole(keys, ansi: true);
        RunDeclarationSelection.Select(candidates, console, false, false, cancellation.Token).Should().Be("decl24");
    }

    [Fact]
    public void Arrow_navigation_remains_available_without_color()
    {
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(5));

        var (console, output, _) =
            CreateConsole(Keys("alpha\n"), ansi: true, colorSystem: ColorSystemSupport.NoColors);

        RunDeclarationSelection.Select(s_candidates, console, false, false, cancellation.Token).Should().Be("alpha");
        output.ToString().Should().NotContain("\u001b[36m");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Redirected_input_requires_an_explicit_selector_and_never_reads_keys(bool batchOnly)
    {
        var (console, output, keys) = CreateConsole(Keys("payload\n"), ansi: true);
        var select = () => RunDeclarationSelection.Select(s_candidates, console, true, batchOnly, default);
        select.Should().Throw<ArgumentException>().WithMessage("*stdin is redirected*");
        keys.Remaining.Should().Be(8);

        Regex.Replace(output.ToString(), @"\x1b\[[0-9;]*m", "")
            .Should().Contain("alpha : String -> Int").And.NotContain("Declaration name or number>");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Picker_observes_cancellation_and_restores_the_cursor(bool ansi)
    {
        using var cancellation = new CancellationTokenSource();
        var (console, output, _) = CreateConsole([], ansi, cancellation.Cancel);
        var select = () => RunDeclarationSelection.Select(s_candidates, console, false, false, cancellation.Token);
        select.Should().Throw<OperationCanceledException>();

        if (ansi)
        {
            var text = output.ToString();

            text.LastIndexOf("\u001b[?25h", StringComparison.Ordinal)
                .Should().BeGreaterThan(text.IndexOf("\u001b[?25l", StringComparison.Ordinal));
        }
    }

    [Fact]
    public void Plain_selection_reports_end_of_input()
    {
        var (console, _, _) = CreateConsole([], ansi: false);
        var select = () => RunDeclarationSelection.Select(s_candidates, console, false, false, default);
        select.Should().Throw<ArgumentException>().WithMessage("*end of input*");
    }

    private static IEnumerable<ConsoleKeyInfo> Keys(string text) =>
        text.Select(
            character =>
            character switch
            {
                '\n' => new ConsoleKeyInfo('\r', ConsoleKey.Enter, false, false, false),
                '\b' => new ConsoleKeyInfo('\b', ConsoleKey.Backspace, false, false, false),

                _ =>
                new ConsoleKeyInfo(character, ConsoleKey.A, false, false, false)
            });

    private static (IAnsiConsole Console, StringWriter Output, KeyInput Input) CreateConsole(
        IEnumerable<ConsoleKeyInfo> keys,
        bool ansi,
        Action? onEmpty = null,
        ColorSystemSupport colorSystem = ColorSystemSupport.Standard)
    {
        var output = new StringWriter();
        var input = new KeyInput(keys, onEmpty);

        var console =
            AnsiConsole.Create(
                new AnsiConsoleSettings
                {
                    Ansi = ansi ? AnsiSupport.Yes : AnsiSupport.No,
                    ColorSystem = colorSystem,
                    Interactive = InteractionSupport.Yes,
                    Enrichment = new ProfileEnrichment { UseDefaultEnrichers = false },
                    Out = new TerminalOutput(output)
                });

        return (new InputConsole(console, input), output, input);
    }

    private sealed class KeyInput(IEnumerable<ConsoleKeyInfo> keys, Action? onEmpty) : IAnsiConsoleInput
    {
        private readonly Queue<ConsoleKeyInfo> _keys = new(keys);

        public int Remaining => _keys.Count;

        public bool IsKeyAvailable() => _keys.Count > 0;

        public ConsoleKeyInfo? ReadKey(bool intercept)
        {
            if (_keys.TryDequeue(out var key))
                return key;

            onEmpty?.Invoke();
            return null;
        }

        public Task<ConsoleKeyInfo?> ReadKeyAsync(bool intercept, CancellationToken cancellationToken)
        {
            var key = ReadKey(intercept);
            cancellationToken.ThrowIfCancellationRequested();
            return Task.FromResult(key);
        }
    }

    private sealed class TerminalOutput(TextWriter writer) : IAnsiConsoleOutput
    {
        public TextWriter Writer => writer;

        public bool IsTerminal => true;

        public int Width => 100;

        public int Height => 30;

        public void SetEncoding(Encoding encoding) { }
    }

    private sealed class InputConsole(IAnsiConsole console, IAnsiConsoleInput input) : IAnsiConsole
    {
        public Profile Profile => console.Profile;

        public IAnsiConsoleCursor Cursor => console.Cursor;

        public IAnsiConsoleInput Input => input;

        public IExclusivityMode ExclusivityMode => console.ExclusivityMode;

        public RenderPipeline Pipeline => console.Pipeline;

        public void Clear(bool home) => console.Clear(home);

        public void Write(IRenderable renderable) => console.Write(renderable);

        public void WriteAnsi(Action<AnsiWriter> action) => console.WriteAnsi(action);
    }
}
