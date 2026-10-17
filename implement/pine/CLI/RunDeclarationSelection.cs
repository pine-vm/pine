using Pine.Core.CLI;
using Spectre.Console;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

namespace Pine.CLI;

internal static class RunDeclarationSelection
{
    internal sealed record Candidate(string Name, string? Signature);

    private static readonly Candidate s_plainSelection = new("", null);

    internal static IAnsiConsole CreateConsole(TextWriter writer, CommandLineColorMode color) =>
        AnsiConsole.Create(
            new AnsiConsoleSettings
            {
                Ansi = AnsiSupport.Detect,
                ColorSystem = FormatCommandShared.ColorSystemSupportForColorMode(color),
                Out = new AnsiConsoleOutput(writer)
            });

    internal static string Select(
        IReadOnlyList<Candidate> candidates,
        IAnsiConsole console,
        bool inputRedirected,
        bool batchOnly,
        CancellationToken token)
    {
        token.ThrowIfCancellationRequested();

        if (inputRedirected)
        {
            WriteCandidates(candidates, console);

            throw new ArgumentException(
                "Select a declaration explicitly using .elm/name or .elm#Lnumber when stdin is redirected. " +
                "Declaration selection does not read redirected input.");
        }

        console.WriteLine(
            "Select a declaration" + (batchOnly ? " (apply accepts batch functions only)." : "."));

        console.WriteLine("Source annotations are shown where available; types are checked after selection.");

        Candidate? selected = null;

        if (console.Profile.Out.IsTerminal &&
            console.Profile.Capabilities.Ansi &&
            console.Profile.Capabilities.Interactive)
        {
            console.WriteLine(
                "Use arrows and Enter, or type to search. Choose the plain-text list to enter a name or number. Ctrl+C cancels.");

            var prompt =
                new SelectionPrompt<Candidate>()
                .PageSize(10)
                .EnableSearch()
                .SearchPlaceholderText("Type to filter declarations")
                .UseConverter(
                    item =>
                    item == s_plainSelection
                    ?
                    "Enter a name or number (plain-text list)"
                    :
                    CandidateMarkup(item));

            prompt.AddChoice(s_plainSelection);

            foreach (var candidate in candidates)
                prompt.AddChoice(candidate);

            try
            {
                selected = prompt.ShowAsync(console, token).GetAwaiter().GetResult();
            }
            finally
            {
                console.Cursor.Show(true);
            }
        }

        if (selected is null || selected == s_plainSelection)
        {
            WriteCandidates(candidates, console);

            while (true)
            {
                console.Profile.Out.Writer.Write("Declaration name or number> ");
                console.Profile.Out.Writer.Flush();
                var response = ReadSelection(console, token).Trim();

                selected =
                    int.TryParse(response, NumberStyles.None, CultureInfo.InvariantCulture, out var number) &&
                    number > 0 && number <= candidates.Count
                    ?
                    candidates[number - 1]
                    :
                    candidates.SingleOrDefault(item => item.Name == response);

                if (selected is not null)
                    break;

                console.WriteLine("No matching declaration. Enter an exact name or a number from the list.");
            }
        }

        console.WriteLine("Selected declaration: " + selected.Name);
        return selected.Name;
    }

    private static string ReadSelection(IAnsiConsole console, CancellationToken token)
    {
        var text = new StringBuilder();

        while (true)
        {
            token.ThrowIfCancellationRequested();
            var key = console.Input.ReadKeyAsync(intercept: true, token).GetAwaiter().GetResult();

            if (key is not { } pressed ||
                pressed.Key is ConsoleKey.D or ConsoleKey.Z && pressed.Modifiers.HasFlag(ConsoleModifiers.Control))
            {
                throw new ArgumentException("No declaration selected (end of input). Use .elm/name or .elm#Lnumber.");
            }

            if (pressed.Key is ConsoleKey.Enter)
            {
                console.WriteLine();
                return text.ToString();
            }

            if (pressed.Key is ConsoleKey.Backspace)
            {
                if (text.Length > 0)
                {
                    text.Length--;
                    console.Profile.Out.Writer.Write("\b \b");
                }
            }
            else if (!char.IsControl(pressed.KeyChar))
            {
                text.Append(pressed.KeyChar);
                console.Profile.Out.Writer.Write(pressed.KeyChar);
            }

            console.Profile.Out.Writer.Flush();
        }
    }

    private static void WriteCandidates(IReadOnlyList<Candidate> candidates, IAnsiConsole console)
    {
        console.WriteLine("Available declarations (source annotations; types not yet checked):");

        for (var index = 0; index < candidates.Count; index++)
        {
            console.Profile.Out.Writer.Write((index + 1).ToString(CultureInfo.InvariantCulture) + ". ");
            console.MarkupLine(CandidateMarkup(candidates[index]));
        }
    }

    private static string CandidateMarkup(Candidate candidate) =>
        candidate.Signature is { } signature
        ?
        SignatureMarkup(signature)
        :
        Markup.Escape(candidate.Name) + " (type not inferred yet)";

    internal static void WriteInferredSignature(IAnsiConsole console, string signature)
    {
        console.WriteLine("Selected declaration (inferred type; '?' means undetermined):");
        console.MarkupLine("    " + SignatureMarkup(signature));
    }

    internal static string SignatureMarkup(string signature)
    {
        var colon = signature.IndexOf(':');
        var result = new StringBuilder();

        foreach (Match token in Regex.Matches(signature, @"[A-Za-z_][A-Za-z0-9_.]*|->|.", RegexOptions.Singleline))
        {
            var text = Markup.Escape(token.Value);

            var style =
                char.IsLetter(token.Value[0]) || token.Value[0] is '_'
                ?
                token.Index < colon ? "yellow bold" : char.IsUpper(token.Value[0]) ? "cyan" : "green"
                :
                token.Value is ":" or "->" or "?" ? "magenta" : null;

            result.Append(style is null ? text : "[" + style + "]" + text + "[/]");
        }

        return result.ToString();
    }
}
