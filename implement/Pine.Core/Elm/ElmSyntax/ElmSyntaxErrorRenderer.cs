using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>Presentation verbosity. Reference preserves the Elm 0.19.2 explanations and hints.</summary>
public enum SyntaxErrorVerbosity
{
    /// <summary>
    /// Just enough to identify the problem and its location. No hints or explanations are included.
    /// </summary>
    Concise,

    /// <summary>
    /// Preserves the Elm 0.19.2 explanations and hints.
    /// </summary>
    Reference
}

/// <summary>
/// Composable rendering configuration. Paths and module names are supplied by the caller, never the parser.
/// </summary>
public sealed record ElmSyntaxErrorJsonOptions(
    string Path,
    string? ModuleName = null,
    SyntaxErrorVerbosity Verbosity = SyntaxErrorVerbosity.Reference,
    bool IncludeSource = true,
    bool IncludeStyling = true,
    bool WriteIndented = false,
    int? MaximumProblems = null);

/// <summary>Explicit presentation boundary for structured Elm diagnostics.</summary>
public static partial class ElmSyntaxErrorRenderer
{
    private readonly record struct Part(string? Text, StyledText? Styling, Func<RenderContext, string>? Compose = null);

    private sealed class Presentation(string title, bool constructContext, Part[] parts)
    {
        public string Title { get; } = title;

        public bool ConstructContext { get; } = constructContext;

        public Part[] Parts { get; } = parts;

        public string[] ConciseMessage { get; } = [SentenceCase(title)];
    }

    // Only message elements are heterogeneous: plain strings or styled text, with no discriminator.
    private sealed record StyledText(bool Bold, bool Underline, string? Color, string String);

    private readonly record struct JsonReport(string Type, JsonFile[] Errors);

    private readonly record struct JsonFile(string Path, string Name, JsonProblem[] Problems);

    private readonly record struct JsonProblem(string Title, JsonRegion Region, object[] Message);

    private readonly record struct JsonRegion(JsonPosition Start, JsonPosition End);

    private readonly record struct JsonPosition(int Line, int Column);

    private static readonly JsonSerializerOptions s_jsonOptions =
        new()
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };

    private static readonly JsonSerializerOptions s_indentedJsonOptions =
        new(s_jsonOptions)
        {
            WriteIndented = true
        };

    private static Part T(string text) => new(text, null);

    private static Part T(Func<RenderContext, string> compose) => new(null, null, compose);

    private static Part S(string text, string? color, bool bold = false, bool underline = false) =>
        new(text, new(bold, underline, color, text));

    private static Part S(
        Func<RenderContext, string> compose,
        string? color,
        bool bold = false,
        bool underline = false) =>
        new(null, new(bold, underline, color, ""), compose);

    /// <summary>Collects fatal or recovered errors, stably ordered by their primary source positions.</summary>
    public static IReadOnlyList<ElmSyntaxParseError> CollectErrors(
        Result<ElmSyntaxParseError, SyntaxModel.File> result)
    {
        if (result.IsErrOrNullable() is { } fatal)
            return [fatal];

        var file = result.Extract(error => throw new InvalidOperationException(error.ToString()));

        return
            [
            .. file.IncompleteDeclarations.Select(d => d.Value.ParseError)
            .Concat(file.AdditionalParseErrors)
            .OrderBy(e => e.Region.Start.Row).ThenBy(e => e.Region.Start.Column)
            ];
    }

    /// <summary>Renders a single diagnostic using Elm's compile-errors JSON protocol.</summary>
    public static string RenderJson(string sourceText, ElmSyntaxParseError error, ElmSyntaxErrorJsonOptions options) =>
        RenderJson(sourceText, [error], options);

    /// <summary>
    /// Renders diagnostics without reparsing or mutating them. Source is used only for excerpts and
    /// source-derived spellings. Protocol coordinates are zero-based.
    /// </summary>
    public static string RenderJson(
        string sourceText,
        IEnumerable<ElmSyntaxParseError> errors,
        ElmSyntaxErrorJsonOptions options)
    {
        ArgumentNullException.ThrowIfNull(sourceText);
        ArgumentNullException.ThrowIfNull(errors);
        ArgumentNullException.ThrowIfNull(options);
        ArgumentNullException.ThrowIfNull(options.Path);

        if (options.MaximumProblems is < 0)
            throw new ArgumentOutOfRangeException(nameof(options), "MaximumProblems cannot be negative.");

        if (!Enum.IsDefined(options.Verbosity))
            throw new ArgumentOutOfRangeException(nameof(options), "Unknown verbosity.");

        var ordered = errors.OrderBy(e => e.Region.Start.Row).ThenBy(e => e.Region.Start.Column);
        var selected = options.MaximumProblems is { } maximum ? ordered.Take(maximum).ToArray() : [.. ordered];
        var problems = new JsonProblem[selected.Length];
        RenderSource? source = null;

        for (var i = 0; i < selected.Length; i++)
        {
            var error = selected[i];
            var presentation = GetPresentation(error);

            var message =
                options.Verbosity is SyntaxErrorVerbosity.Concise
                ?
                presentation.ConciseMessage
                :
                RenderMessage(
                    sourceText,
                    ref source,
                    error,
                    presentation,
                    options.IncludeSource,
                    options.IncludeStyling);

            problems[i] =
                new(
                    presentation.Title,
                    new(
                        new(error.Region.Start.Row - 1, error.Region.Start.Column - 1),
                        new(error.Region.End.Row - 1, error.Region.End.Column - 1)),
                    message);
        }

        JsonFile[] files =
            problems.Length is 0
            ?
            []
            :
            [new(options.Path, options.ModuleName ?? ModuleNameFromPath(options.Path), problems)];

        return
            JsonSerializer.Serialize(
                new JsonReport("compile-errors", files),
                options.WriteIndented ? s_indentedJsonOptions : s_jsonOptions);
    }

    private static object[] RenderMessage(
        string sourceText,
        ref RenderSource? source,
        ElmSyntaxParseError error,
        Presentation presentation,
        bool includeSource,
        bool includeStyling)
    {
        var message = new object[presentation.Parts.Length];
        RenderContext? context = null;

        for (var i = 0; i < message.Length; i++)
        {
            var part = presentation.Parts[i];
            var text = part.Text;

            if (part.Compose is { } compose)
            {
                context ??=
                    new(
                        source ??=
                            new(sourceText),
                        error,
                        presentation.ConstructContext,
                        includeSource);

                text = compose(context);
            }

            message[i] =
                includeStyling && part.Styling is { } styling
                ?
                part.Compose is null ? styling : styling with { String = text! }
                :
                text!;
        }

        return message;
    }

    private static string ModuleNameFromPath(string path)
    {
        var span = path.AsSpan();
        return System.IO.Path.GetFileNameWithoutExtension(span[(span.LastIndexOfAny('/', '\\') + 1)..]).ToString();
    }

    /// <summary>Renders concise prose without owning or requiring the source.</summary>
    public static string RenderConcise(ElmSyntaxParseError error) =>
        GetPresentation(error).ConciseMessage[0];

    /// <summary>
    /// Renders the reference explanation as plain text, without protocol serialization or styling.
    /// Source excerpts are optional; source-derived spellings are retained in either mode.
    /// </summary>
    public static string RenderPlainText(
        string sourceText,
        ElmSyntaxParseError error,
        bool includeSource = false)
    {
        ArgumentNullException.ThrowIfNull(sourceText);

        RenderSource? source = null;

        return
            string.Concat(
                RenderMessage(
                    sourceText,
                    ref source,
                    error,
                    GetPresentation(error),
                    includeSource,
                    includeStyling: false));
    }

    private static string SentenceCase(string title)
    {
        var lower = title.ToLowerInvariant();
        return lower.Length is 0 ? lower : char.ToUpperInvariant(lower[0]) + lower[1..];
    }

    private static Presentation GetPresentation(ElmSyntaxParseError error)
    {
        switch (error.Kind)
        {
            case ElmSyntaxErrorKind.Parse parse:
                return ParsePresentation(parse.Problem);

            case ElmSyntaxErrorKind.ModuleValidation validation:
                return
                    ReferencePresentation(
                        validation.Problem switch
                        {
                            ModuleValidationProblem.ModuleNameMissing => ReferenceMessage.ModuleNameMissing,
                            ModuleValidationProblem.ModuleNameMismatch => ReferenceMessage.ModuleNameMismatch,
                            ModuleValidationProblem.PortInNormalModule => ReferenceMessage.UnexpectedPort,
                            ModuleValidationProblem.PortModuleWithoutPorts => ReferenceMessage.NoPorts,

                            ModuleValidationProblem.PortInPackage or ModuleValidationProblem.PortModuleInPackage or
                            ModuleValidationProblem.EffectModuleOutsideKernel =>
                            throw new NotSupportedException(
                                "The selected module validation policy has no Elm reference rendering."),

                            _ =>
                            throw new NotImplementedException(
                                "GetPresentation does not handle module validation: " + validation.Problem)
                        });

            default:
                throw new NotImplementedException("GetPresentation does not handle error kind: " + error.Kind.GetType().Name);
        }
    }

    private static Presentation ParsePresentation(ElmSyntaxProblem problem)
    {
        switch (problem)
        {
            case ElmSyntaxProblem.Expected expected:
                var description =
                    expected.Syntax switch
                    {
                        ExpectedSyntax.Token token => token.Kind.ToString(),
                        ExpectedSyntax.Keyword keyword => "`" + keyword.Spelling + "`",
                        ExpectedSyntax.Identifier identifier => identifier.Role.ToString(),

                        _ =>
                        throw new NotImplementedException(
                            "ParsePresentation does not handle expectation: " + expected.Syntax.GetType().Name)
                    };

                return new Presentation("SYNTAX PROBLEM", false, [T("Expected " + description + ".")]);

            case ElmSyntaxProblem.Grammar grammar:
                return GrammarPresentation(grammar.Branch);

            case ElmSyntaxProblem.AnnotationNameMismatch:
                return ReferencePresentation(ReferenceMessage.DefNameMatch);

            case ElmSyntaxProblem.AnnotationNameRepeated:
                return ReferencePresentation(ReferenceMessage.DefNameRepeat);

            case ElmSyntaxProblem.Escape escape:
                return
                    ReferencePresentation(
                        escape.Problem switch
                        {
                            EscapeProblem.Unknown => ReferenceMessage.CharEscape,
                            EscapeProblem.UnicodeFormat => ReferenceMessage.StringUnicodeFormat,
                            EscapeProblem.UnicodeShort => ReferenceMessage.StringUnicodeShort,
                            EscapeProblem.UnicodeLong => ReferenceMessage.StringUnicodeLong,
                            EscapeProblem.UnicodeCode => ReferenceMessage.StringUnicodeCode,

                            _ =>
                            throw new NotImplementedException("ParsePresentation does not handle escape: " + escape.Problem)
                        });

            case ElmSyntaxProblem.Number number:
                return
                    ReferencePresentation(
                        number.Problem switch
                        {
                            NumberProblem.Dot => ReferenceMessage.NumberDot,
                            NumberProblem.End => ReferenceMessage.NumberEnd,
                            NumberProblem.Hex => ReferenceMessage.NumberHex,
                            NumberProblem.LeadingZero => ReferenceMessage.NumberLeadingZero,

                            _ =>
                            throw new NotImplementedException("ParsePresentation does not handle number: " + number.Problem)
                        });

            case ElmSyntaxProblem.Wildcard:
                return ReferencePresentation(ReferenceMessage.PatternWildcard);

            case ElmSyntaxProblem.Shader shader:
                return
                    ReferencePresentation(
                        shader.Problem switch
                        {
                            ShaderProblem.Endless => ReferenceMessage.ShaderEndless,
                            ShaderProblem.Invalid => ReferenceMessage.ShaderProblem,

                            _ =>
                            throw new NotImplementedException("ParsePresentation does not handle shader: " + shader.Problem)
                        });

            default:
                throw new NotImplementedException("ParsePresentation does not handle problem: " + problem.GetType().Name);
        }
    }

    private sealed class RenderSource(string text)
    {
        private static readonly string[] s_lineSeparators = ["\r\n", "\r", "\n"];

        private string[]? _lines;

        public string[] Lines => _lines ??= text.Split(s_lineSeparators, StringSplitOptions.None);
    }

    private sealed class RenderContext
    {
        private readonly RenderSource _source;

        private readonly ElmSyntaxParseError _error;

        private readonly bool _constructContext;

        private readonly bool _includeSource;

        private string? _definition;

        private string? _snippet;

        private string? _carets;

        private string? _lowerName;

        public string Found { get; } = "";

        public string Number { get; } = "";

        public string Annotated { get; } = "";

        public string Defined { get; } = "";

        public string BareWildcard { get; } = "";

        public string PaddedEscape { get; } = "";

        public string ShaderDetail { get; } = "";

        public string ExpectedModule { get; } = "";

        public string DeclaredModule { get; } = "";

        public RenderContext(RenderSource source, ElmSyntaxParseError error, bool constructContext, bool includeSource)
        {
            _source = source;
            _error = error;
            _constructContext = constructContext;
            _includeSource = includeSource;

            switch (error.Kind)
            {
                case ElmSyntaxErrorKind.Parse parse:
                    switch (parse.Problem)
                    {
                        case ElmSyntaxProblem.Expected expected:
                            Found = expected.Found.Text;
                            break;

                        case ElmSyntaxProblem.Grammar grammar:
                            Found = grammar.Found?.Text ?? "";
                            break;

                        case ElmSyntaxProblem.AnnotationNameMismatch mismatch:
                            Annotated = mismatch.AnnotatedName.Name;
                            Defined = mismatch.DefinedName;
                            break;

                        case ElmSyntaxProblem.AnnotationNameRepeated repeated:
                            Annotated = repeated.Name;
                            break;

                        case ElmSyntaxProblem.Number literal:
                            Number = literal.IntegerPrefix;
                            break;

                        case ElmSyntaxProblem.Escape escape:
                            if (escape.Problem is EscapeProblem.UnicodeShort)
                            {
                                PaddedEscape =
                                    "\\u{" + (escape.CodePoint ?? 0).ToString("x4", CultureInfo.InvariantCulture) + "}";
                            }

                            break;

                        case ElmSyntaxProblem.Wildcard name:
                            BareWildcard = name.Name.TrimStart('_');
                            break;

                        case ElmSyntaxProblem.Shader shader:
                            if (shader.Problem is ShaderProblem.Invalid)
                                ShaderDetail = RenderShaderDetail(shader);

                            break;

                        default:
                            throw new NotImplementedException(
                                "RenderContext does not handle problem: " + parse.Problem.GetType().Name);
                    }

                    break;

                case ElmSyntaxErrorKind.ModuleValidation validation:
                    ExpectedModule = validation.ExpectedName ?? "";
                    DeclaredModule = validation.DeclaredName ?? "";
                    break;

                default:
                    throw new NotImplementedException(
                        "RenderContext does not handle error kind: " + error.Kind.GetType().Name);
            }
        }

        public string Definition =>
            _definition ??=
                Annotated.Length > 0
                ?
                Annotated
                :
                Regex.Match(
                    _source.Lines.ElementAtOrDefault((_error.ContextRange?.Start.Row ?? _error.Region.Start.Row) - 1) ?? "",
                    @"^\s*([a-z][\w]*)").Groups[1].Value;

        public string Snippet =>
            _snippet ??= _includeSource ? RenderSnippet(_source.Lines, _error, _constructContext) : "";

        public string Carets =>
            _carets ??=
                _includeSource
                ?
                new string(
                    '^',
                    _error.Region.Start.Row == _error.Region.End.Row
                    ?
                    Math.Max(1, _error.Region.End.Column - _error.Region.Start.Column)
                    :
                    1)
                :
                "";

        public string LowerName =>
            _lowerName ??= Found.Length > 0 ? char.ToLowerInvariant(Found[0]) + Found[1..] : "";
    }

    private static string RenderShaderDetail(ElmSyntaxProblem.Shader shader)
    {
        var detail =
            shader.UpstreamDetail ??
            (shader.SyntaxProblem switch
            {
                ShaderSyntaxProblem.ExpectedDeclarator =>
                "unexpected end of input\nexpecting \"/\", space, \"[\", letter, \"_\" or \";\"",

                ShaderSyntaxProblem.UnfinishedDeclaration => "unexpected end of input\nexpecting \";\"",
                ShaderSyntaxProblem.UnclosedDelimiter => "unexpected end of input\nexpecting a closing delimiter",
                ShaderSyntaxProblem.UnexpectedDelimiter => "unexpected closing delimiter",
                null => "",

                _ =>
                throw new NotImplementedException(
                    "RenderShaderDetail does not handle shader syntax: " + shader.SyntaxProblem)
            });

        var builder = new StringBuilder(detail.Length + 4);

        foreach (var line in detail.AsSpan().Split('\n'))
        {
            if (builder.Length > 0)
                builder.Append('\n');

            builder.Append("    ").Append(detail.AsSpan(line));
        }

        return builder.ToString();
    }

    private static string RenderSnippet(string[] lines, ElmSyntaxParseError error, bool context)
    {
        var first = context ? error.ContextRange?.Start.Row ?? error.Region.Start.Row : error.Region.Start.Row;
        var last = error.Region.Start.Row;
        first = Math.Max(1, Math.Min(first, last));
        var width = last.ToString(CultureInfo.InvariantCulture).Length;
        var builder = new StringBuilder();

        for (var row = first; row <= last; row++)
        {
            var label = row.ToString(CultureInfo.InvariantCulture);

            builder.Append(' ', width - label.Length).Append(label).Append("| ")
                .Append(lines.ElementAtOrDefault(row - 1) ?? "").Append('\n');
        }

        return builder.Append(' ', width + 2 + Math.Max(0, error.Region.Start.Column - 1)).ToString();
    }

}
