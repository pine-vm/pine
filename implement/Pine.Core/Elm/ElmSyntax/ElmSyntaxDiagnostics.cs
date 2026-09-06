using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Buffers;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Diagnostic grammar. Unlike the lossless tree parser, this reader tracks the point at which each
/// construct commits, including whitespace before a missing continuation. It does not render text.
/// </summary>
internal static class ElmSyntaxDiagnostics
{
    internal sealed record Diagnostic(ElmSyntaxParseError Error, Range? Declaration, bool PreserveTree = false);

    private sealed record Token(string Text, Range Range, FoundSyntaxKind Kind,
        ElmSyntaxParseError? LexicalError = null)
    {
        public Location Start => Range.Start;

        public Location End => Range.End;

        public FoundSyntax Found => new(Kind, Text);
    }

    private sealed record Failure(ElmSyntaxParseError Error);

    // An error retains the exact cursor at the commitment point, for declaration recovery.
    // Keep its payload off the successful path so each transition only copies a cursor and a reference.
    private readonly record struct ValueResult(Reader State, Failure? Failure = null)
    {
        public ValueResult(Reader state, ElmSyntaxParseError error) : this(state, new Failure(error)) { }

        public ElmSyntaxParseError? Error => Failure?.Error;

        public ValueResult AndThen(Func<Reader, ValueResult> next) =>
            Failure is null ? next(State) : this;

        public static implicit operator ValueResult(Reader state) => new(state);
    }

    private readonly record struct ModuleState(Reader Reader, ImmutableStack<Diagnostic> Diagnostics);

    private readonly record struct Items(ValueResult Result, int Count = 0, bool Done = false);

    // Balanced repetition keeps linear work and logarithmic stack depth even for very long flat
    // inputs. Every step consumes at least one token/character, or makes the predicate false.
    private static T Repeat<T>(T state, Func<T, bool> more, Func<T, T> step, int budget)
    {
        if (budget <= 0 || !more(state))
            return state;

        var next = step(state);

        if (budget is 1)
            return next;

        var remaining = budget - 1;
        var first = Repeat(next, more, step, remaining / 2);
        return Repeat(first, more, step, remaining - remaining / 2);
    }

    internal static IReadOnlyList<Diagnostic> Parse(string source)
    {
        var tokens = Lex(source);
        var initial = new Reader(tokens, 0, tokens[0].Start);

        var prefix =
            initial.Current.LexicalError is { } lexicalError
            ?
            new ValueResult(initial, lexicalError)
            :
            initial.At("port") ? initial.Take() : initial;

        if (prefix.Error is { } prefixError)
            return [new Diagnostic(prefixError, null)];

        // A missing module name requires a path-aware coordinator. The tree parser retains
        // its existing header requirement, without fabricating an expected name here.
        if (!prefix.State.At("module"))
            return [];

        var header = prefix.State.Take().AndThen(reader => reader.ReadHeader(initial.At("port")));

        if (header.Error is { } headerError)
            return [new Diagnostic(headerError, null)];

        var module =
            Repeat(
                new ModuleState(header.State, []),
                state => !state.Reader.End,
                ReadDeclaration,
                tokens.Length);

        return module.Diagnostics.Reverse().ToImmutableArray();
    }

    private static ModuleState ReadDeclaration(ModuleState module)
    {
        var begin = module.Reader.Index;

        var reader =
            module.Reader with
            {
                Construct = module.Reader.Current.Start,
                Indent = 1,
                Site = SyntaxErrorSite.Declaration
            };

        var result = reader.ReadDeclaration();

        if (result.Error is not { } error)
            return module with { Reader = result.State };

        // Recovery owns the declaration extent, not the diagnostic's context range.
        var advanced =
            result.State.Index <= begin
            ?
            result.State with { Index = Math.Min(begin + 1, reader.Tokens.Length - 1) }
            :
            result.State;

        var afterLine =
            !advanced.End && advanced.Current.Start.Column is 1 &&
            advanced.Current.Start.Row == advanced.Previous.End.Row + 1 &&
            error.Region.Start.Row == advanced.Previous.End.Row
            ?
            advanced with { Index = advanced.Index + 1 }
            :
            advanced;

        var recovered =
            Repeat(
                afterLine,
                state => !state.End &&
                    !(state.Current.Start.Column is 1 &&
                (state.Lower || state.At("type") || state.At("port") || state.At("import"))),
                state => state with { Index = state.Index + 1 },
                reader.Tokens.Length - afterLine.Index);

        var last = reader.Tokens[Math.Max(begin, recovered.Index - 1)].End;

        var diagnostic =
            new Diagnostic(
                error,
                new Range(reader.Tokens[begin].Start, last),
                error.Kind is ElmSyntaxErrorKind.Parse
                {
                    Problem: ElmSyntaxProblem.Grammar { Branch: SyntaxErrorBranch.DeclExpecting }
                });

        return new(recovered, module.Diagnostics.Push(diagnostic));
    }

    private readonly record struct Reader(
        ImmutableArray<Token> Tokens,
        int Index,
        Location Construct,
        int Indent = 1,
        SyntaxErrorSite Site = default)
    {
        public Token Current => Tokens[Math.Min(Index, Tokens.Length - 1)];

        public Token Previous => Tokens[Math.Max(0, Index - 1)];

        public bool At(string text) => Current.Text == text;

        public bool End => Current.Kind is FoundSyntaxKind.EndOfFile;

        public bool Lower => Current.Kind is FoundSyntaxKind.LowerIdentifier;

        private bool Upper => Current.Kind is FoundSyntaxKind.UpperIdentifier;

        private bool Indented => !End && (Current.Start.Row == Previous.End.Row || Current.Start.Column > Indent);

        private bool IsTypeStart => Lower || Upper || At("(") || At("{");

        private bool IsPatternStart =>
            Lower || Upper || At("_") || At("(") || At("{") || At("[") ||
            Current.Kind is FoundSyntaxKind.Literal;

        private bool IsExpressionStart =>
            Lower || Upper || Current.Kind is FoundSyntaxKind.Literal ||
            At("(") || At("{") || At("[") || At("\\") || At(".") || At("-") || At("if") || At("let") || At("case");

        public ValueResult Take() =>
            Current.LexicalError is { } error
            ?
            new(this, error)
            :
            new(this with { Index = End ? Index : Index + 1 });

        private ValueResult Fail(
            SyntaxErrorBranch branch,
            Location? position = null,
            Location? end = null,
            Location? context = null,
            ElmSyntaxProblem? problem = null)
        {
            if (Current.LexicalError is { } lexicalError)
                return new(this, lexicalError);

            var start = position ?? Current.Start;
            var contextStart = context ?? Construct;

            return
                new(
                    this,
                    new ElmSyntaxParseError(
                        new Range(start, end ?? start),
                        new Range(contextStart, end ?? start),
                        new ElmSyntaxErrorKind.Parse(
                            problem ??
                            new ElmSyntaxProblem.Grammar(
                                branch,
                                Current.Found,
                                Site,
                                Current.Start.Column <= Indent ? Indent + 1 : null,
                                Current.Start.Column <= Indent ? Current.Start.Column : null))));
        }

        private ValueResult Require(
            bool condition,
            SyntaxErrorBranch branch,
            Location? position = null,
            Location? context = null) =>
            condition ? this : Fail(branch, position, context: context);

        private ValueResult Expect(
            string text,
            SyntaxErrorBranch branch,
            Location? position = null,
            Location? context = null) =>
            At(text) ? Take() : Fail(branch, position, context: context);

        private ValueResult ReadWhile(Func<Reader, bool> more, Func<Reader, ValueResult> read) =>
            ReadWhile(this, more, read, Tokens.Length - Index);

        // Specialize repetition to avoid allocating predicate/step adapters for every grammar atom.
        private static ValueResult ReadWhile(
            ValueResult result,
            Func<Reader, bool> more,
            Func<Reader, ValueResult> read,
            int budget)
        {
            if (budget <= 0 || result.Failure is not null || !more(result.State))
                return result;

            var next = read(result.State);

            if (budget is 1)
                return next;

            var remaining = budget - 1;
            var first = ReadWhile(next, more, read, remaining / 2);
            return ReadWhile(first, more, read, remaining - remaining / 2);
        }

        public ValueResult ReadHeader(bool portModule) =>
            Require(
                Indented,
                portModule ? SyntaxErrorBranch.PortModuleProblem : SyntaxErrorBranch.ModuleProblem,
                Previous.End)
            .AndThen(
                s => s.Require(
                    s.Upper,
                    portModule ? SyntaxErrorBranch.PortModuleName : SyntaxErrorBranch.ModuleName))
            .AndThen(s => s.ReadQualifiedName())
            .AndThen(
                s => s.Expect(
                    "exposing",
                    portModule ? SyntaxErrorBranch.PortModuleExposing : SyntaxErrorBranch.ModuleExposingStart,
                    s.Previous.End))
            .AndThen(
                s => s.Expect(
                    "(",
                    portModule ? SyntaxErrorBranch.PortModuleExposing : SyntaxErrorBranch.ModuleExposingStart,
                    s.Previous.End))
            .AndThen(s => s.ReadExposing())
            .AndThen(
                s => s.Current.Text.StartsWith("{-|", StringComparison.Ordinal) &&
                    s.Current.Text.Contains("@docs", StringComparison.Ordinal)
                ?
                s.Take()
                :
                s)
            .AndThen(s => s.ReadWhile(r => r.At("import") && r.Current.Start.Column is 1, r => r.ReadImport()));

        public ValueResult ReadDeclaration() =>
            Require(
                Current.Start.Column is 1,
                At("import")
                ?
                SyntaxErrorBranch.DeclImportIndent
                :
                At("module") ? SyntaxErrorBranch.FreshModule : SyntaxErrorBranch.FreshType)
            .AndThen(
                s => s.ReadWhile(
                    r => r.Current.Text.StartsWith("{-|", StringComparison.Ordinal),
                    r => r.Take()
                    .AndThen(a => a.Require(!a.End, SyntaxErrorBranch.DeclExpecting))
                    .AndThen(a => a.Require(a.Current.Start.Column is 1, SyntaxErrorBranch.DocCommentFresh))
                    .AndThen(a => a with { Construct = a.Current.Start })))
            .AndThen(s => s.ReadDeclarationBody())
            .AndThen(s => !s.End && s.Current.Start.Column is not 1 ? s.ReadTrailing() : s);

        private ValueResult ReadDeclarationBody()
        {
            if (At("import"))
                return ReadImport();

            if (At("type"))
                return Take().AndThen(s => s.ReadTypeDeclaration());

            if (At("port"))
                return Take().AndThen(s => s.ReadPort());

            if (At("infix"))
                return Take().AndThen(s => s.ReadWhile(r => !r.End && r.Current.Start.Column > 1, r => r.Take()));

            if (Lower)
                return ReadDefinition(false);

            return
                Fail(
                    At("}")
                    ?
                    SyntaxErrorBranch.StrayCurlyBrace
                    :
                    At("]")
                    ?
                    SyntaxErrorBranch.StraySquareBracket
                    :
                    Current.Kind is FoundSyntaxKind.Keyword
                    ?
                    SyntaxErrorBranch.DeclReserved
                    :
                    Upper
                    ?
                    SyntaxErrorBranch.DeclUpper
                    :
                    At(")")
                    ?
                    SyntaxErrorBranch.ModuleEndClose
                    :
                    At(",")
                    ?
                    SyntaxErrorBranch.ModuleEndComma
                    :
                    At(";") ? SyntaxErrorBranch.ModuleEndSemicolon : SyntaxErrorBranch.DeclSymbol,
                    end: Current.Kind is FoundSyntaxKind.Keyword ? Current.End : null);
        }

        private ValueResult ReadQualifiedName() =>
            Take().AndThen(
                s => s.ReadWhile(
                    r => r.At(".") && r.Current.Start == r.Previous.End,
                    r => r.Take()
                    .AndThen(a => a.Require(a.Upper, SyntaxErrorBranch.ModuleName))
                    .AndThen(a => a.Take())));

        private ValueResult ReadImport() =>
            Take()
            .AndThen(s => s with { Construct = s.Previous.Start })
            .AndThen(s => s.Require(s.Indented, SyntaxErrorBranch.ImportIncomplete, s.Previous.End))
            .AndThen(s => s.Require(s.Upper, SyntaxErrorBranch.ImportName))
            .AndThen(s => s.ReadQualifiedName())
            .AndThen(
                s => s.At("as")
                ?
                s.Take()
                .AndThen(r => r.Require(r.Upper, SyntaxErrorBranch.ImportAlias))
                .AndThen(r => r.ReadQualifiedName())
                :
                s)
            .AndThen(
                s => s.At("exposing")
                ?
                s.Take()
                .AndThen(r => r.Expect("(", SyntaxErrorBranch.ImportExposing, r.Previous.End))
                .AndThen(r => r.ReadExposing())
                :
                s)
            .AndThen(s => s.Require(s.End || s.Current.Start.Row != s.Previous.End.Row, SyntaxErrorBranch.ImportEnd));

        private ValueResult ReadExposing()
        {
            if (At(".."))
                return Take().AndThen(s => s.Expect(")", SyntaxErrorBranch.ExposingEnd));

            var items =
                Repeat(
                    new Items(this),
                    item => item.Result.Error is null && !item.Done && !item.Result.State.End,
                    item =>
                    {
                        var reader = item.Result.State;

                        var value =
                            reader.At(")") && item.Count > 0
                            ?
                            reader.Fail(SyntaxErrorBranch.ExposingTrailingComma)
                            :
                            reader.ReadExposedValue();

                        if (value.Error is not null)
                            return item with { Result = value };

                        return
                            value.State.At(")")
                            ?
                            new Items(value.State.Take(), item.Count, true)
                            :
                            new Items(value.State.Expect(",", SyntaxErrorBranch.ExposingEnd), item.Count + 1);
                    },
                    Tokens.Length - Index);

            return
                items.Done
                ?
                items.Result
                :
                items.Result.AndThen(s => s.Fail(SyntaxErrorBranch.ExposingEnd, s.Previous.End));
        }

        private ValueResult ReadExposedValue()
        {
            if (Lower || Upper)
            {
                var upper = Upper;

                return
                    Take().AndThen(
                        s => upper && s.At("(")
                        ?
                        s.Take().AndThen(r => r.ReadExposedTypePrivacy())
                        :
                        s);
            }

            if (At("("))
            {
                return
                    Take()
                    .AndThen(s => s.Require(!s.At(")"), SyntaxErrorBranch.ExposingOperatorEmpty))
                    .AndThen(
                        s => s.Require(
                            !s.At("=") && !s.At("->") && !s.At("|") && !s.At(":"),
                            SyntaxErrorBranch.ExposingOperatorReserved))
                    .AndThen(s => s.Take())
                    .AndThen(s => s.Expect(")", SyntaxErrorBranch.ExposingOperatorClose))
                    .AndThen(s => s.Require(s.At(",") || s.At(")"), SyntaxErrorBranch.ExposingOperatorClose));
            }

            return
                Fail(
                    Current.Kind is FoundSyntaxKind.Keyword
                    ?
                    SyntaxErrorBranch.ExposingValueKeyword
                    :
                    SyntaxErrorBranch.ExposingValueSymbol);
        }

        private ValueResult ReadExposedTypePrivacy()
        {
            if (At(".."))
                return Take().AndThen(s => s.Expect(")", SyntaxErrorBranch.ExposingTypePrivacy));

            var dot = At(".") ? Take() : this;
            return dot.AndThen(s => s.Fail(SyntaxErrorBranch.ExposingTypePrivacy, s.Previous.Start));
        }

        private ValueResult ReadTypeDeclaration()
        {
            var alias = At("alias");
            var prefix = alias ? Take() : this;

            return
                prefix
                .AndThen(s => s.Require(s.Upper, alias ? SyntaxErrorBranch.AliasName : SyntaxErrorBranch.CustomName))
                .AndThen(s => s.Take())
                .AndThen(s => s.ReadWhile(r => r.Lower && r.Indented, r => r.Take()))
                .AndThen(
                    s => s.Require(
                        s.Indented,
                        alias ? SyntaxErrorBranch.AliasIndentEquals : SyntaxErrorBranch.CustomIndentEquals,
                        s.Previous.End))
                .AndThen(s => s.Expect("=", alias ? SyntaxErrorBranch.AliasEquals : SyntaxErrorBranch.CustomEquals))
                .AndThen(
                    s => s.Require(
                        s.Indented,
                        alias
                        ?
                        (s.End ? SyntaxErrorBranch.AliasBody : SyntaxErrorBranch.AliasIndentBody)
                        :
                        SyntaxErrorBranch.CustomIndentAfterEquals,
                        s.Previous.End))
                .AndThen(s => s with { Site = SyntaxErrorSite.TypeAlias })
                .AndThen(s => alias ? s.ReadType() : s.ReadVariants());
        }

        private ValueResult ReadVariants() =>
            ReadVariant().AndThen(
                s => s.ReadWhile(
                    r => r.At("|"),
                    r => r.Take()
                    .AndThen(
                        a => a.Require(
                            a.Indented,
                            a.End ? SyntaxErrorBranch.CustomBar : SyntaxErrorBranch.CustomIndentAfterBar,
                            a.Previous.End))
                    .AndThen(a => a.ReadVariant())));

        private ValueResult ReadVariant() =>
            Require(Upper, SyntaxErrorBranch.CustomVariant)
            .AndThen(s => s.Take())
            .AndThen(s => s.ReadWhile(r => r.Indented && r.IsTypeStart, r => r.ReadTypeAtom()));

        private ValueResult ReadPort() =>
            Require(Indented, SyntaxErrorBranch.PortIndentName, Previous.End)
            .AndThen(s => s.Require(s.Lower, SyntaxErrorBranch.PortName))
            .AndThen(s => s.Take())
            .AndThen(s => s.Require(s.Indented, SyntaxErrorBranch.PortIndentColon, s.Previous.End))
            .AndThen(s => s.Expect(":", SyntaxErrorBranch.PortColon))
            .AndThen(
                s => s.Require(
                    s.Indented,
                    s.End ? SyntaxErrorBranch.PortType : SyntaxErrorBranch.PortIndentType,
                    s.Previous.End))
            .AndThen(s => (s with { Site = SyntaxErrorSite.Port }).ReadType());

        private ValueResult ReadDefinition(bool local)
        {
            var name = Current;
            var oldConstruct = Construct;
            var oldIndent = Indent;

            return
                Take()
                .AndThen(s => s with { Construct = local ? name.Start : oldConstruct, Indent = name.Start.Column })
                .AndThen(s => s.At(":") ? s.Take().AndThen(r => r.ReadAnnotation(name, local)) : s)
                .AndThen(
                    s => s with { Site = local ? SyntaxErrorSite.LetDefinition : SyntaxErrorSite.FunctionArgument })
                .AndThen(s => s.ReadWhile(r => !r.At("="), r => r.ReadDefinitionArgument(local)))
                .AndThen(s => s.Require(s.Indented, SyntaxErrorBranch.DefIndentEquals, s.Previous.End))
                .AndThen(s => s.Take())
                .AndThen(s => s with { Site = SyntaxErrorSite.DefinitionBody })
                .AndThen(
                    s => s.Require(
                        s.Indented,
                        s.End ? SyntaxErrorBranch.DefBody : SyntaxErrorBranch.DefIndentBody,
                        s.Previous.End))
                .AndThen(s => s.ReadExpression())
                .AndThen(s => s with { Indent = oldIndent, Construct = oldConstruct });
        }

        private ValueResult ReadAnnotation(Token name, bool local) =>
            Require(Indented || local, SyntaxErrorBranch.MissingColon, Previous.End)
            .AndThen(s => (s with { Site = SyntaxErrorSite.TypeAnnotation }).ReadType())
            .AndThen(s => s.Require(!s.End && s.Lower, SyntaxErrorBranch.ExpectingDefinition))
            .AndThen(s => s.Take())
            .AndThen(
                s => s.Previous.Text == name.Text
                ?
                s
                :
                s.Fail(
                    SyntaxErrorBranch.ExpectedToken,
                    s.Previous.Start,
                    problem: new ElmSyntaxProblem.AnnotationNameMismatch(
                        new LocatedIdentifier(name.Text, name.Range),
                        s.Previous.Text)))
            .AndThen(
                s => s.At(":")
                ?
                s.Fail(
                    SyntaxErrorBranch.ExpectedToken,
                    problem: new ElmSyntaxProblem.AnnotationNameRepeated(name.Text))
                :
                s);

        private ValueResult ReadDefinitionArgument(bool local) =>
            Require(
                Indented,
                local
                ?
                SyntaxErrorBranch.LetDefEquals
                :
                End ? SyntaxErrorBranch.DefEquals : SyntaxErrorBranch.DefIndentEquals,
                Previous.End)
            .AndThen(
                s => s.Require(
                    !s.At("->") && !s.At("as"),
                    s.At("->") ? SyntaxErrorBranch.DefEqualsArrow : SyntaxErrorBranch.DefEqualsAs))
            .AndThen(s => s.Require(s.IsPatternStart, SyntaxErrorBranch.PatternStart))
            .AndThen(s => s.ReadPatternAtom());

        private ValueResult ReadType() =>
            ReadTypeApplication().AndThen(
                s => s.ReadWhile(
                    r => r.Indented && r.At("->"),
                    r => r.Take().AndThen(a => a.ReadTypeApplication())));

        private ValueResult ReadTypeApplication() =>
            ReadTypeAtom().AndThen(s => s.ReadWhile(r => r.Indented && r.IsTypeStart, r => r.ReadTypeAtom()));

        private ValueResult ReadTypeAtom()
        {
            if (Lower || Upper)
            {
                return
                    Take().AndThen(
                        s => s.ReadWhile(
                            r => r.At("."),
                            r => r.Take().AndThen(a => a.Take())));
            }

            if (At("{"))
                return Take().AndThen(s => s.ReadRecord(true, false, s.Previous.Start));

            if (At("("))
                return Take().AndThen(s => s.ReadSequence(")", true, false, s.Previous.Start));

            return Fail(Site is SyntaxErrorSite.TypeAlias ? SyntaxErrorBranch.TrecordType : SyntaxErrorBranch.TypeStart);
        }

        private ValueResult ReadPattern() =>
            ReadPatternAtom()
            .AndThen(s => s.Indented && s.At("::") ? s.Take().AndThen(r => r.ReadPattern()) : s)
            .AndThen(
                s => s.Indented && s.At("as")
                ?
                s.Take()
                .AndThen(r => r.Require(r.Lower, SyntaxErrorBranch.PatternAlias, r.Previous.End))
                .AndThen(r => r.Take())
                :
                s);

        private ValueResult ReadPatternAtom()
        {
            if (Current.Text.StartsWith('_') && Current.Text.Length > 1)
            {
                return
                    Fail(
                        SyntaxErrorBranch.ExpectedToken,
                        Current.Start,
                        new Location(Current.End.Row, Current.End.Column - 1),
                        problem:
                        new ElmSyntaxProblem.Wildcard(Current.Text, Current.Text.TakeWhile(c => c is '_').Count()));
            }

            if (Current.Kind is FoundSyntaxKind.Literal &&
                char.IsDigit(Current.Text[0]) && Current.Text.Contains('.', StringComparison.Ordinal))
                return Fail(SyntaxErrorBranch.PatternFloat, end: Current.End);

            if (Lower || Upper || At("_"))
            {
                var prefix = Lower || Upper ? new ValueResult(this) : Take();
                return prefix.AndThen(s => s.Previous.Text is "_" && !s.Lower && !s.Upper ? s : s.ReadPatternName());
            }

            if (Current.Kind is FoundSyntaxKind.Literal)
                return Take();

            if (At("("))
                return Take().AndThen(s => s.ReadSequence(")", false, true, s.Previous.Start));

            if (At("["))
                return Take().AndThen(s => s.ReadSequence("]", false, true, s.Previous.Start));

            if (At("{"))
                return Take().AndThen(s => s.ReadRecord(false, true, s.Previous.Start));

            return
                Fail(
                    Site is SyntaxErrorSite.CasePattern
                    ?
                    (Current.Kind is FoundSyntaxKind.Keyword
                    ?
                    SyntaxErrorBranch.CaseReservedPattern
                    :
                    Current.Kind is FoundSyntaxKind.Operator && !At("->")
                    ?
                    SyntaxErrorBranch.CaseUnexpectedOperator
                    :
                    SyntaxErrorBranch.CasePattern)
                    :
                    Current.Kind is FoundSyntaxKind.Keyword
                    ?
                    SyntaxErrorBranch.MissingArgument
                    :
                    SyntaxErrorBranch.FuncArg);
        }

        private ValueResult ReadPatternName()
        {
            var upper = Upper;

            return
                Take().AndThen(
                    s => upper
                    ?
                    s.ReadWhile(
                        r => r.At(".") && r.Current.Start == r.Previous.End,
                        r => r.Take()
                        .AndThen(a => a.Require(a.Upper, SyntaxErrorBranch.PatternStart))
                        .AndThen(a => a.Take()))
                    .AndThen(r => r.ReadWhile(a => a.Indented && a.IsPatternStart, a => a.ReadPatternAtom()))
                    :
                    s);
        }

        private bool HasExpressionContinuation =>
            Indented &&
            !(Site is SyntaxErrorSite.CaseBranch && Current.Start.Row > Previous.End.Row && LooksLikeCaseBranch()) &&
            (At(".") || IsExpressionStart ||
            Current.Kind is FoundSyntaxKind.Operator && !At("$") && !At("->") && !At("|") && !At("=") && !At(":"));

        private ValueResult ReadExpression() =>
            ReadExpressionAtom().AndThen(
                s => s.ReadWhile(r => r.HasExpressionContinuation, r => r.ReadExpressionContinuation()));

        private ValueResult ReadExpressionContinuation()
        {
            if (At("."))
            {
                return
                    Take()
                    .AndThen(
                        s => s.Require(
                            s.Lower && s.Current.Start == s.Previous.End,
                            s.Previous.Start == s.Tokens[Math.Max(0, s.Index - 2)].End
                            ?
                            SyntaxErrorBranch.ExprAccess
                            :
                            SyntaxErrorBranch.ExprBadDot,
                            s.Previous.End))
                    .AndThen(s => s.Take());
            }

            if (IsExpressionStart)
                return ReadExpressionAtom();

            var op = Current;

            return
                Take()
                .AndThen(
                    s => s.Indented && s.IsExpressionStart
                    ?
                    s
                    :
                    s.Fail(
                        op.Text is "?" ? SyntaxErrorBranch.ModuleBadChar : SyntaxErrorBranch.ExprOperatorRight,
                        s.Previous.End,
                        problem: new ElmSyntaxProblem.Grammar(
                            op.Text is "?" ? SyntaxErrorBranch.ModuleBadChar : SyntaxErrorBranch.ExprOperatorRight,
                            op.Found,
                            s.Site)))
                .AndThen(s => s.ReadExpressionAtom());
        }

        private ValueResult ReadExpressionAtom()
        {
            if (Current.LexicalError is not null)
                return Take();

            if (At("-"))
                return Take().AndThen(s => s.ReadExpressionAtom());

            if (At("if"))
                return Take().AndThen(s => s.ReadIf(s.Previous.Start));

            if (At("let"))
                return Take().AndThen(s => s.ReadLet(s.Previous.Start));

            if (At("case"))
                return Take().AndThen(s => s.ReadCase(s.Previous.Start));

            if (At("\\"))
                return Take().AndThen(s => s.ReadLambda(s.Previous.Start));

            if (At("("))
                return Take().AndThen(s => s.ReadSequence(")", false, false, s.Previous.Start));

            if (At("["))
                return Take().AndThen(s => s.ReadSequence("]", false, false, s.Previous.Start));

            if (At("{"))
                return Take().AndThen(s => s.ReadRecord(false, false, s.Previous.Start));

            if (At("."))
            {
                return
                    Take()
                    .AndThen(
                        s =>
                        s.Require(
                            s.Lower && s.Current.Start == s.Previous.End,
                            SyntaxErrorBranch.ExprDot,
                            s.Previous.End))
                    .AndThen(s => s.Take());
            }

            if (Lower || Upper || Current.Kind is FoundSyntaxKind.Literal)
            {
                return
                    Take().AndThen(
                        s => s.ReadWhile(
                            r =>
                            r.Previous.Kind is FoundSyntaxKind.UpperIdentifier && r.At(".") &&
                            r.Current.Start == r.Previous.End,
                            r => r.Take()
                            .AndThen(
                                a => a.Require(
                                    (a.Lower || a.Upper) && a.Current.Start == a.Previous.End,
                                    SyntaxErrorBranch.ExprAccess,
                                    a.Previous.End))
                            .AndThen(a => a.Take())));
            }

            return Fail(SyntaxErrorBranch.UnsupportedExpression);
        }

        private ValueResult ReadLambda(Location start)
        {
            var oldSite = Site;

            return
                Require(Indented, SyntaxErrorBranch.FuncMissingArgument, Previous.End, start)
                .AndThen(s => (s with { Site = SyntaxErrorSite.FunctionArgument }).ReadPattern())
                .AndThen(s => s.ReadWhile(r => r.Indented && !r.At("->") && r.IsPatternStart, r => r.ReadPattern()))
                .AndThen(
                    s => s.Expect(
                        "->",
                        SyntaxErrorBranch.FuncArrow,
                        s.End ? s.Previous.End : s.Current.Start,
                        start))
                .AndThen(s => s.Require(s.Indented, SyntaxErrorBranch.FuncBody, s.Previous.End, start))
                .AndThen(s => (s with { Site = oldSite }).ReadExpression());
        }

        private ValueResult ReadIf(Location start, bool elseIf = false) =>
            Require(IsExpressionStart, SyntaxErrorBranch.IfCondition, context: start)
            .AndThen(s => s.ReadExpression())
            .AndThen(
                s => s.Expect(
                    "then",
                    elseIf ? SyntaxErrorBranch.WeirdElse : SyntaxErrorBranch.IfThen,
                    s.End ? s.Previous.End : s.Current.Start,
                    start))
            .AndThen(s => s.Require(s.IsExpressionStart, SyntaxErrorBranch.IfThenBranch, context: start))
            .AndThen(s => s.ReadExpression())
            .AndThen(
                s =>
                s.At("else") && s.Current.Start.Column < start.Column
                ?
                s.Fail(SyntaxErrorBranch.WeirdElseBranch, s.Current.Start, s.Current.End, context: start)
                :
                s)
            .AndThen(s => s.Expect("else", SyntaxErrorBranch.IfElse, s.End ? s.Previous.End : s.Current.Start, start))
            .AndThen(s => s.Require(s.Indented, SyntaxErrorBranch.IfElseBranch, s.Previous.End, start))
            .AndThen(s => s.At("if") ? s.Take().AndThen(r => r.ReadIf(start, true)) : s.ReadExpression());

        private ValueResult ReadLet(Location start)
        {
            var outerIndent = Indent;
            var definitionIndent = Current.Start.Column;

            return
                Require(
                    IsPatternStart,
                    Current.Kind is FoundSyntaxKind.Keyword
                    ?
                    SyntaxErrorBranch.LetDefName
                    :
                    SyntaxErrorBranch.LetProblem,
                    context: start)
                .AndThen(s => s.ReadLetDefinition(start, outerIndent))
                .AndThen(
                    s => s.ReadWhile(
                        r => !r.End && r.IsPatternStart && r.Current.Start.Column == definitionIndent &&
                            r.Current.Start.Row > r.Previous.End.Row,
                        r => r.ReadLetDefinition(start, outerIndent)))
                .AndThen(
                    s => s.Expect(
                        "in",
                        s.IsPatternStart && s.Current.Start.Column > outerIndent
                        ?
                        SyntaxErrorBranch.LetProblemAlignment
                        :
                        SyntaxErrorBranch.LetIn,
                        s.End ? s.Previous.End : s.Current.Start,
                        start))
                .AndThen(s => s with { Indent = outerIndent })
                .AndThen(s => s.Require(s.Indented, SyntaxErrorBranch.LetBody, s.Previous.End, start))
                .AndThen(s => s.ReadExpression());
        }

        private ValueResult ReadLetDefinition(Location start, int outerIndent)
        {
            if (Lower)
                return ReadDefinition(true);

            var oldSite = Site;

            return
                (this with { Indent = Current.Start.Column, Site = SyntaxErrorSite.LetDefinition }).ReadPattern()
                .AndThen(s => s.Expect("=", SyntaxErrorBranch.LetDefEquals, context: start))
                .AndThen(s => s.ReadExpression())
                .AndThen(s => s with { Indent = outerIndent, Site = oldSite });
        }

        private ValueResult ReadCase(Location start) =>
            ReadExpression()
            .AndThen(s => s.Expect("of", SyntaxErrorBranch.CaseOf, s.End ? s.Previous.End : s.Current.Start, start))
            .AndThen(s => s.ReadCaseBranches(start));

        private ValueResult ReadCaseBranches(Location start)
        {
            var outerSite = Site;
            var outerIndent = Indent;

            return
                ReadCaseBranch(start)
                .AndThen(
                    s => s.ReadWhile(
                        r => !r.End && r.Current.Start.Column >= outerIndent &&
                            (outerSite is not SyntaxErrorSite.CaseBranch || r.Current.Start.Column > outerIndent) &&
                            r.Current.Start.Row > r.Previous.End.Row && r.LooksLikeCaseBranch(),
                        r => r.ReadCaseBranch(start)))
                .AndThen(s => s with { Indent = outerIndent, Site = outerSite });
        }

        private ValueResult ReadCaseBranch(Location start)
        {
            var branchIndent = Current.Start.Column;

            return
                (this with { Site = SyntaxErrorSite.CasePattern }).ReadPattern()
                .AndThen(
                    s => s.Expect(
                        "->",
                        s.At(":")
                        ?
                        SyntaxErrorBranch.CaseArrowColon
                        :
                        s.Current.Kind is FoundSyntaxKind.Operator
                        ?
                        SyntaxErrorBranch.CaseOperator
                        :
                        SyntaxErrorBranch.CaseArrow,
                        context: start))
                .AndThen(s => s with { Indent = branchIndent, Site = SyntaxErrorSite.CaseBranch })
                .AndThen(s => s.Require(s.Indented, SyntaxErrorBranch.CaseBranch, s.Previous.End, start))
                .AndThen(s => s.ReadExpression());
        }

        private bool LooksLikeCaseBranch()
        {
            if (!IsPatternStart)
                return false;

            var tokens = Tokens;
            var row = Current.Start.Row;

            var scan =
                Repeat(
                    (Index, Nesting: 0, Answer: (bool?)null),
                    state => state.Answer is null && state.Index < tokens.Length && tokens[state.Index].Start.Row == row,
                    state =>
                    {
                        var text = tokens[state.Index].Text;

                        var nesting =
                            state.Nesting + (text is "(" or "[" or "{" ? 1 : text is ")" or "]" or "}" ? -1 : 0);

                        bool? answer =
                            nesting < 0 || nesting is 0 && text is "=" or ":" or "\\"
                            ?
                            false
                            :
                            nesting is 0 && text is "->" ? true : null;

                        return (state.Index + 1, nesting, answer);
                    },
                    Tokens.Length - Index);

            return scan.Answer is true;
        }

        private ValueResult ReadSequence(string close, bool type, bool pattern, Location start)
        {
            if (At(close))
                return Take();

            var list = close is "]";

            if (!type && !pattern && !list && (Current.Kind is FoundSyntaxKind.Operator || At("=")) &&
                !(At("-") && Index + 1 < Tokens.Length && Tokens[Index + 1].Start == Current.End && Tokens[Index + 1].Text != close))
            {
                return
                    Require(
                        !At("=") && !At(":") && !At("->") && !At("|"),
                        SyntaxErrorBranch.TupleOpReserved,
                        context: start)
                    .AndThen(s => s.Take())
                    .AndThen(s => s.Expect(close, SyntaxErrorBranch.TupleOperatorClose, s.Previous.End, start));
            }

            return
                Repeat(
                    new Items(this),
                    item => item.Result.Error is null && !item.Done,
                    item => ReadSequenceItem(item, close, type, pattern, start),
                    Tokens.Length - Index).Result;
        }

        private static Items ReadSequenceItem(Items item, string close, bool type, bool pattern, Location start)
        {
            var reader = item.Result.State;
            var list = close is "]";

            var openBranch =
                type
                ?
                (item.Count == 0 ? SyntaxErrorBranch.TtupleOpen : SyntaxErrorBranch.TtupleType)
                :
                pattern
                ?
                (list
                ?
                (item.Count == 0 ? SyntaxErrorBranch.PlistOpen : SyntaxErrorBranch.PlistExpr)
                :
                item.Count == 0 ? SyntaxErrorBranch.PtupleOpen : SyntaxErrorBranch.PtupleExpr)
                :
                list
                ?
                (item.Count == 0 ? SyntaxErrorBranch.ListOpen : SyntaxErrorBranch.ListTrailingComma)
                :
                SyntaxErrorBranch.TupleExpr;

            var element =
                reader.Require(
                    reader.Indented || reader.At(close),
                    type
                    ?
                    (item.Count > 0 ? SyntaxErrorBranch.UnfinishedTupleType : SyntaxErrorBranch.TtupleEnd)
                    :
                    pattern
                    ?
                    (list
                    ?
                    SyntaxErrorBranch.PlistEnd
                    :
                    item.Count > 0 ? SyntaxErrorBranch.UnfinishedTuplePattern : SyntaxErrorBranch.PtupleEnd)
                    :
                    list
                    ?
                    SyntaxErrorBranch.ListEnd
                    :
                    item.Count > 0 ? SyntaxErrorBranch.UnfinishedTuple : SyntaxErrorBranch.TupleEnd,
                    reader.Previous.End,
                    start)
                .AndThen(
                    s =>
                    s.Require(
                        type ? s.IsTypeStart : pattern ? s.IsPatternStart : s.IsExpressionStart,
                        openBranch,
                        context: start))
                .AndThen(s => type ? s.ReadType() : pattern ? s.ReadPattern() : s.ReadExpression());

            if (element.Error is not null)
                return item with { Result = element };

            var count = item.Count + 1;
            var after = element.State;

            return
                after.At(close)
                ?
                new(after.Take(), count, true)
                :
                new(
                    after.Expect(
                        ",",
                        type
                        ?
                        (count is 1 ? SyntaxErrorBranch.TtupleEnd : SyntaxErrorBranch.TtupleFinishedMissing)
                        :
                        pattern
                        ?
                        (list
                        ?
                        SyntaxErrorBranch.PlistEnd
                        :
                        count is 1 ? SyntaxErrorBranch.PtupleEnd : SyntaxErrorBranch.PtupleFinishedMissing)
                        :
                        list
                        ?
                        SyntaxErrorBranch.ListEnd
                        :
                        count is 1 ? SyntaxErrorBranch.TupleEnd : SyntaxErrorBranch.TupleFinishedMissing,
                        after.End ? after.Previous.End : after.Current.Start,
                        start),
                    count);
        }

        private ValueResult ReadRecord(bool type, bool pattern, Location start)
        {
            if (At("}"))
                return Take();

            return
                Repeat(
                    new Items(this),
                    item => item.Result.Error is null && !item.Done,
                    item => ReadRecordItem(item, type, pattern, start),
                    Tokens.Length - Index).Result;
        }

        private static Items ReadRecordItem(Items item, bool type, bool pattern, Location start)
        {
            var reader = item.Result.State;

            var name =
                reader.Require(
                    reader.Lower,
                    type
                    ?
                    (item.Count == 0
                    ?
                    (reader.End ? SyntaxErrorBranch.CustomVariantArg : SyntaxErrorBranch.TrecordOpen)
                    :
                    reader.At(",")
                    ?
                    SyntaxErrorBranch.TrecordExtraComma
                    :
                    reader.At("}") ? SyntaxErrorBranch.TrecordTrailingComma : SyntaxErrorBranch.TrecordField)
                    :
                    pattern
                    ?
                    (item.Count == 0 ? SyntaxErrorBranch.PrecordOpen : SyntaxErrorBranch.PrecordField)
                    :
                    item.Count == 0
                    ?
                    SyntaxErrorBranch.RecordOpen
                    :
                    reader.At(",")
                    ?
                    SyntaxErrorBranch.RecordExtraComma
                    :
                    reader.At("}") ? SyntaxErrorBranch.RecordTrailingComma : SyntaxErrorBranch.RecordFieldKeyword,
                    reader.End ? reader.Previous.End : reader.Current.Start,
                    start)
                .AndThen(s => s.Take());

            if (name.Error is not null)
                return item with { Result = name };

            if (!pattern && item.Count == 0 && name.State.At("|"))
                return item with { Result = name.State.Take() };

            var value = pattern ? name : name.AndThen(s => s.ReadRecordValue(type, start));

            var closing =
                value.AndThen(
                    s => s.Require(
                        !s.At("}") || s.Current.Start.Row <= s.Previous.End.Row || s.Current.Start.Column > s.Indent,
                        type ? SyntaxErrorBranch.NeedIndentRecordType : SyntaxErrorBranch.NeedIndentRecord,
                        context: start));

            if (closing.Error is not null)
                return item with { Result = closing };

            var after = closing.State;

            return
                after.At("}")
                ?
                new(after.Take(), item.Count + 1, true)
                :
                new(
                    after.Expect(
                        ",",
                        type
                        ?
                        SyntaxErrorBranch.TrecordEnd
                        :
                        pattern ? SyntaxErrorBranch.PrecordEnd : SyntaxErrorBranch.RecordEnd,
                        after.End ? after.Previous.End : after.Current.Start,
                        start),
                    item.Count + 1);
        }

        private ValueResult ReadRecordValue(bool type, Location start) =>
            // Pine deliberately canonicalizes either separator in record expressions/types.
            (At(":") || !type && At("=")
            ?
            Take()
            :
            Fail(type ? SyntaxErrorBranch.TrecordColon : SyntaxErrorBranch.RecordEquals, context: start))
            .AndThen(
                s => s.Require(
                    type ? s.IsTypeStart : s.IsExpressionStart,
                    type ? SyntaxErrorBranch.TrecordType : SyntaxErrorBranch.RecordExpr,
                    context: start))
            .AndThen(s => type ? s.ReadType() : s.ReadExpression());

        private ValueResult ReadTrailing() =>
            Fail(
                Current.Text switch
                {
                    "->" => SyntaxErrorBranch.ExprBadArrow,
                    ":" => SyntaxErrorBranch.ExprBadColon,
                    "=" => SyntaxErrorBranch.ExprBadEquals,
                    "|" => SyntaxErrorBranch.ExprBadPipe,
                    "`" => SyntaxErrorBranch.ModuleBadBacktick,
                    "," => SyntaxErrorBranch.ModuleBadComma,
                    "$" => SyntaxErrorBranch.ModuleBadDollar,
                    ";" => SyntaxErrorBranch.ModuleBadSemicolon,

                    _ =>
                    SyntaxErrorBranch.UnexpectedDeclarationToken,
                },
                end: At("->") ? Current.End : null);
    }

    private static bool Keyword(string text) =>
        text is "module" or "exposing" or "import" or "as" or
        "type" or "port" or "if" or "then" or "else" or "let" or "in" or "case" or "of";

    private static readonly SearchValues<char> s_identifierCharacters =
        SearchValues.Create("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_");

    private static readonly SearchValues<char> s_digits = SearchValues.Create("0123456789");

    private static readonly SearchValues<char> s_hexDigits = SearchValues.Create("0123456789abcdefABCDEF");

    private static readonly SearchValues<char> s_operators = SearchValues.Create("+-/*=<>:&|^?%!$.");

    private static readonly SearchValues<char> s_stringSpecialCharacters = SearchValues.Create("\"\\\0\r\n");

    private static readonly SearchValues<char> s_charSpecialCharacters = SearchValues.Create("'\\\0\r\n");

    private static readonly SearchValues<char> s_commentSpecialCharacters = SearchValues.Create("{-\0\r\n");

    private static readonly SearchValues<char> s_shaderSpecialCharacters = SearchValues.Create("|\0\r\n");

    private static readonly SearchValues<char> s_spaces = SearchValues.Create(" \v\f");

    private readonly record struct Lexer(string Source, int Offset = 0, int Row = 1, int Column = 1)
    {
        public Location Here => new(Row, Column);

        public bool End => Offset >= Source.Length;

        public char Peek(int ahead = 0) => ahead < Source.Length - Offset ? Source[Offset + ahead] : '\0';

        public Lexer Advance()
        {
            if (End)
                return this;

            var newline = Peek() is '\r' or '\n';

            return
                this with
                {
                    Offset = Offset + (Peek() is '\r' && Peek(1) is '\n' ? 2 : 1),
                    Row = newline ? Row + 1 : Row,
                    Column = newline ? 1 : Column + 1
                };
        }

        public Lexer SkipAscii(SearchValues<char> characters)
        {
            var other = Source.AsSpan(Offset).IndexOfAnyExcept(characters);
            var length = other < 0 ? Source.Length - Offset : other;
            return this with { Offset = Offset + length, Column = Column + length };
        }

        private Lexer SkipIdentifier() =>
            Repeat(
                SkipAscii(s_identifierCharacters),
                s => char.IsLetterOrDigit(s.Peek()),
                s => s.Advance().SkipAscii(s_identifierCharacters),
                Source.Length - Offset);

        private Lexer SkipDigits() =>
            Repeat(
                SkipAscii(s_digits),
                s => char.IsDigit(s.Peek()),
                s => s.Advance().SkipAscii(s_digits),
                Source.Length - Offset);

        private Lexer AdvanceChunk(SearchValues<char> specialCharacters)
        {
            var special = Source.AsSpan(Offset).IndexOfAny(specialCharacters);
            var length = special < 0 ? Source.Length - Offset : special;
            return length is 0 ? Advance() : this with { Offset = Offset + length, Column = Column + length };
        }

        public Lexeme TokenTo(
            Lexer end,
            FoundSyntaxKind kind = FoundSyntaxKind.Character,
            ElmSyntaxParseError? error = null) =>
            new(end, new Token(Source[Offset..end.Offset], new Range(Here, end.Here), kind, error));

        public Lexeme Read()
        {
            var c = Peek();

            if (c is '\t')
                return TokenTo(Advance(), error: Grammar(Here, Here, SyntaxErrorBranch.Tab));

            if (char.IsWhiteSpace(c))
            {
                return
                    new(
                        Repeat(
                            SkipAscii(s_spaces),
                            s => s.Peek() is not '\t' && char.IsWhiteSpace(s.Peek()),
                            s => s.Advance().SkipAscii(s_spaces),
                            Source.Length - Offset),
                        null);
            }

            if (c is '-' && Peek(1) is '-')
            {
                var end = Source.AsSpan(Offset).IndexOfAny('\0', '\r', '\n');
                var length = end < 0 ? Source.Length - Offset : end;
                return new(this with { Offset = Offset + length, Column = Column + length }, null);
            }

            if (c is '{' && Peek(1) is '-')
                return ReadComment();

            if (c is '[' && Source.AsSpan(Offset).StartsWith("[glsl|"))
                return ReadShader();

            if (c is '"' or '\'')
                return ReadLiteral();

            if (char.IsDigit(c))
                return ReadNumber();

            if (char.IsLetter(c) || c is '_')
            {
                var end = Advance().SkipIdentifier();
                var text = Source[Offset..end.Offset];

                return
                    TokenTo(
                        end,
                        Keyword(text)
                        ?
                        FoundSyntaxKind.Keyword
                        :
                        char.IsUpper(c) ? FoundSyntaxKind.UpperIdentifier : FoundSyntaxKind.LowerIdentifier);
            }

            var first = Advance();
            var isOperator = "+-/*=<>:&|^?%!$".Contains(c, StringComparison.Ordinal);

            var rest =
                isOperator
                ?
                first.SkipAscii(s_operators)
                :
                first;

            return
                TokenTo(
                    c is '.' && rest.Peek() is '.' ? rest.Advance() : rest,
                    isOperator
                    ?
                    FoundSyntaxKind.Operator
                    :
                    "()[]{}".Contains(c, StringComparison.Ordinal)
                    ?
                    FoundSyntaxKind.Delimiter
                    :
                    FoundSyntaxKind.Punctuation);
        }

        private Lexeme ReadComment()
        {
            var comment =
                Repeat(
                    (Cursor: Advance().Advance(), Depth: 1),
                    s => s.Depth > 0 && s.Cursor.Peek() is not '\0',
                    s => s.Cursor.Peek() is '{' && s.Cursor.Peek(1) is '-'
                    ?
                    (s.Cursor.Advance().Advance(), s.Depth + 1)
                    :
                    s.Cursor.Peek() is '-' && s.Cursor.Peek(1) is '}'
                    ?
                    (s.Cursor.Advance().Advance(), s.Depth - 1)
                    :
                    (s.Cursor.AdvanceChunk(s_commentSpecialCharacters), s.Depth),
                    Source.Length - Offset);

            return
                comment.Depth is not 0
                ?
                TokenTo(
                    comment.Cursor,
                    error: Grammar(Here, new Location(Row, Column + 2), SyntaxErrorBranch.EndlessComment))
                :
                Source.AsSpan(Offset).StartsWith("{-|") ? TokenTo(comment.Cursor) : new(comment.Cursor, null);
        }

        private Lexeme ReadShader()
        {
            var body = this with { Offset = Offset + 6, Column = Column + 6 };

            var end =
                Repeat(
                    body,
                    s => s.Peek() is not '\0' && !(s.Peek() is '|' && s.Peek(1) is ']'),
                    s => s.AdvanceChunk(s_shaderSpecialCharacters),
                    Source.Length - Offset);

            if (end.Peek() is '\0')
            {
                return
                    TokenTo(
                        end,
                        FoundSyntaxKind.Literal,
                        Error(Here, Here, new ElmSyntaxProblem.Shader(ShaderProblem.Endless)));
            }

            var after = end.Advance().Advance();
            var glsl = Source[(Offset + 6)..end.Offset];
            var problem = CheckShaderStructure(glsl);

            return
                TokenTo(
                    after,
                    FoundSyntaxKind.Literal,
                    problem is { } shaderProblem
                    ?
                    Error(
                        new Location(after.Row, after.Column - 2),
                        new Location(after.Row, after.Column - 2),
                        new ElmSyntaxProblem.Shader(
                            ShaderProblem.Invalid,
                            shaderProblem,
                            new Location(end.Row - Row + 1, end.Row == Row ? end.Column - Column - 5 : end.Column)))
                    :
                    null);
        }

        private Lexeme ReadLiteral()
        {
            var quote = Peek();
            var first = Advance();
            var triple = quote is '"' && first.Peek() is '"' && first.Peek(1) is '"';
            var content = triple ? first.Advance().Advance() : first;

            var literal =
                Repeat(
                    new LiteralState(content),
                    s =>
                    !s.Closed && s.Cursor.Peek() is not '\0' && (triple || s.Cursor.Peek() is not '\r' and not '\n'),
                    s => ReadLiteralCharacter(s, quote, triple),
                    Source.Length - Offset);

            var error =
                literal.Error ??
                (!literal.Closed
                ?
                Grammar(
                    triple ? Here : literal.Cursor.Here,
                    triple ? new Location(Row, Column + 3) : literal.Cursor.Here,
                    triple
                    ?
                    SyntaxErrorBranch.MultistringEndless
                    :
                    quote is '\'' ? SyntaxErrorBranch.CharEndless : SyntaxErrorBranch.StringEndless)
                :
                quote is '\'' && literal.CharacterCount is not 1
                ?
                Grammar(Here, literal.Cursor.Here, SyntaxErrorBranch.CharNotString)
                :
                null);

            return TokenTo(literal.Cursor, FoundSyntaxKind.Literal, error);
        }

        private Lexeme ReadNumber()
        {
            var digits = Advance().SkipDigits();
            var integer = Source[Offset..digits.Offset];

            ElmSyntaxParseError? error =
                integer.Length > 1 && Peek() is '0'
                ?
                Error(
                    new Location(Row, Column + 1),
                    new Location(Row, Column + 1),
                    new ElmSyntaxProblem.Number(NumberProblem.LeadingZero, integer))
                :
                null;

            if (integer is "0" && digits.Peek() is 'x')
            {
                var hexBegin = digits.Advance();
                var hexEnd = hexBegin.SkipAscii(s_hexDigits);

                return
                    TokenTo(
                        hexEnd,
                        FoundSyntaxKind.Literal,
                        hexEnd.Offset == hexBegin.Offset
                        ?
                        Error(hexEnd.Here, hexEnd.Here, new ElmSyntaxProblem.Number(NumberProblem.Hex, integer))
                        :
                        error);
            }

            var fraction = digits.Peek() is '.' ? digits.Advance().SkipDigits() : digits;

            var fractionError =
                digits.Peek() is '.' && !char.IsDigit(digits.Peek(1))
                ?
                Error(digits.Here, digits.Here, new ElmSyntaxProblem.Number(NumberProblem.Dot, integer))
                :
                error;

            if (fraction.Peek() is not 'e' and not 'E')
                return TokenTo(fraction, FoundSyntaxKind.Literal, fractionError);

            var exponent = fraction.Advance();
            var exponentDigits = exponent.Peek() is '-' or '+' ? exponent.Advance() : exponent;
            var exponentEnd = exponentDigits.SkipDigits();

            return
                TokenTo(
                    exponentEnd,
                    FoundSyntaxKind.Literal,
                    !char.IsDigit(exponentDigits.Peek())
                    ?
                    Error(
                        exponentDigits.Here,
                        exponentDigits.Here,
                        new ElmSyntaxProblem.Number(NumberProblem.End, integer))
                    :
                    fractionError);
        }
    }

    private readonly record struct Lexeme(Lexer Cursor, Token? Token);

    private readonly record struct LiteralState(
        Lexer Cursor, int CharacterCount = 0, bool Closed = false, ElmSyntaxParseError? Error = null);

    private static LiteralState ReadLiteralCharacter(LiteralState literal, char quote, bool triple)
    {
        var cursor = literal.Cursor;

        if (cursor.Peek() == quote && (!triple || cursor.Peek(1) == quote && cursor.Peek(2) == quote))
            return literal with { Cursor = triple ? cursor.Advance().Advance().Advance() : cursor.Advance(), Closed = true };

        var special =
            cursor.Source.AsSpan(cursor.Offset).IndexOfAny(
                quote is '"' ? s_stringSpecialCharacters : s_charSpecialCharacters);

        var ordinaryLength = special < 0 ? cursor.Source.Length - cursor.Offset : special;

        if (ordinaryLength > 0)
        {
            return
                literal with
                {
                    Cursor =
                    cursor with { Offset = cursor.Offset + ordinaryLength, Column = cursor.Column + ordinaryLength },

                    // Only the distinction between zero, one and multiple characters is observable.
                    CharacterCount =
                    Math.Min(
                        2,
                        literal.CharacterCount +
                        (ordinaryLength is 1 ||
                        ordinaryLength is 2 &&
                        char.IsSurrogatePair(cursor.Source, cursor.Offset)
                        ?
                        1
                        :
                        2))
                };
        }

        var first = cursor.Advance();
        var counted = literal with { CharacterCount = literal.CharacterCount + 1 };

        if (cursor.Peek() is not '\\')
        {
            return
                counted with
                {
                    Cursor =
                    char.IsHighSurrogate(cursor.Peek()) && char.IsLowSurrogate(first.Peek()) ? first.Advance() : first
                };
        }

        var escape = first.Peek();

        if (escape is '\0')
            return counted with { Cursor = first };

        var escaped = first.Advance();

        if (escape is 'n' or 'r' or 't' or '"' or '\'' or '\\')
            return counted with { Cursor = escaped };

        if (escape is not 'u')
        {
            return
                counted with
                {
                    Cursor = escaped,
                    Error =
                    literal.Error ??
                    Error(cursor.Here, escaped.Here, new ElmSyntaxProblem.Escape(EscapeProblem.Unknown))
                };
        }

        if (escaped.Peek() is not '{')
        {
            return
                counted with
                {
                    Cursor = escaped,
                    Error =
                    literal.Error ??
                    Error(cursor.Here, escaped.Here, new ElmSyntaxProblem.Escape(EscapeProblem.UnicodeFormat))
                };
        }

        var digitsBegin = escaped.Advance();
        var digitsEnd = digitsBegin.SkipAscii(s_hexDigits);
        var digits = cursor.Source[digitsBegin.Offset..digitsEnd.Offset];
        var hasClose = digitsEnd.Peek() is '}';
        var after = hasClose ? digitsEnd.Advance() : digitsEnd;
        var code = ParseHex(digits);

        EscapeProblem? problem =
            !hasClose
            ?
            EscapeProblem.UnicodeFormat
            :
            digits.Length < 4
            ?
            EscapeProblem.UnicodeShort
            :
            digits.Length > 6
            ?
            EscapeProblem.UnicodeLong
            :
            code is not (>= 0 and <= 0x10ffff and not (>= 0xd800 and <= 0xdfff)) ? EscapeProblem.UnicodeCode : null;

        return
            counted with
            {
                Cursor = after,
                Error =
                literal.Error ??
                (problem is { } p
                ?
                Error(cursor.Here, after.Here, new ElmSyntaxProblem.Escape(p, digits.Length, code))
                :
                null)
            };
    }

    private static int? ParseHex(string digits)
    {
        var significant = digits.AsSpan().IndexOfAnyExcept('0');

        if (digits.Length == 0 || significant >= 0 && digits.Length - significant > 8)
            return null;

        return
            significant < 0
            ?
            0
            :
            Enumerable.Range(significant, digits.Length - significant).Aggregate(
                0,
                (value, index) => unchecked((value << 4) | (digits[index] switch
                {
                    >= '0' and <= '9' => digits[index] - '0',
                    >= 'a' and <= 'f' => digits[index] - 'a' + 10,
                    _ => digits[index] - 'A' + 10
                })));
    }

    private static ElmSyntaxParseError Error(Location start, Location end, ElmSyntaxProblem problem) =>
        new(new Range(start, end), null, new ElmSyntaxErrorKind.Parse(problem));

    private static ElmSyntaxParseError Grammar(Location start, Location end, SyntaxErrorBranch branch) =>
        Error(start, end, new ElmSyntaxProblem.Grammar(branch));

    private static ImmutableArray<Token> Lex(string source)
    {
        var lexed =
            Repeat(
                (Cursor: new Lexer(source), Tokens: ImmutableStack<Token>.Empty),
                state => !state.Cursor.End,
                state =>
                {
                    var next = state.Cursor.Read();
                    return (next.Cursor, next.Token is { } token ? state.Tokens.Push(token) : state.Tokens);
                },
                source.Length);

        var end = new Token("", new Range(lexed.Cursor.Here, lexed.Cursor.Here), FoundSyntaxKind.EndOfFile);
        return [.. lexed.Tokens.Push(end).Reverse()];
    }

    private readonly record struct ShaderState(
        int Index, ImmutableStack<char> Delimiters, int TopLevelIdentifiers = 0,
        bool Terminated = true, ShaderSyntaxProblem? Problem = null);

    private static ShaderSyntaxProblem? CheckShaderStructure(string source)
    {
        var result =
            Repeat(
                new ShaderState(0, []),
                state => state.Problem is null && state.Index < source.Length,
                state => ReadShaderCharacter(source, state),
                source.Length);

        if (result.Problem is { } problem)
            return problem;

        if (!result.Delimiters.IsEmpty)
            return ShaderSyntaxProblem.UnclosedDelimiter;

        return
            result.Terminated
            ?
            null
            :
            result.TopLevelIdentifiers is 1
            ?
            ShaderSyntaxProblem.ExpectedDeclarator
            :
            ShaderSyntaxProblem.UnfinishedDeclaration;
    }

    private static ShaderState ReadShaderCharacter(string source, ShaderState state)
    {
        var c = source[state.Index];
        var next = state with { Index = state.Index + 1 };

        if (char.IsWhiteSpace(c))
            return next;

        if (c is '#' || c is '/' && state.Index + 1 < source.Length && source[state.Index + 1] is '/')
        {
            var newline = source.AsSpan(state.Index).IndexOfAny('\r', '\n');
            return state with { Index = newline < 0 ? source.Length : state.Index + newline + 1 };
        }

        if (c is '/' && state.Index + 1 < source.Length && source[state.Index + 1] is '*')
        {
            var close = source.AsSpan(state.Index + 2).IndexOf("*/", StringComparison.Ordinal);

            return
                close < 0
                ?
                state with { Problem = ShaderSyntaxProblem.UnclosedDelimiter }
                :
                state with { Index = state.Index + close + 4 };
        }

        if (char.IsLetter(c) || c is '_')
        {
            var end =
                Repeat(
                    state.Index + 1,
                    index => index < source.Length && (char.IsLetterOrDigit(source[index]) || source[index] is '_'),
                    index => index + 1,
                    source.Length - state.Index);

            return
                state with
                {
                    Index = end,
                    TopLevelIdentifiers = state.TopLevelIdentifiers + (state.Delimiters.IsEmpty ? 1 : 0),
                    Terminated = false
                };
        }

        if (c is '(' or '[' or '{')
            return next with { Delimiters = state.Delimiters.Push(c), Terminated = false };

        if (c is ')' or ']' or '}')
        {
            if (state.Delimiters.IsEmpty ||
                (state.Delimiters.Peek(), c) is not (('(', ')') or ('[', ']') or ('{', '}')))
                return next with { Problem = ShaderSyntaxProblem.UnexpectedDelimiter };

            var delimiters = state.Delimiters.Pop();

            return
                next with
                {
                    Delimiters = delimiters,
                    Terminated = c is '}' && delimiters.IsEmpty || state.Terminated,
                    TopLevelIdentifiers = c is '}' && delimiters.IsEmpty ? 0 : state.TopLevelIdentifiers
                };
        }

        return c is ';' && state.Delimiters.IsEmpty ? next with { Terminated = true, TopLevelIdentifiers = 0 } : next;
    }
}
