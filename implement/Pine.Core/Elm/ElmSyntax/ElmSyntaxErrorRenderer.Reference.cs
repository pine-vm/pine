using System;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax;

// Reference presentation is intentionally separate from grammar facts.
public static partial class ElmSyntaxErrorRenderer
{
    private enum ReferenceMessage
    {
        AliasBody, AliasEquals, AliasIndentBody, AliasIndentEquals, AliasName, CaseArrow, CaseArrowColon, CaseBranch, CaseOf, CaseOperator, CasePattern, CaseReservedPattern, CaseUnexpectedOperator, CharEndless, CharEscape, CharNotString, CustomBar, CustomEquals, CustomIndentAfterBar, CustomIndentAfterEquals, CustomIndentEquals, CustomName, CustomVariant, CustomVariantArg, DeclExpecting, DeclImportIndent, DeclReserved, DeclSymbol, DeclUpper, DefBody, DefEquals, DefEqualsArrow, DefEqualsAs, DefIndentBody, DefIndentEquals, DefNameMatch, DefNameRepeat, DocCommentFresh, EndlessComment, ExpectingDefinition, ExposingEnd, ExposingOperatorClose, ExposingOperatorEmpty, ExposingOperatorReserved, ExposingTrailingComma, ExposingTypePrivacy, ExposingValueKeyword, ExposingValueSymbol, ExprAccess, ExprBadArrow, ExprBadColon, ExprBadDot, ExprBadEquals, ExprBadPipe, ExprDot, ExprOperatorRight, FreshModule, FreshType, FuncArg, FuncArrow, FuncBody, IfCondition, IfElse, IfElseBranch, IfThen, IfThenBranch, ImportAlias, ImportEnd, ImportExposing, ImportName, LetBody, LetDefEquals, LetDefName, LetIn, LetProblem, ListEnd, ListExpr, ListOpen, ListTrailingComma, MissingArgument, MissingColon, ModuleBadBacktick, ModuleBadChar, ModuleBadComma, ModuleBadDollar, ModuleBadSemicolon, ModuleEndClose, ModuleEndComma, ModuleEndSemicolon, ModuleExposingStart, ModuleName, ModuleNameMismatch, ModuleNameMissing, ModuleProblem, MultistringEndless, NeedIndentRecord, NeedIndentRecordType, NoPorts, NumberDot, NumberEnd, NumberHex, NumberLeadingZero, PatternAlias, PatternFloat, PatternStart, PatternWildcard, PlistEnd, PlistExpr, PlistOpen, PortColon, PortIndentColon, PortIndentName, PortIndentType, PortModuleExposing, PortModuleName, PortModuleProblem, PortName, PortType, PrecordEnd, PrecordField, PrecordOpen, PtupleEnd, PtupleExpr, PtupleFinishedMissing, PtupleOpen, RecordEnd, RecordEquals, RecordExpr, RecordExtraComma, RecordFieldKeyword, RecordOpen, RecordTrailingComma, ShaderEndless, ShaderProblem, StringEndless, StringUnicodeCode, StringUnicodeFormat, StringUnicodeLong, StringUnicodeShort, Tab, TrecordColon, TrecordEnd, TrecordExtraComma, TrecordField, TrecordOpen, TrecordTrailingComma, TrecordType, TtupleEnd, TtupleFinishedMissing, TtupleOpen, TtupleType, TupleEnd, TupleExpr, TupleFinishedMissing, TupleOpReserved, TupleOperatorClose, TypeStart, UnexpectedPort, WeirdElse,
        FuncMissingArgument, LetProblemAlignment, StrayCurlyBrace, StraySquareBracket,
        UnfinishedTuple, UnfinishedTuplePattern, UnfinishedTupleType, WeirdElseBranch
    }

    private static readonly Presentation[] s_referencePresentations =
        [.. Enum.GetValues<ReferenceMessage>().Select(CreateReferencePresentation)];

    private static readonly Presentation s_unexpectedSyntax =
        new("SYNTAX PROBLEM", false, [T("Unexpected syntax at this location.")]);

    private static Presentation ReferencePresentation(ReferenceMessage message) =>
        s_referencePresentations[(int)message];

    private static Presentation CreateReferencePresentation(ReferenceMessage message) =>
        message switch
        {
            ReferenceMessage.AliasBody or ReferenceMessage.AliasIndentBody =>
            new(
                "UNFINISHED TYPE ALIAS",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a type alias, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a type next. Something as simple as "),
                    S("Int", "yellow", false, false),
                    T(" or "),
                    S("Float", "yellow", false, false),
                    T(" would\nwork!\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type alias` for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" "),
                    S("alias", "CYAN", false, false),
                    T(" Person =\n      { name : String\n      , age : Int\n      , height : Float\n      }\n\nThis would let us use `Person` as a shorthand for that record type. Using this\nshorthand makes type annotations much easier to read, and makes changing code\neasier if you decide later that there is more to a person than age and height!"),
                ]),

            ReferenceMessage.AliasEquals =>
            new(
                "PROBLEM IN TYPE ALIAS",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a type alias, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a type variable or an equals sign next.\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type alias` for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" "),
                    S("alias", "CYAN", false, false),
                    T(" Person =\n      { name : String\n      , age : Int\n      , height : Float\n      }\n\nThis would let us use `Person` as a shorthand for that record type. Using this\nshorthand makes type annotations much easier to read, and makes changing code\neasier if you decide later that there is more to a person than age and height!"),
                ]),

            ReferenceMessage.AliasIndentEquals =>
            new(
                "UNFINISHED TYPE ALIAS",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a type alias, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a type variable or an equals sign next.\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type alias` for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" "),
                    S("alias", "CYAN", false, false),
                    T(" Person =\n      { name : String\n      , age : Int\n      , height : Float\n      }\n\nThis would let us use `Person` as a shorthand for that record type. Using this\nshorthand makes type annotations much easier to read, and makes changing code\neasier if you decide later that there is more to a person than age and height!"),
                ]),

            ReferenceMessage.AliasName =>
            new(
                "EXPECTING TYPE ALIAS NAME",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a type alias, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting a name like "),
                    S("Person", "yellow", false, false),
                    T(" or "),
                    S("Point", "yellow", false, false),
                    T(" next. Just make sure it is a name\nthat starts with a capital letter!\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type alias` for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" "),
                    S("alias", "CYAN", false, false),
                    T(" Person =\n      { name : String\n      , age : Int\n      , height : Float\n      }\n\nThis would let us use `Person` as a shorthand for that record type. Using this\nshorthand makes type annotations much easier to read, and makes changing code\neasier if you decide later that there is more to a person than age and height!"),
                ]),

            ReferenceMessage.CaseArrow or ReferenceMessage.CaseArrowColon or ReferenceMessage.CaseOperator =>
            new(
                "MISSING ARROW",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a `case` expression, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see an arrow next.\n\n"),
                    S("Note", null, false, true),
                    T(": Sometimes I get confused by indentation, so try to make your `case` look\nsomething like this:\n\n    "),
                    S("case", "CYAN", false, false),
                    T(" maybeWidth "),
                    S("of", "CYAN", false, false),
                    T("\n      "),
                    S("Just", "BLUE", false, false),
                    T(" width ->\n        width + "),
                    S("200", "yellow", false, false),
                    T("\n\n      "),
                    S("Nothing", "BLUE", false, false),
                    T(" ->\n        "),
                    S("400", "yellow", false, false),
                    T("\n\nNotice the indentation! Patterns are aligned with each other. Same indentation.\nThe expressions after each arrow are all indented a bit more than the patterns.\nThat is important!"),
                ]),

            ReferenceMessage.CaseBranch =>
            new(
                "UNFINISHED CASE",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a `case` expression, but I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see an expression next. What should I do when I run into this\nparticular pattern?\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `case` expression for reference.\n\n    "),
                    S("case", "CYAN", false, false),
                    T(" maybeWidth "),
                    S("of", "CYAN", false, false),
                    T("\n      "),
                    S("Just", "BLUE", false, false),
                    T(" width ->\n        width + "),
                    S("200", "yellow", false, false),
                    T("\n\n      "),
                    S("Nothing", "BLUE", false, false),
                    T(" ->\n        "),
                    S("400", "yellow", false, false),
                    T("\n\nNotice the indentation. Each pattern is aligned, and each branch is indented a\nbit more than the corresponding pattern. That is important!"),
                ]),

            ReferenceMessage.CaseOf =>
            new(
                "UNFINISHED CASE",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a `case` expression, but I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see the "),
                    S("of", "yellow", false, false),
                    T(" keyword next.\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `case` expression for reference.\n\n    "),
                    S("case", "CYAN", false, false),
                    T(" maybeWidth "),
                    S("of", "CYAN", false, false),
                    T("\n      "),
                    S("Just", "BLUE", false, false),
                    T(" width ->\n        width + "),
                    S("200", "yellow", false, false),
                    T("\n\n      "),
                    S("Nothing", "BLUE", false, false),
                    T(" ->\n        "),
                    S("400", "yellow", false, false),
                    T("\n\nNotice the indentation. Each pattern is aligned, and each branch is indented a\nbit more than the corresponding pattern. That is important!"),
                ]),

            ReferenceMessage.CasePattern or ReferenceMessage.CaseReservedPattern or ReferenceMessage.CaseUnexpectedOperator or ReferenceMessage.FuncArg or ReferenceMessage.MissingArgument or ReferenceMessage.PlistExpr or ReferenceMessage.PtupleExpr =>
            new(
                "PROBLEM IN PATTERN",
                false,
                [
                    T(
                        static context =>
                        "I wanted to parse a pattern next, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI am not sure why I am getting stuck exactly. I just know that I want a pattern\nnext. Something as simple as "),
                    S("maybeHeight", "yellow", false, false),
                    T(" or "),
                    S("result", "yellow", false, false),
                    T(" would work!"),
                ]),

            ReferenceMessage.CharEndless =>
            new(
                "MISSING SINGLE QUOTE",
                false,
                [
                    T(
                        static context =>
                        "I thought I was parsing a character, but I got to the end of the line without\nseeing the closing single quote:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nAdd a closing single quote here!"),
                ]),

            ReferenceMessage.CharEscape =>
            new(
                "UNKNOWN ESCAPE",
                false,
                [
                    T(
                        static context =>
                        "Backslashes always start escaped characters, but I do not recognize this one:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nValid escape characters include:\n\n"),
                    S("    \\n\n    \\r\n    \\t\n    \\\"\n    \\'\n    \\\\\n    \\u{003D}", "yellow", false, false),
                    T("\n\nDo you want one of those instead? Maybe you need \\\\ to escape a backslash?\n\n"),
                    S("Note", null, false, true),
                    T(": The last style lets encode ANY character by its Unicode code point. That\nmeans \\u{0009} and \\t are the same. You can use that style for anything not\ncovered by the other six escapes!"),
                ]),

            ReferenceMessage.CharNotString =>
            new(
                "NEEDS DOUBLE QUOTES",
                false,
                [
                    T(static context => "The following string uses single quotes:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nPlease switch to double quotes instead:\n\n    "),
                    S("'this'", "yellow", false, false),
                    T(" => "),
                    S("\"this\"", "GREEN", false, false),
                    T("\n\n"),
                    S("Note", null, false, true),
                    T(": Elm uses double quotes for strings like \"hello\", whereas it uses single\nquotes for individual characters like 'a' and 'ø'. This distinction helps with\ncode like (String.any (\\c -> c == 'X') \"90210\") where you are inspecting\nindividual characters."),
                ]),

            ReferenceMessage.CustomBar or ReferenceMessage.CustomIndentAfterBar =>
            new(
                "UNFINISHED CUSTOM TYPE",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a custom type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI just saw a vertical bar, so I was expecting to see another variant defined\nnext.\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type` declaration for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" Status\n      = Failure\n      | Waiting\n      | Success String\n\nThis defines a new `Status` type with three variants. This could be useful if we\nare waiting for an HTTP request. Maybe we start with `Waiting` and then switch\nto `Failure` or `Success \"message from server\"` depending on how things go.\nNotice that the Success variant has some associated data, allowing us to store a\nString if the request goes well!"),
                ]),

            ReferenceMessage.CustomEquals =>
            new(
                "PROBLEM IN CUSTOM TYPE",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a custom type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a type variable or an equals sign next.\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type` declaration for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" Status\n      = Failure\n      | Waiting\n      | Success String\n\nThis defines a new `Status` type with three variants. This could be useful if we\nare waiting for an HTTP request. Maybe we start with `Waiting` and then switch\nto `Failure` or `Success \"message from server\"` depending on how things go.\nNotice that the Success variant has some associated data, allowing us to store a\nString if the request goes well!"),
                ]),

            ReferenceMessage.CustomIndentAfterEquals =>
            new(
                "UNFINISHED CUSTOM TYPE",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a custom type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI just saw an equals sign, so I was expecting to see the first variant defined\nnext.\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type` declaration for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" Status\n      = Failure\n      | Waiting\n      | Success String\n\nThis defines a new `Status` type with three variants. This could be useful if we\nare waiting for an HTTP request. Maybe we start with `Waiting` and then switch\nto `Failure` or `Success \"message from server\"` depending on how things go.\nNotice that the Success variant has some associated data, allowing us to store a\nString if the request goes well!"),
                ]),

            ReferenceMessage.CustomIndentEquals =>
            new(
                "UNFINISHED CUSTOM TYPE",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a custom type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a type variable or an equals sign next.\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type` declaration for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" Status\n      = Failure\n      | Waiting\n      | Success String\n\nThis defines a new `Status` type with three variants. This could be useful if we\nare waiting for an HTTP request. Maybe we start with `Waiting` and then switch\nto `Failure` or `Success \"message from server\"` depending on how things go.\nNotice that the Success variant has some associated data, allowing us to store a\nString if the request goes well!"),
                ]),

            ReferenceMessage.CustomName =>
            new(
                "EXPECTING TYPE NAME",
                true,
                [
                    T(
                        static context =>
                        "I think I am parsing a type declaration, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting a name like "),
                    S("Status", "yellow", false, false),
                    T(" or "),
                    S("Style", "yellow", false, false),
                    T(" next. Just make sure it is a name\nthat starts with a capital letter!\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type` declaration for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" Status\n      = Failure\n      | Waiting\n      | Success String\n\nThis defines a new `Status` type with three variants. This could be useful if we\nare waiting for an HTTP request. Maybe we start with `Waiting` and then switch\nto `Failure` or `Success \"message from server\"` depending on how things go.\nNotice that the Success variant has some associated data, allowing us to store a\nString if the request goes well!"),
                ]),

            ReferenceMessage.CustomVariant =>
            new(
                "PROBLEM IN CUSTOM TYPE",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a custom type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a variant name next. Something like "),
                    S("Success", "yellow", false, false),
                    T(" or "),
                    S("Sandwich", "yellow", false, false),
                    T(".\nAny name that starts with a capital letter really!\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example of a valid `type` declaration for reference:\n\n    "),
                    S("type", "CYAN", false, false),
                    T(" Status\n      = Failure\n      | Waiting\n      | Success String\n\nThis defines a new `Status` type with three variants. This could be useful if we\nare waiting for an HTTP request. Maybe we start with `Waiting` and then switch\nto `Failure` or `Success \"message from server\"` depending on how things go.\nNotice that the Success variant has some associated data, allowing us to store a\nString if the request goes well!"),
                ]),

            ReferenceMessage.CustomVariantArg =>
            new(
                "UNFINISHED RECORD TYPE",
                true,
                [
                    T(
                        static context =>
                        "I just saw the opening curly brace of a record type, but then I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI am expecting a record like "),
                    S("{ name : String, age : Int }", "yellow", false, false),
                    T(" here. Try defining\nsome fields of your own?\n\n"),
                    S("Note", null, false, true),
                    T(": I may be confused by indentation. For example, if you are trying to define\na record type across multiple lines, I recommend using this format:\n\n    { name : String\n    , age : Int\n    , height : Float\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.DeclExpecting =>
            new(
                "WEIRD DECLARATION",
                false,
                [
                    T(
                        static context =>
                        "I am trying to parse a declaration, but I am getting stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nWhen a line has no spaces at the beginning, I expect it to be a declaration like\none of these:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n    \n    "),
                    S("type", "CYAN", false, false),
                    T(" User = Anonymous | LoggedIn String\n\nTry to make your declaration look like one of those? Or if this is not supposed\nto be a declaration, try adding some spaces before it?"),
                ]),

            ReferenceMessage.DeclImportIndent or ReferenceMessage.DocCommentFresh or ReferenceMessage.FreshModule or ReferenceMessage.FreshType =>
            new(
                "SYNTAX PROBLEM",
                false,
                [
                    T(static context => "I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI am not sure what is going on, but I recommend starting an Elm file with the\nfollowing lines:\n\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html\n    \n    main =\n      Html.text "),
                    S("\"Hello!\"", "yellow", false, false),
                    T("\n\nYou should be able to copy those lines directly into your file. Check out the\nexamples at <https://elm-lang.org/examples> for more help getting started!\n\n"),
                    S("Note", null, false, true),
                    T(": This can also happen when something is indented too much!"),
                ]),

            ReferenceMessage.DeclReserved =>
            new(
                "RESERVED WORD",
                false,
                [
                    T(
                        static context =>
                        "I was not expecting to run into the `" + context.Found + "` keyword here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T(
                        static context =>
                        "\nIt is reserved for writing `" + context.Found +
                        "` expressions. Try using a different name?\n\n"),
                    S("Note", null, false, true),
                    T(
                        static context =>
                        ": If you are trying to write an `" + context.Found +
                        "` expression, it needs to be part of a\ndefinition. So you could write something like this instead:\n\n    greet name =\n      "),
                    S("if", "CYAN", false, false),
                    T(" name == "),
                    S("\"Abraham Lincoln\"", "yellow", false, false),
                    T(" "),
                    S("then", "CYAN", false, false),
                    T(" "),
                    S("\"Greetings Mr. President.\"", "yellow", false, false),
                    T(" "),
                    S("else", "CYAN", false, false),
                    T(" "),
                    S("\"Hey!\"", "yellow", false, false),
                    T("\n\nThis defines a `reviewPowerLevel` function that you can use elsewhere in your\nprogram."),
                ]),

            ReferenceMessage.DeclSymbol =>
            new(
                "UNEXPECTED SYMBOL",
                false,
                [
                    T(
                        static context =>
                        "I am getting stuck because this line starts with the " + context.Found + " symbol:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nWhen a line has no spaces at the beginning, I expect it to be a declaration like\none of these:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n    \n    "),
                    S("type", "CYAN", false, false),
                    T(" User = Anonymous | LoggedIn String\n\nIf this is not supposed to be a declaration, try adding some spaces before it?"),
                ]),

            ReferenceMessage.DeclUpper =>
            new(
                "UNEXPECTED CAPITAL LETTER",
                false,
                [
                    T(
                        static context =>
                        "Declarations always start with a lower-case letter, so I am getting stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nTry a name like "),
                    S(static context => context.LowerName, "GREEN", false, false),
                    T(" instead?\n\n"),
                    S("Note", null, false, true),
                    T(": Here are a couple valid declarations for reference:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n    \n    "),
                    S("type", "CYAN", false, false),
                    T(" User = Anonymous | LoggedIn String\n\nNotice that they always start with a lower-case letter. Capitalization matters!"),
                ]),

            ReferenceMessage.DefBody or ReferenceMessage.DefIndentBody =>
            new(
                "UNFINISHED DEFINITION",
                true,
                [
                    T(
                        static context =>
                        "I got stuck while parsing the `" + context.Definition + "` definition:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see an expression next. What is it equal to?\n\nHere is a valid definition (with a type annotation) for reference:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n\nThe top line (called a \"type annotation\") is optional. You can leave it off if\nyou want. As you get more comfortable with Elm and as your project grows, it\nbecomes more and more valuable to add them though! They work great as\ncompiler-verified documentation, and they often improve error messages!"),
                ]),

            ReferenceMessage.DefEquals or ReferenceMessage.DefIndentEquals or ReferenceMessage.LetDefEquals =>
            new(
                "UNFINISHED DEFINITION",
                true,
                [
                    T(
                        static context =>
                        "I got stuck while parsing the `" + context.Definition + "` definition:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see an argument or an equals sign next.\n\nHere is a valid definition (with a type annotation) for reference:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n\nThe top line (called a \"type annotation\") is optional. You can leave it off if\nyou want. As you get more comfortable with Elm and as your project grows, it\nbecomes more and more valuable to add them though! They work great as\ncompiler-verified documentation, and they often improve error messages!"),
                ]),

            ReferenceMessage.DefEqualsArrow or ReferenceMessage.DefEqualsAs or ReferenceMessage.DefNameRepeat =>
            new(
                "PROBLEM IN DEFINITION",
                true,
                [
                    T(
                        static context =>
                        "I got stuck while parsing the `" + context.Definition + "` definition:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI am not sure what is going wrong exactly, so here is a valid definition (with\nan optional type annotation) for reference:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n\nTry to use that format!"),
                ]),

            ReferenceMessage.DefNameMatch =>
            new(
                "NAME MISMATCH",
                true,
                [
                    T(
                        static context =>
                        $"I just saw a type annotation for `{context.Annotated}`, but it is followed by a definition for\n`{context.Defined}`:\n\n{context.Snippet}"),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nThese names do not match! Is there a typo?\n\n    "),
                    S(static context => context.Defined, "yellow", false, false),
                    T(" -> "),
                    S(static context => context.Annotated, "GREEN", false, false),
                    T(""),
                ]),

            ReferenceMessage.EndlessComment =>
            new(
                "ENDLESS COMMENT",
                false,
                [
                    T(static context => "I cannot find the end of this multi-line comment:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nAdd a -} somewhere after this to end the comment.\n\n"),
                    S("Hint", null, false, true),
                    T(": Multi-line comments can be nested in Elm, so {- {- -} -} is a comment that\nhappens to contain another comment. Like parentheses and curly braces, the start\nand end markers must always be balanced. Maybe that is the problem?"),
                ]),

            ReferenceMessage.ExpectingDefinition =>
            new(
                "EXPECTING DEFINITION",
                true,
                [
                    T(
                        static context =>
                        "I just saw the type annotation for `" + context.Definition +
                        "` so I was expecting to see its\ndefinition here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nType annotations always appear directly above the relevant definition, without\nanything else in between. (Not even doc comments!)\n\nHere is a valid definition (with a type annotation) for reference:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n\nThe top line (called a \"type annotation\") is optional. You can leave it off if\nyou want. As you get more comfortable with Elm and as your project grows, it\nbecomes more and more valuable to add them though! They work great as\ncompiler-verified documentation, and they often improve error messages!"),
                ]),

            ReferenceMessage.ExposingEnd or ReferenceMessage.ExposingOperatorClose =>
            new(
                "UNFINISHED EXPOSING",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing exposed values, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nMaybe there is a comma missing before this?"),
                ]),

            ReferenceMessage.ExposingOperatorEmpty =>
            new(
                "PROBLEM IN EXPOSING",
                true,
                [
                    T(
                        static context =>
                        "I just saw an open parenthesis, so I was expecting an operator next:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nIt is possible to expose operators, so I was expecting to see something like "),
                    S("(+)", "yellow", false, false),
                    T("\nor "),
                    S("(|=)", "yellow", false, false),
                    T(" or "),
                    S("(||)", "yellow", false, false),
                    T(" after I saw that open parenthesis."),
                ]),

            ReferenceMessage.ExposingOperatorReserved =>
            new(
                "RESERVED SYMBOL",
                true,
                [
                    T(static context => "I cannot expose this as an operator:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nMaybe you want "),
                    S("(==)", "yellow", false, false),
                    T(" instead?"),
                ]),

            ReferenceMessage.ExposingTrailingComma or ReferenceMessage.ExposingValueKeyword or ReferenceMessage.ExposingValueSymbol =>
            new(
                "PROBLEM IN EXPOSING",
                true,
                [
                    T(static context => "I got stuck while parsing these exposed values:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI do not have an exact recommendation, so here are some valid examples of\n`exposing` for reference:\n\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n    "),
                    S("import", "CYAN", false, false),
                    T(" Basics "),
                    S("exposing", "CYAN", false, false),
                    T(" (Int, Float, Bool(..), (+), not, sqrt)\n\nThese examples show how to expose types, variants, operators, and functions.\nEverything should be some permutation of these examples, just with different\nnames."),
                ]),

            ReferenceMessage.ExposingTypePrivacy =>
            new(
                "PROBLEM EXPOSING CUSTOM TYPE VARIANTS",
                true,
                [
                    T(
                        static context =>
                        "It looks like you are trying to expose the variants of a custom type:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nYou need to write something like "),
                    S("Status(..)", "yellow", false, false),
                    T(" or "),
                    S("Entity(..)", "yellow", false, false),
                    T(" though. It is all or\nnothing, otherwise `case` expressions could miss a variant and crash!\n\n"),
                    S("Note", null, false, true),
                    T(": It is often best to keep the variants hidden! If someone pattern matches\non the variants, it is a MAJOR change if any new variants are added. Suddenly\ntheir `case` expressions do not cover all variants! So if you do not need people\nto pattern match, keep the variants hidden and expose functions to construct\nvalues of this type. This way you can add new variants as a MINOR change!"),
                ]),

            ReferenceMessage.ExprAccess or ReferenceMessage.ExprBadDot or ReferenceMessage.ExprDot =>
            new(
                "EXPECTING RECORD ACCESSOR",
                false,
                [
                    T(static context => "I am trying to parse a record accessor here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nSomething like "),
                    S(".name", "yellow", false, false),
                    T(" or "),
                    S(".price", "yellow", false, false),
                    T(" that accesses a value from a record.\n\n"),
                    S("Note", null, false, true),
                    T(": Record field names must start with a lower case letter!"),
                ]),

            ReferenceMessage.ExprBadArrow =>
            new(
                "UNEXPECTED ARROW",
                false,
                [
                    T(
                        static context =>
                        "I was partway through parsing an expression when I got stuck on this arrow:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nArrows should only appear in `case` expressions and anonymous functions.\nMaybe it was supposed to be a > sign instead?\n\n"),
                    S("Note", null, false, true),
                    T(": The syntax for anonymous functions is (\\x -> x + 1) so the arguments all\nappear after the backslash and before the arrow. Maybe a backslash is missing\nearlier?"),
                ]),

            ReferenceMessage.ExprBadColon =>
            new(
                "UNEXPECTED SYMBOL",
                false,
                [
                    T(
                        static context =>
                        "I was not expecting to run into the \"has type\" symbol here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nMaybe you want "),
                    S("::", "GREEN", false, false),
                    T(" instead? To put something on the front of a list?\n\n"),
                    S("Note", null, false, true),
                    T(
                        static context =>
                        ": The single colon is reserved for type annotations and record types, but I\nthink I am parsing the definition of `" +
                        context.Definition +
                        "` right now.\n\n"),
                    S("Note", null, false, true),
                    T(
                        static context =>
                        ": I may be getting confused by your indentation. Is this supposed to be part\nof a type annotation AFTER the `" +
                        context.Definition +
                        "` definition? If so, the problem may be a\nbit before the \"has type\" symbol. I need all definitions to be exactly aligned\n(with exactly the same indentation) so the problem may be that this new\ndefinition is indented a bit too much."),
                ]),

            ReferenceMessage.ExprBadEquals =>
            new(
                "UNEXPECTED EQUALS",
                false,
                [
                    T(static context => "I was not expecting to see this equals sign:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nMaybe you want == instead? To check if two values are equal?\n\n"),
                    S("Note", null, false, true),
                    T(
                        static context =>
                        ": I may be getting confused by your indentation. I think I am still parsing\nthe `" +
                        context.Definition +
                        "` definition. Is this supposed to be part of a definition after that?\nIf so, the problem may be a bit before the equals sign. I need all definitions\nto be indented exactly the same amount, so the problem may be that this new\ndefinition has too many spaces in front of it."),
                ]),

            ReferenceMessage.ExprBadPipe =>
            new(
                "UNEXPECTED SYMBOL",
                false,
                [
                    T(static context => "I was not expecting this vertical bar:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nVertical bars should only appear in custom type declarations. Maybe you want ||\ninstead?"),
                ]),

            ReferenceMessage.ExprOperatorRight =>
            new(
                "MISSING EXPRESSION",
                false,
                [
                    T(
                        static context =>
                        "I was expecting to see an expression after this + operator:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nYou can just put anything for now, like "),
                    S("42", "yellow", false, false),
                    T(" or "),
                    S("\"hello\"", "yellow", false, false),
                    T(". Once there is something\nthere, I can probably give a more specific hint!\n\n"),
                    S("Note", null, false, true),
                    T(": I may be getting confused by your indentation? The easiest way to make\nsure this is not an indentation problem is to put the expression on the right of\nthe + operator on the same line."),
                ]),

            ReferenceMessage.FuncArrow =>
            new(
                "UNFINISHED ANONYMOUS FUNCTION",
                true,
                [
                    T(
                        static context =>
                        "I just saw the beginning of an anonymous function, so I was expecting to see an\narrow next:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nThe syntax for anonymous functions is "),
                    S("(\\x -> x + 1)", "yellow", false, false),
                    T(" so I am missing the arrow\nand the body of the function.\n\n"),
                    S("Note", null, false, true),
                    T(": It is possible that I am confused about indetation! I generally recommend\nswitching to named functions if the definition cannot fit inline nicely, so\neither (1) try to fit the whole anonymous function on one line or (2) break the\nwhole thing out into a named function. Things tend to be clearer that way!"),
                ]),

            ReferenceMessage.FuncBody =>
            new(
                "UNFINISHED ANONYMOUS FUNCTION",
                true,
                [
                    T(
                        static context =>
                        "I was expecting to see the body of your anonymous function next:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nThe syntax for anonymous functions is "),
                    S("(\\x -> x + 1)", "yellow", false, false),
                    T(" so I am missing all the\nstuff after the arrow!\n\n"),
                    S("Note", null, false, true),
                    T(": It is possible that I am confused about indetation! I generally recommend\nswitching to named functions if the definition cannot fit inline nicely, so\neither (1) try to fit the whole anonymous function on one line or (2) break the\nwhole thing out into a named function. Things tend to be clearer that way!"),
                ]),

            ReferenceMessage.IfCondition or ReferenceMessage.IfThenBranch or ReferenceMessage.ListExpr =>
            new(
                "MISSING EXPRESSION",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing an `if` expression, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see an expression like "),
                    S("42", "yellow", false, false),
                    T(" or "),
                    S("\"hello\"", "yellow", false, false),
                    T(". Once there is something\nthere, I can probably give a more specific hint!\n\n"),
                    S("Note", null, false, true),
                    T(": This can also happen if I run into reserved words like `let` or `as`\nunexpectedly. Or if I run into operators in unexpected spots. Point is, there\nare a couple ways I can get confused and give sort of weird advice!"),
                ]),

            ReferenceMessage.IfElse =>
            new(
                "UNFINISHED IF",
                true,
                [
                    T(static context => "I was expecting to see an `else` branch after this:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI know what to do when the condition is True, but what happens when it is False?\nAdd an "),
                    S("else", "CYAN", false, false),
                    T(" branch to handle that scenario!"),
                ]),

            ReferenceMessage.IfElseBranch =>
            new(
                "UNFINISHED IF",
                true,
                [
                    T(static context => "I got stuck after the start of this `else` branch:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see an expression next. Maybe it is not filled in yet?\n\n"),
                    S("Note", null, false, true),
                    T(": I can be confused by indentation, so if the `else` branch is already\npresent, it may not be indented enough for me to recognize it."),
                ]),

            ReferenceMessage.IfThen =>
            new(
                "UNFINISHED IF",
                true,
                [
                    T(
                        static context =>
                        "I was expecting to see more of this `if` expression, but I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see the "),
                    S("then", "CYAN", false, false),
                    T(" keyword next."),
                ]),

            ReferenceMessage.ImportAlias =>
            new(
                "EXPECTING IMPORT ALIAS",
                true,
                [
                    T(static context => "I was parsing an `import` until I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see an alias next, like in these examples:\n\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html.Attributes "),
                    S("as", "CYAN", false, false),
                    T(" Attr\n    "),
                    S("import", "CYAN", false, false),
                    T(" WebGL.Texture "),
                    S("as", "CYAN", false, false),
                    T(" Texture\n    "),
                    S("import", "CYAN", false, false),
                    T(" Json.Decode "),
                    S("as", "CYAN", false, false),
                    T(" D\n\nNotice that the alias always starts with a capital letter. That is required!\n\nRead <https://elm-lang.org/0.19.2/imports> to learn more."),
                ]),

            ReferenceMessage.ImportEnd =>
            new(
                "UNFINISHED IMPORT",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing an import, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nHere are some examples of valid `import` declarations:\n\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html "),
                    S("as", "CYAN", false, false),
                    T(" H\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html "),
                    S("as", "CYAN", false, false),
                    T(" H "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html "),
                    S("exposing", "CYAN", false, false),
                    T(" (Html, div, text)\n\nYou are probably trying to import a different module, but try to make it look\nlike one of these examples!\n\nRead <https://elm-lang.org/0.19.2/imports> to learn more."),
                ]),

            ReferenceMessage.ImportExposing =>
            new(
                "UNFINISHED IMPORT",
                true,
                [
                    T(static context => "I was parsing an `import` until I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see the list of exposed values next. For example, here are\ntwo ways to expose values from the `Html` module:\n\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html "),
                    S("exposing", "CYAN", false, false),
                    T(" (Html, div, text)\n\nI generally recommend the second style. It is more explicit, making it much\neasier to figure out where values are coming from in large projects!"),
                ]),

            ReferenceMessage.ImportName =>
            new(
                "EXPECTING IMPORT NAME",
                true,
                [
                    T(static context => "I was parsing an `import` until I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a module name next, like in these examples:\n\n    "),
                    S("import", "CYAN", false, false),
                    T(" Dict\n    "),
                    S("import", "CYAN", false, false),
                    T(" Maybe\n    "),
                    S("import", "CYAN", false, false),
                    T(" Html.Attributes "),
                    S("as", "CYAN", false, false),
                    T(" A\n    "),
                    S("import", "CYAN", false, false),
                    T(" Json.Decode "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n\nNotice that the module names all start with capital letters. That is required!\n\nRead <https://elm-lang.org/0.19.2/imports> to learn more."),
                ]),

            ReferenceMessage.LetBody =>
            new(
                "UNFINISHED LET",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a `let` expression, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting an expression next. Tell me what should happen with the value\nyou just defined!\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example with a valid `let` expression for reference:\n\n    viewPerson person =\n      "),
                    S("let", "CYAN", false, false),
                    T("\n        fullName =\n          person.firstName ++ "),
                    S("\" \"", "yellow", false, false),
                    T(" ++ person.lastName\n      "),
                    S("in", "CYAN", false, false),
                    T("\n      div [] [ text fullName ]\n\nHere we defined a `viewPerson` function that turns a person into some HTML. We\nuse a `let` expression to define the `fullName` we want to show. Notice the\nindentation! The `fullName` is indented more than the `let` keyword, and the\nactual value of `fullName` is indented a bit more than that. That is important!"),
                ]),

            ReferenceMessage.LetDefName or ReferenceMessage.LetProblem =>
            new(
                "UNFINISHED LET",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a `let` expression, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting the name of a definition next.\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example with a valid `let` expression for reference:\n\n    viewPerson person =\n      "),
                    S("let", "CYAN", false, false),
                    T("\n        fullName =\n          person.firstName ++ "),
                    S("\" \"", "yellow", false, false),
                    T(" ++ person.lastName\n      "),
                    S("in", "CYAN", false, false),
                    T("\n      div [] [ text fullName ]\n\nHere we defined a `viewPerson` function that turns a person into some HTML. We\nuse a `let` expression to define the `fullName` we want to show. Notice the\nindentation! The `fullName` is indented more than the `let` keyword, and the\nactual value of `fullName` is indented a bit more than that. That is important!"),
                ]),

            ReferenceMessage.LetIn =>
            new(
                "UNFINISHED LET",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a `let` expression, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see the "),
                    S("in", "CYAN", false, false),
                    T(" keyword next. Or maybe more of that expression?\n\n"),
                    S("Note", null, false, true),
                    T(": Here is an example with a valid `let` expression for reference:\n\n    viewPerson person =\n      "),
                    S("let", "CYAN", false, false),
                    T("\n        fullName =\n          person.firstName ++ "),
                    S("\" \"", "yellow", false, false),
                    T(" ++ person.lastName\n      "),
                    S("in", "CYAN", false, false),
                    T("\n      div [] [ text fullName ]\n\nHere we defined a `viewPerson` function that turns a person into some HTML. We\nuse a `let` expression to define the `fullName` we want to show. Notice the\nindentation! The `fullName` is indented more than the `let` keyword, and the\nactual value of `fullName` is indented a bit more than that. That is important!"),
                ]),

            ReferenceMessage.ListEnd =>
            new(
                "UNFINISHED LIST",
                true,
                [
                    T(static context => "I cannot find the end of this list:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nYou can just add a closing "),
                    S("]", "yellow", false, false),
                    T(" right here, and I will be all set!\n\n"),
                    S("Note", null, false, true),
                    T(": I may be confused by indentation. For example, if you are trying to define\na list across multiple lines, I recommend using this format:\n\n    [ "),
                    S("\"Alice\"", "yellow", false, false),
                    T("\n    , "),
                    S("\"Bob\"", "yellow", false, false),
                    T("\n    , "),
                    S("\"Chuck\"", "yellow", false, false),
                    T("\n    ]\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.ListOpen =>
            new(
                "UNFINISHED LIST",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a list, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a closing square bracket before this, so try adding a "),
                    S("]", "yellow", false, false),
                    T("\nand see if that helps?\n\n"),
                    S("Note", null, false, true),
                    T(": When I get stuck like this, it usually means that there is a missing\nparenthesis or bracket somewhere earlier. It could also be a stray keyword or\noperator."),
                ]),

            ReferenceMessage.ListTrailingComma =>
            new(
                "UNFINISHED LIST",
                true,
                [
                    T(
                        static context =>
                        "I was expecting to see another list entry after that last comma:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nTrailing commas are not allowed in lists, so the fix may be to delete the comma?\n\n"),
                    S("Note", null, false, true),
                    T(": I recommend using the following format for lists that span multiple lines:\n\n    [ "),
                    S("\"Alice\"", "yellow", false, false),
                    T("\n    , "),
                    S("\"Bob\"", "yellow", false, false),
                    T("\n    , "),
                    S("\"Chuck\"", "yellow", false, false),
                    T("\n    ]\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.MissingColon =>
            new(
                "UNFINISHED DEFINITION",
                true,
                [
                    T(
                        static context =>
                        "I got stuck while parsing the `" + context.Definition + "` type annotation:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI just saw a colon, so I am expecting to see a type next.\n\nHere is a valid definition (with a type annotation) for reference:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n\nThe top line (called a \"type annotation\") is optional. You can leave it off if\nyou want. As you get more comfortable with Elm and as your project grows, it\nbecomes more and more valuable to add them though! They work great as\ncompiler-verified documentation, and they often improve error messages!"),
                ]),

            ReferenceMessage.ModuleBadBacktick or ReferenceMessage.ModuleBadComma or ReferenceMessage.ModuleBadDollar or ReferenceMessage.ModuleBadSemicolon =>
            new(
                "SYNTAX PROBLEM",
                false,
                [
                    T(static context => "I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nWhatever I am running into is confusing me a lot! Normally I can give fairly\nspecific hints, but something is really tripping me up this time."),
                ]),

            ReferenceMessage.ModuleBadChar =>
            new(
                "MISSING EXPRESSION",
                false,
                [
                    T(
                        static context =>
                        "I was expecting to see an expression after this ? operator:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nYou can just put anything for now, like "),
                    S("42", "yellow", false, false),
                    T(" or "),
                    S("\"hello\"", "yellow", false, false),
                    T(". Once there is something\nthere, I can probably give a more specific hint!\n\n"),
                    S("Note", null, false, true),
                    T(": I may be getting confused by your indentation? The easiest way to make\nsure this is not an indentation problem is to put the expression on the right of\nthe ? operator on the same line."),
                ]),

            ReferenceMessage.ModuleEndClose =>
            new(
                "STRAY PARENTHESIS",
                true,
                [
                    T(static context => "I was not expecting to see a parenthesis here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nThis ) does not match up with an earlier open parenthesis. Try deleting it?"),
                ]),

            ReferenceMessage.ModuleEndComma or ReferenceMessage.ModuleEndSemicolon =>
            new(
                "WEIRD DECLARATION",
                true,
                [
                    T(
                        static context =>
                        "I am trying to parse a declaration, but I am getting stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nWhen a line has no spaces at the beginning, I expect it to be a declaration like\none of these:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n    \n    "),
                    S("type", "CYAN", false, false),
                    T(" User = Anonymous | LoggedIn String\n\nTry to make your declaration look like one of those? Or if this is not supposed\nto be a declaration, try adding some spaces before it?"),
                ]),

            ReferenceMessage.ModuleExposingStart or ReferenceMessage.ModuleProblem =>
            new(
                "UNFINISHED MODULE DECLARATION",
                true,
                [
                    T(
                        static context =>
                        "I am parsing an `module` declaration, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nHere are some examples of valid `module` declarations:\n\n    "),
                    S("module", "CYAN", false, false),
                    T(" Main "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n    "),
                    S("module", "CYAN", false, false),
                    T(" Dict "),
                    S("exposing", "CYAN", false, false),
                    T(" (Dict, empty, get)\n\nI generally recommend using an explicit exposing list. I can skip compiling a\nbunch of files when the public interface of a module stays the same, so exposing\nfewer values can help improve compile times!"),
                ]),

            ReferenceMessage.ModuleName =>
            new(
                "EXPECTING MODULE NAME",
                true,
                [
                    T(
                        static context =>
                        "I was parsing an `module` declaration until I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see the module name next, like in these examples:\n\n    "),
                    S("module", "CYAN", false, false),
                    T(" Dict "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n    "),
                    S("module", "CYAN", false, false),
                    T(" Maybe "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n    "),
                    S("module", "CYAN", false, false),
                    T(" Html.Attributes "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n    "),
                    S("module", "CYAN", false, false),
                    T(" Json.Decode "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n\nNotice that the module names all start with capital letters. That is required!"),
                ]),

            ReferenceMessage.ModuleNameMismatch =>
            new(
                "MODULE NAME MISMATCH",
                true,
                [
                    T(static context => "It looks like this module name is out of sync:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T(
                        static context =>
                        "\nI need it to match the file path, so I was expecting to see `" + context.ExpectedModule +
                        "` here. Make\nthe following change, and you should be all set!\n\n    "),
                    S(static context => context.DeclaredModule, "yellow", false, false),
                    T(" -> "),
                    S(static context => context.ExpectedModule, "GREEN", false, false),
                    T("\n\n"),
                    S("Note", null, false, true),
                    T(": I require that module names correspond to file paths. This makes it much\neasier to explore unfamiliar codebases! So if you want to keep the current\nmodule name, try renaming the file instead."),
                ]),

            ReferenceMessage.ModuleNameMissing =>
            new(
                "MODULE NAME MISSING",
                true,
                [
                    T("I need the module name to be declared at the top of this file, like this:\n\n    "),
                    S("module", "CYAN", false, false),
                    T(static context => " " + context.ExpectedModule + " "),
                    S("exposing", "CYAN", false, false),
                    T(" (..)\n\nTry adding that as the first line of your file!\n\n"),
                    S("Note", null, false, true),
                    T(": It is best to replace (..) with an explicit list of types and functions\nyou want to expose. When you know a value is only used within this module, you\ncan refactor without worrying about uses elsewhere. Limiting exposed values can\nalso speed up compilation because I can skip a bunch of work if I see that the\nexposed API has not changed."),
                ]),

            ReferenceMessage.MultistringEndless =>
            new(
                "ENDLESS STRING",
                false,
                [
                    T(static context => "I cannot find the end of this multi-line string:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nAdd a \"\"\" somewhere after this to end the string.\n\n"),
                    S("Note", null, false, true),
                    T(": Here is a valid multi-line string for reference:\n\n"),
                    S(
                        "    \"\"\"\n    # Multi-line Strings\n    \n    - start with triple double quotes\n    - write whatever you want\n    - no need to escape newlines or double quotes\n    - end with triple double quotes\n    \"\"\"",
                        "yellow",
                        false,
                        false),
                    T(""),
                ]),

            ReferenceMessage.NeedIndentRecord =>
            new(
                "NEED MORE INDENTATION",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a record, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI need this curly brace to be indented more. Try adding some spaces before it!\n\n"),
                    S("Note", null, false, true),
                    T(": If you are trying to define a record across multiple lines, I recommend\nusing this format:\n\n    { name = "),
                    S("\"Alice\"", "yellow", false, false),
                    T("\n    , age = "),
                    S("42", "yellow", false, false),
                    T("\n    , height = "),
                    S("1.75", "yellow", false, false),
                    T("\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.NeedIndentRecordType =>
            new(
                "NEED MORE INDENTATION",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a record type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI need this curly brace to be indented more. Try adding some spaces before it!\n\n"),
                    S("Note", null, false, true),
                    T(": If you are trying to define a record type across multiple lines, I\nrecommend using this format:\n\n    { name : String\n    , age : Int\n    , height : Float\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.NoPorts =>
            new(
                "NO PORTS",
                true,
                [
                    T(
                        static context =>
                        "This module does not declare any ports, but it says it will:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nSwitch this to "),
                    S("module", "CYAN", false, false),
                    T(" and you should be all set!"),
                ]),

            ReferenceMessage.NumberDot =>
            new(
                "WEIRD NUMBER",
                false,
                [
                    T(static context => "Numbers cannot end with a dot like this:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nSwitching to "),
                    S(static context => context.Number, "GREEN", false, false),
                    T(" or "),
                    S(static context => context.Number + ".0", "GREEN", false, false),
                    T(" will work though!"),
                ]),

            ReferenceMessage.NumberEnd =>
            new(
                "WEIRD NUMBER",
                false,
                [
                    T(
                        static context =>
                        "I thought I was reading a number, but I ran into some weird stuff here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI recognize numbers in the following formats:\n\n    42\n    3.14\n    6.022e23\n    0x002B\n\nSo is there a way to write it like one of those?"),
                ]),

            ReferenceMessage.NumberHex =>
            new(
                "WEIRD HEXIDECIMAL",
                false,
                [
                    T(
                        static context =>
                        "I thought I was reading a hexidecimal number until I got here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nValid hexidecimal digits include 0123456789abcdefABCDEF, so I can only recognize\nthings like this:\n\n    0x2B\n    0x002B\n    0x00ffb3"),
                ]),

            ReferenceMessage.NumberLeadingZero =>
            new(
                "LEADING ZEROS",
                false,
                [
                    T(static context => "I do not accept numbers with leading zeros:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nJust delete the leading zeros and it should work!\n\n"),
                    S("Note", null, false, true),
                    T(": Some languages let you to specify octal numbers by adding a leading zero.\nSo in C, writing 0111 is the same as writing 73. Some people are used to that,\nbut others probably want it to equal 111. Either path is going to surprise\npeople from certain backgrounds, so Elm tries to avoid this whole situation."),
                ]),

            ReferenceMessage.PatternAlias =>
            new(
                "UNFINISHED PATTERN",
                false,
                [
                    T(
                        static context =>
                        "I was expecting to see a variable name after the `as` keyword:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nThe `as` keyword lets you write patterns like (("),
                    S("x", "yellow", false, false),
                    T(","),
                    S("y", "yellow", false, false),
                    T(") "),
                    S("as", "CYAN", false, false),
                    T(""),
                    S(" point", "yellow", false, false),
                    T(") so you can refer\nto individual parts of the tuple with "),
                    S("x", "yellow", false, false),
                    T(" and "),
                    S("y", "yellow", false, false),
                    T(" or you refer to the whole thing\nwith "),
                    S("point", "yellow", false, false),
                    T(".\n\nSo I was expecting to see a variable name after the `as` keyword here. Sometimes\npeople just want to use `as` as a variable name though. Try using a different\nname in that case!"),
                ]),

            ReferenceMessage.PatternFloat =>
            new(
                "UNEXPECTED PATTERN",
                false,
                [
                    T(static context => "I cannot pattern match with floating point numbers:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nEquality on floats can be unreliable, so you usually want to check that they are\nnearby with some sort of "),
                    S("(abs (actual - expected) < 0.001)", "yellow", false, false),
                    T(" check."),
                ]),

            ReferenceMessage.PatternStart =>
            new(
                "PROBLEM IN DEFINITION",
                false,
                [
                    T(
                        static context =>
                        "I got stuck while parsing the `" + context.Definition + "` definition:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI am not sure what is going wrong exactly, so here is a valid definition (with\nan optional type annotation) for reference:\n\n    greet : String -> String\n    greet name =\n      "),
                    S("\"Hello \"", "yellow", false, false),
                    T(" ++ name ++ "),
                    S("\"!\"", "yellow", false, false),
                    T("\n\nTry to use that format!"),
                ]),

            ReferenceMessage.PatternWildcard =>
            new(
                "UNEXPECTED NAME",
                false,
                [
                    T(
                        static context =>
                        "Variable names cannot start with underscores like this:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nYou can either have an underscore like "),
                    S("_", "yellow", false, false),
                    T(" to ignore the value, or you can have a\nname like "),
                    S(static context => context.BareWildcard, "yellow", false, false),
                    T(" to use the matched value."),
                ]),

            ReferenceMessage.PlistEnd =>
            new(
                "UNFINISHED LIST PATTERN",
                true,
                [
                    T(
                        static context =>
                        "I was expecting a closing square bracket to end this list pattern:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nTry adding a "),
                    S("]", "yellow", false, false),
                    T(" to see if that helps?"),
                ]),

            ReferenceMessage.PlistOpen =>
            new(
                "UNFINISHED LIST PATTERN",
                true,
                [
                    T(
                        static context =>
                        "I just saw an open square bracket, but then I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nTry adding a "),
                    S("]", "yellow", false, false),
                    T(" to see if that helps?"),
                ]),

            ReferenceMessage.PortColon =>
            new(
                "PORT PROBLEM",
                true,
                [
                    T(
                        static context =>
                        "I just saw the start of a `port` declaration, but then I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a colon next. And then a type that tells me what type of\nvalues are going to flow through.\n\n"),
                    S("Note", null, false, true),
                    T(": Here are some example `port` declarations for reference:\n\n    "),
                    S("port", "CYAN", false, false),
                    T(" send : String -> Cmd msg\n    "),
                    S("port", "CYAN", false, false),
                    T(" receive : (String -> msg) -> Sub msg\n\nThe first line defines a `send` port so you can send strings out to JavaScript.\nMaybe you send them on a WebSocket or put them into IndexedDB. The second line\ndefines a `receive` port so you can receive strings from JavaScript. Maybe you\nget receive messages when new WebSocket messages come in or when the IndexedDB\nis changed for some external reason."),
                ]),

            ReferenceMessage.PortIndentColon =>
            new(
                "UNFINISHED PORT",
                true,
                [
                    T(
                        static context =>
                        "I just saw the start of a `port` declaration, but then I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a colon next. And then a type that tells me what type of\nvalues are going to flow through.\n\n"),
                    S("Note", null, false, true),
                    T(": Here are some example `port` declarations for reference:\n\n    "),
                    S("port", "CYAN", false, false),
                    T(" send : String -> Cmd msg\n    "),
                    S("port", "CYAN", false, false),
                    T(" receive : (String -> msg) -> Sub msg\n\nThe first line defines a `send` port so you can send strings out to JavaScript.\nMaybe you send them on a WebSocket or put them into IndexedDB. The second line\ndefines a `receive` port so you can receive strings from JavaScript. Maybe you\nget receive messages when new WebSocket messages come in or when the IndexedDB\nis changed for some external reason."),
                ]),

            ReferenceMessage.PortIndentName =>
            new(
                "UNFINISHED PORT",
                true,
                [
                    T(
                        static context =>
                        "I just saw the start of a `port` declaration, but then I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a name like "),
                    S("send", "yellow", false, false),
                    T(" or "),
                    S("receive", "yellow", false, false),
                    T(" next. Something that starts\nwith a lower-case letter.\n\n"),
                    S("Note", null, false, true),
                    T(": Here are some example `port` declarations for reference:\n\n    "),
                    S("port", "CYAN", false, false),
                    T(" send : String -> Cmd msg\n    "),
                    S("port", "CYAN", false, false),
                    T(" receive : (String -> msg) -> Sub msg\n\nThe first line defines a `send` port so you can send strings out to JavaScript.\nMaybe you send them on a WebSocket or put them into IndexedDB. The second line\ndefines a `receive` port so you can receive strings from JavaScript. Maybe you\nget receive messages when new WebSocket messages come in or when the IndexedDB\nis changed for some external reason."),
                ]),

            ReferenceMessage.PortIndentType or ReferenceMessage.PortType =>
            new(
                "UNFINISHED PORT",
                true,
                [
                    T(
                        static context =>
                        "I just saw the start of a `port` declaration, but then I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a type next. Here are examples of outgoing and incoming\nports for reference:\n\n    "),
                    S("port", "CYAN", false, false),
                    T(" send : String -> Cmd msg\n    "),
                    S("port", "CYAN", false, false),
                    T(" receive : (String -> msg) -> Sub msg\n\nThe first line defines a `send` port so you can send strings out to JavaScript.\nMaybe you send them on a WebSocket or put them into IndexedDB. The second line\ndefines a `receive` port so you can receive strings from JavaScript. Maybe you\nget receive messages when new WebSocket messages come in or when an entry in\nIndexedDB changes for some external reason."),
                ]),

            ReferenceMessage.PortModuleExposing or ReferenceMessage.PortModuleProblem =>
            new(
                "UNFINISHED PORT MODULE DECLARATION",
                true,
                [
                    T(
                        static context =>
                        "I am parsing an `port module` declaration, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nHere are some examples of valid `port module` declarations:\n\n    "),
                    S("port", "CYAN", false, false),
                    T(" "),
                    S("module", "CYAN", false, false),
                    T(" WebSockets "),
                    S("exposing", "CYAN", false, false),
                    T(" (send, listen, keepAlive)\n    "),
                    S("port", "CYAN", false, false),
                    T(" "),
                    S("module", "CYAN", false, false),
                    T(" Maps "),
                    S("exposing", "CYAN", false, false),
                    T(" (Location, goto)\n\n"),
                    S("Note", null, false, true),
                    T(": Read <https://elm-lang.org/0.19.2/ports> for more help."),
                ]),

            ReferenceMessage.PortModuleName =>
            new(
                "EXPECTING MODULE NAME",
                true,
                [
                    T(
                        static context =>
                        "I was parsing an `module` declaration until I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see the module name next, like in these examples:\n\n    "),
                    S("port", "CYAN", false, false),
                    T(" "),
                    S("module", "CYAN", false, false),
                    T(" WebSockets "),
                    S("exposing", "CYAN", false, false),
                    T(" (send, listen, keepAlive)\n    "),
                    S("port", "CYAN", false, false),
                    T(" "),
                    S("module", "CYAN", false, false),
                    T(" Maps "),
                    S("exposing", "CYAN", false, false),
                    T(" (Location, goto)\n\nNotice that the module names start with capital letters. That is required!"),
                ]),

            ReferenceMessage.PortName =>
            new(
                "PORT PROBLEM",
                true,
                [
                    T(
                        static context =>
                        "I just saw the start of a `port` declaration, but then I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a name like "),
                    S("send", "yellow", false, false),
                    T(" or "),
                    S("receive", "yellow", false, false),
                    T(" next. Something that starts\nwith a lower-case letter.\n\n"),
                    S("Note", null, false, true),
                    T(": Here are some example `port` declarations for reference:\n\n    "),
                    S("port", "CYAN", false, false),
                    T(" send : String -> Cmd msg\n    "),
                    S("port", "CYAN", false, false),
                    T(" receive : (String -> msg) -> Sub msg\n\nThe first line defines a `send` port so you can send strings out to JavaScript.\nMaybe you send them on a WebSocket or put them into IndexedDB. The second line\ndefines a `receive` port so you can receive strings from JavaScript. Maybe you\nget receive messages when new WebSocket messages come in or when the IndexedDB\nis changed for some external reason."),
                ]),

            ReferenceMessage.PrecordEnd =>
            new(
                "UNFINISHED RECORD PATTERN",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a record pattern, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a closing curly brace next. Try adding a "),
                    S("}", "yellow", false, false),
                    T(" here?\n\n"),
                    S("Hint", null, false, true),
                    T(": A record pattern looks like "),
                    S("{x,y}", "yellow", false, false),
                    T(" or "),
                    S("{name,age}", "yellow", false, false),
                    T(" where you list the field\nnames you want to access."),
                ]),

            ReferenceMessage.PrecordField or ReferenceMessage.PrecordOpen =>
            new(
                "UNFINISHED RECORD PATTERN",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a record pattern, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a field name next.\n\n"),
                    S("Hint", null, false, true),
                    T(": A record pattern looks like "),
                    S("{x,y}", "yellow", false, false),
                    T(" or "),
                    S("{name,age}", "yellow", false, false),
                    T(" where you list the field\nnames you want to access."),
                ]),

            ReferenceMessage.PtupleEnd or ReferenceMessage.PtupleFinishedMissing =>
            new(
                "UNFINISHED PARENTHESES",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a pattern, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting a closing parenthesis next, so try adding a "),
                    S(")", "yellow", false, false),
                    T(" to see if that\nhelps?"),
                ]),

            ReferenceMessage.PtupleOpen =>
            new(
                "UNFINISHED PARENTHESES",
                true,
                [
                    T(static context => "I just saw an open parenthesis, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a pattern next. Maybe it will end up being something like\n"),
                    S("(x,y)", "yellow", false, false),
                    T(" or "),
                    S("(name, _)", "yellow", false, false),
                    T("?"),
                ]),

            ReferenceMessage.RecordEnd =>
            new(
                "UNFINISHED RECORD",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a record, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a closing curly brace next. Try putting a "),
                    S("}", "GREEN", false, false),
                    T(" next and see\nif that helps?\n\n"),
                    S("Note", null, false, true),
                    T(": I may be confused by indentation. For example, if you are trying to define\na record across multiple lines, I recommend using this format:\n\n    { name = "),
                    S("\"Alice\"", "yellow", false, false),
                    T("\n    , age = "),
                    S("42", "yellow", false, false),
                    T("\n    , height = "),
                    S("1.75", "yellow", false, false),
                    T("\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem!"),
                ]),

            ReferenceMessage.RecordEquals =>
            new(
                "PROBLEM IN RECORD",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a record, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI just saw a field name, so I was expecting to see an equals sign next. So try\nputting an "),
                    S("=", "GREEN", false, false),
                    T(" sign here?\n\n"),
                    S("Note", null, false, true),
                    T(": If you are trying to define a record across multiple lines, I recommend\nusing this format:\n\n    { name = "),
                    S("\"Alice\"", "yellow", false, false),
                    T("\n    , age = "),
                    S("42", "yellow", false, false),
                    T("\n    , height = "),
                    S("1.75", "yellow", false, false),
                    T("\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.RecordExpr =>
            new(
                "MISSING EXPRESSION",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a record, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see an expression like "),
                    S("42", "yellow", false, false),
                    T(" or "),
                    S("\"hello\"", "yellow", false, false),
                    T(". Once there is something\nthere, I can probably give a more specific hint!\n\n"),
                    S("Note", null, false, true),
                    T(": This can also happen if I run into reserved words like `let` or `as`\nunexpectedly. Or if I run into operators in unexpected spots. Point is, there\nare a couple ways I can get confused and give sort of weird advice!"),
                ]),

            ReferenceMessage.RecordExtraComma =>
            new(
                "EXTRA COMMA",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a record, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI am seeing two commas in a row. This is the second one!\n\nJust delete one of the commas and you should be all set!\n\n"),
                    S("Note", null, false, true),
                    T(": If you are trying to define a record across multiple lines, I recommend\nusing this format:\n\n    { name = "),
                    S("\"Alice\"", "yellow", false, false),
                    T("\n    , age = "),
                    S("42", "yellow", false, false),
                    T("\n    , height = "),
                    S("1.75", "yellow", false, false),
                    T("\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.RecordFieldKeyword or ReferenceMessage.RecordTrailingComma =>
            new(
                "PROBLEM IN RECORD",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a record, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see another record field defined next, so I am looking for a\nname like "),
                    S("userName", "yellow", false, false),
                    T(" or "),
                    S("plantHeight", "yellow", false, false),
                    T(".\n\n"),
                    S("Note", null, false, true),
                    T(": Field names must start with a lower-case letter. After that, you can use\nany sequence of letters, numbers, and underscores.\n\n"),
                    S("Note", null, false, true),
                    T(": If you are trying to define a record across multiple lines, I recommend\nusing this format:\n\n    { name = "),
                    S("\"Alice\"", "yellow", false, false),
                    T("\n    , age = "),
                    S("42", "yellow", false, false),
                    T("\n    , height = "),
                    S("1.75", "yellow", false, false),
                    T("\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.RecordOpen =>
            new(
                "PROBLEM IN RECORD",
                true,
                [
                    T(static context => "I just started parsing a record, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a record field defined next, so I am looking for a name\nlike "),
                    S("userName", "yellow", false, false),
                    T(" or "),
                    S("plantHeight", "yellow", false, false),
                    T(".\n\n"),
                    S("Note", null, false, true),
                    T(": Field names must start with a lower-case letter. After that, you can use\nany sequence of letters, numbers, and underscores.\n\n"),
                    S("Note", null, false, true),
                    T(": If you are trying to define a record across multiple lines, I recommend\nusing this format:\n\n    { name = "),
                    S("\"Alice\"", "yellow", false, false),
                    T("\n    , age = "),
                    S("42", "yellow", false, false),
                    T("\n    , height = "),
                    S("1.75", "yellow", false, false),
                    T("\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.ShaderEndless =>
            new(
                "ENDLESS SHADER",
                false,
                [
                    T(static context => "I cannot find the end of this shader:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nAdd a |] somewhere after this to end the shader."),
                ]),

            ReferenceMessage.ShaderProblem =>
            new(
                "SHADER PROBLEM",
                false,
                [
                    T(static context => "I ran into a problem while parsing this GLSL block.\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T(
                        static context =>
                        "\nI use a 3rd party GLSL parser for now, and I did my best to extract their error\nmessage:\n\n" +
                        context.ShaderDetail),
                ]),

            ReferenceMessage.StringEndless =>
            new(
                "ENDLESS STRING",
                false,
                [
                    T(
                        static context =>
                        "I got to the end of the line without seeing the closing double quote:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nStrings look like "),
                    S("\"this\"", "GREEN", false, false),
                    T(" with double quotes on each end. Is the closing double\nquote missing in your code?\n\n"),
                    S("Note", null, false, true),
                    T(": For a string that spans multiple lines, you can use the multi-line string\nsyntax like this:\n\n"),
                    S(
                        "    \"\"\"\n    # Multi-line Strings\n    \n    - start with triple double quotes\n    - write whatever you want\n    - no need to escape newlines or double quotes\n    - end with triple double quotes\n    \"\"\"",
                        "yellow",
                        false,
                        false),
                    T(""),
                ]),

            ReferenceMessage.StringUnicodeCode or ReferenceMessage.StringUnicodeLong =>
            new(
                "BAD UNICODE ESCAPE",
                false,
                [
                    T(static context => "This is not a valid code point:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nThe valid code points are between 0 and 10FFFF inclusive."),
                ]),

            ReferenceMessage.StringUnicodeFormat =>
            new(
                "BAD UNICODE ESCAPE",
                false,
                [
                    T(static context => "I ran into an invalid Unicode escape:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nHere are some examples of valid Unicode escapes:\n\n"),
                    S("    \\u{0041}\n    \\u{03BB}\n    \\u{6728}\n    \\u{1F60A}", "yellow", false, false),
                    T("\n\nNotice that the code point is always surrounded by curly braces. Maybe you are\nmissing the opening or closing curly brace?"),
                ]),

            ReferenceMessage.StringUnicodeShort =>
            new(
                "BAD UNICODE ESCAPE",
                false,
                [
                    T(static context => "Every code point needs at least four digits:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nTry "),
                    S(static context => context.PaddedEscape, "GREEN", false, false),
                    T(" instead?"),
                ]),

            ReferenceMessage.Tab =>
            new(
                "NO TABS",
                false,
                [
                    T(
                        static context =>
                        "I ran into a tab, but tabs are not allowed in Elm files.\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nReplace the tab with spaces."),
                ]),

            ReferenceMessage.TrecordColon =>
            new(
                "UNFINISHED RECORD TYPE",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a record type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI just saw a field name, so I was expecting to see a colon next. So try putting\nan "),
                    S(":", "GREEN", false, false),
                    T(" sign here?\n\n"),
                    S("Note", null, false, true),
                    T(": If you are trying to define a record type across multiple lines, I\nrecommend using this format:\n\n    { name : String\n    , age : Int\n    , height : Float\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.TrecordEnd =>
            new(
                "UNFINISHED RECORD TYPE",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a record type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a closing curly brace next. Try putting a "),
                    S("}", "GREEN", false, false),
                    T(" next and see\nif that helps?\n\n"),
                    S("Note", null, false, true),
                    T(": I may be confused by indentation. For example, if you are trying to define\na record type across multiple lines, I recommend using this format:\n\n    { name : String\n    , age : Int\n    , height : Float\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.TrecordExtraComma =>
            new(
                "EXTRA COMMA",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a record type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI am seeing two commas in a row. This is the second one!\n\nJust delete one of the commas and you should be all set!\n\n"),
                    S("Note", null, false, true),
                    T(": If you are trying to define a record type across multiple lines, I\nrecommend using this format:\n\n    { name : String\n    , age : Int\n    , height : Float\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.TrecordField or ReferenceMessage.TrecordTrailingComma =>
            new(
                "PROBLEM IN RECORD TYPE",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a record type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see another record field defined next, so I am looking for a\nname like "),
                    S("userName", "yellow", false, false),
                    T(" or "),
                    S("plantHeight", "yellow", false, false),
                    T(".\n\n"),
                    S("Note", null, false, true),
                    T(": If you are trying to define a record type across multiple lines, I\nrecommend using this format:\n\n    { name : String\n    , age : Int\n    , height : Float\n    }\n\nNotice that each line starts with some indentation. Usually two or four spaces.\nThis is the stylistic convention in the Elm ecosystem."),
                ]),

            ReferenceMessage.TrecordOpen =>
            new(
                "UNFINISHED RECORD TYPE",
                true,
                [
                    T(
                        static context =>
                        "I just started parsing a record type, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nRecord types look like "),
                    S("{ name : String, age : Int },", "yellow", false, false),
                    T(" so I was expecting to see a\nfield name next."),
                ]),

            ReferenceMessage.TrecordType or ReferenceMessage.TtupleOpen or ReferenceMessage.TtupleType =>
            new(
                "PROBLEM IN TYPE ALIAS",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a type alias, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a type next. Try putting "),
                    S("Int", "yellow", false, false),
                    T(" or "),
                    S("String", "yellow", false, false),
                    T(" for now?"),
                ]),

            ReferenceMessage.TtupleEnd or ReferenceMessage.TtupleFinishedMissing or ReferenceMessage.TupleEnd or ReferenceMessage.TupleFinishedMissing =>
            new(
                "UNFINISHED PARENTHESES",
                true,
                [
                    T(static context => "I was expecting to see a closing parenthesis next:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nTry adding a "),
                    S(")", "yellow", false, false),
                    T(" to see if that helps!\n\n"),
                    S("Note", null, false, true),
                    T(": I can get confused by indentation in cases like this, so maybe you have a\nclosing parenthesis but it is not indented enough?"),
                ]),

            ReferenceMessage.TupleExpr =>
            new(
                "MISSING EXPRESSION",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing some parentheses, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see an expression like "),
                    S("42", "yellow", false, false),
                    T(" or "),
                    S("\"hello\"", "yellow", false, false),
                    T(". Once there is something\nthere, I can probably give a more specific hint!\n\n"),
                    S("Note", null, false, true),
                    T(": This can also happen if I run into reserved words like `let` or `as`\nunexpectedly. Or if I run into operators in unexpected spots. Point is, there\nare a couple ways I can get confused and give sort of weird advice!"),
                ]),

            ReferenceMessage.TupleOpReserved =>
            new(
                "UNEXPECTED SYMBOL",
                true,
                [
                    T(static context => "I ran into an unexpected symbol here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nTry "),
                    S("(==)", "yellow", false, false),
                    T(" instead? To make a function that checks equality?"),
                ]),

            ReferenceMessage.TupleOperatorClose =>
            new(
                "UNFINISHED OPERATOR FUNCTION",
                true,
                [
                    T(static context => "I was expecting a closing parenthesis here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nTry adding a "),
                    S(")", "yellow", false, false),
                    T(" to see if that helps!\n\n"),
                    S("Note", null, false, true),
                    T(": I think I am parsing an operator function right now, so I am expecting to\nsee something like (+) or (&&) where an operator is surrounded by parentheses\nwith no extra spaces."),
                ]),

            ReferenceMessage.TypeStart =>
            new(
                "PROBLEM IN TYPE ANNOTATION",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing the `" + context.Definition +
                        "` type annotation, but I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a type next. Try putting "),
                    S("Int", "yellow", false, false),
                    T(" or "),
                    S("String", "yellow", false, false),
                    T(" for now?"),
                ]),

            ReferenceMessage.UnexpectedPort =>
            new(
                "UNEXPECTED PORTS",
                true,
                [
                    T(static context => "You are declaring ports in a normal module.\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nSwitch this to say "),
                    S("port module", "CYAN", false, false),
                    T(" instead, marking that this module contains port\ndeclarations.\n\n"),
                    S("Note", null, false, true),
                    T(": Ports are not a traditional FFI for calling JS functions directly. They\nneed a different mindset! Read <https://elm-lang.org/0.19.2/ports> to learn the\nsyntax and how to use it effectively."),
                ]),

            ReferenceMessage.WeirdElse =>
            new(
                "UNFINISHED IF",
                true,
                [
                    T(
                        static context =>
                        "I was expecting to see more of this `if` expression, but I got stuck here:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see the "),
                    S("then", "CYAN", false, false),
                    T(" keyword next.\n\n"),
                    S("Note", null, false, true),
                    T(": I can be confused by indentation. Maybe something is not indented enough?"),
                ]),

            ReferenceMessage.FuncMissingArgument =>
            new(
                "MISSING ARGUMENT",
                true,
                [
                    T(
                        static context =>
                        "I just saw the beginning of an anonymous function, so I was expecting to see an\nargument next:\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nSomething like "),
                    S("x", "yellow", false, false),
                    T(" or "),
                    S("name", "yellow", false, false),
                    T(". Anything that starts with a lower case letter!\n\n"),
                    S("Note", null, false, true),
                    T(
                        ": The syntax for anonymous functions is (\\x -> x + 1) where the backslash is\nmeant to look a bit like a lambda if you squint. This visual pun seemed like a\nbetter idea at the time!"),
                ]),

            ReferenceMessage.LetProblemAlignment =>
            new(
                "LET PROBLEM",
                true,
                [
                    T(
                        static context =>
                        "I was partway through parsing a `let` expression, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nBased on the indentation, I was expecting to see the "),
                    S("in", "CYAN", false, false),
                    T(" keyword next. Is there a\ntypo?\n\n"),
                    S("Note", null, false, true),
                    T(
                        ": This can also happen if you are trying to define another value within the\n`let` but it is not indented enough. Make sure each definition has exactly the\nsame amount of spaces before it. They should line up exactly!"),
                ]),

            ReferenceMessage.StrayCurlyBrace =>
            new(
                "STRAY CURLY BRACE",
                false,
                [
                    T(static context => "I was not expecting to see a curly brace here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nThis } does not match up with an earlier open curly brace. Try deleting it?"),
                ]),

            ReferenceMessage.StraySquareBracket =>
            new(
                "STRAY SQUARE BRACKET",
                false,
                [
                    T(static context => "I was not expecting to see a square bracket here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nThis ] does not match up with an earlier open square bracket. Try deleting it?"),
                ]),

            ReferenceMessage.UnfinishedTuple =>
            new(
                "UNFINISHED TUPLE",
                true,
                [
                    T(
                        static context =>
                        "I think I am in the middle of parsing a tuple. I just saw a comma, so I was\nexpecting to see an expression next.\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nA tuple looks like "),
                    S("(3,4)", "yellow", false, false),
                    T(" or "),
                    S("(\"Tom\",42)", "yellow", false, false),
                    T(", so I think there is an expression\nmissing here?\n\n"),
                    S("Note", null, false, true),
                    T(
                        ": I can get confused by indentation in cases like this, so maybe you have an\nexpression but it is not indented enough?"),
                ]),

            ReferenceMessage.UnfinishedTuplePattern =>
            new(
                "UNFINISHED TUPLE PATTERN",
                true,
                [
                    T(
                        static context =>
                        "I am partway through parsing a tuple pattern, but I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI was expecting to see a pattern next. I am expecting the final result to be\nsomething like "),
                    S("(x,y)", "yellow", false, false),
                    T(" or "),
                    S("(name, _)", "yellow", false, false),
                    T(".\n\n"),
                    S("Note", null, false, true),
                    T(
                        ": I can get confused by indentation in cases like this, so the problem may\nbe that the next part is not indented enough?"),
                ]),

            ReferenceMessage.UnfinishedTupleType =>
            new(
                "UNFINISHED TUPLE TYPE",
                true,
                [
                    T(
                        static context =>
                        "I think I am in the middle of parsing a tuple type. I just saw a comma, so I was\nexpecting to see a type next.\n\n" +
                        context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nA tuple type looks like "),
                    S("(Float,Float)", "yellow", false, false),
                    T(" or "),
                    S("(String,Int)", "yellow", false, false),
                    T(", so I think there is a\ntype missing here?\n\n"),
                    S("Note", null, false, true),
                    T(
                        ": I can get confused by indentation in cases like this, so maybe you have an\nexpression but it is not indented enough?"),
                ]),

            ReferenceMessage.WeirdElseBranch =>
            new(
                "WEIRD ELSE BRANCH",
                true,
                [
                    T(
                        static context =>
                        "I was partway through an `if` expression when I got stuck here:\n\n" + context.Snippet),
                    S(static context => context.Carets, "RED", false, false),
                    T("\nI think this "),
                    S("else", "CYAN", false, false),
                    T(" keyword needs to be indented more. Try adding some spaces\nbefore it."),
                ]),

            _ =>
            throw new NotImplementedException("CreateReferencePresentation does not handle message: " + message)
        };

    private static Presentation GrammarPresentation(SyntaxErrorBranch branch) =>
        branch switch
        {
            SyntaxErrorBranch.AliasBody => ReferencePresentation(ReferenceMessage.AliasBody),
            SyntaxErrorBranch.AliasEquals => ReferencePresentation(ReferenceMessage.AliasEquals),
            SyntaxErrorBranch.AliasIndentBody => ReferencePresentation(ReferenceMessage.AliasIndentBody),
            SyntaxErrorBranch.AliasIndentEquals => ReferencePresentation(ReferenceMessage.AliasIndentEquals),
            SyntaxErrorBranch.AliasName => ReferencePresentation(ReferenceMessage.AliasName),
            SyntaxErrorBranch.CaseArrow => ReferencePresentation(ReferenceMessage.CaseArrow),
            SyntaxErrorBranch.CaseArrowColon => ReferencePresentation(ReferenceMessage.CaseArrowColon),
            SyntaxErrorBranch.CaseBranch => ReferencePresentation(ReferenceMessage.CaseBranch),
            SyntaxErrorBranch.CaseOf => ReferencePresentation(ReferenceMessage.CaseOf),
            SyntaxErrorBranch.CaseOperator => ReferencePresentation(ReferenceMessage.CaseOperator),
            SyntaxErrorBranch.CasePattern => ReferencePresentation(ReferenceMessage.CasePattern),
            SyntaxErrorBranch.CaseReservedPattern => ReferencePresentation(ReferenceMessage.CaseReservedPattern),
            SyntaxErrorBranch.CaseUnexpectedOperator => ReferencePresentation(ReferenceMessage.CaseUnexpectedOperator),
            SyntaxErrorBranch.CharEndless => ReferencePresentation(ReferenceMessage.CharEndless),
            SyntaxErrorBranch.CharNotString => ReferencePresentation(ReferenceMessage.CharNotString),
            SyntaxErrorBranch.CustomBar => ReferencePresentation(ReferenceMessage.CustomBar),
            SyntaxErrorBranch.CustomEquals => ReferencePresentation(ReferenceMessage.CustomEquals),
            SyntaxErrorBranch.CustomIndentAfterBar => ReferencePresentation(ReferenceMessage.CustomIndentAfterBar),

            SyntaxErrorBranch.CustomIndentAfterEquals =>
            ReferencePresentation(ReferenceMessage.CustomIndentAfterEquals),

            SyntaxErrorBranch.CustomIndentEquals => ReferencePresentation(ReferenceMessage.CustomIndentEquals),
            SyntaxErrorBranch.CustomName => ReferencePresentation(ReferenceMessage.CustomName),
            SyntaxErrorBranch.CustomVariant => ReferencePresentation(ReferenceMessage.CustomVariant),
            SyntaxErrorBranch.CustomVariantArg => ReferencePresentation(ReferenceMessage.CustomVariantArg),
            SyntaxErrorBranch.DeclExpecting => ReferencePresentation(ReferenceMessage.DeclExpecting),
            SyntaxErrorBranch.DeclImportIndent => ReferencePresentation(ReferenceMessage.DeclImportIndent),
            SyntaxErrorBranch.DeclReserved => ReferencePresentation(ReferenceMessage.DeclReserved),
            SyntaxErrorBranch.DeclSymbol => ReferencePresentation(ReferenceMessage.DeclSymbol),
            SyntaxErrorBranch.DeclUpper => ReferencePresentation(ReferenceMessage.DeclUpper),
            SyntaxErrorBranch.DefBody => ReferencePresentation(ReferenceMessage.DefBody),
            SyntaxErrorBranch.DefEquals => ReferencePresentation(ReferenceMessage.DefEquals),
            SyntaxErrorBranch.DefEqualsArrow => ReferencePresentation(ReferenceMessage.DefEqualsArrow),
            SyntaxErrorBranch.DefEqualsAs => ReferencePresentation(ReferenceMessage.DefEqualsAs),
            SyntaxErrorBranch.DefIndentBody => ReferencePresentation(ReferenceMessage.DefIndentBody),
            SyntaxErrorBranch.DefIndentEquals => ReferencePresentation(ReferenceMessage.DefIndentEquals),
            SyntaxErrorBranch.DocCommentFresh => ReferencePresentation(ReferenceMessage.DocCommentFresh),
            SyntaxErrorBranch.EndlessComment => ReferencePresentation(ReferenceMessage.EndlessComment),
            SyntaxErrorBranch.ExpectingDefinition => ReferencePresentation(ReferenceMessage.ExpectingDefinition),
            SyntaxErrorBranch.ExposingEnd => ReferencePresentation(ReferenceMessage.ExposingEnd),
            SyntaxErrorBranch.ExposingOperatorClose => ReferencePresentation(ReferenceMessage.ExposingOperatorClose),
            SyntaxErrorBranch.ExposingOperatorEmpty => ReferencePresentation(ReferenceMessage.ExposingOperatorEmpty),

            SyntaxErrorBranch.ExposingOperatorReserved =>
            ReferencePresentation(ReferenceMessage.ExposingOperatorReserved),

            SyntaxErrorBranch.ExposingTrailingComma => ReferencePresentation(ReferenceMessage.ExposingTrailingComma),
            SyntaxErrorBranch.ExposingTypePrivacy => ReferencePresentation(ReferenceMessage.ExposingTypePrivacy),
            SyntaxErrorBranch.ExposingValueKeyword => ReferencePresentation(ReferenceMessage.ExposingValueKeyword),
            SyntaxErrorBranch.ExposingValueSymbol => ReferencePresentation(ReferenceMessage.ExposingValueSymbol),
            SyntaxErrorBranch.ExprAccess => ReferencePresentation(ReferenceMessage.ExprAccess),
            SyntaxErrorBranch.ExprBadArrow => ReferencePresentation(ReferenceMessage.ExprBadArrow),
            SyntaxErrorBranch.ExprBadColon => ReferencePresentation(ReferenceMessage.ExprBadColon),
            SyntaxErrorBranch.ExprBadDot => ReferencePresentation(ReferenceMessage.ExprBadDot),
            SyntaxErrorBranch.ExprBadEquals => ReferencePresentation(ReferenceMessage.ExprBadEquals),
            SyntaxErrorBranch.ExprBadPipe => ReferencePresentation(ReferenceMessage.ExprBadPipe),
            SyntaxErrorBranch.ExprDot => ReferencePresentation(ReferenceMessage.ExprDot),
            SyntaxErrorBranch.ExprOperatorRight => ReferencePresentation(ReferenceMessage.ExprOperatorRight),
            SyntaxErrorBranch.FreshModule => ReferencePresentation(ReferenceMessage.FreshModule),
            SyntaxErrorBranch.FreshType => ReferencePresentation(ReferenceMessage.FreshType),
            SyntaxErrorBranch.FuncArg => ReferencePresentation(ReferenceMessage.FuncArg),
            SyntaxErrorBranch.FuncArrow => ReferencePresentation(ReferenceMessage.FuncArrow),
            SyntaxErrorBranch.FuncBody => ReferencePresentation(ReferenceMessage.FuncBody),
            SyntaxErrorBranch.FuncMissingArgument => ReferencePresentation(ReferenceMessage.FuncMissingArgument),
            SyntaxErrorBranch.IfCondition => ReferencePresentation(ReferenceMessage.IfCondition),
            SyntaxErrorBranch.IfElse => ReferencePresentation(ReferenceMessage.IfElse),
            SyntaxErrorBranch.IfElseBranch => ReferencePresentation(ReferenceMessage.IfElseBranch),
            SyntaxErrorBranch.IfThen => ReferencePresentation(ReferenceMessage.IfThen),
            SyntaxErrorBranch.IfThenBranch => ReferencePresentation(ReferenceMessage.IfThenBranch),
            SyntaxErrorBranch.ImportAlias => ReferencePresentation(ReferenceMessage.ImportAlias),
            SyntaxErrorBranch.ImportEnd => ReferencePresentation(ReferenceMessage.ImportEnd),
            SyntaxErrorBranch.ImportExposing => ReferencePresentation(ReferenceMessage.ImportExposing),
            SyntaxErrorBranch.ImportName => ReferencePresentation(ReferenceMessage.ImportName),
            SyntaxErrorBranch.ImportIncomplete => new("UNFINISHED IMPORT", true, [T("Unfinished import")]),
            SyntaxErrorBranch.LetBody => ReferencePresentation(ReferenceMessage.LetBody),
            SyntaxErrorBranch.LetDefEquals => ReferencePresentation(ReferenceMessage.LetDefEquals),
            SyntaxErrorBranch.LetDefName => ReferencePresentation(ReferenceMessage.LetDefName),
            SyntaxErrorBranch.LetIn => ReferencePresentation(ReferenceMessage.LetIn),
            SyntaxErrorBranch.LetProblem => ReferencePresentation(ReferenceMessage.LetProblem),
            SyntaxErrorBranch.LetProblemAlignment => ReferencePresentation(ReferenceMessage.LetProblemAlignment),
            SyntaxErrorBranch.ListEnd => ReferencePresentation(ReferenceMessage.ListEnd),
            SyntaxErrorBranch.ListExpr => ReferencePresentation(ReferenceMessage.ListExpr),
            SyntaxErrorBranch.ListOpen => ReferencePresentation(ReferenceMessage.ListOpen),
            SyntaxErrorBranch.ListTrailingComma => ReferencePresentation(ReferenceMessage.ListTrailingComma),
            SyntaxErrorBranch.MissingArgument => ReferencePresentation(ReferenceMessage.MissingArgument),
            SyntaxErrorBranch.MissingColon => ReferencePresentation(ReferenceMessage.MissingColon),
            SyntaxErrorBranch.ModuleBadBacktick => ReferencePresentation(ReferenceMessage.ModuleBadBacktick),
            SyntaxErrorBranch.ModuleBadChar => ReferencePresentation(ReferenceMessage.ModuleBadChar),
            SyntaxErrorBranch.ModuleBadComma => ReferencePresentation(ReferenceMessage.ModuleBadComma),
            SyntaxErrorBranch.ModuleBadDollar => ReferencePresentation(ReferenceMessage.ModuleBadDollar),
            SyntaxErrorBranch.ModuleBadSemicolon => ReferencePresentation(ReferenceMessage.ModuleBadSemicolon),
            SyntaxErrorBranch.ModuleEndClose => ReferencePresentation(ReferenceMessage.ModuleEndClose),
            SyntaxErrorBranch.ModuleEndComma => ReferencePresentation(ReferenceMessage.ModuleEndComma),
            SyntaxErrorBranch.ModuleEndSemicolon => ReferencePresentation(ReferenceMessage.ModuleEndSemicolon),
            SyntaxErrorBranch.ModuleExposingStart => ReferencePresentation(ReferenceMessage.ModuleExposingStart),
            SyntaxErrorBranch.ModuleName => ReferencePresentation(ReferenceMessage.ModuleName),
            SyntaxErrorBranch.ModuleProblem => ReferencePresentation(ReferenceMessage.ModuleProblem),
            SyntaxErrorBranch.MultistringEndless => ReferencePresentation(ReferenceMessage.MultistringEndless),
            SyntaxErrorBranch.NeedIndentRecord => ReferencePresentation(ReferenceMessage.NeedIndentRecord),
            SyntaxErrorBranch.NeedIndentRecordType => ReferencePresentation(ReferenceMessage.NeedIndentRecordType),
            SyntaxErrorBranch.PatternAlias => ReferencePresentation(ReferenceMessage.PatternAlias),
            SyntaxErrorBranch.PatternFloat => ReferencePresentation(ReferenceMessage.PatternFloat),
            SyntaxErrorBranch.PatternStart => ReferencePresentation(ReferenceMessage.PatternStart),
            SyntaxErrorBranch.PlistEnd => ReferencePresentation(ReferenceMessage.PlistEnd),
            SyntaxErrorBranch.PlistExpr => ReferencePresentation(ReferenceMessage.PlistExpr),
            SyntaxErrorBranch.PlistOpen => ReferencePresentation(ReferenceMessage.PlistOpen),
            SyntaxErrorBranch.PortColon => ReferencePresentation(ReferenceMessage.PortColon),
            SyntaxErrorBranch.PortIndentColon => ReferencePresentation(ReferenceMessage.PortIndentColon),
            SyntaxErrorBranch.PortIndentName => ReferencePresentation(ReferenceMessage.PortIndentName),
            SyntaxErrorBranch.PortIndentType => ReferencePresentation(ReferenceMessage.PortIndentType),
            SyntaxErrorBranch.PortModuleExposing => ReferencePresentation(ReferenceMessage.PortModuleExposing),
            SyntaxErrorBranch.PortModuleName => ReferencePresentation(ReferenceMessage.PortModuleName),
            SyntaxErrorBranch.PortModuleProblem => ReferencePresentation(ReferenceMessage.PortModuleProblem),
            SyntaxErrorBranch.PortName => ReferencePresentation(ReferenceMessage.PortName),
            SyntaxErrorBranch.PortType => ReferencePresentation(ReferenceMessage.PortType),
            SyntaxErrorBranch.PrecordEnd => ReferencePresentation(ReferenceMessage.PrecordEnd),
            SyntaxErrorBranch.PrecordField => ReferencePresentation(ReferenceMessage.PrecordField),
            SyntaxErrorBranch.PrecordOpen => ReferencePresentation(ReferenceMessage.PrecordOpen),
            SyntaxErrorBranch.PtupleEnd => ReferencePresentation(ReferenceMessage.PtupleEnd),
            SyntaxErrorBranch.PtupleExpr => ReferencePresentation(ReferenceMessage.PtupleExpr),
            SyntaxErrorBranch.PtupleFinishedMissing => ReferencePresentation(ReferenceMessage.PtupleFinishedMissing),
            SyntaxErrorBranch.PtupleOpen => ReferencePresentation(ReferenceMessage.PtupleOpen),
            SyntaxErrorBranch.RecordEnd => ReferencePresentation(ReferenceMessage.RecordEnd),
            SyntaxErrorBranch.RecordEquals => ReferencePresentation(ReferenceMessage.RecordEquals),
            SyntaxErrorBranch.RecordExpr => ReferencePresentation(ReferenceMessage.RecordExpr),
            SyntaxErrorBranch.RecordExtraComma => ReferencePresentation(ReferenceMessage.RecordExtraComma),
            SyntaxErrorBranch.RecordFieldKeyword => ReferencePresentation(ReferenceMessage.RecordFieldKeyword),
            SyntaxErrorBranch.RecordOpen => ReferencePresentation(ReferenceMessage.RecordOpen),
            SyntaxErrorBranch.RecordTrailingComma => ReferencePresentation(ReferenceMessage.RecordTrailingComma),
            SyntaxErrorBranch.StrayCurlyBrace => ReferencePresentation(ReferenceMessage.StrayCurlyBrace),
            SyntaxErrorBranch.StraySquareBracket => ReferencePresentation(ReferenceMessage.StraySquareBracket),
            SyntaxErrorBranch.StringEndless => ReferencePresentation(ReferenceMessage.StringEndless),
            SyntaxErrorBranch.Tab => ReferencePresentation(ReferenceMessage.Tab),
            SyntaxErrorBranch.TrecordColon => ReferencePresentation(ReferenceMessage.TrecordColon),
            SyntaxErrorBranch.TrecordEnd => ReferencePresentation(ReferenceMessage.TrecordEnd),
            SyntaxErrorBranch.TrecordExtraComma => ReferencePresentation(ReferenceMessage.TrecordExtraComma),
            SyntaxErrorBranch.TrecordField => ReferencePresentation(ReferenceMessage.TrecordField),
            SyntaxErrorBranch.TrecordOpen => ReferencePresentation(ReferenceMessage.TrecordOpen),
            SyntaxErrorBranch.TrecordTrailingComma => ReferencePresentation(ReferenceMessage.TrecordTrailingComma),
            SyntaxErrorBranch.TrecordType => ReferencePresentation(ReferenceMessage.TrecordType),
            SyntaxErrorBranch.TtupleEnd => ReferencePresentation(ReferenceMessage.TtupleEnd),
            SyntaxErrorBranch.TtupleFinishedMissing => ReferencePresentation(ReferenceMessage.TtupleFinishedMissing),
            SyntaxErrorBranch.TtupleOpen => ReferencePresentation(ReferenceMessage.TtupleOpen),
            SyntaxErrorBranch.TtupleType => ReferencePresentation(ReferenceMessage.TtupleType),
            SyntaxErrorBranch.TupleEnd => ReferencePresentation(ReferenceMessage.TupleEnd),
            SyntaxErrorBranch.TupleExpr => ReferencePresentation(ReferenceMessage.TupleExpr),
            SyntaxErrorBranch.TupleFinishedMissing => ReferencePresentation(ReferenceMessage.TupleFinishedMissing),
            SyntaxErrorBranch.TupleOpReserved => ReferencePresentation(ReferenceMessage.TupleOpReserved),
            SyntaxErrorBranch.TupleOperatorClose => ReferencePresentation(ReferenceMessage.TupleOperatorClose),
            SyntaxErrorBranch.TypeStart => ReferencePresentation(ReferenceMessage.TypeStart),
            SyntaxErrorBranch.UnfinishedTuple => ReferencePresentation(ReferenceMessage.UnfinishedTuple),

            SyntaxErrorBranch.UnfinishedTuplePattern =>
            ReferencePresentation(ReferenceMessage.UnfinishedTuplePattern),

            SyntaxErrorBranch.UnfinishedTupleType => ReferencePresentation(ReferenceMessage.UnfinishedTupleType),
            SyntaxErrorBranch.WeirdElse => ReferencePresentation(ReferenceMessage.WeirdElse),
            SyntaxErrorBranch.WeirdElseBranch => ReferencePresentation(ReferenceMessage.WeirdElseBranch),
            SyntaxErrorBranch.ExpectedToken => s_unexpectedSyntax,
            SyntaxErrorBranch.ExpectedKeyword => s_unexpectedSyntax,
            SyntaxErrorBranch.ExpectedAdjacentIdentifier => s_unexpectedSyntax,
            SyntaxErrorBranch.ExpectedLowerIdentifier => s_unexpectedSyntax,
            SyntaxErrorBranch.UnexpectedDeclarationToken => s_unexpectedSyntax,
            SyntaxErrorBranch.ExpectedEffectRecord => s_unexpectedSyntax,
            SyntaxErrorBranch.ExpectedEffectCommand => s_unexpectedSyntax,
            SyntaxErrorBranch.ExpectedEffectSubscription => s_unexpectedSyntax,
            SyntaxErrorBranch.InfixDirection => s_unexpectedSyntax,
            SyntaxErrorBranch.InfixPrecedence => s_unexpectedSyntax,
            SyntaxErrorBranch.UnsupportedTypeAlias => s_unexpectedSyntax,
            SyntaxErrorBranch.UnsupportedType => s_unexpectedSyntax,
            SyntaxErrorBranch.UnsupportedExpression => s_unexpectedSyntax,
            SyntaxErrorBranch.UnsupportedPattern => s_unexpectedSyntax,
            SyntaxErrorBranch.CharacterValue => s_unexpectedSyntax,
            SyntaxErrorBranch.HexPatternRange => s_unexpectedSyntax,
            SyntaxErrorBranch.RecordSeparator => s_unexpectedSyntax,

            _ =>
            throw new NotImplementedException("GrammarPresentation does not handle branch: " + branch)
        };
}
