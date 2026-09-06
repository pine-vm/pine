using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;

using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>A presentation-independent syntax diagnostic. Ranges are one-based and end-exclusive.</summary>
public readonly record struct ElmSyntaxParseError(
    Range Region,
    Range? ContextRange,
    ElmSyntaxErrorKind Kind)
{
    /// <summary>The primary position, retained for compatibility.</summary>
    public Location Location => Region.Start;

    /// <summary>Compatibility presentation; new clients should select an explicit renderer.</summary>
    [Obsolete("Use ElmSyntaxErrorRenderer.RenderConcise or RenderJson.")]
    public string Message => ElmSyntaxErrorRenderer.RenderConcise(this);

    internal ElmSyntaxParseError(Location location, SyntaxErrorBranch branch, FoundSyntax? found = null)
        : this(new Range(location, location), null,
            new ElmSyntaxErrorKind.Parse(new ElmSyntaxProblem.Grammar(branch, found)))
    {
    }

    internal ElmSyntaxParseError(Range region, ElmSyntaxProblem problem)
        : this(region, null, new ElmSyntaxErrorKind.Parse(problem))
    {
    }

    /// <inheritdoc/>
    public override string ToString() => RenderDisplayString(this);

    /// <summary>Compatibility location-prefixed display.</summary>
    public static string RenderDisplayString(ElmSyntaxParseError elmSyntaxParseError) =>
        $"{elmSyntaxParseError.Location.Row}:{elmSyntaxParseError.Location.Column}: {ElmSyntaxErrorRenderer.RenderConcise(elmSyntaxParseError)}";
}

/// <summary>Parsing and environment-dependent validation are separate diagnostic producers.</summary>
public abstract record ElmSyntaxErrorKind
{
    /// <summary>A text parser failure, including a recovered declaration.</summary>
    public sealed record Parse(ElmSyntaxProblem Problem) : ElmSyntaxErrorKind;

    /// <summary>A module coordinator failure; never constructed by the text parser.</summary>
    public sealed record ModuleValidation(ModuleValidationProblem Problem, string? ExpectedName = null, string? DeclaredName = null)
        : ElmSyntaxErrorKind;
}

#pragma warning disable CS1591 // Closed vocabularies are documented at the enum level.

/// <summary>Environment-dependent module checks.</summary>
public enum ModuleValidationProblem
{
    ModuleNameMissing, ModuleNameMismatch, PortInNormalModule, PortModuleWithoutPorts,
    PortInPackage, PortModuleInPackage, EffectModuleOutsideKernel
}

/// <summary>Facts whose shape depends on the failing grammar family. No member contains diagnostic prose.</summary>
public abstract record ElmSyntaxProblem
{
    /// <summary>A lossless-tree parser expectation, including the role of an identifier.</summary>
    public sealed record Expected(ExpectedSyntax Syntax, FoundSyntax Found) : ElmSyntaxProblem;

    /// <summary>A grammar stage and the classified syntax encountered there.</summary>
    public sealed record Grammar(SyntaxErrorBranch Branch, FoundSyntax? Found = null,
        SyntaxErrorSite Site = SyntaxErrorSite.Declaration, int? RequiredColumn = null, int? ActualColumn = null)
        : ElmSyntaxProblem;

    /// <summary>The earlier annotation and the different definition name.</summary>
    public sealed record AnnotationNameMismatch(LocatedIdentifier AnnotatedName, string DefinedName) : ElmSyntaxProblem;

    /// <summary>A second annotation where the definition was required.</summary>
    public sealed record AnnotationNameRepeated(string Name) : ElmSyntaxProblem;

    /// <summary>An invalid literal escape.</summary>
    public sealed record Escape(EscapeProblem Problem, int DigitCount = 0, int? CodePoint = null) : ElmSyntaxProblem;

    /// <summary>An invalid numeric literal.</summary>
    public sealed record Number(NumberProblem Problem, string IntegerPrefix) : ElmSyntaxProblem;

    /// <summary>An identifier beginning with underscores.</summary>
    public sealed record Wildcard(string Name, int UnderscoreCount) : ElmSyntaxProblem;

    /// <summary>GLSL failure; an upstream parser detail is deliberately opaque.</summary>
    public sealed record Shader(ShaderProblem Problem, ShaderSyntaxProblem? SyntaxProblem = null,
        Location? ShaderLocation = null, string? UpstreamDetail = null) : ElmSyntaxProblem;
}

/// <summary>A related identifier, not the primary diagnostic location.</summary>
public sealed record LocatedIdentifier(string Name, Range Range);

/// <summary>Classification is a parser fact, not inferred by a renderer.</summary>
public sealed record FoundSyntax(FoundSyntaxKind Kind, string Text);

/// <summary>The expected grammar symbol when a tree-parser primitive fails.</summary>
public abstract record ExpectedSyntax
{
    /// <summary>A lexical token category.</summary>
    public sealed record Token(SyntaxTokenKind Kind) : ExpectedSyntax;

    /// <summary>An Elm grammar keyword.</summary>
    public sealed record Keyword(string Spelling) : ExpectedSyntax;

    /// <summary>An identifier's syntactic role and adjacency/case constraints.</summary>
    public sealed record Identifier(IdentifierRole Role, bool Adjacent = false, bool Lowercase = false) : ExpectedSyntax;
}

/// <summary>Identifier roles retained by the lossless tree parser.</summary>
public enum IdentifierRole
{
    ModuleName, ModuleNamePart, ModuleAlias, InfixDirection, FunctionName, PortName, TypeAlias,
    GenericTypeParameter, TypeName, TypeParameter, ConstructorName, FunctionFirstIdentifier,
    RecordFieldOrTypeParameter, RecordField, FirstIdentifier, NamespaceItem, FunctionOrValueNamePart,
    PatternName, PatternIdentifier, RecordOrField
}

/// <summary>Token vocabulary shared by tree parsing and structured expectations.</summary>
public enum SyntaxTokenKind
{
    Identifier, StringLiteral, TripleQuotedStringLiteral, CharLiteral, NumberLiteral, GLSLLiteral,
    OpenParen, CloseParen, OpenBrace, CloseBrace, OpenBracket, CloseBracket, Comma, Dot, DotDot,
    Equal, Arrow, Colon, Pipe, Comment, Lambda, Operator, Negation, Unknown, EndOfFile
}

/// <summary>Classes of encountered syntax.</summary>
public enum FoundSyntaxKind
{
    EndOfFile, Keyword, LowerIdentifier, UpperIdentifier, Operator, Delimiter, Punctuation, Literal, Character
}

/// <summary>Context of a nested grammar failure.</summary>
public enum SyntaxErrorSite
{
    Declaration, DefinitionBody, FunctionArgument, CasePattern, CaseBranch, RecordField, TypeAlias, TypeAnnotation, Port, LetDefinition
}

/// <summary>Escape validation stages.</summary>
public enum EscapeProblem
{
    Unknown, UnicodeFormat, UnicodeShort, UnicodeLong, UnicodeCode
}

/// <summary>Numeric validation stages.</summary>
public enum NumberProblem
{
    Dot, End, Hex, LeadingZero
}

/// <summary>Shader validation stages.</summary>
public enum ShaderProblem
{
    Endless, Invalid
}

/// <summary>Structural GLSL failures, independent of the renderer's language.</summary>
public enum ShaderSyntaxProblem
{
    ExpectedDeclarator, UnfinishedDeclaration, UnclosedDelimiter, UnexpectedDelimiter
}

/// <summary>
/// Grammar branch identities, not diagnostic titles. The inventory tests document their correspondence
/// to the reference parser. Branches sharing a title deliberately remain distinct.
/// </summary>
public enum SyntaxErrorBranch
{
    AliasBody, AliasEquals, AliasIndentBody, AliasIndentEquals, AliasName,
    CaseArrow, CaseArrowColon, CaseBranch, CaseOf, CaseOperator, CasePattern, CaseReservedPattern, CaseUnexpectedOperator,
    CharEndless, CharNotString,
    CustomBar, CustomEquals, CustomIndentAfterBar, CustomIndentAfterEquals, CustomIndentEquals, CustomName, CustomVariant, CustomVariantArg,
    DeclExpecting, DeclImportIndent, DeclReserved, DeclSymbol, DeclUpper,
    DefBody, DefEquals, DefEqualsArrow, DefEqualsAs, DefIndentBody, DefIndentEquals,
    DocCommentFresh, EndlessComment, ExpectingDefinition,
    ExposingEnd, ExposingOperatorClose, ExposingOperatorEmpty, ExposingOperatorReserved, ExposingTrailingComma,
    ExposingTypePrivacy, ExposingValueKeyword, ExposingValueSymbol,
    ExprAccess, ExprBadArrow, ExprBadColon, ExprBadDot, ExprBadEquals, ExprBadPipe, ExprDot, ExprOperatorRight,
    FreshModule, FreshType, FuncArg, FuncArrow, FuncBody, FuncMissingArgument,
    IfCondition, IfElse, IfElseBranch, IfThen, IfThenBranch,
    ImportAlias, ImportEnd, ImportExposing, ImportName, ImportIncomplete,
    LetBody, LetDefEquals, LetDefName, LetIn, LetProblem, LetProblemAlignment,
    ListEnd, ListExpr, ListOpen, ListTrailingComma, MissingArgument, MissingColon,
    ModuleBadBacktick, ModuleBadChar, ModuleBadComma, ModuleBadDollar, ModuleBadSemicolon,
    ModuleEndClose, ModuleEndComma, ModuleEndSemicolon, ModuleExposingStart, ModuleName, ModuleProblem,
    MultistringEndless, NeedIndentRecord, NeedIndentRecordType,
    PatternAlias, PatternFloat, PatternStart,
    PlistEnd, PlistExpr, PlistOpen,
    PortColon, PortIndentColon, PortIndentName, PortIndentType, PortModuleExposing, PortModuleName, PortModuleProblem, PortName, PortType,
    PrecordEnd, PrecordField, PrecordOpen, PtupleEnd, PtupleExpr, PtupleFinishedMissing, PtupleOpen,
    RecordEnd, RecordEquals, RecordExpr, RecordExtraComma, RecordFieldKeyword, RecordOpen, RecordTrailingComma,
    StrayCurlyBrace, StraySquareBracket, StringEndless, Tab,
    TrecordColon, TrecordEnd, TrecordExtraComma, TrecordField, TrecordOpen, TrecordTrailingComma, TrecordType,
    TtupleEnd, TtupleFinishedMissing, TtupleOpen, TtupleType,
    TupleEnd, TupleExpr, TupleFinishedMissing, TupleOpReserved, TupleOperatorClose, TypeStart,
    UnfinishedTuple, UnfinishedTuplePattern, UnfinishedTupleType, WeirdElse, WeirdElseBranch,
    ExpectedToken, ExpectedKeyword, ExpectedAdjacentIdentifier, ExpectedLowerIdentifier,
    UnexpectedDeclarationToken, ExpectedEffectRecord, ExpectedEffectCommand, ExpectedEffectSubscription,
    InfixDirection, InfixPrecedence, UnsupportedTypeAlias, UnsupportedType, UnsupportedExpression,
    UnsupportedPattern, CharacterValue, HexPatternRange, RecordSeparator
}

#pragma warning restore CS1591
