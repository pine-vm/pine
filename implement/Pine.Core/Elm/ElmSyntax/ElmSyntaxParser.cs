using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

// Alias to avoid ambiguity with System.Range
using Range = Pine.Core.Elm.ElmSyntax.SyntaxModel.Range;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Parsing Elm module source text into syntax trees.
/// <para>
/// To learn about the design goals, see 'guide\elm-syntax-model-and-parser.md'
/// </para>
/// </summary>
public class ElmSyntaxParser
{
    /// <summary>
    /// Parses Elm module text and encodes the resulting syntax tree as an Elm value for downstream processing.
    /// </summary>
    /// <param name="elmModuleText">Source code of the Elm module to parse.</param>
    /// <returns>Result containing either the encoded Elm value or an error description.</returns>
    public static Result<ElmSyntaxParseError, ElmValue> ParseModuleTextAsElmSyntaxElmValue(
        string elmModuleText)
    {
        var parseResult =
            ParseModuleText(elmModuleText);

        if (parseResult.IsErrOrNullable() is { } err)
        {
            return err;
        }

        if (parseResult.IsOkOrNull() is not { } parseOk)
        {
            throw new NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().Name);
        }

        var asAst = FromFullSyntaxModel.Convert(parseOk);

        return EncodeAsElmValue.EncodeFile(asAst);
    }

    /// <summary>
    /// Parses Elm module text into the classic syntax tree representation.
    /// The parser always preserves parentheses/tuples in the syntax tree.
    /// Use <see cref="FromFullSyntaxModel.Convert(SyntaxTypes.Expression)"/> to get the
    /// abstract syntax model that matches the original stil4m/elm-syntax behavior
    /// (with single-element tuples unwrapped).
    /// </summary>
    /// <param name="elmModuleText">
    /// Source code of the Elm module to parse.
    /// </param>
    /// <returns>
    /// Result containing either the parsed file structure or an error description.
    /// </returns>
    public static Result<ElmSyntaxParseError, SyntaxTypes.File> ParseModuleText(
        string elmModuleText)
    {
        var tokenizer = new Tokenizer(elmModuleText);

        if (!TryUnwrap(tokenizer.Tokenize(), out var tokens, out var tokenizeErr))
        {
            return tokenizeErr;
        }

        var parser = new Parser(tokens);

        return parser.ParseFile().ToPublicResult();
    }

    /// <summary>
    /// Contains the module name and import information extracted from a module header.
    /// This is used for building module dependency graphs before full parsing.
    /// </summary>
    /// <param name="ModuleName">The name of the module.</param>
    /// <param name="ImportedModuleNames">Names of modules imported by this module.</param>
    public record ModuleHeaderInfo(
        ModuleName ModuleName,
        IReadOnlyList<ModuleName> ImportedModuleNames);

    /// <summary>
    /// Extracts module name and import information from Elm module text with error recovery.
    /// This method can extract header information even from incomplete or malformed modules,
    /// as long as the module header and import statements are parseable.
    /// Used for building module dependency graphs before full parsing.
    /// </summary>
    /// <param name="elmModuleText">Source code of the Elm module.</param>
    /// <returns>
    /// Result containing either the module header info or an error if the module header itself cannot be parsed.
    /// </returns>
    public static Result<string, ModuleHeaderInfo> ParseModuleHeader(
        string elmModuleText)
    {
        var tokenizer = new Tokenizer(elmModuleText);

        if (!TryUnwrap(tokenizer.Tokenize(), out var tokens, out var tokenizeErr))
        {
            return ElmSyntaxParseError.RenderDisplayString(tokenizeErr);
        }

        var parser = new Parser(tokens);

        return parser.ParseModuleHeaderWithRecovery();
    }

    /// <summary>
    /// Parses a standalone Elm expression directly from <paramref name="elmExpressionText"/>,
    /// without wrapping it in a synthetic module declaration.
    /// The parser always preserves parentheses/tuples in the syntax tree.
    /// Use <see cref="ElmSyntaxAbstract.ConvertFromConcrete.FromExpression(SyntaxTypes.Expression)"/>
    /// to get the abstract syntax model that matches the original stil4m/elm-syntax behavior.
    /// </summary>
    /// <param name="elmExpressionText">Source text of the Elm expression to parse.</param>
    /// <returns>
    /// Result containing either the parsed expression or an error description.
    /// </returns>
    public static Result<string, SyntaxTypes.Expression> ParseExpression(
        string elmExpressionText)
    {
        var tokenizer = new Tokenizer(elmExpressionText);

        if (!TryUnwrap(tokenizer.Tokenize(), out var tokens, out var tokenizeErr))
        {
            return ElmSyntaxParseError.RenderDisplayString(tokenizeErr);
        }

        var parser = new Parser(tokens);

        return parser.ParseExpressionTopLevel();
    }

    /// <summary>
    /// Union of the two top-level syntax forms returned by
    /// <see cref="ParseDeclarationOrExpression(string)"/>: either a top-level
    /// <see cref="SyntaxTypes.Declaration"/> or a standalone
    /// <see cref="SyntaxTypes.Expression"/>.
    /// </summary>
    public abstract record DeclarationOrExpression
    {
        /// <summary>
        /// The parsed input was a top-level declaration (for example a function,
        /// type, type alias, port or infix declaration).
        /// </summary>
        public sealed record DeclarationSyntax(SyntaxTypes.Declaration Declaration)
            : DeclarationOrExpression;

        /// <summary>
        /// The parsed input was a standalone expression.
        /// </summary>
        public sealed record ExpressionSyntax(SyntaxTypes.Expression Expression)
            : DeclarationOrExpression;
    }

    /// <summary>
    /// Parses <paramref name="elmText"/> as either a top-level Elm declaration or a
    /// standalone expression, returning whichever form the input represents.
    /// <para>
    /// Inputs starting with the <c>type</c>, <c>port</c> or <c>infix</c> keywords, as well as
    /// function/value declarations of the form <c>name args = body</c> (optionally preceded by a
    /// <c>name : Type</c> signature), are parsed as declarations. Any other input is parsed as an
    /// expression.
    /// </para>
    /// </summary>
    /// <param name="elmText">Source text of the Elm declaration or expression to parse.</param>
    /// <returns>
    /// Result containing either the parsed declaration-or-expression union or an error description.
    /// </returns>
    public static Result<string, DeclarationOrExpression> ParseDeclarationOrExpression(
        string elmText)
    {
        var tokenizer = new Tokenizer(elmText);

        if (!TryUnwrap(tokenizer.Tokenize(), out var tokens, out var tokenizeErr))
        {
            return ElmSyntaxParseError.RenderDisplayString(tokenizeErr);
        }

        var parser = new Parser(tokens);

        return parser.ParseDeclarationOrExpressionTopLevel();
    }

    private record InfixOperatorInfo(
        int Precedence,
        InfixDirection Direction)
    {
        public static InfixOperatorInfo? GetInfo(string symbol) =>
            /*
             * module Basics:
             * ----
            infix right 0 (<|) = apL
            infix left  0 (|>) = apR
            infix right 2 (||) = or
            infix right 3 (&&) = and
            infix non   4 (==) = eq
            infix non   4 (/=) = neq
            infix non   4 (<) = lt
            infix non   4 (>) = gt
            infix non   4 (<=) = le
            infix non   4 (>=) = ge
            infix right 5 (++) = append
            infix left  6 (+) = add
            infix left  6 (-) = sub
            infix left  7 (*) = mul
            infix left  7 (//) = idiv
            infix right 8 (^) = pow
            infix left  9 (<<) = composeL
            infix right 9 (>>) = composeR

            * module List:
            * ----
            infix right 5 (::) = cons

            * module Parser
            * ----
            infix left  5 (|=) = keeper
            infix left  6 (|.) = ignorer

            https://github.com/elm/url/blob/384b1dcf84065a500a71402ec367f3982b35093d/src/Url/Parser.elm#L49-L50
            infix right 7 (</>) = slash
            infix left  8 (<?>) = questionMark

             * */
            symbol switch
            {
                "<|" => new InfixOperatorInfo(0, InfixDirection.Right),
                "|>" => new InfixOperatorInfo(0, InfixDirection.Left),

                "||" => new InfixOperatorInfo(2, InfixDirection.Right),
                "&&" => new InfixOperatorInfo(3, InfixDirection.Right),

                "==" => new InfixOperatorInfo(4, InfixDirection.Non),
                "/=" => new InfixOperatorInfo(4, InfixDirection.Non),

                "<" => new InfixOperatorInfo(4, InfixDirection.Non),
                ">" => new InfixOperatorInfo(4, InfixDirection.Non),

                "<=" => new InfixOperatorInfo(4, InfixDirection.Non),
                ">=" => new InfixOperatorInfo(4, InfixDirection.Non),

                "++" => new InfixOperatorInfo(5, InfixDirection.Right),

                "::" => new InfixOperatorInfo(5, InfixDirection.Right),

                "+" => new InfixOperatorInfo(6, InfixDirection.Left),
                "-" => new InfixOperatorInfo(6, InfixDirection.Left),
                "*" => new InfixOperatorInfo(7, InfixDirection.Left),
                "//" => new InfixOperatorInfo(7, InfixDirection.Left),
                "/" => new InfixOperatorInfo(7, InfixDirection.Left),
                "^" => new InfixOperatorInfo(8, InfixDirection.Right),

                "<<" => new InfixOperatorInfo(9, InfixDirection.Left),
                ">>" => new InfixOperatorInfo(9, InfixDirection.Right),

                "|=" => new InfixOperatorInfo(5, InfixDirection.Left),
                "|." => new InfixOperatorInfo(6, InfixDirection.Left),

                "</>" => new InfixOperatorInfo(7, InfixDirection.Right),
                "<?>" => new InfixOperatorInfo(8, InfixDirection.Left),

                // Unknown operator symbol: not one of the recognized infix operators above.
                // Returning null (rather than throwing) lets the caller treat it as "does not
                // continue an infix expression" so the surrounding expression/statement parsing
                // can report a normal parse error for the leftover token.
                _ =>
                null,
            };
    }

    private record Token(
        TokenType Type,
        string Lexeme,
        Location Start,
        Location End,
        string? RawText = null)
    {
        public Range Range =>
            new(Start, End);
    }

    private enum TokenType
    {
        /*
         * TODO: Explore using dedicated tokens for keywords like 'case', 'of', 'let', 'in', etc.
         * This might simplify expression parsing code.
         * */

        Identifier,
        StringLiteral,
        TripleQuotedStringLiteral,
        CharLiteral,
        NumberLiteral,
        GLSLLiteral,
        OpenParen,
        CloseParen,
        OpenBrace,
        CloseBrace,
        OpenBracket,
        CloseBracket,
        Comma,
        Dot,
        DotDot,
        Equal,
        Arrow,
        Colon,
        Pipe,
        Comment,
        Whitespace,
        Newline,
        Lambda,
        Operator,
        Negation,
        Unknown,

        /// <summary>
        /// Sentinel token type returned by <see cref="Parser.Peek"/> once the cursor has moved past
        /// the last real token. Modeling "no more input" as an ordinary (non-throwing) token keeps
        /// every existing <c>Peek.Type is ...</c> check correct without special-casing end-of-input,
        /// since no real token type ever matches <see cref="EndOfFile"/>.
        /// </summary>
        EndOfFile,
    }

    private static bool IsKeyword(Token token)
    {
        if (token.Type is TokenType.Identifier)
        {
            return token.Lexeme switch
            {
                "case" or "of" or "let" or "in" or "if" or "then" or "else" or "import" or "module" or "exposing" =>
                true,

                _ =>
                false,
            };
        }

        return false;
    }

    /// <summary>
    /// Checks if a token is a keyword that can start a top-level declaration.
    /// </summary>
    private static bool IsDeclarationStartKeyword(Token token)
    {
        if (token.Type is TokenType.Identifier)
        {
            return token.Lexeme switch
            {
                "type" or "port" or "infix" or "import" =>
                true,

                _ =>
                false,
            };
        }

        return false;
    }

    /// <summary>
    /// Checks if a token marks the start of a new declaration at the module level.
    /// A declaration boundary is a token at column 1 that can start a declaration.
    /// </summary>
    private static bool IsDeclarationBoundary(Token token)
    {
        // Must be at column 1 to start a new declaration
        if (token.Start.Column is not 1)
        {
            return false;
        }

        // Comments and declaration keywords at column 1 indicate a new declaration
        if (token.Type is TokenType.Comment)
        {
            return true;
        }

        // Declaration keywords (type, port, infix) at column 1 start declarations
        if (IsDeclarationStartKeyword(token))
        {
            return true;
        }

        // Non-keyword identifiers at column 1 start function declarations
        if (token.Type is TokenType.Identifier && !IsKeyword(token))
        {
            return true;
        }

        return false;
    }

    /// <summary>
    /// Reconstructs readable text from a list of tokens, inserting appropriate whitespace.
    /// </summary>
    private static string ReconstructTextFromTokens(IReadOnlyList<Token> tokens)
    {
        if (tokens.Count is 0)
        {
            return string.Empty;
        }

        var sb = new StringBuilder();
        Token? previousToken = null;

        foreach (var token in tokens)
        {
            if (previousToken is not null)
            {
                // Add newlines if tokens are on different rows
                var rowDiff = token.Start.Row - previousToken.End.Row;

                if (rowDiff > 0)
                {
                    for (var i = 0; i < rowDiff; i++)
                    {
                        sb.Append('\n');
                    }
                    // Add indentation for the new line
                    for (var i = 1; i < token.Start.Column; i++)
                    {
                        sb.Append(' ');
                    }
                }
                else
                {
                    // Same row - add spaces between tokens
                    var columnDiff = token.Start.Column - previousToken.End.Column;

                    for (var i = 0; i < columnDiff; i++)
                    {
                        sb.Append(' ');
                    }
                }
            }

            sb.Append(LexemeSourceText(token));
            previousToken = token;
        }

        return sb.ToString();
    }

    /// <summary>
    /// Returns the source-text representation of a token for reconstructing the
    /// original text of an incomplete declaration. For string and character
    /// literals the lexeme holds the decoded value (without the surrounding
    /// quotes and with escapes resolved), so we restore the delimiters and the
    /// raw, unescaped content captured during tokenization.
    /// </summary>
    private static string LexemeSourceText(Token token)
    {
        return token.Type switch
        {
            TokenType.StringLiteral =>
            "\"" + (token.RawText ?? token.Lexeme) + "\"",

            TokenType.TripleQuotedStringLiteral =>
            "\"\"\"" + (token.RawText ?? token.Lexeme) + "\"\"\"",

            TokenType.CharLiteral =>
            "'" + (token.RawText ?? token.Lexeme) + "'",

            _ =>
            token.Lexeme,
        };
    }

    /// <summary>
    /// Trims trailing empty lines from a string, keeping content up to and including the last non-empty line.
    /// </summary>
    private static string TrimTrailingEmptyLines(string text)
    {
        if (string.IsNullOrEmpty(text))
        {
            return text;
        }

        var lines = text.Split('\n');
        var lastNonEmptyIndex = -1;

        for (var i = lines.Length - 1; i >= 0; i--)
        {
            if (!string.IsNullOrWhiteSpace(lines[i]))
            {
                lastNonEmptyIndex = i;
                break;
            }
        }

        if (lastNonEmptyIndex < 0)
        {
            // All lines are empty
            return string.Empty;
        }

        // Return lines up to and including the last non-empty line
        return string.Join("\n", lines.Take(lastNonEmptyIndex + 1));
    }

    /// <summary>
    /// Calculates the end location given a start location and text content.
    /// </summary>
    private static Location CalculateEndLocation(Location start, string text)
    {
        var row = start.Row;
        var column = start.Column;

        foreach (var ch in text)
        {
            if (ch is '\n')
            {
                row++;
                column = 1;
            }
            else
            {
                column++;
            }
        }

        return new Location(row, column);
    }

    private static bool CharCanAppearInOperator(char Char) =>
        Char switch
        {
            '|' or '>' or '<' or '.' or ':' or '%' or '^' or '&' or '*' or '/' or '-' or '+' or '=' or '?' =>
            true,

            _ =>
            false,
        };

    internal readonly struct ParseResult<T>
    {
        private ParseResult(bool isOk, T? value, ElmSyntaxParseError error)
        {
            IsOk = isOk;
            Value = value;
            Error = error;
        }

        public bool IsOk { get; }

        public T? Value { get; }

        public ElmSyntaxParseError Error { get; }

        public static implicit operator ParseResult<T>(T value) =>
            new(isOk: true, value, error: default);

        public static implicit operator ParseResult<T>(ElmSyntaxParseError error) =>
            new(isOk: false, value: default, error);

        public Result<ElmSyntaxParseError, T> ToPublicResult() =>
            IsOk
            ?
            Result<ElmSyntaxParseError, T>.ok(Value!)
            :
            Result<ElmSyntaxParseError, T>.err(Error);
    }

    private static bool TryUnwrap<T>(
        ParseResult<T> result,
        out T value,
        out ElmSyntaxParseError error)
    {
        if (result.IsOk)
        {
            value = result.Value!;
            error = default;
            return true;
        }

        value = default!;
        error = result.Error;
        return false;
    }

    /// <summary>
    /// Total (non-throwing) equivalent of <see cref="char.ConvertToUtf32(string, int)"/> for the
    /// decoded contents of a char literal or char pattern token: returns <see langword="false"/>
    /// instead of throwing whenever <paramref name="text"/> is empty or starts with an unpaired
    /// surrogate, so malformed input can be reported as a parse error rather than a runtime crash.
    /// </summary>
    private static bool TryConvertToUtf32(string text, out int codePoint)
    {
        if (text.Length is 0)
        {
            codePoint = 0;
            return false;
        }

        if (char.IsHighSurrogate(text[0]))
        {
            if (text.Length > 1 && char.IsLowSurrogate(text[1]))
            {
                codePoint = char.ConvertToUtf32(text[0], text[1]);
                return true;
            }

            codePoint = 0;
            return false;
        }

        if (char.IsLowSurrogate(text[0]))
        {
            codePoint = 0;
            return false;
        }

        codePoint = text[0];
        return true;
    }

    private class Tokenizer(string input)
    {
        private readonly string _input = input;

        private int _position;

        private int _line = 1;

        private int _column = 1;

        public ParseResult<Token[]> Tokenize()
        {
            var tokens = new List<Token>();

            while (!IsAtEnd())
            {
                if (!TryUnwrap(NextToken(), out var token, out var err))
                {
                    return err;
                }

                tokens.Add(token);
            }

            return tokens.ToArray();
        }

        private bool IsAtEnd() =>
            _position >= _input.Length;

        private char Peek() =>
            IsAtEnd()
            ?
            '\0'
            :
            _input[_position];

        private bool LookAhead(string expected)
        {
            if (_position + expected.Length > _input.Length)
                return false;

            for (var i = 0; i < expected.Length; i++)
            {
                if (_input[_position + i] != expected[i])
                    return false;
            }

            return true;
        }

        private char PeekNext() =>
            PeekNext(1);

        private char PeekNext(int offset) =>
            (_position + offset < _input.Length)
            ?
            _input[_position + offset]
            :
            '\0';

        private char PeekPrevious(int offset = 1) =>
            (_position - offset >= 0)
            ?
            _input[_position - offset]
            :
            '\0';

        /// <summary>
        /// Checks if a character can be the last character of an expression,
        /// meaning a minus sign after it should be treated as an operator rather than negation.
        /// </summary>
        private static bool CharCanEndExpression(char c) =>
            char.IsLetterOrDigit(c) || c is ')' || c is ']' || c is '}' || c is '_' || c is '\'';

        private char Advance()
        {
            var current = _input[_position];
            _position++;

            if (current is '\n')
            {
                _line++;
                _column = 1;
            }
            else if (current is '\r')
            {
                // \r does not affect position tracking - it's part of CRLF line ending
                // and will be handled as part of the newline sequence
                // We don't increment column for \r since it's not visible content
            }
            else
            {
                // Only increment column for non-surrogate characters and low surrogates
                // High surrogates (first half of a surrogate pair) don't increment the column
                // because we want to count UTF-32 code points, not UTF-16 code units
                if (!char.IsHighSurrogate(current))
                {
                    _column++;
                }
            }

            return current;
        }

        private ParseResult<Token> NextToken()
        {
            // Capture the start location for this token.
            Location start = new(_line, _column);
            var current = Peek();

            // Handle whitespace and newlines
            // We treat \n, \r\n, and lone \r all as line breaks
            if (char.IsWhiteSpace(current))
            {
                if (current is '\n')
                {
                    Advance();
                    Location end = new(_line, _column);
                    return new Token(TokenType.Newline, "\n", start, end);
                }
                else if (current is '\r')
                {
                    // Handle \r\n (CRLF) or lone \r as a single newline
                    Advance(); // Consume \r (doesn't affect line count, column stays same)

                    if (Peek() is '\n')
                    {
                        Advance(); // Consume \n following \r (this increments line)
                    }
                    else
                    {
                        // Lone \r - manually increment line since Advance() on \r doesn't
                        _line++;
                        _column = 1;
                    }

                    Location end = new(_line, _column);
                    return new Token(TokenType.Newline, "\n", start, end);
                }
                else
                {
                    var whitespace = new StringBuilder(capacity: 16);

                    // Stop at \r or \n since both can be line endings
                    while (!IsAtEnd() && char.IsWhiteSpace(Peek()) && Peek() is not '\n' && Peek() is not '\r')
                    {
                        whitespace.Append(Advance());
                    }

                    Location end = new(_line, _column);

                    return new Token(TokenType.Whitespace, whitespace.ToString(), start, end);
                }
            }

            // Handle single-line comments
            if (current is '-' && PeekNext() is '-')
            {
                var comment = "";

                // Stop at either \r or \n since both can be line endings
                while (!IsAtEnd() && Peek() is not '\n' && Peek() is not '\r')
                {
                    comment += Advance();
                }

                Location end = new(_line, _column);

                return new Token(TokenType.Comment, comment, start, end);
            }

            /*
             * Parse multi-line comments, which start with '{-' and end with '-}'
             * Multi-line comments can be nested.
             * */
            if (Peek() is '{' && PeekNext() is '-')
            {
                var comment = new StringBuilder(capacity: 100);

                comment.Append(Advance()); // Consume '{'

                comment.Append(Advance()); // Consume '-'

                var nestedLevel = 1;

                while (!IsAtEnd() && nestedLevel > 0)
                {
                    if (Peek() is '{' && PeekNext() is '-')
                    {
                        comment.Append(Advance()); // Consume '{'
                        comment.Append(Advance()); // Consume '-'
                        nestedLevel++;
                    }
                    else if (Peek() is '-' && PeekNext() is '}')
                    {
                        comment.Append(Advance()); // Consume '-'
                        comment.Append(Advance()); // Consume '}'
                        nestedLevel--;
                    }
                    else
                    {
                        comment.Append(Advance());
                    }
                }

                if (nestedLevel > 0)
                {
                    // Unterminated multi-line comment; here you might want to throw an error.
                    return new Token(TokenType.Unknown, comment.ToString(), start, new Location(_line, _column));
                }

                Location end = new(_line, _column);

                return new Token(TokenType.Comment, comment.ToString(), start, end);
            }

            // Handle string literals
            if (current is '"')
            {
                Advance(); // Consume the opening quote

                // Check if this is a triple-quoted string

                if (Peek() is '"' && PeekNext() is '"')
                {
                    Advance(); // Consume the first quote
                    Advance(); // Consume the second quote

                    var innerStart = _position;

                    if (!TryUnwrap(ParseStringLiteral(termination: "\"\"\""), out var literal, out var literalErr))
                    {
                        return literalErr;
                    }

                    if (literal is not null)
                    {
                        Location end = new(_line, _column);

                        var rawText = CaptureRawLiteralText(innerStart, terminationLength: 3);

                        return new Token(TokenType.TripleQuotedStringLiteral, literal, start, end, rawText);
                    }
                }
                else
                {
                    var innerStart = _position;

                    if (!TryUnwrap(ParseStringLiteral(termination: "\""), out var literal, out var literalErr))
                    {
                        return literalErr;
                    }

                    if (literal is not null)
                    {
                        Location end = new(_line, _column);

                        var rawText = CaptureRawLiteralText(innerStart, terminationLength: 1);

                        return new Token(TokenType.StringLiteral, literal, start, end, rawText);
                    }
                }

                // Unterminated string literal.
                return
                    new ElmSyntaxParseError(
                        new Location(_line, _column),
                        "Unterminated string literal");
            }

            // Handle character literals
            if (current is '\'' && PeekNext() is not '\'')
            {
                Advance(); // Consume the opening quote

                var innerStart = _position;

                if (!TryUnwrap(ParseStringLiteral(termination: "'"), out var literal, out var literalErr))
                {
                    return literalErr;
                }

                if (literal is not null)
                {
                    Location end = new(_line, _column);

                    var rawText = CaptureRawLiteralText(innerStart, terminationLength: 1);

                    return new Token(TokenType.CharLiteral, literal, start, end, rawText);
                }

                // Unterminated character literal.
                return
                    new ElmSyntaxParseError(
                        new Location(_line, _column),
                        "Unterminated character literal");
            }

            // Handle number literals
            if (char.IsDigit(current))
            {
                var number = new StringBuilder(capacity: 16);

                var isHex = false;

                if (current is '0' && PeekNext() is 'x')
                {
                    number.Append(Advance()); // Consume '0'
                    number.Append(Advance()); // Consume 'x'

                    isHex = true;
                }

                while (!IsAtEnd())
                {
                    var nextPeeked = Peek();

                    if (!(isHex && char.IsAsciiHexDigit(nextPeeked) || char.IsDigit(nextPeeked)))
                    {
                        break;
                    }

                    number.Append(Advance());
                }

                // Handle decimal point for floats (only for non-hex numbers)
                if (!isHex && Peek() is '.' && PeekNext() is char nextChar && char.IsDigit(nextChar))
                {
                    number.Append(Advance()); // Consume '.'

                    while (!IsAtEnd() && char.IsDigit(Peek()))
                    {
                        number.Append(Advance());
                    }
                }

                // Handle exponent notation for floats (only for non-hex numbers)
                if (!isHex && Peek() is 'e' or 'E')
                {
                    number.Append(Advance()); // Consume 'e' or 'E'

                    // Optional '+' or '-' after exponent
                    if (Peek() is '+' or '-')
                    {
                        number.Append(Advance());
                    }

                    while (!IsAtEnd() && char.IsDigit(Peek()))
                    {
                        number.Append(Advance());
                    }
                }

                Location end = new(_line, _column);

                return new Token(TokenType.NumberLiteral, number.ToString(), start, end);
            }

            // Handle identifiers (start with letter or underscore, then letters, digits or underscores)
            if (char.IsLetter(current) || current is '_')
            {
                var identifierBuilder = new StringBuilder(capacity: 16);

                while (!IsAtEnd() && (char.IsLetterOrDigit(Peek()) || Peek() is '_'))
                {
                    identifierBuilder.Append(Advance());
                }

                Location end = new(_line, _column);

                var identifier = identifierBuilder.ToString();

                identifier = string.IsInterned(identifier) ?? identifier;

                return new Token(TokenType.Identifier, identifier, start, end);
            }

            // Handle single-character tokens and multi-character tokens like the arrow "->"
            switch (current)
            {
                case '(':
                    Advance();
                    return new Token(TokenType.OpenParen, "(", start, new Location(_line, _column));

                case ')':
                    Advance();
                    return new Token(TokenType.CloseParen, ")", start, new Location(_line, _column));

                case '{':
                    Advance();
                    return new Token(TokenType.OpenBrace, "{", start, new Location(_line, _column));

                case '}':
                    Advance();
                    return new Token(TokenType.CloseBrace, "}", start, new Location(_line, _column));

                case '[':
                    Advance();

                    // Check for GLSL literal: [glsl| ... |]
                    if (LookAhead("glsl|"))
                    {
                        // Consume "glsl|"
                        for (var k = 0; k < 5; k++)
                            Advance();

                        // Read until |]
                        var glslContent = new StringBuilder();

                        while (!IsAtEnd())
                        {
                            if (Peek() is '|' && PeekNext() is ']')
                            {
                                Advance(); // |
                                Advance(); // ]
                                break;
                            }

                            glslContent.Append(Advance());
                        }

                        return
                            new Token(
                                TokenType.GLSLLiteral,
                                glslContent.ToString(),
                                start,
                                new Location(_line, _column));
                    }

                    return new Token(TokenType.OpenBracket, "[", start, new Location(_line, _column));

                case ']':
                    Advance();
                    return new Token(TokenType.CloseBracket, "]", start, new Location(_line, _column));

                case ',':
                    Advance();
                    return new Token(TokenType.Comma, ",", start, new Location(_line, _column));

                case '.':
                    Advance();

                    if (Peek() is '.')
                    {
                        Advance();
                        return new Token(TokenType.DotDot, "..", start, new Location(_line, _column));
                    }

                    // Check if this is part of a two-character operator:

                    if (CharCanAppearInOperator(Peek()))
                    {
                        var operatorToken = "." + Advance();

                        return
                            new Token(
                                TokenType.Operator,
                                operatorToken,
                                start,
                                new Location(_line, _column));
                    }

                    return new Token(TokenType.Dot, ".", start, new Location(_line, _column));

                case '=':

                    Advance();

                    // Check if this is part of a two-character operator:
                    if (CharCanAppearInOperator(Peek()))
                    {
                        var operatorToken = "=" + Advance();

                        return
                            new Token(
                                TokenType.Operator,
                                operatorToken,
                                start,
                                new Location(_line, _column));
                    }

                    return new Token(TokenType.Equal, "=", start, new Location(_line, _column));

                case '|':

                    Advance();

                    // Check if this is part of a two-character operator:

                    if (CharCanAppearInOperator(Peek()))
                    {
                        var operatorToken = "|" + Advance();

                        return
                            new Token(
                                TokenType.Operator,
                                operatorToken,
                                start,
                                new Location(_line, _column));
                    }

                    return new Token(TokenType.Pipe, "|", start, new Location(_line, _column));


                case '/':

                    Advance();

                    // Check if this is part of a two-character operator:

                    if (CharCanAppearInOperator(Peek()))
                    {
                        var operatorToken = current.ToString() + Advance();

                        return
                            new Token(
                                TokenType.Operator,
                                operatorToken,
                                start,
                                new Location(_line, _column));
                    }

                    return new Token(TokenType.Operator, current.ToString(), start, new Location(_line, _column));

                case '-':

                    // Check if this is part of an arrow token "->"
                    if (PeekNext() is '>')
                    {
                        Advance(); // Consume '-'
                        Advance(); // Consume '>'
                        return new Token(TokenType.Arrow, "->", start, new Location(_line, _column));
                    }

                    // Check if the previous character can end an expression
                    // If so, the minus is an infix operator, not a negation
                    var prevChar = PeekPrevious();

                    if (CharCanEndExpression(prevChar))
                    {
                        Advance();
                        return new Token(TokenType.Operator, "-", start, new Location(_line, _column));
                    }

                    switch (PeekNext())
                    {
                        case ' ':
                        case '\n':
                        case '\r':
                        case '\t':
                        case ')':
                            Advance();
                            return new Token(TokenType.Operator, "-", start, new Location(_line, _column));
                    }

                    {
                        Advance();
                        return new Token(TokenType.Negation, "-", start, new Location(_line, _column));
                    }

                case '\\':
                    Advance();

                    return new Token(TokenType.Lambda, "\\", start, new Location(_line, _column));

                case ':':

                    Advance();

                    // Check if this is part of a two-character operator:

                    if (CharCanAppearInOperator(Peek()))
                    {
                        var operatorToken = ":" + Advance();

                        return
                            new Token(
                                TokenType.Operator,
                                operatorToken,
                                start,
                                new Location(_line, _column));
                    }

                    return new Token(TokenType.Colon, ":", start, new Location(_line, _column));

                case '<':
                case '>':
                default:

                    if (CharCanAppearInOperator(current))
                    {
                        var operatorToken = Advance().ToString();

                        if (CharCanAppearInOperator(Peek()))
                        {
                            operatorToken += Advance();
                        }

                        if (CharCanAppearInOperator(Peek()))
                        {
                            operatorToken += Advance();
                        }

                        return
                            new Token(
                                TokenType.Operator,
                                operatorToken,
                                start,
                                new Location(_line, _column));
                    }

                    // For any unknown character, consume it and return an Unknown token.
                    Advance();
                    return new Token(TokenType.Unknown, current.ToString(), start, new Location(_line, _column));
            }
        }

        /// <summary>
        /// Captures the original source text of a string literal's content (the characters between
        /// the opening and closing quotes, excluding the closing termination characters that were
        /// already consumed by <see cref="ParseStringLiteral"/>). Physical line endings are normalized
        /// to '\n' to match the file-wide linebreak normalization, while escape sequences are kept
        /// verbatim so the renderer can reproduce the original representation of characters.
        /// </summary>
        private string CaptureRawLiteralText(int innerStart, int terminationLength)
        {
            var innerEnd = _position - terminationLength;

            if (innerEnd < innerStart)
            {
                innerEnd = innerStart;
            }

            var raw = _input[innerStart..innerEnd];

            return raw.Replace("\r\n", "\n").Replace("\r", "\n");
        }

        private ParseResult<string?> ParseStringLiteral(string termination)
        {
            var terminationFirstChar = termination[0];

            var sb = new StringBuilder();

            while (!IsAtEnd())
            {
                if (Peek() == terminationFirstChar)
                {
                    var matchesTermination = true;

                    for (var i = 1; i < termination.Length; i++)
                    {
                        if (PeekNext(i) != termination[i])
                        {
                            matchesTermination = false;
                            break;
                        }
                    }

                    if (matchesTermination)
                    {
                        for (var i = 0; i < termination.Length; i++)
                        {
                            Advance(); // Consume the termination characters
                        }

                        return sb.ToString();
                    }
                }

                // If we see a backslash, check for an escaped character.
                if (Peek() is '\\')
                {
                    Advance(); // Consume the backslash

                    if (!IsAtEnd())
                    {
                        var escaped = Advance(); // Consume the character after the backslash
                        // Handle specific escape sequences
                        if (escaped is '"')
                        {
                            sb.Append('"'); // Escaped quote
                        }
                        else if (escaped is 'n')
                        {
                            sb.Append('\n'); // Newline escape
                        }
                        else if (escaped is 't')
                        {
                            sb.Append('\t'); // Tab escape
                        }
                        else if (escaped is '\\')
                        {
                            sb.Append('\\'); // Backslash escape
                        }
                        // pattern like  "\u{000C}"
                        else if (escaped is 'u' && Peek() is '{')
                        {
                            Advance(); // Consume the '{'

                            var unicode = "";

                            while (!IsAtEnd() && Peek() is not '}')
                            {
                                unicode += Advance();
                            }

                            if (!IsAtEnd())
                            {
                                Advance(); // Consume the '}'

                                if (!int.TryParse(
                                    unicode,
                                    System.Globalization.NumberStyles.HexNumber,
                                    System.Globalization.CultureInfo.InvariantCulture,
                                    out var codePoint) ||
                                    codePoint < 0 ||
                                    codePoint > 0x10FFFF ||
                                    (codePoint >= 0xD800 && codePoint <= 0xDFFF))
                                {
                                    return
                                        new ElmSyntaxParseError(
                                            new Location(_line, _column),
                                            "Invalid unicode escape sequence: \\u{" + unicode + "}");
                                }

                                sb.Append(char.ConvertFromUtf32(codePoint));
                            }
                        }
                        else if (escaped is 'r')
                        {
                            sb.Append('\r'); // Carriage return escape
                        }
                        else if (escaped is 'b')
                        {
                            sb.Append('\b'); // Backspace escape
                        }
                        else
                        {
                            // For any other escape, just append the character (or handle error)
                            sb.Append(escaped);
                        }
                    }
                }
                else if (Peek() is '\r')
                {
                    // Normalize line endings inside string literals:
                    // \r\n -> \n (CRLF)
                    // \r -> \n (lone CR)
                    Advance(); // Consume the \r

                    if (Peek() is '\n')
                    {
                        Advance(); // Consume the \n following \r
                    }

                    sb.Append('\n'); // Always append just \n
                }
                else
                {
                    sb.Append(Advance());
                }
            }

            return (string?)null;
        }
    }

    private class Parser(
        ReadOnlyMemory<Token> tokens)
    {
        private int _current = 0;

        // Entry point: parse the entire file and return a File record.
        public ParseResult<SyntaxTypes.File> ParseFile()
        {
            IReadOnlyList<Token> allComments =
                [.. tokens.ToArray().Where(t => t.Type is TokenType.Comment)];

            IReadOnlyList<Token> docComments =
                [.. allComments.Where(c => c.Lexeme.StartsWith("{-|"))];

            ConsumeAllTrivia();

            // Parse the module header
            if (!TryUnwrap(ParseModule(), out var moduleDefinition, out var moduleErr))
            {
                return moduleErr;
            }

            ConsumeAllTrivia();

            // Parse the imports (if any). Failures here are not recoverable: unlike the imports
            // parsed inside the declarations loop below, there are no preceding declarations to
            // fall back on, so a leading import that fails to parse fails the whole file.
            var imports = new List<Node<SyntaxTypes.Import>>();

            while (NextTokenMatches(t => t.Type is TokenType.Identifier && t.Lexeme is "import"))
            {
                if (!TryUnwrap(ParseImport(), out var import, out var importErr))
                {
                    return importErr;
                }

                imports.Add(import);

                ConsumeAllTrivia();
            }

            var declarations = new List<Node<SyntaxTypes.Declaration>>();
            var incompleteDeclarations = new List<Node<IncompleteDeclaration>>();

            ConsumeAllTrivia();

            // Parse the declarations until we've consumed all tokens.
            while (!IsAtEnd())
            {
                if (Peek.Start.Column is not 1)
                {
                    return
                        ErrorAtCurrentLocation(
                            "Unexpected token '" + Peek.Lexeme + "' after parsing " +
                            declarations.Count + " declarations");
                }

                bool CanAttachComment(Token commentToken)
                {
                    // Check if there are any other comments (non-doc) between this doc comment and the declaration
                    var hasInterveningComments =
                        allComments.Any(
                            c =>
                            !c.Lexeme.StartsWith("{-|") && // not a doc comment
                            c.Start.Row > commentToken.End.Row && // after this doc comment
                            c.Start.Row < Peek.Start.Row); // before the declaration

                    if (hasInterveningComments)
                        return false;

                    if (declarations.Count is 0)
                    {
                        if (imports.LastOrDefault() is { } lastImport &&
                            lastImport.Range.End.Row < commentToken.End.Row)
                        {
                            return commentToken.End.Row < Peek.Start.Row;
                        }

                        // If there are no imports (or comment is before imports), check if comment is immediately before declaration
                        return commentToken.End.Row + 1 == Peek.Start.Row;
                    }
                    else
                    {
                        // For subsequent declarations, check if the comment is between the last declaration and the current one
                        if (declarations.LastOrDefault() is { } lastDecl)
                        {
                            return
                                lastDecl.Range.End.Row < commentToken.End.Row &&
                                commentToken.End.Row < Peek.Start.Row;
                        }
                    }

                    return commentToken.End.Row + 1 == Peek.Start.Row;
                }

                var docComment =
                    docComments
                    .LastOrDefault(CanAttachComment);

                // Save position before attempting to parse
                var declStartPosition = _current;
                var declStartToken = Peek;

                ParseResult<Node<SyntaxTypes.Import>>? importAttempt = null;
                ParseResult<Node<SyntaxTypes.Declaration>>? declarationAttempt = null;

                if (NextTokenMatches(token => token.Type is TokenType.Identifier && token.Lexeme is "import"))
                {
                    importAttempt = ParseImport();
                }
                else
                {
                    declarationAttempt = ParseDeclaration(docComment);
                }

                var attemptError =
                    importAttempt is { IsOk: false } importErrCase
                    ?
                    importErrCase.Error
                    :
                    declarationAttempt is { IsOk: false } declErrCase
                    ?
                    declErrCase.Error
                    :
                    (ElmSyntaxParseError?)null;

                if (attemptError is { } error)
                {
                    // Declaration parsing failed - capture as incomplete declaration.
                    // Skip to the next declaration boundary (column 1 token that can start a declaration)
                    var incompleteTokens = new List<Token>();

                    // Reset to start of failed declaration
                    _current = declStartPosition;

                    var errorLocation = error.Location;
                    var errorMessage = error.Message;

                    if (errorMessage is "Unfinished definition" && declarations.Count is not 0)
                    {
                        errorLocation = declStartToken.Start;
                    }

                    // Collect tokens until we find the next declaration boundary or end of file
                    while (!IsAtEnd())
                    {
                        var currentToken = Peek;

                        // Check if this token starts a new declaration
                        if (incompleteTokens.Count > 0 &&
                            IsDeclarationBoundary(currentToken))
                        {
                            break;
                        }

                        incompleteTokens.Add(Advance());
                    }

                    // Store incomplete declaration with trimmed trailing empty lines
                    if (incompleteTokens.Count > 0)
                    {
                        var firstToken = incompleteTokens[0];

                        // Reconstruct the text from tokens, preserving structure
                        var incompleteText = ReconstructTextFromTokens(incompleteTokens);

                        // Trim trailing empty lines
                        incompleteText = TrimTrailingEmptyLines(incompleteText);

                        // Calculate end location based on trimmed text
                        var endLocation = CalculateEndLocation(firstToken.Start, incompleteText);
                        var range = new Range(firstToken.Start, endLocation);

                        // Add as Node<IncompleteDeclaration> with proper range and error info
                        incompleteDeclarations.Add(
                            new Node<IncompleteDeclaration>(
                                range,
                                new IncompleteDeclaration(
                                    incompleteText,
                                    new ElmSyntaxParseError(errorLocation, errorMessage))));
                    }
                }
                else if (importAttempt is { IsOk: true, Value: { } importOk })
                {
                    imports.Add(importOk);
                }
                else if (declarationAttempt is { IsOk: true, Value: { } declarationOk })
                {
                    declarations.Add(declarationOk);
                }

                ConsumeAllTrivia();
            }

            IReadOnlyList<Node<string>> commentsOnDeclarations =
                [
                ..declarations.SelectWhereNotNull(
                    decl =>
                    decl.Value switch
                    {
                        SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration =>
                        functionDeclaration.Function.Documentation,

                        SyntaxTypes.Declaration.ChoiceTypeDeclaration typeDecl =>
                        typeDecl.TypeDeclaration.Documentation,

                        SyntaxTypes.Declaration.AliasDeclaration aliasDecl =>
                        aliasDecl.TypeAlias.Documentation,

                        SyntaxTypes.Declaration.InfixDeclaration =>
                        null,

                        SyntaxTypes.Declaration.PortDeclaration =>
                        null,

                        _ =>
                        throw new NotImplementedException(
                            "Unexpected declaration type: " + decl.GetType().Name),
                    })
                ];

            bool CommentEmittedInGlobalList(Token commentToken)
            {
                if (commentsOnDeclarations.Any(onDecl => onDecl.Range == commentToken.Range))
                {
                    return false;
                }

                return true;
            }

            IReadOnlyList<Node<string>> commentsGlobalList =
                [
                .. allComments
                .Where(CommentEmittedInGlobalList)
                .Select(token => new Node<string>(token.Range, token.Lexeme))
                ];

            return
                new SyntaxTypes.File(
                    moduleDefinition,
                    imports,
                    declarations,
                    Comments: commentsGlobalList,
                    IncompleteDeclarations: incompleteDeclarations);
        }

        /// <summary>
        /// Parses module header (module definition and imports) with error recovery for imports.
        /// If an import fails to parse, it records the successfully parsed imports up to that point
        /// and returns successfully. This allows building dependency graphs even for incomplete modules.
        /// </summary>
        public Result<string, ModuleHeaderInfo> ParseModuleHeaderWithRecovery()
        {
            ConsumeAllTrivia();

            // Parse the module header
            if (!TryUnwrap(ParseModule(), out var moduleDefinition, out var moduleErr))
            {
                return ElmSyntaxParseError.RenderDisplayString(moduleErr);
            }

            var moduleName = SyntaxTypes.Module.GetModuleName(moduleDefinition.Value).Value;

            ConsumeAllTrivia();

            // Parse imports with error recovery: if an import fails to parse, stop parsing imports
            // but don't fail the whole call. This allows modules with incomplete imports to still
            // provide their name and any successfully parsed imports.
            var importedModuleNames = new List<ModuleName>();

            while (NextTokenMatches(t => t.Type is TokenType.Identifier && t.Lexeme is "import"))
            {
                if (!TryUnwrap(ParseImport(), out var import, out _))
                    break;

                importedModuleNames.Add(import.Value.ModuleName.Value);
                ConsumeAllTrivia();
            }

            return new ModuleHeaderInfo(moduleName, importedModuleNames);
        }

        // Parses the module header and returns a Node<SyntaxTypes.Module>
        private ParseResult<Node<SyntaxTypes.Module>> ParseModule()
        {
            if (NextTokenMatches(p => p.Lexeme is "effect"))
            {
                /*
                 * Example syntax:
                 * ----
                 * effect module Time where { subscription = MySub } exposing
                 * */

                if (!TryUnwrap(ConsumeKeyword("effect"), out var effectKeyword, out var effectErr))
                    return effectErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ConsumeKeyword("module"), out var moduleKeyword, out var moduleKeywordErr))
                    return moduleKeywordErr;

                ConsumeAllTrivia();

                var moduleNameParts = new List<Token>();

                if (!TryUnwrap(ConsumeAnyIdentifier("module name"), out var firstModuleNamePart, out var firstNameErr))
                    return firstNameErr;

                moduleNameParts.Add(firstModuleNamePart);

                while (Peek.Type is TokenType.Dot)
                {
                    if (!TryUnwrap(Consume(TokenType.Dot), out _, out var dotErr))
                        return dotErr;

                    if (!TryUnwrap(ConsumeAnyIdentifier("module name part"), out var moduleNamePart, out var namePartErr))
                        return namePartErr;

                    moduleNameParts.Add(moduleNamePart);
                }

                var moduleNameNode =
                    new Node<ModuleName>(
                        MakeRange(firstModuleNamePart.Start, moduleNameParts.Last().End),
                        [.. moduleNameParts.Select(t => t.Lexeme)]);

                ConsumeAllTrivia();

                Node<string>? command = null;
                Node<string>? subscription = null;

                if (NextTokenMatches(peek => peek.Lexeme is "where"))
                {
                    if (!TryUnwrap(ConsumeKeyword("where"), out _, out var whereErr))
                        return whereErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ParseRecordExpr(indentMin: 1), out var recordExprNode, out var recordExprErr))
                        return recordExprErr;

                    if (recordExprNode.Value is not SyntaxTypes.Expression.RecordExpr recordExpr)
                    {
                        return
                            ErrorAtCurrentLocation(
                                "Expected record expression after 'where', found: " +
                                recordExprNode.Value.GetType().Name);
                    }

                    foreach (var recordField in FromFullSyntaxModel.ToList(recordExpr.Fields))
                    {
                        if (recordField.FieldName.Value is "command")
                        {
                            if (recordField.ValueExpr.Value is not SyntaxTypes.Expression.FunctionOrValue functionOrValue)
                            {
                                return
                                    ErrorAtCurrentLocation(
                                        "Expected function or value for 'command', found: " +
                                        recordField.ValueExpr.GetType().Name);
                            }

                            command =
                                new Node<string>(
                                    recordField.ValueExpr.Range,
                                    functionOrValue.Name);
                        }

                        if (recordField.FieldName.Value is "subscription")
                        {
                            if (recordField.ValueExpr.Value is not SyntaxTypes.Expression.FunctionOrValue functionOrValue)
                            {
                                return
                                    ErrorAtCurrentLocation(
                                        "Expected function or value for 'subscription', found: " +
                                        recordField.ValueExpr.GetType().Name);
                            }

                            subscription =
                                new Node<string>(
                                    recordField.ValueExpr.Range,
                                    functionOrValue.Name);
                        }
                    }

                    ConsumeAllTrivia();
                }

                if (!TryUnwrap(ParseExposing(), out var exposingNode, out var exposingErr))
                    return exposingErr;

                var moduleData =
                    new SyntaxTypes.EffectModuleData(
                        ModuleName: moduleNameNode,
                        ExposingTokenLocation: exposingNode.Range.Start,
                        ExposingList: exposingNode,
                        Command: command,
                        Subscription: subscription);

                var moduleNodeValue =
                    new SyntaxTypes.Module.EffectModule(
                        EffectTokenLocation: effectKeyword.Start,
                        ModuleTokenLocation: moduleKeyword.Start,
                        ModuleData: moduleData);

                var moduleNode =
                    new Node<SyntaxTypes.Module>(MakeRange(effectKeyword.Start, exposingNode.Range.End), moduleNodeValue);

                return moduleNode;
            }

            if (NextTokenMatches(p => p.Lexeme is "port"))
            {
                /*
                 * Example syntax:
                 * ----
                 * port module Main exposing (..)
                 * */

                if (!TryUnwrap(ConsumeKeyword("port"), out var portKeyword, out var portErr))
                    return portErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ConsumeKeyword("module"), out var moduleKeyword, out var moduleKeywordErr))
                    return moduleKeywordErr;

                ConsumeAllTrivia();

                // Parse module name (e.g. Main)
                var moduleNameParts = new List<Token>();

                if (!TryUnwrap(ConsumeAnyIdentifier("module name"), out var firstModuleNamePart, out var firstNameErr))
                    return firstNameErr;

                moduleNameParts.Add(firstModuleNamePart);

                while (Peek.Type is TokenType.Dot)
                {
                    if (!TryUnwrap(Consume(TokenType.Dot), out _, out var dotErr))
                        return dotErr;

                    if (!TryUnwrap(ConsumeAnyIdentifier("module name part"), out var moduleNamePart, out var namePartErr))
                        return namePartErr;

                    moduleNameParts.Add(moduleNamePart);
                }

                // Create a Node<IReadOnlyList<string>> for the module name.
                var moduleNameNode =
                    new Node<ModuleName>(
                        MakeRange(firstModuleNamePart.Start, moduleNameParts.Last().End),
                        [.. moduleNameParts.Select(t => t.Lexeme)]);

                ConsumeAllTrivia();

                if (!TryUnwrap(ParseExposing(), out var exposingNode, out var exposingErr))
                    return exposingErr;

                // Build the module data and wrap it in a Module.PortModule.
                var moduleData =
                    new SyntaxTypes.DefaultModuleData(
                        ModuleName: moduleNameNode,
                        ExposingTokenLocation: exposingNode.Range.Start,
                        ExposingList: exposingNode);

                var moduleNodeValue =
                    new SyntaxTypes.Module.PortModule(
                        PortTokenLocation: portKeyword.Start,
                        ModuleTokenLocation: moduleKeyword.Start,
                        ModuleData: moduleData);

                var moduleNode =
                    new Node<SyntaxTypes.Module>(
                        MakeRange(portKeyword.Start, exposingNode.Range.End),
                        moduleNodeValue);

                return moduleNode;
            }

            {
                /*
                 * Example syntax:
                 * ----
                 * module CompilationInterface.ElmMake exposing (..)
                 * */

                // Expect the "module" keyword (this could be a token with Type Identifier "module")

                if (!TryUnwrap(ConsumeKeyword("module"), out var keywordToken, out var keywordErr))
                    return keywordErr;

                ConsumeAllTrivia();

                // Parse module name (e.g. CompilationInterface.ElmMake.Generated_ElmMake)
                var moduleNameParts = new List<Token>();

                if (!TryUnwrap(ConsumeAnyIdentifier("module name"), out var firstModuleNamePart, out var firstNameErr))
                    return firstNameErr;

                moduleNameParts.Add(firstModuleNamePart);

                while (Peek.Type is TokenType.Dot)
                {
                    if (!TryUnwrap(Consume(TokenType.Dot), out _, out var dotErr))
                        return dotErr;

                    if (!TryUnwrap(ConsumeAnyIdentifier("module name part"), out var moduleNamePart, out var namePartErr))
                        return namePartErr;

                    moduleNameParts.Add(moduleNamePart);
                }

                // Create a Node<IReadOnlyList<string>> for the module name.
                var moduleNameNode =
                    new Node<ModuleName>(
                        MakeRange(firstModuleNamePart.Start, moduleNameParts.Last().End),
                        [.. moduleNameParts.Select(t => t.Lexeme)]);

                ConsumeAllTrivia();

                if (!TryUnwrap(ParseExposing(), out var exposingNode, out var exposingErr))
                    return exposingErr;

                // Build the module data and wrap it in a Module.NormalModule.
                var moduleData =
                    new SyntaxTypes.DefaultModuleData(
                        ModuleName: moduleNameNode,
                        ExposingTokenLocation: exposingNode.Range.Start,
                        ExposingList: exposingNode);

                var moduleNodeValue =
                    new SyntaxTypes.Module.NormalModule(
                        ModuleTokenLocation: keywordToken.Start,
                        ModuleData: moduleData);

                var moduleNode =
                    new Node<SyntaxTypes.Module>(
                        MakeRange(keywordToken.Start, exposingNode.Range.End),
                        moduleNodeValue);

                return moduleNode;
            }
        }

        private ParseResult<Node<SyntaxTypes.Import>> ParseImport()
        {
            /*
             * Examples of covered syntax:
             * 
            import Bytes.Encode
            import CompilerGenerated.Base64 as Base64
            import WorkspaceState_2021_01
            import Json.Encode exposing (Value(..))
             * */

            ConsumeAllTrivia();

            if (!TryUnwrap(ConsumeKeyword("import"), out var importKeyword, out var importKeywordErr))
                return importKeywordErr;

            ConsumeAllTrivia();
            // Parse module name (e.g. CompilationInterface.ElmMake.Generated_ElmMake)

            // The module name must be indented relative to the start of the line, i.e. it cannot
            // start a new top-level construct. If the next token is at column 1 (or we reached the
            // end of the file), the import statement is unfinished.
            if (IsAtEnd() || Peek.Start.Column is 1)
            {
                return
                    new ElmSyntaxParseError(
                        new Location(importKeyword.End.Row, importKeyword.End.Column),
                        "Unfinished import");
            }

            if (!TryUnwrap(ConsumeAnyIdentifier("module name"), out var firstModuleNamePart, out var firstNameErr))
                return firstNameErr;

            var moduleNameParts = new List<Token>([firstModuleNamePart]);

            while (Peek.Type is TokenType.Dot)
            {
                if (!TryUnwrap(Consume(TokenType.Dot), out _, out var dotErr))
                    return dotErr;

                if (!TryUnwrap(ConsumeAnyIdentifier("module name part"), out var moduleNamePart, out var namePartErr))
                    return namePartErr;

                moduleNameParts.Add(moduleNamePart);
            }

            var moduleNameNode =
                new Node<ModuleName>(
                    MakeRange(firstModuleNamePart.Start, moduleNameParts.Last().End),
                    [.. moduleNameParts.Select(t => t.Lexeme)]);

            ConsumeAllTrivia();

            // Parse the optional alias (e.g. "as Base64")

            (Location AsTokenLocation, Node<ModuleName> Alias)? moduleAlias = null;

            if (Peek.Type is TokenType.Identifier && Peek.Lexeme is "as")
            {
                if (!TryUnwrap(ConsumeKeyword("as"), out var asKeyword, out var asErr))
                    return asErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ConsumeAnyIdentifier("module alias"), out var aliasToken, out var aliasErr))
                    return aliasErr;

                ConsumeAllTrivia();

                var aliasNode =
                    new Node<ModuleName>(
                        aliasToken.Range,
                        new[] { aliasToken.Lexeme }.AsReadOnly());

                moduleAlias = (asKeyword.Start, aliasNode);
            }

            (Location ExposingTokenLocation, Node<SyntaxTypes.Exposing> ExposingList)? exposingList = null;

            // Parse the optional exposing clause (e.g. "exposing (Value(..))")
            if (Peek.Type is TokenType.Identifier && Peek.Lexeme is "exposing")
            {
                if (!TryUnwrap(ParseExposing(), out var exposingNode, out var exposingErr))
                    return exposingErr;

                exposingList = (exposingNode.Range.Start, exposingNode);

                ConsumeAllTrivia();
            }

            var importRangeEnd =
                exposingList is null
                ?
                moduleAlias is null
                ?
                moduleNameNode.Range.End
                :
                moduleAlias.Value.Alias.Range.End
                :
                exposingList.Value.ExposingList.Range.End;

            var importNode =
                new Node<SyntaxTypes.Import>(
                    MakeRange(importKeyword.Start, importRangeEnd),
                    new SyntaxTypes.Import(
                        ImportTokenLocation: importKeyword.Start,
                        ModuleName: moduleNameNode,
                        ModuleAlias: moduleAlias,
                        ExposingList: exposingList));

            return importNode;
        }

        private ParseResult<Node<SyntaxTypes.Exposing>> ParseExposing()
        {
            if (!TryUnwrap(ConsumeKeyword("exposing"), out var keyword, out var keywordErr))
                return keywordErr;

            ConsumeAllTrivia();

            if (!TryUnwrap(Consume(TokenType.OpenParen), out var openParen, out var openParenErr))
                return openParenErr;

            ConsumeAllTrivia();

            if (IsAtEnd())
            {
                return
                    ErrorAtCurrentLocation(
                        "Unexpected end of file in exposing list");
            }

            if (Peek.Type is TokenType.DotDot)
            {
                if (!TryUnwrap(Consume(TokenType.DotDot), out var dotDotToken, out var dotDotErr))
                    return dotDotErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(Consume(TokenType.CloseParen), out var closeParen, out var closeParenErr))
                    return closeParenErr;

                return
                    new Node<SyntaxTypes.Exposing>(
                        MakeRange(keyword.Start, closeParen.End),
                        new SyntaxTypes.Exposing.All(
                            MakeRange(
                                dotDotToken.Start,
                                new Location(Row: dotDotToken.End.Row, Column: closeParen.End.Column - 1))));
            }

            {
                Node<SyntaxTypes.TopLevelExpose>? firstNode = null;
                var restNodes = new List<(Location SeparatorLocation, Node<SyntaxTypes.TopLevelExpose> Node)>();

                while (!IsAtEnd() && Peek.Type is not TokenType.CloseParen)
                {
                    if (firstNode is not null)
                    {
                        if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                            return commaErr;

                        ConsumeAllTrivia();

                        if (IsAtEnd())
                        {
                            return
                                ErrorAtCurrentLocation(
                                    "Unexpected end of file in exposing list");
                        }

                        if (!TryUnwrap(ParseTopLevelExpose(), out var topLevelExposeNode, out var exposeErr))
                            return exposeErr;

                        restNodes.Add((commaToken.Start, topLevelExposeNode));

                        ConsumeAllTrivia();
                    }
                    else
                    {
                        if (!TryUnwrap(ParseTopLevelExpose(), out var firstExposeNode, out var exposeErr))
                            return exposeErr;

                        firstNode = firstExposeNode;

                        ConsumeAllTrivia();
                    }
                }

                if (IsAtEnd())
                {
                    return
                        ErrorAtCurrentLocation(
                            "Unexpected end of file: expected ')' to close exposing list");
                }

                if (!TryUnwrap(Consume(TokenType.CloseParen), out var closeParen, out var closeParenErr))
                    return closeParenErr;

                SeparatedSyntaxList<Node<SyntaxTypes.TopLevelExpose>> nodesList =
                    firstNode is null
                    ?
                    new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.TopLevelExpose>>.Empty()
                    :
                    new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.TopLevelExpose>>.NonEmpty(
                        First: firstNode,
                        Rest: restNodes);

                return
                    new Node<SyntaxTypes.Exposing>(
                        MakeRange(keyword.Start, closeParen.End),
                        new SyntaxTypes.Exposing.Explicit(
                            OpenParenLocation: openParen.Start,
                            Nodes: nodesList,
                            CloseParenLocation: closeParen.Start));
            }
        }

        private ParseResult<Node<SyntaxTypes.TopLevelExpose>> ParseTopLevelExpose()
        {
            if (Peek.Type is TokenType.OpenParen)
            {
                if (!TryUnwrap(Consume(TokenType.OpenParen), out var topLevelOpenParen, out var openParenErr))
                    return openParenErr;

                if (!TryUnwrap(Consume(TokenType.Operator), out var operatorToken, out var operatorErr))
                    return operatorErr;

                if (!TryUnwrap(Consume(TokenType.CloseParen), out var topLevelClosingParen, out var closeParenErr))
                    return closeParenErr;

                return
                    new Node<SyntaxTypes.TopLevelExpose>(
                        MakeRange(topLevelOpenParen.Start, topLevelClosingParen.End),
                        new SyntaxTypes.TopLevelExpose.InfixExpose(operatorToken.Lexeme));
            }

            if (Peek.Type is TokenType.Identifier)
            {
                if (!TryUnwrap(Consume(TokenType.Identifier), out var topLevelIdentifier, out var identifierErr))
                    return identifierErr;

                ConsumeAllTrivia();

                if (char.IsLower(topLevelIdentifier.Lexeme[0]))
                {
                    return
                        new Node<SyntaxTypes.TopLevelExpose>(
                            MakeRange(topLevelIdentifier.Start, topLevelIdentifier.End),
                            new SyntaxTypes.TopLevelExpose.FunctionExpose(topLevelIdentifier.Lexeme));
                }

                if (Peek.Type is TokenType.OpenParen)
                {
                    if (!TryUnwrap(Consume(TokenType.OpenParen), out var openParen, out var openParenErr))
                        return openParenErr;

                    ConsumeAllTrivia();

                    var open = false;

                    if (Peek.Type is TokenType.DotDot)
                    {
                        if (!TryUnwrap(Consume(TokenType.DotDot), out _, out var dotDotErr))
                            return dotDotErr;

                        open = true;
                    }

                    ConsumeAllTrivia();

                    if (!TryUnwrap(Consume(TokenType.CloseParen), out var closeParen, out var closeParenErr))
                        return closeParenErr;

                    var openRange =
                        open
                        ?
                        MakeRange(openParen.Start, closeParen.End)
                        :
                        null;

                    return
                        new Node<SyntaxTypes.TopLevelExpose>(
                            MakeRange(topLevelIdentifier.Start, closeParen.End),
                            new SyntaxTypes.TopLevelExpose.TypeExpose(
                                new SyntaxTypes.ExposedType(
                                    topLevelIdentifier.Lexeme,
                                    Open: openRange)));
                }

                return
                    new Node<SyntaxTypes.TopLevelExpose>(
                        topLevelIdentifier.Range,
                        new SyntaxTypes.TopLevelExpose.TypeOrAliasExpose(
                            topLevelIdentifier.Lexeme));
            }

            return
                ErrorAtCurrentLocation(
                    "Unexpected token in exposing list: " + Peek.Type);
        }

        /// <summary>
        /// Parse next declaration (infix, type, function, etc.)
        /// </summary>
        // Entry point: parse a standalone expression and return it.
        public Result<string, SyntaxTypes.Expression> ParseExpressionTopLevel()
        {
            ConsumeAllTrivia();

            if (IsAtEnd())
            {
                return "No tokens to parse as an expression.";
            }

            if (!TryUnwrap(ParseExpression(indentMin: 0), out var expression, out var expressionErr))
            {
                return "Failed to parse expression: " + ElmSyntaxParseError.RenderDisplayString(expressionErr);
            }

            ConsumeAllTrivia();

            if (!IsAtEnd())
            {
                return
                    "Unexpected token '" + Peek.Lexeme +
                    "' after parsing expression.";
            }

            return expression.Value;
        }

        // Entry point: parse either a top-level declaration or a standalone expression.
        public Result<string, DeclarationOrExpression> ParseDeclarationOrExpressionTopLevel()
        {
            ConsumeAllTrivia();

            if (IsAtEnd())
            {
                return "No tokens to parse as a declaration or expression.";
            }

            // The 'type', 'port' and 'infix' keywords unambiguously start a declaration.
            var startsWithDeclarationKeyword =
                Peek.Type is TokenType.Identifier &&
                Peek.Lexeme is "type" or "port" or "infix";

            if (startsWithDeclarationKeyword)
            {
                if (!TryUnwrap(ParseDeclaration(docComment: null), out var declaration, out var declarationErr))
                {
                    return
                        "Failed to parse declaration or expression: " +
                        ElmSyntaxParseError.RenderDisplayString(declarationErr);
                }

                ConsumeAllTrivia();

                if (!IsAtEnd())
                {
                    return
                        "Unexpected token '" + Peek.Lexeme +
                        "' after parsing declaration.";
                }

                return new DeclarationOrExpression.DeclarationSyntax(declaration.Value);
            }

            // Otherwise the input is either a function/value declaration ('name args = body',
            // optionally preceded by a 'name : Type' signature) or an expression. A complete
            // function declaration always contains a top-level '=' which an expression can never
            // contain, so attempting the declaration parse first is unambiguous: it only succeeds
            // and consumes all tokens for an actual declaration. On failure (or leftover tokens)
            // we rewind and parse the input as an expression instead.
            var startPosition = _current;

            if (TryUnwrap(
                ParseDeclaration(docComment: null),
                out var declarationAttempt,
                out _))
            {
                ConsumeAllTrivia();

                if (IsAtEnd())
                {
                    return new DeclarationOrExpression.DeclarationSyntax(declarationAttempt.Value);
                }

                // Parsed a declaration but tokens remain: treat the input as an expression.
                _current = startPosition;
            }
            else
            {
                _current = startPosition;
            }

            ConsumeAllTrivia();

            if (!TryUnwrap(ParseExpression(indentMin: 0), out var expression, out var expressionErr))
            {
                return
                    "Failed to parse declaration or expression: " +
                    ElmSyntaxParseError.RenderDisplayString(expressionErr);
            }

            ConsumeAllTrivia();

            if (!IsAtEnd())
            {
                return
                    "Unexpected token '" + Peek.Lexeme +
                    "' after parsing expression.";
            }

            return new DeclarationOrExpression.ExpressionSyntax(expression.Value);
        }

        private ParseResult<Node<SyntaxTypes.Declaration>> ParseDeclaration(
            Token? docComment)
        {
            if (Peek.Lexeme is "infix")
            {
                if (!TryUnwrap(ConsumeKeyword("infix"), out var infixKeywordToken, out var infixKeywordErr))
                    return infixKeywordErr;

                /*
                infix right 0 (<|) = apL
                infix left  0 (|>) = apR
                infix right 2 (||) = or
                 * */

                ConsumeAllTrivia();

                if (!TryUnwrap(ConsumeAnyIdentifier("infix direction"), out var infixDirectionToken, out var infixDirectionTokenErr))
                    return infixDirectionTokenErr;

                InfixDirection infixDirection;

                switch (infixDirectionToken.Lexeme)
                {
                    case "left":
                        infixDirection = InfixDirection.Left;
                        break;

                    case "right":
                        infixDirection = InfixDirection.Right;
                        break;

                    case "non":
                        infixDirection = InfixDirection.Non;
                        break;

                    default:
                        return
                            ErrorAtCurrentLocation(
                                "Infix direction is not a valid value: " +
                                infixDirectionToken.Lexeme);
                }

                ConsumeAllTrivia();

                if (!TryUnwrap(Consume(TokenType.NumberLiteral), out var precedenceToken, out var precedenceTokenErr))
                    return precedenceTokenErr;

                if (!int.TryParse(precedenceToken.Lexeme, out var precedence))
                {
                    return
                        ErrorAtCurrentLocation(
                            "Infix precedence is not a number: " + precedenceToken.Lexeme);
                }

                ConsumeAllTrivia();

                if (!TryUnwrap(Consume(TokenType.OpenParen), out var operatorOpenParen, out var operatorOpenParenErr))
                    return operatorOpenParenErr;

                if (!TryUnwrap(Consume(TokenType.Operator), out var operatorToken, out var operatorTokenErr))
                    return operatorTokenErr;

                if (!TryUnwrap(Consume(TokenType.CloseParen), out var operatorCloseParen, out var operatorCloseParenErr))
                    return operatorCloseParenErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(Consume(TokenType.Equal), out var equalToken, out var equalTokenErr))
                    return equalTokenErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ConsumeAnyIdentifier("function name"), out var functionNameToken, out var functionNameTokenErr))
                    return functionNameTokenErr;

                ConsumeAllTrivia();

                return
                    new Node<SyntaxTypes.Declaration>(
                        MakeRange(infixKeywordToken.Start, functionNameToken.End),
                        new SyntaxTypes.Declaration.InfixDeclaration(
                            new SyntaxTypes.Infix(
                                InfixTokenLocation: infixKeywordToken.Start,
                                Direction:
                                new Node<InfixDirection>(
                                    infixDirectionToken.Range,
                                    infixDirection),
                                Precedence:
                                new Node<int>(precedenceToken.Range, precedence),
                                Operator:
                                new Node<string>(
                                    MakeRange(operatorOpenParen.Start, operatorCloseParen.End),
                                    operatorToken.Lexeme),
                                EqualsTokenLocation: equalToken.Start,
                                FunctionName:
                                new Node<string>(functionNameToken.Range, functionNameToken.Lexeme))));
            }

            if (Peek.Lexeme is "type")
            {
                return ParseTypeDeclaration(docComment);
            }

            if (Peek.Lexeme is "port")
            {
                return ParsePortDeclaration();
            }

            if (!TryUnwrap(
                ParseFunctionDeclaration(docComment),
                out var functionDeclaration,
                out var functionDeclarationError))
            {
                return functionDeclarationError;
            }

            return functionDeclaration.Cast<SyntaxTypes.Declaration>();
        }

        private ParseResult<Node<SyntaxTypes.Declaration>> ParsePortDeclaration()
        {
            /*
             * Example syntax:
             * ----
             * port sendMessageToMonacoFrame : Json.Encode.Value -> Cmd msg
             * */

            if (!TryUnwrap(ConsumeKeyword("port"), out var portKeywordToken, out var portKeywordErr))
                return portKeywordErr;

            ConsumeAllTrivia();

            if (!TryUnwrap(ConsumeAnyIdentifier("port name"), out var portNameToken, out var portNameErr))
                return portNameErr;

            ConsumeAllTrivia();

            if (!TryUnwrap(Consume(TokenType.Colon), out var colonToken, out var colonErr))
                return colonErr;

            ConsumeAllTrivia();

            if (!TryUnwrap(ParseTypeAnnotation(indentMin: 0), out var typeAnnotation, out var typeAnnotationErr))
                return typeAnnotationErr;

            ConsumeAllTrivia();

            var signature =
                new SyntaxTypes.Signature(
                    Name: new Node<string>(portNameToken.Range, portNameToken.Lexeme),
                    ColonLocation: colonToken.Start,
                    TypeAnnotation: typeAnnotation);

            return
                new Node<SyntaxTypes.Declaration>(
                    MakeRange(portKeywordToken.Start, typeAnnotation.Range.End),
                    new SyntaxTypes.Declaration.PortDeclaration(
                        PortTokenLocation: portKeywordToken.Start,
                        Signature: signature));
        }

        private ParseResult<Node<SyntaxTypes.Declaration>> ParseTypeDeclaration(Token? docComment)
        {
            if (!TryUnwrap(ConsumeKeyword("type"), out var typeKeywordToken, out var typeKeywordErr))
                return typeKeywordErr;

            /*
             * Parse type declaration, like:
             * 
            type FileTreeNode blobStructure
                = BlobNode blobStructure
                | TreeNode (List ( String, FileTreeNode blobStructure ))
             * */

            ConsumeAllTrivia();

            if (NextTokenMatches(peek => peek.Type is TokenType.Identifier && peek.Lexeme is "alias"))
            {
                // Parse type alias

                if (!TryUnwrap(ConsumeKeyword("alias"), out var aliasKeyword, out var aliasKeywordErr))
                    return aliasKeywordErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ConsumeAnyIdentifier("type alias"), out var typeAliasToken, out var typeAliasTokenErr))
                    return typeAliasTokenErr;

                ConsumeAllTrivia();

                var generics = new List<Node<string>>();

                while (Peek.Type is TokenType.Identifier)
                {
                    if (!TryUnwrap(ConsumeAnyIdentifier("generic type parameter"), out var genericToken, out var genericTokenErr))
                        return genericTokenErr;

                    generics.Add(
                        new Node<string>(
                            genericToken.Range,
                            genericToken.Lexeme));

                    ConsumeAllTrivia();
                }

                if (!TryUnwrap(Consume(TokenType.Equal), out var equalToken, out var equalTokenErr))
                    return equalTokenErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ParseTypeAnnotation(indentMin: 0), out var typeAliasTypeAnnotation, out var typeAliasTypeAnnotationErr))
                    return typeAliasTypeAnnotationErr;

                ConsumeAllTrivia();

                var rangeStart =
                    docComment is null
                    ?
                    typeKeywordToken.Start
                    :
                    docComment.Range.Start;

                return
                    new Node<SyntaxTypes.Declaration>(
                        MakeRange(rangeStart, typeAliasTypeAnnotation.Range.End),
                        new SyntaxTypes.Declaration.AliasDeclaration(
                            new SyntaxTypes.TypeAlias(
                                Documentation:
                                docComment is null
                                ?
                                null
                                :
                                new Node<string>(
                                    docComment.Range,
                                    docComment.Lexeme),
                                TypeTokenLocation: typeKeywordToken.Start,
                                AliasTokenLocation: aliasKeyword.Start,
                                Name:
                                new Node<string>(
                                    MakeRange(typeAliasToken.Start, typeAliasToken.End),
                                    typeAliasToken.Lexeme),
                                Generics: generics,
                                EqualsTokenLocation: equalToken.Start,
                                TypeAnnotation: typeAliasTypeAnnotation)));
            }

            {
                // Parse type name

                if (!TryUnwrap(ConsumeAnyIdentifier("type name"), out var typeNameToken, out var typeNameTokenErr))
                    return typeNameTokenErr;

                ConsumeAllTrivia();

                var typeParameters = new List<Node<string>>();

                // Type parameters are optional

                if (Peek.Type is TokenType.Identifier)
                {
                    if (Peek.Lexeme is "alias")
                    {
                        if (!TryUnwrap(ConsumeKeyword("alias"), out _, out var aliasKeywordErr))
                            return aliasKeywordErr;

                        // A type parameter literally named "alias" immediately after the type name
                        // is not supported by this parser (pre-existing gap, preserved here as a
                        // parse error rather than a construct we silently mis-parse).
                        return ErrorAtCurrentLocation("Type alias not implemented.");
                    }

                    while (Peek.Type is TokenType.Identifier)
                    {
                        if (!TryUnwrap(ConsumeAnyIdentifier("type parameter"), out var typeParameterToken, out var typeParameterErr))
                            return typeParameterErr;

                        typeParameters.Add(
                            new Node<string>(
                                MakeRange(typeParameterToken.Start, typeParameterToken.End),
                                typeParameterToken.Lexeme));

                        ConsumeAllTrivia();
                    }
                }

                if (!TryUnwrap(Consume(TokenType.Equal), out var equalToken, out var equalTokenErr))
                    return equalTokenErr;

                ConsumeAllTrivia();

                var constructors =
                    new List<(Location? PipeTokenLocation, Node<SyntaxTypes.ValueConstructor> Constructor)>();

                Location? pipeLocation = null;

                while (true)
                {
                    ConsumeAllTrivia();

                    if (!TryUnwrap(ConsumeAnyIdentifier("constructor name"), out var constructorNameToken, out var constructorNameErr))
                        return constructorNameErr;

                    ConsumeAllTrivia();

                    var constructorArguments = new List<Node<SyntaxTypes.TypeAnnotation>>();

                    while (NextTokenMatches(
                        peek =>
                        constructorNameToken.Start.Column <= Peek.Start.Column &&
                        CanStartTypeAnnotation(peek)))
                    {
                        if (!TryUnwrap(
                            ParseTypeAnnotationTypedArg(indentMin: constructorNameToken.Start.Column),
                            out var argumentAnnotation,
                            out var argumentAnnotationErr))
                            return argumentAnnotationErr;

                        constructorArguments.Add(argumentAnnotation);

                        ConsumeAllTrivia();
                    }

                    var constructorEnd =
                        constructorArguments.Count is 0
                        ?
                        constructorNameToken.End
                        :
                        constructorArguments.Last().Range.End;

                    constructors.Add(
                        (PipeTokenLocation: pipeLocation,
                        Constructor: new Node<SyntaxTypes.ValueConstructor>(
                            MakeRange(constructorNameToken.Start, constructorEnd),
                            new SyntaxTypes.ValueConstructor(
                                new Node<string>(
                                    MakeRange(constructorNameToken.Start, constructorNameToken.End),
                                    constructorNameToken.Lexeme),
                                constructorArguments))));

                    ConsumeAllTrivia();

                    if (NextTokenMatches(peek => peek.Type is TokenType.Pipe))
                    {
                        if (!TryUnwrap(Consume(TokenType.Pipe), out var pipeToken, out var pipeErr))
                            return pipeErr;

                        pipeLocation = pipeToken.Start;
                    }
                    else
                    {
                        break;
                    }
                }

                var rangeStart =
                    docComment is null
                    ?
                    typeKeywordToken.Start
                    :
                    docComment.Range.Start;

                return
                    new Node<SyntaxTypes.Declaration>(
                        MakeRange(rangeStart, constructors.Last().Constructor.Range.End),
                        new SyntaxTypes.Declaration.ChoiceTypeDeclaration(
                            new SyntaxTypes.TypeStruct(
                                Documentation:
                                docComment is null
                                ?
                                null
                                :
                                new Node<string>(
                                    MakeRange(docComment.Start, docComment.End),
                                    docComment.Lexeme),
                                TypeTokenLocation: typeKeywordToken.Start,
                                Name:
                                new Node<string>(
                                    MakeRange(typeNameToken.Start, typeNameToken.End),
                                    typeNameToken.Lexeme),
                                Generics: typeParameters,
                                EqualsTokenLocation: equalToken.Start,
                                Constructors: constructors)));
            }
        }


        private ParseResult<Node<SyntaxTypes.Declaration.FunctionDeclaration>> ParseFunctionDeclaration(
            Token? docComment)
        {
            if (!TryUnwrap(ConsumeAnyIdentifier("function first identifier"), out var functionFirstNameToken, out var functionFirstNameErr))
                return functionFirstNameErr;

            var functionLastNameToken = functionFirstNameToken;

            ConsumeAllTrivia();

            Node<SyntaxTypes.Signature>? signature = null;
            Token? colonToken = null;

            if (Peek.Type is TokenType.Colon)
            {
                // Parse the optional signature (e.g. "toLower : String -> String")

                if (!TryUnwrap(Consume(TokenType.Colon), out var colonTokenValue, out var colonErr))
                    return colonErr;

                colonToken = colonTokenValue;

                ConsumeAllTrivia();

                if (!TryUnwrap(
                    ParseTypeAnnotation(indentMin: functionFirstNameToken.Start.Column),
                    out var signatureTypeAnnotation,
                    out var signatureTypeAnnotationErr))
                    return signatureTypeAnnotationErr;

                signature =
                    new Node<SyntaxTypes.Signature>(
                        MakeRange(functionFirstNameToken.Start, signatureTypeAnnotation.Range.End),
                        new SyntaxTypes.Signature(
                            Name:
                            new Node<string>(
                                MakeRange(functionFirstNameToken.Start, functionFirstNameToken.End),
                                functionFirstNameToken.Lexeme),
                            ColonLocation: colonToken.Start,
                            TypeAnnotation: signatureTypeAnnotation));

                ConsumeAllTrivia();

                if (!TryUnwrap(ConsumeAnyIdentifier("function name"), out var declNameAgain, out var declNameAgainErr))
                    return declNameAgainErr;

                if (declNameAgain.Lexeme != functionFirstNameToken.Lexeme)
                {
                    return
                        ErrorAtCurrentLocation(
                            "Function name does not match signature: " +
                            declNameAgain.Lexeme + " != " + functionFirstNameToken.Lexeme);
                }

                functionLastNameToken = declNameAgain;

                ConsumeAllTrivia();
            }

            var arguments = new List<Node<SyntaxTypes.Pattern>>();

            while (NextTokenMatches(CanStartArgumentPattern))
            {
                if (!TryUnwrap(
                    ParsePatternLessUncons(indentMin: functionFirstNameToken.Start.Column),
                    out var argument,
                    out var argumentErr))
                    return argumentErr;

                ConsumeAllTrivia();

                arguments.Add(argument);
            }

            ConsumeAllTrivia();

            if (!TryUnwrap(Consume(TokenType.Equal), out var equalToken, out var equalTokenErr))
                return equalTokenErr;

            ConsumeAllTrivia();

            var expressionStartPosition = _current;

            var expressionResult = ParseExpression(indentMin: functionFirstNameToken.Start.Column + 1);

            if (!expressionResult.IsOk && IsAtEnd())
            {
                // Running out of tokens while parsing the body is reported as "Unfinished
                // definition" (or "Unfinished list" when an open bracket was never closed),
                // regardless of the specific error that the incomplete expression produced.
                var unclosedListEndLocation =
                    FindUnclosedListEndLocationSince(expressionStartPosition);

                var errorMessage =
                    unclosedListEndLocation.HasValue ? "Unfinished list" : "Unfinished definition";

                return
                    new ElmSyntaxParseError(
                        new Location(
                            unclosedListEndLocation?.Row ?? equalToken.End.Row,
                            unclosedListEndLocation?.Column ?? equalToken.End.Column),
                        errorMessage);
            }

            if (!TryUnwrap(expressionResult, out var expression, out var expressionErr))
                return expressionErr;

            var functionImpl =
                new SyntaxTypes.FunctionImplementation(
                    Name: new Node<string>(functionLastNameToken.Range, functionFirstNameToken.Lexeme),
                    Arguments: arguments,
                    EqualsTokenLocation: equalToken.Start,
                    Expression: expression);

            var functionStruct =
                new SyntaxTypes.FunctionStruct(
                    Documentation:
                    docComment is null
                    ?
                    null
                    :
                    new Node<string>(
                        docComment.Range,
                        docComment.Lexeme),
                    Signature:
                    signature,
                    Declaration:
                    new Node<SyntaxTypes.FunctionImplementation>(
                        MakeRange(functionLastNameToken.Start, expression.Range.End),
                        functionImpl));

            var declaration =
                new SyntaxTypes.Declaration.FunctionDeclaration(functionStruct);

            var rangeStart =
                docComment is null
                ?
                functionFirstNameToken.Start
                :
                docComment.Start;

            return
                new Node<SyntaxTypes.Declaration.FunctionDeclaration>(
                    MakeRange(rangeStart, expression.Range.End),
                    declaration);
        }

        private Location? FindUnclosedListEndLocationSince(int startIndex)
        {
            var listDepth = 0;
            Location? unclosedListEndLocation = null;

            for (var i = startIndex; i < tokens.Length; i++)
            {
                var token = tokens.Span[i];

                if (token.Type is TokenType.OpenBracket)
                {
                    listDepth++;
                    unclosedListEndLocation = token.End;
                    continue;
                }

                if (token.Type is TokenType.CloseBracket && 0 < listDepth)
                {
                    listDepth--;

                    if (listDepth is 0)
                    {
                        unclosedListEndLocation = null;
                    }

                    continue;
                }

                if (0 < listDepth &&
                    token.Type is not TokenType.Whitespace &&
                    token.Type is not TokenType.Newline &&
                    token.Type is not TokenType.Comment)
                {
                    unclosedListEndLocation = token.End;
                }
            }

            return unclosedListEndLocation;
        }

        private ParseResult<Node<SyntaxTypes.TypeAnnotation>> ParseTypeAnnotation(
            int indentMin)
        {
            if (!TryUnwrap(ParseTypeAnnotationFunctionParam(indentMin: indentMin), out var paramType, out var paramTypeErr))
                return paramTypeErr;

            ConsumeAllTrivia();

            if (NextTokenMatches(peek => peek.Type is TokenType.Arrow))
            {
                if (!TryUnwrap(Consume(TokenType.Arrow), out var arrowToken, out var arrowErr))
                    return arrowErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ParseTypeAnnotation(paramType.Range.Start.Column), out var returnType, out var returnTypeErr))
                    return returnTypeErr;

                var range =
                    MakeRange(
                        paramType.Range.Start,
                        returnType.Range.End);

                return
                    new Node<SyntaxTypes.TypeAnnotation>(
                        range,
                        new SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation(
                            ArgumentType: paramType,
                            ArrowLocation: arrowToken.Start,
                            ReturnType: returnType));
            }

            return paramType;
        }

        private ParseResult<Node<SyntaxTypes.TypeAnnotation>> ParseTypeAnnotationFunctionParam(
            int indentMin)
        {
            if (!TryUnwrap(ParseTypeAnnotationTypedArg(indentMin), out var lessApplication, out var lessApplicationErr))
                return lessApplicationErr;

            if (lessApplication.Value is SyntaxTypes.TypeAnnotation.Typed typedLessApp &&
                typedLessApp.TypeArguments.Count is 0)
            {
                ConsumeAllTrivia();

                var typeArguments =
                    new List<Node<SyntaxTypes.TypeAnnotation>>();

                while (NextTokenMatches(
                    peek =>
                    lessApplication.Range.Start.Column < peek.Start.Column &&
                    indentMin < peek.Start.Column &&
                    CanStartTypeAnnotation(peek)))
                {
                    if (!TryUnwrap(ParseTypeAnnotationTypedArg(indentMin: indentMin), out var typeArgument, out var typeArgumentErr))
                        return typeArgumentErr;

                    typeArguments.Add(typeArgument);

                    ConsumeAllTrivia();
                }

                ConsumeAllTrivia();

                var range =
                    typeArguments.Count is 0
                    ?
                    lessApplication.Range
                    :
                    lessApplication.Range with
                    {
                        End = typeArguments.Last().Range.End
                    };

                return
                    new Node<SyntaxTypes.TypeAnnotation>(
                        range,
                        new SyntaxTypes.TypeAnnotation.Typed(
                            TypeName: typedLessApp.TypeName,
                            typeArguments));
            }

            return lessApplication;
        }

        private ParseResult<Node<SyntaxTypes.TypeAnnotation>> ParseTypeAnnotationTypedArg(
            int indentMin)
        {
            var start = Peek;

            if (start.Type is TokenType.OpenParen)
            {
                // Is either Tupled or Typed

                if (!TryUnwrap(Consume(TokenType.OpenParen), out var openToken, out var openTokenErr))
                    return openTokenErr;

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.CloseParen))
                {
                    if (!TryUnwrap(Consume(TokenType.CloseParen), out var closeToken, out var closeTokenErr))
                        return closeTokenErr;

                    return
                        new Node<SyntaxTypes.TypeAnnotation>(
                            MakeRange(openToken.Start, closeToken.End),
                            new SyntaxTypes.TypeAnnotation.Unit());
                }

                if (!TryUnwrap(ParseTypeAnnotation(indentMin: indentMin), out var firstTypeAnnotation, out var firstTypeAnnotationErr))
                    return firstTypeAnnotationErr;

                ConsumeAllTrivia();

                if (Peek.Type is TokenType.Comma)
                {
                    // | Tupled (List (Node TypeAnnotation))

                    if (!TryUnwrap(Consume(TokenType.Comma), out var firstCommaToken, out var firstCommaErr))
                        return firstCommaErr;

                    ConsumeAllTrivia();

                    var restItems = new List<(Location SeparatorLocation, Node<SyntaxTypes.TypeAnnotation> Node)>();

                    while (true)
                    {
                        if (!TryUnwrap(ParseTypeAnnotation(indentMin: indentMin), out var typeAnnotation, out var typeAnnotationErr))
                            return typeAnnotationErr;

                        restItems.Add((firstCommaToken.Start, typeAnnotation));

                        ConsumeAllTrivia();

                        if (Peek.Type is TokenType.Comma)
                        {
                            if (!TryUnwrap(Consume(TokenType.Comma), out firstCommaToken, out var nextCommaErr))
                                return nextCommaErr;

                            ConsumeAllTrivia();
                        }
                        else
                        {
                            break;
                        }
                    }

                    if (!TryUnwrap(Consume(TokenType.CloseParen), out var closingToken, out var closingTokenErr))
                        return closingTokenErr;

                    var range =
                        MakeRange(openToken.Start, closingToken.End);

                    return
                        new Node<SyntaxTypes.TypeAnnotation>(
                            range,
                            new SyntaxTypes.TypeAnnotation.Tupled(
                                TypeAnnotations: new SeparatedSyntaxList<Node<SyntaxTypes.TypeAnnotation>>.NonEmpty(
                                    First: firstTypeAnnotation,
                                    Rest: restItems)));
                }

                {
                    if (!TryUnwrap(Consume(TokenType.CloseParen), out var closingToken, out var closingTokenErr))
                        return closingTokenErr;

                    var range = MakeRange(openToken.Start, closingToken.End);

                    return
                        new Node<SyntaxTypes.TypeAnnotation>(
                            range,
                            new SyntaxTypes.TypeAnnotation.Tupled(
                                TypeAnnotations: new SeparatedSyntaxList<Node<SyntaxTypes.TypeAnnotation>>.NonEmpty(
                                    First: firstTypeAnnotation,
                                    Rest: [])));
                }
            }

            if (start.Type is TokenType.OpenBrace)
            {
                if (!TryUnwrap(Consume(TokenType.OpenBrace), out var openToken, out var openTokenErr))
                    return openTokenErr;

                ConsumeAllTrivia();

                Node<SyntaxTypes.RecordField>? firstField = null;
                var restFields = new List<(Location SeparatorLocation, Node<SyntaxTypes.RecordField> Node)>();

                Token? genericName = null;
                Location? pipeTokenLocation = null;

                // Check if this is a generic record (e.g., { a | field : Type })
                if (NextTokenMatches(peek => peek.Type is TokenType.Identifier))
                {
                    if (!TryUnwrap(
                        ConsumeAnyIdentifier("record field name or generic type parameter"),
                        out var firstIdentifier,
                        out var firstIdentifierErr))
                        return firstIdentifierErr;

                    ConsumeAllTrivia();

                    if (NextTokenMatches(peek => peek.Type is TokenType.Pipe))
                    {
                        // Generic record
                        genericName = firstIdentifier;

                        if (!TryUnwrap(Consume(TokenType.Pipe), out var pipeToken, out var pipeErr))
                            return pipeErr;

                        pipeTokenLocation = pipeToken.Start;
                        ConsumeAllTrivia();
                    }
                    else if (NextTokenMatches(peek => peek.Type is TokenType.Colon))
                    {
                        // Regular record - first field
                        if (!TryUnwrap(Consume(TokenType.Colon), out var colonToken, out var colonErr))
                            return colonErr;

                        ConsumeAllTrivia();

                        if (!TryUnwrap(
                            ParseTypeAnnotation(indentMin: firstIdentifier.Start.Column),
                            out var fieldTypeAnnotation,
                            out var fieldTypeAnnotationErr))
                            return fieldTypeAnnotationErr;

                        ConsumeAllTrivia();

                        var fieldRangeEnd = fieldTypeAnnotation.Range.End;

                        firstField =
                            new Node<SyntaxTypes.RecordField>(
                                MakeRange(firstIdentifier.Start, fieldRangeEnd),
                                new SyntaxTypes.RecordField(
                                    FieldName: new Node<string>(
                                        firstIdentifier.Range,
                                        firstIdentifier.Lexeme),
                                    ColonLocation: colonToken.Start,
                                    FieldType: fieldTypeAnnotation));

                        // Parse remaining fields
                        while (Peek.Type is TokenType.Comma)
                        {
                            if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                                return commaErr;

                            ConsumeAllTrivia();

                            if (!TryUnwrap(ConsumeAnyIdentifier("record field name"), out var nextFieldNameToken, out var nextFieldNameErr))
                                return nextFieldNameErr;

                            ConsumeAllTrivia();

                            if (!TryUnwrap(Consume(TokenType.Colon), out var nextColonToken, out var nextColonErr))
                                return nextColonErr;

                            ConsumeAllTrivia();

                            if (!TryUnwrap(
                                ParseTypeAnnotation(indentMin: nextFieldNameToken.Start.Column),
                                out var nextFieldTypeAnnotation,
                                out var nextFieldTypeAnnotationErr))
                                return nextFieldTypeAnnotationErr;

                            ConsumeAllTrivia();

                            // Use the type annotation's range end - field range should end at type, not include trailing trivia
                            var nextFieldRangeEnd = nextFieldTypeAnnotation.Range.End;

                            var nextField =
                                new Node<SyntaxTypes.RecordField>(
                                    MakeRange(nextFieldNameToken.Start, nextFieldRangeEnd),
                                    new SyntaxTypes.RecordField(
                                        FieldName: new Node<string>(
                                            nextFieldNameToken.Range,
                                            nextFieldNameToken.Lexeme),
                                        ColonLocation: nextColonToken.Start,
                                        FieldType: nextFieldTypeAnnotation));

                            restFields.Add((commaToken.Start, nextField));
                        }
                    }
                }

                // For generic record, parse the fields after the pipe
                if (genericName is not null)
                {
                    if (NextTokenMatches(peek => peek.Type is not TokenType.CloseBrace))
                    {
                        if (!TryUnwrap(ConsumeAnyIdentifier("record field name"), out var fieldNameToken, out var fieldNameErr))
                            return fieldNameErr;

                        ConsumeAllTrivia();

                        if (!TryUnwrap(Consume(TokenType.Colon), out var colonToken, out var colonErr))
                            return colonErr;

                        ConsumeAllTrivia();

                        if (!TryUnwrap(
                            ParseTypeAnnotation(indentMin: fieldNameToken.Start.Column),
                            out var fieldTypeAnnotation,
                            out var fieldTypeAnnotationErr))
                            return fieldTypeAnnotationErr;

                        ConsumeAllTrivia();

                        // Use the type annotation's range end - field range should end at type, not include trailing trivia
                        var fieldRangeEnd = fieldTypeAnnotation.Range.End;

                        firstField =
                            new Node<SyntaxTypes.RecordField>(
                                MakeRange(fieldNameToken.Start, fieldRangeEnd),
                                new SyntaxTypes.RecordField(
                                    FieldName: new Node<string>(
                                        fieldNameToken.Range,
                                        fieldNameToken.Lexeme),
                                    ColonLocation: colonToken.Start,
                                    FieldType: fieldTypeAnnotation));

                        // Parse remaining fields
                        while (Peek.Type is TokenType.Comma)
                        {
                            if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                                return commaErr;

                            ConsumeAllTrivia();

                            if (!TryUnwrap(ConsumeAnyIdentifier("record field name"), out var nextFieldNameToken, out var nextFieldNameErr))
                                return nextFieldNameErr;

                            ConsumeAllTrivia();

                            if (!TryUnwrap(Consume(TokenType.Colon), out var nextColonToken, out var nextColonErr))
                                return nextColonErr;

                            ConsumeAllTrivia();

                            if (!TryUnwrap(
                                ParseTypeAnnotation(indentMin: nextFieldNameToken.Start.Column),
                                out var nextFieldTypeAnnotation,
                                out var nextFieldTypeAnnotationErr))
                                return nextFieldTypeAnnotationErr;

                            ConsumeAllTrivia();

                            // Use the type annotation's range end - field range should end at type, not include trailing trivia
                            var nextFieldRangeEnd = nextFieldTypeAnnotation.Range.End;

                            var nextField =
                                new Node<SyntaxTypes.RecordField>(
                                    MakeRange(nextFieldNameToken.Start, nextFieldRangeEnd),
                                    new SyntaxTypes.RecordField(
                                        FieldName: new Node<string>(
                                            nextFieldNameToken.Range,
                                            nextFieldNameToken.Lexeme),
                                        ColonLocation: nextColonToken.Start,
                                        FieldType: nextFieldTypeAnnotation));

                            restFields.Add((commaToken.Start, nextField));
                        }
                    }
                }

                // Capture the end position before consuming the closing brace
                // This includes any trailing whitespace after the last field
                var recordDefinitionEnd = EnumeratePrecedingTokensBackwards().First().End;

                if (!TryUnwrap(Consume(TokenType.CloseBrace), out var closingToken, out var closingTokenErr))
                    return closingTokenErr;

                var range =
                    MakeRange(
                        openToken.Start,
                        closingToken.End);

                SeparatedSyntaxList<Node<SyntaxTypes.RecordField>> fieldsList =
                    firstField is null
                    ?
                    new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.RecordField>>.Empty()
                    :
                    new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.RecordField>>.NonEmpty(
                        First: firstField,
                        Rest: restFields);

                if (genericName is not null && pipeTokenLocation is { } pipeLocation)
                {
                    return
                        new Node<SyntaxTypes.TypeAnnotation>(
                            range,
                            new SyntaxTypes.TypeAnnotation.GenericRecord(
                                GenericName: new Node<string>(
                                    genericName.Range,
                                    genericName.Lexeme),
                                PipeLocation: pipeLocation,
                                RecordDefinition: new Node<SyntaxTypes.RecordDefinition>(
                                    MakeRange(
                                        genericName.Range.End with
                                        {
                                            Column = genericName.Range.End.Column + 2
                                        },
                                        recordDefinitionEnd),
                                    new SyntaxTypes.RecordDefinition(fieldsList))));
                }

                return
                    new Node<SyntaxTypes.TypeAnnotation>(
                        range,
                        new SyntaxTypes.TypeAnnotation.Record(
                            RecordDefinition: new SyntaxTypes.RecordDefinition(fieldsList)));
            }

            if (start.Type is TokenType.Identifier)
            {
                if (!TryUnwrap(ConsumeAnyIdentifier("first identifier"), out var firstIdentifierToken, out var firstIdentifierTokenErr))
                    return firstIdentifierTokenErr;

                if (char.IsLower(firstIdentifierToken.Lexeme.First()))
                {
                    // GenericType String

                    return
                        new Node<SyntaxTypes.TypeAnnotation>(
                            MakeRange(start.Start, start.End),
                            new SyntaxTypes.TypeAnnotation.GenericType(firstIdentifierToken.Lexeme));
                }

                // Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))

                // https://github.com/stil4m/elm-syntax/blob/c99a05ac96d3fa15fb3a8dc5ca39eaf78d1e510a/src/Elm/Parser/TypeAnnotation.elm#L336-L357

                var namespaces = new List<Token>();

                while (!IsAtEnd() && Peek.Type is TokenType.Dot)
                {
                    if (!TryUnwrap(Consume(TokenType.Dot), out _, out var dotErr))
                        return dotErr;

                    if (!TryUnwrap(ConsumeAnyIdentifier("namespace item"), out var namespaceToken, out var namespaceErr))
                        return namespaceErr;

                    namespaces.Add(namespaceToken);
                }

                ConsumeAllTrivia();

                IReadOnlyList<Token> moduleName =
                    [.. namespaces.Prepend(firstIdentifierToken).SkipLast(1)];

                var typeNameToken =
                    namespaces.LastOrDefault() ?? firstIdentifierToken;

                var instantiatedRangeStart =
                    moduleName.Count is 0
                    ?
                    typeNameToken.Start
                    :
                    moduleName[0].Start;

                var range =
                    MakeRange(instantiatedRangeStart, typeNameToken.End);

                return
                    new Node<SyntaxTypes.TypeAnnotation>(
                        range,
                        new SyntaxTypes.TypeAnnotation.Typed(
                            new Node<(ModuleName ModuleName, string Name)>(
                                MakeRange(instantiatedRangeStart, typeNameToken.End),
                                ([.. moduleName.Select(t => t.Lexeme)],
                                typeNameToken.Lexeme)),
                            TypeArguments: []));
            }

            return
                ErrorAtCurrentLocation(
                    "Unsupported type annotation type: " + start.Type +
                    " at " + start.Start.Row + ":" + start.Start.Column +
                    " - " + start.End.Row + ":" + start.End.Column +
                    " - " + start.Lexeme);
        }

        private static bool CanStartTypeAnnotation(Token token)
        {
            return token.Type switch
            {
                TokenType.Identifier or
                TokenType.OpenParen or
                TokenType.OpenBrace =>
                true,

                _ =>
                false,
            };
        }

        private ParseResult<Node<SyntaxTypes.Expression>> ParseExpression(
            int indentMin,
            int minPrecedence = 0)
        {
            // Parse a primary expression first.
            if (!TryUnwrap(ParsePrimaryExpression(indentMin), out var left, out var leftErr))
                return leftErr;

            ConsumeAllTrivia();

            // Loop while the next token is a recognized infix operator with sufficient precedence.
            // An operator-shaped token that isn't one of the known infix operators (InfixOperatorInfo.GetInfo
            // returning null) simply ends the loop instead of failing outright, leaving the token for the
            // surrounding context (e.g. the "unexpected token" checks at the call sites) to report.
            while (true)
            {
                if (IsAtEnd() || Peek.Type is not TokenType.Operator)
                {
                    break;
                }

                if (InfixOperatorInfo.GetInfo(Peek.Lexeme) is not { } opInfo || opInfo.Precedence < minPrecedence)
                {
                    break;
                }

                if (!TryUnwrap(Consume(TokenType.Operator), out var opToken, out var opTokenErr))
                    return opTokenErr;

                // Create a Node for the operator with its source range
                var operatorNode =
                    new Node<string>(
                        MakeRange(opToken.Start, opToken.End),
                        opToken.Lexeme);

                // Determine the next minimum precedence for the right-hand side.
                // For left-associative operators, use (precedence + 1).
                var nextMinPrecedence =
                    opInfo.Direction is InfixDirection.Left
                    ?
                    opInfo.Precedence + 1
                    :
                    opInfo.Precedence;

                ConsumeAllTrivia();

                // Recursively parse the right-hand side with the adjusted precedence.
                if (!TryUnwrap(
                    ParseExpression(
                        indentMin: indentMin,
                        minPrecedence: nextMinPrecedence),
                    out var right,
                    out var rightErr))
                    return rightErr;

                // Combine the left and right expressions into an OperatorApplication node.
                left =
                    new Node<SyntaxTypes.Expression>(
                        MakeRange(left.Range.Start, right.Range.End),
                        new SyntaxTypes.Expression.OperatorApplication(operatorNode, opInfo.Direction, left, right));

                ConsumeAllTrivia();
            }

            return left;
        }

        private ParseResult<Node<SyntaxTypes.Expression>> ParsePrimaryExpression(
            int indentMin)
        {
            if (!TryUnwrap(ParseBasicPrimaryExpression(indentMin), out var functionExpr, out var functionExprErr))
                return functionExprErr;

            ConsumeAllTrivia();

            /*
             * Following expressions are arguments if indented at least as much as the first identifier
             * and not matching any keyword.
             * */

            var argumentsNodes =
                new List<Node<SyntaxTypes.Expression>>();

            while (NextTokenMatches(
                peek =>
                indentMin < peek.Start.Column &&
                CanStartArgumentExpression(peek)))
            {
                if (!TryUnwrap(ParseBasicPrimaryExpression(indentMin), out var argumentExpr, out var argumentExprErr))
                    return argumentExprErr;

                argumentsNodes.Add(argumentExpr);

                ConsumeAllTrivia();
            }

            if (0 < argumentsNodes.Count)
            {
                var applicationRange =
                    MakeRange(
                        functionExpr.Range.Start,
                        argumentsNodes.Last().Range.End);

                var applicationExpr =
                    new SyntaxTypes.Expression.Application(functionExpr, argumentsNodes);

                return
                    new Node<SyntaxTypes.Expression>(
                        applicationRange,
                        applicationExpr);
            }

            return functionExpr;
        }

        private static bool CanStartArgumentExpression(Token token)
        {
            if (IsKeyword(token))
            {
                return false;
            }

            // You want to allow tokens that can start expressions but not tokens that are infix operators,
            // commas, closing parentheses/brackets, etc.
            return token.Type switch
            {
                TokenType.StringLiteral or
                TokenType.TripleQuotedStringLiteral or
                TokenType.NumberLiteral or
                TokenType.CharLiteral or
                TokenType.Identifier or
                TokenType.OpenParen or
                TokenType.OpenBrace or
                TokenType.OpenBracket or
                TokenType.Negation or
                TokenType.Dot =>
                true,

                _ =>
                false,
            };
        }

        private ParseResult<Node<SyntaxTypes.Expression>> ParseBasicPrimaryExpression(
            int indentMin)
        {
            if (!TryUnwrap(
                ParseBasicPrimaryExpressionLessRecordAccess(indentMin: indentMin),
                out var lessRecordAccess,
                out var lessRecordAccessErr))
                return lessRecordAccessErr;

            if (NextTokenMatches(peek => peek.Type is TokenType.Dot))
            {
                // | RecordAccess (Node Expression) (Node String)

                var lastRecordAccess = lessRecordAccess;

                while (NextTokenMatches(peek => peek.Type is TokenType.Dot))
                {
                    if (!TryUnwrap(Consume(TokenType.Dot), out _, out var dotErr))
                        return dotErr;

                    if (!TryUnwrap(ConsumeAnyIdentifier("record field name"), out var recordFieldToken, out var recordFieldErr))
                        return recordFieldErr;

                    var recordAccessRange =
                        MakeRange(
                            lastRecordAccess.Range.Start,
                            recordFieldToken.End);

                    var recordAccessExpr =
                        new SyntaxTypes.Expression.RecordAccess(
                            lastRecordAccess,
                            new Node<string>(
                                MakeRange(recordFieldToken.Start, recordFieldToken.End),
                                recordFieldToken.Lexeme));

                    lastRecordAccess =
                        new Node<SyntaxTypes.Expression>(
                            recordAccessRange,
                            recordAccessExpr);
                }

                return lastRecordAccess;
            }

            return lessRecordAccess;
        }

        private ParseResult<Node<SyntaxTypes.Expression>> ParseBasicPrimaryExpressionLessRecordAccess(
            int indentMin)
        {
            var start = Peek;

            if (start.Type is TokenType.StringLiteral)
            {
                if (!TryUnwrap(Consume(TokenType.StringLiteral), out var stringLiteral, out var stringLiteralErr))
                    return stringLiteralErr;

                var literalExpr =
                    new SyntaxTypes.Expression.Literal(
                        stringLiteral.Lexeme,
                        SourceText: stringLiteral.RawText);

                return new Node<SyntaxTypes.Expression>(stringLiteral.Range, literalExpr);
            }

            if (start.Type is TokenType.TripleQuotedStringLiteral)
            {
                if (!TryUnwrap(Consume(TokenType.TripleQuotedStringLiteral), out var stringLiteral, out var stringLiteralErr))
                    return stringLiteralErr;

                var literalExpr =
                    new SyntaxTypes.Expression.MultilineStringLiteral(
                        stringLiteral.Lexeme,
                        SourceLines:
                        stringLiteral.RawText is { } rawText
                        ?
                        [.. rawText.Split('\n')]
                        :
                        null);

                return new Node<SyntaxTypes.Expression>(stringLiteral.Range, literalExpr);
            }

            if (start.Type is TokenType.CharLiteral)
            {
                if (!TryUnwrap(Consume(TokenType.CharLiteral), out var charToken, out var charTokenErr))
                    return charTokenErr;

                if (!TryConvertToUtf32(charToken.Lexeme, out var charCodePoint))
                {
                    return
                        new ElmSyntaxParseError(
                            charToken.Start,
                            "Invalid character literal: '" + charToken.Lexeme + "'");
                }

                var literalExpr =
                    new SyntaxTypes.Expression.CharLiteral(charCodePoint);

                return new Node<SyntaxTypes.Expression>(charToken.Range, literalExpr);
            }

            if (start.Type is TokenType.OpenBrace)
            {
                return ParseRecordExpr(indentMin);
            }

            if (start.Type is TokenType.Identifier)
            {
                if (!TryUnwrap(ConsumeAnyIdentifier("first identifier"), out var firstIdentifierToken, out var firstIdentifierErr))
                    return firstIdentifierErr;

                // | FunctionOrValue ModuleName String
                // | Application (List (Node Expression))

                if (firstIdentifierToken.Lexeme is "let")
                {
                    /*
                     * Example A:
                     * 
                        let
                            alfa =
                                13

                            beta =
                                71
                        in
                     * --------
                     * 
                     * Example B:
                     * 
                        let
                            ( itemNodeCount, itemByteCount ) =
                                countValueContent item
                        in
                     * ---------
                     * 
                     * Example C:
                     * 
                        let
                            (EvalEnvironment environmentValue) =
                                context
                        in
                     * ---------
                     *
                     * */

                    ConsumeAllTrivia();

                    var letDecls = new List<Node<SyntaxTypes.Expression.LetDeclaration>>();

                    // Use the minimum of indentMin and the let keyword column for checking declaration indentation.
                    // This handles the case where let is inside parentheses and on the same line,
                    // causing declarations to be at a lower column than the let keyword itself.
                    var letDeclIndentCheck = Math.Min(indentMin, firstIdentifierToken.Range.Start.Column);

                    while (NextTokenMatches(
                        peek =>
                        letDeclIndentCheck < peek.Start.Column &&
                        peek.Lexeme is not "in"))
                    {
                        if (!TryUnwrap(ParseLetDeclaration(indentMin: letDeclIndentCheck + 1), out var letDecl, out var letDeclErr))
                            return letDeclErr;

                        letDecls.Add(letDecl);

                        ConsumeAllTrivia();
                    }

                    if (!TryUnwrap(ConsumeKeyword("in"), out var letInToken, out var letInTokenErr))
                        return letInTokenErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ParseExpression(letInToken.Start.Column), out var letInExpr, out var letInExprErr))
                        return letInExprErr;

                    var letBlockRange =
                        MakeRange(
                            firstIdentifierToken.Start,
                            letInExpr.Range.End);

                    var letBlockExpr =
                        new SyntaxTypes.Expression.LetBlock(
                            LetTokenLocation: firstIdentifierToken.Start,
                            Declarations: letDecls,
                            InTokenLocation: letInToken.Start,
                            Expression: letInExpr);

                    return
                        new Node<SyntaxTypes.Expression>(
                            letBlockRange,
                            new SyntaxTypes.Expression.LetExpression(letBlockExpr));
                }

                if (firstIdentifierToken.Lexeme is "if")
                {
                    // For `else if` chains, the inner `if`'s branches inherit the
                    // outer expression's indentation. Without this, a body like
                    //   if outer then a else if inner then Good { ... } else b
                    // would parse `Good`'s record argument with the inner `if`'s
                    // column as indentMin and fail to consume it.
                    var ifBranchIndentMin =
                        Math.Min(indentMin, firstIdentifierToken.Range.Start.Column);

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ParseExpression(indentMin: ifBranchIndentMin), out var condition, out var conditionErr))
                        return conditionErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ConsumeKeyword("then"), out var thenToken, out var thenTokenErr))
                        return thenTokenErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ParseExpression(indentMin: ifBranchIndentMin), out var thenBranch, out var thenBranchErr))
                        return thenBranchErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ConsumeKeyword("else"), out var elseToken, out var elseTokenErr))
                        return elseTokenErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ParseExpression(indentMin: ifBranchIndentMin), out var elseBranch, out var elseBranchErr))
                        return elseBranchErr;

                    var ifBlockRange =
                        MakeRange(
                            firstIdentifierToken.Start,
                            elseBranch.Range.End);

                    var ifBlockExpr =
                        new SyntaxTypes.Expression.IfBlock(
                            IfTokenLocation: firstIdentifierToken.Start,
                            Condition: condition,
                            ThenTokenLocation: thenToken.Start,
                            ThenBlock: thenBranch,
                            ElseTokenLocation: elseToken.Start,
                            ElseBlock: elseBranch);

                    return
                        new Node<SyntaxTypes.Expression>(
                            ifBlockRange,
                            ifBlockExpr);
                }

                if (firstIdentifierToken.Lexeme is "case")
                {
                    // case valueToEncode of

                    ConsumeAllTrivia();

                    if (!TryUnwrap(
                        ParseExpression(indentMin: firstIdentifierToken.Range.Start.Column),
                        out var caseValue,
                        out var caseValueErr))
                        return caseValueErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ConsumeKeyword("of"), out var caseOfToken, out var caseOfTokenErr))
                        return caseOfTokenErr;

                    ConsumeAllTrivia();

                    var casesIndentMin = Peek.Start.Column;

                    var caseBranchesIndentLowerBound =
                        casesIndentMin <= indentMin
                        ?
                        casesIndentMin
                        :
                        indentMin + 1;

                    var caseBranches = new List<SyntaxTypes.Case>();

                    while (!IsAtEnd() &&
                        caseBranchesIndentLowerBound <= Peek.Start.Column &&
                        Peek.Type is not TokenType.Comma &&
                        Peek.Type is not TokenType.CloseParen &&
                        Peek.Type is not TokenType.CloseBracket &&
                        Peek.Type is not TokenType.CloseBrace)
                    {
                        var branchStartIndex = _current;

                        // Attempt to parse one more case branch; on failure (for any reason) rewind
                        // and treat whatever branches were already accumulated as the complete list,
                        // leaving the token that failed to start a branch for the surrounding context.
                        if (!TryUnwrap(
                            ParseCaseBranch(casesIndentMin),
                            out var caseBranch,
                            out _))
                        {
                            _current = branchStartIndex;
                            break;
                        }

                        caseBranches.Add(caseBranch.Value);

                        ConsumeAllTrivia();
                    }

                    if (caseBranches.Count is 0)
                    {
                        return ErrorAtCurrentLocation("Expected at least one case branch after 'of'");
                    }

                    var caseBlockRange =
                        MakeRange(
                            firstIdentifierToken.Start,
                            caseBranches.Last().Expression.Range.End);

                    var caseBlockExpr =
                        new SyntaxTypes.CaseBlock(
                            CaseTokenLocation: firstIdentifierToken.Start,
                            Expression: caseValue,
                            OfTokenLocation: caseOfToken.Start,
                            Cases: caseBranches);

                    return
                        new Node<SyntaxTypes.Expression>(
                            caseBlockRange,
                            new SyntaxTypes.Expression.CaseExpression(caseBlockExpr));
                }

                var identifiers = new List<Token>([firstIdentifierToken]);

                /*
                 * If last identifier does not start with uppercase, means record access starts here.
                 * Example of record access following FunctionOrValue:
                 * 
                 * Alfa.Beta.gamma.delta
                 * */
                while (char.IsUpper(identifiers.Last().Lexeme[0]) &&
                    NextTokenMatches(peek => peek.Type is TokenType.Dot))
                {
                    if (!TryUnwrap(Consume(TokenType.Dot), out _, out var dotErr))
                        return dotErr;

                    if (!TryUnwrap(ConsumeAnyIdentifier("function or value name part"), out var furtherNamePart, out var furtherNamePartErr))
                        return furtherNamePartErr;

                    identifiers.Add(furtherNamePart);
                }

                var firstExpr =
                    new Node<SyntaxTypes.Expression>(
                        MakeRange(
                            firstIdentifierToken.Start,
                            identifiers[^1].End),
                        new SyntaxTypes.Expression.FunctionOrValue(
                            [.. identifiers.SkipLast(1).Select(t => t.Lexeme)],
                            identifiers[^1].Lexeme));

                return firstExpr;
            }

            if (start.Type is TokenType.OpenBracket)
            {
                // | ListExpr (List (Node Expression))

                if (!TryUnwrap(Consume(TokenType.OpenBracket), out var listOpenToken, out var listOpenErr))
                    return listOpenErr;

                ConsumeAllTrivia();

                Node<SyntaxTypes.Expression>? firstElement = null;
                var restElements = new List<(Location SeparatorLocation, Node<SyntaxTypes.Expression> Node)>();

                if (Peek.Type is not TokenType.CloseBracket)
                {
                    // Parse first element
                    if (!TryUnwrap(ParseExpression(indentMin), out var firstElementValue, out var firstElementErr))
                        return firstElementErr;

                    firstElement = firstElementValue;
                    ConsumeAllTrivia();

                    // Parse remaining elements (each preceded by comma)
                    while (Peek.Type is TokenType.Comma)
                    {
                        if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                            return commaErr;

                        ConsumeAllTrivia();

                        if (!TryUnwrap(ParseExpression(indentMin), out var nextElement, out var nextElementErr))
                            return nextElementErr;

                        restElements.Add((commaToken.Start, nextElement));
                        ConsumeAllTrivia();
                    }
                }

                if (!TryUnwrap(Consume(TokenType.CloseBracket), out var listCloseToken, out var listCloseErr))
                    return listCloseErr;

                var listRange =
                    MakeRange(listOpenToken.Start, listCloseToken.End);

                SeparatedSyntaxList<Node<SyntaxTypes.Expression>> elements =
                    firstElement is null
                    ?
                    new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Expression>>.Empty()
                    :
                    new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Expression>>.NonEmpty(
                        First: firstElement,
                        Rest: restElements);

                var listExpr =
                    new SyntaxTypes.Expression.ListExpr(
                        Elements: elements);

                return new Node<SyntaxTypes.Expression>(listRange, listExpr);
            }

            if (start.Type is TokenType.OpenParen)
            {
                /*
                 * Can be either of:
                 * | TupledExpression (List (Node Expression))
                 * | ParenthesizedExpression (Node Expression)
                 * */

                if (!TryUnwrap(Consume(TokenType.OpenParen), out var parenOpenToken, out var parenOpenErr))
                    return parenOpenErr;

                var nextTwoTokens =
                    EnumerateFollowingTokens().Take(2).ToArray();

                if (nextTwoTokens.Length is 2)
                {
                    if (nextTwoTokens[0].Type is TokenType.Operator &&
                        nextTwoTokens[1].Type is TokenType.CloseParen)
                    {
                        if (!TryUnwrap(Consume(TokenType.Operator), out var operatorToken, out var operatorErr))
                            return operatorErr;

                        if (!TryUnwrap(Consume(TokenType.CloseParen), out var parenCloseToken, out var parenCloseErr))
                            return parenCloseErr;

                        return
                            new Node<SyntaxTypes.Expression>(
                                MakeRange(parenOpenToken.Start, parenCloseToken.End),
                                new SyntaxTypes.Expression.PrefixOperator(operatorToken.Lexeme));

                    }
                }

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.CloseParen))
                {
                    if (!TryUnwrap(Consume(TokenType.CloseParen), out var parenCloseToken, out var parenCloseErr))
                        return parenCloseErr;

                    var parenRange =
                        MakeRange(parenOpenToken.Start, parenCloseToken.End);

                    return
                        new Node<SyntaxTypes.Expression>(
                            parenRange,
                            new SyntaxTypes.Expression.UnitExpr());
                }

                if (!TryUnwrap(ParseExpression(indentMin), out var firstItemExpr, out var firstItemExprErr))
                    return firstItemExprErr;

                ConsumeAllTrivia();

                var furtherItems = new List<(Location SeparatorLocation, Node<SyntaxTypes.Expression> Node)>();

                while (Peek.Type is TokenType.Comma)
                {
                    if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                        return commaErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ParseExpression(indentMin), out var furtherItemExpr, out var furtherItemExprErr))
                        return furtherItemExprErr;

                    furtherItems.Add((commaToken.Start, furtherItemExpr));

                    ConsumeAllTrivia();
                }

                {
                    if (!TryUnwrap(Consume(TokenType.CloseParen), out var parenCloseToken, out var parenCloseErr))
                        return parenCloseErr;

                    var parenRange =
                        MakeRange(parenOpenToken.Start, parenCloseToken.End);

                    if (furtherItems.Count is 0)
                    {
                        var parenthesizedExpr =
                            new SyntaxTypes.Expression.ParenthesizedExpression(
                                Expression: firstItemExpr);

                        return new Node<SyntaxTypes.Expression>(parenRange, parenthesizedExpr);
                    }

                    var tupledExpr =
                        new SyntaxTypes.Expression.TupledExpression(
                            Elements: new SeparatedSyntaxList<Node<SyntaxTypes.Expression>>.NonEmpty(
                                First: firstItemExpr,
                                Rest: furtherItems));

                    return new Node<SyntaxTypes.Expression>(parenRange, tupledExpr);
                }
            }

            if (start.Type is TokenType.NumberLiteral)
            {
                if (!TryUnwrap(Consume(TokenType.NumberLiteral), out _, out var numberErr))
                    return numberErr;

                return
                    new Node<SyntaxTypes.Expression>(
                        start.Range,
                        ParseNumber(start.Lexeme));
            }

            if (start.Type is TokenType.GLSLLiteral)
            {
                if (!TryUnwrap(Consume(TokenType.GLSLLiteral), out var glslToken, out var glslErr))
                    return glslErr;

                return
                    new Node<SyntaxTypes.Expression>(
                        glslToken.Range,
                        new SyntaxTypes.Expression.GLSLExpression(glslToken.Lexeme));
            }

            if (start.Type is TokenType.Negation)
            {
                if (!TryUnwrap(Consume(TokenType.Negation), out var negationToken, out var negationErr))
                    return negationErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ParseBasicPrimaryExpression(indentMin), out var negatedExpr, out var negatedExprErr))
                    return negatedExprErr;

                var negationRange =
                    MakeRange(negationToken.Start, negatedExpr.Range.End);

                return
                    new Node<SyntaxTypes.Expression>(
                        negationRange,
                        new SyntaxTypes.Expression.Negation(negatedExpr));
            }

            if (start.Type is TokenType.Lambda)
            {
                if (!TryUnwrap(ParseLambdaExpression(indentMin), out var lambdaNode, out var lambdaErr))
                    return lambdaErr;

                return new Node<SyntaxTypes.Expression>(lambdaNode.Range, lambdaNode.Value);
            }

            if (start.Type is TokenType.Dot)
            {
                // | RecordAccessFunction String

                if (!TryUnwrap(Consume(TokenType.Dot), out var dotToken, out var dotErr))
                    return dotErr;

                if (!TryUnwrap(ConsumeAnyIdentifier("record field name"), out var recordFieldToken, out var recordFieldErr))
                    return recordFieldErr;

                var recordAccessRange =
                    MakeRange(dotToken.Start, recordFieldToken.End);

                var recordAccessExpr =
                    new SyntaxTypes.Expression.RecordAccessFunction(
                        /*
                         * elm-syntax currently includes the dot:
                         * https://github.com/stil4m/elm-syntax/pull/188
                         * */
                        dotToken.Lexeme + recordFieldToken.Lexeme);

                return
                    new Node<SyntaxTypes.Expression>(
                        recordAccessRange,
                        recordAccessExpr);
            }

            return
                ErrorAtCurrentLocation(
                    "Unsupported token type in expression: " + start.Type +
                    " at " + start.Start.Row + ":" + start.Start.Column +
                    " - " + start.End.Row + ":" + start.End.Column +
                    " - " + start.Lexeme);
        }

        private ParseResult<Node<SyntaxTypes.Expression.LambdaExpression>> ParseLambdaExpression(
            int indentMin)
        {
            // | Lambda (Node Expression)

            if (!TryUnwrap(Consume(TokenType.Lambda), out var lambdaToken, out var lambdaTokenErr))
                return lambdaTokenErr;

            ConsumeAllTrivia();

            /*
             * Example:
             * 
             * (\type_arg -> json_encode_Bytes type_arg)
             * */

            var arguments = new List<Node<SyntaxTypes.Pattern>>();

            while (NextTokenMatches(CanStartArgumentPattern))
            {
                if (!TryUnwrap(ParsePatternLessUncons(indentMin: indentMin), out var argument, out var argumentErr))
                    return argumentErr;

                ConsumeAllTrivia();

                arguments.Add(argument);
            }

            ConsumeAllTrivia();

            if (!TryUnwrap(Consume(TokenType.Arrow), out var arrowToken, out var arrowTokenErr))
                return arrowTokenErr;

            ConsumeAllTrivia();

            if (!TryUnwrap(ParseExpression(indentMin), out var expression, out var expressionErr))
                return expressionErr;

            var lambdaRange =
                MakeRange(lambdaToken.Start, expression.Range.End);

            var lambdaExpr =
                new SyntaxTypes.LambdaStruct(
                    BackslashLocation: lambdaToken.Start,
                    Arguments: arguments,
                    ArrowLocation: arrowToken.Start,
                    Expression: expression);

            return
                new Node<SyntaxTypes.Expression.LambdaExpression>(
                    lambdaRange,
                    new SyntaxTypes.Expression.LambdaExpression(lambdaExpr));
        }

        private ParseResult<Node<SyntaxTypes.Expression.LetDeclaration>> ParseLetDeclaration(
            int indentMin)
        {
            /*
             * LetDeclaration can be either LetDestructuring or LetFunction:
             * 
                (fst, snd) = identifier

                function arg = identifier
             */

            if (Peek.Type is TokenType.Identifier)
            {
                // LetFunction

                if (!TryUnwrap(ParseFunctionDeclaration(docComment: null), out var parsedDecl, out var parsedDeclErr))
                    return parsedDeclErr;

                return
                    new Node<SyntaxTypes.Expression.LetDeclaration>(
                        parsedDecl.Range,
                        new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                            parsedDecl.Value.Function));
            }

            {
                // LetDestructuring

                if (!TryUnwrap(ParsePatternLessUncons(indentMin: indentMin), out var pattern, out var patternErr))
                    return patternErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(Consume(TokenType.Equal), out var equalToken, out var equalTokenErr))
                    return equalTokenErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(
                    ParseExpression(indentMin: pattern.Range.Start.Column + 1),
                    out var expression,
                    out var expressionErr))
                    return expressionErr;

                var letDeclRange =
                    MakeRange(pattern.Range.Start, expression.Range.End);

                var letDecl =
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        Pattern: pattern,
                        EqualsTokenLocation: equalToken.Start,
                        Expression: expression);

                return
                    new Node<SyntaxTypes.Expression.LetDeclaration>(
                        letDeclRange,
                        letDecl);
            }
        }

        private static SyntaxTypes.Expression ParseNumber(string expression)
        {
            // Check for hexadecimal format first - hex literals can contain 'e' as a digit
            // and should not be misidentified as floats
            if (expression.StartsWith("0x") || expression.StartsWith("-0x"))
            {
                // Hexadecimal integer - preserve the original literal string
                return new SyntaxTypes.Expression.Integer(expression);
            }

            // Check if the number contains a decimal point or exponent notation
            if (expression.Contains('.') || expression.Contains('e') || expression.Contains('E'))
            {
                // Float number - preserve the original literal string for exact roundtripping
                return new SyntaxTypes.Expression.FloatLiteral(expression);
            }

            // Decimal integer - preserve the original literal string
            return new SyntaxTypes.Expression.Integer(expression);
        }

        private ParseResult<Node<SyntaxTypes.Case>> ParseCaseBranch(int indentMin)
        {
            /*
             * Example:
             * 
                Nothing ->
                    [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object
             */

            if (!TryUnwrap(ParsePattern(indentMin), out var pattern, out var patternErr))
                return patternErr;

            ConsumeAllTrivia();

            if (!TryUnwrap(Consume(TokenType.Arrow), out var arrowToken, out var arrowTokenErr))
                return arrowTokenErr;

            ConsumeAllTrivia();

            // The case branch body forms a layout block whose enclosing indentation
            // is the branch's pattern column (indentMin). Continuation lines (for
            // example further arguments of a multi-line function application) belong
            // to the body as long as they are indented more than that branch indent,
            // even when they are indented less than the body's first token. Using the
            // body's first token column here would incorrectly reject such uneven
            // indentation (e.g. `abcd` followed by an argument on the next line at an
            // equal or smaller column).
            //
            // We use indentMin + 1 (rather than indentMin) so that branches whose
            // patterns are misaligned by a single column - which this parser tolerates
            // leniently, like avh4 (see the case-arm indentation tests) - are still
            // recognized as separate branches instead of being absorbed as trailing
            // arguments of the preceding branch's body.
            var expressionIndentMin = indentMin + 1;

            if (!TryUnwrap(ParseExpression(expressionIndentMin), out var expression, out var expressionErr))
                return expressionErr;

            var caseRange =
                MakeRange(pattern.Range.Start, expression.Range.End);

            var caseBranch =
                new SyntaxTypes.Case(
                    Pattern: pattern,
                    ArrowLocation: arrowToken.Start,
                    Expression: expression);

            return
                new Node<SyntaxTypes.Case>(
                    caseRange,
                    caseBranch);
        }

        private ParseResult<Node<SyntaxTypes.Pattern>> ParsePattern(
            int indentMin)
        {
            if (!TryUnwrap(ParsePatternLessUncons(indentMin: indentMin), out var lessUncons, out var lessUnconsErr))
                return lessUnconsErr;

            ConsumeAllTrivia();

            // Augment a bare NamedPattern with its constructor arguments (e.g. `Just x`,
            // `Parser parse`, `Good a b c`) before checking for `::` or `as`. Otherwise a
            // pattern like `Parser parse :: rest` or `Good a b c as step` would stop after
            // the head identifier and never see the trailing operator/keyword.
            if (lessUncons.Value is SyntaxTypes.Pattern.NamedPattern namedLeft &&
                namedLeft.Arguments.Count is 0 &&
                NextTokenMatches(
                    peek =>
                    lessUncons.Range.Start.Column <= peek.Start.Column &&
                    CanStartNamedPatternArgument(peek)))
            {
                var patternArguments = new List<Node<SyntaxTypes.Pattern>>();

                while (NextTokenMatches(
                    peek =>
                    lessUncons.Range.Start.Column <= peek.Start.Column &&
                    CanStartNamedPatternArgument(peek)))
                {
                    if (!TryUnwrap(ParsePatternLessUncons(indentMin), out var patternArgument, out var patternArgumentErr))
                        return patternArgumentErr;

                    patternArguments.Add(patternArgument);

                    ConsumeAllTrivia();
                }

                if (patternArguments.Count > 0)
                {
                    var patternRangeEnd =
                        patternArguments[^1].Range.End;

                    var patternRange =
                        lessUncons.Range with
                        {
                            End = patternRangeEnd
                        };

                    lessUncons =
                        new Node<SyntaxTypes.Pattern>(
                            patternRange,
                            new SyntaxTypes.Pattern.NamedPattern(
                                namedLeft.Name,
                                patternArguments));
                }
            }

            if (NextTokenMatches(peek => peek.Lexeme is "::"))
            {
                // | UnConsPattern (Node Pattern) (Node Pattern)

                if (!TryUnwrap(Consume(TokenType.Operator), out var unconsSymbol, out var unconsSymbolErr))
                    return unconsSymbolErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ParsePattern(indentMin: unconsSymbol.End.Column), out var tailPattern, out var tailPatternErr))
                    return tailPatternErr;

                ConsumeAllTrivia();

                return
                    new Node<SyntaxTypes.Pattern>(
                        MakeRange(lessUncons.Range.Start, tailPattern.Range.End),
                        new SyntaxTypes.Pattern.UnConsPattern(
                            Head: lessUncons,
                            ConsOperatorLocation: unconsSymbol.Start,
                            Tail: tailPattern));
            }

            if (NextTokenMatches(peek => peek.Lexeme is "as"))
            {
                // | NamedPattern (Node Pattern) (Node String)

                if (!TryUnwrap(ConsumeKeyword("as"), out var asToken, out var asTokenErr))
                    return asTokenErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ConsumeAnyIdentifier("pattern name"), out var nameToken, out var nameTokenErr))
                    return nameTokenErr;

                var asPattern =
                    new SyntaxTypes.Pattern.AsPattern(
                        Pattern: lessUncons,
                        AsTokenLocation: asToken.Start,
                        Name: new Node<string>(
                            MakeRange(nameToken.Start, nameToken.End),
                            nameToken.Lexeme));

                return
                    new Node<SyntaxTypes.Pattern>(
                        MakeRange(lessUncons.Range.Start, nameToken.End),
                        asPattern);
            }

            return lessUncons;
        }

        private ParseResult<Node<SyntaxTypes.Pattern>> ParsePatternLessUncons(
            int indentMin)
        {
            var start = Peek;

            if (start.Type is TokenType.Identifier)
            {
                if (!TryUnwrap(ConsumeAnyIdentifier("pattern identifier"), out var identifierToken, out var identifierErr))
                    return identifierErr;

                if (identifierToken.Lexeme is "_")
                {
                    // | AllPattern

                    return
                        new Node<SyntaxTypes.Pattern>(
                            identifierToken.Range,
                            new SyntaxTypes.Pattern.AllPattern());
                }

                if (char.IsLower(identifierToken.Lexeme[0]))
                {
                    // | VarPattern String

                    return
                        new Node<SyntaxTypes.Pattern>(
                            MakeRange(identifierToken.Start, identifierToken.End),
                            new SyntaxTypes.Pattern.VarPattern(identifierToken.Lexeme));
                }

                /*
                 * | NamedPattern QualifiedNameRef (List (Node Pattern))
                 * 
                 * like
                 * "Nothing"
                 * or
                 * "Just (Node Pattern)"
                 * or
                 * "Just just"
                 * */

                var namespaces = new List<Token>([identifierToken]);

                while (Peek.Type is TokenType.Dot)
                {
                    if (!TryUnwrap(Consume(TokenType.Dot), out _, out var dotErr))
                        return dotErr;

                    if (!TryUnwrap(ConsumeAnyIdentifier("namespace item"), out var namespaceToken, out var namespaceErr))
                        return namespaceErr;

                    namespaces.Add(namespaceToken);
                }

                var patternNameToken = namespaces.Last();

                ConsumeAllTrivia();

                var patternRange =
                    MakeRange(start.Start, patternNameToken.End);

                var namedPattern =
                    new SyntaxTypes.Pattern.NamedPattern(
                        new SyntaxTypes.QualifiedNameRef(
                            [.. namespaces.SkipLast(1).Select(t => t.Lexeme)],
                            patternNameToken.Lexeme),
                        Arguments: []);

                return
                    new Node<SyntaxTypes.Pattern>(
                        patternRange,
                        namedPattern);
            }

            if (start.Type is TokenType.OpenParen)
            {
                // | TupledPattern (List (Node Pattern))

                /*
                 * Example:
                 * ( a, b )
                 * */

                if (!TryUnwrap(Consume(TokenType.OpenParen), out var parenOpenToken, out var parenOpenErr))
                    return parenOpenErr;

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.CloseParen))
                {
                    if (!TryUnwrap(Consume(TokenType.CloseParen), out var parenCloseToken, out var parenCloseErr))
                        return parenCloseErr;

                    var parenRange =
                        MakeRange(parenOpenToken.Start, parenCloseToken.End);

                    return
                        new Node<SyntaxTypes.Pattern>(
                            parenRange,
                            new SyntaxTypes.Pattern.UnitPattern());
                }

                {
                    if (!TryUnwrap(ParsePattern(indentMin), out var firstPattern, out var firstPatternErr))
                        return firstPatternErr;

                    ConsumeAllTrivia();

                    var furtherPatternsWithCommas =
                        new List<(Location CommaLocation, Node<SyntaxTypes.Pattern> Pattern)>();

                    while (Peek.Type is TokenType.Comma)
                    {
                        if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                            return commaErr;

                        ConsumeAllTrivia();

                        if (!TryUnwrap(ParsePattern(indentMin), out var furtherPattern, out var furtherPatternErr))
                            return furtherPatternErr;

                        furtherPatternsWithCommas.Add((commaToken.Start, furtherPattern));

                        ConsumeAllTrivia();
                    }

                    if (!TryUnwrap(Consume(TokenType.CloseParen), out var parenCloseToken, out var parenCloseErr))
                        return parenCloseErr;

                    var parenRange =
                        MakeRange(parenOpenToken.Start, parenCloseToken.End);

                    if (furtherPatternsWithCommas.Count is 0)
                    {
                        return
                            new Node<SyntaxTypes.Pattern>(
                                parenRange,
                                new SyntaxTypes.Pattern.ParenthesizedPattern(
                                    Pattern: firstPattern));
                    }

                    var tupledPattern =
                        new SyntaxTypes.Pattern.TuplePattern(
                            Elements: new SeparatedSyntaxList<Node<SyntaxTypes.Pattern>>.NonEmpty(
                                firstPattern,
                                furtherPatternsWithCommas));

                    return
                        new Node<SyntaxTypes.Pattern>(
                            parenRange,
                            tupledPattern);
                }
            }

            if (start.Type is TokenType.StringLiteral or TokenType.TripleQuotedStringLiteral)
            {
                // | StringPattern String

                if (!TryUnwrap(Consume(start.Type), out var literalToken, out var literalTokenErr))
                    return literalTokenErr;

                var stringPattern =
                    new SyntaxTypes.Pattern.StringPattern(literalToken.Lexeme);

                return
                    new Node<SyntaxTypes.Pattern>(
                        MakeRange(start.Start, literalToken.End),
                        stringPattern);
            }


            if (start.Type is TokenType.CharLiteral)
            {
                // | CharPattern Char

                if (!TryUnwrap(Consume(TokenType.CharLiteral), out var literalToken, out var literalTokenErr))
                    return literalTokenErr;

                if (!TryConvertToUtf32(literalToken.Lexeme, out var charCodePoint))
                {
                    return
                        new ElmSyntaxParseError(
                            literalToken.Start,
                            "Invalid character literal: '" + literalToken.Lexeme + "'");
                }

                var charPattern =
                    new SyntaxTypes.Pattern.CharPattern(charCodePoint);

                return
                    new Node<SyntaxTypes.Pattern>(
                        MakeRange(start.Start, literalToken.End),
                        charPattern);
            }

            if (start.Type is TokenType.NumberLiteral)
            {
                if (start.Lexeme.StartsWith("0x"))
                {
                    // | HexPattern Int

                    if (!TryUnwrap(Consume(TokenType.NumberLiteral), out var literalToken, out var literalTokenErr))
                        return literalTokenErr;

                    if (!long.TryParse(
                        literalToken.Lexeme.AsSpan(2),
                        System.Globalization.NumberStyles.HexNumber,
                        System.Globalization.CultureInfo.InvariantCulture,
                        out var hexValue))
                    {
                        return
                            new ElmSyntaxParseError(
                                literalToken.Start,
                                "Hexadecimal pattern literal out of range: " + literalToken.Lexeme);
                    }

                    var hexPattern =
                        new SyntaxTypes.Pattern.HexPattern(hexValue);

                    return
                        new Node<SyntaxTypes.Pattern>(
                            MakeRange(start.Start, literalToken.End),
                            hexPattern);
                }

                // | IntegerPattern Int
                if (long.TryParse(start.Lexeme, out var number))
                {
                    if (!TryUnwrap(Consume(TokenType.NumberLiteral), out var literalToken, out var literalTokenErr))
                        return literalTokenErr;

                    var integerPattern =
                        new SyntaxTypes.Pattern.IntPattern(number);

                    return
                        new Node<SyntaxTypes.Pattern>(
                            MakeRange(start.Start, literalToken.End),
                            integerPattern);
                }
            }

            if (start.Type is TokenType.OpenBracket)
            {
                // | ListPattern (List (Node Pattern))

                if (!TryUnwrap(Consume(TokenType.OpenBracket), out var listOpenToken, out var listOpenErr))
                    return listOpenErr;

                ConsumeAllTrivia();

                Node<SyntaxTypes.Pattern>? firstItem = null;
                var restItems = new List<(Location CommaLocation, Node<SyntaxTypes.Pattern> Pattern)>();

                // Parse first item if any
                if (NextTokenMatches(peek => peek.Type is not TokenType.CloseBracket))
                {
                    if (!TryUnwrap(ParsePattern(indentMin), out var firstItemValue, out var firstItemErr))
                        return firstItemErr;

                    firstItem = firstItemValue;
                    ConsumeAllTrivia();

                    // Parse remaining items
                    while (Peek.Type is TokenType.Comma)
                    {
                        if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                            return commaErr;

                        ConsumeAllTrivia();

                        if (NextTokenMatches(peek => peek.Type is not TokenType.CloseBracket))
                        {
                            if (!TryUnwrap(ParsePattern(indentMin), out var nextPattern, out var nextPatternErr))
                                return nextPatternErr;

                            restItems.Add((commaToken.Start, nextPattern));
                            ConsumeAllTrivia();
                        }
                    }
                }

                if (!TryUnwrap(Consume(TokenType.CloseBracket), out var listCloseToken, out var listCloseErr))
                    return listCloseErr;

                var listRange =
                    MakeRange(listOpenToken.Start, listCloseToken.End);

                SeparatedSyntaxList<Node<SyntaxTypes.Pattern>> elementsList =
                    firstItem is null
                    ?
                    new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Pattern>>.Empty()
                    :
                    new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Pattern>>.NonEmpty(
                        firstItem,
                        restItems);

                var listPatternValue =
                    new SyntaxTypes.Pattern.ListPattern(
                        Elements: elementsList);

                return new Node<SyntaxTypes.Pattern>(listRange, listPatternValue);
            }

            if (start.Type is TokenType.OpenBrace)
            {
                // | RecordPattern (List (Node String))

                if (!TryUnwrap(Consume(TokenType.OpenBrace), out var recordOpenToken, out var recordOpenErr))
                    return recordOpenErr;

                ConsumeAllTrivia();

                Node<string>? firstField = null;
                var restFields = new List<(Location CommaLocation, Node<string> FieldName)>();

                // Parse first field if any
                if (Peek.Type is not TokenType.CloseBrace)
                {
                    if (!TryUnwrap(ConsumeAnyIdentifier("field name"), out var fieldName, out var fieldNameErr))
                        return fieldNameErr;

                    firstField = new Node<string>(fieldName.Range, fieldName.Lexeme);
                    ConsumeAllTrivia();

                    // Parse remaining fields
                    while (Peek.Type is TokenType.Comma)
                    {
                        if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                            return commaErr;

                        ConsumeAllTrivia();

                        if (Peek.Type is not TokenType.CloseBrace)
                        {
                            if (!TryUnwrap(ConsumeAnyIdentifier("field name"), out var nextFieldName, out var nextFieldNameErr))
                                return nextFieldNameErr;

                            var nextFieldNode = new Node<string>(nextFieldName.Range, nextFieldName.Lexeme);
                            restFields.Add((commaToken.Start, nextFieldNode));
                            ConsumeAllTrivia();
                        }
                    }
                }

                if (!TryUnwrap(Consume(TokenType.CloseBrace), out var recordCloseToken, out var recordCloseErr))
                    return recordCloseErr;

                var recordRange =
                    MakeRange(recordOpenToken.Start, recordCloseToken.End);

                SeparatedSyntaxList<Node<string>> fieldsList =
                    firstField is null
                    ?
                    new SyntaxTypes.SeparatedSyntaxList<Node<string>>.Empty()
                    :
                    new SyntaxTypes.SeparatedSyntaxList<Node<string>>.NonEmpty(
                        firstField,
                        restFields);

                var recordPattern =
                    new SyntaxTypes.Pattern.RecordPattern(
                        Fields: fieldsList);

                return new Node<SyntaxTypes.Pattern>(recordRange, recordPattern);
            }

            return
                ErrorAtCurrentLocation(
                    "Unsupported pattern type: " + start.Type +
                    " at " + start.Start.Row + ":" + start.Start.Column +
                    " - " + start.End.Row + ":" + start.End.Column +
                    " - " + start.Lexeme);
        }

        private static bool CanStartArgumentPattern(Token token)
        {
            if (token.Type is
                TokenType.StringLiteral or TokenType.TripleQuotedStringLiteral or TokenType.NumberLiteral or TokenType.CharLiteral)
            {
                return false;
            }

            return CanStartPattern(token);
        }

        private static bool CanStartPattern(Token token)
        {
            // You want to allow tokens that can start patterns but not tokens that are infix operators,
            // commas, closing parentheses/brackets, etc.

            return token.Type switch
            {
                TokenType.StringLiteral or
                TokenType.TripleQuotedStringLiteral or
                TokenType.CharLiteral or
                TokenType.NumberLiteral or
                TokenType.Identifier or
                TokenType.OpenParen or
                TokenType.OpenBrace or
                TokenType.OpenBracket =>
                true,

                _ =>
                false,
            };
        }

        // Like CanStartPattern, but additionally excludes keywords that, while
        // tokenized as identifiers, terminate the argument list of a NamedPattern
        // (e.g. `as` in `Good a b c as step`, `of` ending a case scrutinee
        // pattern, `then`/`else`/`in`/`let` ending a let-destructuring pattern).
        private static bool CanStartNamedPatternArgument(Token token)
        {
            if (!CanStartPattern(token))
            {
                return false;
            }

            if (token.Type is TokenType.Identifier &&
                token.Lexeme is
                "as" or "of" or "then" or "else" or "in" or "let")
            {
                return false;
            }

            return true;
        }

        private ParseResult<Node<SyntaxTypes.Expression>> ParseRecordExpr(
            int indentMin)
        {
            var start = Peek;

            if (!TryUnwrap(Consume(TokenType.OpenBrace), out var openBrace, out var openBraceErr))
                return openBraceErr;

            ConsumeAllTrivia();

            Token? updatedRecord = null;
            Location pipeLocation = default;

            // Check for record update syntax: { name | field = value }
            if (Peek.Type is TokenType.Identifier)
            {
                if (!TryUnwrap(ConsumeAnyIdentifier("record name or field name"), out var nameToken, out var nameTokenErr))
                    return nameTokenErr;

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.Pipe))
                {
                    // Record update expression
                    updatedRecord = nameToken;

                    if (!TryUnwrap(Consume(TokenType.Pipe), out var pipeToken, out var pipeErr))
                        return pipeErr;

                    pipeLocation = pipeToken.Start;
                    ConsumeAllTrivia();
                }
                else if (NextTokenMatches(peek => peek.Type is TokenType.Equal))
                {
                    // Regular record, first field parsed
                    // Put the identifier back - but we already consumed it, so we need to parse the field now
                    if (!TryUnwrap(Consume(TokenType.Equal), out var equalsToken, out var equalsErr))
                        return equalsErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ParseExpression(indentMin), out var valueExpr, out var valueExprErr))
                        return valueExprErr;

                    ConsumeAllTrivia();

                    var recordFirstField =
                        new RecordExprField(
                            new Node<string>(nameToken.Range, nameToken.Lexeme),
                            equalsToken.Start,
                            valueExpr);

                    var recordRestFields = new List<(Location SeparatorLocation, RecordExprField Node)>();

                    // Parse remaining fields
                    while (Peek.Type is TokenType.Comma)
                    {
                        if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                            return commaErr;

                        ConsumeAllTrivia();

                        if (!TryUnwrap(ConsumeAnyIdentifier("field name"), out var nextFieldName, out var nextFieldNameErr))
                            return nextFieldNameErr;

                        ConsumeAllTrivia();

                        if (!TryUnwrap(Consume(TokenType.Equal), out var nextEqualsToken, out var nextEqualsErr))
                            return nextEqualsErr;

                        ConsumeAllTrivia();

                        if (!TryUnwrap(ParseExpression(indentMin), out var nextValueExpr, out var nextValueExprErr))
                            return nextValueExprErr;

                        ConsumeAllTrivia();

                        var nextField =
                            new RecordExprField(
                                new Node<string>(nextFieldName.Range, nextFieldName.Lexeme),
                                nextEqualsToken.Start,
                                nextValueExpr);

                        recordRestFields.Add((commaToken.Start, nextField));
                    }

                    if (!TryUnwrap(Consume(TokenType.CloseBrace), out var closeBrace, out var closeBraceErr))
                        return closeBraceErr;

                    var range = MakeRange(start.Start, closeBrace.End);

                    return
                        new Node<SyntaxTypes.Expression>(
                            range,
                            new SyntaxTypes.Expression.RecordExpr(
                                Fields: new SeparatedSyntaxList<RecordExprField>.NonEmpty(
                                    First: recordFirstField,
                                    Rest: recordRestFields)));
                }
                else if (NextTokenMatches(peek => peek.Type is TokenType.CloseBrace))
                {
                    // Empty record or just a name followed by close brace? This shouldn't happen in valid Elm
                    if (!TryUnwrap(Consume(TokenType.CloseBrace), out var closeBrace, out var closeBraceErr))
                        return closeBraceErr;

                    var range = MakeRange(start.Start, closeBrace.End);

                    return
                        new Node<SyntaxTypes.Expression>(
                            range,
                            new SyntaxTypes.Expression.RecordExpr(
                                Fields: new SeparatedSyntaxList<RecordExprField>.Empty()));
                }
            }

            // Either empty record or record update expression - parse fields
            RecordExprField? firstField = null;
            var restFieldsUpdate = new List<(Location SeparatorLocation, RecordExprField Node)>();

            if (Peek.Type is not TokenType.CloseBrace)
            {
                // Parse first field
                if (!TryUnwrap(ConsumeAnyIdentifier("field name"), out var fieldName, out var fieldNameErr))
                    return fieldNameErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(Consume(TokenType.Equal), out var equalsToken, out var equalsErr))
                    return equalsErr;

                ConsumeAllTrivia();

                if (!TryUnwrap(ParseExpression(indentMin), out var valueExpr, out var valueExprErr))
                    return valueExprErr;

                ConsumeAllTrivia();

                firstField =
                    new RecordExprField(
                        new Node<string>(fieldName.Range, fieldName.Lexeme),
                        equalsToken.Start,
                        valueExpr);

                // Parse remaining fields
                while (Peek.Type is TokenType.Comma)
                {
                    if (!TryUnwrap(Consume(TokenType.Comma), out var commaToken, out var commaErr))
                        return commaErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ConsumeAnyIdentifier("field name"), out var nextFieldName, out var nextFieldNameErr))
                        return nextFieldNameErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(Consume(TokenType.Equal), out var nextEqualsToken, out var nextEqualsErr))
                        return nextEqualsErr;

                    ConsumeAllTrivia();

                    if (!TryUnwrap(ParseExpression(indentMin), out var nextValueExpr, out var nextValueExprErr))
                        return nextValueExprErr;

                    ConsumeAllTrivia();

                    var nextField =
                        new RecordExprField(
                            new Node<string>(nextFieldName.Range, nextFieldName.Lexeme),
                            nextEqualsToken.Start,
                            nextValueExpr);

                    restFieldsUpdate.Add((commaToken.Start, nextField));
                }
            }

            if (!TryUnwrap(Consume(TokenType.CloseBrace), out var closeBraceUpdate, out var closeBraceUpdateErr))
                return closeBraceUpdateErr;

            var rangeUpdate = MakeRange(start.Start, closeBraceUpdate.End);

            SeparatedSyntaxList<RecordExprField> fieldsList =
                firstField is null
                ?
                new SyntaxTypes.SeparatedSyntaxList<SyntaxTypes.RecordExprField>.Empty()
                :
                new SyntaxTypes.SeparatedSyntaxList<SyntaxTypes.RecordExprField>.NonEmpty(
                    First: firstField,
                    Rest: restFieldsUpdate);

            if (updatedRecord is not null)
            {
                var recordUpdateExpr =
                    new SyntaxTypes.Expression.RecordUpdateExpression(
                        RecordName: new Node<string>(
                            updatedRecord.Range,
                            updatedRecord.Lexeme),
                        PipeLocation: pipeLocation,
                        Fields: fieldsList);

                return new Node<SyntaxTypes.Expression>(rangeUpdate, recordUpdateExpr);
            }

            return
                new Node<SyntaxTypes.Expression>(
                    rangeUpdate,
                    new SyntaxTypes.Expression.RecordExpr(
                        Fields: fieldsList));
        }

        // Helper methods

        /// <summary>
        /// Creates a Range from start and end locations.
        /// </summary>
        private static Range MakeRange(Location start, Location end) =>
            new(start, end);

        private bool IsAtEnd() =>
            _current >= tokens.Length;

        /// <summary>
        /// Returns the next token without consuming it. Once the cursor has moved past the last
        /// real token this yields a synthetic <see cref="TokenType.EndOfFile"/> token located right
        /// after the last real token (or at 1:1 for a completely empty token stream) instead of
        /// indexing out of bounds, so every existing <c>Peek.Type is ...</c>/<c>Peek.Lexeme</c> check
        /// keeps working — including at the end of input — without throwing.
        /// </summary>
        private Token Peek =>
            _current < tokens.Length
            ?
            tokens.Span[_current]
            :
            EndOfFileToken;

        private Token EndOfFileToken
        {
            get
            {
                var location =
                    tokens.Length is 0
                    ?
                    new Location(1, 1)
                    :
                    tokens.Span[tokens.Length - 1].End;

                return new Token(TokenType.EndOfFile, "", location, location);
            }
        }

        private Token Advance() =>
            tokens.Span[_current++];

        private bool NextTokenMatches(Func<Token, bool> predicate)
        {
            if (IsAtEnd())
            {
                return false;
            }

            return predicate(Peek);
        }

        private IEnumerable<Token> EnumeratePrecedingTokensBackwards()
        {
            var pointer = _current - 1;

            while (pointer >= 0)
            {
                yield return tokens.Span[pointer--];
            }
        }

        private IEnumerable<Token> EnumerateFollowingTokens()
        {
            var pointer = _current;

            while (pointer < tokens.Length)
            {
                yield return tokens.Span[pointer++];
            }
        }

        private ParseResult<Token> ConsumeKeyword(string expectedLexeme)
        {
            return
                Consume(TokenType.Identifier, expectedLexeme);
        }

        private ParseResult<Token> ConsumeAnyIdentifier(string description)
        {
            return
                Consume(
                    TokenType.Identifier,
                    expectedLexeme: null,
                    tokenDescription: description);
        }

        // Consume a token of a given type, reporting a parse error (rather than throwing) if the
        // next token does not match.
        private ParseResult<Token> Consume(
            TokenType expectedType,
            string? expectedLexeme = null,
            string? tokenDescription = null)
        {
            var nextToken = Peek;

            if (nextToken.Type != expectedType)
            {
                return
                    new ElmSyntaxParseError(
                        nextToken.Start,
                        "Expected " + (tokenDescription ?? "token") + " of type " + expectedType +
                        " but found " + nextToken.Type);
            }

            if (expectedLexeme is not null && nextToken.Lexeme != expectedLexeme)
            {
                var errorDescription =
                    (expectedType is TokenType.Identifier
                    ?
                    "Expected keyword '" + expectedLexeme
                    :
                    "Expected token with lexeme " + expectedLexeme) +
                    "' but found '" + nextToken.Lexeme + "'";

                return
                    new ElmSyntaxParseError(
                        nextToken.Start,
                        errorDescription);
            }

            return Advance();
        }

        /// <summary>
        /// Builds an <see cref="ElmSyntaxParseError"/> pointing at the next unconsumed token (or the
        /// end-of-file location if no tokens remain), for parse failures that are detected by a
        /// direct check rather than by <see cref="Consume"/> rejecting an unexpected token.
        /// </summary>
        private ElmSyntaxParseError ErrorAtCurrentLocation(string message)
        {
            return new ElmSyntaxParseError(Peek.Start, message);
        }

        private IReadOnlyList<Token> ConsumeAllTrivia()
        {
            return
                [
                .. ConsumeWhileLazy(
                    token =>
                    token.Type is TokenType.Comment ||
                    token.Type is TokenType.Whitespace ||
                    token.Type is TokenType.Newline)
                ];
        }

        private IEnumerable<Token> ConsumeWhileLazy(
            Func<Token, bool> predicate)
        {
            while (!IsAtEnd() && predicate(Peek))
            {
                yield return Advance();
            }
        }
    }
}
