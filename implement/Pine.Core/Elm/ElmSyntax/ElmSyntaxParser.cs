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
    public static Result<string, ElmValue> ParseModuleTextAsElmSyntaxElmValue(
        string elmModuleText)
    {
        var parseResult =
            ParseModuleText(elmModuleText);

        if (parseResult.IsErrOrNull() is { } err)
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
    public static Result<string, SyntaxTypes.File> ParseModuleText(
        string elmModuleText)
    {
        var tokenizer = new Tokenizer(elmModuleText);

        var tokens = tokenizer.Tokenize().ToArray();

        var parser = new Parser(tokens);

        return parser.ParseFile();
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

        var tokens = tokenizer.Tokenize().ToArray();

        var parser = new Parser(tokens);

        return parser.ParseModuleHeaderWithRecovery();
    }

    private record InfixOperatorInfo(
        int Precedence,
        InfixDirection Direction)
    {
        public static InfixOperatorInfo GetInfo(string symbol) =>
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

                _ =>
                throw new ArgumentException("Unknown operator: " + symbol),
            };
    }

    private record Token(
        TokenType Type,
        string Lexeme,
        Location Start,
        Location End)
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
    }

    private static bool IsKeyword(Token token)
    {
        if (token.Type is TokenType.Identifier)
        {
            return token.Lexeme switch
            {
                "case" or "of" or "let" or "in" or "if" or "then" or "else" or "import" or "module" or "exposing" =>
                true,

                _ => false,
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
                "type" or "port" or "infix" =>
                true,

                _ => false,
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

            sb.Append(token.Lexeme);
            previousToken = token;
        }

        return sb.ToString();
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

            _ => false,
        };

    private class Tokenizer(string input)
    {
        private readonly string _input = input;
        private int _position;
        private int _line = 1;
        private int _column = 1;

        public IEnumerable<Token> Tokenize()
        {
            while (!IsAtEnd())
            {
                var token = NextToken();

                if (token is not null)
                {
                    yield return token;
                }
            }
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

        private Token NextToken()
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

                    var literal = ParseStringLiteral(termination: "\"\"\"");

                    if (literal is not null)
                    {
                        Location end = new(_line, _column);

                        return new Token(TokenType.TripleQuotedStringLiteral, literal, start, end);
                    }
                }
                else
                {
                    var literal = ParseStringLiteral(termination: "\"");

                    if (literal is not null)
                    {
                        Location end = new(_line, _column);

                        return new Token(TokenType.StringLiteral, literal, start, end);
                    }
                }

                // Unterminated string literal; here you might want to throw an error.
                throw new ParserException(
                    "Unterminated string literal",
                    lineNumber: _line,
                    columnNumber: _column);
            }

            // Handle character literals
            if (current is '\'' && PeekNext() is not '\'')
            {
                Advance(); // Consume the opening quote

                var literal = ParseStringLiteral(termination: "'");

                if (literal is not null)
                {
                    Location end = new(_line, _column);
                    return new Token(TokenType.CharLiteral, literal, start, end);
                }

                // Unterminated character literal; here you might want to throw an error.
                throw new ParserException(
                    "Unterminated character literal",
                    lineNumber: _line,
                    columnNumber: _column);
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
                        for (var k = 0; k < 5; k++) Advance();

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

                        return new Token(TokenType.GLSLLiteral, glslContent.ToString(), start, new Location(_line, _column));
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

                        return new Token(
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

                        return new Token(
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

                        return new Token(
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

                        return new Token(
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

                        return new Token(
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

                        return new Token(
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

        private string? ParseStringLiteral(string termination)
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

                                var codePoint = int.Parse(unicode, System.Globalization.NumberStyles.HexNumber);

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

            return null;
        }
    }

    private class Parser(
        ReadOnlyMemory<Token> tokens)
    {
        private int _current = 0;

        // Entry point: parse the entire file and return a File record.
        public Result<string, SyntaxTypes.File> ParseFile()
        {
            try
            {
                IReadOnlyList<Token> allComments =
                    [.. tokens.ToArray().Where(t => t.Type is TokenType.Comment)];

                IReadOnlyList<Token> docComments =
                    [.. allComments.Where(c => c.Lexeme.StartsWith("{-|"))];

                ConsumeAllTrivia();

                // Parse the module header
                var moduleDefinition = ParseModule();

                ConsumeAllTrivia();

                // Parse the imports (if any)

                var imports = new List<Node<SyntaxTypes.Import>>();

                while (NextTokenMatches(t => t.Type is TokenType.Identifier && t.Lexeme is "import"))
                {
                    var import = ParseImport();

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
                        throw ExceptionForCurrentLocation(
                            "Unexpected token '" + Peek.Lexeme + "' after parsing " +
                            declarations.Count + " declarations");
                    }

                    bool CanAttachComment(Token commentToken)
                    {
                        // Check if there are any other comments (non-doc) between this doc comment and the declaration
                        var hasInterveningComments = allComments.Any(c =>
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
                                return lastDecl.Range.End.Row < commentToken.End.Row && commentToken.End.Row < Peek.Start.Row;
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

                    try
                    {
                        declarations.Add(ParseDeclaration(docComment));
                    }
                    catch (Exception ex)
                    {
                        // Declaration parsing failed - capture as incomplete declaration.
                        // We catch Exception (not just ParserException) because parsing failures
                        // can manifest as various exception types including InvalidOperationException
                        // from LINQ operations when tokens are exhausted.
                        // Skip to the next declaration boundary (column 1 token that can start a declaration)
                        var incompleteTokens = new List<Token>();

                        // Reset to start of failed declaration
                        _current = declStartPosition;

                        // Capture error information
                        Location errorLocation;
                        string errorMessage;

                        if (ex is ParserException parserEx && parserEx.LineNumber.HasValue && parserEx.ColumnNumber.HasValue)
                        {
                            // Use the location from the parser exception
                            errorLocation = new Location(parserEx.LineNumber.Value, parserEx.ColumnNumber.Value);
                            errorMessage = ex.Message;
                        }
                        else
                        {
                            // Use the current token location as the error location
                            var currentToken = IsAtEnd() ? tokens.Span[tokens.Length - 1] : Peek;
                            errorLocation = currentToken.Start;
                            errorMessage = ex.Message;
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
                            var lastToken = incompleteTokens[^1];

                            // Reconstruct the text from tokens, preserving structure
                            var incompleteText = ReconstructTextFromTokens(incompleteTokens);

                            // Trim trailing empty lines
                            incompleteText = TrimTrailingEmptyLines(incompleteText);

                            // Calculate end location based on trimmed text
                            var endLocation = CalculateEndLocation(firstToken.Start, incompleteText);
                            var range = new Range(firstToken.Start, endLocation);

                            // Add as Node<IncompleteDeclaration> with proper range and error info
                            incompleteDeclarations.Add(new Node<IncompleteDeclaration>(
                                range,
                                new IncompleteDeclaration(
                                    incompleteText,
                                    errorLocation,
                                    errorMessage)));
                        }
                    }

                    ConsumeAllTrivia();
                }

                IReadOnlyList<Node<string>> commentsOnDeclarations =
                    [..declarations.SelectWhereNotNull(decl =>
                    decl.Value switch
                    {
                        SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration =>
                        functionDeclaration.Function.Documentation,

                        SyntaxTypes.Declaration.CustomTypeDeclaration typeDecl =>
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
                    })];

                bool CommentEmittedInGlobalList(Token commentToken)
                {
                    if (commentsOnDeclarations.Any(onDecl => onDecl.Range == commentToken.Range))
                    {
                        return false;
                    }

                    return true;
                }

                IReadOnlyList<Node<string>> commentsGlobalList =
                    [.. allComments
                    .Where(CommentEmittedInGlobalList)
                    .Select(token => new Node<string>(token.Range, token.Lexeme))
                    ];

                return new SyntaxTypes.File(
                    moduleDefinition,
                    imports,
                    declarations,
                    Comments: commentsGlobalList,
                    IncompleteDeclarations: incompleteDeclarations);
            }
            catch (Exception ex)
            {
                return ex.Message;
            }
        }

        /// <summary>
        /// Parses module header (module definition and imports) with error recovery for imports.
        /// If an import fails to parse, it records the successfully parsed imports up to that point
        /// and returns successfully. This allows building dependency graphs even for incomplete modules.
        /// </summary>
        public Result<string, ModuleHeaderInfo> ParseModuleHeaderWithRecovery()
        {
            try
            {
                ConsumeAllTrivia();

                // Parse the module header
                var moduleDefinition = ParseModule();

                var moduleName = SyntaxTypes.Module.GetModuleName(moduleDefinition.Value).Value;

                ConsumeAllTrivia();

                // Parse imports with error recovery
                var importedModuleNames = new List<ModuleName>();

                while (NextTokenMatches(t => t.Type is TokenType.Identifier && t.Lexeme is "import"))
                {
                    try
                    {
                        var import = ParseImport();
                        importedModuleNames.Add(import.Value.ModuleName.Value);
                        ConsumeAllTrivia();
                    }
                    catch (ParserException)
                    {
                        // Import parsing failed - stop parsing imports but don't fail
                        // This allows modules with incomplete imports to still provide their name
                        // and any successfully parsed imports
                        break;
                    }
                    catch (InvalidOperationException)
                    {
                        // Can happen when tokens are exhausted during parsing
                        break;
                    }
                }

                return new ModuleHeaderInfo(moduleName, importedModuleNames);
            }
            catch (Exception ex)
            {
                return ex.Message;
            }
        }

        // Parses the module header and returns a Node<SyntaxTypes.Module>
        private Node<SyntaxTypes.Module> ParseModule()
        {
            if (NextTokenMatches(p => p.Lexeme is "effect"))
            {
                /*
                 * Example syntax:
                 * ----
                 * effect module Time where { subscription = MySub } exposing
                 * */

                var effectKeyword = ConsumeKeyword("effect");

                ConsumeAllTrivia();

                var moduleKeyword = ConsumeKeyword("module");

                ConsumeAllTrivia();

                var moduleNameParts = new List<Token>();

                var firstModuleNamePart =
                    ConsumeAnyIdentifier("module name");

                moduleNameParts.Add(firstModuleNamePart);

                while (Peek.Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var moduleNamePart = ConsumeAnyIdentifier("module name part");

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
                    ConsumeKeyword("where");

                    ConsumeAllTrivia();

                    var recordExprNode = ParseRecordExpr(indentMin: 1);

                    if (recordExprNode.Value is not SyntaxTypes.Expression.RecordExpr recordExpr)
                    {
                        throw ExceptionForCurrentLocation(
                            "Expected record expression after 'where', found: " +
                            recordExprNode.Value.GetType().Name);
                    }

                    foreach (var recordField in FromFullSyntaxModel.ToList(recordExpr.Fields))
                    {
                        if (recordField.FieldName.Value is "command")
                        {
                            if (recordField.ValueExpr.Value is not SyntaxTypes.Expression.FunctionOrValue functionOrValue)
                            {
                                throw ExceptionForCurrentLocation(
                                    "Expected function or value for 'command', found: " +
                                    recordField.ValueExpr.GetType().Name);
                            }

                            command = new Node<string>(
                                recordField.ValueExpr.Range,
                                functionOrValue.Name);
                        }

                        if (recordField.FieldName.Value is "subscription")
                        {
                            if (recordField.ValueExpr.Value is not SyntaxTypes.Expression.FunctionOrValue functionOrValue)
                            {
                                throw ExceptionForCurrentLocation(
                                    "Expected function or value for 'subscription', found: " +
                                    recordField.ValueExpr.GetType().Name);
                            }

                            subscription = new Node<string>(
                                recordField.ValueExpr.Range,
                                functionOrValue.Name);
                        }
                    }

                    ConsumeAllTrivia();
                }

                var exposingNode = ParseExposing();

                var moduleData =
                    new SyntaxTypes.EffectModuleData(
                        ModuleName: moduleNameNode,
                        ExposingTokenLocation: exposingNode.Range.Start,
                        ExposingList: exposingNode,
                        Command: command,
                        Subscription: subscription);

                var moduleNodeValue = new SyntaxTypes.Module.EffectModule(
                    EffectTokenLocation: effectKeyword.Start,
                    ModuleTokenLocation: moduleKeyword.Start,
                    ModuleData: moduleData);
                var moduleNode = new Node<SyntaxTypes.Module>(MakeRange(effectKeyword.Start, exposingNode.Range.End), moduleNodeValue);

                return moduleNode;
            }

            if (NextTokenMatches(p => p.Lexeme is "port"))
            {
                /*
                 * Example syntax:
                 * ----
                 * port module Main exposing (..)
                 * */

                var portKeyword = ConsumeKeyword("port");

                ConsumeAllTrivia();

                var moduleKeyword = ConsumeKeyword("module");

                ConsumeAllTrivia();

                // Parse module name (e.g. Main)
                var moduleNameParts = new List<Token>();

                var firstModuleNamePart =
                    ConsumeAnyIdentifier("module name");

                moduleNameParts.Add(firstModuleNamePart);

                while (Peek.Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var moduleNamePart = ConsumeAnyIdentifier("module name part");

                    moduleNameParts.Add(moduleNamePart);
                }

                // Create a Node<IReadOnlyList<string>> for the module name.
                var moduleNameNode =
                    new Node<ModuleName>(
                        MakeRange(firstModuleNamePart.Start, moduleNameParts.Last().End),
                        [.. moduleNameParts.Select(t => t.Lexeme)]);

                ConsumeAllTrivia();

                var exposingNode = ParseExposing();

                // Build the module data and wrap it in a Module.PortModule.
                var moduleData = new SyntaxTypes.DefaultModuleData(
                    ModuleName: moduleNameNode,
                    ExposingTokenLocation: exposingNode.Range.Start,
                    ExposingList: exposingNode);
                var moduleNodeValue = new SyntaxTypes.Module.PortModule(
                    PortTokenLocation: portKeyword.Start,
                    ModuleTokenLocation: moduleKeyword.Start,
                    ModuleData: moduleData);

                var moduleNode = new Node<SyntaxTypes.Module>(
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

                var keywordToken = ConsumeKeyword("module");

                ConsumeAllTrivia();

                // Parse module name (e.g. CompilationInterface.ElmMake.Generated_ElmMake)
                var moduleNameParts = new List<Token>();

                var firstModuleNamePart =
                    ConsumeAnyIdentifier("module name");

                moduleNameParts.Add(firstModuleNamePart);

                while (Peek.Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var moduleNamePart = ConsumeAnyIdentifier("module name part");

                    moduleNameParts.Add(moduleNamePart);
                }

                // Create a Node<IReadOnlyList<string>> for the module name.
                var moduleNameNode =
                    new Node<ModuleName>(
                        MakeRange(firstModuleNamePart.Start, moduleNameParts.Last().End),
                        [.. moduleNameParts.Select(t => t.Lexeme)]);

                ConsumeAllTrivia();

                var exposingNode = ParseExposing();

                // Build the module data and wrap it in a Module.NormalModule.
                var moduleData = new SyntaxTypes.DefaultModuleData(
                    ModuleName: moduleNameNode,
                    ExposingTokenLocation: exposingNode.Range.Start,
                    ExposingList: exposingNode);
                var moduleNodeValue = new SyntaxTypes.Module.NormalModule(
                    ModuleTokenLocation: keywordToken.Start,
                    ModuleData: moduleData);

                var moduleNode = new Node<SyntaxTypes.Module>(
                    MakeRange(keywordToken.Start, exposingNode.Range.End),
                    moduleNodeValue);

                return moduleNode;
            }
        }

        private Node<SyntaxTypes.Import> ParseImport()
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

            var importKeyword = ConsumeKeyword("import");

            ConsumeAllTrivia();
            // Parse module name (e.g. CompilationInterface.ElmMake.Generated_ElmMake)

            var firstModuleNamePart =
                ConsumeAnyIdentifier("module name");

            var moduleNameParts = new List<Token>([firstModuleNamePart]);

            while (Peek.Type is TokenType.Dot)
            {
                Consume(TokenType.Dot);

                var moduleNamePart = ConsumeAnyIdentifier("module name part");

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
                var asKeyword = ConsumeKeyword("as");

                ConsumeAllTrivia();

                var aliasToken = ConsumeAnyIdentifier("module alias");

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
                var exposingNode = ParseExposing();

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

        private Node<SyntaxTypes.Exposing> ParseExposing()
        {
            var keyword = ConsumeKeyword("exposing");

            ConsumeAllTrivia();

            var openParen = Consume(TokenType.OpenParen);

            ConsumeAllTrivia();

            if (IsAtEnd())
            {
                throw ExceptionForCurrentLocation(
                    "Unexpected end of file in exposing list");
            }

            if (Peek.Type is TokenType.DotDot)
            {
                var dotDotToken = Consume(TokenType.DotDot);

                ConsumeAllTrivia();

                var closeParen = Consume(TokenType.CloseParen);

                return new Node<SyntaxTypes.Exposing>(
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
                        var commaToken = Consume(TokenType.Comma);

                        ConsumeAllTrivia();

                        if (IsAtEnd())
                        {
                            throw ExceptionForCurrentLocation(
                                "Unexpected end of file in exposing list");
                        }

                        var topLevelExposeNode = ParseTopLevelExpose();

                        restNodes.Add((commaToken.Start, topLevelExposeNode));

                        ConsumeAllTrivia();
                    }
                    else
                    {
                        firstNode = ParseTopLevelExpose();

                        ConsumeAllTrivia();
                    }
                }

                if (IsAtEnd())
                {
                    throw ExceptionForCurrentLocation(
                        "Unexpected end of file: expected ')' to close exposing list");
                }

                var closeParen = Consume(TokenType.CloseParen);

                SeparatedSyntaxList<Node<SyntaxTypes.TopLevelExpose>> nodesList =
                    firstNode is null
                    ? new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.TopLevelExpose>>.Empty()
                    : new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.TopLevelExpose>>.NonEmpty(
                        First: firstNode,
                        Rest: restNodes);

                return new Node<SyntaxTypes.Exposing>(
                    MakeRange(keyword.Start, closeParen.End),
                    new SyntaxTypes.Exposing.Explicit(
                        OpenParenLocation: openParen.Start,
                        Nodes: nodesList,
                        CloseParenLocation: closeParen.Start));
            }
        }

        Node<SyntaxTypes.TopLevelExpose> ParseTopLevelExpose()
        {
            if (Peek.Type is TokenType.OpenParen)
            {
                var topLevelOpenParen = Consume(TokenType.OpenParen);

                var operatorToken = Consume(TokenType.Operator);

                var topLevelClosingParen = Consume(TokenType.CloseParen);

                return new Node<SyntaxTypes.TopLevelExpose>(
                    MakeRange(topLevelOpenParen.Start, topLevelClosingParen.End),
                    new SyntaxTypes.TopLevelExpose.InfixExpose(operatorToken.Lexeme));
            }

            if (Peek.Type is TokenType.Identifier)
            {
                var topLevelIdentifier = Consume(TokenType.Identifier);

                ConsumeAllTrivia();

                if (char.IsLower(topLevelIdentifier.Lexeme[0]))
                {
                    return new Node<SyntaxTypes.TopLevelExpose>(
                        MakeRange(topLevelIdentifier.Start, topLevelIdentifier.End),
                        new SyntaxTypes.TopLevelExpose.FunctionExpose(topLevelIdentifier.Lexeme));
                }

                if (Peek.Type is TokenType.OpenParen)
                {
                    var openParen = Consume(TokenType.OpenParen);

                    ConsumeAllTrivia();

                    var open = false;

                    if (Peek.Type is TokenType.DotDot)
                    {
                        Consume(TokenType.DotDot);

                        open = true;
                    }

                    ConsumeAllTrivia();

                    var closeParen = Consume(TokenType.CloseParen);

                    var openRange =
                        open
                        ?
                        MakeRange(openParen.Start, closeParen.End)
                        :
                        null;

                    return new Node<SyntaxTypes.TopLevelExpose>(
                        MakeRange(topLevelIdentifier.Start, closeParen.End),
                        new SyntaxTypes.TopLevelExpose.TypeExpose(
                            new SyntaxTypes.ExposedType(
                                topLevelIdentifier.Lexeme,
                                Open: openRange)));
                }

                return new Node<SyntaxTypes.TopLevelExpose>(
                    topLevelIdentifier.Range,
                    new SyntaxTypes.TopLevelExpose.TypeOrAliasExpose(
                        topLevelIdentifier.Lexeme));
            }

            throw ExceptionForCurrentLocation(
                "Unexpected token in exposing list: " + Peek.Type);
        }

        /// <summary>
        /// Parse next declaration (infix, type, function, etc.)
        /// </summary>
        private Node<SyntaxTypes.Declaration> ParseDeclaration(
            Token? docComment)
        {
            if (Peek.Lexeme is "infix")
            {
                var infixKeywordToken = ConsumeKeyword("infix");

                /*
                infix right 0 (<|) = apL
                infix left  0 (|>) = apR
                infix right 2 (||) = or
                 * */

                ConsumeAllTrivia();

                var infixDirectionToken = ConsumeAnyIdentifier("infix direction");

                var infixDirection =
                    infixDirectionToken.Lexeme switch
                    {
                        "left" =>
                        InfixDirection.Left,

                        "right" =>
                        InfixDirection.Right,

                        "non" =>
                        InfixDirection.Non,

                        _ =>
                        throw ExceptionForCurrentLocation(
                            "Infix direction is not a valid value: " +
                            infixDirectionToken.Lexeme),
                    };

                ConsumeAllTrivia();

                var precedenceToken = Consume(TokenType.NumberLiteral);

                if (!int.TryParse(precedenceToken.Lexeme, out var precedence))
                {
                    throw ExceptionForCurrentLocation(
                        "Infix precedence is not a number: " + precedenceToken.Lexeme);
                }

                ConsumeAllTrivia();

                var operatorOpenParen = Consume(TokenType.OpenParen);

                var operatorToken = Consume(TokenType.Operator);

                var operatorCloseParen = Consume(TokenType.CloseParen);

                ConsumeAllTrivia();

                var equalToken = Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var functionNameToken = ConsumeAnyIdentifier("function name");

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
                                    MakeRange(operatorOpenParen.Start, operatorCloseParen.End), operatorToken.Lexeme),
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

            return
                ParseFunctionDeclaration(docComment)
                .Cast<SyntaxTypes.Declaration>();
        }

        private Node<SyntaxTypes.Declaration> ParsePortDeclaration()
        {
            /*
             * Example syntax:
             * ----
             * port sendMessageToMonacoFrame : Json.Encode.Value -> Cmd msg
             * */

            var portKeywordToken = ConsumeKeyword("port");

            ConsumeAllTrivia();

            var portNameToken = ConsumeAnyIdentifier("port name");

            ConsumeAllTrivia();

            var colonToken = Consume(TokenType.Colon);

            ConsumeAllTrivia();

            var typeAnnotation = ParseTypeAnnotation(indentMin: 0);

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

        private Node<SyntaxTypes.Declaration> ParseTypeDeclaration(Token? docComment)
        {
            var typeKeywordToken = ConsumeKeyword("type");

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

                var aliasKeyword = ConsumeKeyword("alias");

                ConsumeAllTrivia();

                var typeAliasToken = ConsumeAnyIdentifier("type alias");

                ConsumeAllTrivia();

                var generics = new List<Node<string>>();

                while (Peek.Type is TokenType.Identifier)
                {
                    var genericToken = ConsumeAnyIdentifier("generic type parameter");

                    generics.Add(
                        new Node<string>(
                            genericToken.Range,
                            genericToken.Lexeme));

                    ConsumeAllTrivia();
                }

                var equalToken = Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var typeAliasTypeAnnotation =
                    ParseTypeAnnotation(indentMin: 0);

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

                var typeNameToken = ConsumeAnyIdentifier("type name");

                ConsumeAllTrivia();

                var typeParameters = new List<Node<string>>();

                // Type parameters are optional

                if (Peek.Type is TokenType.Identifier)
                {
                    if (Peek.Lexeme is "alias")
                    {
                        ConsumeKeyword("alias");

                        throw new NotImplementedException(
                            "Type alias not implemented.");
                    }

                    while (Peek.Type is TokenType.Identifier)
                    {
                        var typeParameterToken = ConsumeAnyIdentifier("type parameter");

                        typeParameters.Add(
                            new Node<string>(
                                MakeRange(typeParameterToken.Start, typeParameterToken.End),
                                typeParameterToken.Lexeme));

                        ConsumeAllTrivia();
                    }
                }

                var equalToken = Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var constructors = new List<(Location? PipeTokenLocation, Node<SyntaxTypes.ValueConstructor> Constructor)>();

                Location? pipeLocation = null;

                while (true)
                {
                    ConsumeAllTrivia();

                    var constructorNameToken = ConsumeAnyIdentifier("constructor name");

                    ConsumeAllTrivia();

                    var constructorArguments = new List<Node<SyntaxTypes.TypeAnnotation>>();

                    while (
                        NextTokenMatches(peek =>
                        constructorNameToken.Start.Column <= Peek.Start.Column &&
                        CanStartTypeAnnotation(peek)))
                    {
                        var argumentAnnotation =
                            ParseTypeAnnotationTypedArg(indentMin: constructorNameToken.Start.Column);

                        constructorArguments.Add(argumentAnnotation);

                        ConsumeAllTrivia();
                    }

                    var constructorEnd =
                        constructorArguments.Count is 0
                        ?
                        constructorNameToken.End
                        :
                        constructorArguments.Last().Range.End;

                    constructors.Add((
                        PipeTokenLocation: pipeLocation,
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
                        var pipeToken = Consume(TokenType.Pipe);
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
                        new SyntaxTypes.Declaration.CustomTypeDeclaration(
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


        private Node<SyntaxTypes.Declaration.FunctionDeclaration> ParseFunctionDeclaration(
            Token? docComment)
        {
            var functionFirstNameToken = ConsumeAnyIdentifier("function first identifier");

            var functionLastNameToken = functionFirstNameToken;

            ConsumeAllTrivia();

            Node<SyntaxTypes.Signature>? signature = null;
            Token? colonToken = null;

            if (Peek.Type is TokenType.Colon)
            {
                // Parse the optional signature (e.g. "toLower : String -> String")

                colonToken = Consume(TokenType.Colon);

                ConsumeAllTrivia();

                var signatureTypeAnnotation =
                    ParseTypeAnnotation(
                        indentMin: functionFirstNameToken.Start.Column);

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

                var declNameAgain =
                    ConsumeAnyIdentifier("function name");

                if (declNameAgain.Lexeme != functionFirstNameToken.Lexeme)
                {
                    throw ExceptionForCurrentLocation(
                        "Function name does not match signature: " +
                        declNameAgain.Lexeme + " != " + functionFirstNameToken.Lexeme);
                }

                functionLastNameToken = declNameAgain;

                ConsumeAllTrivia();
            }

            var arguments = new List<Node<SyntaxTypes.Pattern>>();

            while (NextTokenMatches(CanStartArgumentPattern))
            {
                var argument =
                    ParsePatternLessUncons(indentMin: functionFirstNameToken.Start.Column);

                ConsumeAllTrivia();

                arguments.Add(argument);
            }

            ConsumeAllTrivia();

            var equalToken = Consume(TokenType.Equal);

            ConsumeAllTrivia();

            var expression =
                ParseExpression(indentMin: functionFirstNameToken.Start.Column + 1);

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
                        MakeRange(functionLastNameToken.Start, expression.Range.End), functionImpl));

            var declaration =
                new SyntaxTypes.Declaration.FunctionDeclaration(functionStruct);

            var rangeStart =
                docComment is null
                ?
                functionFirstNameToken.Start
                :
                docComment.Start;

            return new Node<SyntaxTypes.Declaration.FunctionDeclaration>(
                MakeRange(rangeStart, expression.Range.End),
                declaration);
        }

        private Node<SyntaxTypes.TypeAnnotation> ParseTypeAnnotation(
            int indentMin)
        {
            var paramType =
                ParseTypeAnnotationFunctionParam(indentMin: indentMin);

            ConsumeAllTrivia();

            if (NextTokenMatches(peek => peek.Type is TokenType.Arrow))
            {
                var arrowToken = Consume(TokenType.Arrow);

                ConsumeAllTrivia();

                var returnType =
                    ParseTypeAnnotation(
                        paramType.Range.Start.Column);

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

        private Node<SyntaxTypes.TypeAnnotation> ParseTypeAnnotationFunctionParam(int indentMin)
        {
            var lessApplication = ParseTypeAnnotationTypedArg(indentMin);

            if (lessApplication.Value is SyntaxTypes.TypeAnnotation.Typed typedLessApp &&
                typedLessApp.TypeArguments.Count is 0)
            {
                ConsumeAllTrivia();

                var typeArguments =
                    new List<Node<SyntaxTypes.TypeAnnotation>>();

                while (
                    NextTokenMatches(peek =>
                    lessApplication.Range.Start.Column < peek.Start.Column &&
                    indentMin < peek.Start.Column &&
                    CanStartTypeAnnotation(peek)))
                {
                    var typeArgument =
                        ParseTypeAnnotationTypedArg(indentMin: indentMin);

                    typeArguments.Add(typeArgument);

                    ConsumeAllTrivia();
                }

                ConsumeAllTrivia();

                var range =
                    typeArguments.Count is 0
                    ?
                    lessApplication.Range
                    :
                    lessApplication.Range
                    with
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

        private Node<SyntaxTypes.TypeAnnotation> ParseTypeAnnotationTypedArg(int indentMin)
        {
            var start = Peek;

            if (start.Type is TokenType.OpenParen)
            {
                // Is either Tupled or Typed

                var openToken = Consume(TokenType.OpenParen);

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.CloseParen))
                {
                    var closeToken = Consume(TokenType.CloseParen);

                    return
                        new Node<SyntaxTypes.TypeAnnotation>(
                            MakeRange(openToken.Start, closeToken.End),
                            new SyntaxTypes.TypeAnnotation.Unit());
                }

                var firstTypeAnnotation =
                    ParseTypeAnnotation(indentMin: indentMin);

                ConsumeAllTrivia();

                if (Peek.Type is TokenType.Comma)
                {
                    // | Tupled (List (Node TypeAnnotation))

                    var firstCommaToken = Consume(TokenType.Comma);

                    ConsumeAllTrivia();

                    var restItems = new List<(Location SeparatorLocation, Node<SyntaxTypes.TypeAnnotation> Node)>();

                    while (true)
                    {
                        var typeAnnotation =
                            ParseTypeAnnotation(indentMin: indentMin);

                        restItems.Add((firstCommaToken.Start, typeAnnotation));

                        ConsumeAllTrivia();

                        if (Peek.Type is TokenType.Comma)
                        {
                            firstCommaToken = Consume(TokenType.Comma);
                            ConsumeAllTrivia();
                        }
                        else
                        {
                            break;
                        }
                    }

                    var closingToken = Consume(TokenType.CloseParen);

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
                    var closingToken = Consume(TokenType.CloseParen);

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
                var openToken = Consume(TokenType.OpenBrace);

                ConsumeAllTrivia();

                Node<SyntaxTypes.RecordField>? firstField = null;
                var restFields = new List<(Location SeparatorLocation, Node<SyntaxTypes.RecordField> Node)>();

                Token? genericName = null;
                Location? pipeTokenLocation = null;

                // Check if this is a generic record (e.g., { a | field : Type })
                if (NextTokenMatches(peek => peek.Type is TokenType.Identifier))
                {
                    var firstIdentifier = ConsumeAnyIdentifier("record field name or generic type parameter");
                    ConsumeAllTrivia();

                    if (NextTokenMatches(peek => peek.Type is TokenType.Pipe))
                    {
                        // Generic record
                        genericName = firstIdentifier;
                        var pipeToken = Consume(TokenType.Pipe);
                        pipeTokenLocation = pipeToken.Start;
                        ConsumeAllTrivia();
                    }
                    else if (NextTokenMatches(peek => peek.Type is TokenType.Colon))
                    {
                        // Regular record - first field
                        var colonToken = Consume(TokenType.Colon);
                        ConsumeAllTrivia();

                        var fieldTypeAnnotation =
                            ParseTypeAnnotation(
                                indentMin: firstIdentifier.Start.Column);

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
                            var commaToken = Consume(TokenType.Comma);
                            ConsumeAllTrivia();

                            var nextFieldNameToken = ConsumeAnyIdentifier("record field name");
                            ConsumeAllTrivia();

                            var nextColonToken = Consume(TokenType.Colon);
                            ConsumeAllTrivia();

                            var nextFieldTypeAnnotation =
                                ParseTypeAnnotation(
                                    indentMin: nextFieldNameToken.Start.Column);

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
                        var fieldNameToken = ConsumeAnyIdentifier("record field name");
                        ConsumeAllTrivia();

                        var colonToken = Consume(TokenType.Colon);
                        ConsumeAllTrivia();

                        var fieldTypeAnnotation =
                            ParseTypeAnnotation(
                                indentMin: fieldNameToken.Start.Column);

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
                            var commaToken = Consume(TokenType.Comma);
                            ConsumeAllTrivia();

                            var nextFieldNameToken = ConsumeAnyIdentifier("record field name");
                            ConsumeAllTrivia();

                            var nextColonToken = Consume(TokenType.Colon);
                            ConsumeAllTrivia();

                            var nextFieldTypeAnnotation =
                                ParseTypeAnnotation(
                                    indentMin: nextFieldNameToken.Start.Column);

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

                var closingToken = Consume(TokenType.CloseBrace);

                var range =
                    MakeRange(
                        openToken.Start,
                        closingToken.End);

                SeparatedSyntaxList<Node<SyntaxTypes.RecordField>> fieldsList =
                    firstField is null
                    ? new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.RecordField>>.Empty()
                    : new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.RecordField>>.NonEmpty(
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
                                        genericName.Range.End
                                        with
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
                var firstIdentifierToken =
                    ConsumeAnyIdentifier("first identifier");

                if (char.IsLower(firstIdentifierToken.Lexeme.First()))
                {
                    // GenericType String

                    return new Node<SyntaxTypes.TypeAnnotation>(
                        MakeRange(start.Start, start.End),
                        new SyntaxTypes.TypeAnnotation.GenericType(firstIdentifierToken.Lexeme));
                }

                // Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))

                // https://github.com/stil4m/elm-syntax/blob/c99a05ac96d3fa15fb3a8dc5ca39eaf78d1e510a/src/Elm/Parser/TypeAnnotation.elm#L336-L357

                var namespaces = new List<Token>();

                while (Peek.Type is TokenType.Dot)
                {
                    Consume(TokenType.Dot);

                    var namespaceToken = ConsumeAnyIdentifier("namespace item");

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
                                (
                                [.. moduleName.Select(t => t.Lexeme)],
                                typeNameToken.Lexeme)),
                            TypeArguments: []));
            }

            throw ExceptionForCurrentLocation(
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

        private Node<SyntaxTypes.Expression> ParseExpression(
            int indentMin,
            int minPrecedence = 0)
        {
            // Parse a primary expression first.
            var left = ParsePrimaryExpression(indentMin);

            ConsumeAllTrivia();

            // Loop while the next token is an infix operator.
            while (
                NextTokenMatches(peek =>
                peek.Type is TokenType.Operator &&
                InfixOperatorInfo.GetInfo(peek.Lexeme).Precedence >= minPrecedence))
            {
                var opToken = Consume(TokenType.Operator);

                var opInfo = InfixOperatorInfo.GetInfo(opToken.Lexeme);

                // Create a Node for the operator with its source range
                var operatorNode = new Node<string>(
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
                var right =
                    ParseExpression(
                        indentMin: indentMin,
                        minPrecedence: nextMinPrecedence);

                // Combine the left and right expressions into an OperatorApplication node.
                left =
                    new Node<SyntaxTypes.Expression>(
                        MakeRange(left.Range.Start, right.Range.End),
                        new SyntaxTypes.Expression.OperatorApplication(operatorNode, opInfo.Direction, left, right));

                ConsumeAllTrivia();
            }

            return left;
        }

        private Node<SyntaxTypes.Expression> ParsePrimaryExpression(
            int indentMin)
        {
            var functionExpr =
                ParseBasicPrimaryExpression(indentMin);

            ConsumeAllTrivia();

            /*
             * Following expressions are arguments if indented at least as much as the first identifier
             * and not matching any keyword.
             * */

            var argumentsNodes =
                new List<Node<SyntaxTypes.Expression>>();

            while (
                NextTokenMatches(peek =>
                indentMin < peek.Start.Column &&
                CanStartArgumentExpression(peek)))
            {
                var argumentExpr = ParseBasicPrimaryExpression(indentMin);

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
                    new SyntaxTypes.Expression.Application([functionExpr, .. argumentsNodes]);

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

        private Node<SyntaxTypes.Expression> ParseBasicPrimaryExpression(
            int indentMin)
        {
            var lessRecordAccess =
                ParseBasicPrimaryExpressionLessRecordAccess(indentMin: indentMin);

            if (NextTokenMatches(peek => peek.Type is TokenType.Dot))
            {
                // | RecordAccess (Node Expression) (Node String)

                var lastRecordAccess = lessRecordAccess;

                while (NextTokenMatches(peek => peek.Type is TokenType.Dot))
                {
                    Consume(TokenType.Dot);

                    var recordFieldToken =
                        ConsumeAnyIdentifier("record field name");

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

        private Node<SyntaxTypes.Expression> ParseBasicPrimaryExpressionLessRecordAccess(
            int indentMin)
        {
            var start = Peek;

            if (start.Type is TokenType.StringLiteral)
            {
                var stringLiteral =
                    Consume(TokenType.StringLiteral);

                var literalExpr = new SyntaxTypes.Expression.Literal(stringLiteral.Lexeme, IsTripleQuoted: false);

                return new Node<SyntaxTypes.Expression>(stringLiteral.Range, literalExpr);
            }

            if (start.Type is TokenType.TripleQuotedStringLiteral)
            {
                var stringLiteral =
                    Consume(TokenType.TripleQuotedStringLiteral);

                var literalExpr = new SyntaxTypes.Expression.Literal(stringLiteral.Lexeme, IsTripleQuoted: true);

                return new Node<SyntaxTypes.Expression>(stringLiteral.Range, literalExpr);
            }

            if (start.Type is TokenType.CharLiteral)
            {
                var charToken = Consume(TokenType.CharLiteral);

                var literalExpr =
                    new SyntaxTypes.Expression.CharLiteral(char.ConvertToUtf32(charToken.Lexeme, 0));

                return new Node<SyntaxTypes.Expression>(charToken.Range, literalExpr);
            }

            if (start.Type is TokenType.OpenBrace)
            {
                return ParseRecordExpr(indentMin);
            }

            if (start.Type is TokenType.Identifier)
            {
                var firstIdentifierToken = ConsumeAnyIdentifier("first identifier");

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

                    while (
                        NextTokenMatches(
                            peek =>
                            letDeclIndentCheck < peek.Start.Column &&
                            peek.Lexeme is not "in"))
                    {
                        var letDecl =
                            ParseLetDeclaration(indentMin: letDeclIndentCheck + 1);

                        letDecls.Add(letDecl);

                        ConsumeAllTrivia();
                    }

                    var letInToken = ConsumeKeyword("in");

                    ConsumeAllTrivia();

                    var letInExpr = ParseExpression(letInToken.Start.Column);

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
                    ConsumeAllTrivia();

                    var condition =
                        ParseExpression(indentMin: firstIdentifierToken.Range.Start.Column);

                    ConsumeAllTrivia();

                    var thenToken = ConsumeKeyword("then");

                    ConsumeAllTrivia();

                    var thenBranch =
                        ParseExpression(indentMin: firstIdentifierToken.Range.Start.Column);

                    ConsumeAllTrivia();

                    var elseToken = ConsumeKeyword("else");

                    ConsumeAllTrivia();

                    var elseBranch =
                        ParseExpression(indentMin: firstIdentifierToken.Range.Start.Column);

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

                    var caseValue =
                        ParseExpression(indentMin: firstIdentifierToken.Range.Start.Column);

                    ConsumeAllTrivia();

                    var caseOfToken = ConsumeKeyword("of");

                    ConsumeAllTrivia();

                    var casesIndentMin = Peek.Start.Column;
                    // For robustness with inconsistently indented case arms, we also accept
                    // arms that are strictly indented more than the 'case' keyword itself.
                    // This handles malformed input like:
                    //     case x of
                    //          Just y -> ...   (column 10)
                    //         Nothing -> ...   (column 9, still > case keyword column)
                    var caseKeywordColumn = firstIdentifierToken.Range.Start.Column;

                    var caseBranches = new List<SyntaxTypes.Case>();

                    while (
                        !IsAtEnd() &&
                        (casesIndentMin <= Peek.Start.Column || caseKeywordColumn < Peek.Start.Column) &&
                        Peek.Type is not TokenType.Comma &&
                        Peek.Type is not TokenType.CloseParen &&
                        Peek.Type is not TokenType.CloseBracket &&
                        Peek.Type is not TokenType.CloseBrace)
                    {
                        var caseBranch = ParseCaseBranch(casesIndentMin);

                        caseBranches.Add(caseBranch.Value);

                        ConsumeAllTrivia();
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
                while (
                    char.IsUpper(identifiers.Last().Lexeme[0]) &&
                    NextTokenMatches(peek => peek.Type is TokenType.Dot))
                {
                    Consume(TokenType.Dot);

                    var furtherNamePart =
                        ConsumeAnyIdentifier("function or value name part");

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

                var listOpenToken =
                    Consume(TokenType.OpenBracket);

                ConsumeAllTrivia();

                Node<SyntaxTypes.Expression>? firstElement = null;
                var restElements = new List<(Location SeparatorLocation, Node<SyntaxTypes.Expression> Node)>();

                if (Peek.Type is not TokenType.CloseBracket)
                {
                    // Parse first element
                    firstElement = ParseExpression(indentMin);
                    ConsumeAllTrivia();

                    // Parse remaining elements (each preceded by comma)
                    while (Peek.Type is TokenType.Comma)
                    {
                        var commaToken = Consume(TokenType.Comma);
                        ConsumeAllTrivia();

                        var nextElement = ParseExpression(indentMin);
                        restElements.Add((commaToken.Start, nextElement));
                        ConsumeAllTrivia();
                    }
                }

                var listCloseToken =
                    Consume(TokenType.CloseBracket);

                var listRange =
                    MakeRange(listOpenToken.Start, listCloseToken.End);

                SeparatedSyntaxList<Node<SyntaxTypes.Expression>> elements =
                    firstElement is null
                    ? new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Expression>>.Empty()
                    : new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Expression>>.NonEmpty(
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

                var parenOpenToken =
                    Consume(TokenType.OpenParen);

                var nextTwoTokens =
                    EnumerateFollowingTokens().Take(2).ToArray();

                if (nextTwoTokens.Length is 2)
                {
                    if (nextTwoTokens[0].Type is TokenType.Operator &&
                        nextTwoTokens[1].Type is TokenType.CloseParen)
                    {
                        var operatorToken = Consume(TokenType.Operator);

                        var parenCloseToken =
                            Consume(TokenType.CloseParen);

                        return
                            new Node<SyntaxTypes.Expression>(
                                MakeRange(parenOpenToken.Start, parenCloseToken.End),
                                new SyntaxTypes.Expression.PrefixOperator(operatorToken.Lexeme));

                    }
                }

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.CloseParen))
                {
                    var parenCloseToken =
                        Consume(TokenType.CloseParen);

                    var parenRange =
                        MakeRange(parenOpenToken.Start, parenCloseToken.End);

                    return
                        new Node<SyntaxTypes.Expression>(
                            parenRange,
                            new SyntaxTypes.Expression.UnitExpr());
                }

                var firstItemExpr = ParseExpression(indentMin);

                ConsumeAllTrivia();

                var furtherItems = new List<(Location SeparatorLocation, Node<SyntaxTypes.Expression> Node)>();

                while (Peek.Type is TokenType.Comma)
                {
                    var commaToken = Consume(TokenType.Comma);

                    ConsumeAllTrivia();

                    var furtherItemExpr = ParseExpression(indentMin);

                    furtherItems.Add((commaToken.Start, furtherItemExpr));

                    ConsumeAllTrivia();
                }

                {
                    var parenCloseToken =
                        Consume(TokenType.CloseParen);

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
                Consume(TokenType.NumberLiteral);

                return
                    new Node<SyntaxTypes.Expression>(
                        start.Range,
                        ParseNumber(start.Lexeme));
            }

            if (start.Type is TokenType.GLSLLiteral)
            {
                var glslToken = Consume(TokenType.GLSLLiteral);

                return
                    new Node<SyntaxTypes.Expression>(
                        glslToken.Range,
                        new SyntaxTypes.Expression.GLSLExpression(glslToken.Lexeme));
            }

            if (start.Type is TokenType.Negation)
            {
                var negationToken = Consume(TokenType.Negation);

                ConsumeAllTrivia();

                var negatedExpr = ParseBasicPrimaryExpression(indentMin);

                var negationRange =
                    MakeRange(negationToken.Start, negatedExpr.Range.End);

                return
                    new Node<SyntaxTypes.Expression>(
                        negationRange,
                        new SyntaxTypes.Expression.Negation(negatedExpr));
            }

            if (start.Type is TokenType.Lambda)
            {
                var lambdaNode = ParseLambdaExpression(indentMin);

                return new Node<SyntaxTypes.Expression>(lambdaNode.Range, lambdaNode.Value);
            }

            if (start.Type is TokenType.Dot)
            {
                // | RecordAccessFunction String

                var dotToken = Consume(TokenType.Dot);

                var recordFieldToken =
                    ConsumeAnyIdentifier("record field name");

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

            throw ExceptionForCurrentLocation(
                "Unsupported token type in expression: " + start.Type +
                " at " + start.Start.Row + ":" + start.Start.Column +
                " - " + start.End.Row + ":" + start.End.Column +
                " - " + start.Lexeme);
        }

        private Node<SyntaxTypes.Expression.LambdaExpression> ParseLambdaExpression(int indentMin)
        {
            // | Lambda (Node Expression)

            var lambdaToken = Consume(TokenType.Lambda);

            ConsumeAllTrivia();

            /*
             * Example:
             * 
             * (\type_arg -> json_encode_Bytes type_arg)
             * */

            var arguments = new List<Node<SyntaxTypes.Pattern>>();

            while (NextTokenMatches(CanStartArgumentPattern))
            {
                var argument =
                    ParsePatternLessUncons(indentMin: indentMin);

                ConsumeAllTrivia();

                arguments.Add(argument);
            }

            ConsumeAllTrivia();

            var arrowToken = Consume(TokenType.Arrow);

            ConsumeAllTrivia();

            var expression = ParseExpression(indentMin);

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

        private Node<SyntaxTypes.Expression.LetDeclaration> ParseLetDeclaration(int indentMin)
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

                var parsedDecl = ParseFunctionDeclaration(docComment: null);

                return
                    new Node<SyntaxTypes.Expression.LetDeclaration>(
                        parsedDecl.Range,
                        new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                            parsedDecl.Value.Function));
            }

            {
                // LetDestructuring

                var pattern =
                    ParsePatternLessUncons(indentMin: indentMin);

                ConsumeAllTrivia();

                var equalToken = Consume(TokenType.Equal);

                ConsumeAllTrivia();

                var expression =
                    ParseExpression(indentMin: pattern.Range.Start.Column + 1);

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
                return new SyntaxTypes.Expression.Floatable(expression);
            }

            // Decimal integer - preserve the original literal string
            return new SyntaxTypes.Expression.Integer(expression);
        }

        private Node<SyntaxTypes.Case> ParseCaseBranch(int indentMin)
        {
            /*
             * Example:
             * 
                Nothing ->
                    [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object
             */

            var pattern = ParsePattern(indentMin);

            ConsumeAllTrivia();

            var arrowToken = Consume(TokenType.Arrow);

            ConsumeAllTrivia();

            var expression = ParseExpression(indentMin);

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

        private Node<SyntaxTypes.Pattern> ParsePattern(
            int indentMin)
        {
            var lessUncons = ParsePatternLessUncons(indentMin: indentMin);

            ConsumeAllTrivia();

            if (NextTokenMatches(peek => peek.Lexeme is "::"))
            {
                // | UnConsPattern (Node Pattern) (Node Pattern)

                var unconsSymbol = Consume(TokenType.Operator);

                ConsumeAllTrivia();

                var tailPattern = ParsePattern(indentMin: unconsSymbol.End.Column);

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

                var asToken = ConsumeKeyword("as");

                ConsumeAllTrivia();

                var nameToken = ConsumeAnyIdentifier("pattern name");

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

            if (lessUncons.Value is SyntaxTypes.Pattern.NamedPattern namedLeft &&
                namedLeft.Arguments.Count is 0 &&
                NextTokenMatches(CanStartPattern))
            {
                ConsumeAllTrivia();

                var patternArguments = new List<Node<SyntaxTypes.Pattern>>();

                while (
                    NextTokenMatches(peek =>
                    lessUncons.Range.Start.Column <= peek.Start.Column &&
                    CanStartPattern(peek)))
                {
                    var patternArgument = ParsePatternLessUncons(indentMin);

                    patternArguments.Add(patternArgument);

                    ConsumeAllTrivia();
                }

                if (patternArguments.Count is 0)
                {
                    return lessUncons;
                }

                var patternRangeEnd =
                    patternArguments.Last().Range.End;

                var patternRange =
                    lessUncons.Range
                    with
                    {
                        End = patternRangeEnd
                    };

                var namedPattern =
                    new SyntaxTypes.Pattern.NamedPattern(
                        namedLeft.Name,
                        patternArguments);

                return
                    new Node<SyntaxTypes.Pattern>(
                        patternRange,
                        namedPattern);
            }

            return lessUncons;
        }

        private Node<SyntaxTypes.Pattern> ParsePatternLessUncons(
            int indentMin)
        {
            var start = Peek;

            if (start.Type is TokenType.Identifier)
            {
                var identifierToken = ConsumeAnyIdentifier("pattern identifier");

                if (identifierToken.Lexeme is "_")
                {
                    // | AllPattern

                    return new Node<SyntaxTypes.Pattern>(
                        identifierToken.Range,
                        new SyntaxTypes.Pattern.AllPattern());
                }

                if (char.IsLower(identifierToken.Lexeme[0]))
                {
                    // | VarPattern String

                    return new Node<SyntaxTypes.Pattern>(
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
                    Consume(TokenType.Dot);

                    var namespaceToken = ConsumeAnyIdentifier("namespace item");

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

                var parenOpenToken = Consume(TokenType.OpenParen);

                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.CloseParen))
                {
                    var parenCloseToken =
                        Consume(TokenType.CloseParen);

                    var parenRange =
                        MakeRange(parenOpenToken.Start, parenCloseToken.End);

                    return
                        new Node<SyntaxTypes.Pattern>(
                            parenRange,
                            new SyntaxTypes.Pattern.UnitPattern());
                }

                {
                    var firstPattern = ParsePattern(indentMin);

                    ConsumeAllTrivia();

                    var furtherPatternsWithCommas = new List<(Location CommaLocation, Node<SyntaxTypes.Pattern> Pattern)>();

                    while (Peek.Type is TokenType.Comma)
                    {
                        var commaToken = Consume(TokenType.Comma);

                        ConsumeAllTrivia();

                        var furtherPattern = ParsePattern(indentMin);

                        furtherPatternsWithCommas.Add((commaToken.Start, furtherPattern));

                        ConsumeAllTrivia();
                    }

                    var parenCloseToken =
                        Consume(TokenType.CloseParen);

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

                var literalToken = Consume(start.Type);

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

                var literalToken = Consume(TokenType.CharLiteral);

                var charPattern =
                    new SyntaxTypes.Pattern.CharPattern(char.ConvertToUtf32(literalToken.Lexeme, 0));

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

                    var literalToken = Consume(TokenType.NumberLiteral);

                    var hexPattern =
                        new SyntaxTypes.Pattern.HexPattern(
                            long.Parse(literalToken.Lexeme[2..], System.Globalization.NumberStyles.HexNumber));

                    return
                        new Node<SyntaxTypes.Pattern>(
                            MakeRange(start.Start, literalToken.End),
                            hexPattern);
                }

                // | IntegerPattern Int
                if (long.TryParse(start.Lexeme, out var number))
                {
                    var literalToken = Consume(TokenType.NumberLiteral);

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

                var listOpenToken =
                    Consume(TokenType.OpenBracket);

                ConsumeAllTrivia();

                Node<SyntaxTypes.Pattern>? firstItem = null;
                var restItems = new List<(Location CommaLocation, Node<SyntaxTypes.Pattern> Pattern)>();

                // Parse first item if any
                if (NextTokenMatches(peek => peek.Type is not TokenType.CloseBracket))
                {
                    firstItem = ParsePattern(indentMin);
                    ConsumeAllTrivia();

                    // Parse remaining items
                    while (Peek.Type is TokenType.Comma)
                    {
                        var commaToken = Consume(TokenType.Comma);
                        ConsumeAllTrivia();

                        if (NextTokenMatches(peek => peek.Type is not TokenType.CloseBracket))
                        {
                            var nextPattern = ParsePattern(indentMin);
                            restItems.Add((commaToken.Start, nextPattern));
                            ConsumeAllTrivia();
                        }
                    }
                }

                var listCloseToken =
                    Consume(TokenType.CloseBracket);

                var listRange =
                    MakeRange(listOpenToken.Start, listCloseToken.End);

                SeparatedSyntaxList<Node<SyntaxTypes.Pattern>> elementsList =
                    firstItem is null
                        ? new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Pattern>>.Empty()
                        : new SyntaxTypes.SeparatedSyntaxList<Node<SyntaxTypes.Pattern>>.NonEmpty(
                            firstItem, restItems);

                var listPatternValue =
                    new SyntaxTypes.Pattern.ListPattern(
                        Elements: elementsList);

                return new Node<SyntaxTypes.Pattern>(listRange, listPatternValue);
            }

            if (start.Type is TokenType.OpenBrace)
            {
                // | RecordPattern (List (Node String))

                var recordOpenToken = Consume(TokenType.OpenBrace);

                ConsumeAllTrivia();

                Node<string>? firstField = null;
                var restFields = new List<(Location CommaLocation, Node<string> FieldName)>();

                // Parse first field if any
                if (Peek.Type is not TokenType.CloseBrace)
                {
                    var fieldName = ConsumeAnyIdentifier("field name");
                    firstField = new Node<string>(fieldName.Range, fieldName.Lexeme);
                    ConsumeAllTrivia();

                    // Parse remaining fields
                    while (Peek.Type is TokenType.Comma)
                    {
                        var commaToken = Consume(TokenType.Comma);
                        ConsumeAllTrivia();

                        if (Peek.Type is not TokenType.CloseBrace)
                        {
                            var nextFieldName = ConsumeAnyIdentifier("field name");
                            var nextFieldNode = new Node<string>(nextFieldName.Range, nextFieldName.Lexeme);
                            restFields.Add((commaToken.Start, nextFieldNode));
                            ConsumeAllTrivia();
                        }
                    }
                }

                var recordCloseToken = Consume(TokenType.CloseBrace);

                var recordRange =
                    MakeRange(recordOpenToken.Start, recordCloseToken.End);

                SeparatedSyntaxList<Node<string>> fieldsList =
                    firstField is null
                        ? new SyntaxTypes.SeparatedSyntaxList<Node<string>>.Empty()
                        : new SyntaxTypes.SeparatedSyntaxList<Node<string>>.NonEmpty(
                            firstField, restFields);

                var recordPattern =
                    new SyntaxTypes.Pattern.RecordPattern(
                        Fields: fieldsList);

                return new Node<SyntaxTypes.Pattern>(recordRange, recordPattern);
            }

            throw ExceptionForCurrentLocation(
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

        private Node<SyntaxTypes.Expression> ParseRecordExpr(
            int indentMin)
        {
            var start = Peek;

            var openBrace = Consume(TokenType.OpenBrace);

            ConsumeAllTrivia();

            Token? updatedRecord = null;
            Location pipeLocation = default;

            // Check for record update syntax: { name | field = value }
            if (Peek.Type is TokenType.Identifier)
            {
                var nameToken = ConsumeAnyIdentifier("record name or field name");
                ConsumeAllTrivia();

                if (NextTokenMatches(peek => peek.Type is TokenType.Pipe))
                {
                    // Record update expression
                    updatedRecord = nameToken;
                    var pipeToken = Consume(TokenType.Pipe);
                    pipeLocation = pipeToken.Start;
                    ConsumeAllTrivia();
                }
                else if (NextTokenMatches(peek => peek.Type is TokenType.Equal))
                {
                    // Regular record, first field parsed
                    // Put the identifier back - but we already consumed it, so we need to parse the field now
                    var equalsToken = Consume(TokenType.Equal);
                    ConsumeAllTrivia();

                    var valueExpr = ParseExpression(indentMin);
                    ConsumeAllTrivia();

                    var firstFieldRangeEnd = valueExpr.Range.End;

                    var recordFirstField = new RecordExprField(
                        new Node<string>(nameToken.Range, nameToken.Lexeme),
                        equalsToken.Start,
                        valueExpr);

                    var recordRestFields = new List<(Location SeparatorLocation, RecordExprField Node)>();

                    // Parse remaining fields
                    while (Peek.Type is TokenType.Comma)
                    {
                        var commaToken = Consume(TokenType.Comma);
                        ConsumeAllTrivia();

                        var nextFieldName = ConsumeAnyIdentifier("field name");
                        ConsumeAllTrivia();

                        var nextEqualsToken = Consume(TokenType.Equal);
                        ConsumeAllTrivia();

                        var nextValueExpr = ParseExpression(indentMin);
                        ConsumeAllTrivia();

                        var nextField = new RecordExprField(
                            new Node<string>(nextFieldName.Range, nextFieldName.Lexeme),
                            nextEqualsToken.Start,
                            nextValueExpr);

                        recordRestFields.Add((commaToken.Start, nextField));
                    }

                    var closeBrace = Consume(TokenType.CloseBrace);
                    var range = MakeRange(start.Start, closeBrace.End);

                    return new Node<SyntaxTypes.Expression>(
                        range,
                        new SyntaxTypes.Expression.RecordExpr(
                            Fields: new SeparatedSyntaxList<RecordExprField>.NonEmpty(
                                First: recordFirstField,
                                Rest: recordRestFields)));
                }
                else if (NextTokenMatches(peek => peek.Type is TokenType.CloseBrace))
                {
                    // Empty record or just a name followed by close brace? This shouldn't happen in valid Elm
                    var closeBrace = Consume(TokenType.CloseBrace);
                    var range = MakeRange(start.Start, closeBrace.End);

                    return new Node<SyntaxTypes.Expression>(
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
                var fieldName = ConsumeAnyIdentifier("field name");
                ConsumeAllTrivia();

                var equalsToken = Consume(TokenType.Equal);
                ConsumeAllTrivia();

                var valueExpr = ParseExpression(indentMin);
                ConsumeAllTrivia();

                firstField = new RecordExprField(
                    new Node<string>(fieldName.Range, fieldName.Lexeme),
                    equalsToken.Start,
                    valueExpr);

                // Parse remaining fields
                while (Peek.Type is TokenType.Comma)
                {
                    var commaToken = Consume(TokenType.Comma);
                    ConsumeAllTrivia();

                    var nextFieldName = ConsumeAnyIdentifier("field name");
                    ConsumeAllTrivia();

                    var nextEqualsToken = Consume(TokenType.Equal);
                    ConsumeAllTrivia();

                    var nextValueExpr = ParseExpression(indentMin);
                    ConsumeAllTrivia();

                    var nextField = new RecordExprField(
                        new Node<string>(nextFieldName.Range, nextFieldName.Lexeme),
                        nextEqualsToken.Start,
                        nextValueExpr);

                    restFieldsUpdate.Add((commaToken.Start, nextField));
                }
            }

            var closeBraceUpdate = Consume(TokenType.CloseBrace);
            var rangeUpdate = MakeRange(start.Start, closeBraceUpdate.End);

            SeparatedSyntaxList<RecordExprField> fieldsList =
                firstField is null
                ? new SyntaxTypes.SeparatedSyntaxList<SyntaxTypes.RecordExprField>.Empty()
                : new SyntaxTypes.SeparatedSyntaxList<SyntaxTypes.RecordExprField>.NonEmpty(
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

            return new Node<SyntaxTypes.Expression>(
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

        private Token Peek =>
            tokens.Span[_current];

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

        private Token ConsumeKeyword(string expectedLexeme)
        {
            return
                Consume(TokenType.Identifier, expectedLexeme);
        }

        private Token ConsumeAnyIdentifier(string description)
        {
            return
                Consume(
                    TokenType.Identifier,
                    expectedLexeme: null,
                    tokenDescription: description);
        }

        // Consume a token of a given type, throwing an error if the token does not match.
        private Token Consume(
            TokenType expectedType,
            string? expectedLexeme = null,
            string? tokenDescription = null)
        {
            var nextToken = Peek;

            if (nextToken.Type != expectedType)
            {
                throw new ParserException(
                    "Expected " + (tokenDescription ?? "token") + " of type " + expectedType +
                    " but found " + nextToken.Type,
                    lineNumber: nextToken.Start.Row,
                    columnNumber: nextToken.Start.Column);
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

                throw new ParserException(
                    errorDescription,
                    lineNumber: nextToken.Start.Row,
                    columnNumber: nextToken.Start.Column);
            }

            return Advance();
        }

        private ParserException ExceptionForCurrentLocation(string message)
        {
            var token =
            IsAtEnd() ?
            tokens.Span[tokens.Length - 1] :
            Peek;

            return new ParserException(
            message,
            lineNumber: token.Start.Row,
            columnNumber: token.Start.Column);
        }

        private IReadOnlyList<Token> ConsumeAllTrivia()
        {
            return
            [.. ConsumeWhileLazy(
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
