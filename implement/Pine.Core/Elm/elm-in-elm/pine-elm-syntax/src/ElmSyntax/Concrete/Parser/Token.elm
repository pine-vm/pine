module ElmSyntax.Concrete.Parser.Token exposing (..)

import ElmSyntax.Concrete.Range exposing (Location, Range)



{-

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
-}


type alias Token =
    { tokenType : TokenType
    , lexeme : String
    , start : Location
    , end : Location
    , rawText : Maybe String
    }


type TokenType
    = Identifier
    | StringLiteral
    | TripleQuotedStringLiteral
    | CharLiteral
    | NumberLiteral
    | GLSLLiteral
    | OpenParen
    | CloseParen
    | OpenBrace
    | CloseBrace
    | OpenBracket
    | CloseBracket
    | Comma
    | Dot
    | DotDot
    | Equal
    | Arrow
    | Colon
    | Pipe
    | Comment
    | Whitespace
    | Newline
    | Lambda
    | Operator
    | Negation
    | Unknown
    | EndOfFile
