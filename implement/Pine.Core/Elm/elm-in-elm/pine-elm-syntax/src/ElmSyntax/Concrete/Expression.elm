module ElmSyntax.Concrete.Expression exposing (..)

import ElmSyntax.Concrete.Infix exposing (InfixDirection)
import ElmSyntax.Concrete.Node exposing (Node)
import ElmSyntax.Concrete.Pattern exposing (Pattern)
import ElmSyntax.Concrete.Range exposing (Location, Range)
import ElmSyntax.Concrete.SeparatedSyntaxList exposing (SeparatedSyntaxList)
import ElmSyntax.Concrete.TypeAnnotation exposing (TypeAnnotation)



{-

/// <summary>
/// Elm expressions: literals, applications, control flow, data structures, lambda, let, case, etc.
/// </summary>
public abstract record Expression
{
    /// <summary>Expression producing unit.</summary>
    public sealed record UnitExpr
        : Expression;

    /// <summary>
    /// Simple (single-quoted) string literal expression.
    /// Triple-quoted multiline string literals are represented by <see cref="MultilineStringLiteral"/>.
    /// </summary>
    public sealed record StringLiteral(
        string Value,

        /*
         * The original source text of the string literal content (the characters between the opening
         * and closing quotes, excluding the quotes themselves). Escape sequences such as "\n", "\t"
         * or "\u{000A}" are preserved verbatim. This enables the renderer to reproduce the varied
         * character representations found in the source instead of re-encoding the (decoded)
         * <see cref="Value"/> into a canonical form.
         * It is null for literals that were not produced from source text (for example synthesized
         * during lowering), in which case the renderer falls back to encoding <see cref="Value"/>.
         * */
        string? SourceText = null)
        : Expression;

    /// <summary>
    /// Triple-quoted (multiline) string literal expression.
    /// </summary>
    /// <remarks>
    /// In contrast to <see cref="StringLiteral"/>, the original source representation is modelled as an
    /// immutable array of lines (<see cref="SourceLines"/>) rather than a single string. The lines
    /// are the original source text content (between the opening and closing <c>"""</c> delimiters)
    /// split on physical line endings, with escape sequences preserved verbatim. Storing the content
    /// already split into lines keeps rendering simple: each line is emitted as-is and the configured
    /// linebreak style is inserted between lines, avoiding a character-by-character scan.
    /// <see cref="SourceLines"/> is null for literals that were not produced from source text (for
    /// example synthesized during lowering), in which case the renderer falls back to encoding the
    /// decoded <see cref="Value"/>.
    /// </remarks>
    public sealed record MultilineStringLiteral(
        string Value,
        IReadOnlyList<string>? SourceLines = null)
        : Expression, System.IEquatable<MultilineStringLiteral>
    {
        /// <inheritdoc/>
        public bool Equals(MultilineStringLiteral? other) =>
            other is not null &&
            Value == other.Value &&
            (SourceLines is null
            ?
            other.SourceLines is null
            :
            other.SourceLines is not null &&
            Enumerable.SequenceEqual(SourceLines, other.SourceLines));

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hash = new System.HashCode();

            hash.Add(Value);

            if (SourceLines is not null)
            {
                foreach (var line in SourceLines)
                {
                    hash.Add(line);
                }
            }

            return hash.ToHashCode();
        }
    }

    /// <summary>Character literal expression.</summary>
    public sealed record CharLiteral(
        int Value)
        : Expression;

    /// <summary>Integer literal expression (decimal or hexadecimal).</summary>
    /// <remarks>
    /// The original literal string is preserved to enable exact roundtripping of source code.
    /// The string may represent a decimal (e.g., "42", "-123") or hexadecimal (e.g., "0xFF", "-0x1A") literal.
    /// </remarks>
    public sealed record IntegerLiteral(
        string LiteralText)
        : Expression;

    /// <summary>Floating-point literal expression.</summary>
    /// <remarks>
    /// The original literal string is preserved to enable exact roundtripping of source code.
    /// Use <see cref="FloatLiteralConversion.ToElmFloat(string)"/> to convert to the numeric representation.
    /// </remarks>
    public sealed record FloatLiteral(
        string LiteralText)
        : Expression;

    /// <summary>Arithmetic negation of an expression.</summary>
    public sealed record Negation(
        Node<Expression> Expression)
        : Expression;

    /// <summary>List literal expression.</summary>
    public sealed record ListExpr(
        SeparatedSyntaxList<Node<Expression>> Elements)
        : Expression;

    /// <summary>Reference to a function or value.</summary>
    public sealed record Identifier(
        ModuleName ModuleName,
        string Name)
        : Expression, System.IEquatable<Identifier>
    {
        /// <inheritdoc/>
        public bool Equals(Identifier? other) =>
            other is not null &&
            Name == other.Name &&
            Enumerable.SequenceEqual(ModuleName, other.ModuleName);

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hash = new System.HashCode();

            foreach (var part in ModuleName)
            {
                hash.Add(part);
            }

            hash.Add(Name);

            return hash.ToHashCode();
        }

        /// <summary>
        /// Creates a new instance of <see cref="Identifier"/> from a fully qualified name, splitting the name into
        /// module and value components.
        /// </summary>
        public static Identifier FromFullName(string fullName)
        {
            var parts = fullName.Split('.');

            if (parts.Length is 0)
                throw new System.ArgumentException("Full name cannot be empty", nameof(fullName));

            var value = parts[^1];

            var moduleName =
                parts.Length > 1
                ?
                parts[..^1]
                :
                [];

            return new Identifier(moduleName, value);
        }
    }

    /// <summary>Conditional expression with then/else branches.</summary>
    public sealed record IfBlock(
        Location IfTokenLocation,
        Node<Expression> Condition,
        Location ThenTokenLocation,
        Node<Expression> ThenBlock,
        Location ElseTokenLocation,
        Node<Expression> ElseBlock)
        : Expression;

    /// <summary>Prefix operator expression.</summary>
    public sealed record PrefixOperator(
        string Operator)
        : Expression;

    /// <summary>Parenthesized subexpression.</summary>
    public sealed record Parenthesized(
        Node<Expression> Expression)
        : Expression;

    /// <summary>Function application expression.</summary>
    public sealed record Application(
        Node<Expression> Function,
        IReadOnlyList<Node<Expression>> Arguments)
        : Expression
    {
        /*
         * Aligned with stil4m/elm-syntax v8: Use dedicated property for the 'Function' part:
         * + https://github.com/stil4m/elm-syntax/issues/43
         * + https://github.com/stil4m/elm-syntax/pull/48
         * + https://github.com/stil4m/elm-syntax/blob/4268c7d850577f35ae0975862e376c7365b3064c/src/Elm/Syntax/Expression.elm#L112
         * */

        /// <inheritdoc/>
        public bool Equals(Application? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                EqualityComparer<Node<Expression>>.Default.Equals(Function, other.Function) &&
                Enumerable.SequenceEqual(Arguments, other.Arguments);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(Function);

            foreach (var item in Arguments)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>Operator application with explicit direction.</summary>
    public sealed record OperatorApplication(
        Node<string> Operator,
        InfixDirection Direction,
        Node<Expression> Left,
        Node<Expression> Right)
        : Expression;

    /// <summary>Tuple literal expression.</summary>
    public sealed record TupledExpression(
        SeparatedSyntaxList<Node<Expression>> Elements)
        : Expression;

    /// <summary>Lambda expression.</summary>
    public sealed record LambdaExpression(
        LambdaStruct Lambda)
        : Expression;

    /// <summary>Case expression matching an expression over patterns.</summary>
    public sealed record CaseExpression(
        CaseBlock CaseBlock)
        : Expression;

    /// <summary>Let expression introducing local declarations.</summary>
    public sealed record LetExpression(
        LetBlock Value)
        : Expression;

    /// <summary>
    /// Let block containing declarations and final expression.
    /// </summary>
    public sealed record LetBlock(
        Location LetTokenLocation,
        IReadOnlyList<Node<LetDeclaration>> Declarations,
        Location InTokenLocation,
        Node<Expression> Expression)
    {
        /// <inheritdoc/>
        public bool Equals(LetBlock? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                LetTokenLocation.Equals(other.LetTokenLocation) &&
                Enumerable.SequenceEqual(Declarations, other.Declarations) &&
                InTokenLocation.Equals(other.InTokenLocation) &&
                Expression.Equals(other.Expression);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(LetTokenLocation);

            foreach (var item in Declarations)
                hashCode.Add(item);

            hashCode.Add(InTokenLocation);
            hashCode.Add(Expression);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>
    /// Declarations inside a let: nested functions or destructuring bindings.
    /// </summary>
    public abstract record LetDeclaration
    {
        /// <summary>Local function declaration inside a let block.</summary>
        public sealed record LetFunction(
            FunctionStruct Function)
            : LetDeclaration;

        /// <summary>Local binding via pattern destructuring.</summary>
        public sealed record LetDestructuring(
            Node<Pattern> Pattern,
            Location EqualsTokenLocation,
            Node<Expression> Expression)
            : LetDeclaration;
    }

    /// <summary>Record literal expression.</summary>
    public sealed record RecordExpr(
        SeparatedSyntaxList<RecordExprField> Fields)
        : Expression;

    /// <summary>Expression accessing a record field.</summary>
    public sealed record RecordAccess(
        Node<Expression> Record,
        Node<string> FieldName)
        : Expression;

    /// <summary>Expression yielding a getter function for a record field.</summary>
    public sealed record RecordAccessFunction(
        string FunctionName)
        : Expression;

    /// <summary>Expression updating a record value.</summary>
    public sealed record RecordUpdateExpression(
        Node<string> RecordName,
        Location PipeLocation,
        SeparatedSyntaxList<RecordExprField> Fields)
        : Expression;

    /// <summary>GLSL shader expression [glsl| ... |].</summary>
    public sealed record GLSLExpression(
        string ShaderCode)
        : Expression;
}

-}


type Expression
    = UnitExpr
    | StringLiteral String (Maybe String)
    | MultilineStringLiteral String (Maybe (List String))
    | CharLiteral Int
    | IntegerLiteral String
    | FloatLiteral String
    | Negation (Node Expression)
    | ListExpr (SeparatedSyntaxList (Node Expression))
    | Identifier (List String) String
    | IfBlock Location (Node Expression) Location (Node Expression) Location (Node Expression)
    | PrefixOperator String
    | Parenthesized (Node Expression)
    | Application (Node Expression) (List (Node Expression))
    | OperatorApplication (Node String) InfixDirection (Node Expression) (Node Expression)
    | TupledExpression (SeparatedSyntaxList (Node Expression))
    | LambdaExpression LambdaStruct
    | CaseExpression CaseBlock
    | LetExpression LetBlock
    | RecordExpr (SeparatedSyntaxList RecordExprField)
    | RecordAccess (Node Expression) (Node String)
    | RecordAccessFunction String
    | RecordUpdateExpression (Node String) Location (SeparatedSyntaxList RecordExprField)
    | GLSLExpression String



{-


/// <summary>
/// A record expression field with field name, equals sign location, and value expression.
/// Used in RecordExpr and RecordUpdateExpression.
/// </summary>
public record RecordExprField(
    Node<string> FieldName,
    Location EqualsLocation,
    Node<Expression> ValueExpr);


/// <summary>
/// Lambda expression capturing argument patterns and body.
/// </summary>
public record LambdaStruct(
    Location BackslashLocation,
    IReadOnlyList<Node<Pattern>> Arguments,
    Location ArrowLocation,
    Node<Expression> Expression)
{
    /// <inheritdoc/>
    public virtual bool Equals(LambdaStruct? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            BackslashLocation.Equals(other.BackslashLocation) &&
            Enumerable.SequenceEqual(Arguments, other.Arguments) &&
            ArrowLocation.Equals(other.ArrowLocation) &&
            Expression.Equals(other.Expression);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(BackslashLocation);

        foreach (var item in Arguments)
            hashCode.Add(item);

        hashCode.Add(ArrowLocation);
        hashCode.Add(Expression);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Case expression block containing scrutinee and branches.
/// </summary>
public record CaseBlock(
    Location CaseTokenLocation,
    Node<Expression> Expression,
    Location OfTokenLocation,
    IReadOnlyList<Case> Cases)
{
    /// <inheritdoc/>
    public virtual bool Equals(CaseBlock? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            CaseTokenLocation.Equals(other.CaseTokenLocation) &&
            Expression.Equals(other.Expression) &&
            OfTokenLocation.Equals(other.OfTokenLocation) &&
            Enumerable.SequenceEqual(Cases, other.Cases);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(CaseTokenLocation);
        hashCode.Add(Expression);
        hashCode.Add(OfTokenLocation);

        foreach (var item in Cases)
            hashCode.Add(item);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Single case: pattern and resulting expression.
/// </summary>
public record Case(
    Node<Pattern> Pattern,
    Location ArrowLocation,
    Node<Expression> Expression);

-}


type alias RecordExprField =
    { fieldName : Node String
    , equalsLocation : Location
    , valueExpr : Node Expression
    }


type alias LambdaStruct =
    { backslashLocation : Location
    , arguments : List (Node Pattern)
    , arrowLocation : Location
    , expression : Node Expression
    }


type alias CaseBlock =
    { caseTokenLocation : Location
    , expression : Node Expression
    , ofTokenLocation : Location
    , cases : List Case
    }


type alias Case =
    { pattern : Node Pattern
    , arrowLocation : Location
    , expression : Node Expression
    }



{-


    /// <summary>
    /// Let block containing declarations and final expression.
    /// </summary>
    public sealed record LetBlock(
        Location LetTokenLocation,
        IReadOnlyList<Node<LetDeclaration>> Declarations,
        Location InTokenLocation,
        Node<Expression> Expression)
    {
        /// <inheritdoc/>
        public bool Equals(LetBlock? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                LetTokenLocation.Equals(other.LetTokenLocation) &&
                Enumerable.SequenceEqual(Declarations, other.Declarations) &&
                InTokenLocation.Equals(other.InTokenLocation) &&
                Expression.Equals(other.Expression);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(LetTokenLocation);

            foreach (var item in Declarations)
                hashCode.Add(item);

            hashCode.Add(InTokenLocation);
            hashCode.Add(Expression);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>
    /// Declarations inside a let: nested functions or destructuring bindings.
    /// </summary>
    public abstract record LetDeclaration
    {
        /// <summary>Local function declaration inside a let block.</summary>
        public sealed record LetFunction(
            FunctionStruct Function)
            : LetDeclaration;

        /// <summary>Local binding via pattern destructuring.</summary>
        public sealed record LetDestructuring(
            Node<Pattern> Pattern,
            Location EqualsTokenLocation,
            Node<Expression> Expression)
            : LetDeclaration;
    }
-}


type alias LetBlock =
    { letTokenLocation : Location
    , declarations : List (Node LetDeclaration)
    , inTokenLocation : Location
    , expression : Node Expression
    }


type LetDeclaration
    = LetFunction FunctionStruct
    | LetDestructuring (Node Pattern) Location (Node Expression)


type alias FunctionStruct =
    { documentation : Maybe (Node String)
    , signature : Maybe (Node Signature)
    , declaration : Node FunctionImplementation
    }


type alias FunctionImplementation =
    { name : Node String
    , arguments : List (Node Pattern)
    , equalsTokenLocation : Location
    , expression : Node Expression
    }


type alias Signature =
    { name : Node String
    , colonLocation : Location
    , typeAnnotation : Node TypeAnnotation
    }
