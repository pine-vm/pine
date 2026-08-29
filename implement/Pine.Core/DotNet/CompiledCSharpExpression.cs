using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Pine.Core.DotNet;

/// <summary>
/// Represents a generated C# expression together with the most specific Pine value category known for its result.
/// </summary>
public record CompiledCSharpExpression
{
    /// <summary>
    /// Roslyn expression syntax that computes the value.
    /// </summary>
    public ExpressionSyntax ExpressionSyntax { get; init; }

    /// <summary>
    /// Most specific Pine value category known for the expression result.
    /// </summary>
    public CompiledCSharpExpression.ValueType Type { get; init; }

    /// <summary>
    /// Constructs a compiled C# expression from its syntax and the Pine value category it evaluates to.
    /// </summary>
    public CompiledCSharpExpression(
        ExpressionSyntax ExpressionSyntax,
        CompiledCSharpExpression.ValueType Type)
    {
        this.ExpressionSyntax = ExpressionSyntax;
        this.Type = Type;
    }

    /// <summary>
    /// Deconstructs this expression into its syntax and the Pine value category it evaluates to.
    /// </summary>
    public void Deconstruct(
        out ExpressionSyntax ExpressionSyntax,
        out CompiledCSharpExpression.ValueType Type)
    {
        ExpressionSyntax = this.ExpressionSyntax;
        Type = this.Type;
    }

    /// <summary>
    /// Wraps syntax that already yields a generic PineValue.
    /// </summary>
    public static CompiledCSharpExpression Generic(
        ExpressionSyntax expressionSyntax) =>
        new(expressionSyntax, ValueType.Generic);

    /// <summary>
    /// Wraps syntax that yields a specialized boolean result.
    /// </summary>
    public static CompiledCSharpExpression Boolean(
        ExpressionSyntax expressionSyntax) =>
        new(expressionSyntax, ValueType.Boolean);

    /// <summary>
    /// Wraps syntax that yields a specialized integer result.
    /// </summary>
    public static CompiledCSharpExpression Integer(
        ExpressionSyntax expressionSyntax) =>
        new(expressionSyntax, ValueType.Integer);

    /// <summary>
    /// Classifies which specialized Pine value representation a compiled expression produces.
    /// </summary>
    public enum ValueType
    {
        /// <summary>
        /// The expression already yields a plain PineValue.
        /// </summary>
        // Plain PineValue
        Generic = 10,

        /// <summary>
        /// The expression yields a specialized boolean result.
        /// </summary>
        Boolean = 30,

        /// <summary>
        /// The expression yields a specialized integer result.
        /// </summary>
        Integer = 40,
    }

    /// <summary>
    /// Converts this expression to syntax that evaluates to a generic PineValue, wrapping specialized booleans and integers when needed.
    /// </summary>
    public ExpressionSyntax AsGenericValue(
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        return
            Type switch
            {
                ValueType.Generic =>
                ExpressionSyntax,

                ValueType.Boolean =>
                PineCSharpSyntaxFactory.PineValueFromBoolExpression(
                    ExpressionSyntax,
                    declarationSyntaxContext),

                ValueType.Integer =>
                PineCSharpSyntaxFactory.GenericExpressionFromIntegerExpression(
                    ExpressionSyntax,
                    declarationSyntaxContext),

                _ =>
                throw new System.NotImplementedException(
                    "Unhandled ValueType " + Type),
            };
    }

    /// <summary>
    /// Converts this expression to syntax that evaluates to a boolean, deriving false for expressions known to be integers.
    /// </summary>
    public ExpressionSyntax AsBooleanValue(
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        return
            Type switch
            {
                ValueType.Generic =>
                // boolean == PineKernelValues.TrueValue
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.EqualsExpression,
                    EnsureIsParenthesizedForComposition(ExpressionSyntax),
                    EnsureIsParenthesizedForComposition(
                        PineCSharpSyntaxFactory.ExpressionForPineValueBooleanLiteral(true, declarationSyntaxContext))),

                ValueType.Boolean =>
                ExpressionSyntax,

                ValueType.Integer =>
                SyntaxFactory.LiteralExpression(
                    SyntaxKind.FalseLiteralExpression),

                _ =>
                throw new System.NotImplementedException("Unhandled ValueType " + Type),
            };
    }

    /// <summary>
    /// Wraps an expression in parentheses when composition into a larger syntax tree would otherwise change parsing.
    /// </summary>
    public static ExpressionSyntax EnsureIsParenthesizedForComposition(
        ExpressionSyntax expression)
    {
        if (ExpressionNeedsParensForComposition(expression))
        {
            return SyntaxFactory.ParenthesizedExpression(expression);
        }

        return expression;
    }

    /// <summary>
    /// Determines whether an expression must be parenthesized before it is embedded into surrounding generated syntax.
    /// </summary>
    public static bool ExpressionNeedsParensForComposition(
        ExpressionSyntax expression)
    {
        if (expression is IdentifierNameSyntax)
            return false;

        if (expression is QualifiedNameSyntax)
            return false;

        if (expression is MemberAccessExpressionSyntax memberAccess)
            return false;

        if (expression is LiteralExpressionSyntax)
            return false;

        if (expression is InvocationExpressionSyntax)
            return false;

        if (expression is ParenthesizedExpressionSyntax)
            return false;

        if (expression is ThrowExpressionSyntax)
        {
            // https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/grammar#a3-syntactic-grammar
            return false;
        }

        return true;
    }
}
