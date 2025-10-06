using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Pine.Core.DotNet;

public record CompiledCSharpExpression(
    ExpressionSyntax ExpressionSyntax,
    CompiledCSharpExpression.ValueType Type)
{
    public static CompiledCSharpExpression Generic(
        ExpressionSyntax expressionSyntax) =>
        new(expressionSyntax, ValueType.Generic);

    public static CompiledCSharpExpression Boolean(
        ExpressionSyntax expressionSyntax) =>
        new(expressionSyntax, ValueType.Boolean);

    public static CompiledCSharpExpression Integer(
        ExpressionSyntax expressionSyntax) =>
        new(expressionSyntax, ValueType.Integer);

    public enum ValueType
    {
        // Plain PineValue
        Generic = 10,

        Boolean = 30,

        Integer = 40,

    }

    public ExpressionSyntax AsGenericValue(
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        return Type switch
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

    public ExpressionSyntax AsBooleanValue(
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        return Type switch
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

    public static ExpressionSyntax EnsureIsParenthesizedForComposition(
        ExpressionSyntax expression)
    {
        if (ExpressionNeedsParensForComposition(expression))
        {
            return SyntaxFactory.ParenthesizedExpression(expression);
        }

        return expression;
    }

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
