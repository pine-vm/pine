using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.PineVM;

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
                ExpressionSyntax,
                SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        CompileTypeSyntax.TypeSyntaxFromType(
                            typeof(PineKernelValues),
                            declarationSyntaxContext),
                        SyntaxFactory.IdentifierName(nameof(PineKernelValues.TrueValue)))),

            ValueType.Boolean =>
            ExpressionSyntax,

            ValueType.Integer =>
            SyntaxFactory.LiteralExpression(
                SyntaxKind.FalseLiteralExpression),

            _ =>
            throw new System.NotImplementedException("Unhandled ValueType " + Type),
        };
    }
}
