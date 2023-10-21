using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using System.Globalization;

namespace Pine.CompilePineToDotNet;

public static class PineCSharpSyntaxFactory
{
    public static LiteralExpressionSyntax ExpressionSyntaxForIntegerLiteral(long integer) =>
        SyntaxFactory.LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            SyntaxTokenForIntegerLiteral(integer));

    public static SyntaxToken SyntaxTokenForIntegerLiteral(long integer) =>
        SyntaxFactory.Literal(
            integer.ToString("N0", IntegerLiteralNumberFormatInfo),
            integer);

    static readonly NumberFormatInfo IntegerLiteralNumberFormatInfo = new()
    {
        NumberGroupSeparator = "_",
        NumberGroupSizes = [3]
    };

    public static readonly ExpressionSyntax PineValueEmptyListSyntax =
        SyntaxFactory.MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            SyntaxFactory.IdentifierName(nameof(PineValue)),
            SyntaxFactory.IdentifierName(nameof(PineValue.EmptyList)));
}
