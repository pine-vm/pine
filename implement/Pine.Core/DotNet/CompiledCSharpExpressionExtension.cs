
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace Pine.Core.DotNet;

public static class CompiledCSharpExpressionExtension
{
    public static ExpressionSyntax AsGenericValue(
        this IEnumerable<CompiledCSharpExpression> expressions,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        CompiledCSharpExpression? lastSeen = null;

        foreach (var expr in expressions)
        {
            if (expr.Type is CompiledCSharpExpression.ValueType.Generic)
            {
                return expr.ExpressionSyntax;
            }

            lastSeen = expr;
        }

        return lastSeen.AsGenericValue(declarationSyntaxContext);
    }
}
