
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace Pine.Core.DotNet;

/// <summary>
/// Provides helpers for working with collections of compiled C# expressions during code generation.
/// </summary>
public static class CompiledCSharpExpressionExtension
{
    /// <summary>
    /// Returns the first expression already typed as a generic PineValue, or converts the last candidate as a fallback.
    /// </summary>
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
