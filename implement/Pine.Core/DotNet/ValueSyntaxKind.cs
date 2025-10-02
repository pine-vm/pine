using System.Collections.Generic;

namespace Pine.Core.DotNet;

/// <summary>
/// Classifies the preferred C# literal syntax for a given <c>PineValue</c>.
/// </summary>
/// <remarks>
/// Instances are produced by <see cref="PineCSharpSyntaxFactory.CompileToCSharpLiteralExpression(PineValue, System.Func{PineValue, Microsoft.CodeAnalysis.CSharp.Syntax.ExpressionSyntax?}, DeclarationSyntaxContext)"/>
/// and consumed to order generated declarations (see <see cref="CSharpDeclarationOrder.ValueSyntaxKindDeclarationOrder"/>).
/// </remarks>
public abstract record ValueSyntaxKind
{
    /// <summary>
    /// The value is represented as a canonical signed integer literal.
    /// </summary>
    /// <param name="Value">The 64-bit integer value that will be emitted.</param>
    public record AsSignedInteger(long Value)
        : ValueSyntaxKind;

    /// <summary>
    /// The value is a list where each element is a canonical signed integer literal.
    /// </summary>
    /// <param name="Values">The ordered sequence of 64-bit integer items.</param>
    public record AsListOfSignedIntegers(IReadOnlyList<long> Values)
        : ValueSyntaxKind;

    /// <summary>
    /// The value is a blob recognized as a string and emitted via string literal syntax.
    /// </summary>
    /// <param name="Value">The string content to emit.</param>
    public record AsString(string Value)
        : ValueSyntaxKind;

    /// <summary>
    /// Fallback classification for any other representation (e.g., arbitrary blobs, mixed lists, empty list, etc.).
    /// </summary>
    public record Other
        : ValueSyntaxKind;
}
