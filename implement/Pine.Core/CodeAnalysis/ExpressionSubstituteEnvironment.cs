using Pine.Core.Runtime.InternalExpression;

namespace Pine.Core.CodeAnalysis;

/// <summary>Public canonical adapter for retained environment substitution.</summary>
public static class ExpressionSubstituteEnvironment
{
    /// <summary>Substitutes environment references, retaining unchanged syntax.</summary>
    public static PineLanguage.Expression SubstituteEnvironmentNode(
        PineLanguage.Expression expression,
        PineLanguage.Expression environmentReplacement) =>
        ExpressionConversion.ToCanonicalCached(
            EnvironmentSubstitution.SubstituteEnvironmentNode(
                ExpressionConversion.FromCanonicalCached(expression),
                ExpressionConversion.FromCanonicalCached(environmentReplacement)));
}
