using System;
using System.Numerics;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Provides conversion methods for Elm float literals between their string representation
/// and the numeric number representation used by the Elm compiler.
/// </summary>
public static class FloatLiteralConversion
{
    /// <summary>
    /// Converts a float literal string (as it appears in Elm source code) to an <see cref="ElmValue.ElmFloat"/>
    /// numeric representation.
    /// </summary>
    /// <param name="literal">The float literal string, e.g., "6.022e23", "3.14", "1.5e-10"</param>
    /// <returns>The numeric representation of the value.</returns>
    /// <exception cref="FormatException">Thrown when the literal cannot be parsed.</exception>
    public static ElmValue.ElmFloat ToElmFloat(string literal)
    {
        // First, parse the literal to understand its structure
        // Elm supports decimal notation (3.14) and scientific notation (6.022e23, 1.5e-10)

        var normalized = literal.Trim();

        // Check for scientific notation
        var eIndex = normalized.IndexOfAny(['e', 'E']);

        if (eIndex >= 0)
        {
            return ParseScientificNotation(normalized, eIndex);
        }
        else
        {
            return ParseDecimalNotation(normalized);
        }
    }

    private static ElmValue.ElmFloat ParseScientificNotation(string literal, int eIndex)
    {
        // Split into mantissa and exponent parts
        var mantissaPart = literal[..eIndex];
        var exponentPart = literal[(eIndex + 1)..];

        // Parse the exponent
        var exponent = int.Parse(exponentPart, System.Globalization.CultureInfo.InvariantCulture);

        // Parse the mantissa
        var mantissaParsed = ParseDecimalNotation(mantissaPart);

        // Apply the exponent: multiply by 10^exponent
        if (exponent >= 0)
        {
            // Positive exponent: multiply numerator by 10^exponent
            var multiplier = BigInteger.Pow(10, exponent);

            return
                ElmValue.ElmFloat.Normalized(
                    mantissaParsed.Numerator * multiplier,
                    mantissaParsed.Denominator);
        }
        else
        {
            // Negative exponent: multiply denominator by 10^(-exponent)
            var multiplier = BigInteger.Pow(10, -exponent);

            return
                ElmValue.ElmFloat.Normalized(
                    mantissaParsed.Numerator,
                    mantissaParsed.Denominator * multiplier);
        }
    }

    private static ElmValue.ElmFloat ParseDecimalNotation(string literal)
    {
        var isNegative = literal.StartsWith('-');
        var absLiteral = isNegative ? literal[1..] : literal;

        var dotIndex = absLiteral.IndexOf('.');

        if (dotIndex < 0)
        {
            // No decimal point - it's an integer
            var intValue =
                BigInteger.Parse(absLiteral, System.Globalization.CultureInfo.InvariantCulture);

            return
                ElmValue.ElmFloat.Normalized(
                    isNegative ? -intValue : intValue,
                    BigInteger.One);
        }

        // Has decimal point
        var integerPart = absLiteral[..dotIndex];
        var fractionalPart = absLiteral[(dotIndex + 1)..];

        // Calculate the denominator as 10^(number of fractional digits)
        var denominator = BigInteger.Pow(10, fractionalPart.Length);

        // Calculate the numerator: integer part * denominator + fractional part
        var integerValue =
            string.IsNullOrEmpty(integerPart)
            ?
            BigInteger.Zero
            :
            BigInteger.Parse(integerPart, System.Globalization.CultureInfo.InvariantCulture);

        var fractionalValue =
            string.IsNullOrEmpty(fractionalPart)
            ?
            BigInteger.Zero
            :
            BigInteger.Parse(fractionalPart, System.Globalization.CultureInfo.InvariantCulture);

        var numerator = integerValue * denominator + fractionalValue;

        return
            ElmValue.ElmFloat.Normalized(
                isNegative ? -numerator : numerator,
                denominator);
    }

    /// <summary>
    /// Converts a float literal string to a <see cref="double"/> value.
    /// </summary>
    /// <param name="literal">The float literal string.</param>
    /// <returns>The double value.</returns>
    /// <remarks>
    /// This method may lose precision for very large or very small values.
    /// Use <see cref="ToElmFloat"/> for exact numeric representation.
    /// </remarks>
    public static double ToDouble(string literal) =>
        double.Parse(literal, System.Globalization.CultureInfo.InvariantCulture);
}
