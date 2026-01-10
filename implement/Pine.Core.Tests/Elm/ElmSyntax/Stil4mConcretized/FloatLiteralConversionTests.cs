using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using System;
using System.Numerics;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized;

/// <summary>
/// Unit tests for <see cref="FloatLiteralConversion"/> which handles conversion between
/// Elm float literal strings and the numeric (<see cref="ElmValue.ElmFloat"/>) representation.
/// </summary>
public class FloatLiteralConversionTests
{
    [Theory]
    [InlineData("3.14", 157, 50)]  // 314/100 = 157/50 (normalized)
    [InlineData("0.5", 1, 2)]
    [InlineData("1.0", 1, 1)]
    [InlineData("2.5", 5, 2)]
    [InlineData("0.25", 1, 4)]
    [InlineData("0.125", 1, 8)]
    [InlineData("10.0", 10, 1)]
    [InlineData("100.0", 100, 1)]
    [InlineData("0.1", 1, 10)]
    [InlineData("0.01", 1, 100)]
    public void ToElmFloat_simple_decimal_literals(string literal, long expectedNumerator, long expectedDenominator)
    {
        var result = FloatLiteralConversion.ToElmFloat(literal);

        result.Numerator.Should().Be(new BigInteger(expectedNumerator));
        result.Denominator.Should().Be(new BigInteger(expectedDenominator));
    }

    [Theory]
    [InlineData("-3.14", -157, 50)]  // -314/100 = -157/50 (normalized)
    [InlineData("-0.5", -1, 2)]
    [InlineData("-1.0", -1, 1)]
    [InlineData("-2.5", -5, 2)]
    public void ToElmFloat_negative_decimal_literals(string literal, long expectedNumerator, long expectedDenominator)
    {
        var result = FloatLiteralConversion.ToElmFloat(literal);

        result.Numerator.Should().Be(new BigInteger(expectedNumerator));
        result.Denominator.Should().Be(new BigInteger(expectedDenominator));
    }

    [Theory]
    [InlineData("1e3", 1000, 1)]
    [InlineData("1.0e3", 1000, 1)]
    [InlineData("1e0", 1, 1)]
    [InlineData("1e1", 10, 1)]
    [InlineData("1e2", 100, 1)]
    [InlineData("5e2", 500, 1)]
    [InlineData("2.5e2", 250, 1)]
    public void ToElmFloat_positive_exponent_literals(string literal, long expectedNumerator, long expectedDenominator)
    {
        var result = FloatLiteralConversion.ToElmFloat(literal);

        result.Numerator.Should().Be(new BigInteger(expectedNumerator));
        result.Denominator.Should().Be(new BigInteger(expectedDenominator));
    }

    [Theory]
    [InlineData("1e-1", 1, 10)]
    [InlineData("1e-2", 1, 100)]
    [InlineData("1e-3", 1, 1000)]
    [InlineData("5e-1", 1, 2)]
    [InlineData("2.5e-1", 1, 4)]
    [InlineData("1.0e-1", 1, 10)]
    public void ToElmFloat_negative_exponent_literals(string literal, long expectedNumerator, long expectedDenominator)
    {
        var result = FloatLiteralConversion.ToElmFloat(literal);

        result.Numerator.Should().Be(new BigInteger(expectedNumerator));
        result.Denominator.Should().Be(new BigInteger(expectedDenominator));
    }

    [Theory]
    [InlineData("6.022e23")]
    [InlineData("3.0e8")]
    [InlineData("1.6e-19")]
    [InlineData("2.99792458e8")]
    [InlineData("9.81e0")]
    [InlineData("1.0e-10")]
    [InlineData("5.0e4")]
    [InlineData("1.0e12")]
    [InlineData("7.5e-3")]
    [InlineData("6.000022e39")]
    public void ToElmFloat_scientific_notation_parses_successfully(string literal)
    {
        // These are the exact literals from the Preserve_float_expressions_in_various_forms test
        var result = FloatLiteralConversion.ToElmFloat(literal);

        // Just verify the conversion doesn't throw and produces valid non-zero values
        result.Numerator.Should().NotBe(BigInteger.Zero);
        result.Denominator.Should().NotBe(BigInteger.Zero);
    }

    [Theory]
    [InlineData("6.022e23")]
    [InlineData("3.0e8")]
    [InlineData("1.6e-19")]
    public void ToElmFloat_preserves_precision_for_large_numbers(string literal)
    {
        var elmFloat = FloatLiteralConversion.ToElmFloat(literal);
        var doubleValue = FloatLiteralConversion.ToDouble(literal);

        // Convert ElmFloat back to double for comparison
        var elmFloatAsDouble = (double)elmFloat.Numerator / (double)elmFloat.Denominator;

        // The rational representation should give the same result as parsing to double
        // (within floating point precision limits)
        elmFloatAsDouble.Should().BeApproximately(doubleValue, Math.Abs(doubleValue * 1e-15) + 1e-308);
    }

    [Theory]
    [InlineData("1E3", 1000, 1)]
    [InlineData("1.0E3", 1000, 1)]
    [InlineData("2.5E-1", 1, 4)]
    public void ToElmFloat_uppercase_exponent_marker(string literal, long expectedNumerator, long expectedDenominator)
    {
        var result = FloatLiteralConversion.ToElmFloat(literal);

        result.Numerator.Should().Be(new BigInteger(expectedNumerator));
        result.Denominator.Should().Be(new BigInteger(expectedDenominator));
    }

    [Theory]
    [InlineData("3.14", 3.14)]
    [InlineData("0.5", 0.5)]
    [InlineData("1e3", 1000.0)]
    [InlineData("6.022e23", 6.022e23)]
    [InlineData("1.6e-19", 1.6e-19)]
    public void ToDouble_parses_literal_to_double(string literal, double expected)
    {
        var result = FloatLiteralConversion.ToDouble(literal);

        result.Should().Be(expected);
    }

    [Theory]
    [InlineData("  3.14  ", 157, 50)]  // 314/100 = 157/50 (normalized)
    [InlineData(" 1e3 ", 1000, 1)]
    public void ToElmFloat_trims_whitespace(string literal, long expectedNumerator, long expectedDenominator)
    {
        var result = FloatLiteralConversion.ToElmFloat(literal);

        result.Numerator.Should().Be(new BigInteger(expectedNumerator));
        result.Denominator.Should().Be(new BigInteger(expectedDenominator));
    }

    [Fact]
    public void ToElmFloat_avogadro_number_exact_representation()
    {
        // 6.022e23 = 6.022 * 10^23 = 6022 * 10^20 = 6022 * 100000000000000000000
        var result = FloatLiteralConversion.ToElmFloat("6.022e23");

        // The numerator should be 6022 * 10^20
        var expectedNumerator = BigInteger.Parse("6022") * BigInteger.Pow(10, 20);

        result.Numerator.Should().Be(expectedNumerator);
        result.Denominator.Should().Be(BigInteger.One);
    }

    [Fact]
    public void ToElmFloat_elementary_charge_exact_representation()
    {
        // 1.6e-19 = 1.6 * 10^-19 = 16/10 * 10^-19 = 16 * 10^-20 = 16 / 10^20
        var result = FloatLiteralConversion.ToElmFloat("1.6e-19");

        // After normalization, numerator = 16 / GCD(16, 10^20), denominator = 10^20 / GCD(16, 10^20)
        // GCD(16, 10^20) = GCD(16, 2^20 * 5^20) = 16 (since 16 = 2^4)
        // So: 16/16 = 1, 10^20/16 = 10^20/2^4 = (2^20 * 5^20) / 2^4 = 2^16 * 5^20
        var expectedNumerator = BigInteger.One;
        var expectedDenominator = BigInteger.Pow(2, 16) * BigInteger.Pow(5, 20);

        result.Numerator.Should().Be(expectedNumerator);
        result.Denominator.Should().Be(expectedDenominator);
    }

    [Fact]
    public void ToElmFloat_speed_of_light_exact_representation()
    {
        // 2.99792458e8 = 299792458 (exactly)
        var result = FloatLiteralConversion.ToElmFloat("2.99792458e8");

        // This should give an exact integer representation
        result.Numerator.Should().Be(new BigInteger(299792458));
        result.Denominator.Should().Be(BigInteger.One);
    }
}
