using AwesomeAssertions;
using Pine.Core.CLI;
using System;
using System.Globalization;
using Xunit;

namespace Pine.Core.Tests.CLI;

public class CommandLineInterfaceTests
{
    [Theory]
    [InlineData(0, "0 ms")]
    [InlineData(0.1, "0 ms")]
    [InlineData(12.6, "13 ms")]
    [InlineData(999, "999 ms")]
    [InlineData(999.9, "1000 ms")]
    [InlineData(1000, "1.00 s")]
    [InlineData(1001, "1.00 s")]
    [InlineData(1234, "1.23 s")]
    [InlineData(12345, "12.35 s")]
    [InlineData(60000, "60.00 s")]
    public void Format_duration_selects_units_before_rounding_and_is_culture_invariant(
        double milliseconds,
        string expected)
    {
        var previousCulture = CultureInfo.CurrentCulture;

        try
        {
            CultureInfo.CurrentCulture = CultureInfo.GetCultureInfo("fr-FR");
            CommandLineInterface.FormatDuration(TimeSpan.FromMilliseconds(milliseconds)).Should().Be(expected);
        }
        finally
        {
            CultureInfo.CurrentCulture = previousCulture;
        }
    }

    [Theory]
    [InlineData(0, "0")]
    [InlineData(999, "999")]
    [InlineData(1000, "1_000")]
    [InlineData(1234567, "1_234_567")]
    [InlineData(-1234567, "-1_234_567")]
    [InlineData(long.MinValue, "-9_223_372_036_854_775_808")]
    [InlineData(long.MaxValue, "9_223_372_036_854_775_807")]
    public void Format_integer_preserves_sign_and_groups_digits(long value, string expected)
    {
        CommandLineInterface.FormatIntegerForDisplay(value).Should().Be(expected);
    }

    [Fact]
    public void Format_integer_supports_a_custom_separator()
    {
        CommandLineInterface.FormatIntegerForDisplay(-1234567, ',').Should().Be("-1,234,567");
    }
}
