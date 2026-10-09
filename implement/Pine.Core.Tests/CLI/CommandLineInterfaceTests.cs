using AwesomeAssertions;
using Pine.Core.CLI;
using Xunit;

namespace Pine.Core.Tests.CLI;

public class CommandLineInterfaceTests
{
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
