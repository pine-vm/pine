using AwesomeAssertions;
using Pine.Core.CLI;
using System.Collections.Generic;
using System.Globalization;
using Xunit;

namespace Pine.Core.Tests.CLI;

public class NumericInputTests
{
    [Theory]
    [InlineData("0", 0L)]
    [InlineData("+0", 0L)]
    [InlineData("-0", 0L)]
    [InlineData("123", 123L)]
    [InlineData("+123", 123L)]
    [InlineData("-123", -123L)]
    [InlineData("000123", 123L)]
    [InlineData("1_000", 1_000L)]
    [InlineData("1__2___3", 123L)]
    [InlineData("+0__0__1", 1L)]
    [InlineData("-1__2__3", -123L)]
    [InlineData(" \t\r\n-1__234 \t\r\n", -1_234L)]
    [InlineData("\u00a0+12\u00a0", 12L)]
    [InlineData("9223372036854775807", long.MaxValue)]
    [InlineData("+9_223_372_036_854_775_807", long.MaxValue)]
    [InlineData("-9223372036854775808", long.MinValue)]
    [InlineData("-9__223__372__036__854__775__808", long.MinValue)]
    public void Parse_all_unitless_forms(string input, long expected)
    {
        NumericInput.ParseInteger(input).Should().Be(Result<string, long>.ok(expected));

        NumericInput.ParseCount(input).Should().Be(
            Result<string, (long, CountUnit)>.ok((expected, CountUnit.None)));

        NumericInput.ParseTime(input).Should().Be(
            Result<string, (long, TimeUnit?)>.ok((expected, null)));
    }

    public static IEnumerable<object?[]> InvalidNumbers =>
        [
            [null],
            [""],
            [" \t\r\n"],
            ["+"],
            ["-"],
            ["_"],
            ["_1"],
            ["__1"],
            ["+_1"],
            ["-_1"],
            ["1_"],
            ["1__"],
            ["1_ "],
            ["1__ 2"],
            ["1 _2"],
            ["1 2"],
            ["+ 1"],
            ["- 1"],
            ["1\t2"],
            ["1\n2"],
            ["1\u00a02"],
            ["--1"],
            ["++1"],
            ["+-1"],
            ["-+1"],
            ["1-2"],
            ["1+2"],
            ["1.0"],
            ["0.5"],
            ["1,000"],
            ["1e3"],
            ["1E3"],
            ["0x10"],
            ["NaN"],
            ["Infinity"],
            ["１２"],
            ["١٢"],
            ["−1"],
            ["1\0"],
            ["9223372036854775808"],
            ["+9223372036854775808"],
            ["-9223372036854775809"],
            ["9__223__372__036__854__775__808"],
            ["-9__223__372__036__854__775__809"],
            ["999999999999999999999999999999999999999999999999999999999999"],
        ];

    [Theory]
    [MemberData(nameof(InvalidNumbers))]
    public void Reject_invalid_number_in_all_parsers(string? input)
    {
        NumericInput.ParseInteger(input).Should().BeOfType<Result<string, long>.Err>();

        NumericInput.ParseCount(input).Should().BeOfType<
            Result<string, (long, CountUnit)>.Err>();

        NumericInput.ParseTime(input).Should().BeOfType<
            Result<string, (long, TimeUnit?)>.Err>();
    }

    [Fact]
    public void Accept_arbitrary_internal_underscore_repetitions_and_leading_zeros()
    {
        var repeatedUnderscores = "1" + new string('_', 200) + "2";
        var leadingZeros = new string('0', 200) + "123";

        NumericInput.ParseInteger(repeatedUnderscores).Should().Be(Result<string, long>.ok(12));
        NumericInput.ParseInteger(leadingZeros).Should().Be(Result<string, long>.ok(123));
    }

    [Theory]
    [InlineData("k", CountUnit.Kilo)]
    [InlineData("M", CountUnit.Mega)]
    [InlineData("G", CountUnit.Giga)]
    public void Parse_count_units_with_signs_and_whitespace(string suffix, CountUnit unit)
    {
        foreach (var separator in new[] { "", " ", "   ", "\t" })
        {
            NumericInput.ParseCount(" \t+1__234" + separator + suffix + " \r\n")
                .Should().Be(Result<string, (long, CountUnit)>.ok((1_234L, unit)));

            NumericInput.ParseCount("-12" + separator + suffix)
                .Should().Be(Result<string, (long, CountUnit)>.ok((-12L, unit)));

            NumericInput.ParseCount("9223372036854775807" + separator + suffix)
                .Should().Be(Result<string, (long, CountUnit)>.ok((long.MaxValue, unit)));

            NumericInput.ParseCount("-9223372036854775808" + separator + suffix)
                .Should().Be(Result<string, (long, CountUnit)>.ok((long.MinValue, unit)));

            NumericInput.ParseInteger("12" + separator + suffix)
                .Should().BeOfType<Result<string, long>.Err>();
        }
    }

    [Theory]
    [InlineData("1K")]
    [InlineData("1m")]
    [InlineData("1g")]
    [InlineData("1kilo")]
    [InlineData("1mega")]
    [InlineData("1giga")]
    [InlineData("1KB")]
    [InlineData("1Ki")]
    [InlineData("1kk")]
    [InlineData("1MG")]
    [InlineData("1 k M")]
    [InlineData("1 M k")]
    [InlineData("1__k")]
    [InlineData("1_ k")]
    [InlineData("1_2_ M")]
    [InlineData("_1M")]
    [InlineData("+_1G")]
    [InlineData("1 2k")]
    [InlineData("1.5k")]
    [InlineData("1,5M")]
    [InlineData("1e3G")]
    [InlineData("9223372036854775808k")]
    [InlineData("-9223372036854775809 M")]
    public void Reject_invalid_count_units_and_magnitudes(string input)
    {
        NumericInput.ParseCount(input)
            .Should().BeOfType<Result<string, (long, CountUnit)>.Err>();
    }

    [Theory]
    [InlineData("ms", TimeUnit.Milliseconds)]
    [InlineData("millisecond", TimeUnit.Milliseconds)]
    [InlineData("milliseconds", TimeUnit.Milliseconds)]
    [InlineData("s", TimeUnit.Seconds)]
    [InlineData("second", TimeUnit.Seconds)]
    [InlineData("seconds", TimeUnit.Seconds)]
    [InlineData("min", TimeUnit.Minutes)]
    [InlineData("m", TimeUnit.Minutes)]
    [InlineData("minute", TimeUnit.Minutes)]
    [InlineData("minutes", TimeUnit.Minutes)]
    [InlineData("h", TimeUnit.Hours)]
    [InlineData("hour", TimeUnit.Hours)]
    [InlineData("hours", TimeUnit.Hours)]
    public void Parse_time_units_with_signs_and_whitespace(string suffix, TimeUnit unit)
    {
        foreach (var separator in new[] { "", " ", "   ", "\t" })
        {
            NumericInput.ParseTime(" \t+1__234" + separator + suffix + " \r\n")
                .Should().Be(Result<string, (long, TimeUnit?)>.ok((1_234L, unit)));

            NumericInput.ParseTime("-12" + separator + suffix)
                .Should().Be(Result<string, (long, TimeUnit?)>.ok((-12L, unit)));

            NumericInput.ParseTime("9223372036854775807" + separator + suffix)
                .Should().Be(Result<string, (long, TimeUnit?)>.ok((long.MaxValue, unit)));

            NumericInput.ParseTime("-9223372036854775808" + separator + suffix)
                .Should().Be(Result<string, (long, TimeUnit?)>.ok((long.MinValue, unit)));

            NumericInput.ParseInteger("12" + separator + suffix)
                .Should().BeOfType<Result<string, long>.Err>();
        }
    }

    [Theory]
    [InlineData("1MS")]
    [InlineData("1Ms")]
    [InlineData("1S")]
    [InlineData("1Min")]
    [InlineData("1M")]
    [InlineData("1H")]
    [InlineData("1Seconds")]
    [InlineData("1MINUTES")]
    [InlineData("1Hour")]
    [InlineData("1sec")]
    [InlineData("1mins")]
    [InlineData("1hr")]
    [InlineData("1k")]
    [InlineData("1G")]
    [InlineData("1minutesseconds")]
    [InlineData("1 ms s")]
    [InlineData("1s h")]
    [InlineData("1m s")]
    [InlineData("1 mi n")]
    [InlineData("1 hour s")]
    [InlineData("1__s")]
    [InlineData("1_ ms")]
    [InlineData("_1h")]
    [InlineData("-_1min")]
    [InlineData("1 2s")]
    [InlineData("1.5s")]
    [InlineData("1,5min")]
    [InlineData("1e3ms")]
    [InlineData("9223372036854775808ms")]
    [InlineData("-9223372036854775809 hours")]
    public void Reject_invalid_time_units_and_magnitudes(string input)
    {
        NumericInput.ParseTime(input)
            .Should().BeOfType<Result<string, (long, TimeUnit?)>.Err>();
    }

    [Fact]
    public void Parsing_is_independent_of_current_culture()
    {
        var previousCulture = CultureInfo.CurrentCulture;
        var customCulture = (CultureInfo)CultureInfo.InvariantCulture.Clone();

        customCulture.NumberFormat.PositiveSign = "!";
        customCulture.NumberFormat.NegativeSign = "~";
        customCulture.NumberFormat.NumberDecimalSeparator = ",";
        customCulture.NumberFormat.NumberGroupSeparator = "_";

        try
        {
            CultureInfo.CurrentCulture = customCulture;

            NumericInput.ParseInteger("+1__234").Should().Be(Result<string, long>.ok(1_234));
            NumericInput.ParseInteger("-1__234").Should().Be(Result<string, long>.ok(-1_234));
            NumericInput.ParseInteger("~1234").Should().BeOfType<Result<string, long>.Err>();

            NumericInput.ParseCount("-1__234 M").Should().Be(
                Result<string, (long, CountUnit)>.ok((-1_234L, CountUnit.Mega)));

            NumericInput.ParseTime("+1__234 minutes").Should().Be(
                Result<string, (long, TimeUnit?)>.ok((1_234L, TimeUnit.Minutes)));

            NumericInput.ParseCount("1,5M")
                .Should().BeOfType<Result<string, (long, CountUnit)>.Err>();

            NumericInput.ParseTime("1,5s")
                .Should().BeOfType<Result<string, (long, TimeUnit?)>.Err>();
        }
        finally
        {
            CultureInfo.CurrentCulture = previousCulture;
        }
    }
}
