using AwesomeAssertions;
using Pine.Core.CLI;
using System;
using Xunit;

namespace Pine.Core.Tests.CLI;

public class NumericInputExtensionsTests
{
    [Theory]
    [InlineData(CountUnit.None, 1L)]
    [InlineData(CountUnit.Kilo, 1_000L)]
    [InlineData(CountUnit.Mega, 1_000_000L)]
    [InlineData(CountUnit.Giga, 1_000_000_000L)]
    public void Count_conversion_applies_decimal_multiplier_and_checks_limits(
        CountUnit unit,
        long multiplier)
    {
        foreach (var value in new[] { 0L, 1L, -1L, 123L, -123L })
        {
            (value, unit).ToCount().Should().Be(Result<string, long>.ok(value * multiplier));
        }

        var positiveLimit = long.MaxValue / multiplier;
        var negativeLimit = long.MinValue / multiplier;

        (positiveLimit, unit).ToCount()
            .Should().Be(Result<string, long>.ok(positiveLimit * multiplier));

        (negativeLimit, unit).ToCount()
            .Should().Be(Result<string, long>.ok(negativeLimit * multiplier));

        if (multiplier > 1)
        {
            (positiveLimit + 1, unit).ToCount()
                .Should().BeOfType<Result<string, long>.Err>();

            (negativeLimit - 1, unit).ToCount()
                .Should().BeOfType<Result<string, long>.Err>();

            (long.MaxValue, unit).ToCount()
                .Should().BeOfType<Result<string, long>.Err>();

            (long.MinValue, unit).ToCount()
                .Should().BeOfType<Result<string, long>.Err>();
        }
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(4)]
    [InlineData(int.MaxValue)]
    public void Count_conversion_rejects_invalid_enum_even_for_zero(int unit)
    {
        foreach (var value in new[] { 0L, 1L, long.MinValue, long.MaxValue })
        {
            (value, (CountUnit)unit).ToCount()
                .Should().BeOfType<Result<string, long>.Err>();
        }
    }

    [Theory]
    [InlineData(TimeUnit.Milliseconds, 1L)]
    [InlineData(TimeUnit.Seconds, 1_000L)]
    [InlineData(TimeUnit.Minutes, 60_000L)]
    [InlineData(TimeUnit.Hours, 3_600_000L)]
    public void All_time_units_convert_with_defaults_and_explicit_overrides(
        TimeUnit unit,
        long millisecondsPerUnit)
    {
        foreach (var value in new[] { 0L, 1L, -1L, 1_234L, -1_234L })
        {
            (long Value, TimeUnit? Unit) explicitInput = (value, unit);
            (long Value, TimeUnit? Unit) implicitInput = (value, null);

            var expectedMilliseconds = Result<string, long>.ok(value * millisecondsPerUnit);
            var expectedSeconds = Result<string, decimal>.ok(value * millisecondsPerUnit / 1_000m);

            var expectedTimespan =
                Result<string, TimeSpan>.ok(
                    TimeSpan.FromTicks(value * millisecondsPerUnit * TimeSpan.TicksPerMillisecond));

            explicitInput.ToMilliseconds().Should().Be(expectedMilliseconds);
            explicitInput.ToSeconds().Should().Be(expectedSeconds);
            explicitInput.ToTimespan().Should().Be(expectedTimespan);

            implicitInput.ToMilliseconds(unit).Should().Be(expectedMilliseconds);
            implicitInput.ToSeconds(unit).Should().Be(expectedSeconds);
            implicitInput.ToTimespan(unit).Should().Be(expectedTimespan);

            foreach (var defaultUnit in Enum.GetValues<TimeUnit>())
            {
                explicitInput.ToMilliseconds(defaultUnit).Should().Be(expectedMilliseconds);
                explicitInput.ToSeconds(defaultUnit).Should().Be(expectedSeconds);
                explicitInput.ToTimespan(defaultUnit).Should().Be(expectedTimespan);
            }
        }
    }

    [Theory]
    [InlineData(0L)]
    [InlineData(1L)]
    [InlineData(-1L)]
    [InlineData(long.MaxValue)]
    [InlineData(long.MinValue)]
    public void Unitless_time_without_default_is_an_error(long value)
    {
        (long Value, TimeUnit? Unit) input = (value, null);

        input.ToMilliseconds().Should().BeOfType<Result<string, long>.Err>();
        input.ToSeconds().Should().BeOfType<Result<string, decimal>.Err>();
        input.ToTimespan().Should().BeOfType<Result<string, TimeSpan>.Err>();
    }

    [Theory]
    [InlineData(TimeUnit.Milliseconds, 1L)]
    [InlineData(TimeUnit.Seconds, 1_000L)]
    [InlineData(TimeUnit.Minutes, 60_000L)]
    [InlineData(TimeUnit.Hours, 3_600_000L)]
    public void Milliseconds_conversion_checks_both_Int64_boundaries(
        TimeUnit unit,
        long multiplier)
    {
        var positiveLimit = long.MaxValue / multiplier;
        var negativeLimit = long.MinValue / multiplier;

        (long Value, TimeUnit? Unit) positiveInput = (positiveLimit, unit);
        (long Value, TimeUnit? Unit) negativeInput = (negativeLimit, unit);

        positiveInput.ToMilliseconds()
            .Should().Be(Result<string, long>.ok(positiveLimit * multiplier));

        negativeInput.ToMilliseconds()
            .Should().Be(Result<string, long>.ok(negativeLimit * multiplier));

        if (multiplier > 1)
        {
            (long Value, TimeUnit? Unit) positiveOverflow = (positiveLimit + 1, unit);
            (long Value, TimeUnit? Unit) negativeOverflow = (negativeLimit - 1, unit);
            (long Value, TimeUnit? Unit) implicitOverflow = (positiveLimit + 1, null);

            positiveOverflow.ToMilliseconds().Should().BeOfType<Result<string, long>.Err>();
            negativeOverflow.ToMilliseconds().Should().BeOfType<Result<string, long>.Err>();
            implicitOverflow.ToMilliseconds(unit).Should().BeOfType<Result<string, long>.Err>();
        }
    }

    [Theory]
    [InlineData(TimeUnit.Milliseconds, 1L)]
    [InlineData(TimeUnit.Seconds, 1_000L)]
    [InlineData(TimeUnit.Minutes, 60_000L)]
    [InlineData(TimeUnit.Hours, 3_600_000L)]
    public void Seconds_conversion_preserves_precision_for_entire_Int64_range(
        TimeUnit unit,
        long multiplier)
    {
        foreach (var value in new[] { long.MinValue, long.MaxValue })
        {
            (long Value, TimeUnit? Unit) input = (value, unit);
            (long Value, TimeUnit? Unit) implicitInput = (value, null);

            var expected = Result<string, decimal>.ok(value * (decimal)multiplier / 1_000m);

            input.ToSeconds().Should().Be(expected);
            implicitInput.ToSeconds(unit).Should().Be(expected);
        }
    }

    [Theory]
    [InlineData(TimeUnit.Milliseconds, 1L)]
    [InlineData(TimeUnit.Seconds, 1_000L)]
    [InlineData(TimeUnit.Minutes, 60_000L)]
    [InlineData(TimeUnit.Hours, 3_600_000L)]
    public void Timespan_conversion_checks_both_tick_boundaries(
        TimeUnit unit,
        long millisecondsPerUnit)
    {
        var ticksPerUnit = millisecondsPerUnit * TimeSpan.TicksPerMillisecond;
        var positiveLimit = long.MaxValue / ticksPerUnit;
        var negativeLimit = long.MinValue / ticksPerUnit;

        (long Value, TimeUnit? Unit) positiveInput = (positiveLimit, unit);
        (long Value, TimeUnit? Unit) negativeInput = (negativeLimit, unit);
        (long Value, TimeUnit? Unit) positiveOverflow = (positiveLimit + 1, unit);
        (long Value, TimeUnit? Unit) negativeOverflow = (negativeLimit - 1, unit);
        (long Value, TimeUnit? Unit) implicitOverflow = (negativeLimit - 1, null);
        (long Value, TimeUnit? Unit) largestMagnitude = (long.MaxValue, unit);
        (long Value, TimeUnit? Unit) smallestMagnitude = (long.MinValue, unit);

        positiveInput.ToTimespan().Should().Be(
            Result<string, TimeSpan>.ok(TimeSpan.FromTicks(positiveLimit * ticksPerUnit)));

        negativeInput.ToTimespan().Should().Be(
            Result<string, TimeSpan>.ok(TimeSpan.FromTicks(negativeLimit * ticksPerUnit)));

        positiveOverflow.ToTimespan().Should().BeOfType<Result<string, TimeSpan>.Err>();
        negativeOverflow.ToTimespan().Should().BeOfType<Result<string, TimeSpan>.Err>();
        implicitOverflow.ToTimespan(unit).Should().BeOfType<Result<string, TimeSpan>.Err>();
        largestMagnitude.ToTimespan().Should().BeOfType<Result<string, TimeSpan>.Err>();
        smallestMagnitude.ToTimespan().Should().BeOfType<Result<string, TimeSpan>.Err>();

        positiveOverflow.ToMilliseconds().IsOk().Should().BeTrue();
        negativeOverflow.ToMilliseconds().IsOk().Should().BeTrue();
        positiveOverflow.ToSeconds().IsOk().Should().BeTrue();
        negativeOverflow.ToSeconds().IsOk().Should().BeTrue();
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(4)]
    [InlineData(int.MaxValue)]
    public void Time_conversions_reject_invalid_explicit_or_default_units(int invalidUnit)
    {
        foreach (var value in new[] { 0L, 1L, long.MinValue, long.MaxValue })
        {
            (long Value, TimeUnit? Unit) invalidInput = (value, (TimeUnit)invalidUnit);
            (long Value, TimeUnit? Unit) implicitInput = (value, null);
            (long Value, TimeUnit? Unit) explicitInput = (value, TimeUnit.Milliseconds);

            invalidInput.ToMilliseconds().Should().BeOfType<Result<string, long>.Err>();
            invalidInput.ToSeconds().Should().BeOfType<Result<string, decimal>.Err>();
            invalidInput.ToTimespan().Should().BeOfType<Result<string, TimeSpan>.Err>();

            invalidInput.ToMilliseconds(TimeUnit.Seconds)
                .Should().BeOfType<Result<string, long>.Err>();

            invalidInput.ToSeconds(TimeUnit.Seconds)
                .Should().BeOfType<Result<string, decimal>.Err>();

            invalidInput.ToTimespan(TimeUnit.Seconds)
                .Should().BeOfType<Result<string, TimeSpan>.Err>();

            implicitInput.ToMilliseconds((TimeUnit)invalidUnit)
                .Should().BeOfType<Result<string, long>.Err>();

            implicitInput.ToSeconds((TimeUnit)invalidUnit)
                .Should().BeOfType<Result<string, decimal>.Err>();

            implicitInput.ToTimespan((TimeUnit)invalidUnit)
                .Should().BeOfType<Result<string, TimeSpan>.Err>();

            explicitInput.ToMilliseconds((TimeUnit)invalidUnit)
                .Should().BeOfType<Result<string, long>.Err>();

            explicitInput.ToSeconds((TimeUnit)invalidUnit)
                .Should().BeOfType<Result<string, decimal>.Err>();

            explicitInput.ToTimespan((TimeUnit)invalidUnit)
                .Should().BeOfType<Result<string, TimeSpan>.Err>();
        }
    }

    [Fact]
    public void Parsed_values_chain_into_conversions()
    {
        NumericInput.ParseCount("+1__234 k").AndThen(input => input.ToCount())
            .Should().Be(Result<string, long>.ok(1_234_000));

        NumericInput.ParseTime("-1__234 ms").AndThen(input => input.ToSeconds())
            .Should().Be(Result<string, decimal>.ok(-1.234m));

        NumericInput.ParseTime("+2").AndThen(input => input.ToMilliseconds(TimeUnit.Hours))
            .Should().Be(Result<string, long>.ok(7_200_000));

        NumericInput.ParseTime("2 m").AndThen(input => input.ToTimespan())
            .Should().Be(Result<string, TimeSpan>.ok(TimeSpan.FromMinutes(2)));

        NumericInput.ParseCount("9223372036854775807 G").AndThen(input => input.ToCount())
            .Should().BeOfType<Result<string, long>.Err>();

        NumericInput.ParseTime("-9223372036854775808 h").AndThen(input => input.ToMilliseconds())
            .Should().BeOfType<Result<string, long>.Err>();
    }
}
