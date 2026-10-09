using System;
using System.Numerics;

namespace Pine.Core.CLI;

/// <summary>
/// Converts parsed numeric inputs, returning errors for invalid units or overflowing results.
/// </summary>
public static class NumericInputExtensions
{
    /// <summary>
    /// Applies the decimal SI multiplier, checking the resulting Int64 range.
    /// </summary>
    public static Result<string, long> ToCount(this (long Value, CountUnit Unit) input)
    {
        var multiplier =
            input.Unit switch
            {
                CountUnit.None => 1L,
                CountUnit.Kilo => 1_000L,
                CountUnit.Mega => 1_000_000L,
                CountUnit.Giga => 1_000_000_000L,

                _ =>
                0L,
            };

        if (multiplier is 0)
            return "Invalid count unit.";

        return ToInt64((BigInteger)input.Value * multiplier, "Count");
    }

    /// <summary>
    /// Converts to integer milliseconds. A default unit is required for unitless inputs;
    /// an explicit unit overrides a valid default.
    /// </summary>
    public static Result<string, long> ToMilliseconds(
        this (long Value, TimeUnit? Unit) input,
        TimeUnit? defaultUnit = null) =>
        ResolveMillisecondsPerUnit(input.Unit, defaultUnit)
        .AndThen(
            multiplier =>
            ToInt64((BigInteger)input.Value * multiplier, "Milliseconds"));

    /// <summary>
    /// Converts to exact decimal seconds, retaining millisecond fractions.
    /// A default unit is required for unitless inputs; an explicit unit overrides a valid default.
    /// </summary>
    public static Result<string, decimal> ToSeconds(
        this (long Value, TimeUnit? Unit) input,
        TimeUnit? defaultUnit = null) =>
        ResolveMillisecondsPerUnit(input.Unit, defaultUnit)
        .Map(multiplier => input.Value * (decimal)multiplier / 1_000m);

    /// <summary>
    /// Converts to an exact time span, checking the resulting tick range.
    /// A default unit is required for unitless inputs; an explicit unit overrides a valid default.
    /// </summary>
    public static Result<string, TimeSpan> ToTimespan(
        this (long Value, TimeUnit? Unit) input,
        TimeUnit? defaultUnit = null) =>
        ResolveMillisecondsPerUnit(input.Unit, defaultUnit)
        .AndThen(
            multiplier =>
            ToInt64(
                (BigInteger)input.Value * multiplier * TimeSpan.TicksPerMillisecond,
                "Time span"))
        .Map(TimeSpan.FromTicks);

    private static Result<string, long> ResolveMillisecondsPerUnit(
        TimeUnit? unit,
        TimeUnit? defaultUnit)
    {
        if (defaultUnit.HasValue && MillisecondsPerUnit(defaultUnit.Value) is 0)
            return "Invalid default time unit.";

        var resolvedUnit = unit ?? defaultUnit;

        if (!resolvedUnit.HasValue)
            return "A time unit or default time unit is required.";

        var multiplier = MillisecondsPerUnit(resolvedUnit.Value);

        return
            multiplier is 0
            ?
            Result<string, long>.err("Invalid time unit.")
            :
            Result<string, long>.ok(multiplier);
    }

    private static long MillisecondsPerUnit(TimeUnit unit) =>
        unit switch
        {
            TimeUnit.Milliseconds => 1L,
            TimeUnit.Seconds => 1_000L,
            TimeUnit.Minutes => 60_000L,
            TimeUnit.Hours => 3_600_000L,

            _ =>
            0L,
        };

    private static Result<string, long> ToInt64(BigInteger value, string quantity) =>
        value < long.MinValue || value > long.MaxValue
        ?
        Result<string, long>.err(quantity + " is outside the Int64 range.")
        :
        Result<string, long>.ok((long)value);
}
