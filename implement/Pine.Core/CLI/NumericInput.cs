using System;
using System.Globalization;

namespace Pine.Core.CLI;

/// <summary>
/// Parses invariant, signed decimal integers with optional internal underscores and units.
/// </summary>
public static class NumericInput
{
    /// <summary>
    /// Parses a signed integer without a unit. Underscores may repeat between the first and last digits.
    /// </summary>
    public static Result<string, long> ParseInteger(string? input) =>
        ParseNumberAndUnit(input)
        .AndThen(
            parsed =>
            parsed.Unit.Length is 0
            ?
            Result<string, long>.ok(parsed.Value)
            :
            Result<string, long>.err("An integer cannot include a unit."));

    /// <summary>
    /// Parses an integer count with an optional, case-sensitive decimal SI suffix: k, M, or G.
    /// The magnitude is not multiplied by the unit during parsing.
    /// </summary>
    public static Result<string, (long Value, CountUnit Unit)> ParseCount(string? input) =>
        ParseNumberAndUnit(input)
        .AndThen(
            parsed =>
            parsed.Unit switch
            {
                "" => Result<string, (long, CountUnit)>.ok((parsed.Value, CountUnit.None)),
                "k" => Result<string, (long, CountUnit)>.ok((parsed.Value, CountUnit.Kilo)),
                "M" => Result<string, (long, CountUnit)>.ok((parsed.Value, CountUnit.Mega)),
                "G" => Result<string, (long, CountUnit)>.ok((parsed.Value, CountUnit.Giga)),

                _ =>
                Result<string, (long, CountUnit)>.err("Invalid count unit: " + parsed.Unit),
            });

    /// <summary>
    /// Parses an integer time with an optional, case-sensitive unit: ms, s, min, m, h,
    /// or the singular or plural full name. Unitless inputs retain a null unit.
    /// </summary>
    public static Result<string, (long Value, TimeUnit? Unit)> ParseTime(string? input) =>
        ParseNumberAndUnit(input)
        .AndThen(
            parsed =>
            parsed.Unit switch
            {
                "" => Result<string, (long, TimeUnit?)>.ok((parsed.Value, null)),

                "ms" or "millisecond" or "milliseconds" =>
                Result<string, (long, TimeUnit?)>.ok((parsed.Value, TimeUnit.Milliseconds)),

                "s" or "second" or "seconds" =>
                Result<string, (long, TimeUnit?)>.ok((parsed.Value, TimeUnit.Seconds)),

                "min" or "m" or "minute" or "minutes" =>
                Result<string, (long, TimeUnit?)>.ok((parsed.Value, TimeUnit.Minutes)),

                "h" or "hour" or "hours" =>
                Result<string, (long, TimeUnit?)>.ok((parsed.Value, TimeUnit.Hours)),

                _ =>
                Result<string, (long, TimeUnit?)>.err("Invalid time unit: " + parsed.Unit),
            });

    private static Result<string, (long Value, string Unit)> ParseNumberAndUnit(string? input)
    {
        if (input is null)
            return "Numeric input is required.";

        var text = input.AsSpan().Trim();

        if (text.IsEmpty)
            return "Numeric input is required.";

        var firstDigit = text[0] is '+' or '-' ? 1 : 0;

        if (firstDigit >= text.Length || !IsAsciiDigit(text[firstDigit]))
            return "Expected a signed decimal integer.";

        var numberEnd = firstDigit + 1;

        while (numberEnd < text.Length &&
            (IsAsciiDigit(text[numberEnd]) || text[numberEnd] is '_'))
        {
            ++numberEnd;
        }

        if (!IsAsciiDigit(text[numberEnd - 1]))
            return "Underscores must be between the first and last digits.";

        var numberText =
            text[..numberEnd].ToString().Replace("_", "", StringComparison.Ordinal);

        if (!long.TryParse(
            numberText,
            NumberStyles.AllowLeadingSign,
            CultureInfo.InvariantCulture,
            out var value))
        {
            return "Integer magnitude is outside the Int64 range.";
        }

        return (value, text[numberEnd..].Trim().ToString());
    }

    private static bool IsAsciiDigit(char character) =>
        character is >= '0' and <= '9';
}
