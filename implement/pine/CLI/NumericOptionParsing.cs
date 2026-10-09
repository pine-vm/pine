using Pine.Core;
using Pine.Core.CLI;
using System;
using System.CommandLine;
using System.Globalization;

namespace Pine.CLI;

/// <summary>
/// Configures numeric options with shared parsing and checked narrowing to their existing value types.
/// Optional validators receive successfully parsed values without re-entering System.CommandLine value conversion.
/// </summary>
public static class NumericOptionParsing
{
    /// <summary>Accepts integers, including internal underscores, without units.</summary>
    public static void SetIntegerParser(Option<int> option, Func<int, string?>? validate = null) =>
        SetParser(option, input => NumericInput.ParseInteger(input).AndThen(ToInt32), validate);

    /// <summary>Accepts nullable integers, including internal underscores, without units.</summary>
    public static void SetIntegerParser(Option<int?> option, Func<int?, string?>? validate = null) =>
        SetParser(
            option,
            input => NumericInput.ParseInteger(input).AndThen(ToInt32).Map<int?>(value => value),
            validate);

    /// <summary>Accepts unsigned integers, including internal underscores, without units.</summary>
    public static void SetIntegerParser(Option<uint> option) =>
        SetParser(option, input => NumericInput.ParseInteger(input).AndThen(ToUInt32));

    /// <summary>Accepts nullable unsigned integers, including internal underscores, without units.</summary>
    public static void SetIntegerParser(Option<uint?> option) =>
        SetParser(option, input => NumericInput.ParseInteger(input).AndThen(ToUInt32).Map<uint?>(value => value));

    /// <summary>Accepts integer counts, including internal underscores and decimal SI units.</summary>
    public static void SetCountParser(Option<int> option, Func<int, string?>? validate = null) =>
        SetParser(option, input => ParseCount(input).AndThen(ToInt32), validate);

    /// <summary>Accepts nullable counts, including internal underscores and decimal SI units.</summary>
    public static void SetCountParser(Option<int?> option, Func<int?, string?>? validate = null) =>
        SetParser(option, input => ParseCount(input).AndThen(ToInt32).Map<int?>(value => value), validate);

    /// <summary>Accepts unsigned counts, including internal underscores and decimal SI units.</summary>
    public static void SetCountParser(Option<uint> option, Func<uint, string?>? validate = null) =>
        SetParser(option, input => ParseCount(input).AndThen(ToUInt32), validate);

    /// <summary>Accepts nullable unsigned counts, including internal underscores and decimal SI units.</summary>
    public static void SetCountParser(Option<uint?> option, Func<uint?, string?>? validate = null) =>
        SetParser(option, input => ParseCount(input).AndThen(ToUInt32).Map<uint?>(value => value), validate);

    /// <summary>
    /// Accepts integer times with units and legacy unitless fractional or exponent values, returning seconds.
    /// </summary>
    public static void SetTimeParser(
        Option<double> option,
        TimeUnit defaultUnit,
        Func<double, string?>? validate = null) =>
        SetParser(option, input => ParseSeconds(input, defaultUnit), validate);

    /// <summary>
    /// Accepts nullable integer times with units and legacy unitless fractional or exponent values, returning seconds.
    /// </summary>
    public static void SetTimeParser(
        Option<double?> option,
        TimeUnit defaultUnit,
        Func<double?, string?>? validate = null) =>
        SetParser(
            option,
            input => ParseSeconds(input, defaultUnit).Map<double?>(value => value),
            validate);

    private static void SetParser<T>(
        Option<T> option,
        Func<string?, Result<string, T>> parse,
        Func<T, string?>? validate = null)
    {
        option.Arity = ArgumentArity.ExactlyOne;
        option.AllowMultipleArgumentsPerToken = false;

        option.CustomParser =
            result =>
            {
                if (result.Tokens.Count != 1)
                {
                    result.AddError(option.Name + " requires exactly one numeric value.");
                    return default!;
                }

                return
                    parse(result.Tokens[0].Value)
                    .Unpack(
                        error =>
                        {
                            result.AddError(option.Name + ": " + error);
                            return default!;
                        },
                        value =>
                        {
                            if (validate?.Invoke(value) is { } error)
                                result.AddError(error);

                            return value;
                        });
            };
    }

    private static Result<string, long> ParseCount(string? input) =>
        NumericInput.ParseCount(input).AndThen(value => value.ToCount());

    private static Result<string, int> ToInt32(long value) =>
        value < int.MinValue || value > int.MaxValue
        ?
        Result<string, int>.err("Value is outside the signed 32-bit integer range.")
        :
        Result<string, int>.ok((int)value);

    private static Result<string, uint> ToUInt32(long value) =>
        value < uint.MinValue || value > uint.MaxValue
        ?
        Result<string, uint>.err("Value is outside the unsigned 32-bit integer range.")
        :
        Result<string, uint>.ok((uint)value);

    private static Result<string, double> ParseSeconds(string? input, TimeUnit defaultUnit)
    {
        var parsed =
            NumericInput.ParseTime(input)
            .AndThen(value => value.ToSeconds(defaultUnit))
            .Map(seconds => (double)seconds);

        if (parsed.IsOk())
            return parsed;

        // Only legacy floating-point syntax may bypass the integer parser, never malformed underscores or units.
        if (input is null || input.Contains('_') ||
            (input.IndexOf('.') < 0 && input.IndexOf('e') < 0 && input.IndexOf('E') < 0) ||
            !double.TryParse(input, NumberStyles.Float, CultureInfo.InvariantCulture, out var magnitude))
        {
            return parsed;
        }

        var secondsPerUnit =
            defaultUnit switch
            {
                TimeUnit.Milliseconds => 0.001,
                TimeUnit.Seconds => 1,
                TimeUnit.Minutes => 60,
                TimeUnit.Hours => 3600,

                _ =>
                0,
            };

        if (secondsPerUnit == 0)
            return "Invalid default time unit.";

        var seconds = magnitude * secondsPerUnit;

        return
            double.IsFinite(seconds)
            ?
            Result<string, double>.ok(seconds)
            :
            Result<string, double>.err("Time must be finite and fit the numeric range.");
    }
}
