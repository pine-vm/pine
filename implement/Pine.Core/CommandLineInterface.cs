using System;

namespace Pine.Core;

/// <summary>
/// Functions for displaying information in a command-line interface.
/// </summary>
public class CommandLineInterface
{
    /// <summary>
    /// Converts the specified 64-bit signed <paramref name="integer"/> to its
    /// decimal string representation, inserting underscores every three digits
    /// for readability (e.g. <c>1_234_567</c>).
    /// </summary>
    /// <param name="integer">The value to format.</param>
    /// <returns>
    /// A culture-invariant string containing <paramref name="integer"/> with
    /// digit groups separated by underscores.
    /// </returns>
    /// <example>
    /// <code>
    /// Console.WriteLine(CommandLineInterface.FormatIntegerForDisplay(1234567));
    /// // Prints "1_234_567"
    /// </code>
    /// </example>
    public static string FormatIntegerForDisplay(long integer) =>
        FormatIntegerForDisplay(integer, '_');

    /// <summary>
    /// Converts the specified 64-bit signed <paramref name="integer"/> to its
    /// decimal string representation, inserting the given
    /// <paramref name="thousandsSeparator"/> every three digits.
    /// </summary>
    /// <param name="integer">The value to format.</param>
    /// <param name="thousandsSeparator">
    /// The character to insert between digit groups (for example ‘ ’ for a thin
    /// space or ‘,’ for a comma).
    /// </param>
    /// <returns>A string containing the formatted number.</returns>
    public static string FormatIntegerForDisplay(
        long integer,
        char thousandsSeparator)
    {
        // 19 digits + 6 underscores + sign = 26 – round up for safety
        Span<char> buffer = stackalloc char[27];

        var remainder = (ulong)integer;
        var neg = false;

        if (integer < 0)
        {
            neg = true;
            remainder = (ulong)-integer;      // avoid costly Math.Abs on long.MinValue
        }

        var pos = buffer.Length;
        var digitInGroup = 0;

        do
        {
            if (digitInGroup is 3)
            {
                buffer[--pos] = thousandsSeparator;
                digitInGroup = 0;
            }

            buffer[--pos] = (char)('0' + (remainder % 10));
            remainder /= 10;
            digitInGroup++;

        } while (remainder is not 0);

        if (neg) buffer[--pos] = '-';

        return new string(buffer[pos..]);
    }
}
