using System.Globalization;

namespace Pine.Core;

/// <summary>
/// Functions for displaying information in a command-line interface.
/// </summary>
public class CommandLineInterface
{
    /// <summary>
    /// Formats an integer into a string with underscores as thousands separators.
    /// </summary>
    public static string FormatIntegerForDisplay(long integer) =>
        FormatIntegerForDisplay(integer, '_');

    /// <summary>
    /// Formats an integer into a string with thousands separators.
    /// </summary>
    public static string FormatIntegerForDisplay(long integer, char thousandsSeparator) =>
        integer.ToString("N",
            BuildNumberFormatInfo(thousandsSeparator));

    private static NumberFormatInfo BuildNumberFormatInfo(char thousandsSeparator) =>
        new()
        {
            NumberGroupSizes = [3, 3, 3, 3, 3, 3],
            NumberGroupSeparator = thousandsSeparator.ToString(),
            NumberDecimalDigits = 0
        };
}
