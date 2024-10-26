using System.Globalization;

namespace Pine.Core;

public class CommandLineInterface
{
    public static string FormatIntegerForDisplay(long integer) =>
        FormatIntegerForDisplay(integer, '_');

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
