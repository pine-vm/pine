using System.Globalization;

namespace Pine;

public class CommandLineInterface
{
    public static string FormatIntegerForDisplay(long integer) =>
        FormatIntegerForDisplay(integer, '_');

    public static string FormatIntegerForDisplay(long integer, char thousandsSeparator) =>
        integer.ToString("N",
            new NumberFormatInfo
            {
                NumberGroupSizes = [3, 3, 3, 3, 3, 3],
                NumberGroupSeparator = thousandsSeparator.ToString(),
                NumberDecimalDigits = 0
            });
}
