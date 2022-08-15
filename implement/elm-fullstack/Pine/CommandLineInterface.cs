using System.Globalization;

namespace Pine;

public class CommandLineInterface
{
    static public string FormatIntegerForDisplay(long integer) =>
        FormatIntegerForDisplay(integer, '_');

    static public string FormatIntegerForDisplay(long integer, char thousandsSeparator) =>
        integer.ToString("N",
            new NumberFormatInfo
            {
                NumberGroupSizes = new[] { 3, 3, 3, 3, 3, 3 },
                NumberGroupSeparator = thousandsSeparator.ToString(),
                NumberDecimalDigits = 0
            });
}
