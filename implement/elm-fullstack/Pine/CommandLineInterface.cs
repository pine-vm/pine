namespace Pine;

public class CommandLineInterface
{
    static public string FormatIntegerForDisplay(long integer) =>
        integer.ToString("### ### ### ### ### ### ##0").Trim();
}
