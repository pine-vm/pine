namespace Pine.Core.PopularEncodings;

/// <summary>
/// Relay to <see cref="CommonEncodings.StringEncoding"/>
/// </summary>
public class StringEncoding
{
    /// <summary>
    /// Converts a .NET string to a Pine blob value containing UTF-32 encoded characters.
    /// </summary>
    public static PineValue ValueFromString(string str) =>
        CommonEncodings.StringEncoding.ValueFromString(str);
}
