namespace Pine.Core.PopularEncodings;

/// <summary>
/// Relay to <see cref="CommonEncodings.IntegerEncoding"/>
/// </summary>
public class IntegerEncoding
{
    /// <summary>
    /// Encode the given integer in signed form.
    /// </summary>
    public static PineValue EncodeSignedInteger(System.Numerics.BigInteger integer) =>
        CommonEncodings.IntegerEncoding.EncodeSignedInteger(integer);
}
