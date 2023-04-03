using System.Numerics;
using System.Runtime.CompilerServices;

namespace Pine;

/// <summary>
/// https://github.com/dotnet/runtime/blob/e05944dd74118db06d6987fe2724813950725737/src/libraries/System.Private.CoreLib/src/System/Text/UnicodeUtility.cs
/// </summary>
public class UnicodeUtility
{
    /// <summary>
    /// Returns <see langword="true"/> iff <paramref name="value"/> is a valid Unicode scalar
    /// value, i.e., is in [ U+0000..U+D7FF ], inclusive; or [ U+E000..U+10FFFF ], inclusive.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool IsValidUnicodeScalar(uint value)
    {
        // This is an optimized check that on x86 is just three instructions: lea, xor, cmp.
        //
        // After the subtraction operation, the input value is modified as such:
        // [ 00000000..0010FFFF ] -> [ FFEF0000..FFFFFFFF ]
        //
        // We now want to _exclude_ the range [ FFEFD800..FFEFDFFF ] (surrogates) from being valid.
        // After the xor, this particular exclusion range becomes [ FFEF0000..FFEF07FF ].
        //
        // So now the range [ FFEF0800..FFFFFFFF ] contains all valid code points,
        // excluding surrogates. This allows us to perform a single comparison.

        return ((value - 0x110000u) ^ 0xD800u) >= 0xFFEF0800u;
    }

    public static bool IsValidUnicodeScalar(BigInteger value) =>
        value > 0 && value < int.MaxValue && IsValidUnicodeScalar((uint)value);
}
