using System.Threading;

namespace Pine.Core.Elm.Syntax.Concrete;

internal static class SyntaxHashCache
{
    // Syntax children and collections are immutable after construction. Races may compute the same hash
    // more than once; no synchronization is needed around that pure computation.
    // Zero means uninitialized. Encoding in a long leaves room for every int hash, including zero.
    // Publish the value and initialization state together so concurrent readers cannot see a partial result.
    public static bool TryGet(ref long cache, out int hash)
    {
        var encoded = Volatile.Read(ref cache);
        hash = unchecked((int)(encoded - 1));
        return encoded != 0;
    }

    public static int Store(ref long cache, int hash)
    {
        Volatile.Write(ref cache, (long)unchecked((uint)hash) + 1);
        return hash;
    }

    // Match the compiler's positional-record hash combination, rather than System.HashCode.Combine.
    public static int Combine<T>(int hash, T value) =>
        unchecked(hash * -1521134295 + System.Collections.Generic.EqualityComparer<T>.Default.GetHashCode(value!));
}
