using System;

namespace Pine.Core.Runtime.InternalExpression;

/// <summary>Release-private identifiers, not Pine wire-format function names.</summary>
public enum BuiltinFunction
{
    /// <summary>Equality of all input elements or bytes.</summary>
    Equal,
    /// <summary>Sequence length.</summary>
    Length,
    /// <summary>First list element.</summary>
    Head,
    /// <summary>Drop a prefix.</summary>
    Skip,
    /// <summary>Take a prefix.</summary>
    Take,
    /// <summary>Concatenate sequences.</summary>
    Concat,
    /// <summary>Reverse a sequence.</summary>
    Reverse,
    /// <summary>Flip the encoded sign.</summary>
    Negate,
    /// <summary>Integer addition.</summary>
    IntAdd,
    /// <summary>Integer multiplication.</summary>
    IntMul,
    /// <summary>Ascending integer order.</summary>
    IntIsSortedAsc,
    /// <summary>Bitwise conjunction.</summary>
    BitAnd,
    /// <summary>Bitwise disjunction.</summary>
    BitOr,
    /// <summary>Bitwise exclusive disjunction.</summary>
    BitXor,
    /// <summary>Bitwise complement.</summary>
    BitNot,
    /// <summary>Left shift.</summary>
    BitShiftLeft,
    /// <summary>Right shift.</summary>
    BitShiftRight
}

/// <summary>Exact, case-sensitive conversion of Pine builtin names.</summary>
public static class BuiltinFunctions
{
    /// <summary>Rejects unknown names rather than giving them a success-shaped fallback.</summary>
    public static BuiltinFunction FromName(string name) =>
        TryFromName(name, out var function)
        ?
        function
        :
        throw new ArgumentException("Unknown Pine builtin function: " + name, nameof(name));

    /// <summary>Parses a name without exceptions, for value-decoder error results.</summary>
    public static bool TryFromName(string name, out BuiltinFunction function)
    {
        BuiltinFunction? parsed =
            name switch
            {
                "equal" => BuiltinFunction.Equal,
                "length" => BuiltinFunction.Length,
                "head" => BuiltinFunction.Head,
                "skip" => BuiltinFunction.Skip,
                "take" => BuiltinFunction.Take,
                "concat" => BuiltinFunction.Concat,
                "reverse" => BuiltinFunction.Reverse,
                "negate" => BuiltinFunction.Negate,
                "int_add" => BuiltinFunction.IntAdd,
                "int_mul" => BuiltinFunction.IntMul,
                "int_is_sorted_asc" => BuiltinFunction.IntIsSortedAsc,
                "bit_and" => BuiltinFunction.BitAnd,
                "bit_or" => BuiltinFunction.BitOr,
                "bit_xor" => BuiltinFunction.BitXor,
                "bit_not" => BuiltinFunction.BitNot,
                "bit_shift_left" => BuiltinFunction.BitShiftLeft,
                "bit_shift_right" => BuiltinFunction.BitShiftRight,

                _ =>
                null
            };

        function = parsed.GetValueOrDefault();
        return parsed.HasValue;
    }

    /// <summary>Rejects undefined enum values.</summary>
    public static string ToName(BuiltinFunction function) =>
        function switch
        {
            BuiltinFunction.Equal => "equal",
            BuiltinFunction.Length => "length",
            BuiltinFunction.Head => "head",
            BuiltinFunction.Skip => "skip",
            BuiltinFunction.Take => "take",
            BuiltinFunction.Concat => "concat",
            BuiltinFunction.Reverse => "reverse",
            BuiltinFunction.Negate => "negate",
            BuiltinFunction.IntAdd => "int_add",
            BuiltinFunction.IntMul => "int_mul",
            BuiltinFunction.IntIsSortedAsc => "int_is_sorted_asc",
            BuiltinFunction.BitAnd => "bit_and",
            BuiltinFunction.BitOr => "bit_or",
            BuiltinFunction.BitXor => "bit_xor",
            BuiltinFunction.BitNot => "bit_not",
            BuiltinFunction.BitShiftLeft => "bit_shift_left",
            BuiltinFunction.BitShiftRight => "bit_shift_right",

            _ =>
            throw new ArgumentOutOfRangeException(nameof(function), function, "Unknown Pine builtin identifier.")
        };
}
