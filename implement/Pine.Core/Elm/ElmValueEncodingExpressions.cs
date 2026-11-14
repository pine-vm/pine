using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;

namespace Pine.Core.Elm;

/// <summary>Reusable Pine constructor expressions for the canonical Elm value encoding.</summary>
public static class ElmValueEncodingExpressions
{
    /// <summary>
    /// Encoded Pine expression producing an Elm string from environment <c>[asciiBlob]</c>.
    /// Input bytes must be ASCII (0 through 127). The result carries the canonical UTF-32 blob.
    /// </summary>
    public static PineValue StringFromAscii => EncodedExpressions.StringFromAscii;

    /// <summary>Content hash of <see cref="StringFromAscii"/>, without initializing the expression.</summary>
    public static ReadOnlySpan<byte> StringFromAsciiHash =>
        [
        0x81, 0x1f, 0xb6, 0x22, 0x5a, 0xca, 0xeb, 0xe8, 0x5c, 0x1b, 0x03, 0x65, 0x43, 0xe8, 0x1f, 0x44,
        0x6d, 0xeb, 0x83, 0xb1, 0x85, 0x86, 0x1d, 0x2e, 0xa2, 0xb1, 0xe2, 0xa1, 0x7d, 0x0b, 0xe1, 0xde
        ];

    private static class EncodedExpressions
    {
        internal static readonly PineValue StringFromAscii = BuildStringFromAscii();
    }

    private static PineValue BuildStringFromAscii()
    {
        static Expression Number(int value) => Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(value));

        static Expression Builtin(string name, params Expression[] arguments) =>
            Expression.BuiltinInst(name, Expression.ListInst(arguments));

        static Expression At(int index) =>
            Expression.BuiltinInst("head", Builtin("skip", Number(index), Expression.EnvironmentInstance));

        var empty = Expression.LitralInst(PineValue.EmptyBlob);
        var self = At(0);
        var bytes = At(1);
        var length = Expression.BuiltinInst("length", bytes);

        // Halve the unsigned magnitude, keeping the signed-integer encoding's positive prefix.
        var half =
            Builtin(
                "concat",
                Expression.LitralInst(PineValue.Blob([4])),
                Builtin("bit_shift_right", Number(1), Builtin("skip", Number(1), length)));

        Expression Recur(Expression part) =>
            new Expression.Eval(self, Expression.ListInst([self, part]));

        var helper =
            Expression.ConditionalInst(
                Builtin("equal", bytes, empty),
                Expression.ConditionalInst(
                    Builtin("equal", length, Number(1)),
                    Builtin(
                        "concat",
                        Recur(Builtin("take", half, bytes)),
                        Recur(Builtin("skip", half, bytes))),
                    Builtin("concat", Expression.LitralInst(PineValue.Blob([0, 0, 0])), bytes)),
                empty);

        var encodedHelper = Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(helper));

        return
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ListInst(
                    [
                    Expression.LitralInst(StringEncoding.ValueFromString(ElmValue.ElmChoiceTypeTagName)),
                    Expression.LitralInst(StringEncoding.ValueFromString(ElmValue.ElmStringTypeTagName)),
                    new Expression.Eval(encodedHelper, Expression.ListInst([encodedHelper, At(0)]))
                    ]));
    }
}
