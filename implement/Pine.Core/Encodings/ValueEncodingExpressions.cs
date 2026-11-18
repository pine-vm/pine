using Pine.Core.PineLanguage;
using System;

namespace Pine.Core.Encodings;

/// <summary>Reusable Pine expressions for frontend-independent value encoding specializations.</summary>
public static class ValueEncodingExpressions
{
    /// <summary>
    /// Encoded Pine expression consuming environment <c>[blob]</c> and returning a blob
    /// with three zero bytes prepended to each input byte. All byte values are preserved.
    /// </summary>
    public static PineValue BlobPadBytesTo4 => EncodedExpressions.BlobPadBytesTo4;

    /// <summary>Content hash of <see cref="BlobPadBytesTo4"/>, without initializing the expression.</summary>
    public static ReadOnlySpan<byte> BlobPadBytesTo4Hash =>
        [
        0x4c, 0xc8, 0x72, 0x14, 0xc8, 0x02, 0x76, 0x40, 0x5a, 0x8e, 0xb6, 0x92, 0x8e, 0x14, 0x0c, 0xb9,
        0x77, 0xcc, 0x32, 0x5d, 0x6b, 0x19, 0x60, 0xe8, 0x02, 0xa4, 0x63, 0x3e, 0x89, 0x7f, 0xc9, 0x87
        ];

    private static class EncodedExpressions
    {
        internal static readonly PineValue BlobPadBytesTo4 = BuildBlobPadBytesTo4();
    }

    private static PineValue BuildBlobPadBytesTo4()
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
                new Expression.Eval(encodedHelper, Expression.ListInst([encodedHelper, At(0)])));
    }
}
