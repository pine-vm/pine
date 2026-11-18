using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.IO;
using System.Runtime.InteropServices;

using TreeReader = Pine.Core.Encodings.PineTreeBinaryEncoding.TreeDecoder<object>;

namespace Pine.Core.Runtime.InternalExpression;

public static partial class ExpressionBinary
{
    /// <summary>Reads a root directly into the retained model; trailing bytes remain available.</summary>
    public static Expression Decode(ReadOnlyMemory<byte> bytes, out int bytesRead)
    {
        using var decoder = new Decoder(bytes);
        var result = decoder.ReadExpression();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>Shares completed retained references across roots. Input must remain immutable.</summary>
    public ref struct Decoder
    {
        private TreeReader _reader;

        /// <summary>Indexes references without materializing canonical expressions or values.</summary>
        public Decoder(ReadOnlyMemory<byte> source)
        {
            try
            {
                _reader = new(source);
            }
            catch (PineTreeBinaryEncoding.DecodeException error)
            {
                throw new InvalidDataException(error.Message, error);
            }
        }

        /// <summary>The consumed extent, for embedding.</summary>
        public readonly int BytesRead => _reader.BytesRead;

        /// <summary>Reads exactly one retained root.</summary>
        public Expression ReadExpression()
        {
            try
            {
                return Read(ref _reader);
            }
            catch (PineTreeBinaryEncoding.DecodeException error)
            {
                throw new InvalidDataException(error.Message, error);
            }
        }

        /// <summary>Returns temporary reference storage.</summary>
        public void Dispose() => _reader.Dispose();
    }

    private static Expression Read(ref TreeReader reader) => reader.Read<Expression, ExpressionDecoding>(default);

    private static PineValue Value(ref TreeReader reader) =>
        reader.Read<PineValue, PineTreeBinaryEncoding.PineDecoding<object>>(default);

    private static int Int32(ref TreeReader reader)
    {
        if (!TryCanonicalInt32(reader.ReadInlineBlob().Span, out var result))
            throw reader.Invalid("Expected a canonical signed Int32");

        return result;
    }

    internal static bool TryCanonicalInt32(ReadOnlySpan<byte> bytes, out int result)
    {
        result = default;

        if (bytes.Length is < 2 or > 5 || bytes[0] is not (2 or 4) ||
            (bytes.Length > 2 && bytes[1] is 0) || (bytes[0] is 2 && bytes.Length is 2 && bytes[1] is 0))
        {
            return false;
        }

        uint magnitude = 0;

        foreach (var item in bytes[1..])
            magnitude = (magnitude << 8) | item;

        var integer = bytes[0] is 2 ? -(long)magnitude : magnitude;

        if (integer < int.MinValue || integer > int.MaxValue)
            return false;

        result = (int)integer;
        return true;
    }

    private static void Arity(ref TreeReader reader, int actual, int expected)
    {
        if (actual != expected)
            throw reader.Invalid("Invalid expression constructor environment arity");
    }

    private readonly struct ExpressionDecoding : PineTreeBinaryEncoding.IDecoding<Expression, object>
    {
        public Expression Blob(ReadOnlyMemory<byte> bytes) =>
            throw new InvalidDataException("Expected an expression constructor, not a blob.");

        public Expression List(Expression[] items) =>
            throw new InvalidDataException("Expected an expression constructor, not a list.");

        public Expression? ResolveExternal(ReadOnlyMemory<byte> reference) => null;

        public ValueResult<string, Expression> Evaluate(Expression expression, Expression environment) =>
            "Expected an external constructor expression.";

        public PineTreeBinaryEncoding.DecodeError Rejected(
            long offset, Expression expression, Expression environment, string reason) =>
            throw new InvalidDataException(reason);

        public bool HasExternalEvaluation => true;

        public int MaximumDepth => 512;

        public Expression EvaluateExternal(ref TreeReader reader, ReadOnlyMemory<byte> reference)
        {
            for (var index = 0; index < s_constructorHashes.Length / 32; index++)
            {
                if (ConstructorHash((Constructor)index).SequenceEqual(reference.Span))
                    return Construct(ref reader, (Constructor)index, reader.ReadInlineListLength());
            }

            throw reader.Invalid("Unknown internal expression constructor hash");
        }
    }

    private static Expression Construct(ref TreeReader reader, Constructor kind, int count)
    {
        switch (kind)
        {
            case Constructor.Literal:
                Arity(ref reader, count, 1);
                return new Expression.Literal(Value(ref reader));

            case Constructor.List:
                if (count > reader.Remaining / 8)
                    throw reader.Invalid("Invalid expression list length");

                var items = count is 0 ? [] : new Expression[count];

                for (var index = 0; index < count; index++)
                    items[index] = Read(ref reader);

                return new Expression.List(ImmutableCollectionsMarshal.AsImmutableArray(items));

            case Constructor.Eval:
                Arity(ref reader, count, 2);
                return new Expression.Eval(Read(ref reader), Read(ref reader));

            case Constructor.Builtin:
                Arity(ref reader, count, 2);
                var function = Int32(ref reader);

                if (!Enum.IsDefined((BuiltinFunction)function))
                    throw reader.Invalid("Unknown internal builtin identifier");

                return new Expression.Builtin((BuiltinFunction)function, Read(ref reader));

            case Constructor.Conditional:
                Arity(ref reader, count, 3);
                return new Expression.Conditional(Read(ref reader), Read(ref reader), Read(ref reader));

            case Constructor.Environment:
                Arity(ref reader, count, 0);
                return Expression.Environment.Instance;

            case Constructor.Label:
                Arity(ref reader, count, 2);
                return new Expression.Label(Value(ref reader), Read(ref reader));

            case Constructor.BuiltinBinary:
                Arity(ref reader, count, 3);
                var binaryFunction = Int32(ref reader);

                if (!Enum.IsDefined((BuiltinFunction)binaryFunction))
                    throw reader.Invalid("Unknown internal builtin identifier");

                return new Expression.BuiltinBinary((BuiltinFunction)binaryFunction, Read(ref reader), Read(ref reader));

            case Constructor.Length_Equal_Binary:
                Arity(ref reader, count, 3);
                var sequence = Read(ref reader);
                var other = Read(ref reader);
                var lengthOnLeft = Int32(ref reader);

                if (lengthOnLeft is not (0 or 1))
                    throw reader.Invalid("Invalid length-equality ordering flag");

                return new Expression.Length_Equal_Binary(sequence, other, lengthOnLeft is 1);

            case Constructor.HeadSkip:
                Arity(ref reader, count, 2);
                return new Expression.HeadSkip(Int32(ref reader), Read(ref reader));

            default:
                throw new NotImplementedException(nameof(Construct) + " does not handle constructor: " + kind);
        }
    }
}
