using System;
using System.Buffers;
using System.Buffers.Binary;
using System.IO;
using System.Numerics;
using System.Text;
using System.Text.Unicode;

namespace Pine.Core.Encodings;

internal static class BinaryEncoding
{
    private static readonly UTF8Encoding s_utf8 = new(false, true);

    internal sealed class Writer(Action<ReadOnlySpan<byte>> write) : IDisposable
    {
        private byte[]? _buffer = ArrayPool<byte>.Shared.Rent(4096);

        private int _position;

        public void Flush()
        {
            var buffer = Buffer;

            if (_position is 0)
                return;

            var count = _position;
            _position = 0;
            write(buffer.AsSpan(0, count));
        }

        private byte[] Buffer =>
            _buffer ?? throw new ObjectDisposedException(nameof(Writer));

        private Span<byte> Reserve(int count)
        {
            if (Buffer.Length - _position < count)
                Flush();

            var result = Buffer.AsSpan(_position, count);
            _position += count;
            return result;
        }

        public void Byte(byte value) => Reserve(1)[0] = value;

        public void Int32(int value) => BinaryPrimitives.WriteInt32BigEndian(Reserve(4), value);

        public void Int64(long value) => BinaryPrimitives.WriteInt64BigEndian(Reserve(8), value);

        public void Bytes(ReadOnlySpan<byte> bytes)
        {
            if (bytes.Length <= Buffer.Length)
            {
                bytes.CopyTo(Reserve(bytes.Length));
                return;
            }

            Flush();
            write(bytes);
        }

        public void String(string value)
        {
            var count = s_utf8.GetByteCount(value);
            Int32(count);

            if (count <= Buffer.Length)
            {
                s_utf8.GetBytes(value.AsSpan(), Reserve(count));
                return;
            }

            var remaining = value.AsSpan();

            while (!remaining.IsEmpty)
            {
                if (Buffer.Length - _position < 4)
                    Flush();

                var status =
                    Utf8.FromUtf16(
                        remaining,
                        Buffer.AsSpan(_position),
                        out var charsRead,
                        out var bytesWritten,
                        replaceInvalidSequences: false);

                _position += bytesWritten;
                remaining = remaining[charsRead..];

                if (status is not (OperationStatus.Done or OperationStatus.DestinationTooSmall))
                    throw new EncoderFallbackException("Invalid UTF-16 in encoded string.");

                if (status is OperationStatus.DestinationTooSmall)
                    Flush();
            }
        }

        public void Integer(BigInteger value)
        {
            var count = value.GetByteCount();
            Int32(count);

            if (count <= Buffer.Length)
            {
                if (!value.TryWriteBytes(Reserve(count), out _, isBigEndian: true))
                    throw new InvalidOperationException("Failed writing an integer.");

                return;
            }

            var rented = ArrayPool<byte>.Shared.Rent(count);

            try
            {
                if (!value.TryWriteBytes(rented.AsSpan(0, count), out _, isBigEndian: true))
                    throw new InvalidOperationException("Failed writing an integer.");

                Bytes(rented.AsSpan(0, count));
            }
            finally
            {
                ArrayPool<byte>.Shared.Return(rented);
            }
        }

        public void Dispose()
        {
            if (_buffer is not { } buffer)
                return;

            try
            {
                Flush();
            }
            finally
            {
                _buffer = null;
                ArrayPool<byte>.Shared.Return(buffer);
            }
        }
    }

    internal ref struct Reader(ReadOnlyMemory<byte> source)
    {
        private readonly ReadOnlyMemory<byte> _source = source;

        public int Position { get; private set; }

        public int Remaining => _source.Length - Position;

        public ReadOnlyMemory<byte> Memory(int count)
        {
            if (count < 0 || count > Remaining)
                throw Error("Truncated or invalid length");

            var result = _source.Slice(Position, count);
            Position += count;
            return result;
        }

        public byte Byte() => Memory(1).Span[0];

        public int Int32() => BinaryPrimitives.ReadInt32BigEndian(Memory(4).Span);

        public long Int64() => BinaryPrimitives.ReadInt64BigEndian(Memory(8).Span);

        public int Count()
        {
            var count = Int32();

            if (count < 0 || count > Remaining)
                throw Error("Invalid collection length");

            return count;
        }

        public string String()
        {
            var bytes = Memory(Int32());

            try
            {
                return s_utf8.GetString(bytes.Span);
            }
            catch (DecoderFallbackException error)
            {
                throw new InvalidDataException("Invalid UTF-8 at offset " + Position + ".", error);
            }
        }

        public BigInteger Integer()
        {
            var bytes = Memory(Int32()).Span;

            if (bytes.IsEmpty)
                throw Error("An integer must contain at least one byte");

            return new BigInteger(bytes, isBigEndian: true);
        }

        public InvalidDataException Error(string message) =>
            new(message + " at offset " + Position + ".");
    }
}
