using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Buffers;
using System.IO;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;

namespace Pine.Core.Runtime.InternalExpression;

/// <summary>
/// Release-private retained-expression encoding on the Pine deterministic binary format.
/// Hash-addressed constructor Evals define its schema; there is no version header.
/// </summary>
public static partial class ExpressionBinary
{
    private sealed record Digest(ReadOnlyMemory<byte> Bytes);

    private static readonly ConditionalWeakTable<Expression, Digest> s_hashes = [];

    /// <summary>Streams standalone bytes into SHA-256; not a canonical Pine expression hash.</summary>
    public static ReadOnlyMemory<byte> ComputeHash(Expression expression) =>
        s_hashes.GetValue(expression, static node => new(ComputeHashUncached(node))).Bytes;

    /// <summary>Hashes standalone bytes without memoization, buffering small encoder writes.</summary>
    public static ReadOnlyMemory<byte> ComputeHashUncached(Expression expression)
    {
        using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);
        var buffer = ArrayPool<byte>.Shared.Rent(4096);
        var count = 0;

        try
        {
            Encode(
                bytes =>
                {
                    while (!bytes.IsEmpty)
                    {
                        if (count == 0 && bytes.Length >= buffer.Length)
                        {
                            hash.AppendData(bytes);
                            return;
                        }

                        var length = Math.Min(bytes.Length, buffer.Length - count);
                        bytes[..length].CopyTo(buffer.AsSpan(count));
                        count += length;
                        bytes = bytes[length..];

                        if (count == buffer.Length)
                        {
                            hash.AppendData(buffer);
                            count = 0;
                        }
                    }
                },
                expression);

            if (count != 0)
                hash.AppendData(buffer.AsSpan(0, count));

            return hash.GetHashAndReset();
        }
        finally
        {
            ArrayPool<byte>.Shared.Return(buffer, clearArray: true);
        }
    }

    /// <summary>Encodes one root in an independent reference scope, without canonical model conversion.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, Expression expression)
    {
        using var encoder = new Encoder(write);
        encoder.WriteExpression(expression);
    }

    /// <summary>Leaves the caller's stream open.</summary>
    public static void Encode(Stream stream, Expression expression) => Encode(stream.Write, expression);

    /// <summary>A scoped writer sharing structurally equal retained nodes across roots.</summary>
    public struct Encoder(Action<ReadOnlySpan<byte>> write) : IDisposable
    {
        private PineTreeBinaryEncoding.TreeEncoder<object> _writer = new(write, reuseReferences: true);

        /// <summary>Writes a root without resetting session references.</summary>
        public void WriteExpression(Expression expression) => WriteExpression(expression, 0);

        private static void CheckDepth(int depth)
        {
            if (depth > 512 || !RuntimeHelpers.TryEnsureSufficientExecutionStack())
                throw new ArgumentException("Internal expression encoding exceeds the supported nesting depth.");
        }

        private void Start(Constructor kind, int count) => _writer.StartExternalEval(ConstructorHash(kind), count);

        private void WriteExpression(Expression expression, int depth)
        {
            ArgumentNullException.ThrowIfNull(expression);
            CheckDepth(depth);

            if (_writer.TryReference(expression))
                return;

            switch (expression)
            {
                case Expression.Literal literal:
                    Start(Constructor.Literal, 1);
                    Value(literal.Value, depth + 1);
                    break;

                case Expression.List list:
                    Start(Constructor.List, list.Items.Length);

                    foreach (var item in list.Items)
                        WriteExpression(item, depth + 1);

                    break;

                case Expression.Eval eval:
                    Start(Constructor.Eval, 2);
                    WriteExpression(eval.Encoded, depth + 1);
                    WriteExpression(eval.Environment, depth + 1);
                    break;

                case Expression.Builtin builtin:
                    Start(Constructor.Builtin, 2);
                    _writer.Integer((int)builtin.Function);
                    WriteExpression(builtin.Input, depth + 1);
                    break;

                case Expression.Conditional conditional:
                    Start(Constructor.Conditional, 3);
                    WriteExpression(conditional.Condition, depth + 1);
                    WriteExpression(conditional.FalseBranch, depth + 1);
                    WriteExpression(conditional.TrueBranch, depth + 1);
                    break;

                case Expression.Environment:
                    Start(Constructor.Environment, 0);
                    break;

                case Expression.Label label:
                    Start(Constructor.Label, 2);
                    Value(label.Value, depth + 1);
                    WriteExpression(label.Tagged, depth + 1);
                    break;

                case Expression.BuiltinBinary equal:
                    Start(Constructor.BuiltinBinary, 3);
                    _writer.Integer((int)equal.Function);
                    WriteExpression(equal.Left, depth + 1);
                    WriteExpression(equal.Right, depth + 1);
                    break;

                case Expression.Length_Equal_Binary length:
                    Start(Constructor.Length_Equal_Binary, 3);
                    WriteExpression(length.Sequence, depth + 1);
                    WriteExpression(length.Other, depth + 1);
                    _writer.Integer(length.LengthOnLeft ? 1 : 0);
                    break;

                case Expression.HeadSkip head:
                    Start(Constructor.HeadSkip, 2);
                    _writer.Integer(head.Count);
                    WriteExpression(head.Input, depth + 1);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteExpression) + " does not handle expression variant: " + expression.GetType().Name);
            }
        }

        private void Value(PineValue value, int depth)
        {
            CheckDepth(depth);

            switch (value)
            {
                case PineValue.BlobValue blob:
                    if (!_writer.TryBlobReference(blob))
                        _writer.Blob(blob.Bytes.Span);

                    break;

                case PineValue.ListValue list:
                    if (_writer.TryReference(list))
                        break;

                    _writer.StartList(list.Items.Length);

                    foreach (var item in list.Items.Span)
                        Value(item, depth + 1);

                    break;

                default:
                    throw new NotImplementedException(
                        nameof(Value) + " does not handle value variant: " + value.GetType().Name);
            }
        }

        /// <summary>Clears/releases bounded reference scratch, never retained model storage.</summary>
        public void Dispose() => _writer.Dispose();
    }
}
