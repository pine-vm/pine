using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Buffers.Binary;
using System.IO;
using System.Numerics;
using System.Runtime.InteropServices;
using System.Text;

using InfixDirection = Pine.Core.Elm.Syntax.Concrete.InfixDirection;
using TreeReader = Pine.Core.Encodings.ValueEncodingBinaryDeterministic.TreeDecoder<object>;

namespace Pine.Core.Elm.Syntax.Abstract;

public static partial class ElmSyntaxAbstractBinary
{
    private static readonly object s_noSignature = new();

    /// <summary>Reads a syntax session, preserving references across roots. Dispose releases scratch storage.</summary>
    /// <remarks>Starts an independent reference scope over immutable input.</remarks>
    public ref struct Decoder(ReadOnlyMemory<byte> source)
    {
        private TreeReader _reader = new(source);

        /// <summary>Extent consumed by all roots read in this session.</summary>
        public readonly int BytesRead => _reader.BytesRead;

        /// <summary>Reads one expression in the current scope.</summary>
        public Expression ReadExpression() => Read<Expression>(ref _reader);

        /// <summary>Reads one declaration in the current scope.</summary>
        public Declaration ReadDeclaration() => Read<Declaration>(ref _reader);

        /// <summary>Reads one annotation in the current scope.</summary>
        public TypeAnnotation ReadTypeAnnotation() => Read<TypeAnnotation>(ref _reader);

        /// <summary>Reads a declaration list without resetting session references.</summary>
        public Declaration[] ReadDeclarations()
        {
            try
            {
                return Array<Declaration>(ref _reader, _reader.ReadInlineListLength());
            }
            catch (ValueEncodingBinaryDeterministic.DecodeException error)
            {
                throw new InvalidDataException(error.Message, error);
            }
        }

        /// <summary>Releases and clears temporary reference storage.</summary>
        public void Dispose() => _reader.Dispose();
    }

    private static T Read<T>(ref TreeReader reader) where T : class
    {
        try
        {
            return reader.Read<T, SyntaxDecoding<T>>(default);
        }
        catch (ValueEncodingBinaryDeterministic.DecodeException error)
        {
            throw new InvalidDataException(error.Message, error);
        }
    }

    private static T[] Array<T>(ref TreeReader reader, int count) where T : class
    {
        if (count < 0 || count > reader.Remaining / 8)
            throw reader.Invalid("Invalid retained collection length");

        if (count == 0)
            return [];

        var values = new T[count];

        for (var index = 0; index < count; ++index)
            values[index] = Read<T>(ref reader);

        return values;
    }

    private static PineValue Value(ref TreeReader reader) =>
        reader.Read<PineValue, ValueEncodingBinaryDeterministic.PineDecoding<object>>(default);

    private static BigInteger Integer(ref TreeReader reader)
    {
        var bytes = reader.ReadInlineBlob().Span;

        if (bytes.Length < 2 || bytes[0] is not (2 or 4) ||
            (bytes.Length > 2 && bytes[1] == 0) || (bytes[0] == 2 && bytes.Length == 2 && bytes[1] == 0))
        {
            throw reader.Invalid("Invalid canonical signed integer");
        }

        var magnitude = new BigInteger(bytes[1..], isUnsigned: true, isBigEndian: true);
        return bytes[0] == 2 ? -magnitude : magnitude;
    }

    private static int Int32(ref TreeReader reader)
    {
        var value = Integer(ref reader);

        if (value < int.MinValue || value > int.MaxValue)
            throw reader.Invalid("Integer is outside Int32 range");

        return (int)value;
    }

    private static int Count(ref TreeReader reader, int maximum)
    {
        var count = Int32(ref reader);

        if (count < 0 || count > maximum)
            throw reader.Invalid("Count exceeds constructor environment");

        return count;
    }

    private static InfixDirection Direction(ref TreeReader reader) =>
        Int32(ref reader) switch
        {
            0 => InfixDirection.Left,
            1 => InfixDirection.Right,
            2 => InfixDirection.Non,

            _ =>
            throw reader.Invalid("Invalid infix direction")
        };

    private static void Arity(ref TreeReader reader, int actual, int expected, bool minimum = false)
    {
        if (minimum ? actual < expected : actual != expected)
            throw reader.Invalid("Invalid constructor environment arity");
    }

    private readonly struct SyntaxDecoding<T> : ValueEncodingBinaryDeterministic.IDecoding<T, object> where T : class
    {
        public T Blob(ReadOnlyMemory<byte> bytes) =>
            throw new InvalidDataException("Expected a syntax constructor, not a blob.");

        public T List(T[] items) => throw new InvalidDataException("Expected a syntax constructor, not a list.");

        public T? ResolveExternal(ReadOnlyMemory<byte> reference) => null;

        public ValueResult<string, T> Evaluate(T expression, T environment) =>
            "Expected an external constructor expression.";

        public ValueEncodingBinaryDeterministic.DecodeError Rejected(
            long offset,
            T expression,
            T environment,
            string reason) =>
            throw new InvalidDataException(reason);

        public bool HasExternalEvaluation => true;

        public int MaximumDepth => 512;

        public T EvaluateExternal(ref TreeReader reader, ReadOnlyMemory<byte> reference)
        {
            var constructor = -1;

            for (var index = 0; index < ConstructorHashBytes.Length / 32; ++index)
            {
                if (ConstructorHash((Constructor)index).SequenceEqual(reference.Span))
                {
                    constructor = index;
                    break;
                }
            }

            if (constructor < 0)
                throw reader.Invalid("Unknown syntax constructor expression hash");

            var result = Construct(ref reader, (Constructor)constructor, reader.ReadInlineListLength());
            return result is T typed ? typed : throw reader.Invalid("Constructor result has the wrong retained type");
        }
    }

    private static object Construct(ref TreeReader reader, Constructor constructor, int count)
    {
        switch (constructor)
        {
            case Constructor.String:
                Arity(ref reader, count, 1);

                {
                    var bytes = reader.ReadInlineBlob();

                    if (bytes.Length % 4 != 0)
                        throw reader.Invalid("UTF-32 blob length is not a multiple of four");

                    var length = 0;

                    for (var index = 0; index < bytes.Length; index += 4)
                    {
                        var scalar = BinaryPrimitives.ReadInt32BigEndian(bytes.Span[index..]);

                        if (!Rune.IsValid(scalar))
                            throw reader.Invalid("Invalid UTF-32 Unicode scalar");

                        length += scalar > 0xffff ? 2 : 1;
                    }

                    return
                        string.Create(
                            length,
                            bytes,
                            static (destination, source) =>
                            {
                                var position = 0;

                                for (var index = 0; index < source.Length; index += 4)
                                {
                                    position +=
                                        new Rune(BinaryPrimitives.ReadInt32BigEndian(source.Span[index..]))
                                        .EncodeToUtf16(destination[position..]);
                                }
                            });
                }

            case Constructor.QualifiedDeclaration:
                {
                    Arity(ref reader, count, 1, minimum: true);
                    var name = Read<string>(ref reader);
                    return DeclQualifiedName.Create(Array<string>(ref reader, count - 1), name);
                }

            case Constructor.QualifiedPattern:
                {
                    Arity(ref reader, count, 1, minimum: true);
                    var name = Read<string>(ref reader);
                    return new QualifiedNameRef(Array<string>(ref reader, count - 1), name);
                }

            case Constructor.None:
                Arity(ref reader, count, 0);
                return s_noSignature;

            case Constructor.Some:
                Arity(ref reader, count, 1);
                return Read<Signature>(ref reader);

            case Constructor.Function:
                {
                    Arity(ref reader, count, 2);
                    var signature = Read<object>(ref reader);

                    if (signature is not Signature && !ReferenceEquals(signature, s_noSignature))
                        throw reader.Invalid("Invalid optional signature");

                    return new FunctionStruct(signature as Signature, Read<FunctionImplementation>(ref reader));
                }

            case Constructor.Implementation:
                {
                    Arity(ref reader, count, 2, minimum: true);
                    var name = Read<string>(ref reader);
                    var body = Read<Expression>(ref reader);
                    return new FunctionImplementation(name, Array<Pattern>(ref reader, count - 2), body);
                }

            case Constructor.Signature:
                Arity(ref reader, count, 2);
                return new Signature(Read<string>(ref reader), Read<TypeAnnotation>(ref reader));

            case Constructor.Choice:
                {
                    Arity(ref reader, count, 2, minimum: true);
                    var name = Read<string>(ref reader);
                    var genericsCount = Count(ref reader, count - 2);
                    var generics = Array<string>(ref reader, genericsCount);

                    return
                        new ChoiceTypeStruct(
                            name,
                            generics,
                            Array<ValueConstructor>(ref reader, count - 2 - genericsCount));
                }

            case Constructor.Alias:
                {
                    Arity(ref reader, count, 3, minimum: true);
                    var name = Read<string>(ref reader);
                    var genericsCount = Count(ref reader, count - 3);
                    Arity(ref reader, count, 3 + genericsCount);

                    return
                        new TypeAlias(
                            name,
                            Array<string>(ref reader, genericsCount),
                            Read<TypeAnnotation>(ref reader));
                }

            case Constructor.Infix:
                Arity(ref reader, count, 4);

                return
                    new Infix(
                        Direction(ref reader),
                        Int32(ref reader),
                        Read<string>(ref reader),
                        Read<string>(ref reader));

            case Constructor.ValueConstructor:
                Arity(ref reader, count, 2, minimum: true);

                return
                    new ValueConstructor(
                        Read<string>(ref reader),
                        Value(ref reader),
                        Array<TypeAnnotation>(ref reader, count - 2));

            case Constructor.RecordDefinition:
                return new RecordDefinition(Array<RecordField>(ref reader, count));

            case Constructor.RecordField:
                Arity(ref reader, count, 3);
                return new RecordField(Read<string>(ref reader), Value(ref reader), Read<TypeAnnotation>(ref reader));

            case Constructor.RecordSetter:
                Arity(ref reader, count, 3);
                return new RecordSetter(Read<string>(ref reader), Value(ref reader), Read<Expression>(ref reader));

            case Constructor.Case:
                Arity(ref reader, count, 2);
                return new Case(Read<Pattern>(ref reader), Read<Expression>(ref reader));

            case Constructor.LetFunction:
                Arity(ref reader, count, 1);
                return new LetDeclaration.LetFunction(Read<FunctionStruct>(ref reader));

            case Constructor.LetDestructuring:
                Arity(ref reader, count, 2);
                return new LetDeclaration.LetDestructuring(Read<Pattern>(ref reader), Read<Expression>(ref reader));

            case Constructor.FunctionDeclaration:
                Arity(ref reader, count, 1);
                return new Declaration.FunctionDeclaration(Read<FunctionStruct>(ref reader));

            case Constructor.ChoiceTypeDeclaration:
                Arity(ref reader, count, 1);
                return new Declaration.ChoiceTypeDeclaration(Read<ChoiceTypeStruct>(ref reader));

            case Constructor.AliasDeclaration:
                Arity(ref reader, count, 1);
                return new Declaration.AliasDeclaration(Read<TypeAlias>(ref reader));

            case Constructor.PortDeclaration:
                Arity(ref reader, count, 1);
                return new Declaration.PortDeclaration(Read<Signature>(ref reader));

            case Constructor.InfixDeclaration:
                Arity(ref reader, count, 1);
                return new Declaration.InfixDeclaration(Read<Infix>(ref reader));

            case Constructor.GenericType:
                Arity(ref reader, count, 1);
                return new TypeAnnotation.GenericType(Read<string>(ref reader));

            case Constructor.Typed:
                {
                    Arity(ref reader, count, 2, minimum: true);
                    var moduleCount = Count(ref reader, count - 2);
                    var modules = Array<string>(ref reader, moduleCount);
                    var name = Read<string>(ref reader);

                    return
                        new TypeAnnotation.Typed(
                            modules,
                            name,
                            Array<TypeAnnotation>(ref reader, count - 2 - moduleCount));
                }

            case Constructor.Unit:
                Arity(ref reader, count, 0);
                return new TypeAnnotation.Unit();

            case Constructor.Tupled:
                return new TypeAnnotation.Tupled(Array<TypeAnnotation>(ref reader, count));

            case Constructor.Record:
                Arity(ref reader, count, 1);
                return new TypeAnnotation.Record(Read<RecordDefinition>(ref reader));

            case Constructor.GenericRecord:
                Arity(ref reader, count, 2);
                return new TypeAnnotation.GenericRecord(Read<string>(ref reader), Read<RecordDefinition>(ref reader));

            case Constructor.FunctionTypeAnnotation:
                Arity(ref reader, count, 2);

                return
                    new TypeAnnotation.FunctionTypeAnnotation(
                        Read<TypeAnnotation>(ref reader),
                        Read<TypeAnnotation>(ref reader));

            case Constructor.AllPattern:
                Arity(ref reader, count, 0);
                return new Pattern.AllPattern();

            case Constructor.VarPattern:
                Arity(ref reader, count, 1);
                return new Pattern.VarPattern(Read<string>(ref reader));

            case Constructor.UnitPattern:
                Arity(ref reader, count, 0);
                return new Pattern.UnitPattern();

            case Constructor.CharPattern:
                Arity(ref reader, count, 2);
                return new Pattern.CharPattern(Int32(ref reader), Value(ref reader));

            case Constructor.StringPattern:
                Arity(ref reader, count, 2);
                return new Pattern.StringPattern(Read<string>(ref reader), Value(ref reader));

            case Constructor.IntPattern:
                Arity(ref reader, count, 2);
                return new Pattern.IntPattern(Integer(ref reader), Value(ref reader));

            case Constructor.FloatPattern:
                Arity(ref reader, count, 2);
                return new Pattern.FloatPattern(Integer(ref reader), Integer(ref reader));

            case Constructor.TuplePattern:
                return new Pattern.TuplePattern(Array<Pattern>(ref reader, count));

            case Constructor.RecordPattern:
                {
                    Arity(ref reader, count, 1, minimum: true);
                    var fieldCount = Count(ref reader, (count - 1) / 2);
                    Arity(ref reader, count, 1 + fieldCount * 2);
                    var fields = new (string FieldName, PineValue FieldNameValue)[fieldCount];

                    for (var index = 0; index < fields.Length; ++index)
                        fields[index].FieldName = Read<string>(ref reader);

                    for (var index = 0; index < fields.Length; ++index)
                        fields[index].FieldNameValue = Value(ref reader);

                    return new Pattern.RecordPattern(ImmutableCollectionsMarshal.AsImmutableArray(fields));
                }

            case Constructor.UnConsPattern:
                Arity(ref reader, count, 2);
                return new Pattern.UnConsPattern(Read<Pattern>(ref reader), Read<Pattern>(ref reader));

            case Constructor.ListPattern:
                return new Pattern.ListPattern(Array<Pattern>(ref reader, count));

            case Constructor.NamedPattern:
                {
                    Arity(ref reader, count, 2, minimum: true);
                    var name = Read<QualifiedNameRef>(ref reader);
                    var tag = Value(ref reader);
                    return new Pattern.NamedPattern(name, Array<Pattern>(ref reader, count - 2), tag);
                }

            case Constructor.AsPattern:
                Arity(ref reader, count, 2);
                return new Pattern.AsPattern(Read<Pattern>(ref reader), Read<string>(ref reader));

            case Constructor.UnitExpr:
                Arity(ref reader, count, 0);
                return Expression.UnitExpr.Instance;

            case Constructor.StringLiteral:
                Arity(ref reader, count, 2);
                return new Expression.StringLiteral(Read<string>(ref reader), Value(ref reader));

            case Constructor.CharLiteral:
                Arity(ref reader, count, 2);
                return new Expression.CharLiteral(Int32(ref reader), Value(ref reader));

            case Constructor.IntegerLiteral:
                Arity(ref reader, count, 2);
                return new Expression.IntegerLiteral(Integer(ref reader), Value(ref reader));

            case Constructor.FloatLiteral:
                Arity(ref reader, count, 2);
                return new Expression.FloatLiteral(Integer(ref reader), Integer(ref reader));

            case Constructor.Negation:
                Arity(ref reader, count, 1);
                return new Expression.Negation(Read<Expression>(ref reader));

            case Constructor.ListExpr:
                return new Expression.ListExpr(Array<Expression>(ref reader, count));

            case Constructor.Identifier:
                Arity(ref reader, count, 1);
                return new Expression.Identifier(Read<DeclQualifiedName>(ref reader));

            case Constructor.IfBlock:
                Arity(ref reader, count, 3);

                return
                    new Expression.IfBlock(
                        Read<Expression>(ref reader),
                        Read<Expression>(ref reader),
                        Read<Expression>(ref reader));

            case Constructor.PrefixOperator:
                Arity(ref reader, count, 1);
                return new Expression.PrefixOperator(Read<string>(ref reader));

            case Constructor.Application:
                Arity(ref reader, count, 1, minimum: true);
                return new Expression.Application(Read<Expression>(ref reader), Array<Expression>(ref reader, count - 1));

            case Constructor.OperatorApplication:
                Arity(ref reader, count, 4);

                return
                    new Expression.OperatorApplication(
                        Read<string>(ref reader),
                        Direction(ref reader),
                        Read<Expression>(ref reader),
                        Read<Expression>(ref reader));

            case Constructor.TupledExpression:
                return new Expression.TupledExpression(Array<Expression>(ref reader, count));

            case Constructor.LambdaExpression:
                {
                    Arity(ref reader, count, 1, minimum: true);
                    var body = Read<Expression>(ref reader);
                    return new Expression.LambdaExpression(Array<Pattern>(ref reader, count - 1), body);
                }

            case Constructor.CaseExpression:
                Arity(ref reader, count, 1, minimum: true);
                return new Expression.CaseExpression(Read<Expression>(ref reader), Array<Case>(ref reader, count - 1));

            case Constructor.LetExpression:
                {
                    Arity(ref reader, count, 1, minimum: true);
                    var body = Read<Expression>(ref reader);
                    return new Expression.LetExpression(Array<LetDeclaration>(ref reader, count - 1), body);
                }

            case Constructor.RecordExpr:
                return new Expression.RecordExpr(Array<RecordSetter>(ref reader, count));

            case Constructor.RecordAccess:
                Arity(ref reader, count, 3);

                return
                    new Expression.RecordAccess(
                        Read<Expression>(ref reader),
                        Read<string>(ref reader),
                        Value(ref reader));

            case Constructor.RecordAccessFunction:
                Arity(ref reader, count, 2);
                return new Expression.RecordAccessFunction(Read<string>(ref reader), Value(ref reader));

            case Constructor.RecordUpdateExpression:
                Arity(ref reader, count, 1, minimum: true);

                return
                    new Expression.RecordUpdateExpression(
                        Read<string>(ref reader),
                        Array<RecordSetter>(ref reader, count - 1));

            case Constructor.GLSLExpression:
                Arity(ref reader, count, 1);
                return new Expression.GLSLExpression(Read<string>(ref reader));

            default:
                throw new NotImplementedException("Unknown syntax constructor: " + constructor);
        }
    }
}
