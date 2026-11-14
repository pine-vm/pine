using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.IO;

using Ast = Pine.Core.Elm.Syntax.Abstract.ElmSyntaxAbstractBinary;
using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;
using TreeReader = Pine.Core.Encodings.ValueEncodingBinaryDeterministic.TreeDecoder<object>;

namespace Pine.Core.Elm.Syntax.Interpreter;

public static partial class ElmSyntaxInterpreterPreparedBinary
{
    /// <summary>Reads mixed prepared/abstract roots in a shared scope over immutable input.</summary>
    public ref struct Decoder(ReadOnlyMemory<byte> source)
    {
        private TreeReader _reader = new(source);

        /// <summary>Number of bytes consumed by this session.</summary>
        public readonly int BytesRead => _reader.BytesRead;

        /// <summary>Reads a prepared app without running preparation.</summary>
        public Engine.Prepared ReadPrepared() => Read<Engine.Prepared>(ref _reader);

        /// <summary>Reads a prepared declaration.</summary>
        public Engine.PreparedDeclaration ReadDeclaration() => Read<Engine.PreparedDeclaration>(ref _reader);

        /// <summary>Reads a prepared expression.</summary>
        public Engine.PreparedExpression ReadExpression() => Read<Engine.PreparedExpression>(ref _reader);

        /// <summary>Reads both retained source syntax and its prepared declaration.</summary>
        public PreparedSourceDeclaration ReadSourceDeclaration() => Read<PreparedSourceDeclaration>(ref _reader);

        /// <summary>Reads abstract syntax in the shared reference scope.</summary>
        public Abstract.Declaration ReadSyntaxDeclaration() => Ast.Read<Abstract.Declaration>(ref _reader);

        /// <summary>Clears and releases temporary reference storage.</summary>
        public void Dispose() => _reader.Dispose();
    }

    private static T Read<T>(ref TreeReader reader) where T : class
    {
        try
        {
            return reader.Read<T, PreparedDecoding<T>>(default);
        }
        catch (ValueEncodingBinaryDeterministic.DecodeException error)
        {
            throw new InvalidDataException(error.Message, error);
        }
    }

    private static T[] Array<T>(ref TreeReader reader, int count) where T : class
    {
        if (count < 0 || count > reader.Remaining / 8)
            throw reader.Invalid("Invalid prepared collection length");

        if (count == 0)
            return [];

        var result = new T[count];

        for (var index = 0; index < result.Length; ++index)
            result[index] = Read<T>(ref reader);

        return result;
    }

    private readonly struct PreparedDecoding<T> : ValueEncodingBinaryDeterministic.IDecoding<T, object> where T : class
    {
        public T Blob(ReadOnlyMemory<byte> bytes) =>
            throw new InvalidDataException("Expected a prepared constructor, not a blob.");

        public T List(T[] items) =>
            throw new InvalidDataException("Expected a prepared constructor, not a list.");

        public T? ResolveExternal(ReadOnlyMemory<byte> reference) => null;

        public ValueResult<string, T> Evaluate(T expression, T environment) =>
            "Expected an external prepared constructor expression.";

        public ValueEncodingBinaryDeterministic.DecodeError Rejected(
            long offset, T expression, T environment, string reason) =>
            throw new InvalidDataException(reason);

        public bool HasExternalEvaluation => true;

        public int MaximumDepth => 512;

        public T EvaluateExternal(ref TreeReader reader, ReadOnlyMemory<byte> reference)
        {
            for (var index = 0; index < ConstructorHashBytes.Length / 32; ++index)
            {
                if (!ConstructorHash((Constructor)index).SequenceEqual(reference.Span))
                    continue;

                var result = Construct(ref reader, (Constructor)index, reader.ReadInlineListLength());
                return result is T typed ? typed : throw reader.Invalid("Wrong retained prepared constructor type");
            }

            throw reader.Invalid("Unknown prepared constructor expression hash");
        }
    }

    private static object Construct(ref TreeReader reader, Constructor constructor, int count)
    {
        switch (constructor)
        {
            case Constructor.App:
                {
                    if (count % 2 != 0 || count > reader.Remaining / 8)
                        throw reader.Invalid("Invalid prepared app entry count");

                    var declarations = new Dictionary<DeclQualifiedName, Engine.PreparedDeclaration>(count / 2);

                    for (var index = 0; index < count / 2; ++index)
                    {
                        var name = Ast.Read<DeclQualifiedName>(ref reader);
                        var declaration = Read<Engine.PreparedDeclaration>(ref reader);

                        if (!declarations.TryAdd(name, declaration))
                            throw reader.Invalid("Duplicate prepared declaration name");
                    }

                    return new Engine.Prepared(declarations);
                }

            case Constructor.SourceDeclaration:
                Ast.Arity(ref reader, count, 2);

                return
                    new PreparedSourceDeclaration(
                        Ast.Read<Abstract.Declaration>(ref reader),
                        Read<Engine.PreparedDeclaration>(ref reader));

            case Constructor.FunctionDeclaration:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedDeclaration.FunctionDeclaration(Read<Engine.PreparedFunctionStruct>(ref reader));

            case Constructor.ChoiceTypeDeclaration:
                Ast.Arity(ref reader, count, 1);

                return
                    new Engine.PreparedDeclaration.ChoiceTypeDeclaration(
                        Ast.Read<Abstract.ChoiceTypeStruct>(ref reader));

            case Constructor.AliasDeclaration:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedDeclaration.AliasDeclaration(Ast.Read<Abstract.TypeAlias>(ref reader));

            case Constructor.PortDeclaration:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedDeclaration.PortDeclaration(Ast.Read<Abstract.Signature>(ref reader));

            case Constructor.InfixDeclaration:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedDeclaration.InfixDeclaration(Ast.Read<Abstract.Infix>(ref reader));

            case Constructor.Function:
                {
                    Ast.Arity(ref reader, count, 2);
                    var signature = Ast.OptionalSignature(ref reader);

                    return
                        new Engine.PreparedFunctionStruct(
                            signature,
                            Read<Engine.PreparedFunctionImplementation>(ref reader));
                }

            case Constructor.Implementation:
                {
                    Ast.Arity(ref reader, count, 2, minimum: true);
                    var name = Ast.Read<string>(ref reader);
                    var body = Read<Engine.PreparedExpression>(ref reader);
                    return new Engine.PreparedFunctionImplementation(name, Patterns(ref reader, count - 2), body);
                }

            case Constructor.ValueLiteral:
                Ast.Arity(ref reader, count, 1);

                return
                    new Engine.PreparedExpression.ValueLiteral(
                        PineValueInProcess.CreateFullyRepresented(Ast.Value(ref reader)));

            case Constructor.Negation:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedExpression.Negation(Read<Engine.PreparedExpression>(ref reader));

            case Constructor.ListExpr:
                return new Engine.PreparedExpression.ListExpr(Array<Engine.PreparedExpression>(ref reader, count));

            case Constructor.Identifier:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedExpression.Identifier(Ast.Read<DeclQualifiedName>(ref reader));

            case Constructor.IfBlock:
                Ast.Arity(ref reader, count, 3);

                return
                    new Engine.PreparedExpression.IfBlock(
                        Read<Engine.PreparedExpression>(ref reader),
                        Read<Engine.PreparedExpression>(ref reader),
                        Read<Engine.PreparedExpression>(ref reader));

            case Constructor.PrefixOperator:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedExpression.PrefixOperator(Ast.Read<string>(ref reader));

            case Constructor.Application:
                {
                    Ast.Arity(ref reader, count, 1, minimum: true);
                    var function = Read<Engine.PreparedExpression>(ref reader);

                    return
                        new Engine.PreparedExpression.Application(
                            function,
                            Array<Engine.PreparedExpression>(ref reader, count - 1));
                }

            case Constructor.OperatorApplication:
                Ast.Arity(ref reader, count, 4);

                return
                    new Engine.PreparedExpression.OperatorApplication(
                        Ast.Read<string>(ref reader),
                        Ast.Direction(ref reader),
                        Read<Engine.PreparedExpression>(ref reader),
                        Read<Engine.PreparedExpression>(ref reader));

            case Constructor.TupledExpression:
                return new Engine.PreparedExpression.TupledExpression(Array<Engine.PreparedExpression>(ref reader, count));

            case Constructor.LambdaExpression:
                {
                    Ast.Arity(ref reader, count, 1, minimum: true);
                    var body = Read<Engine.PreparedExpression>(ref reader);
                    return new Engine.PreparedExpression.LambdaExpression(Patterns(ref reader, count - 1), body);
                }

            case Constructor.CaseExpression:
                Ast.Arity(ref reader, count, 2);

                return
                    new Engine.PreparedExpression.CaseExpression(
                        Read<Engine.PreparedExpression>(ref reader),
                        Read<Engine.PreparedCaseDispatch>(ref reader));

            case Constructor.LetExpression:
                {
                    Ast.Arity(ref reader, count, 1, minimum: true);
                    var body = Read<Engine.PreparedExpression>(ref reader);

                    return
                        new Engine.PreparedExpression.LetExpression(
                            Array<Engine.PreparedLetDeclaration>(ref reader, count - 1),
                            body);
                }

            case Constructor.RecordExpr:
                return new Engine.PreparedExpression.RecordExpr(Array<Engine.PreparedRecordSetter>(ref reader, count));

            case Constructor.RecordAccess:
                Ast.Arity(ref reader, count, 3);

                return
                    new Engine.PreparedExpression.RecordAccess(
                        Read<Engine.PreparedExpression>(ref reader),
                        Ast.Read<string>(ref reader),
                        Ast.Value(ref reader));

            case Constructor.RecordAccessFunction:
                Ast.Arity(ref reader, count, 2);

                return
                    new Engine.PreparedExpression.RecordAccessFunction(
                        Ast.Read<string>(ref reader),
                        Ast.Value(ref reader));

            case Constructor.RecordUpdateExpression:
                {
                    Ast.Arity(ref reader, count, 1, minimum: true);
                    var name = Ast.Read<string>(ref reader);

                    return
                        new Engine.PreparedExpression.RecordUpdateExpression(
                            name,
                            Array<Engine.PreparedRecordSetter>(ref reader, count - 1));
                }

            case Constructor.GLSLExpression:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedExpression.GLSLExpression(Ast.Read<string>(ref reader));

            case Constructor.LetFunction:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedLetDeclaration.LetFunction(Read<Engine.PreparedFunctionStruct>(ref reader));

            case Constructor.LetDestructuring:
                Ast.Arity(ref reader, count, 2);

                return
                    new Engine.PreparedLetDeclaration.LetDestructuring(
                        Ast.Read<Abstract.Pattern>(ref reader),
                        Read<Engine.PreparedExpression>(ref reader));

            case Constructor.RecordSetter:
                Ast.Arity(ref reader, count, 3);

                return
                    new Engine.PreparedRecordSetter(
                        Ast.Read<string>(ref reader),
                        Ast.Value(ref reader),
                        Read<Engine.PreparedExpression>(ref reader));

            case Constructor.CaseDispatch:
                return new Engine.PreparedCaseDispatch(Array<Engine.PreparedCaseDispatchSegment>(ref reader, count));

            case Constructor.ConstantValuePatterns:
                return
                    new Engine.PreparedCaseDispatchSegment.ConstantValuePatterns(
                        Array<Engine.PreparedConstantCase>(ref reader, count));

            case Constructor.PatternCase:
                Ast.Arity(ref reader, count, 2);

                return
                    new Engine.PreparedCaseDispatchSegment.PatternCase(
                        Ast.Read<Abstract.Pattern>(ref reader),
                        Read<Engine.PreparedExpression>(ref reader));

            case Constructor.DiscardCase:
                Ast.Arity(ref reader, count, 1);
                return new Engine.PreparedCaseDispatchSegment.DiscardCase(Read<Engine.PreparedExpression>(ref reader));

            case Constructor.ConstantCase:
                Ast.Arity(ref reader, count, 3);

                return
                    new Engine.PreparedConstantCase(
                        Ast.Read<Abstract.Pattern>(ref reader),
                        Ast.Value(ref reader),
                        Read<Engine.PreparedExpression>(ref reader));

            default:
                throw new NotImplementedException("Unknown prepared constructor: " + constructor);
        }
    }

    private static Abstract.Pattern[] Patterns(ref TreeReader reader, int count)
    {
        if (count < 0 || count > reader.Remaining / 8)
            throw reader.Invalid("Invalid prepared pattern count");

        if (count == 0)
            return [];

        var patterns = new Abstract.Pattern[count];

        for (var index = 0; index < patterns.Length; ++index)
            patterns[index] = Ast.Read<Abstract.Pattern>(ref reader);

        return patterns;
    }
}
