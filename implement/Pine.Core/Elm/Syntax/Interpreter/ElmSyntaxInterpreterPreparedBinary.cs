using Pine.Core.Caching;
using Pine.Core.Encodings;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Cryptography;

using Ast = Pine.Core.Elm.Syntax.Abstract.ElmSyntaxAbstractBinary;
using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Elm.Syntax.Interpreter;

/// <summary>
/// Deterministic binary storage of prepared syntax, without repeating interpretation preparation.
/// Sessions share AST leaves and prepared expressions in the deterministic Pine tree format.
/// </summary>
public static partial class ElmSyntaxInterpreterPreparedBinary
{
    /// <summary>Writes an app with declarations sorted by ordinal qualified name.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, Engine.Prepared prepared)
    {
        using var encoder = new Encoder(write);
        encoder.WritePrepared(prepared);
    }

    /// <summary>Writes one prepared declaration in an independent reference scope.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, Engine.PreparedDeclaration declaration)
    {
        using var encoder = new Encoder(write);
        encoder.WriteDeclaration(declaration);
    }

    /// <summary>Writes one prepared expression in an independent reference scope.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, Engine.PreparedExpression expression)
    {
        using var encoder = new Encoder(write);
        encoder.WriteExpression(expression);
    }

    /// <summary>Writes both source syntax and its retained prepared declaration.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, PreparedSourceDeclaration declaration)
    {
        using var encoder = new Encoder(write);
        encoder.WriteSourceDeclaration(declaration);
    }

    /// <summary>Writes an app without closing the stream.</summary>
    public static void Encode(Stream stream, Engine.Prepared prepared) => Encode(stream.Write, prepared);

    /// <summary>Writes a prepared declaration without closing the stream.</summary>
    public static void Encode(Stream stream, Engine.PreparedDeclaration declaration) =>
        Encode(stream.Write, declaration);

    /// <summary>Writes a prepared expression without closing the stream.</summary>
    public static void Encode(Stream stream, Engine.PreparedExpression expression) => Encode(stream.Write, expression);

    /// <summary>Writes a source/prepared pair without closing the stream.</summary>
    public static void Encode(Stream stream, PreparedSourceDeclaration declaration) =>
        Encode(stream.Write, declaration);

    /// <summary>Reads an app and reports its consumed extent for embedding.</summary>
    public static Engine.Prepared DecodePrepared(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        using var decoder = new Decoder(source);
        var result = decoder.ReadPrepared();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>Reads a prepared declaration and reports its consumed extent.</summary>
    public static Engine.PreparedDeclaration DecodeDeclaration(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        using var decoder = new Decoder(source);
        var result = decoder.ReadDeclaration();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>Reads a prepared expression and reports its consumed extent.</summary>
    public static Engine.PreparedExpression DecodeExpression(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        using var decoder = new Decoder(source);
        var result = decoder.ReadExpression();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>Reads a source/prepared pair and reports its consumed extent.</summary>
    public static PreparedSourceDeclaration DecodeSourceDeclaration(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        using var decoder = new Decoder(source);
        var result = decoder.ReadSourceDeclaration();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>Streams the standalone declaration encoding into SHA-256.</summary>
    public static ReadOnlyMemory<byte> ComputeHash(Engine.PreparedDeclaration declaration)
    {
        using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);
        Encode(hash.AppendData, declaration);
        return hash.GetHashAndReset();
    }

    /// <summary>
    /// Complete-entry binary cache codec shared with <see cref="PreparedSourceDeclaration.Codec"/>.
    /// </summary>
    public static ICacheCodec<PreparedSourceDeclaration> SourceDeclarationCodec { get; } = new SourceCodec();

    private sealed class SourceCodec : ICacheCodec<PreparedSourceDeclaration>
    {
        public ReadOnlyMemory<byte> Encode(PreparedSourceDeclaration value)
        {
            using var buffer = new BinaryEncoding.PooledBuffer();
            ElmSyntaxInterpreterPreparedBinary.Encode(buffer.Write, value);
            return buffer.WrittenSpan.ToArray();
        }

        public PreparedSourceDeclaration Decode(ReadOnlyMemory<byte> bytes)
        {
            var result = DecodeSourceDeclaration(bytes, out var consumed);

            if (consumed != bytes.Length)
                throw new InvalidDataException("Trailing bytes after prepared source declaration.");

            return result;
        }
    }

    private enum Constructor
    {
        App, SourceDeclaration,
        FunctionDeclaration, ChoiceTypeDeclaration, AliasDeclaration, PortDeclaration, InfixDeclaration,
        Function, Implementation, ValueLiteral, Negation, ListExpr, Identifier, IfBlock, PrefixOperator,
        Application, OperatorApplication, TupledExpression, LambdaExpression, CaseExpression, LetExpression,
        RecordExpr, RecordAccess, RecordAccessFunction, RecordUpdateExpression, GLSLExpression,
        LetFunction, LetDestructuring, RecordSetter, CaseDispatch,
        ConstantValuePatterns, PatternCase, DiscardCase, ConstantCase
    }

    /// <summary>Streaming prepared/abstract syntax session. Disposal releases reference dictionaries.</summary>
    public sealed class Encoder(Action<ReadOnlySpan<byte>> write) : IDisposable
    {
        private readonly Ast.Encoder _syntax = new(write, PreparedExpressionComparer.Instance);

        private void Start(Constructor constructor, int count) =>
            _syntax.Writer.StartExternalEval(ConstructorHash(constructor), count);

        /// <summary>All writes are synchronous; flush does not reset references.</summary>
        public void Flush() => _syntax.Flush();

        /// <inheritdoc/>
        public void Dispose() => _syntax.Dispose();

        /// <summary>Writes an app; dictionary enumeration order never affects its bytes.</summary>
        public void WritePrepared(Engine.Prepared prepared)
        {
            Start(Constructor.App, checked(prepared.Declarations.Count * 2));

            foreach (var (name, declaration) in prepared.Declarations.OrderBy(entry => entry.Key))
            {
                _syntax.QualifiedName(name);
                WriteDeclaration(declaration);
            }
        }

        /// <summary>Writes both the original syntax and the already prepared declaration.</summary>
        public void WriteSourceDeclaration(PreparedSourceDeclaration declaration)
        {
            Start(Constructor.SourceDeclaration, 2);
            _syntax.WriteDeclaration(declaration.Syntax);
            WriteDeclaration(declaration.Prepared);
        }

        /// <summary>Writes abstract syntax in the same reference scope.</summary>
        public void WriteSyntaxDeclaration(Abstract.Declaration declaration) => _syntax.WriteDeclaration(declaration);

        /// <summary>Writes a prepared declaration without rebuilding its source representation.</summary>
        public void WriteDeclaration(Engine.PreparedDeclaration declaration)
        {
            switch (declaration)
            {
                case Engine.PreparedDeclaration.FunctionDeclaration function:
                    Start(Constructor.FunctionDeclaration, 1);
                    Function(function.Function);
                    break;

                case Engine.PreparedDeclaration.ChoiceTypeDeclaration choice:
                    Start(Constructor.ChoiceTypeDeclaration, 1);
                    _syntax.Choice(choice.TypeDeclaration);
                    break;

                case Engine.PreparedDeclaration.AliasDeclaration alias:
                    Start(Constructor.AliasDeclaration, 1);
                    _syntax.Alias(alias.TypeAlias);
                    break;

                case Engine.PreparedDeclaration.PortDeclaration port:
                    Start(Constructor.PortDeclaration, 1);
                    _syntax.Signature(port.Signature, optional: false);
                    break;

                case Engine.PreparedDeclaration.InfixDeclaration infix:
                    Start(Constructor.InfixDeclaration, 1);
                    _syntax.Infix(infix.Infix);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteDeclaration) + " does not handle declaration variant: " +
                        declaration.GetType().Name);
            }
        }

        private void Function(Engine.PreparedFunctionStruct function)
        {
            Start(Constructor.Function, 2);
            _syntax.OptionalSignature(function.Signature);
            Start(Constructor.Implementation, 2 + function.Declaration.Arguments.Count);
            _syntax.String(function.Declaration.Name);
            WriteExpression(function.Declaration.Expression);
            _syntax.Patterns(function.Declaration.Arguments);
        }

        private void Expressions(IReadOnlyList<Engine.PreparedExpression> expressions)
        {
            for (var index = 0; index < expressions.Count; ++index)
                WriteExpression(expressions[index]);
        }

        private void Setters(IReadOnlyList<Engine.PreparedRecordSetter> setters)
        {
            for (var index = 0; index < setters.Count; ++index)
            {
                var setter = setters[index];
                Start(Constructor.RecordSetter, 3);
                _syntax.String(setter.FieldName);
                _syntax.Value(setter.FieldNameValue);
                WriteExpression(setter.Value);
            }
        }

        /// <summary>Writes an expression with content-based reuse, independent of list/heap sharing.</summary>
        public void WriteExpression(Engine.PreparedExpression expression)
        {
            if (_syntax.Writer.TryReference(expression))
                return;

            switch (expression)
            {
                case Engine.PreparedExpression.ValueLiteral literal:
                    Start(Constructor.ValueLiteral, 1);
                    _syntax.Value(literal.Value.Evaluate());
                    break;

                case Engine.PreparedExpression.Negation negation:
                    Start(Constructor.Negation, 1);
                    WriteExpression(negation.Expression);
                    break;

                case Engine.PreparedExpression.ListExpr list:
                    Start(Constructor.ListExpr, list.Elements.Count);
                    Expressions(list.Elements);
                    break;

                case Engine.PreparedExpression.Identifier identifier:
                    Start(Constructor.Identifier, 1);
                    _syntax.QualifiedName(identifier.QualifiedName);
                    break;

                case Engine.PreparedExpression.IfBlock conditional:
                    Start(Constructor.IfBlock, 3);
                    WriteExpression(conditional.Condition);
                    WriteExpression(conditional.ThenBlock);
                    WriteExpression(conditional.ElseBlock);
                    break;

                case Engine.PreparedExpression.PrefixOperator prefix:
                    Start(Constructor.PrefixOperator, 1);
                    _syntax.String(prefix.Operator);
                    break;

                case Engine.PreparedExpression.Application application:
                    Start(Constructor.Application, 1 + application.Arguments.Count);
                    WriteExpression(application.Function);
                    Expressions(application.Arguments);
                    break;

                case Engine.PreparedExpression.OperatorApplication operation:
                    Start(Constructor.OperatorApplication, 4);
                    _syntax.String(operation.Operator);
                    _syntax.Direction(operation.Direction);
                    WriteExpression(operation.Left);
                    WriteExpression(operation.Right);
                    break;

                case Engine.PreparedExpression.TupledExpression tuple:
                    Start(Constructor.TupledExpression, tuple.Elements.Count);
                    Expressions(tuple.Elements);
                    break;

                case Engine.PreparedExpression.LambdaExpression lambda:
                    Start(Constructor.LambdaExpression, 1 + lambda.Arguments.Count);
                    WriteExpression(lambda.Expression);
                    _syntax.Patterns(lambda.Arguments);
                    break;

                case Engine.PreparedExpression.CaseExpression cases:
                    Start(Constructor.CaseExpression, 2);
                    WriteExpression(cases.Expression);
                    Dispatch(cases.Dispatch);
                    break;

                case Engine.PreparedExpression.LetExpression let:
                    Start(Constructor.LetExpression, 1 + let.Declarations.Count);
                    WriteExpression(let.Expression);

                    for (var index = 0; index < let.Declarations.Count; ++index)
                    {
                        var declaration = let.Declarations[index];

                        switch (declaration)
                        {
                            case Engine.PreparedLetDeclaration.LetFunction function:
                                Start(Constructor.LetFunction, 1);
                                Function(function.Function);
                                break;

                            case Engine.PreparedLetDeclaration.LetDestructuring destructuring:
                                Start(Constructor.LetDestructuring, 2);
                                _syntax.WritePattern(destructuring.Pattern);
                                WriteExpression(destructuring.Expression);
                                break;

                            default:
                                throw new NotImplementedException(
                                    nameof(WriteExpression) + " does not handle let variant: " +
                                    declaration.GetType().Name);
                        }
                    }

                    break;

                case Engine.PreparedExpression.RecordExpr record:
                    Start(Constructor.RecordExpr, record.Fields.Count);
                    Setters(record.Fields);
                    break;

                case Engine.PreparedExpression.RecordAccess access:
                    Start(Constructor.RecordAccess, 3);
                    WriteExpression(access.Record);
                    _syntax.String(access.FieldName);
                    _syntax.Value(access.FieldNameValue);
                    break;

                case Engine.PreparedExpression.RecordAccessFunction access:
                    Start(Constructor.RecordAccessFunction, 2);
                    _syntax.String(access.FieldName);
                    _syntax.Value(access.FieldNameValue);
                    break;

                case Engine.PreparedExpression.RecordUpdateExpression update:
                    Start(Constructor.RecordUpdateExpression, 1 + update.Fields.Count);
                    _syntax.String(update.RecordName);
                    Setters(update.Fields);
                    break;

                case Engine.PreparedExpression.GLSLExpression glsl:
                    Start(Constructor.GLSLExpression, 1);
                    _syntax.String(glsl.Code);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteExpression) + " does not handle expression variant: " + expression.GetType().Name);
            }
        }

        private void Dispatch(Engine.PreparedCaseDispatch dispatch)
        {
            Start(Constructor.CaseDispatch, dispatch.Steps.Count);

            for (var index = 0; index < dispatch.Steps.Count; ++index)
            {
                var step = dispatch.Steps[index];

                switch (step)
                {
                    case Engine.PreparedCaseDispatchSegment.ConstantValuePatterns constants:
                        Start(Constructor.ConstantValuePatterns, constants.Cases.Count);

                        for (var caseIndex = 0; caseIndex < constants.Cases.Count; ++caseIndex)
                        {
                            var branch = constants.Cases[caseIndex];
                            Start(Constructor.ConstantCase, 3);
                            _syntax.WritePattern(branch.Pattern);
                            _syntax.Value(branch.Value);
                            WriteExpression(branch.Expression);
                        }

                        break;

                    case Engine.PreparedCaseDispatchSegment.PatternCase branch:
                        Start(Constructor.PatternCase, 2);
                        _syntax.WritePattern(branch.Pattern);
                        WriteExpression(branch.Expression);
                        break;

                    case Engine.PreparedCaseDispatchSegment.DiscardCase discard:
                        Start(Constructor.DiscardCase, 1);
                        WriteExpression(discard.Expression);
                        break;

                    default:
                        throw new NotImplementedException(
                            nameof(Dispatch) + " does not handle dispatch variant: " + step.GetType().Name);
                }
            }
        }
    }
}
