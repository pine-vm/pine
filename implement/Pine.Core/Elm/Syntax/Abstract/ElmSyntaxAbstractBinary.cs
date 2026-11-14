using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;

using InfixDirection = Pine.Core.Elm.Syntax.Concrete.InfixDirection;

namespace Pine.Core.Elm.Syntax.Abstract;

/// <summary>
/// Versioned, self-delimiting binary syntax with structural back-references.
/// A session can contain several roots, sharing expression, annotation and Pine-value instances.
/// This format contains no source-resolution or compiler identities.
/// </summary>
public static partial class ElmSyntaxAbstractBinary
{
    private const int Header = 0x45415302;

    private sealed record Digest(ReadOnlyMemory<byte> Bytes);

    private static readonly ConditionalWeakTable<Declaration, Digest> s_hashes = [];

    /// <summary>Hashes the binary declaration directly, without materializing its encoded bytes.</summary>
    public static ReadOnlyMemory<byte> ComputeHash(Declaration declaration) =>
        s_hashes.GetValue(
            declaration,
            static syntax =>
            {
                using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);
                Encode(hash.AppendData, syntax);
                return new(hash.GetHashAndReset());
            }).Bytes;

    /// <summary>Writes one expression, starting a fresh reference scope.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, Expression expression)
    {
        using var encoder = new Encoder(write);
        encoder.WriteExpression(expression);
    }

    /// <summary>Writes one declaration, starting a fresh reference scope.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, Declaration declaration)
    {
        using var encoder = new Encoder(write);
        encoder.WriteDeclaration(declaration);
    }

    /// <summary>Writes a counted batch whose declarations share internal references.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, IReadOnlyList<Declaration> declarations)
    {
        using var encoder = new Encoder(write);
        encoder.WriteDeclarations(declarations);
    }

    /// <summary>Writes one expression to a stream without closing it.</summary>
    public static void Encode(Stream stream, Expression expression) => Encode(stream.Write, expression);

    /// <summary>Writes one declaration to a stream without closing it.</summary>
    public static void Encode(Stream stream, Declaration declaration) => Encode(stream.Write, declaration);

    /// <summary>Writes a declaration batch to a stream without closing it.</summary>
    public static void Encode(Stream stream, IReadOnlyList<Declaration> declarations) =>
        Encode(stream.Write, declarations);

    /// <summary>Reads one expression and reports its extent; trailing bytes belong to the caller.</summary>
    public static Expression DecodeExpression(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        var decoder = new Decoder(source);
        var result = decoder.ReadExpression();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>Reads one declaration and reports its extent; trailing bytes belong to the caller.</summary>
    public static Declaration DecodeDeclaration(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        var decoder = new Decoder(source);
        var result = decoder.ReadDeclaration();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>Reads a counted batch, preserving shared instances across declarations.</summary>
    public static IReadOnlyList<Declaration> DecodeDeclarations(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        var decoder = new Decoder(source);
        var result = decoder.ReadDeclarations();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>
    /// Buffered syntax writer. Dispose or Flush before consuming the destination.
    /// Internal-reference IDs are completion-order indices, independent of object identity.
    /// </summary>
    public sealed class Encoder(Action<ReadOnlySpan<byte>> write) : IDisposable
    {
        private readonly BinaryEncoding.Writer _writer = CreateWriter(write);

        private Dictionary<Expression, int>? _expressions;

        private Dictionary<TypeAnnotation, int>? _annotations;

        private Dictionary<PineValue, int>? _values;

        private static BinaryEncoding.Writer CreateWriter(Action<ReadOnlySpan<byte>> write)
        {
            var writer = new BinaryEncoding.Writer(write);
            writer.Int32(Header);
            return writer;
        }

        /// <summary>Flushes buffered bytes without resetting reference tables.</summary>
        public void Flush() => _writer.Flush();

        /// <inheritdoc/>
        public void Dispose() => _writer.Dispose();

        /// <summary>Writes a counted batch without resetting the session's references.</summary>
        public void WriteDeclarations(IReadOnlyList<Declaration> declarations)
        {
            _writer.Int32(declarations.Count);

            for (var index = 0; index < declarations.Count; index++)
                WriteDeclaration(declarations[index]);
        }

        /// <summary>Writes a declaration in this reference scope.</summary>
        public void WriteDeclaration(Declaration declaration)
        {
            switch (declaration)
            {
                case Declaration.FunctionDeclaration function:
                    _writer.Byte(1);
                    WriteFunction(function.Function);
                    break;

                case Declaration.ChoiceTypeDeclaration choice:
                    _writer.Byte(2);
                    _writer.String(choice.TypeDeclaration.Name);
                    WriteStrings(choice.TypeDeclaration.Generics);
                    _writer.Int32(choice.TypeDeclaration.Constructors.Count);

                    for (var index = 0; index < choice.TypeDeclaration.Constructors.Count; index++)
                    {
                        var constructor = choice.TypeDeclaration.Constructors[index];
                        _writer.String(constructor.Name);
                        WriteValue(constructor.NameValue);
                        WriteAnnotations(constructor.Arguments);
                    }

                    break;

                case Declaration.AliasDeclaration alias:
                    _writer.Byte(3);
                    _writer.String(alias.TypeAlias.Name);
                    WriteStrings(alias.TypeAlias.Generics);
                    WriteTypeAnnotation(alias.TypeAlias.TypeAnnotation);
                    break;

                case Declaration.PortDeclaration port:
                    _writer.Byte(4);
                    WriteSignature(port.Signature);
                    break;

                case Declaration.InfixDeclaration infix:
                    _writer.Byte(5);
                    WriteDirection(infix.Infix.Direction);
                    _writer.Int32(infix.Infix.Precedence);
                    _writer.String(infix.Infix.Operator);
                    _writer.String(infix.Infix.FunctionName);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteDeclaration) + " does not handle declaration variant: " +
                        declaration.GetType().Name);
            }
        }

        private void WriteFunction(FunctionStruct function)
        {
            _writer.Byte(function.Signature is null ? (byte)0 : (byte)1);

            if (function.Signature is { } signature)
                WriteSignature(signature);

            _writer.String(function.Declaration.Name);
            WritePatterns(function.Declaration.Arguments);
            WriteExpression(function.Declaration.Expression);
        }

        private void WriteSignature(Signature signature)
        {
            _writer.String(signature.Name);
            WriteTypeAnnotation(signature.TypeAnnotation);
        }

        private void WriteDirection(InfixDirection direction) =>
            _writer.Byte(
                direction switch
                {
                    InfixDirection.Left => 0,
                    InfixDirection.Right => 1,
                    InfixDirection.Non => 2,

                    _ =>
                    throw new ArgumentOutOfRangeException(nameof(direction), direction, "Unknown infix direction.")
                });

        private void WriteStrings(IReadOnlyList<string> strings)
        {
            _writer.Int32(strings.Count);

            for (var index = 0; index < strings.Count; index++)
                _writer.String(strings[index]);
        }

        private void WriteAnnotations(IReadOnlyList<TypeAnnotation> annotations)
        {
            _writer.Int32(annotations.Count);

            for (var index = 0; index < annotations.Count; index++)
                WriteTypeAnnotation(annotations[index]);
        }

        /// <summary>Writes an annotation, or an internal reference to its earlier structural equal.</summary>
        public void WriteTypeAnnotation(TypeAnnotation annotation)
        {
            _annotations ??= [];

            if (_annotations.TryGetValue(annotation, out var reference))
            {
                _writer.Byte(0);
                _writer.Int32(reference);
                return;
            }

            switch (annotation)
            {
                case TypeAnnotation.GenericType generic:
                    _writer.Byte(1);
                    _writer.String(generic.Name);
                    break;

                case TypeAnnotation.Typed typed:
                    _writer.Byte(2);
                    WriteStrings(typed.ModuleName);
                    _writer.String(typed.Name);
                    WriteAnnotations(typed.TypeArguments);
                    break;

                case TypeAnnotation.Unit:
                    _writer.Byte(3);
                    break;

                case TypeAnnotation.Tupled tuple:
                    _writer.Byte(4);
                    WriteAnnotations(tuple.TypeAnnotations);
                    break;

                case TypeAnnotation.Record record:
                    _writer.Byte(5);
                    WriteRecordDefinition(record.RecordDefinition);
                    break;

                case TypeAnnotation.GenericRecord record:
                    _writer.Byte(6);
                    _writer.String(record.GenericName);
                    WriteRecordDefinition(record.RecordDefinition);
                    break;

                case TypeAnnotation.FunctionTypeAnnotation function:
                    _writer.Byte(7);
                    WriteTypeAnnotation(function.ArgumentType);
                    WriteTypeAnnotation(function.ReturnType);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteTypeAnnotation) + " does not handle annotation variant: " +
                        annotation.GetType().Name);
            }

            _annotations.Add(annotation, _annotations.Count);
        }

        private void WriteRecordDefinition(RecordDefinition record)
        {
            _writer.Int32(record.Fields.Count);

            for (var index = 0; index < record.Fields.Count; index++)
            {
                var field = record.Fields[index];
                _writer.String(field.FieldName);
                WriteValue(field.FieldNameValue);
                WriteTypeAnnotation(field.FieldType);
            }
        }

        private void WriteExpressions(IReadOnlyList<Expression> expressions)
        {
            _writer.Int32(expressions.Count);

            for (var index = 0; index < expressions.Count; index++)
                WriteExpression(expressions[index]);
        }

        /// <summary>Writes an expression, or an internal reference to its earlier structural equal.</summary>
        public void WriteExpression(Expression expression)
        {
            _expressions ??= [];

            if (_expressions.TryGetValue(expression, out var reference))
            {
                _writer.Byte(0);
                _writer.Int32(reference);
                return;
            }

            switch (expression)
            {
                case Expression.UnitExpr:
                    _writer.Byte(1);
                    break;

                case Expression.StringLiteral literal:
                    _writer.Byte(2);
                    _writer.String(literal.Value);
                    WriteValue(literal.ValueAsPineValue);
                    break;

                case Expression.CharLiteral literal:
                    _writer.Byte(3);
                    _writer.Int32(literal.Value);
                    WriteValue(literal.ValueAsPineValue);
                    break;

                case Expression.IntegerLiteral literal:
                    _writer.Byte(4);
                    _writer.Integer(literal.Value);
                    WriteValue(literal.ValueAsPineValue);
                    break;

                case Expression.FloatLiteral literal:
                    _writer.Byte(5);
                    _writer.Integer(literal.Numerator);
                    _writer.Integer(literal.Denominator);
                    break;

                case Expression.Negation negation:
                    _writer.Byte(6);
                    WriteExpression(negation.Expression);
                    break;

                case Expression.ListExpr list:
                    _writer.Byte(7);
                    WriteExpressions(list.Elements);
                    break;

                case Expression.Identifier identifier:
                    _writer.Byte(8);
                    WriteStrings(identifier.QualifiedName.Namespaces);
                    _writer.String(identifier.QualifiedName.DeclName);
                    break;

                case Expression.IfBlock conditional:
                    _writer.Byte(9);
                    WriteExpression(conditional.Condition);
                    WriteExpression(conditional.ThenBlock);
                    WriteExpression(conditional.ElseBlock);
                    break;

                case Expression.PrefixOperator prefix:
                    _writer.Byte(10);
                    _writer.String(prefix.Operator);
                    break;

                case Expression.Application application:
                    _writer.Byte(11);
                    WriteExpression(application.Function);
                    WriteExpressions(application.Arguments);
                    break;

                case Expression.OperatorApplication operation:
                    _writer.Byte(12);
                    _writer.String(operation.Operator);
                    WriteDirection(operation.Direction);
                    WriteExpression(operation.Left);
                    WriteExpression(operation.Right);
                    break;

                case Expression.TupledExpression tuple:
                    _writer.Byte(13);
                    WriteExpressions(tuple.Elements);
                    break;

                case Expression.LambdaExpression lambda:
                    _writer.Byte(14);
                    WritePatterns(lambda.Arguments);
                    WriteExpression(lambda.Expression);
                    break;

                case Expression.CaseExpression cases:
                    _writer.Byte(15);
                    WriteExpression(cases.Expression);
                    _writer.Int32(cases.Cases.Count);

                    for (var index = 0; index < cases.Cases.Count; index++)
                    {
                        var branch = cases.Cases[index];
                        WritePattern(branch.Pattern);
                        WriteExpression(branch.Expression);
                    }

                    break;

                case Expression.LetExpression let:
                    _writer.Byte(16);
                    _writer.Int32(let.Declarations.Count);

                    for (var index = 0; index < let.Declarations.Count; index++)
                    {
                        var declaration = let.Declarations[index];

                        switch (declaration)
                        {
                            case LetDeclaration.LetFunction function:
                                _writer.Byte(1);
                                WriteFunction(function.Function);
                                break;

                            case LetDeclaration.LetDestructuring destructuring:
                                _writer.Byte(2);
                                WritePattern(destructuring.Pattern);
                                WriteExpression(destructuring.Expression);
                                break;

                            default:
                                throw new NotImplementedException(
                                    nameof(WriteExpression) + " does not handle let variant: " +
                                    declaration.GetType().Name);
                        }
                    }

                    WriteExpression(let.Expression);
                    break;

                case Expression.RecordExpr record:
                    _writer.Byte(17);
                    WriteSetters(record.Fields);
                    break;

                case Expression.RecordAccess access:
                    _writer.Byte(18);
                    WriteExpression(access.Record);
                    _writer.String(access.FieldName);
                    WriteValue(access.FieldNameValue);
                    break;

                case Expression.RecordAccessFunction access:
                    _writer.Byte(19);
                    _writer.String(access.FieldName);
                    WriteValue(access.FieldNameValue);
                    break;

                case Expression.RecordUpdateExpression update:
                    _writer.Byte(20);
                    _writer.String(update.RecordName);
                    WriteSetters(update.Fields);
                    break;

                case Expression.GLSLExpression glsl:
                    _writer.Byte(21);
                    _writer.String(glsl.ShaderCode);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteExpression) + " does not handle expression variant: " + expression.GetType().Name);
            }

            _expressions.Add(expression, _expressions.Count);
        }

        private void WriteSetters(IReadOnlyList<RecordSetter> fields)
        {
            _writer.Int32(fields.Count);

            for (var index = 0; index < fields.Count; index++)
            {
                var field = fields[index];
                _writer.String(field.FieldName);
                WriteValue(field.FieldNameValue);
                WriteExpression(field.Value);
            }
        }

        private void WritePatterns(IReadOnlyList<Pattern> patterns)
        {
            _writer.Int32(patterns.Count);

            for (var index = 0; index < patterns.Count; index++)
                WritePattern(patterns[index]);
        }

        private void WritePattern(Pattern pattern)
        {
            switch (pattern)
            {
                case Pattern.AllPattern:
                    _writer.Byte(1);
                    break;

                case Pattern.VarPattern variable:
                    _writer.Byte(2);
                    _writer.String(variable.Name);
                    break;

                case Pattern.UnitPattern:
                    _writer.Byte(3);
                    break;

                case Pattern.CharPattern literal:
                    _writer.Byte(4);
                    _writer.Int32(literal.Value);
                    WriteValue(literal.ValueAsPineValue);
                    break;

                case Pattern.StringPattern literal:
                    _writer.Byte(5);
                    _writer.String(literal.Value);
                    WriteValue(literal.ValueAsPineValue);
                    break;

                case Pattern.IntPattern literal:
                    _writer.Byte(6);
                    _writer.Integer(literal.Value);
                    WriteValue(literal.ValueAsPineValue);
                    break;

                case Pattern.FloatPattern literal:
                    _writer.Byte(7);
                    _writer.Integer(literal.Numerator);
                    _writer.Integer(literal.Denominator);
                    break;

                case Pattern.TuplePattern tuple:
                    _writer.Byte(8);
                    WritePatterns(tuple.Elements);
                    break;

                case Pattern.RecordPattern record:
                    _writer.Byte(9);
                    _writer.Int32(record.Fields.Length);

                    foreach (var field in record.Fields)
                    {
                        _writer.String(field.FieldName);
                        WriteValue(field.FieldNameValue);
                    }

                    break;

                case Pattern.UnConsPattern cons:
                    _writer.Byte(10);
                    WritePattern(cons.Head);
                    WritePattern(cons.Tail);
                    break;

                case Pattern.ListPattern list:
                    _writer.Byte(11);
                    WritePatterns(list.Elements);
                    break;

                case Pattern.NamedPattern named:
                    _writer.Byte(12);
                    WriteStrings(named.Name.ModuleName);
                    _writer.String(named.Name.Name);
                    WritePatterns(named.Arguments);
                    WriteValue(named.TagNameAsPineValue);
                    break;

                case Pattern.AsPattern alias:
                    _writer.Byte(13);
                    WritePattern(alias.Pattern);
                    _writer.String(alias.Name);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WritePattern) + " does not handle pattern variant: " + pattern.GetType().Name);
            }
        }

        private void WriteValue(PineValue value)
        {
            _values ??= [];

            if (_values.TryGetValue(value, out var reference))
            {
                _writer.Byte(0);
                _writer.Int32(reference);
                return;
            }

            switch (value)
            {
                case PineValue.BlobValue blob:
                    _writer.Byte(1);
                    _writer.Int32(blob.Bytes.Length);
                    _writer.Bytes(blob.Bytes.Span);
                    break;

                case PineValue.ListValue list:
                    _writer.Byte(2);
                    _writer.Int32(list.Items.Length);

                    foreach (var item in list.Items.Span)
                        WriteValue(item);

                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteValue) + " does not handle Pine value variant: " + value.GetType().Name);
            }

            _values.Add(value, _values.Count);
        }
    }
}
