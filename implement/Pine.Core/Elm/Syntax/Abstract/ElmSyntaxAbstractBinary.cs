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
/// Abstract syntax represented by hash-addressed Pine constructor expressions and shallow
/// environments. Sessions share retained nodes; no canonical Pine trees are built at runtime.
/// </summary>
public static partial class ElmSyntaxAbstractBinary
{
    private sealed record Digest(ReadOnlyMemory<byte> Bytes);

    private static readonly ConditionalWeakTable<Declaration, Digest> s_hashes = [];

    /// <summary>Streams the standalone declaration bytes into SHA-256, with weak identity memoization.</summary>
    public static ReadOnlyMemory<byte> ComputeHash(Declaration declaration) =>
        s_hashes.GetValue(
            declaration,
            static syntax =>
            {
                using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);
                Encode(hash.AppendData, syntax);
                return new(hash.GetHashAndReset());
            }).Bytes;

    /// <summary>Writes one expression in an independent reference scope.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, Expression expression)
    {
        using var encoder = new Encoder(write);
        encoder.WriteExpression(expression);
    }

    /// <summary>Writes one declaration in an independent reference scope.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, Declaration declaration)
    {
        using var encoder = new Encoder(write);
        encoder.WriteDeclaration(declaration);
    }

    /// <summary>Writes a declaration list sharing one reference scope.</summary>
    public static void Encode(Action<ReadOnlySpan<byte>> write, IReadOnlyList<Declaration> declarations)
    {
        using var encoder = new Encoder(write);
        encoder.WriteDeclarations(declarations);
    }

    /// <summary>Writes one expression without closing the stream.</summary>
    public static void Encode(Stream stream, Expression expression) => Encode(stream.Write, expression);

    /// <summary>Writes one declaration without closing the stream.</summary>
    public static void Encode(Stream stream, Declaration declaration) => Encode(stream.Write, declaration);

    /// <summary>Writes a declaration list without closing the stream.</summary>
    public static void Encode(Stream stream, IReadOnlyList<Declaration> declarations) =>
        Encode(stream.Write, declarations);

    /// <summary>Reads one expression, reporting its extent and leaving trailing bytes to the caller.</summary>
    public static Expression DecodeExpression(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        using var decoder = new Decoder(source);
        var result = decoder.ReadExpression();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>Reads one declaration, reporting its extent.</summary>
    public static Declaration DecodeDeclaration(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        using var decoder = new Decoder(source);
        var result = decoder.ReadDeclaration();
        bytesRead = decoder.BytesRead;
        return result;
    }

    /// <summary>Reads a declaration list with shared retained instances, reporting its extent.</summary>
    public static IReadOnlyList<Declaration> DecodeDeclarations(ReadOnlyMemory<byte> source, out int bytesRead)
    {
        using var decoder = new Decoder(source);
        var result = decoder.ReadDeclarations();
        bytesRead = decoder.BytesRead;
        return result;
    }

    private enum Constructor
    {
        String, QualifiedDeclaration, QualifiedPattern, None, Some,
        Function, Implementation, Signature, Choice, Alias, Infix, ValueConstructor,
        RecordDefinition, RecordField, RecordSetter, Case, LetFunction, LetDestructuring,
        FunctionDeclaration, ChoiceTypeDeclaration, AliasDeclaration, PortDeclaration, InfixDeclaration,
        GenericType, Typed, Unit, Tupled, Record, GenericRecord, FunctionTypeAnnotation,
        AllPattern, VarPattern, UnitPattern, CharPattern, StringPattern, IntPattern, FloatPattern,
        TuplePattern, RecordPattern, UnConsPattern, ListPattern, NamedPattern, AsPattern,
        UnitExpr, StringLiteral, CharLiteral, IntegerLiteral, FloatLiteral, Negation, ListExpr,
        Identifier, IfBlock, PrefixOperator, Application, OperatorApplication, TupledExpression,
        LambdaExpression, CaseExpression, LetExpression, RecordExpr, RecordAccess,
        RecordAccessFunction, RecordUpdateExpression, GLSLExpression
    }

    /// <summary>Streaming writer; disposing releases bounded, reusable reference dictionaries.</summary>
    /// <remarks>Starts an independent reference scope.</remarks>
    public sealed class Encoder(Action<ReadOnlySpan<byte>> write) : IDisposable
    {
        private ValueEncodingBinaryDeterministic.TreeEncoder<object> _writer = new(write, reuseReferences: true);

        /// <summary>All writes are synchronous; no bytes remain buffered.</summary>
        public void Flush() => _writer.EnsureActive();

        /// <inheritdoc/>
        public void Dispose() => _writer.Dispose();

        private void Start(Constructor constructor, int count) =>
            _writer.StartExternalEval(ConstructorHash(constructor), count);

        private void String(string value)
        {
            if (_writer.TryReference(value))
                return;

            Start(Constructor.String, 1);
            _writer.Utf32(value);
        }

        private void Strings(IReadOnlyList<string> values)
        {
            for (var index = 0; index < values.Count; ++index)
                String(values[index]);
        }

        private void Value(PineValue value)
        {
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
                        Value(item);

                    break;

                default:
                    throw new NotImplementedException(
                        nameof(Value) + " does not handle Pine value variant: " + value.GetType().Name);
            }
        }

        private void Direction(InfixDirection direction)
        {
            if (direction is not (InfixDirection.Left or InfixDirection.Right or InfixDirection.Non))
                throw new ArgumentOutOfRangeException(nameof(direction), direction, "Unknown infix direction.");

            _writer.Integer(direction == InfixDirection.Left ? 0 : direction == InfixDirection.Right ? 1 : 2);
        }

        /// <summary>Writes a declaration list without resetting session references.</summary>
        public void WriteDeclarations(IReadOnlyList<Declaration> declarations)
        {
            _writer.StartList(declarations.Count);

            for (var index = 0; index < declarations.Count; ++index)
                WriteDeclaration(declarations[index]);
        }

        /// <summary>Writes a declaration in this session.</summary>
        public void WriteDeclaration(Declaration declaration)
        {
            switch (declaration)
            {
                case Declaration.FunctionDeclaration function:
                    Start(Constructor.FunctionDeclaration, 1);
                    Function(function.Function);
                    break;

                case Declaration.ChoiceTypeDeclaration choice:
                    Start(Constructor.ChoiceTypeDeclaration, 1);

                    Start(
                        Constructor.Choice,
                        2 + choice.TypeDeclaration.Generics.Count + choice.TypeDeclaration.Constructors.Count);

                    String(choice.TypeDeclaration.Name);
                    _writer.Integer(choice.TypeDeclaration.Generics.Count);
                    Strings(choice.TypeDeclaration.Generics);

                    for (var index = 0; index < choice.TypeDeclaration.Constructors.Count; ++index)
                    {
                        var constructor = choice.TypeDeclaration.Constructors[index];
                        Start(Constructor.ValueConstructor, 2 + constructor.Arguments.Count);
                        String(constructor.Name);
                        Value(constructor.NameValue);
                        Annotations(constructor.Arguments);
                    }

                    break;

                case Declaration.AliasDeclaration alias:
                    Start(Constructor.AliasDeclaration, 1);
                    Start(Constructor.Alias, 3 + alias.TypeAlias.Generics.Count);
                    String(alias.TypeAlias.Name);
                    _writer.Integer(alias.TypeAlias.Generics.Count);
                    Strings(alias.TypeAlias.Generics);
                    WriteTypeAnnotation(alias.TypeAlias.TypeAnnotation);
                    break;

                case Declaration.PortDeclaration port:
                    Start(Constructor.PortDeclaration, 1);
                    Signature(port.Signature, optional: false);
                    break;

                case Declaration.InfixDeclaration infix:
                    Start(Constructor.InfixDeclaration, 1);
                    Start(Constructor.Infix, 4);
                    Direction(infix.Infix.Direction);
                    _writer.Integer(infix.Infix.Precedence);
                    String(infix.Infix.Operator);
                    String(infix.Infix.FunctionName);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteDeclaration) + " does not handle declaration variant: " +
                        declaration.GetType().Name);
            }
        }

        private void Signature(Signature signature, bool optional)
        {
            if (optional)
                Start(Constructor.Some, 1);

            Start(Constructor.Signature, 2);
            String(signature.Name);
            WriteTypeAnnotation(signature.TypeAnnotation);
        }

        private void Function(FunctionStruct function)
        {
            Start(Constructor.Function, 2);

            if (function.Signature is { } signature)
                Signature(signature, optional: true);

            else
                Start(Constructor.None, 0);

            Start(Constructor.Implementation, 2 + function.Declaration.Arguments.Count);
            String(function.Declaration.Name);
            WriteExpression(function.Declaration.Expression);
            Patterns(function.Declaration.Arguments);
        }

        private void Annotations(IReadOnlyList<TypeAnnotation> annotations)
        {
            for (var index = 0; index < annotations.Count; ++index)
                WriteTypeAnnotation(annotations[index]);
        }

        /// <summary>Writes an annotation, referencing an earlier structural equal when available.</summary>
        public void WriteTypeAnnotation(TypeAnnotation annotation)
        {
            if (_writer.TryReference(annotation))
                return;

            switch (annotation)
            {
                case TypeAnnotation.GenericType generic:
                    Start(Constructor.GenericType, 1);
                    String(generic.Name);
                    break;

                case TypeAnnotation.Typed typed:
                    Start(Constructor.Typed, 2 + typed.ModuleName.Count + typed.TypeArguments.Count);
                    _writer.Integer(typed.ModuleName.Count);
                    Strings(typed.ModuleName);
                    String(typed.Name);
                    Annotations(typed.TypeArguments);
                    break;

                case TypeAnnotation.Unit:
                    Start(Constructor.Unit, 0);
                    break;

                case TypeAnnotation.Tupled tuple:
                    Start(Constructor.Tupled, tuple.TypeAnnotations.Count);
                    Annotations(tuple.TypeAnnotations);
                    break;

                case TypeAnnotation.Record record:
                    Start(Constructor.Record, 1);
                    RecordDefinition(record.RecordDefinition);
                    break;

                case TypeAnnotation.GenericRecord record:
                    Start(Constructor.GenericRecord, 2);
                    String(record.GenericName);
                    RecordDefinition(record.RecordDefinition);
                    break;

                case TypeAnnotation.FunctionTypeAnnotation function:
                    Start(Constructor.FunctionTypeAnnotation, 2);
                    WriteTypeAnnotation(function.ArgumentType);
                    WriteTypeAnnotation(function.ReturnType);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteTypeAnnotation) + " does not handle annotation variant: " +
                        annotation.GetType().Name);
            }
        }

        private void RecordDefinition(RecordDefinition record)
        {
            Start(Constructor.RecordDefinition, record.Fields.Count);

            for (var index = 0; index < record.Fields.Count; ++index)
            {
                var field = record.Fields[index];
                Start(Constructor.RecordField, 3);
                String(field.FieldName);
                Value(field.FieldNameValue);
                WriteTypeAnnotation(field.FieldType);
            }
        }

        private void Patterns(IReadOnlyList<Pattern> patterns)
        {
            for (var index = 0; index < patterns.Count; ++index)
                WritePattern(patterns[index]);
        }

        private void WritePattern(Pattern pattern)
        {
            switch (pattern)
            {
                case Pattern.AllPattern:
                    Start(Constructor.AllPattern, 0);
                    break;

                case Pattern.VarPattern variable:
                    Start(Constructor.VarPattern, 1);
                    String(variable.Name);
                    break;

                case Pattern.UnitPattern:
                    Start(Constructor.UnitPattern, 0);
                    break;

                case Pattern.CharPattern literal:
                    Start(Constructor.CharPattern, 2);
                    _writer.Integer(literal.Value);
                    Value(literal.ValueAsPineValue);
                    break;

                case Pattern.StringPattern literal:
                    Start(Constructor.StringPattern, 2);
                    String(literal.Value);
                    Value(literal.ValueAsPineValue);
                    break;

                case Pattern.IntPattern literal:
                    Start(Constructor.IntPattern, 2);
                    _writer.Integer(literal.Value);
                    Value(literal.ValueAsPineValue);
                    break;

                case Pattern.FloatPattern literal:
                    Start(Constructor.FloatPattern, 2);
                    _writer.Integer(literal.Numerator);
                    _writer.Integer(literal.Denominator);
                    break;

                case Pattern.TuplePattern tuple:
                    Start(Constructor.TuplePattern, tuple.Elements.Count);
                    Patterns(tuple.Elements);
                    break;

                case Pattern.RecordPattern record:
                    Start(Constructor.RecordPattern, 1 + record.Fields.Length * 2);
                    _writer.Integer(record.Fields.Length);

                    foreach (var field in record.Fields)
                        String(field.FieldName);

                    foreach (var field in record.Fields)
                        Value(field.FieldNameValue);

                    break;

                case Pattern.UnConsPattern cons:
                    Start(Constructor.UnConsPattern, 2);
                    WritePattern(cons.Head);
                    WritePattern(cons.Tail);
                    break;

                case Pattern.ListPattern list:
                    Start(Constructor.ListPattern, list.Elements.Count);
                    Patterns(list.Elements);
                    break;

                case Pattern.NamedPattern named:
                    Start(Constructor.NamedPattern, 2 + named.Arguments.Count);

                    if (!_writer.TryReference(named.Name))
                    {
                        Start(Constructor.QualifiedPattern, 1 + named.Name.ModuleName.Count);
                        String(named.Name.Name);
                        Strings(named.Name.ModuleName);
                    }

                    Value(named.TagNameAsPineValue);
                    Patterns(named.Arguments);
                    break;

                case Pattern.AsPattern alias:
                    Start(Constructor.AsPattern, 2);
                    WritePattern(alias.Pattern);
                    String(alias.Name);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WritePattern) + " does not handle pattern variant: " + pattern.GetType().Name);
            }
        }

        private void Expressions(IReadOnlyList<Expression> expressions)
        {
            for (var index = 0; index < expressions.Count; ++index)
                WriteExpression(expressions[index]);
        }

        private void Setters(IReadOnlyList<RecordSetter> setters)
        {
            for (var index = 0; index < setters.Count; ++index)
            {
                var setter = setters[index];
                Start(Constructor.RecordSetter, 3);
                String(setter.FieldName);
                Value(setter.FieldNameValue);
                WriteExpression(setter.Value);
            }
        }

        /// <summary>Writes an expression, referencing an earlier structural equal when available.</summary>
        public void WriteExpression(Expression expression)
        {
            if (_writer.TryReference(expression))
                return;

            switch (expression)
            {
                case Expression.UnitExpr:
                    Start(Constructor.UnitExpr, 0);
                    break;

                case Expression.StringLiteral literal:
                    Start(Constructor.StringLiteral, 2);
                    String(literal.Value);
                    Value(literal.ValueAsPineValue);
                    break;

                case Expression.CharLiteral literal:
                    Start(Constructor.CharLiteral, 2);
                    _writer.Integer(literal.Value);
                    Value(literal.ValueAsPineValue);
                    break;

                case Expression.IntegerLiteral literal:
                    Start(Constructor.IntegerLiteral, 2);
                    _writer.Integer(literal.Value);
                    Value(literal.ValueAsPineValue);
                    break;

                case Expression.FloatLiteral literal:
                    Start(Constructor.FloatLiteral, 2);
                    _writer.Integer(literal.Numerator);
                    _writer.Integer(literal.Denominator);
                    break;

                case Expression.Negation negation:
                    Start(Constructor.Negation, 1);
                    WriteExpression(negation.Expression);
                    break;

                case Expression.ListExpr list:
                    Start(Constructor.ListExpr, list.Elements.Count);
                    Expressions(list.Elements);
                    break;

                case Expression.Identifier identifier:
                    Start(Constructor.Identifier, 1);

                    if (!_writer.TryReference(identifier.QualifiedName))
                    {
                        Start(Constructor.QualifiedDeclaration, 1 + identifier.QualifiedName.Namespaces.Count);
                        String(identifier.QualifiedName.DeclName);
                        Strings(identifier.QualifiedName.Namespaces);
                    }

                    break;

                case Expression.IfBlock conditional:
                    Start(Constructor.IfBlock, 3);
                    WriteExpression(conditional.Condition);
                    WriteExpression(conditional.ThenBlock);
                    WriteExpression(conditional.ElseBlock);
                    break;

                case Expression.PrefixOperator prefix:
                    Start(Constructor.PrefixOperator, 1);
                    String(prefix.Operator);
                    break;

                case Expression.Application application:
                    Start(Constructor.Application, 1 + application.Arguments.Count);
                    WriteExpression(application.Function);
                    Expressions(application.Arguments);
                    break;

                case Expression.OperatorApplication operation:
                    Start(Constructor.OperatorApplication, 4);
                    String(operation.Operator);
                    Direction(operation.Direction);
                    WriteExpression(operation.Left);
                    WriteExpression(operation.Right);
                    break;

                case Expression.TupledExpression tuple:
                    Start(Constructor.TupledExpression, tuple.Elements.Count);
                    Expressions(tuple.Elements);
                    break;

                case Expression.LambdaExpression lambda:
                    Start(Constructor.LambdaExpression, 1 + lambda.Arguments.Count);
                    WriteExpression(lambda.Expression);
                    Patterns(lambda.Arguments);
                    break;

                case Expression.CaseExpression cases:
                    Start(Constructor.CaseExpression, 1 + cases.Cases.Count);
                    WriteExpression(cases.Expression);

                    for (var index = 0; index < cases.Cases.Count; ++index)
                    {
                        var branch = cases.Cases[index];
                        Start(Constructor.Case, 2);
                        WritePattern(branch.Pattern);
                        WriteExpression(branch.Expression);
                    }

                    break;

                case Expression.LetExpression let:
                    Start(Constructor.LetExpression, 1 + let.Declarations.Count);
                    WriteExpression(let.Expression);

                    for (var index = 0; index < let.Declarations.Count; ++index)
                    {
                        var declaration = let.Declarations[index];

                        switch (declaration)
                        {
                            case LetDeclaration.LetFunction function:
                                Start(Constructor.LetFunction, 1);
                                Function(function.Function);
                                break;

                            case LetDeclaration.LetDestructuring destructuring:
                                Start(Constructor.LetDestructuring, 2);
                                WritePattern(destructuring.Pattern);
                                WriteExpression(destructuring.Expression);
                                break;

                            default:
                                throw new NotImplementedException(
                                    nameof(WriteExpression) + " does not handle let variant: " +
                                    declaration.GetType().Name);
                        }
                    }

                    break;

                case Expression.RecordExpr record:
                    Start(Constructor.RecordExpr, record.Fields.Count);
                    Setters(record.Fields);
                    break;

                case Expression.RecordAccess access:
                    Start(Constructor.RecordAccess, 3);
                    WriteExpression(access.Record);
                    String(access.FieldName);
                    Value(access.FieldNameValue);
                    break;

                case Expression.RecordAccessFunction access:
                    Start(Constructor.RecordAccessFunction, 2);
                    String(access.FieldName);
                    Value(access.FieldNameValue);
                    break;

                case Expression.RecordUpdateExpression update:
                    Start(Constructor.RecordUpdateExpression, 1 + update.Fields.Count);
                    String(update.RecordName);
                    Setters(update.Fields);
                    break;

                case Expression.GLSLExpression glsl:
                    Start(Constructor.GLSLExpression, 1);
                    String(glsl.ShaderCode);
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(WriteExpression) + " does not handle expression variant: " + expression.GetType().Name);
            }
        }
    }
}
