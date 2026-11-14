using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using InfixDirection = Pine.Core.Elm.Syntax.Concrete.InfixDirection;

namespace Pine.Core.Elm.Syntax.Abstract;

public static partial class ElmSyntaxAbstractBinary
{
    /// <summary>
    /// Reads roots from one encoding session. BytesRead includes the header and permits
    /// embedding in an enclosing byte sequence. Malformed input throws InvalidDataException.
    /// </summary>
    public ref struct Decoder
    {
        private BinaryEncoding.Reader _reader;

        private List<Expression>? _expressions;

        private List<TypeAnnotation>? _annotations;

        private List<PineValue>? _values;

        private int _depth;

        /// <summary>Starts a reference scope at the beginning of the supplied memory.</summary>
        public Decoder(ReadOnlyMemory<byte> source)
        {
            _reader = new(source);

            if (_reader.Int32() != Header)
                throw _reader.Error("Unsupported abstract syntax format or version");
        }

        /// <summary>Number of consumed bytes, excluding any trailing enclosing data.</summary>
        public readonly int BytesRead => _reader.Position;

        private void Enter()
        {
            if (++_depth > 512)
                throw _reader.Error("Abstract syntax nesting exceeds 512 levels");
        }

        private T Reference<T>(List<T>? entries)
        {
            var index = _reader.Int32();

            if (entries is null || (uint)index >= (uint)entries.Count)
                throw _reader.Error("Invalid or forward internal reference");

            return entries[index];
        }

        /// <summary>Reads a counted declaration batch within this session.</summary>
        public Declaration[] ReadDeclarations()
        {
            var result = new Declaration[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
                result[index] = ReadDeclaration();

            return result;
        }

        /// <summary>Reads a declaration, retaining its expression and annotation instances.</summary>
        public Declaration ReadDeclaration() =>
            _reader.Byte() switch
            {
                1 => new Declaration.FunctionDeclaration(ReadFunction()),

                2 =>
                new Declaration.ChoiceTypeDeclaration(
                    new(_reader.String(), ReadStrings(), ReadConstructors())),

                3 =>
                new Declaration.AliasDeclaration(
                    new(_reader.String(), ReadStrings(), ReadTypeAnnotation())),

                4 => new Declaration.PortDeclaration(ReadSignature()),

                5 =>
                new Declaration.InfixDeclaration(
                    new(ReadDirection(), _reader.Int32(), _reader.String(), _reader.String())),

                _ =>
                throw _reader.Error("Unknown declaration tag")
            };

        private FunctionStruct ReadFunction()
        {
            var signature =
                _reader.Byte() switch
                {
                    0 => null,
                    1 => ReadSignature(),

                    _ =>
                    throw _reader.Error("Invalid optional signature tag")
                };

            return new(signature, new(_reader.String(), ReadPatterns(), ReadExpression()));
        }

        private Signature ReadSignature() => new(_reader.String(), ReadTypeAnnotation());

        private string[] ReadStrings()
        {
            var result = new string[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
                result[index] = _reader.String();

            return result;
        }

        private ValueConstructor[] ReadConstructors()
        {
            var result = new ValueConstructor[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
                result[index] = new(_reader.String(), ReadValue(), ReadAnnotations());

            return result;
        }

        private TypeAnnotation[] ReadAnnotations()
        {
            var result = new TypeAnnotation[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
                result[index] = ReadTypeAnnotation();

            return result;
        }

        /// <summary>Reads an annotation; back-references return the exact previously decoded instance.</summary>
        public TypeAnnotation ReadTypeAnnotation()
        {
            Enter();

            try
            {
                var tag = _reader.Byte();

                if (tag is 0)
                    return Reference(_annotations);

                TypeAnnotation result =
                    tag switch
                    {
                        1 => new TypeAnnotation.GenericType(_reader.String()),
                        2 => new TypeAnnotation.Typed(ReadStrings(), _reader.String(), ReadAnnotations()),
                        3 => new TypeAnnotation.Unit(),
                        4 => new TypeAnnotation.Tupled(ReadAnnotations()),
                        5 => new TypeAnnotation.Record(ReadRecordDefinition()),
                        6 => new TypeAnnotation.GenericRecord(_reader.String(), ReadRecordDefinition()),
                        7 => new TypeAnnotation.FunctionTypeAnnotation(ReadTypeAnnotation(), ReadTypeAnnotation()),

                        _ =>
                        throw _reader.Error("Unknown annotation tag")
                    };

                (_annotations ??= []).Add(result);
                return result;
            }
            finally
            {
                --_depth;
            }
        }

        private RecordDefinition ReadRecordDefinition()
        {
            var result = new RecordField[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
                result[index] = new(_reader.String(), ReadValue(), ReadTypeAnnotation());

            return new(result);
        }

        private Expression[] ReadExpressions()
        {
            var result = new Expression[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
                result[index] = ReadExpression();

            return result;
        }

        /// <summary>Reads an expression; back-references return the exact previously decoded instance.</summary>
        public Expression ReadExpression()
        {
            Enter();

            try
            {
                var tag = _reader.Byte();

                if (tag is 0)
                    return Reference(_expressions);

                Expression result =
                    tag switch
                    {
                        1 => Expression.UnitExpr.Instance,
                        2 => new Expression.StringLiteral(_reader.String(), ReadValue()),
                        3 => new Expression.CharLiteral(_reader.Int32(), ReadValue()),
                        4 => new Expression.IntegerLiteral(_reader.Integer(), ReadValue()),
                        5 => new Expression.FloatLiteral(_reader.Integer(), _reader.Integer()),
                        6 => new Expression.Negation(ReadExpression()),
                        7 => new Expression.ListExpr(ReadExpressions()),
                        8 => new Expression.Identifier(DeclQualifiedName.Create(ReadStrings(), _reader.String())),
                        9 => new Expression.IfBlock(ReadExpression(), ReadExpression(), ReadExpression()),
                        10 => new Expression.PrefixOperator(_reader.String()),
                        11 => new Expression.Application(ReadExpression(), ReadExpressions()),

                        12 =>
                        new Expression.OperatorApplication(
                            _reader.String(),
                            ReadDirection(),
                            ReadExpression(),
                            ReadExpression()),

                        13 => new Expression.TupledExpression(ReadExpressions()),
                        14 => new Expression.LambdaExpression(ReadPatterns(), ReadExpression()),
                        15 => new Expression.CaseExpression(ReadExpression(), ReadCases()),
                        16 => new Expression.LetExpression(ReadLetDeclarations(), ReadExpression()),
                        17 => new Expression.RecordExpr(ReadSetters()),
                        18 => new Expression.RecordAccess(ReadExpression(), _reader.String(), ReadValue()),
                        19 => new Expression.RecordAccessFunction(_reader.String(), ReadValue()),
                        20 => new Expression.RecordUpdateExpression(_reader.String(), ReadSetters()),
                        21 => new Expression.GLSLExpression(_reader.String()),

                        _ =>
                        throw _reader.Error("Unknown expression tag")
                    };

                (_expressions ??= []).Add(result);
                return result;
            }
            finally
            {
                --_depth;
            }
        }

        private InfixDirection ReadDirection() =>
            _reader.Byte() switch
            {
                0 => InfixDirection.Left,
                1 => InfixDirection.Right,
                2 => InfixDirection.Non,

                _ =>
                throw _reader.Error("Invalid infix direction")
            };

        private Case[] ReadCases()
        {
            var result = new Case[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
                result[index] = new(ReadPattern(), ReadExpression());

            return result;
        }

        private LetDeclaration[] ReadLetDeclarations()
        {
            var result = new LetDeclaration[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
            {
                result[index] =
                    _reader.Byte() switch
                    {
                        1 => new LetDeclaration.LetFunction(ReadFunction()),
                        2 => new LetDeclaration.LetDestructuring(ReadPattern(), ReadExpression()),

                        _ =>
                        throw _reader.Error("Unknown let-declaration tag")
                    };
            }

            return result;
        }

        private RecordSetter[] ReadSetters()
        {
            var result = new RecordSetter[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
                result[index] = new(_reader.String(), ReadValue(), ReadExpression());

            return result;
        }

        private Pattern[] ReadPatterns()
        {
            var result = new Pattern[_reader.Count()];

            for (var index = 0; index < result.Length; index++)
                result[index] = ReadPattern();

            return result;
        }

        private ImmutableArray<(string, PineValue)> ReadPatternFields()
        {
            var result = ImmutableArray.CreateBuilder<(string, PineValue)>(_reader.Count());

            while (result.Count < result.Capacity)
                result.Add((_reader.String(), ReadValue()));

            return result.MoveToImmutable();
        }

        private Pattern ReadPattern()
        {
            Enter();

            try
            {
                return _reader.Byte() switch
                {
                    1 => new Pattern.AllPattern(),
                    2 => new Pattern.VarPattern(_reader.String()),
                    3 => new Pattern.UnitPattern(),
                    4 => new Pattern.CharPattern(_reader.Int32(), ReadValue()),
                    5 => new Pattern.StringPattern(_reader.String(), ReadValue()),
                    6 => new Pattern.IntPattern(_reader.Integer(), ReadValue()),
                    7 => new Pattern.FloatPattern(_reader.Integer(), _reader.Integer()),
                    8 => new Pattern.TuplePattern(ReadPatterns()),
                    9 => new Pattern.RecordPattern(ReadPatternFields()),
                    10 => new Pattern.UnConsPattern(ReadPattern(), ReadPattern()),
                    11 => new Pattern.ListPattern(ReadPatterns()),

                    12 =>
                    new Pattern.NamedPattern(
                        new(ReadStrings(), _reader.String()),
                        ReadPatterns(),
                        ReadValue()),

                    13 => new Pattern.AsPattern(ReadPattern(), _reader.String()),

                    _ =>
                    throw _reader.Error("Unknown pattern tag")
                };
            }
            finally
            {
                --_depth;
            }
        }

        private PineValue ReadValue()
        {
            Enter();

            try
            {
                var tag = _reader.Byte();

                if (tag is 0)
                    return Reference(_values);

                PineValue result;

                switch (tag)
                {
                    case 1:
                        result = PineValue.Blob(_reader.Memory(_reader.Int32()));
                        break;

                    case 2:
                        var items = new PineValue[_reader.Count()];

                        for (var index = 0; index < items.Length; index++)
                            items[index] = ReadValue();

                        result = PineValue.List(items);
                        break;

                    default:
                        throw _reader.Error("Unknown Pine value tag");
                }

                (_values ??= []).Add(result);
                return result;
            }
            finally
            {
                --_depth;
            }
        }
    }
}
