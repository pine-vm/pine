using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Runtime;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.IO;
using System.Numerics;
using System.Runtime.ExceptionServices;
using System.Security.Cryptography;
using System.Threading;
using Xunit;

using InfixDirection = Pine.Core.Elm.Syntax.Concrete.InfixDirection;
using PineValue = Pine.Core.PineLanguage.PineValue;

namespace Pine.Core.Tests.Elm.Syntax.Abstract;

public class BinaryEncodingTests
{
    private static Expression Integer(BigInteger value) =>
        new Expression.IntegerLiteral(value, IntegerEncoding.EncodeSignedInteger(value));

    private static Declaration.FunctionDeclaration Function(
        string name, Expression body, TypeAnnotation? type = null) =>
        new(new(type is null ? null : new(name, type), new(name, [], body)));

    private static byte[] Encode(Expression expression)
    {
        using var stream = new MemoryStream();
        ElmSyntaxAbstractBinary.Encode(stream, expression);
        return stream.ToArray();
    }

    private static byte[] Encode(IReadOnlyList<Declaration> declarations)
    {
        using var stream = new MemoryStream();
        ElmSyntaxAbstractBinary.Encode(stream, declarations);
        return stream.ToArray();
    }

    private static byte[] EncodeNestedNegations(int depth)
    {
        using var stream = new MemoryStream();
        using var writer = new ValueEncodingBinaryDeterministic.TreeEncoder<object>(stream.Write);
        var negationHash = Encode(new Expression.Negation(Expression.UnitExpr.Instance)).AsSpan(8, 32);

        for (var index = 0; index < depth; ++index)
            writer.StartExternalEval(negationHash, 1);

        writer.StartExternalEval(Encode(Expression.UnitExpr.Instance).AsSpan(8, 32), 0);
        return stream.ToArray();
    }

    private static Pattern[] Patterns() =>
        [
            new Pattern.AllPattern(),
            new Pattern.VarPattern("value"),
            new Pattern.UnitPattern(),
            new Pattern.CharPattern(0x1f600),
            new Pattern.StringPattern("escaped\n\"text\"\u00e9"),
            new Pattern.IntPattern(-(BigInteger.One << 130)),
            new Pattern.FloatPattern(-(BigInteger.One << 130), BigInteger.Pow(10, 1001)),
            new Pattern.TuplePattern([new Pattern.VarPattern("x"), new Pattern.AllPattern()]),
            Pattern.RecordPattern.Create(["x", "y"]),
            new Pattern.UnConsPattern(new Pattern.VarPattern("head"), new Pattern.VarPattern("tail")),
            new Pattern.ListPattern([new Pattern.IntPattern(1)]),
            new Pattern.NamedPattern(new(["Maybe"], "Just"), [new Pattern.VarPattern("x")]),
            new Pattern.AsPattern(new Pattern.AllPattern(), "whole"),
            new Pattern.NamedPattern(
                new(["Acme", "Tree"], "Branch"),
                [
                new Pattern.VarPattern("x"),
                new Pattern.VarPattern("y")
                ]),
            new Pattern.NamedPattern(new([], "Nothing"), []),
            new Pattern.TuplePattern([]),
            Pattern.RecordPattern.Create([]),
            new Pattern.ListPattern([]),
            new Pattern.CharPattern(0),
            new Pattern.StringPattern(""),
            new Pattern.IntPattern(0),
            new Pattern.IntPattern(BigInteger.One << 130),
            new Pattern.FloatPattern(7, -3)
        ];

    private static Expression[] Expressions() =>
        [
            Expression.UnitExpr.Instance,
            Expression.StringLiteral.Create("escaped\n\"text\"\u00e9\U0001f600"),
            Expression.CharLiteral.Create(0x1f600),
            Integer(-(BigInteger.One << 130)),
            Integer(0),
            new Expression.FloatLiteral(-(BigInteger.One << 80), 100),
            new Expression.Negation(Integer(2)),
            new Expression.ListExpr([Integer(0), Integer(1)]),
            new Expression.ListExpr([]),
            new Expression.Identifier(DeclQualifiedName.Create(["Some", "Module"], "value")),
            new Expression.IfBlock(Integer(1), Integer(2), Integer(3)),
            new Expression.PrefixOperator("+"),
            new Expression.Application(Expression.Identifier.Create([], "f"), [Integer(1)]),
            new Expression.OperatorApplication("+", InfixDirection.Left, Integer(1), Integer(2)),
            new Expression.OperatorApplication("|>", InfixDirection.Right, Integer(1), Integer(2)),
            new Expression.OperatorApplication("==", InfixDirection.Non, Integer(1), Integer(2)),
            new Expression.TupledExpression([Integer(1), Integer(2)]),
            new Expression.LambdaExpression(Patterns(), Expression.Identifier.Create([], "value")),
            new Expression.CaseExpression(
                Integer(0),
                [new(new Pattern.IntPattern(0), Integer(1)), new(new Pattern.AllPattern(), Integer(2))]),
            new Expression.LetExpression(
                [
                    new LetDeclaration.LetFunction(Function("local", Integer(1)).Function),
                    new LetDeclaration.LetDestructuring(new Pattern.VarPattern("bound"), Integer(2))
                ],
                Expression.Identifier.Create([], "bound")),
            new Expression.RecordExpr([new("field", StringEncoding.ValueFromString("field"), Integer(3))]),
            new Expression.RecordAccess(
                Expression.Identifier.Create([], "record"),
                "field",
                StringEncoding.ValueFromString("field")),
            new Expression.RecordAccessFunction("field", StringEncoding.ValueFromString("field")),
            new Expression.RecordUpdateExpression(
                "record",
                [
                new("field", StringEncoding.ValueFromString("field"), Integer(4))
                ]),
            new Expression.GLSLExpression("void main() {}"),
            // Precomputed values are payload, not silently regenerated by the codec.
            new Expression.StringLiteral("literal", PineValue.List(PineValue.Blob([7, 8, 9]))),
            Expression.StringLiteral.Create(""),
            Expression.CharLiteral.Create(0),
            Integer(BigInteger.One << 130),
            new Expression.FloatLiteral(7, -3),
            new Expression.Application(Expression.UnitExpr.Instance, []),
            new Expression.TupledExpression([]),
            new Expression.LambdaExpression([], Expression.UnitExpr.Instance),
            new Expression.CaseExpression(Expression.UnitExpr.Instance, []),
            new Expression.LetExpression([], Expression.UnitExpr.Instance),
            new Expression.RecordExpr([]),
            new Expression.RecordUpdateExpression("", []),
            new Expression.GLSLExpression("")
        ];

    private static TypeAnnotation[] Annotations() =>
        [
            new TypeAnnotation.GenericType("a"),
            new TypeAnnotation.Typed(["Some", "Module"], "Type", [new TypeAnnotation.GenericType("a")]),
            new TypeAnnotation.Unit(),
            new TypeAnnotation.Tupled([new TypeAnnotation.Unit(), new TypeAnnotation.GenericType("a")]),
            new TypeAnnotation.Record(new([RecordField.Create("field", new TypeAnnotation.Unit())])),
            new TypeAnnotation.GenericRecord(
                "row",
                new([RecordField.Create("field", new TypeAnnotation.GenericType("a"))])),
            new TypeAnnotation.FunctionTypeAnnotation(new TypeAnnotation.GenericType("a"), new TypeAnnotation.Unit()),
            new TypeAnnotation.Typed([], "Int", []),
            new TypeAnnotation.Tupled([]),
            new TypeAnnotation.Record(new([])),
            new TypeAnnotation.GenericRecord("", new([]))
        ];

    private static IReadOnlyList<Declaration> Declarations()
    {
        var declarations = new List<Declaration>();

        foreach (var annotation in Annotations())
            declarations.Add(Function("function", new Expression.ListExpr(Expressions()), annotation));

        declarations.Add(
            new Declaration.ChoiceTypeDeclaration(
                new(
                    "Choice",
                    [
                    "a"
                    ],
                    [
                    ValueConstructor.Create("Empty", []),
                    ValueConstructor.Create("Full", Annotations())
                    ])));

        declarations.Add(new Declaration.AliasDeclaration(new("Alias", ["a"], new TypeAnnotation.GenericType("a"))));
        declarations.Add(new Declaration.PortDeclaration(new("send", new TypeAnnotation.Typed(["Basics"], "Int", []))));
        declarations.Add(new Declaration.InfixDeclaration(new(InfixDirection.Right, 5, "++", "append")));
        declarations.Add(Function("unsigned", Expression.UnitExpr.Instance));
        declarations.Add(new Declaration.ChoiceTypeDeclaration(new("Empty", [], [])));
        return declarations;
    }

    [Fact]
    public void Roundtrips_all_expression_pattern_annotation_and_declaration_variants()
    {
        foreach (var expression in Expressions())
        {
            var bytes = Encode(expression);
            var decoded = ElmSyntaxAbstractBinary.DecodeExpression(bytes, out var consumed);
            consumed.Should().Be(bytes.Length);
            decoded.Should().Be(expression);
            Encode(decoded).Should().Equal(bytes);
        }

        var declarations = Declarations();
        var encoded = Encode(declarations);
        var restored = ElmSyntaxAbstractBinary.DecodeDeclarations(encoded, out var length);
        length.Should().Be(encoded.Length);
        restored.Should().Equal(declarations);
        Encode(restored).Should().Equal(encoded);
    }

    [Fact]
    public void Specialized_decoding_equals_interpreted_Pine_decoding_as_Elm_values_for_all_concrete_variants()
    {
        var cache = new PineVMParseCache();
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

        ValueResult<string, PineValue> Evaluate(PineValue expression, PineValue environment) =>
            interpreter.EvaluateExpressionDefault(
                cache.ParseExpression(expression).Extract(error => throw new Exception(error)),
                environment);

        ElmValue Reference(byte[] bytes) =>
            ElmValueEncoding.PineValueAsElmValue(
                ValueEncodingBinaryDeterministic.DecodeRoot(
                    bytes,
                    resolveExternalReference: ElmSyntaxAbstractBinary.ResolveConstructorExpression,
                    evaluate: Evaluate).Extract(error => throw new Exception(error.ToString())),
                null,
                null).Extract(error => throw new Exception(error));

        foreach (var expression in Expressions())
        {
            var bytes = Encode(expression);
            var decoded = ElmSyntaxAbstractBinary.DecodeExpression(bytes, out _);
            var expected = SyntaxElmValueEncoding.Encode(expression);
            SyntaxElmValueEncoding.Encode(decoded).Should().Be(expected);
            Reference(bytes).Should().Be(expected, expression.GetType().Name);
        }

        foreach (var annotation in Annotations())
        {
            using var stream = new MemoryStream();

            using (var encoder = new ElmSyntaxAbstractBinary.Encoder(stream.Write))
                encoder.WriteTypeAnnotation(annotation);

            var bytes = stream.ToArray();
            using var decoder = new ElmSyntaxAbstractBinary.Decoder(bytes);
            var decoded = decoder.ReadTypeAnnotation();
            SyntaxElmValueEncoding.Encode(decoded).Should().Be(Reference(bytes));
            SyntaxElmValueEncoding.Encode(decoded).Should().Be(SyntaxElmValueEncoding.Encode(annotation));
        }

        var declarations = Declarations();
        var encoded = Encode(declarations);
        var restored = ElmSyntaxAbstractBinary.DecodeDeclarations(encoded, out _);
        SyntaxElmValueEncoding.Encode(restored).Should().Be(Reference(encoded));
        SyntaxElmValueEncoding.Encode(restored).Should().Be(SyntaxElmValueEncoding.Encode(declarations));
    }

    [Fact]
    public void Rejects_unknown_constructor_hashes_wrong_types_arities_and_references_to_skipped_entries()
    {
        var unit = Encode(Expression.UnitExpr.Instance);
        var unknown = (byte[])unit.Clone();
        unknown.AsSpan(8, 32).Clear();
        var wrongArity = (byte[])unit.Clone();
        BinaryPrimitives.WriteInt32BigEndian(wrongArity.AsSpan(44), 1);
        var invalidString = Encode(new Expression.GLSLExpression("a"));
        invalidString.AsSpan(invalidString.Length - 4).Fill(0xff);
        var wrongType = Encode(new Expression.Application(Expression.UnitExpr.Instance, []));
        Encode(new Expression.GLSLExpression("")).AsSpan(8, 32).CopyTo(wrongType.AsSpan(8, 32));

        foreach (var bytes in new[] { unknown, wrongArity, invalidString, wrongType })
        {
            Action decode = () => ElmSyntaxAbstractBinary.DecodeExpression(bytes, out _);
            decode.Should().Throw<InvalidDataException>();
        }

        foreach (var target in new[] { 4, 40 })
        {
            var bytes = new byte[unit.Length + 8];
            unit.CopyTo(bytes, 0);

            BinaryPrimitives.WriteInt32BigEndian(
                bytes.AsSpan(unit.Length),
                ValueEncodingBinaryDeterministic.TagReferenceInteral);

            BinaryPrimitives.WriteInt32BigEndian(bytes.AsSpan(unit.Length + 4), target - (unit.Length + 4));

            Action decode =
                () =>
                {
                    using var decoder = new ElmSyntaxAbstractBinary.Decoder(bytes);
                    decoder.ReadExpression();
                    decoder.ReadExpression();
                };

            decode.Should().Throw<InvalidDataException>(
                "constructor expressions and inline environments are not retained nodes");
        }
    }

    [Fact]
    public void Preserves_noncanonical_precomputed_payloads_and_reuses_decoded_strings()
    {
        var payload = PineValue.List(PineValue.Blob([7, 8, 9, 10]));

        var original =
            new Expression.LambdaExpression(
                [
                    new Pattern.CharPattern(65, payload),
                    new Pattern.StringPattern("text", payload),
                    new Pattern.IntPattern(42, payload),
                    new Pattern.NamedPattern(new(["Acme", "Tree"], "Branch"), [], payload)
                ],
                new Expression.TupledExpression(
                    [
                        new Expression.CharLiteral(65, payload),
                        new Expression.IntegerLiteral(42, payload),
                        new Expression.RecordAccessFunction("field", payload),
                        new Expression.RecordAccess(Expression.UnitExpr.Instance, "field", payload),
                        new Expression.RecordExpr([new("field", payload, Expression.UnitExpr.Instance)])
                    ]));

        var decoded = (Expression.LambdaExpression)ElmSyntaxAbstractBinary.DecodeExpression(Encode(original), out _);
        decoded.Should().Be(original);

        ((Pattern.CharPattern)decoded.Arguments[0]).ValueAsPineValue.Should().BeSameAs(
            ((Pattern.NamedPattern)decoded.Arguments[3]).TagNameAsPineValue);

        var body = (Expression.TupledExpression)decoded.Expression;

        ((Expression.RecordAccessFunction)body.Elements[2]).FieldName.Should().BeSameAs(
            ((Expression.RecordAccess)body.Elements[3]).FieldName);

        ((Expression.RecordExpr)body.Elements[4]).Fields[0].FieldNameValue.Should().Be(payload);
    }

    [Fact]
    public void Reuses_structurally_equal_expressions_and_annotations_across_declarations()
    {
        static Declaration.FunctionDeclaration Make(string name) =>
            Function(
                name,
                new Expression.Application(Expression.Identifier.Create([], "identity"), [Integer(42)]),
                new TypeAnnotation.Typed(["Basics"], "Int", []));

        var original = new[] { Make("first"), Make("second") };
        var bytes = Encode(original);
        var decoded = ElmSyntaxAbstractBinary.DecodeDeclarations(bytes, out _);
        var first = decoded[0].Should().BeOfType<Declaration.FunctionDeclaration>().Subject.Function;
        var second = decoded[1].Should().BeOfType<Declaration.FunctionDeclaration>().Subject.Function;
        second.Declaration.Expression.Should().BeSameAs(first.Declaration.Expression);
        second.Signature!.TypeAnnotation.Should().BeSameAs(first.Signature!.TypeAnnotation);

        var shared =
            new[]
            {
                original[0],
                Function(
                    "second",
                    original[0].Function.Declaration.Expression,
                    original[0].Function.Signature!.TypeAnnotation)
            };

        Encode(shared).Should().Equal(bytes);
    }

    [Fact]
    public void Supports_embedded_sequences_mixed_roots_and_independent_reference_scopes()
    {
        using var stream = new MemoryStream();
        stream.WriteByte(0xaa);
        var expression = new Expression.ListExpr([Integer(1), Integer(2)]);

        using (var encoder = new ElmSyntaxAbstractBinary.Encoder(stream.Write))
        {
            encoder.WriteExpression(expression);
            encoder.WriteDeclaration(Function("root", expression));
            encoder.WriteTypeAnnotation(new TypeAnnotation.Unit());
            encoder.WriteTypeAnnotation(new TypeAnnotation.Unit());
        }

        var extent = (int)stream.Length - 1;
        ElmSyntaxAbstractBinary.Encode(stream, Integer(99));
        stream.WriteByte(0xbb);

        var bytes = stream.ToArray();
        using var decoder = new ElmSyntaxAbstractBinary.Decoder(bytes.AsMemory(1));
        var first = decoder.ReadExpression();
        var declaration = (Declaration.FunctionDeclaration)decoder.ReadDeclaration();
        declaration.Function.Declaration.Expression.Should().BeSameAs(first);
        var annotation = decoder.ReadTypeAnnotation();
        decoder.ReadTypeAnnotation().Should().BeSameAs(annotation);
        decoder.BytesRead.Should().Be(extent);

        ElmSyntaxAbstractBinary.DecodeExpression(bytes.AsMemory(1 + extent), out var secondExtent).Should().Be(
            Integer(99));

        bytes[1 + extent + secondExtent].Should().Be(0xbb);

        using var single = new MemoryStream();
        ElmSyntaxAbstractBinary.Encode(single, declaration);
        ElmSyntaxAbstractBinary.DecodeDeclaration(single.ToArray(), out var consumed).Should().Be(declaration);
        consumed.Should().Be((int)single.Length);
        ElmSyntaxAbstractBinary.ComputeHash(declaration).ToArray().Should().Equal(SHA256.HashData(single.ToArray()));
    }

    [Fact]
    public void Binary_layout_uses_hash_addressed_Eval_and_shallow_environments_without_a_version_header()
    {
        var bytes = Encode(new Expression.TupledExpression([Expression.UnitExpr.Instance, new Expression.UnitExpr()]));
        BinaryPrimitives.ReadInt32BigEndian(bytes).Should().Be(ValueEncodingBinaryDeterministic.TagEval);

        BinaryPrimitives.ReadInt32BigEndian(bytes.AsSpan(4)).Should().Be(
            ValueEncodingBinaryDeterministic.TagReferenceExternal256);

        ElmSyntaxAbstractBinary.ResolveConstructorExpression(bytes.AsMemory(8, 32)).Should().NotBeNull();
        BinaryPrimitives.ReadInt32BigEndian(bytes.AsSpan(40)).Should().Be(ValueEncodingBinaryDeterministic.TagList);
        BinaryPrimitives.ReadInt32BigEndian(bytes.AsSpan(44)).Should().Be(2);

        BinaryPrimitives.ReadInt32BigEndian(bytes.AsSpan(96)).Should().Be(
            ValueEncodingBinaryDeterministic.TagReferenceInteral);

        BinaryPrimitives.ReadInt32BigEndian(bytes.AsSpan(100)).Should().Be(-52);
        bytes.Length.Should().Be(104);

        var application = Encode(new Expression.Application(Expression.UnitExpr.Instance, [Integer(1), Integer(2)]));

        BinaryPrimitives.ReadInt32BigEndian(application.AsSpan(44)).Should().Be(
            3,
            "function and arguments share one shallow environment");
    }

    [Fact]
    public void Rejects_truncated_invalid_and_forward_reference_data()
    {
        var bytes = Encode(new Expression.LambdaExpression(Patterns(), new Expression.ListExpr(Expressions())));

        for (var length = 0; length < bytes.Length; length++)
        {
            var truncated = bytes.AsMemory(0, length);
            Action decode = () => ElmSyntaxAbstractBinary.DecodeExpression(truncated, out _);
            decode.Should().Throw<InvalidDataException>();
        }

        byte[][] malformed =
            [
                [0x45, 0x41, 0x53, 1, 1],
                [0, 0, 0, 255],
                [0, 0, 0, 4, 0xff, 0xff, 0xff, 0xfc],
                [0, 0, 0, 3, 0x7f, 0xff, 0xff, 0xff],
                [0, 0, 0, 6, 0, 0, 0, 3, 0, 0, 0, 0]
            ];

        foreach (var malformedBytes in malformed)
        {
            Action decode = () => ElmSyntaxAbstractBinary.DecodeExpression(malformedBytes, out _);
            decode.Should().Throw<InvalidDataException>();
        }

        var nestedBytes = EncodeNestedNegations(520);
        Action nested = () => ElmSyntaxAbstractBinary.DecodeExpression(nestedBytes, out _);
        nested.Should().Throw<InvalidDataException>();

        Action invalidDirection =
            () => Encode(
                new Expression.OperatorApplication("+", (InfixDirection)256, Integer(1), Integer(2)));

        invalidDirection.Should().Throw<ArgumentOutOfRangeException>();
    }

    [Theory]
    [InlineData(256 * 1024)]
    [InlineData(1024 * 1024)]
    public void Rejects_excessive_nesting_without_overflowing_small_thread_stacks(int stackSize)
    {
        var shallow = EncodeNestedNegations(8);
        var excessive = EncodeNestedNegations(520);
        ExceptionDispatchInfo? failure = null;

        var thread =
            new Thread(
                () =>
                {
                    try
                    {
                        var decoded = ElmSyntaxAbstractBinary.DecodeExpression(shallow, out var consumed);
                        consumed.Should().Be(shallow.Length);

                        for (var index = 0; index < 8; ++index)
                            decoded = decoded.Should().BeOfType<Expression.Negation>().Subject.Expression;

                        decoded.Should().Be(Expression.UnitExpr.Instance);
                        Action decode = () => ElmSyntaxAbstractBinary.DecodeExpression(excessive, out _);
                        decode.Should().Throw<InvalidDataException>();
                    }
                    catch (Exception error)
                    {
                        failure = ExceptionDispatchInfo.Capture(error);
                    }
                },
                maxStackSize: stackSize);

        thread.Start();
        thread.Join();
        failure?.Throw();
    }

    [Fact]
    public void Specialized_decoding_stays_within_specialized_codec_allocation_budgets()
    {
        var unit = Encode(Expression.UnitExpr.Instance);
        ElmSyntaxAbstractBinary.DecodeExpression(unit, out _);
        var before = GC.GetAllocatedBytesForCurrentThread();
        ElmSyntaxAbstractBinary.DecodeExpression(unit, out _);

        (GC.GetAllocatedBytesForCurrentThread() - before).Should().Be(
            0,
            "a constructor hash needs no expression, frame, environment or result wrapper");

        var application =
            Encode(
                new Expression.Application(
                    Expression.Identifier.Create(["Acme", "Tree"], "f"),
                    [
                    Integer(1),
                    Integer(2),
                    Integer(3)
                    ]));

        ElmSyntaxAbstractBinary.DecodeExpression(application, out _);
        before = GC.GetAllocatedBytesForCurrentThread();
        ElmSyntaxAbstractBinary.DecodeExpression(application, out _);

        (GC.GetAllocatedBytesForCurrentThread() - before).Should().BeLessThanOrEqualTo(
            800,
            "the replaced specialized codec allocated 800 bytes");

        var values = new Expression[1024];

        for (var index = 0; index < values.Length; ++index)
            values[index] = Integer(100000 + index);

        var wide = Encode(new Expression.ListExpr(values));
        ElmSyntaxAbstractBinary.DecodeExpression(wide, out _);
        before = GC.GetAllocatedBytesForCurrentThread();
        ElmSyntaxAbstractBinary.DecodeExpression(wide, out _);

        (GC.GetAllocatedBytesForCurrentThread() - before).Should().BeLessThanOrEqualTo(
            139776,
            "only retained nodes and owned arrays should be allocated");
    }

    [Fact]
    public void Streaming_encoding_does_not_allocate_payload_sized_arrays()
    {
        var text = new string('x', 4094) + "\U0001f600" + new string('\u00e9', 4096);
        var unicode = new Expression.GLSLExpression(text);
        ElmSyntaxAbstractBinary.DecodeExpression(Encode(unicode), out _).Should().Be(unicode);

        var expression =
            new Expression.TupledExpression(
                [new Expression.GLSLExpression(new string('x', 1_000_000)), Integer(BigInteger.One << 80_000)]);

        long length = 0;
        Action<ReadOnlySpan<byte>> write = bytes => length += bytes.Length;
        ElmSyntaxAbstractBinary.Encode(write, expression);
        length = 0;
        var before = GC.GetAllocatedBytesForCurrentThread();
        ElmSyntaxAbstractBinary.Encode(write, expression);
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        length.Should().BeGreaterThan(1_000_000);
        allocated.Should().BeLessThan(8192, "large UTF-8 strings and integers use pooled span buffers");
    }
}
