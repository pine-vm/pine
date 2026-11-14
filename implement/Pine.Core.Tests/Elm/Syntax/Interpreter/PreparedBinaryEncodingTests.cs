using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.Runtime;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Runtime.ExceptionServices;
using System.Security.Cryptography;
using System.Threading;
using Xunit;

using Ast = Pine.Core.Elm.Syntax.Abstract.ElmSyntaxAbstractBinary;
using AstExpression = Pine.Core.Elm.Syntax.Abstract.Expression;
using Codec = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreterPreparedBinary;
using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;
using Expr = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter.PreparedExpression;
using InfixDirection = Pine.Core.Elm.Syntax.Concrete.InfixDirection;
using PineValue = Pine.Core.PineLanguage.PineValue;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public partial class PreparedBinaryEncodingTests
{
    private static Expr.ValueLiteral Literal(int value) => new(PineValueInProcess.CreateInteger(value));

    private static Engine.PreparedDeclaration.FunctionDeclaration Function(
        string name, Expr body, Signature? signature = null, params Pattern[] arguments) =>
        new(new(signature, new(name, arguments, body)));

    private static byte[] Encode(Expr expression)
    {
        using var stream = new MemoryStream();
        Codec.Encode(stream, expression);
        return stream.ToArray();
    }

    private static byte[] Encode(Engine.PreparedDeclaration declaration)
    {
        using var stream = new MemoryStream();
        Codec.Encode(stream, declaration);
        return stream.ToArray();
    }

    private static byte[] Encode(Engine.Prepared prepared)
    {
        using var stream = new MemoryStream();
        Codec.Encode(stream, prepared);
        return stream.ToArray();
    }

    private static Pattern[] Patterns() =>
        [
        new Pattern.AllPattern(),
        new Pattern.VarPattern("value"),
        new Pattern.UnitPattern(),
        new Pattern.CharPattern(0x1f600),
        new Pattern.StringPattern("ascii\0\u007f"),
        new Pattern.IntPattern(-(BigInteger.One << 130)),
        new Pattern.FloatPattern(7, -3),
        new Pattern.TuplePattern([new Pattern.VarPattern("a"), new Pattern.AllPattern()]),
        Pattern.RecordPattern.Create(["a", "b"]),
        new Pattern.UnConsPattern(new Pattern.VarPattern("head"), new Pattern.VarPattern("tail")),
        new Pattern.ListPattern([]),
        new Pattern.NamedPattern(new(["Acme", "Tree"], "Branch"), [new Pattern.VarPattern("x")]),
        new Pattern.AsPattern(new Pattern.AllPattern(), "all")
        ];

    private static Expr[] Expressions()
    {
        var value = Literal(42);
        var fieldValue = StringEncoding.ValueFromString("field");

        return
            [
            new Expr.ValueLiteral(PineValueInProcess.EmptyList),
            new Expr.ValueLiteral(PineValueInProcess.EmptyBlob),
            new Expr.ValueLiteral(PineValueInProcess.CreateInteger(-(BigInteger.One << 130))),
            new Expr.ValueLiteral(
                PineValueInProcess.CreateFullyRepresented(
                    PineValue.List(PineValue.Blob([7, 8, 9, 10, 11]), PineValue.List(PineValue.Blob([4, 0, 42]))))),
            value,
            new Expr.Negation(value),
            new Expr.ListExpr([value, Literal(7)]),
            new Expr.ListExpr([]),
            Expr.Identifier.Create(["Acme", "Module"], "value"),
            Expr.Identifier.Create([], ""),
            new Expr.IfBlock(value, Literal(1), Literal(2)),
            new Expr.PrefixOperator("+"),
            new Expr.Application(Expr.Identifier.Create([], "f"), [value, Literal(7)]),
            new Expr.Application(value, []),
            new Expr.OperatorApplication("+", InfixDirection.Left, value, Literal(7)),
            new Expr.OperatorApplication("|>", InfixDirection.Right, value, Literal(7)),
            new Expr.OperatorApplication("==", InfixDirection.Non, value, Literal(7)),
            new Expr.TupledExpression([value, Literal(7)]),
            new Expr.TupledExpression([]),
            new Expr.LambdaExpression(Patterns(), value),
            new Expr.LambdaExpression([], value),
            new Expr.CaseExpression(
                value,
                new(
                    [
                    new Engine.PreparedCaseDispatchSegment.ConstantValuePatterns(
                        [
                        new(new Pattern.IntPattern(0), IntegerEncoding.EncodeSignedInteger(0), Literal(1)),
                        new(new Pattern.IntPattern(1), IntegerEncoding.EncodeSignedInteger(1), Literal(2))
                        ]),
                    new Engine.PreparedCaseDispatchSegment.PatternCase(new Pattern.VarPattern("x"), value),
                    new Engine.PreparedCaseDispatchSegment.DiscardCase(Literal(3)),
                    new Engine.PreparedCaseDispatchSegment.ConstantValuePatterns([])
                    ])),
            new Expr.CaseExpression(value, new([])),
            new Expr.LetExpression(
                [
                new Engine.PreparedLetDeclaration.LetFunction(Function("local", value).Function),
                new Engine.PreparedLetDeclaration.LetDestructuring(new Pattern.VarPattern("x"), Literal(7))
                ],
                value),
            new Expr.LetExpression([], value),
            new Expr.RecordExpr([new("field", fieldValue, value)]),
            new Expr.RecordExpr([]),
            new Expr.RecordAccess(value, "field", fieldValue),
            new Expr.RecordAccessFunction("field", fieldValue),
            new Expr.RecordUpdateExpression("record", [new("field", fieldValue, value)]),
            new Expr.RecordUpdateExpression("", []),
            new Expr.GLSLExpression("ascii\0\u007f"),
            new Expr.GLSLExpression("unicode\u00e9\U0001f600"),
            new Expr.GLSLExpression("")
            ];
    }

    private static Engine.PreparedDeclaration[] Declarations() =>
        [
        Function(
            "withSignature",
            new Expr.ListExpr(Expressions()),
            new(
                "withSignature",
                new TypeAnnotation.FunctionTypeAnnotation(
                    new TypeAnnotation.GenericType("a"),
                    new TypeAnnotation.Typed(["Basics"], "Int", []))),
            Patterns()),
        Function("withoutSignature", Literal(42)),
        new Engine.PreparedDeclaration.ChoiceTypeDeclaration(
            new(
                "Choice",
                [
                "a"
                ],
                [
                ValueConstructor.Create("Empty", []),
                ValueConstructor.Create("Full", [new TypeAnnotation.GenericType("a")])
                ])),
        new Engine.PreparedDeclaration.AliasDeclaration(
            new("Alias", [], new TypeAnnotation.Record(new([RecordField.Create("field", new TypeAnnotation.Unit())])))),
        new Engine.PreparedDeclaration.PortDeclaration(new("send", new TypeAnnotation.Typed([], "Int", []))),
        new Engine.PreparedDeclaration.InfixDeclaration(new(InfixDirection.Right, 5, "++", "append"))
        ];

    [Fact]
    public void Prepared_binary_roundtrips_all_variants_and_matches_ordinary_Pine_evaluation()
    {
        var cache = new PineVMParseCache();
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

        PineValue Reference(byte[] bytes) =>
            ValueEncodingBinaryDeterministic.DecodeRoot(
                bytes,
                resolveExternalReference: Codec.ResolveConstructorExpression,
                evaluate:
                (expression, environment) =>
                interpreter.EvaluateExpressionDefault(
                    cache.ParseExpression(expression).Extract(error => throw new Exception(error)),
                    environment))
            .Extract(error => throw new Exception(error.ToString()));

        foreach (var expression in Expressions())
        {
            var bytes = Encode(expression);
            var decoded = Codec.DecodeExpression(bytes, out var consumed);
            consumed.Should().Be(bytes.Length);
            decoded.GetType().Should().Be(expression.GetType());
            Encode(decoded).Should().Equal(bytes);
            Project(decoded).Should().Be(Project(expression));
            Reference(bytes).Should().Be(Project(expression), expression.GetType().Name);
        }

        foreach (var declaration in Declarations())
        {
            var bytes = Encode(declaration);
            var decoded = Codec.DecodeDeclaration(bytes, out var consumed);
            consumed.Should().Be(bytes.Length);
            decoded.GetType().Should().Be(declaration.GetType());
            Encode(decoded).Should().Equal(bytes);
            Project(decoded).Should().Be(Project(declaration));
            Reference(bytes).Should().Be(Project(declaration));
            Codec.ComputeHash(declaration).ToArray().Should().Equal(SHA256.HashData(bytes));
        }

        var program =
            new Engine.Prepared(
                Declarations().Select(
                    (declaration, index) =>
                    KeyValuePair.Create(DeclQualifiedName.Create(["Acme", "Module"], "decl" + index), declaration)).ToDictionary());

        var encoded = Encode(program);
        var app = Codec.DecodePrepared(encoded, out var extent);
        extent.Should().Be(encoded.Length);
        Project(app).Should().Be(Project(program));
        Reference(encoded).Should().Be(Project(program));
        Encode(app).Should().Equal(encoded);

        var empty = new Engine.Prepared(new Dictionary<DeclQualifiedName, Engine.PreparedDeclaration>());
        Codec.DecodePrepared(Encode(empty), out _).Declarations.Should().BeEmpty();

        var source =
            PreparedSourceDeclaration.Prepare(
                new Declaration.PortDeclaration(new("send", new TypeAnnotation.Unit())));

        Reference(Codec.SourceDeclarationCodec.Encode(source).ToArray()).Should().Be(Project(source));
    }

    [Fact]
    public void Prepared_binary_is_independent_of_map_order_list_identity_and_prior_reference_scopes()
    {
        static Expr Make() =>
            new Expr.LetExpression(
                [
                    new Engine.PreparedLetDeclaration.LetFunction(
                        Function("local", new Expr.TupledExpression([Literal(1), Literal(2)])).Function)
                ],
                new Expr.CaseExpression(
                    new Expr.LambdaExpression([new Pattern.VarPattern("x")], Expr.Identifier.Create([], "x")),
                    new(
                        [
                        new Engine.PreparedCaseDispatchSegment.ConstantValuePatterns(
                            [new(new Pattern.IntPattern(1), IntegerEncoding.EncodeSignedInteger(1), Literal(3))]),
                        new Engine.PreparedCaseDispatchSegment.PatternCase(new Pattern.VarPattern("x"), Literal(4)),
                        new Engine.PreparedCaseDispatchSegment.DiscardCase(
                            new Expr.RecordExpr([new("field", StringEncoding.ValueFromString("field"), Literal(5))]))
                        ])));

        var shared = Make();
        var repeated = new Expr.ListExpr([shared, shared]);
        var distinct = new Expr.ListExpr([Make(), Make()]);
        Encode(distinct).Should().Equal(Encode(repeated));
        var restored = (Expr.ListExpr)Codec.DecodeExpression(Encode(distinct), out _);
        restored.Elements[0].Should().BeSameAs(restored.Elements[1]);

        var first = DeclQualifiedName.Create(["Module"], "first");
        var second = DeclQualifiedName.Create(["Module"], "second");

        var a =
            new Engine.Prepared(
                new Dictionary<DeclQualifiedName, Engine.PreparedDeclaration>
                {
                    [first] = Function("root", repeated),
                    [second] = Function("root", distinct)
                });

        var b =
            new Engine.Prepared(
                new Dictionary<DeclQualifiedName, Engine.PreparedDeclaration>
                {
                    [second] = Function("root", distinct),
                    [first] = Function("root", repeated)
                });

        Encode(a).Should().Equal(Encode(b));

        Codec.ComputeHash(Function("root", repeated)).ToArray().Should().Equal(
            Codec.ComputeHash(Function("root", distinct)).ToArray());
    }

    [Fact]
    public void Prepared_binary_preserves_exact_payloads_not_only_matching_or_reprepared_values()
    {
        var payload = PineValue.List(PineValue.Blob([7, 8, 9, 10, 11]));
        var pattern = new Pattern.CharPattern(65, payload);

        var original =
            new Expr.CaseExpression(
                new Expr.ValueLiteral(PineValueInProcess.CreateFullyRepresented(payload)),
                new(
                    [
                    new Engine.PreparedCaseDispatchSegment.ConstantValuePatterns(
                        [new(pattern, payload, new Expr.RecordAccessFunction("field", payload))])
                    ]));

        var decoded = (Expr.CaseExpression)Codec.DecodeExpression(Encode(original), out _);
        var constants = (Engine.PreparedCaseDispatchSegment.ConstantValuePatterns)decoded.Dispatch.Steps[0];
        var branch = constants.Cases[0];
        var value = ((Expr.ValueLiteral)decoded.Expression).Value.Evaluate();
        value.Should().Be(payload);
        branch.Value.Should().BeSameAs(value);
        ((Pattern.CharPattern)branch.Pattern).ValueAsPineValue.Should().BeSameAs(value);
        ((Expr.RecordAccessFunction)branch.Expression).FieldNameValue.Should().BeSameAs(value);

        var raw =
            new Expr.ListExpr(
                [
                new Expr.ValueLiteral(PineValueInProcess.CreateFullyRepresented(PineValue.Blob([4, 0, 42]))),
                new Expr.ValueLiteral(PineValueInProcess.CreateInteger(42))
                ]);

        var restored = (Expr.ListExpr)Codec.DecodeExpression(Encode(raw), out _);
        ((Expr.ValueLiteral)restored.Elements[0]).Value.Evaluate().Should().Be(PineValue.Blob([4, 0, 42]));
        ((Expr.ValueLiteral)restored.Elements[1]).Value.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Prepared_source_binary_preserves_paths_shared_AST_leaves_and_rebinds_fresh_observer_sites()
    {
        const string text =
            """
            module Test exposing (..)

            root : Int -> Int
            root x =
                case x of
                    0 -> 1
                    _ -> 1
            """;

        var syntax =
            ConvertFromConcrete.FromFile(
                ElmSyntaxParser.ParseModuleText(text).Extract(error => throw new Exception(error.ToString())))
            .Declarations.Single();

        var original = PreparedSourceDeclaration.Prepare(syntax);
        var encoded = Codec.SourceDeclarationCodec.Encode(original);
        var decoded = Codec.SourceDeclarationCodec.Decode(encoded);
        decoded.Syntax.Should().Be(syntax);
        Codec.SourceDeclarationCodec.Encode(decoded).ToArray().Should().Equal(encoded.ToArray());

        decoded.ExpressionPaths.Select(entry => entry.Path).Should().Equal(
            original.ExpressionPaths.Select(entry => entry.Path));

        var sourceSignature = ((Declaration.FunctionDeclaration)decoded.Syntax).Function.Signature!;
        var preparedSignature = ((Engine.PreparedDeclaration.FunctionDeclaration)decoded.Prepared).Function.Signature!;
        preparedSignature.TypeAnnotation.Should().BeSameAs(sourceSignature.TypeAnnotation);

        var rebound = decoded.Rebind(name => name);
        var expressions = rebound.ExpressionPaths.Select(entry => entry.Expression).ToArray();
        expressions.Distinct(ReferenceEqualityComparer.Instance).Count().Should().Be(expressions.Length);

        // A reduced prepared literal need not match a newly prepared source body.
        var reduced = new PreparedSourceDeclaration(syntax, Function("root", Literal(999)));
        var back = Codec.SourceDeclarationCodec.Decode(Codec.SourceDeclarationCodec.Encode(reduced));

        ((Expr.ValueLiteral)((Engine.PreparedDeclaration.FunctionDeclaration)back.Prepared).Function.Declaration.Expression)
            .Value.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(999));

        var trailer = new byte[encoded.Length + 1];
        encoded.CopyTo(trailer);
        Action decode = () => Codec.SourceDeclarationCodec.Decode(trailer);
        decode.Should().Throw<InvalidDataException>();
    }

    [Fact]
    public void Prepared_binary_supports_mixed_embedded_roots_with_shared_references()
    {
        var syntax = new Declaration.PortDeclaration(new("port", new TypeAnnotation.Typed([], "Int", [])));
        var prepared = new Engine.PreparedDeclaration.PortDeclaration(syntax.Signature);
        var expression = new Expr.GLSLExpression("ascii");
        using var stream = new MemoryStream();
        stream.WriteByte(0xaa);

        using (var encoder = new Codec.Encoder(stream.Write))
        {
            encoder.WriteSyntaxDeclaration(syntax);
            encoder.WriteDeclaration(prepared);
            encoder.Flush();
            encoder.WriteExpression(expression);
            encoder.WriteExpression(new Expr.GLSLExpression(new string("ascii".ToCharArray())));
        }

        var extent = (int)stream.Length - 1;
        Codec.Encode(stream, expression);
        stream.WriteByte(0xbb);
        var bytes = stream.ToArray();
        using var decoder = new Codec.Decoder(bytes.AsMemory(1));
        var abstractPort = (Declaration.PortDeclaration)decoder.ReadSyntaxDeclaration();
        var preparedPort = (Engine.PreparedDeclaration.PortDeclaration)decoder.ReadDeclaration();
        preparedPort.Signature.TypeAnnotation.Should().BeSameAs(abstractPort.Signature.TypeAnnotation);
        var first = decoder.ReadExpression();
        decoder.ReadExpression().Should().BeSameAs(first);
        decoder.BytesRead.Should().Be(extent);
        Codec.DecodeExpression(bytes.AsMemory(1 + extent), out var secondExtent).Should().Be(expression);
        bytes[1 + extent + secondExtent].Should().Be(0xbb);
    }

    [Fact]
    public void Prepared_binary_restores_execution_of_literals_local_functions_and_case_dispatch()
    {
        const string text =
            """
            module Test exposing (..)

            classify x =
                let
                    local y = y + 1
                in
                case x of
                    0 -> local 10
                    1 -> local 20
                    value -> local value

            folded = ( 1, "two" )
            """;

        var original = Engine.PrepareModules([text]).Extract(error => throw new Exception(error));
        var back = Codec.DecodePrepared(Encode(original), out _);

        foreach (var expression in new[] { "Test.classify 0", "Test.classify 1", "Test.classify 9", "Test.folded" })
        {
            var expected =
                Engine.InterpretAsElmValue(expression, original).Extract(error => throw new Exception(error.ToString()));

            Engine.InterpretAsElmValue(expression, back)
                .Extract(error => throw new Exception(error.ToString())).Should().Be(expected);
        }
    }

    [Fact]
    public void Prepared_binary_rejects_truncation_unknown_hashes_types_arities_and_invalid_references()
    {
        var bytes = Encode(Function("root", new Expr.ListExpr(Expressions())));

        for (var length = 0; length < bytes.Length; ++length)
        {
            var prefix = bytes.AsMemory(0, length);
            Action truncated = () => Codec.DecodeDeclaration(prefix, out _);
            truncated.Should().Throw<InvalidDataException>();
        }

        var literal = Encode(Literal(1));
        var unknown = (byte[])literal.Clone();
        unknown.AsSpan(8, 32).Clear();
        var arity = (byte[])literal.Clone();
        BinaryPrimitives.WriteInt32BigEndian(arity.AsSpan(44), 0);
        var forward = new byte[] { 0, 0, 0, 4, 0, 0, 0, 4 };

        foreach (var invalid in new[] { unknown, arity, forward, bytes, literal[..48], new byte[8] })
        {
            Action decode = () => Codec.DecodeExpression(invalid, out _);
            decode.Should().Throw<InvalidDataException>();
        }

        Action syntaxType = () => Codec.DecodeExpression(EncodeSyntax(new AstExpression.GLSLExpression("x")), out _);
        syntaxType.Should().Throw<InvalidDataException>();
    }

    private static byte[] EncodeSyntax(AstExpression expression)
    {
        using var stream = new MemoryStream();
        Ast.Encode(stream, expression);
        return stream.ToArray();
    }

    [Fact]
    public void Prepared_binary_rejects_skipped_incomplete_and_wrong_typed_reference_targets()
    {
        var bytes = Encode(new Expr.ListExpr([Literal(1), Literal(1)]));
        var second = bytes.Length - 8;
        BinaryPrimitives.ReadInt32BigEndian(bytes.AsSpan(second)).Should().Be(4);

        // The first expression begins at 48; its raw value begins at 96.
        foreach (var target in new[] { 0, 48 + 4, 48 + 40, 96 })
        {
            var invalid = (byte[])bytes.Clone();
            BinaryPrimitives.WriteInt32BigEndian(invalid.AsSpan(second + 4), target - (second + 4));
            Action decode = () => Codec.DecodeExpression(invalid, out _);
            decode.Should().Throw<InvalidDataException>();
        }

        var operation = Encode(new Expr.OperatorApplication("+", InfixDirection.Left, Literal(1), Literal(2)));
        using var stream = new MemoryStream();

        using (var writer = new ValueEncodingBinaryDeterministic.TreeEncoder<object>(stream.Write))
        {
            writer.StartExternalEval(operation.AsSpan(8, 32), 4);
            stream.Write(Encode(new Expr.GLSLExpression("+")).AsSpan(48));
            writer.Integer(3);
            stream.Write(Encode(Literal(1)));
            stream.Write(Encode(Literal(2)));
        }

        Action direction = () => Codec.DecodeExpression(stream.ToArray(), out _);
        direction.Should().Throw<InvalidDataException>().WithMessage("*infix direction*");
    }

    [Fact]
    public void Prepared_binary_rejects_duplicate_app_keys_and_invalid_app_counts()
    {
        var app = Encode(new Engine.Prepared(new Dictionary<DeclQualifiedName, Engine.PreparedDeclaration>()));
        var odd = (byte[])app.Clone();
        BinaryPrimitives.WriteInt32BigEndian(odd.AsSpan(44), 1);
        Action oddCount = () => Codec.DecodePrepared(odd, out _);
        oddCount.Should().Throw<InvalidDataException>();

        using var stream = new MemoryStream();

        using (var writer = new ValueEncodingBinaryDeterministic.TreeEncoder<object>(stream.Write))
        {
            writer.StartExternalEval(app.AsSpan(8, 32), 4);
            var syntax = new Declaration.PortDeclaration(new("same", new TypeAnnotation.Unit()));
            // Write a qualified name from an identifier's environment, twice, without cross-scope references.
            var name = EncodeSyntax(AstExpression.Identifier.Create(["Module"], "same")).AsSpan(48).ToArray();
            stream.Write(name);
            stream.Write(Encode(new Engine.PreparedDeclaration.PortDeclaration(syntax.Signature)));
            stream.Write(name);
            stream.Write(Encode(new Engine.PreparedDeclaration.PortDeclaration(syntax.Signature)));
        }

        Action duplicate = () => Codec.DecodePrepared(stream.ToArray(), out _);
        duplicate.Should().Throw<InvalidDataException>().WithMessage("*Duplicate*");
    }

    [Theory]
    [InlineData(256 * 1024)]
    [InlineData(1024 * 1024)]
    public void Prepared_binary_rejects_excessive_nesting_on_small_stacks(int stackSize)
    {
        var hash = Encode(new Expr.Negation(Literal(1))).AsSpan(8, 32).ToArray();
        using var stream = new MemoryStream();

        using (var writer = new ValueEncodingBinaryDeterministic.TreeEncoder<object>(stream.Write))
        {
            for (var index = 0; index < 520; ++index)
                writer.StartExternalEval(hash, 1);
        }

        stream.Write(Encode(Literal(1)));
        var excessive = stream.ToArray();
        var shallow = Encode(new Expr.Negation(Literal(1)));
        ExceptionDispatchInfo? failure = null;

        var thread =
            new Thread(
                () =>
                {
                    try
                    {
                        Project(Codec.DecodeExpression(shallow, out _)).Should().Be(
                            Project(new Expr.Negation(Literal(1))));

                        Action decode = () => Codec.DecodeExpression(excessive, out _);
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
    public void Prepared_binary_streams_large_ASCII_payloads_without_payload_sized_scratch_allocations()
    {
        var expression = new Expr.GLSLExpression(new string('x', 1_000_000));
        long length = 0;
        Action<ReadOnlySpan<byte>> write = bytes => length += bytes.Length;
        Codec.Encode(write, expression);
        length = 0;
        var before = GC.GetAllocatedBytesForCurrentThread();
        Codec.Encode(write, expression);
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        length.Should().Be(1_000_104);
        allocated.Should().BeLessThan(8192);
    }
}
