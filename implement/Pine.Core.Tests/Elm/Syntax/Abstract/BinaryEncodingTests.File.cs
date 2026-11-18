using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using Pine.Core.Runtime;
using System;
using System.Buffers.Binary;
using System.IO;
using Xunit;

using File = Pine.Core.Elm.Syntax.Abstract.File;

namespace Pine.Core.Tests.Elm.Syntax.Abstract;

public partial class BinaryEncodingTests
{
    [Fact]
    public void Full_files_preserve_all_module_import_and_exposing_variants_and_match_ordinary_Pine()
    {
        var exposing =
            new Exposing.Explicit(
                [
                new TopLevelExpose.InfixExpose("+"),
                new TopLevelExpose.FunctionExpose("value"),
                new TopLevelExpose.TypeOrAliasExpose("Alias"),
                new TopLevelExpose.TypeExpose(new("Open", true)),
                new TopLevelExpose.TypeExpose(new("Closed", false))
                ]);

        Module[] modules =
            [
            new Module.NormalModule(new(["Acme", "Module"], exposing)),
            new Module.PortModule(new([], new Exposing.Explicit([]))),
            new Module.EffectModule(new(["Effect"], new Exposing.All(), null, null)),
            new Module.EffectModule(new(["Effect"], exposing, "", "Subscription")),
            new Module.EffectModule(new(["Effect"], exposing, "Command", null)),
            new Module.EffectModule(new(["Effect"], exposing, null, "Subscription"))
            ];

        Import[] imports =
            [
            new(["Dict"], null, null),
            new(["List"], ["L"], exposing),
            new(["Empty"], [], new Exposing.Explicit([])),
            new([], null, new Exposing.All())
            ];

        var cache = new PineVMParseCache();
        var interpreter = DirectInterpreter.WithoutEvalCaching(cache);

        foreach (var module in modules)
        {
            var file = new File(module, imports, Declarations());
            using var stream = new MemoryStream();
            ElmSyntaxAbstractBinary.Encode(stream, file);
            var bytes = stream.ToArray();
            var decoded = ElmSyntaxAbstractBinary.DecodeFile(bytes, out var consumed);
            consumed.Should().Be(bytes.Length);
            decoded.Should().Be(file);
            ElmSyntaxAbstractTestHelper.Encode(decoded).Should().Equal(bytes);

            var ordinary =
                PineTreeBinaryEncoding.DecodeRoot(
                    bytes,
                    resolveExternalReference: ElmSyntaxAbstractBinary.ResolveConstructorExpression,
                    evaluate:
                    (expression, environment) =>
                    interpreter.EvaluateExpressionDefault(
                        cache.ParseExpression(expression).Extract(error => throw new Exception(error)),
                        environment))
                .Extract(error => throw new Exception(error.ToString()));

            ElmValueEncoding.PineValueAsElmValue(ordinary, null, null)
                .Extract(error => throw new Exception(error)).Should().Be(SyntaxElmValueEncoding.Encode(file));

            stream.WriteByte(0xab);
            using var encoder = new ElmSyntaxAbstractBinary.Encoder(stream.Write);
            encoder.WriteFile(new File(module, [], []));
            using var decoder = new ElmSyntaxAbstractBinary.Decoder(stream.ToArray());
            decoder.ReadFile().Should().Be(file);
            decoder.BytesRead.Should().Be(bytes.Length);

            ElmSyntaxAbstractBinary.DecodeFile(
                stream.ToArray().AsMemory(bytes.Length + 1),
                out _).Should().Be(new File(module, [], []));
        }
    }

    [Fact]
    public void File_decoder_rejects_truncation_wrong_optional_types_and_invalid_collection_counts()
    {
        var file =
            new File(
                new Module.EffectModule(new(["Effect"], new Exposing.All(), "Cmd", "Sub")),
                [new(["List"], ["L"], new Exposing.Explicit([]))],
                []);

        var bytes = ElmSyntaxAbstractTestHelper.Encode(file);

        for (var length = 0; length < bytes.Length; ++length)
        {
            var truncated = bytes.AsMemory(0, length);
            Action decode = () => ElmSyntaxAbstractBinary.DecodeFile(truncated, out _);
            decode.Should().Throw<InvalidDataException>();
        }

        var invalidCount = (byte[])bytes.Clone();
        BinaryPrimitives.WriteInt32BigEndian(invalidCount.AsSpan(44), 1);
        Action count = () => ElmSyntaxAbstractBinary.DecodeFile(invalidCount, out _);
        count.Should().Throw<InvalidDataException>();

        var exposed = ElmSyntaxAbstractTestHelper.Encode(new TopLevelExpose.TypeExpose(new("", true)));
        exposed[^3] = 3;

        Action boolean =
            () =>
            {
                using var decoder = new ElmSyntaxAbstractBinary.Decoder(exposed);
                decoder.ReadTopLevelExpose();
            };

        boolean.Should().Throw<InvalidDataException>();

        using var malformed = new MemoryStream();
        var import = ElmSyntaxAbstractTestHelper.Encode(new Import([], ["L"], null));

        using (var writer = new PineTreeBinaryEncoding.TreeEncoder<object>(malformed.Write))
        {
            writer.StartExternalEval(import.AsSpan(8, 32), 2);
            malformed.Write(ElmSyntaxAbstractTestHelper.Encode(new Exposing.All()));
            malformed.Write(import.AsSpan(import.Length - 48));
        }

        var invalidOptional = malformed.ToArray();

        Action optional =
            () =>
            {
                using var decoder = new ElmSyntaxAbstractBinary.Decoder(invalidOptional);
                decoder.ReadImport();
            };

        optional.Should().Throw<InvalidDataException>();

        foreach (var nestedNone in new[] { false, true })
        {
            using var invalid = new MemoryStream();

            using (var writer = new PineTreeBinaryEncoding.TreeEncoder<object>(invalid.Write))
            {
                writer.StartExternalEval(import.AsSpan(8, 32), 2);
                writer.StartExternalEval(import.AsSpan(56, 32), 1);

                invalid.Write(
                    nestedNone
                    ?
                    import.AsSpan(import.Length - 48)
                    :
                    ElmSyntaxAbstractTestHelper.Encode(new Expression.GLSLExpression("wrong")).AsSpan(48));

                invalid.Write(import.AsSpan(import.Length - 48));
            }

            var invalidBytes = invalid.ToArray();

            Action payload =
                () =>
                {
                    using var decoder = new ElmSyntaxAbstractBinary.Decoder(invalidBytes);
                    decoder.ReadImport();
                };

            payload.Should().Throw<InvalidDataException>().WithMessage("*optional*");
        }
    }
}
