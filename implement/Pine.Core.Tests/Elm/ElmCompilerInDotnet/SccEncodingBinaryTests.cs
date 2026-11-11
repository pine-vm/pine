using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Security.Cryptography;
using Xunit;

using TypeDeclaration = Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits.TypeDeclaration;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class SccEncodingBinaryTests
{
    private static DeclQualifiedName Name(string module, string symbol) => DeclQualifiedName.Create([module], symbol);

    private static ValueDeclaration.FunctionDeclaration Function(
        string name,
        Expression body,
        TypeAnnotation? type = null) =>
        new(new(type is null ? null : new(name, type), new(name, [], body)));

    private static Expression Integer(int value) =>
        new Expression.IntegerLiteral(value, IntegerEncoding.EncodeSignedInteger(value));

    private static SccModel.ValueScc Value(string name, Expression body) =>
        new(new(ImmutableDictionary<string, ValueDeclaration>.Empty.Add(name, Function(name, body)), [], []));

    private static byte[] Encode(SccModel model)
    {
        using var stream = new MemoryStream();
        SccEncodingBinary.Encode(stream, model);
        return stream.ToArray();
    }

    private static SccModel.TypeScc Type() =>
        new(
            new(
                ImmutableDictionary<string, TypeDeclaration>.Empty.Add(
                    "Widget",
                    new TypeDeclaration.ChoiceTypeDeclaration(
                        new("Widget", [], [ValueConstructor.Create("MakeWidget", [])]))),
                []));

    [Fact]
    public void Roundtrips_external_member_and_dependency_references_without_copying_graphs()
    {
        var type = Type();
        var helper = Value("helper", Integer(42));

        var members =
            ImmutableDictionary<string, ValueDeclaration>.Empty
            .Add(
                "first",
                Function(
                    "first",
                    new Expression.TupledExpression(
                        [
                            Expression.Identifier.Create([], "second"),
                            Expression.Identifier.Create(["External"], "helper"),
                            Expression.Identifier.Create(["Types"], "MakeWidget")
                        ]),
                    new TypeAnnotation.Typed(["Types"], "Widget", [])))
            .Add("second", Function("second", Expression.Identifier.Create([], "first")));

        var model =
            new SccModel.ValueScc(
                new(
                    members,
                    ImmutableDictionary<DeclQualifiedName, SccModel>.Empty
                    .Add(Name("Types", "MakeWidget"), type)
                    .Add(Name("External", "helper"), helper)
                    .Add(Name("External.WithDot", "helper"), helper),
                    ImmutableDictionary<DeclQualifiedName, TypeSccModel>.Empty.Add(Name("Types", "Widget"), type.Value)));

        var declarations = new Dictionary<string, Declaration>();
        var dependencies = new Dictionary<string, SccModel>();
        using var stream = new MemoryStream();

        SccEncodingBinary.Encode(
            stream,
            model,
            declaration =>
            {
                var reference = ElmSyntaxAbstractBinary.ComputeHash(declaration);
                declarations[Convert.ToHexString(reference.Span)] = declaration;
                return reference;
            },
            dependency =>
            {
                var reference = dependency.FingerprintBytes;
                dependencies.TryAdd(Convert.ToHexString(reference.Span), dependency);
                return reference;
            });

        var bytes = stream.ToArray();
        bytes.Should().Equal(Encode(model));
        SHA256.HashData(bytes).Should().Equal(model.FingerprintBytes.ToArray());
        declarations.Count.Should().Be(2);
        dependencies.Count.Should().Be(2);

        var decoded =
            SccEncodingBinary.Decode(
                bytes,
                reference => declarations.GetValueOrDefault(Convert.ToHexString(reference.Span)),
                reference => dependencies.GetValueOrDefault(Convert.ToHexString(reference.Span)),
                out var consumed).Should().BeOfType<SccModel.ValueScc>().Subject;

        consumed.Should().Be(bytes.Length);
        decoded.Should().Be(model);
        decoded.Value.Dependencies[Name("External.WithDot", "helper")].Should().BeSameAs(helper);
        decoded.Value.Dependencies[Name("External", "helper")].Should().BeSameAs(helper);
        decoded.Value.Dependencies[Name("Types", "MakeWidget")].Should().BeSameAs(type);
        decoded.Value.TypeDependencies[Name("Types", "Widget")].Should().BeSameAs(type.Value);

        ((ValueDeclaration.FunctionDeclaration)decoded.Value.Members["first"]).Function
            .Should().BeSameAs(((ValueDeclaration.FunctionDeclaration)model.Value.Members["first"]).Function);

        Encode(decoded).Should().Equal(bytes);

        // The declarations can be persisted together, sharing syntax instances independently of SCC blobs.
        using var syntaxStream = new MemoryStream();
        var syntax = new List<Declaration>(declarations.Values);
        ElmSyntaxAbstractBinary.Encode(syntaxStream, syntax);
        var loadedSyntax = ElmSyntaxAbstractBinary.DecodeDeclarations(syntaxStream.ToArray(), out _);
        declarations.Clear();

        foreach (var declaration in loadedSyntax)
            declarations[Convert.ToHexString(ElmSyntaxAbstractBinary.ComputeHash(declaration).Span)] = declaration;

        SccEncodingBinary.Decode(
            bytes,
            reference => declarations.GetValueOrDefault(Convert.ToHexString(reference.Span)),
            reference => dependencies.GetValueOrDefault(Convert.ToHexString(reference.Span)),
            out _).Should().Be(model);
    }

    [Fact]
    public void Type_blobs_preserve_aliases_constructor_roles_intrinsics_and_embedding()
    {
        var dependency = Type();

        var model =
            new SccModel.TypeScc(
                new(
                    ImmutableDictionary<string, TypeDeclaration>.Empty.Add(
                        "Text",
                        new TypeDeclaration.AliasDeclaration(
                            new(
                                "Text",
                                [],
                                new TypeAnnotation.Typed(["Types"], "Widget", [])))),
                    ImmutableDictionary<DeclQualifiedName, TypeSccModel>.Empty.Add(
                        Name("Types", "Widget"),
                        dependency.Value),
                    ImmutableDictionary<string, string>.Empty.Add("Text", "String")));

        Declaration? stored = null;
        using var stream = new MemoryStream();
        stream.WriteByte(0xaa);

        SccEncodingBinary.Encode(
            stream,
            model,
            declaration =>
            {
                stored = declaration;
                return ElmSyntaxAbstractBinary.ComputeHash(declaration);
            });

        stream.WriteByte(0xbb);

        var decoded =
            SccEncodingBinary.Decode(
                stream.ToArray().AsMemory(1),
                _ => stored,
                _ => dependency,
                out var consumed).Should().BeOfType<SccModel.TypeScc>().Subject;

        consumed.Should().Be((int)stream.Length - 2);
        decoded.Should().Be(model);
        decoded.Value.IntrinsicRepresentations["Text"].Should().Be("String");
        decoded.Value.TypeDependencies[Name("Types", "Widget")].Should().BeSameAs(dependency.Value);
        SHA256.HashData(Encode(model)).Should().Equal(model.FingerprintBytes.ToArray());

        Action wrongDependency =
            () => SccEncodingBinary.Decode(
                stream.ToArray().AsMemory(1),
                _ => stored,
                _ => Value("Widget", Integer(0)),
                out _);

        wrongDependency.Should().Throw<InvalidDataException>();

        var ordinary = new SccModel.TypeScc(new(model.Value.Members, model.Value.TypeDependencies));
        ordinary.Fingerprint.Should().NotBe(model.Fingerprint);
    }

    [Fact]
    public void Blob_size_is_independent_of_member_bodies_and_dependency_graph_sizes()
    {
        var small = Value("root", new Expression.GLSLExpression("x"));
        var large = Value("root", new Expression.GLSLExpression(new string('x', 1_000_000)));
        var smallBytes = Encode(small);
        var largeBytes = Encode(large);
        smallBytes.Length.Should().Be(largeBytes.Length);
        largeBytes.Length.Should().BeLessThan(100);
        largeBytes.Should().NotEqual(smallBytes);
    }

    [Fact]
    public void Fingerprints_follow_content_and_symbols_not_dictionary_order_or_record_copy_caches()
    {
        var first = Value("helper", Integer(1));
        var second = Value("helper", Integer(2));
        var member = Function("root", Integer(0));
        var members = ImmutableDictionary<string, ValueDeclaration>.Empty.Add("root", member);
        var names = new[] { Name("Zulu", "helper"), Name("Alpha", "helper") };

        var dependencies =
            ImmutableDictionary<DeclQualifiedName, SccModel>.Empty.Add(names[0], first).Add(names[1], first);

        var original = new SccModel.ValueScc(new(members, dependencies, []));

        var reordered =
            new SccModel.ValueScc(
                new(
                    members,
                    ImmutableDictionary<DeclQualifiedName, SccModel>.Empty.Add(names[1], first).Add(names[0], first),
                    []));

        Encode(reordered).Should().Equal(Encode(original));
        reordered.Fingerprint.Should().Be(original.Fingerprint);

        new SccModel.ValueScc(new(members, dependencies.SetItem(names[0], second), []))
            .Fingerprint.Should().NotBe(original.Fingerprint);

        var copied = member with { Function = Function("root", Integer(99)).Function };
        var changed = new SccModel.ValueScc(new(members.SetItem("root", copied), dependencies, []));
        changed.Fingerprint.Should().NotBe(original.Fingerprint);

        var fresh =
            new SccModel.ValueScc(new(members.SetItem("root", Function("root", Integer(99))), dependencies, []));

        Encode(changed).Should().Equal(Encode(fresh));
    }

    [Fact]
    public void Rejects_truncated_unresolved_wrong_kind_and_invalid_graph_references()
    {
        var model = Value("root", Integer(42));
        Declaration? declaration = null;
        using var stream = new MemoryStream();

        SccEncodingBinary.Encode(
            stream,
            model,
            syntax =>
            {
                declaration = syntax;
                return ElmSyntaxAbstractBinary.ComputeHash(syntax);
            });

        var bytes = stream.ToArray();

        for (var length = 0; length < bytes.Length; length++)
        {
            var prefix = bytes.AsMemory(0, length);
            Action truncated = () => SccEncodingBinary.Decode(prefix, _ => declaration, _ => null, out _);
            truncated.Should().Throw<InvalidDataException>();
        }

        Action unresolved = () => SccEncodingBinary.Decode(bytes, _ => null, _ => null, out _);
        unresolved.Should().Throw<InvalidDataException>();

        Action wrongKind =
            () => SccEncodingBinary.Decode(
                bytes,
                _ => new Declaration.AliasDeclaration(new("root", [], new TypeAnnotation.Unit())),
                _ => null,
                out _);

        wrongKind.Should().Throw<InvalidDataException>();

        Action wrongName =
            () => SccEncodingBinary.Decode(
                bytes,
                _ => new Declaration.FunctionDeclaration(Function("other", Integer(42)).Function),
                _ => null,
                out _);

        wrongName.Should().Throw<InvalidDataException>();
        Action shortReference = () => SccEncodingBinary.Encode(new MemoryStream(), model, _ => new byte[31]);
        shortReference.Should().Throw<ArgumentException>();
    }
}
