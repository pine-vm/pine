using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using System;
using System.Buffers;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Security.Cryptography;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>
/// Versioned SCC blobs with 256-bit external references to every member declaration
/// and dependency SCC. Declaration bodies and dependency graphs are never embedded.
/// </summary>
public static class SccEncodingBinary
{
    private const int Header = 0x45534301;

    /// <summary>
    /// Writes a deterministic SCC blob. By default external IDs are SHA-256 binary syntax
    /// hashes and dependency fingerprints. Callbacks may store referenced objects and return
    /// caller-defined 32-byte IDs. They must be stable for equal content.
    /// </summary>
    public static void Encode(
        Action<ReadOnlySpan<byte>> write,
        SccModel model,
        Func<Declaration, ReadOnlyMemory<byte>>? declarationReference = null,
        Func<SccModel, ReadOnlyMemory<byte>>? sccReference = null)
    {
        using var writer = new BinaryEncoding.Writer(write);
        writer.Int32(Header);

        switch (model)
        {
            case SccModel.ValueScc value:
                WriteValue(writer, value.Value, declarationReference, sccReference);
                break;

            case SccModel.TypeScc type:
                WriteType(writer, type.Value, declarationReference, sccReference);
                break;

            default:
                throw new NotImplementedException(
                    nameof(Encode) + " does not handle SCC variant: " + model.GetType().Name);
        }
    }

    /// <summary>Writes an SCC blob without closing the destination stream.</summary>
    public static void Encode(
        Stream stream,
        SccModel model,
        Func<Declaration, ReadOnlyMemory<byte>>? declarationReference = null,
        Func<SccModel, ReadOnlyMemory<byte>>? sccReference = null) =>
        Encode(stream.Write, model, declarationReference, sccReference);

    internal static ReadOnlyMemory<byte> ComputeHash(ValueSccModel model)
    {
        using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);

        using (var writer = new BinaryEncoding.Writer(hash.AppendData))
        {
            writer.Int32(Header);
            WriteValue(writer, model, null, null);
        }

        return hash.GetHashAndReset();
    }

    internal static ReadOnlyMemory<byte> ComputeHash(TypeSccModel model)
    {
        using var hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256);

        using (var writer = new BinaryEncoding.Writer(hash.AppendData))
        {
            writer.Int32(Header);
            WriteType(writer, model, null, null);
        }

        return hash.GetHashAndReset();
    }

    private static void Reference(BinaryEncoding.Writer writer, ReadOnlyMemory<byte> reference)
    {
        if (reference.Length is not 32)
            throw new ArgumentException("External SCC/declaration references must contain exactly 32 bytes.");

        writer.Bytes(reference.Span);
    }

    private static void WriteValue(
        BinaryEncoding.Writer writer,
        ValueSccModel model,
        Func<Declaration, ReadOnlyMemory<byte>>? declarationReference,
        Func<SccModel, ReadOnlyMemory<byte>>? sccReference)
    {
        writer.Byte(1);

        using var members =
            new SortedEntries<string, ValueDeclaration>(
                model.Members,
                static (left, right) => StringComparer.Ordinal.Compare(left.Key, right.Key));

        writer.Int32(model.Members.Count);

        foreach (var (name, member) in members.Entries)
        {
            writer.String(name);

            Reference(
                writer,
                declarationReference?.Invoke(member.Syntax) ?? ElmSyntaxAbstractBinary.ComputeHash(member.Syntax));
        }

        using var values =
            new SortedEntries<DeclQualifiedName, SccModel>(
                model.Dependencies,
                static (left, right) => left.Key.CompareTo(right.Key));

        writer.Int32(model.Dependencies.Count);

        foreach (var (name, dependency) in values.Entries)
        {
            WriteName(writer, name);
            Reference(writer, sccReference?.Invoke(dependency) ?? dependency.FingerprintBytes);
        }

        WriteTypes(writer, model.TypeDependencies, sccReference);
    }

    private static void WriteType(
        BinaryEncoding.Writer writer,
        TypeSccModel model,
        Func<Declaration, ReadOnlyMemory<byte>>? declarationReference,
        Func<SccModel, ReadOnlyMemory<byte>>? sccReference)
    {
        writer.Byte(2);

        using var members =
            new SortedEntries<string, TypeDeclaration>(
                model.Members,
                static (left, right) => StringComparer.Ordinal.Compare(left.Key, right.Key));

        writer.Int32(model.Members.Count);

        foreach (var (name, member) in members.Entries)
        {
            writer.String(name);

            Reference(
                writer,
                declarationReference?.Invoke(member.Syntax) ?? ElmSyntaxAbstractBinary.ComputeHash(member.Syntax));
        }

        WriteTypes(writer, model.TypeDependencies, sccReference);

        using var intrinsics =
            new SortedEntries<string, string>(
                model.IntrinsicRepresentations,
                static (left, right) => StringComparer.Ordinal.Compare(left.Key, right.Key));

        writer.Int32(model.IntrinsicRepresentations.Count);

        foreach (var (member, role) in intrinsics.Entries)
        {
            writer.String(member);
            writer.String(role);
        }
    }

    private static void WriteTypes(
        BinaryEncoding.Writer writer,
        ImmutableDictionary<DeclQualifiedName, TypeSccModel> dependencies,
        Func<SccModel, ReadOnlyMemory<byte>>? sccReference)
    {
        using var entries =
            new SortedEntries<DeclQualifiedName, TypeSccModel>(
                dependencies,
                static (left, right) => left.Key.CompareTo(right.Key));

        writer.Int32(dependencies.Count);

        foreach (var (name, dependency) in entries.Entries)
        {
            WriteName(writer, name);

            Reference(
                writer,
                sccReference is null ? dependency.FingerprintBytes : sccReference(new SccModel.TypeScc(dependency)));
        }
    }

    private static void WriteName(BinaryEncoding.Writer writer, DeclQualifiedName name)
    {
        writer.Int32(name.Namespaces.Count);

        for (var index = 0; index < name.Namespaces.Count; index++)
            writer.String(name.Namespaces[index]);

        writer.String(name.DeclName);
    }

    private static DeclQualifiedName ReadName(ref BinaryEncoding.Reader reader)
    {
        var namespaces = new string[reader.Count()];

        for (var index = 0; index < namespaces.Length; index++)
            namespaces[index] = reader.String();

        return DeclQualifiedName.Create(namespaces, reader.String());
    }

    private readonly ref struct SortedEntries<TKey, TValue> : IDisposable
        where TKey : notnull
    {
        private readonly KeyValuePair<TKey, TValue>[] _entries;

        private readonly int _count;

        public SortedEntries(
            ImmutableDictionary<TKey, TValue> entries,
            Comparison<KeyValuePair<TKey, TValue>> compare)
        {
            _count = entries.Count;
            _entries = _count is 0 ? [] : ArrayPool<KeyValuePair<TKey, TValue>>.Shared.Rent(_count);
            var index = 0;

            foreach (var entry in entries)
                _entries[index++] = entry;

            _entries.AsSpan(0, _count).Sort(compare);
        }

        public ReadOnlySpan<KeyValuePair<TKey, TValue>> Entries => _entries.AsSpan(0, _count);

        public void Dispose()
        {
            if (_count is not 0)
                ArrayPool<KeyValuePair<TKey, TValue>>.Shared.Return(_entries, clearArray: true);
        }
    }

    /// <summary>
    /// Resolves a single SCC blob, retaining the supplied declaration/dependency instances.
    /// Reports its extent for embedding; unresolved, wrong-kind, or invalid graph data fails explicitly.
    /// </summary>
    public static SccModel Decode(
        ReadOnlyMemory<byte> source,
        Func<ReadOnlyMemory<byte>, Declaration?> resolveDeclaration,
        Func<ReadOnlyMemory<byte>, SccModel?> resolveScc,
        out int bytesRead)
    {
        var reader = new BinaryEncoding.Reader(source);

        if (reader.Int32() != Header)
            throw reader.Error("Unsupported SCC format or version");

        var kind = reader.Byte();

        if (kind is not (1 or 2))
            throw reader.Error("Unknown SCC kind");

        var valueMembers = ImmutableDictionary.CreateBuilder<string, ValueDeclaration>(StringComparer.Ordinal);
        var typeMembers = ImmutableDictionary.CreateBuilder<string, TypeDeclaration>(StringComparer.Ordinal);
        var memberCount = reader.Count();

        for (var index = 0; index < memberCount; index++)
        {
            var name = reader.String();

            var declaration =
                resolveDeclaration(reader.Memory(32)) ??
                throw reader.Error("Unresolved external declaration reference");

            switch (declaration)
            {
                case Declaration.FunctionDeclaration function:
                    AddValue(name, new ValueDeclaration.FunctionDeclaration(function.Function));
                    break;

                case Declaration.PortDeclaration port:
                    AddValue(name, new ValueDeclaration.PortDeclaration(port.Signature));
                    break;

                case Declaration.InfixDeclaration infix:
                    AddValue(name, new ValueDeclaration.InfixDeclaration(infix.Infix));
                    break;

                case Declaration.ChoiceTypeDeclaration choice:
                    AddType(name, new TypeDeclaration.ChoiceTypeDeclaration(choice.TypeDeclaration));
                    break;

                case Declaration.AliasDeclaration alias:
                    AddType(name, new TypeDeclaration.AliasDeclaration(alias.TypeAlias));
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(Decode) + " does not handle declaration variant: " + declaration.GetType().Name);
            }
        }

        var values = ImmutableDictionary.CreateBuilder<DeclQualifiedName, SccModel>();

        if (kind is 1)
        {
            var count = reader.Count();

            for (var index = 0; index < count; index++)
            {
                var name = ReadName(ref reader);

                var dependency =
                    resolveScc(reader.Memory(32)) ??
                    throw reader.Error("Unresolved external value SCC reference");

                if (!values.TryAdd(name, dependency))
                    throw reader.Error("Duplicate value dependency");
            }
        }

        var types = ImmutableDictionary.CreateBuilder<DeclQualifiedName, TypeSccModel>();
        var typeCount = reader.Count();

        for (var index = 0; index < typeCount; index++)
        {
            var name = ReadName(ref reader);

            if (resolveScc(reader.Memory(32)) is not SccModel.TypeScc dependency)
                throw reader.Error("Unresolved or wrong-kind external type SCC reference");

            if (!types.TryAdd(name, dependency.Value))
                throw reader.Error("Duplicate type dependency");
        }

        var intrinsics = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.Ordinal);

        if (kind is 2)
        {
            var count = reader.Count();

            for (var index = 0; index < count; index++)
            {
                if (!intrinsics.TryAdd(reader.String(), reader.String()))
                    throw reader.Error("Duplicate intrinsic representation");
            }
        }

        bytesRead = reader.Position;

        try
        {
            return
                kind is 1
                ?
                new SccModel.ValueScc(new(valueMembers.ToImmutable(), values.ToImmutable(), types.ToImmutable()))
                :
                new SccModel.TypeScc(new(typeMembers.ToImmutable(), types.ToImmutable(), intrinsics.ToImmutable()));
        }
        catch (ArgumentException error)
        {
            throw new InvalidDataException("Invalid SCC graph.", error);
        }

        void AddValue(string name, ValueDeclaration declaration)
        {
            if (kind is not 1 || !valueMembers.TryAdd(name, declaration))
                throw new InvalidDataException("Wrong-kind or duplicate SCC member: " + name);
        }

        void AddType(string name, TypeDeclaration declaration)
        {
            if (kind is not 2 || !typeMembers.TryAdd(name, declaration))
                throw new InvalidDataException("Wrong-kind or duplicate SCC member: " + name);
        }
    }
}
