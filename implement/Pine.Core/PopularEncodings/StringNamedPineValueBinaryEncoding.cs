using System;
using System.Collections.Generic;
using System.IO;

namespace Pine.Core.PopularEncodings;

/// <summary>
/// Wraps binary encoding of PineValues (<see cref="PineValueBinaryEncoding"/>) to model a
/// set of values each associated with a string name.
/// </summary>
public static class StringNamedPineValueBinaryEncoding
{
    /// <summary>
    /// Encodes a set of named <see cref="PineValue"/> declarations into a binary format and writes it to the provided stream.
    /// </summary>
    /// <param name="stream">The stream to write the encoded data to.</param>
    /// <param name="declarations">A dictionary containing string names and their associated <see cref="PineValue"/> objects.</param>
    public static void Encode(
        Stream stream,
        IReadOnlyDictionary<string, PineValue> declarations)
    {
        void Write(ReadOnlySpan<byte> bytes)
        {
            stream.Write(bytes);
        }

        Encode(declarations, Write);
    }

    private static void Encode(
        IReadOnlyDictionary<string, PineValue> declarations,
        Action<ReadOnlySpan<byte>> write)
    {
        static PineValue DeclToValue(string name, PineValue value)
        {
            // A declaration is represented as a 2-element array: [name, value]

            return PineValue.List(
                [
                StringEncoding.ValueFromString(name),
                value
                ]);
        }

        var declsValues = new PineValue[declarations.Count];

        var i = 0;

        foreach (var (name, value) in declarations)
        {
            declsValues[i] = DeclToValue(name, value);
            i++;
        }

        PineValue declarationComposition =
            PineValue.List([.. declsValues]);

        PineValueBinaryEncoding.Encode(write, declarationComposition);
    }

    /// <summary>
    /// Decodes a binary-encoded set of named <see cref="PineValue"/> declarations from the provided bytes.
    /// </summary>
    /// <param name="bytes">The binary data to decode.</param>
    /// <returns>
    /// A tuple containing the decoded sequence of <see cref="PineValue"/> objects and a dictionary mapping string names to their associated <see cref="PineValue"/> objects.
    /// </returns>
    public static (IReadOnlyList<PineValue> decodedSequence, IReadOnlyDictionary<string, PineValue> decls)
        Decode(
        ReadOnlyMemory<byte> bytes)
    {
        var sequence = new List<PineValue>();

        PineValue? rootValue = null;

        foreach (var decl in PineValueBinaryEncoding.DecodeSequence(bytes))
        {
            rootValue = decl.declValue;

            sequence.Add(decl.declValue);
        }

        if (rootValue is not PineValue.ListValue declsList)
        {
            throw new InvalidDataException(
                "Expected a list of declarations at the root but got: " + rootValue.GetType());
        }

        var declarations = new Dictionary<string, PineValue>();

        for (var declIndex = 0; declIndex < declsList.Elements.Length; declIndex++)
        {
            try
            {
                var declValue = declsList.Elements.Span[declIndex];

                if (declValue is not PineValue.ListValue declList)
                {
                    throw new InvalidDataException(
                        "Expected a declaration to be a 2-element list but got: " + declValue.GetType());
                }

                if (declList.Elements.Length is not 2)
                {
                    throw new InvalidDataException(
                        "Expected a declaration to be a 2-element list but got a list with " +
                        declList.Elements.Length + " elements.");
                }

                var nameValue = declList.Elements.Span[0];
                var valueValue = declList.Elements.Span[1];

                var nameResult = StringEncoding.StringFromValue(nameValue);

                if (nameResult is Result<string, string>.Err err)
                {
                    throw new InvalidDataException(
                        "Expected the first element of a declaration to be a string but got an error: " + err.Value);
                }

                if (nameResult is not Result<string, string>.Ok ok)
                {
                    throw new InvalidDataException(
                        "Expected the first element of a declaration to be a string but got an unexpected result type: " +
                        nameResult.GetType().FullName);
                }

                var name = ok.Value;

                if (declarations.ContainsKey(name))
                {
                    throw new InvalidDataException(
                        "Duplicate declaration for name: " + name);
                }

                declarations[name] = valueValue;
            }
            catch (Exception ex)
            {
                throw new InvalidDataException(
                    "Error decoding declaration at index " + declIndex + " of " + declsList.Elements.Length + ": " + ex.Message, ex);
            }
        }

        return
            (decodedSequence: sequence, decls: declarations);
    }
}
