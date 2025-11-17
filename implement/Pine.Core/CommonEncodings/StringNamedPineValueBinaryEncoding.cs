using System;
using System.Collections.Generic;
using System.IO;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// Wraps binary encoding of PineValues (<see cref="ValueBinaryEncodingClassic"/>) to model a
/// set of values each associated with a string name.
/// </summary>
public static class StringNamedPineValueBinaryEncoding
{
    /// <summary>
    /// Encodes a set of named <see cref="PineValue"/> declarations into a binary format and writes it to the provided stream.
    /// </summary>
    /// <param name="stream">The stream to write the encoded data to.</param>
    /// <param name="declarations">A dictionary containing string names and their associated <see cref="PineValue"/> objects.</param>
    /// <param name="componentDeclWritten">An optional callback invoked after each component declaration is written, with the <see cref="PineValue"/> and its position.</param>
    public static void Encode(
        Stream stream,
        IReadOnlyDictionary<string, PineValue> declarations,
        Action<PineValue, long>? componentDeclWritten = null)
    {
        void Write(ReadOnlySpan<byte> bytes)
        {
            stream.Write(bytes);
        }

        Encode(declarations, Write, componentDeclWritten);
    }

    private static void Encode(
        IReadOnlyDictionary<string, PineValue> declarations,
        Action<ReadOnlySpan<byte>> write,
        Action<PineValue, long>? componentDeclWritten = null)
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

        ValueBinaryEncodingClassic.Encode(write, declarationComposition, onDeclarationWritten: componentDeclWritten);
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

        foreach (var (declId, declValue) in ValueBinaryEncodingClassic.DecodeSequence(bytes))
        {
            rootValue = declValue;

            sequence.Add(declValue);
        }

        if (rootValue is null)
        {
            throw new InvalidDataException(
                "Expected at least one declaration but got none.");
        }

        if (rootValue is not PineValue.ListValue declsList)
        {
            throw new InvalidDataException(
                "Expected a list of declarations at the root but got: " + rootValue.GetType());
        }

        var declarations = new Dictionary<string, PineValue>();

        for (var declIndex = 0; declIndex < declsList.Items.Length; declIndex++)
        {
            try
            {
                var declValue = declsList.Items.Span[declIndex];

                if (declValue is not PineValue.ListValue declList)
                {
                    throw new InvalidDataException(
                        "Expected a declaration to be a 2-element list but got: " + declValue.GetType());
                }

                if (declList.Items.Length is not 2)
                {
                    throw new InvalidDataException(
                        "Expected a declaration to be a 2-element list but got a list with " +
                        declList.Items.Length + " elements.");
                }

                var nameValue = declList.Items.Span[0];
                var valueValue = declList.Items.Span[1];

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
                    "Error decoding declaration at index " + declIndex + " of " + declsList.Items.Length + ": " + ex.Message, ex);
            }
        }

        return
            (decodedSequence: sequence, decls: declarations);
    }
}
