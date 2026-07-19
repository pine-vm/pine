using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Precompiled leaves for functions in the kernel <c>Bytes.Decode</c> and <c>Bytes.Encode</c> modules.
/// </summary>
public static class KernelBytesPrecompiledLeaves
{
    /// <summary>
    /// Pine value key for the precompiled <c>Bytes.Decode.decodeBlobAsCharsRec</c> leaf.
    /// </summary>
    public static PineValue DecodeBlobAsCharsRecLeafKey =>
        s_leafInfos.Value["decodeBlobAsCharsRec"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>Bytes.Encode.encodeCharsAsBlobHelp</c> leaf.
    /// </summary>
    public static PineValue EncodeCharsAsBlobHelpLeafKey =>
        s_leafInfos.Value["encodeCharsAsBlobHelp"].leafKey;

    private static readonly Lazy<IReadOnlyDictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>>
        s_leafInfos =
        new(BuildLeafInfos);

    /// <summary>
    /// Default precompiled-leaves dictionary contributed by the kernel <c>Bytes</c> modules.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
            .Add(
                DecodeBlobAsCharsRecLeafKey,
                DecodeBlobAsCharsRecLeafDelegate)
            .Add(
                EncodeCharsAsBlobHelpLeafKey,
                EncodeCharsAsBlobHelpLeafDelegate));

    private static IReadOnlyDictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>
        BuildLeafInfos()
    {
        var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

        var rootFilePaths =
            kernelModulesTree.EnumerateFilesTransitive()
            .Where(
                file =>
                file.path.Count is 2 &&
                file.path[0].Equals("Bytes", StringComparison.OrdinalIgnoreCase) &&
                (file.path[1].Equals("Decode.elm", StringComparison.OrdinalIgnoreCase) ||
                file.path[1].Equals("Encode.elm", StringComparison.OrdinalIgnoreCase)))
            .Select(file => (IReadOnlyList<string>)file.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                kernelModulesTree,
                rootFilePaths: rootFilePaths)
            .Map(result => result.compiledEnvValue)
            .Extract(
                error => throw new Exception(
                    "Failed compiling kernel Bytes modules to derive leaf info: " + error));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(
                error => throw new Exception(
                    "Failed parsing kernel Bytes modules to derive leaf info: " + error));

        var parseCache = new PineVMParseCache();
        var infos = new Dictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>();

        AddLeafInfo("Bytes.Decode", "decodeBlobAsCharsRec");
        AddLeafInfo("Bytes.Encode", "encodeCharsAsBlobHelp");

        return infos;

        void AddLeafInfo(string moduleName, string functionName)
        {
            var functionValue =
                parsedEnv.Modules
                .First(module => module.moduleName == moduleName)
                .moduleContent.FunctionDeclarations[functionName];

            var record =
                FunctionRecord.ParseFunctionRecordTagged(functionValue, parseCache)
                .Extract(
                    error => throw new Exception(
                        $"Failed parsing {moduleName}.{functionName} function record to derive leaf info: {error}"));

            infos.Add(
                functionName,
                (ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction),
                PineValue.List([.. record.EnvFunctions.ToArray()])));
        }
    }

    public static PineValue? EncodeCharsAsBlobHelpLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "encodeCharsAsBlobHelp"))
        {
            return null;
        }

        var acc = environment.ValueFromPathOrEmptyList([1]);
        var offsetValue = environment.ValueFromPathOrEmptyList([2]);
        var charsValue = environment.ValueFromPathOrEmptyList([3]);

        if (IntegerEncoding.ParseSignedIntegerRelaxed(offsetValue).IsOkOrNullable() is not { } offset ||
            offset < 0)
        {
            return null;
        }

        if (charsValue == PineValue.EmptyList)
        {
            return acc;
        }

        if (charsValue is not PineValue.BlobValue chars)
        {
            return null;
        }

        if (offset >= chars.Bytes.Length)
        {
            return acc;
        }

        if (offset > int.MaxValue ||
            acc is not PineValue.BlobValue && acc != PineValue.EmptyList)
        {
            return null;
        }

        var accBytes =
            acc is PineValue.BlobValue accBlob
            ?
            accBlob.Bytes
            :
            ReadOnlyMemory<byte>.Empty;

        using var output =
            new MemoryStream(
                capacity: accBytes.Length + chars.Bytes.Length - (int)offset);

        output.Write(accBytes.Span);

        for (var index = (int)offset; index + 4 <= chars.Bytes.Length; index += 4)
        {
            var code = BinaryPrimitives.ReadInt32BigEndian(chars.Bytes.Span[index..]);

            if (code <= 0x7F)
            {
                output.WriteByte((byte)code);
            }
            else if (code <= 0x07FF)
            {
                output.WriteByte((byte)(0xC0 | (code / 64)));
                output.WriteByte((byte)(0x80 | (code & 63)));
            }
            else if (code <= 0xFFFF)
            {
                output.WriteByte((byte)(0xE0 | (code / 4096)));
                output.WriteByte((byte)(0x80 | ((code / 64) & 63)));
                output.WriteByte((byte)(0x80 | (code & 63)));
            }
            else
            {
                output.WriteByte((byte)(0xF0 | (code / 262144)));
                output.WriteByte((byte)(0x80 | ((code / 4096) & 63)));
                output.WriteByte((byte)(0x80 | ((code / 64) & 63)));
                output.WriteByte((byte)(0x80 | (code & 63)));
            }
        }

        return PineValue.Blob(output.ToArray());
    }

    public static PineValue? DecodeBlobAsCharsRecLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "decodeBlobAsCharsRec") ||
            IntegerEncoding.ParseSignedIntegerRelaxed(
                environment.ValueFromPathOrEmptyList([1]))
            .IsOkOrNullable() is not { } offset ||
            offset < 0 ||
            environment.ValueFromPathOrEmptyList([3]) is not PineValue.ListValue initialChars)
        {
            return null;
        }

        var blobValue = environment.ValueFromPathOrEmptyList([2]);

        ReadOnlyMemory<byte> blob;

        if (blobValue is PineValue.BlobValue blobValueAsBlob)
        {
            blob = blobValueAsBlob.Bytes;
        }
        else if (blobValue == PineValue.EmptyList)
        {
            blob = ReadOnlyMemory<byte>.Empty;
        }
        else
        {
            return null;
        }

        if (offset > int.MaxValue)
        {
            return StringValueFromReversedChars(initialChars.Items.Span, []);
        }

        var decodedChars = new List<int>();

        for (var sourceOffset = (int)offset; sourceOffset < blob.Length;)
        {
            var firstByte = ByteAtOrZero(blob.Span, sourceOffset);
            int code;
            int consumed;

            if (firstByte <= 0x7F)
            {
                code = firstByte;
                consumed = 1;
            }
            else if ((firstByte & 0xE0) == 0xC0)
            {
                code =
                    ((firstByte & 0x1F) * 64) |
                    (ByteAtOrZero(blob.Span, sourceOffset + 1) & 0x3F);

                consumed = 2;
            }
            else if ((firstByte & 0xF0) == 0xE0)
            {
                code =
                    ((firstByte & 0x0F) * 4096) |
                    ((ByteAtOrZero(blob.Span, sourceOffset + 1) & 0x3F) * 64) |
                    (ByteAtOrZero(blob.Span, sourceOffset + 2) & 0x3F);

                consumed = 3;
            }
            else if ((firstByte & 0xF8) == 0xF0)
            {
                code =
                    ((firstByte & 0x07) * 262144) |
                    ((ByteAtOrZero(blob.Span, sourceOffset + 1) & 0x3F) * 4096) |
                    ((ByteAtOrZero(blob.Span, sourceOffset + 2) & 0x3F) * 64) |
                    (ByteAtOrZero(blob.Span, sourceOffset + 3) & 0x3F);

                consumed = 4;
            }
            else
            {
                code = 0xFFFD;
                consumed = 1;
            }

            decodedChars.Add(code);
            sourceOffset += consumed;
        }

        return StringValueFromReversedChars(initialChars.Items.Span, decodedChars);
    }

    private static PineValue? StringValueFromReversedChars(
        ReadOnlySpan<PineValue> initialChars,
        IReadOnlyList<int> decodedChars)
    {
        var charsBytes = new byte[(initialChars.Length + decodedChars.Count) * 4];
        var destinationIndex = 0;

        for (var index = initialChars.Length - 1; index >= 0; --index)
        {
            if (IntegerEncoding.ParseUnsignedInteger(initialChars[index]).IsOkOrNullable() is not { } code ||
                code > int.MaxValue)
            {
                return null;
            }

            BinaryPrimitives.WriteInt32BigEndian(
                charsBytes.AsSpan(destinationIndex * 4),
                (int)code);

            destinationIndex++;
        }

        for (var index = 0; index < decodedChars.Count; ++index)
        {
            BinaryPrimitives.WriteInt32BigEndian(
                charsBytes.AsSpan(destinationIndex * 4),
                decodedChars[index]);

            destinationIndex++;
        }

        return
            PineValue.List(
                [
                ElmValue.ElmStringTypeTagNameAsValue,
                PineValue.List([PineValue.Blob(charsBytes)]),
                ]);
    }

    private static byte ByteAtOrZero(ReadOnlySpan<byte> bytes, int index) =>
        index < bytes.Length ? bytes[index] : (byte)0;

    private static bool EnvironmentMatches(PineValue environment, string functionName) =>
        environment.ValueFromPathOrEmptyList([0]) ==
        s_leafInfos.Value[functionName].envFunctionsValue;
}
