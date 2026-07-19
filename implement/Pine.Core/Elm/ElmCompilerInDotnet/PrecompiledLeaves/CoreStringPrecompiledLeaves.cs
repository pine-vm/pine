using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Precompiled leaves for functions in the kernel <c>String</c> module.
/// </summary>
public static class CoreStringPrecompiledLeaves
{
    /// <summary>
    /// Pine value key for the precompiled <c>String.toListRecursive</c> leaf.
    /// </summary>
    public static PineValue ToListRecursiveLeafKey =>
        s_leafInfos.Value["toListRecursive"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>String.splitHelperOnBlob</c> leaf.
    /// </summary>
    public static PineValue SplitHelperOnBlobLeafKey =>
        s_leafInfos.Value["splitHelperOnBlob"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>String.linesHelper</c> leaf.
    /// </summary>
    public static PineValue LinesHelperLeafKey =>
        s_leafInfos.Value["linesHelper"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>String.toFloat</c> leaf.
    /// </summary>
    public static PineValue ToFloatLeafKey =>
        s_leafInfos.Value["toFloat"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>String.toInt</c> leaf.
    /// </summary>
    public static PineValue ToIntLeafKey =>
        s_leafInfos.Value["toInt"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>String.fromInt</c> leaf.
    /// </summary>
    public static PineValue FromIntLeafKey =>
        s_leafInfos.Value["fromInt"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>String.trimLeftCountBytesTrimmed</c> leaf.
    /// </summary>
    public static PineValue TrimLeftCountBytesTrimmedLeafKey =>
        s_leafInfos.Value["trimLeftCountBytesTrimmed"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>String.trimRightCountBytesRemaining</c> leaf.
    /// </summary>
    public static PineValue TrimRightCountBytesRemainingLeafKey =>
        s_leafInfos.Value["trimRightCountBytesRemaining"].leafKey;

    private static readonly Lazy<IReadOnlyDictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>>
        s_leafInfos =
        new(BuildLeafInfos);

    private static IReadOnlyDictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>
        BuildLeafInfos()
    {
        var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

        var rootFilePaths =
            kernelModulesTree.EnumerateFilesTransitive()
            .Where(file => file.path[^1].Equals("String.elm", StringComparison.OrdinalIgnoreCase))
            .Select(file => (IReadOnlyList<string>)file.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                kernelModulesTree,
                rootFilePaths: rootFilePaths)
            .Map(result => result.compiledEnvValue)
            .Extract(
                error => throw new Exception(
                    "Failed compiling kernel String module to derive leaf info: " + error));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(
                error => throw new Exception(
                    "Failed parsing kernel String module to derive leaf info: " + error));

        var stringModule =
            parsedEnv.Modules
            .First(module => module.moduleName is "String")
            .moduleContent;

        var parseCache = new PineVMParseCache();
        var infos = new Dictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>();

        foreach (var functionName in new[]
        {
            "toListRecursive",
            "splitHelperOnBlob",
            "linesHelper",
            "toFloat",
            "toInt",
            "fromInt",
            "trimLeftCountBytesTrimmed",
            "trimRightCountBytesRemaining",
        })
        {
            var record =
                FunctionRecord.ParseFunctionRecordTagged(
                    stringModule.FunctionDeclarations[functionName],
                    parseCache)
                .Extract(
                    error => throw new Exception(
                        $"Failed parsing String.{functionName} function record to derive leaf info: {error}"));

            infos.Add(
                functionName,
                (ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction),
                PineValue.List([.. record.EnvFunctions.ToArray()])));
        }

        return infos;
    }

    /// <summary>
    /// Executes <c>String.toListRecursive</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValue? ToListRecursiveLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "toListRecursive") ||
            !TryParseIndex(environment.ValueFromPathOrEmptyList([1]), out var offset) ||
            environment.ValueFromPathOrEmptyList([2]) is not PineValue.ListValue collected ||
            environment.ValueFromPathOrEmptyList([3]) is not PineValue.BlobValue chars)
        {
            return null;
        }

        var result = new List<PineValue>(collected.Items.ToArray());

        for (var index = offset; index < chars.Bytes.Length; index += 4)
        {
            result.Add(
                PineValue.Blob(
                    chars.Bytes.Slice(index, Math.Min(4, chars.Bytes.Length - index))));
        }

        return PineValue.List([.. result]);
    }

    /// <summary>
    /// Executes <c>String.splitHelperOnBlob</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValue? SplitHelperOnBlobLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "splitHelperOnBlob") ||
            !TryParseIndex(environment.ValueFromPathOrEmptyList([1]), out var offset) ||
            environment.ValueFromPathOrEmptyList([2]) is not PineValue.ListValue collected ||
            !TryParseIndex(environment.ValueFromPathOrEmptyList([3]), out var lastStart) ||
            environment.ValueFromPathOrEmptyList([4]) is not PineValue.BlobValue separator ||
            environment.ValueFromPathOrEmptyList([5]) is not PineValue.BlobValue chars ||
            separator.Bytes.Length is 0 ||
            lastStart > offset ||
            offset > chars.Bytes.Length ||
            lastStart > chars.Bytes.Length)
        {
            return null;
        }

        var result = new List<PineValue>(collected.Items.ToArray());

        while (separator.Bytes.Length <= chars.Bytes.Length - offset)
        {
            var slice = chars.Bytes.Slice(offset, separator.Bytes.Length);

            if (slice.Span.SequenceEqual(separator.Bytes.Span))
            {
                result.Add(StringValue(chars.Bytes.Slice(lastStart, offset - lastStart)));
                offset += separator.Bytes.Length;
                lastStart = offset;
            }
            else
            {
                offset += 4;
            }
        }

        result.Add(StringValue(chars.Bytes[lastStart..]));

        return PineValue.List([.. result]);
    }

    /// <summary>
    /// Executes <c>String.linesHelper</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValue? LinesHelperLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "linesHelper") ||
            !TryParseIndex(environment.ValueFromPathOrEmptyList([1]), out var currentLineStart) ||
            environment.ValueFromPathOrEmptyList([2]) is not PineValue.ListValue currentLines ||
            !TryParseIndex(environment.ValueFromPathOrEmptyList([3]), out var offset) ||
            environment.ValueFromPathOrEmptyList([4]) is not PineValue.BlobValue chars ||
            currentLineStart > offset ||
            offset > chars.Bytes.Length ||
            currentLineStart > chars.Bytes.Length)
        {
            return null;
        }

        var result = new List<PineValue>(currentLines.Items.ToArray());

        while (offset < chars.Bytes.Length)
        {
            if (4 <= chars.Bytes.Length - offset)
            {
                var nextChar = System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(chars.Bytes.Span[offset..]);

                if (nextChar is 0x0d &&
                    8 <= chars.Bytes.Length - offset &&
                    System.Buffers.Binary.BinaryPrimitives.ReadInt32BigEndian(chars.Bytes.Span[(offset + 4)..]) is 0x0a)
                {
                    result.Add(StringValue(chars.Bytes.Slice(currentLineStart, offset - currentLineStart)));
                    offset += 8;
                    currentLineStart = offset;
                    continue;
                }

                if (nextChar is 0x0a or 0x0d)
                {
                    result.Add(StringValue(chars.Bytes.Slice(currentLineStart, offset - currentLineStart)));
                    offset += 4;
                    currentLineStart = offset;
                    continue;
                }
            }

            offset += 4;
        }

        result.Add(StringValue(chars.Bytes[currentLineStart..]));

        return PineValue.List([.. result]);
    }

    /// <summary>
    /// Executes <c>String.toFloat</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValue? ToFloatLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "toFloat") ||
            !TryGetStringChars(environment.ValueFromPathOrEmptyList([1]), out var chars))
        {
            return null;
        }

        if (chars.Length is 0)
        {
            return s_nothing;
        }

        var firstCodePoint =
            System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(chars.Span);

        var isNegative = firstCodePoint is '-';
        var unsignedChars = isNegative ? chars[4..] : chars;
        var (numerator, denominator) = ToRationalComponentsLessSign(unsignedChars);

        if (denominator is null)
        {
            return s_nothing;
        }

        if (isNegative)
        {
            numerator = -numerator;
        }

        return
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.TagInstance(
                    "Just",
                    [ElmValue.ElmFloat.NotNormalized(numerator, denominator.Value)]));
    }

    /// <summary>
    /// Executes <c>String.toInt</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValue? ToIntLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "toInt") ||
            !TryGetStringChars(environment.ValueFromPathOrEmptyList([1]), out var chars))
        {
            return null;
        }

        if (ParseInt(chars) is not { } integer)
        {
            return s_nothing;
        }

        return
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.TagInstance("Just", [ElmValue.Integer(integer)]));
    }

    /// <summary>
    /// Executes <c>String.fromInt</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValue? FromIntLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "fromInt") ||
            IntegerEncoding.ParseSignedIntegerRelaxed(
                environment.ValueFromPathOrEmptyList([1]))
            .IsOkOrNullable() is not { } integer)
        {
            return null;
        }

        return
            StringValue(
                StringEncoding.BlobValueFromString(
                    integer.ToString(System.Globalization.CultureInfo.InvariantCulture))
                .Bytes);
    }

    /// <summary>
    /// Executes <c>String.trimLeftCountBytesTrimmed</c> directly, or returns <c>null</c>
    /// for an unexpected environment.
    /// </summary>
    public static PineValue? TrimLeftCountBytesTrimmedLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "trimLeftCountBytesTrimmed") ||
            !TryParseIndex(environment.ValueFromPathOrEmptyList([1]), out var offset) ||
            environment.ValueFromPathOrEmptyList([2]) is not PineValue.BlobValue chars ||
            chars.Bytes.Length % 4 is not 0 ||
            offset > chars.Bytes.Length ||
            offset % 4 is not 0)
        {
            return null;
        }

        while (offset < chars.Bytes.Length &&
            IsCharRemovedOnTrim(chars.Bytes.Span[offset..]))
        {
            offset += 4;
        }

        return IntegerEncoding.EncodeSignedInteger(offset);
    }

    /// <summary>
    /// Executes <c>String.trimRightCountBytesRemaining</c> directly, or returns <c>null</c>
    /// for an unexpected environment.
    /// </summary>
    public static PineValue? TrimRightCountBytesRemainingLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, "trimRightCountBytesRemaining") ||
            !TryParseIndex(environment.ValueFromPathOrEmptyList([1]), out var remainingLength) ||
            environment.ValueFromPathOrEmptyList([2]) is not PineValue.BlobValue chars ||
            chars.Bytes.Length % 4 is not 0 ||
            remainingLength > chars.Bytes.Length ||
            remainingLength % 4 is not 0)
        {
            return null;
        }

        while (remainingLength > 0 &&
            IsCharRemovedOnTrim(chars.Bytes.Span[(remainingLength - 4)..]))
        {
            remainingLength -= 4;
        }

        return IntegerEncoding.EncodeSignedInteger(remainingLength);
    }

    private static bool IsCharRemovedOnTrim(ReadOnlySpan<byte> charBytes)
    {
        var codePoint =
            System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(charBytes);

        return codePoint is ' ' or '\t' or '\n' or '\r' or 0x00A0;
    }

    private static (System.Numerics.BigInteger Numerator, System.Numerics.BigInteger? Denominator)
        ToRationalComponentsLessSign(ReadOnlyMemory<byte> chars)
    {
        var lower = ParseWithExponent(chars, 'e');

        if (lower.Denominator is not null)
        {
            return lower;
        }

        var upper = ParseWithExponent(chars, 'E');

        return
            upper.Denominator is not null
            ?
            upper
            :
            ToRationalComponentsWithoutExponent(chars);
    }

    private static (System.Numerics.BigInteger Numerator, System.Numerics.BigInteger? Denominator)
        ParseWithExponent(ReadOnlyMemory<byte> chars, uint exponentChar)
    {
        var segments = SplitOnCodePoint(chars, exponentChar);

        if (segments.Count is not 2 ||
            segments[0].Length is 0 ||
            segments[1].Length is 0)
        {
            return (0, null);
        }

        var mantissa = ToRationalComponentsWithoutExponent(segments[0]);

        if (mantissa.Denominator is not { } denominator ||
            ParseInt(segments[1]) is not { } exponent)
        {
            return (0, null);
        }

        var exponentIsNonPositive = exponent <= 0;
        var exponentMagnitude = exponentIsNonPositive ? -exponent : exponent;

        if (exponentMagnitude > int.MaxValue)
        {
            return (0, null);
        }

        var powerOfTen = System.Numerics.BigInteger.Pow(10, (int)exponentMagnitude);

        return
            exponentIsNonPositive
            ?
            (mantissa.Numerator, denominator * powerOfTen)
            :
            (mantissa.Numerator * powerOfTen, denominator);
    }

    private static (System.Numerics.BigInteger Numerator, System.Numerics.BigInteger? Denominator)
        ToRationalComponentsWithoutExponent(ReadOnlyMemory<byte> chars)
    {
        var segments = SplitOnCodePoint(chars, '.');

        if (segments.Count is 1)
        {
            return
                ParseUnsignedInt(segments[0]) is { } whole
                ?
                (whole, 1)
                :
                (0, null);
        }

        if (segments.Count is not 2)
        {
            return (0, null);
        }

        var beforeSeparator = segments[0];
        var afterSeparator = segments[1];

        if (afterSeparator.Length is 0)
        {
            return
                beforeSeparator.Length is not 0 &&
                ParseUnsignedInt(beforeSeparator) is { } beforeOnly
                ?
                (beforeOnly, 1)
                :
                (0, null);
        }

        var before =
            beforeSeparator.Length is 0
            ?
            System.Numerics.BigInteger.Zero
            :
            ParseUnsignedInt(beforeSeparator);

        if (before is null ||
            ParseUnsignedInt(afterSeparator) is not { } after)
        {
            return (0, null);
        }

        var fractionalDigits = afterSeparator.Length / 4;
        var denominator =
            fractionalDigits is >= 1 and <= 10
            ?
            System.Numerics.BigInteger.Pow(10, fractionalDigits)
            :
            System.Numerics.BigInteger.One;

        return ((before.Value * denominator) + after, denominator);
    }

    private static List<ReadOnlyMemory<byte>> SplitOnCodePoint(
        ReadOnlyMemory<byte> chars,
        uint codePoint)
    {
        var segments = new List<ReadOnlyMemory<byte>>();
        var lastStart = 0;

        for (var offset = 0; offset < chars.Length; offset += 4)
        {
            if (System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(chars.Span[offset..]) == codePoint)
            {
                segments.Add(chars[lastStart..offset]);
                lastStart = offset + 4;
            }
        }

        segments.Add(chars[lastStart..]);

        return segments;
    }

    private static System.Numerics.BigInteger? ParseInt(ReadOnlyMemory<byte> chars)
    {
        if (chars.Length is 0)
        {
            return null;
        }

        var firstCodePoint =
            System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(chars.Span);

        if (firstCodePoint is '-')
        {
            return
                ParseUnsignedInt(chars[4..]) is { } magnitude
                ?
                -magnitude
                :
                null;
        }

        if (firstCodePoint is '+')
        {
            return ParseUnsignedInt(chars[4..]);
        }

        return ParseUnsignedInt(chars);
    }

    private static System.Numerics.BigInteger? ParseUnsignedInt(ReadOnlyMemory<byte> chars)
    {
        if (chars.Length is 0)
        {
            return null;
        }

        System.Numerics.BigInteger accumulator = 0;

        for (var offset = 0; offset < chars.Length; offset += 4)
        {
            var codePoint =
                System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(chars.Span[offset..]);

            if (codePoint is < '0' or > '9')
            {
                return null;
            }

            accumulator = (accumulator * 10) + (codePoint - '0');
        }

        return accumulator;
    }

    private static bool TryGetStringChars(
        PineValue value,
        out ReadOnlyMemory<byte> chars)
    {
        if (value is PineValue.ListValue stringValue &&
            stringValue.Items.Length is 2 &&
            stringValue.Items.Span[0] == ElmValue.ElmStringTypeTagNameAsValue &&
            stringValue.Items.Span[1] is PineValue.ListValue arguments &&
            arguments.Items.Length is 1 &&
            arguments.Items.Span[0] is PineValue.BlobValue charsBlob &&
            charsBlob.Bytes.Length % 4 is 0)
        {
            chars = charsBlob.Bytes;
            return true;
        }

        chars = default;
        return false;
    }

    private static readonly PineValue s_nothing =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("Nothing", []));

    private static bool EnvironmentMatches(PineValue environment, string functionName) =>
        environment.ValueFromPathOrEmptyList([0]) ==
        s_leafInfos.Value[functionName].envFunctionsValue;

    private static bool TryParseIndex(PineValue value, out int index)
    {
        var parsed = IntegerEncoding.ParseSignedIntegerRelaxed(value).IsOkOrNullable();

        if (parsed is null || parsed < 0 || parsed > int.MaxValue)
        {
            index = 0;
            return false;
        }

        index = (int)parsed;
        return true;
    }

    private static PineValue StringValue(ReadOnlyMemory<byte> chars) =>
        PineValue.List(
            [
            ElmValue.ElmStringTypeTagNameAsValue,
            PineValue.List([PineValue.Blob(chars)]),
            ]);

    /// <summary>
    /// Default precompiled-leaves dictionary contributed by the kernel <c>String</c> module.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
            .Add(ToListRecursiveLeafKey, ToListRecursiveLeafDelegate)
            .Add(SplitHelperOnBlobLeafKey, SplitHelperOnBlobLeafDelegate)
            .Add(LinesHelperLeafKey, LinesHelperLeafDelegate)
            .Add(ToFloatLeafKey, ToFloatLeafDelegate)
            .Add(ToIntLeafKey, ToIntLeafDelegate)
            .Add(FromIntLeafKey, FromIntLeafDelegate)
            .Add(TrimLeftCountBytesTrimmedLeafKey, TrimLeftCountBytesTrimmedLeafDelegate)
            .Add(TrimRightCountBytesRemainingLeafKey, TrimRightCountBytesRemainingLeafDelegate));
}
