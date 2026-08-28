using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Precompiled leaves for the recursive string scanners in
/// <c>ElmSyntax.Concrete.Parser.StringParsing</c>, the module shared by the direct-source parser
/// (<c>ElmSyntax.Concrete.Parser.FromString</c>) and the tokenizer
/// (<c>ElmSyntax.Concrete.Parser.TokensFromString</c>). Because both parsers call the same
/// declarations, a single set of leaves accelerates both.
/// <para>
/// The location-aware whitespace scanner is the one exception: it tracks the parser state of
/// <c>ElmSyntax.Concrete.Parser.FromString</c> (including the comments collected so far) and
/// therefore lives in that module. It is still generic over all trivia rather than tied to any
/// subset of the grammar.
/// </para>
/// </summary>
public static class ElmSyntaxConcreteParserPrecompiledLeaves
{
    private const string StringParsingModuleName = "ElmSyntax.Concrete.Parser.StringParsing";

    private const string FromStringModuleName = "ElmSyntax.Concrete.Parser.FromString";

    private static readonly string[] s_stringParsingFunctionNames =
        [
            "skipInlineWhitespace",
            "skipToIdentifierEnd",
            "skipToAsciiDecimalDigitEnd",
            "skipToAsciiHexDigitEnd",
            "numberEndDecimal",
            "isFloatLiteralAt",
            "scanUnicodeEscapeDigits",
            "convert0OrMoreHexadecimalValue",
            "findLiteralRunEnd",
            "skipOperatorChars",
        ];

    private static readonly string[] s_fromStringFunctionNames =
        [
            "skipWhitespaceAt",
        ];

    /// <summary>Gets the leaf key for skipping inline whitespace.</summary>
    public static PineValue SkipInlineWhitespaceLeafKey => LeafKey(StringParsingModuleName, "skipInlineWhitespace");

    /// <summary>Gets the leaf key for skipping whitespace with location tracking.</summary>
    public static PineValue SkipWhitespaceAtLeafKey => LeafKey(FromStringModuleName, "skipWhitespaceAt");

    /// <summary>Gets the leaf key for scanning to an identifier's end.</summary>
    public static PineValue SkipToIdentifierEndLeafKey => LeafKey(StringParsingModuleName, "skipToIdentifierEnd");

    /// <summary>Gets the leaf key for scanning to an ASCII decimal number's end.</summary>
    public static PineValue SkipToAsciiDecimalDigitEndLeafKey =>
        LeafKey(StringParsingModuleName, "skipToAsciiDecimalDigitEnd");

    /// <summary>Gets the leaf key for scanning to an ASCII hexadecimal number's end.</summary>
    public static PineValue SkipToAsciiHexDigitEndLeafKey =>
        LeafKey(StringParsingModuleName, "skipToAsciiHexDigitEnd");

    /// <summary>Gets the leaf key for scanning the decimal portion of an Elm number.</summary>
    public static PineValue NumberEndDecimalLeafKey =>
        LeafKey(StringParsingModuleName, "numberEndDecimal");

    /// <summary>Gets the leaf key for testing whether a numeric literal is a float.</summary>
    public static PineValue IsFloatLiteralAtLeafKey =>
        LeafKey(StringParsingModuleName, "isFloatLiteralAt");

    /// <summary>Gets the leaf key for accumulating hexadecimal digits.</summary>
    public static PineValue Convert0OrMoreHexadecimalValueLeafKey =>
        LeafKey(StringParsingModuleName, "convert0OrMoreHexadecimalValue");

    /// <summary>Gets the leaf key for scanning Unicode escape digits.</summary>
    public static PineValue ScanUnicodeEscapeDigitsLeafKey =>
        LeafKey(StringParsingModuleName, "scanUnicodeEscapeDigits");

    /// <summary>Gets the leaf key for finding a literal run's end.</summary>
    public static PineValue FindLiteralRunEndLeafKey => LeafKey(StringParsingModuleName, "findLiteralRunEnd");

    /// <summary>Gets the leaf key for skipping operator characters.</summary>
    public static PineValue SkipOperatorCharsLeafKey => LeafKey(StringParsingModuleName, "skipOperatorChars");

    private static readonly Lazy<IReadOnlyDictionary<(string moduleName, string functionName), LeafInfo>> s_leafInfos =
        new(BuildLeafInfos);

    private sealed record LeafInfo(PineValue LeafKey, PineValue EnvFunctionsValue);

    private readonly record struct TriviaScan(
        BigInteger Offset,
        BigInteger Row,
        BigInteger Column);

    private static IReadOnlyDictionary<(string moduleName, string functionName), LeafInfo> BuildLeafInfos()
    {
        var mergedTree = BundledFiles.ElmKernelModulesDefault.Value;
        var compilerSourceTree = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var parserSourceTree =
            compilerSourceTree.GetNodeAtPath(["pine-elm-syntax", "src"])
            ?? throw new Exception("Did not find pine-elm-syntax/src");

        foreach (var (path, file) in parserSourceTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        var rootFilePaths =
            mergedTree.EnumerateFilesTransitive()
            .Where(
                file =>
                file.path[^1].Equals("StringParsing.elm", StringComparison.OrdinalIgnoreCase) ||
                file.path[^1].Equals("FromString.elm", StringComparison.OrdinalIgnoreCase))
            .Select(file => (IReadOnlyList<string>)file.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(mergedTree, rootFilePaths: rootFilePaths)
            .Map(result => result.compiledEnvValue)
            .Extract(
                error => throw new Exception(
                    "Failed compiling ElmSyntax.Concrete.Parser modules to derive leaf info: " + error));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(
                error => throw new Exception(
                    "Failed parsing ElmSyntax.Concrete.Parser modules to derive leaf info: " + error));

        var parseCache = new PineVMParseCache();
        var infos = new Dictionary<(string moduleName, string functionName), LeafInfo>();

        AddModuleFunctions(StringParsingModuleName, s_stringParsingFunctionNames);
        AddModuleFunctions(FromStringModuleName, s_fromStringFunctionNames);

        return infos;

        void AddModuleFunctions(string moduleName, IEnumerable<string> functionNames)
        {
            var module =
                parsedEnv.Modules
                .First(parsedModule => parsedModule.moduleName == moduleName)
                .moduleContent;

            foreach (var functionName in functionNames)
            {
                var record =
                    FunctionRecord.ParseFunctionRecordTagged(
                        module.FunctionDeclarations[functionName],
                        parseCache)
                    .Extract(
                        error => throw new Exception(
                            $"Failed parsing {moduleName}.{functionName} function record: {error}"));

                infos.Add(
                    (moduleName, functionName),
                    new(
                        ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction),
                        PineValue.List([.. record.EnvFunctions.ToArray()])));
            }
        }
    }

    /// <summary>Skips inline whitespace from the current string offset.</summary>
    public static PineValue? SkipInlineWhitespaceLeafDelegate(PineValue environment) =>
        ScanStringOffset(environment, "skipInlineWhitespace", codePoint => codePoint is ' ' or '\t');

    /// <summary>
    /// Advances the parser state over whitespace while keeping the source location.
    /// </summary>
    public static PineValue? SkipWhitespaceAtLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, FromStringModuleName, "skipWhitespaceAt") ||
            !TryGetStringBytes(environment.ValueFromPathOrEmptyList([1]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([3]), out var row) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([4]), out var column))
        {
            return null;
        }

        var scanned = ScanWhitespace(source, offset, row, column);

        return
            ParserStateValue(
                environment.ValueFromPathOrEmptyList([1]),
                scanned.Offset,
                scanned.Row,
                scanned.Column,
                environment.ValueFromPathOrEmptyList([5]));
    }

    /// <summary>Scans from the current offset to an identifier's end.</summary>
    public static PineValue? SkipToIdentifierEndLeafDelegate(PineValue environment) =>
        ScanStringOffset(
            environment,
            "skipToIdentifierEnd",
            codePoint =>
            codePoint is '_' ||
            codePoint is >= '0' and <= '9' ||
            codePoint is >= 'a' and <= 'z' ||
            codePoint is >= 'A' and <= 'Z');

    /// <summary>Scans from the current offset to an ASCII decimal number's end.</summary>
    public static PineValue? SkipToAsciiDecimalDigitEndLeafDelegate(PineValue environment) =>
        ScanStringOffset(
            environment,
            "skipToAsciiDecimalDigitEnd",
            codePoint => codePoint is >= '0' and <= '9');

    /// <summary>Scans from the current offset to an ASCII hexadecimal number's end.</summary>
    public static PineValue? SkipToAsciiHexDigitEndLeafDelegate(PineValue environment) =>
        ScanStringOffset(environment, "skipToAsciiHexDigitEnd", IsAsciiHexDigit);

    /// <summary>Scans a decimal number's fractional and exponent suffixes in one operation.</summary>
    public static PineValue? NumberEndDecimalLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, StringParsingModuleName, "numberEndDecimal") ||
            !TryGetStringBytes(environment.ValueFromPathOrEmptyList([1]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset))
        {
            return null;
        }

        var sourceCodePointCount = source.Length / 4;

        while (offset < sourceCodePointCount &&
            ReadStringCodePoint(source, (int)offset) is >= '0' and <= '9')
        {
            offset++;
        }

        if (offset + 1 < sourceCodePointCount &&
            ReadStringCodePoint(source, (int)offset) is '.' &&
            ReadStringCodePoint(source, (int)(offset + 1)) is >= '0' and <= '9')
        {
            offset += 2;

            while (offset < sourceCodePointCount &&
                ReadStringCodePoint(source, (int)offset) is >= '0' and <= '9')
            {
                offset++;
            }
        }

        if (offset < sourceCodePointCount &&
            ReadStringCodePoint(source, (int)offset) is 'e' or 'E')
        {
            offset++;

            if (offset < sourceCodePointCount &&
                ReadStringCodePoint(source, (int)offset) is '+' or '-')
            {
                offset++;
            }

            while (offset < sourceCodePointCount &&
                ReadStringCodePoint(source, (int)offset) is >= '0' and <= '9')
            {
                offset++;
            }
        }

        return IntegerValue(offset);
    }

    /// <summary>Tests a numeric literal for a decimal or exponent marker without allocating slices.</summary>
    public static PineValue? IsFloatLiteralAtLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, StringParsingModuleName, "isFloatLiteralAt") ||
            !TryGetStringBytes(environment.ValueFromPathOrEmptyList([1]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset))
        {
            return null;
        }

        var sourceCodePointCount = source.Length / 4;

        while (offset < sourceCodePointCount)
        {
            switch (ReadStringCodePoint(source, (int)offset))
            {
                case '.':
                case 'e':
                case 'E':
                    return s_true;

                default:
                    offset++;
                    break;
            }
        }

        return s_false;
    }

    /// <summary>Accumulates hexadecimal digits without creating a slice for every digit.</summary>
    public static PineValue? Convert0OrMoreHexadecimalValueLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(
                environment,
                StringParsingModuleName,
                "convert0OrMoreHexadecimalValue") ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([1]), out var value) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset) ||
            !TryGetStringBytes(environment.ValueFromPathOrEmptyList([3]), out var source))
        {
            return null;
        }

        var sourceCodePointCount = source.Length / 4;

        while (offset < sourceCodePointCount)
        {
            if (!TryHexDigitValue(ReadStringCodePoint(source, (int)offset), out var digit))
            {
                return s_nothing;
            }

            value = value * 16 + digit;
            offset++;
        }

        return Just(IntegerValue(value));
    }

    /// <summary>Skips operator characters from the current string offset.</summary>
    public static PineValue? SkipOperatorCharsLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, StringParsingModuleName, "skipOperatorChars") ||
            !TryGetStringBytes(environment.ValueFromPathOrEmptyList([1]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([3]), out var offsetMax))
        {
            return null;
        }

        var sourceCodePointCount = source.Length / 4;

        while (offset < offsetMax &&
            offset < sourceCodePointCount &&
            IsOperatorChar(ReadStringCodePoint(source, (int)offset)))
        {
            offset++;
        }

        return IntegerValue(offset);
    }

    /// <summary>Scans and decodes Unicode escape digits.</summary>
    public static PineValue? ScanUnicodeEscapeDigitsLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, StringParsingModuleName, "scanUnicodeEscapeDigits") ||
            !TryGetStringBytes(environment.ValueFromPathOrEmptyList([1]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset))
        {
            return null;
        }

        var sourceCodePointCount = source.Length / 4;

        if (offset >= sourceCodePointCount ||
            !TryHexDigitValue(ReadStringCodePoint(source, (int)offset), out var firstDigit))
        {
            return s_nothing;
        }

        BigInteger value = firstDigit;
        offset++;

        while (offset < sourceCodePointCount &&
            TryHexDigitValue(ReadStringCodePoint(source, (int)offset), out var digit))
        {
            value = value * 16 + digit;
            offset++;
        }

        return Just(PineValue.List([IntegerValue(offset), IntegerValue(value)]));
    }

    /// <summary>Finds the boundary ending a literal run.</summary>
    public static PineValue? FindLiteralRunEndLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, StringParsingModuleName, "findLiteralRunEnd") ||
            !TryParseLiteralTermination(
                environment.ValueFromPathOrEmptyList([1]),
                out var termination) ||
            !TryGetStringBytes(environment.ValueFromPathOrEmptyList([2]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([3]), out var offset))
        {
            return null;
        }

        var sourceCodePointCount = source.Length / 4;

        while (offset < sourceCodePointCount)
        {
            var index = (int)offset;
            var codePoint = ReadStringCodePoint(source, index);

            if ((termination is LiteralTermination.SingleQuote && codePoint is '\'') ||
                (termination is LiteralTermination.DoubleQuote && codePoint is '"') ||
                (termination is LiteralTermination.TripleQuote &&
                codePoint is '"' &&
                index + 2 < sourceCodePointCount &&
                ReadStringCodePoint(source, index + 1) is '"' &&
                ReadStringCodePoint(source, index + 2) is '"'))
            {
                return LiteralRunResult(offset, "LiteralRunTermination");
            }

            if (codePoint is '\\')
            {
                return LiteralRunResult(offset, "LiteralRunBackslash");
            }

            if (codePoint is '\n')
            {
                return LiteralRunResult(offset, "LiteralRunNewlineLF");
            }

            if (codePoint is '\r')
            {
                return
                    LiteralRunResult(
                        offset,
                        index + 1 < sourceCodePointCount &&
                        ReadStringCodePoint(source, index + 1) is '\n'
                        ?
                        "LiteralRunNewlineCRLF"
                        :
                        "LiteralRunNewlineCR");
            }

            offset++;
        }

        return LiteralRunResult(offset, "LiteralRunUnterminated");
    }

    private static PineValue? ScanStringOffset(
        PineValue environment,
        string functionName,
        Func<uint, bool> continuePredicate)
    {
        if (!EnvironmentMatches(environment, StringParsingModuleName, functionName) ||
            !TryGetStringBytes(environment.ValueFromPathOrEmptyList([1]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset))
        {
            return null;
        }

        var sourceCodePointCount = source.Length / 4;

        while (offset < sourceCodePointCount &&
            continuePredicate(ReadStringCodePoint(source, (int)offset)))
        {
            offset++;
        }

        return IntegerValue(offset);
    }

    private static TriviaScan ScanWhitespace(
        ReadOnlyMemory<byte> source,
        BigInteger offset,
        BigInteger row,
        BigInteger column)
    {
        var sourceCodePointCount = source.Length / 4;

        while (offset < sourceCodePointCount)
        {
            var index = (int)offset;

            switch (ReadStringCodePoint(source, index))
            {
                case ' ':
                case '\t':
                    offset++;
                    column++;
                    continue;

                case '\n':
                    offset++;
                    row++;
                    column = 1;
                    continue;

                case '\r':
                    offset +=
                        index + 1 < sourceCodePointCount &&
                        ReadStringCodePoint(source, index + 1) is '\n'
                        ?
                        2
                        :
                        1;

                    row++;
                    column = 1;
                    continue;

                default:
                    return new(offset, row, column);
            }
        }

        return new(offset, row, column);
    }

    private static bool TryGetLocation(
        PineValue value,
        out BigInteger row,
        out BigInteger column)
    {
        if (!TryGetRecordField(value, s_rowFieldName, out var rowValue) ||
            !TryGetRecordField(value, s_columnFieldName, out var columnValue) ||
            !TryParseNonnegativeInteger(rowValue, out row) ||
            !TryParseNonnegativeInteger(columnValue, out column))
        {
            row = 0;
            column = 0;
            return false;
        }

        return true;
    }

    private static bool TryGetRecordField(
        PineValue record,
        PineValue fieldName,
        out PineValue value)
    {
        if (record is PineValue.ListValue recordList &&
            recordList.Items.Length % 2 is 1 &&
            recordList.Items.Length >= 3 &&
            recordList.Items.Span[0] == ElmValue.ElmRecordTypeTagNameAsValue)
        {
            for (var index = 1; index + 1 < recordList.Items.Length; index += 2)
            {
                if (recordList.Items.Span[index] == fieldName)
                {
                    value = recordList.Items.Span[index + 1];
                    return true;
                }
            }
        }

        value = PineValue.EmptyList;
        return false;
    }

    private static bool TryGetStringBytes(PineValue value, out ReadOnlyMemory<byte> bytes)
    {
        if (value is PineValue.ListValue stringValue &&
            stringValue.Items.Length is 3 &&
            stringValue.Items.Span[0] == ElmValue.ElmChoiceTypeTagNameAsValue &&
            stringValue.Items.Span[1] == ElmValue.ElmStringTypeTagNameAsValue &&
            stringValue.Items.Span[2] is PineValue.BlobValue chars &&
            chars.Bytes.Length % 4 is 0)
        {
            bytes = chars.Bytes;
            return true;
        }

        bytes = ReadOnlyMemory<byte>.Empty;
        return false;
    }

    private static uint ReadStringCodePoint(ReadOnlyMemory<byte> bytes, int index) =>
        BinaryPrimitives.ReadUInt32BigEndian(bytes.Span.Slice(index * 4, 4));

    private static bool TryParseNonnegativeInteger(PineValue value, out BigInteger integer)
    {
        var parsed = IntegerEncoding.ParseSignedIntegerRelaxed(value).IsOkOrNullable();

        if (parsed is null || parsed < 0)
        {
            integer = 0;
            return false;
        }

        integer = parsed.Value;
        return true;
    }

    private static bool TryParseLiteralTermination(PineValue value, out LiteralTermination termination)
    {
        if (value == s_singleQuoteTermination)
        {
            termination = LiteralTermination.SingleQuote;
            return true;
        }

        if (value == s_doubleQuoteTermination)
        {
            termination = LiteralTermination.DoubleQuote;
            return true;
        }

        if (value == s_tripleQuoteTermination)
        {
            termination = LiteralTermination.TripleQuote;
            return true;
        }

        termination = default;
        return false;
    }

    private enum LiteralTermination
    {
        SingleQuote,
        DoubleQuote,
        TripleQuote,
    }

    private static bool IsAsciiHexDigit(uint codePoint) =>
        codePoint is >= '0' and <= '9' or >= 'a' and <= 'f' or >= 'A' and <= 'F';

    private static bool TryHexDigitValue(uint codePoint, out int value)
    {
        if (codePoint is >= '0' and <= '9')
        {
            value = (int)(codePoint - '0');
            return true;
        }

        if (codePoint is >= 'a' and <= 'f')
        {
            value = (int)(codePoint - 'a' + 10);
            return true;
        }

        if (codePoint is >= 'A' and <= 'F')
        {
            value = (int)(codePoint - 'A' + 10);
            return true;
        }

        value = 0;
        return false;
    }

    private static bool IsOperatorChar(uint codePoint) =>
        codePoint is '+' or '-' or '/' or '*' or '=' or '.' or '$' or '<' or '>' or ':' or '&' or '|' or '^' or '?' or '%' or '#' or '!';

    private static PineValue LiteralRunResult(BigInteger offset, string boundaryTag) =>
        PineValue.List([IntegerValue(offset), Tag(boundaryTag)]);

    private static PineValue ParserStateValue(
        PineValue sourceValue,
        BigInteger offset,
        BigInteger row,
        BigInteger column,
        PineValue commentsRevValue) =>
        PineValue.List(
            [
            ElmValue.ElmRecordTypeTagNameAsValue,
            s_columnFieldName,
            IntegerValue(column),
            s_commentsRevFieldName,
            commentsRevValue,
            s_offsetFieldName,
            IntegerValue(offset),
            s_rowFieldName,
            IntegerValue(row),
            s_sourceFieldName,
            sourceValue,
            ]);

    private static PineValue IntegerValue(BigInteger value) =>
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(value));

    private static PineValue Tag(string name) =>
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance(name, []));

    private static PineValue Just(PineValue value) =>
        ElmValueEncoding.TagAsPineValue("Just", [value]);

    private static PineValue LeafKey(string moduleName, string functionName) =>
        s_leafInfos.Value[(moduleName, functionName)].LeafKey;

    private static bool EnvironmentMatches(PineValue environment, string moduleName, string functionName) =>
        environment.ValueFromPathOrEmptyList([0]) ==
        s_leafInfos.Value[(moduleName, functionName)].EnvFunctionsValue;

    private static readonly PineValue s_false = Tag("False");

    private static readonly PineValue s_nothing = Tag("Nothing");

    private static readonly PineValue s_columnFieldName = StringEncoding.ValueFromString("column");

    private static readonly PineValue s_commentsRevFieldName = StringEncoding.ValueFromString("commentsRev");

    private static readonly PineValue s_offsetFieldName = StringEncoding.ValueFromString("offset");

    private static readonly PineValue s_rowFieldName = StringEncoding.ValueFromString("row");

    private static readonly PineValue s_sourceFieldName = StringEncoding.ValueFromString("source");

    private static readonly PineValue s_singleQuoteTermination = Tag("SingleQuoteTermination");

    private static readonly PineValue s_true = Tag("True");

    private static readonly PineValue s_doubleQuoteTermination = Tag("DoubleQuoteTermination");

    private static readonly PineValue s_tripleQuoteTermination = Tag("TripleQuoteTermination");

    /// <summary>Gets the default precompiled parser leaves by leaf key.</summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
            .Add(SkipInlineWhitespaceLeafKey, SkipInlineWhitespaceLeafDelegate)
            .Add(SkipWhitespaceAtLeafKey, SkipWhitespaceAtLeafDelegate)
            .Add(SkipToIdentifierEndLeafKey, SkipToIdentifierEndLeafDelegate)
            .Add(SkipToAsciiDecimalDigitEndLeafKey, SkipToAsciiDecimalDigitEndLeafDelegate)
            .Add(SkipToAsciiHexDigitEndLeafKey, SkipToAsciiHexDigitEndLeafDelegate)
            .Add(NumberEndDecimalLeafKey, NumberEndDecimalLeafDelegate)
            .Add(IsFloatLiteralAtLeafKey, IsFloatLiteralAtLeafDelegate)
            .Add(ScanUnicodeEscapeDigitsLeafKey, ScanUnicodeEscapeDigitsLeafDelegate)
            .Add(Convert0OrMoreHexadecimalValueLeafKey, Convert0OrMoreHexadecimalValueLeafDelegate)
            .Add(FindLiteralRunEndLeafKey, FindLiteralRunEndLeafDelegate)
            .Add(SkipOperatorCharsLeafKey, SkipOperatorCharsLeafDelegate));
}
