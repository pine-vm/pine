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
/// Precompiled leaves for recursive helpers in <c>ElmSyntax.Concrete.Parser</c> modules.
/// </summary>
public static class ElmSyntaxConcreteParserPrecompiledLeaves
{
    private const string FromStringModuleName = "ElmSyntax.Concrete.Parser.FromString";
    private const string TokensFromStringModuleName = "ElmSyntax.Concrete.Parser.TokensFromString";

    private static readonly string[] s_tokensFromStringFunctionNames =
    [
        "skipInlineWhitespace",
        "skipToIdentifierEnd",
        "skipToAsciiDecimalDigitEnd",
        "skipToAsciiHexDigitEnd",
        "scanUnicodeEscapeDigits",
        "findLiteralRunEnd",
        "skipOperatorChars",
    ];

    private static readonly string[] s_fromStringFunctionNames =
    [
        "dropTrivia",
        "tokenLexemes",
        "hexStringToInt",
    ];

    /// <summary>Gets the leaf key for skipping inline whitespace.</summary>
    public static PineValue SkipInlineWhitespaceLeafKey => LeafKey(TokensFromStringModuleName, "skipInlineWhitespace");

    /// <summary>Gets the leaf key for scanning to an identifier's end.</summary>
    public static PineValue SkipToIdentifierEndLeafKey => LeafKey(TokensFromStringModuleName, "skipToIdentifierEnd");

    /// <summary>Gets the leaf key for scanning to an ASCII decimal number's end.</summary>
    public static PineValue SkipToAsciiDecimalDigitEndLeafKey =>
        LeafKey(TokensFromStringModuleName, "skipToAsciiDecimalDigitEnd");

    /// <summary>Gets the leaf key for scanning to an ASCII hexadecimal number's end.</summary>
    public static PineValue SkipToAsciiHexDigitEndLeafKey =>
        LeafKey(TokensFromStringModuleName, "skipToAsciiHexDigitEnd");

    /// <summary>Gets the leaf key for scanning Unicode escape digits.</summary>
    public static PineValue ScanUnicodeEscapeDigitsLeafKey =>
        LeafKey(TokensFromStringModuleName, "scanUnicodeEscapeDigits");

    /// <summary>Gets the leaf key for finding a literal run's end.</summary>
    public static PineValue FindLiteralRunEndLeafKey => LeafKey(TokensFromStringModuleName, "findLiteralRunEnd");

    /// <summary>Gets the leaf key for skipping operator characters.</summary>
    public static PineValue SkipOperatorCharsLeafKey => LeafKey(TokensFromStringModuleName, "skipOperatorChars");

    /// <summary>Gets the leaf key for dropping trivia.</summary>
    public static PineValue DropTriviaLeafKey => LeafKey(FromStringModuleName, "dropTrivia");

    /// <summary>Gets the leaf key for extracting token lexemes.</summary>
    public static PineValue TokenLexemesLeafKey => LeafKey(FromStringModuleName, "tokenLexemes");

    /// <summary>Gets the leaf key for parsing a hexadecimal string.</summary>
    public static PineValue HexStringToIntLeafKey => LeafKey(FromStringModuleName, "hexStringToInt");

    private static readonly Lazy<IReadOnlyDictionary<(string moduleName, string functionName), LeafInfo>> s_leafInfos =
        new(BuildLeafInfos);

    private sealed record LeafInfo(PineValue LeafKey, PineValue EnvFunctionsValue);

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
                file.path[^1].Equals("TokensFromString.elm", StringComparison.OrdinalIgnoreCase) ||
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

        AddModuleFunctions(TokensFromStringModuleName, s_tokensFromStringFunctionNames);
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

    /// <summary>Skips operator characters from the current string offset.</summary>
    public static PineValue? SkipOperatorCharsLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, TokensFromStringModuleName, "skipOperatorChars") ||
            !TryGetStringCodePoints(environment.ValueFromPathOrEmptyList([1]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([3]), out var offsetMax))
        {
            return null;
        }

        while (offset < offsetMax && offset < source.Length && IsOperatorChar(source[(int)offset]))
        {
            offset++;
        }

        return IntegerValue(offset);
    }

    /// <summary>Scans and decodes Unicode escape digits.</summary>
    public static PineValue? ScanUnicodeEscapeDigitsLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, TokensFromStringModuleName, "scanUnicodeEscapeDigits") ||
            !TryGetStringCodePoints(environment.ValueFromPathOrEmptyList([1]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset))
        {
            return null;
        }

        if (offset >= source.Length || !TryHexDigitValue(source[(int)offset], out var firstDigit))
        {
            return s_nothing;
        }

        BigInteger value = firstDigit;
        offset++;

        while (offset < source.Length && TryHexDigitValue(source[(int)offset], out var digit))
        {
            value = value * 16 + digit;
            offset++;
        }

        return Just(PineValue.List([IntegerValue(offset), IntegerValue(value)]));
    }

    /// <summary>Finds the boundary ending a literal run.</summary>
    public static PineValue? FindLiteralRunEndLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, TokensFromStringModuleName, "findLiteralRunEnd") ||
            !TryParseLiteralTermination(
                environment.ValueFromPathOrEmptyList([1]),
                out var termination) ||
            !TryGetStringCodePoints(environment.ValueFromPathOrEmptyList([2]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([3]), out var offset))
        {
            return null;
        }

        while (offset < source.Length)
        {
            var index = (int)offset;
            var codePoint = source[index];

            if ((termination is LiteralTermination.SingleQuote && codePoint is '\'') ||
                (termination is LiteralTermination.DoubleQuote && codePoint is '"') ||
                (termination is LiteralTermination.TripleQuote &&
                 codePoint is '"' &&
                 index + 2 < source.Length &&
                 source[index + 1] is '"' &&
                 source[index + 2] is '"'))
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
                return LiteralRunResult(
                    offset,
                    index + 1 < source.Length && source[index + 1] is '\n'
                    ? "LiteralRunNewlineCRLF"
                    : "LiteralRunNewlineCR");
            }

            offset++;
        }

        return LiteralRunResult(offset, "LiteralRunUnterminated");
    }

    /// <summary>Drops leading trivia tokens.</summary>
    public static PineValue? DropTriviaLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, FromStringModuleName, "dropTrivia") ||
            environment.ValueFromPathOrEmptyList([1]) is not PineValue.ListValue tokens)
        {
            return null;
        }

        var firstNonTrivia = 0;

        while (firstNonTrivia < tokens.Items.Length)
        {
            if (!TryGetRecordField(tokens.Items.Span[firstNonTrivia], s_tokenTypeFieldName, out var tokenType))
            {
                return null;
            }

            if (tokenType != s_commentTokenType)
            {
                break;
            }

            firstNonTrivia++;
        }

        return PineValue.List(tokens.Items[firstNonTrivia..]);
    }

    /// <summary>Extracts lexemes from tokens.</summary>
    public static PineValue? TokenLexemesLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, FromStringModuleName, "tokenLexemes") ||
            environment.ValueFromPathOrEmptyList([1]) is not PineValue.ListValue tokens)
        {
            return null;
        }

        var lexemes = new PineValue[tokens.Items.Length];

        for (var index = 0; index < tokens.Items.Length; ++index)
        {
            if (!TryGetRecordField(tokens.Items.Span[index], s_lexemeFieldName, out lexemes[index]))
            {
                return null;
            }
        }

        return PineValue.List(lexemes);
    }

    /// <summary>Parses a hexadecimal string as an integer.</summary>
    public static PineValue? HexStringToIntLeafDelegate(PineValue environment)
    {
        if (!EnvironmentMatches(environment, FromStringModuleName, "hexStringToInt") ||
            !TryGetStringCodePoints(environment.ValueFromPathOrEmptyList([1]), out var digits))
        {
            return null;
        }

        if (digits.Length is 0)
        {
            return s_nothing;
        }

        if (digits[0] is '0')
        {
            return Just(IntegerValue(0));
        }

        BigInteger value = 0;

        foreach (var codePoint in digits)
        {
            if (!TryHexDigitValue(codePoint, out var digit))
            {
                return s_nothing;
            }

            value = value * 16 + digit;
        }

        return Just(IntegerValue(value));
    }

    private static PineValue? ScanStringOffset(
        PineValue environment,
        string functionName,
        Func<uint, bool> continuePredicate)
    {
        if (!EnvironmentMatches(environment, TokensFromStringModuleName, functionName) ||
            !TryGetStringCodePoints(environment.ValueFromPathOrEmptyList([1]), out var source) ||
            !TryParseNonnegativeInteger(environment.ValueFromPathOrEmptyList([2]), out var offset))
        {
            return null;
        }

        while (offset < source.Length && continuePredicate(source[(int)offset]))
        {
            offset++;
        }

        return IntegerValue(offset);
    }

    private static bool TryGetStringCodePoints(PineValue value, out uint[] codePoints)
    {
        if (value is PineValue.ListValue stringValue &&
            stringValue.Items.Length is 2 &&
            stringValue.Items.Span[0] == ElmValue.ElmStringTypeTagNameAsValue &&
            stringValue.Items.Span[1] is PineValue.ListValue arguments &&
            arguments.Items.Length is 1 &&
            arguments.Items.Span[0] is PineValue.BlobValue chars &&
            chars.Bytes.Length % 4 is 0)
        {
            codePoints = new uint[chars.Bytes.Length / 4];

            for (var index = 0; index < codePoints.Length; ++index)
            {
                codePoints[index] = BinaryPrimitives.ReadUInt32BigEndian(chars.Bytes.Span[(index * 4)..]);
            }

            return true;
        }

        codePoints = [];
        return false;
    }

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

    private static bool TryGetRecordField(PineValue record, PineValue fieldName, out PineValue value)
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

    private static PineValue IntegerValue(BigInteger value) =>
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(value));

    private static PineValue Just(PineValue value) =>
        PineValue.List([StringEncoding.ValueFromString("Just"), PineValue.List([value])]);

    private static PineValue Tag(string name) =>
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance(name, []));

    private static PineValue LeafKey(string moduleName, string functionName) =>
        s_leafInfos.Value[(moduleName, functionName)].LeafKey;

    private static bool EnvironmentMatches(PineValue environment, string moduleName, string functionName) =>
        environment.ValueFromPathOrEmptyList([0]) ==
        s_leafInfos.Value[(moduleName, functionName)].EnvFunctionsValue;

    private static readonly PineValue s_nothing = Tag("Nothing");
    private static readonly PineValue s_singleQuoteTermination = Tag("SingleQuoteTermination");
    private static readonly PineValue s_doubleQuoteTermination = Tag("DoubleQuoteTermination");
    private static readonly PineValue s_tripleQuoteTermination = Tag("TripleQuoteTermination");
    private static readonly PineValue s_commentTokenType = Tag("Comment");
    private static readonly PineValue s_tokenTypeFieldName = StringEncoding.ValueFromString("tokenType");
    private static readonly PineValue s_lexemeFieldName = StringEncoding.ValueFromString("lexeme");

    /// <summary>Gets the default precompiled parser leaves by leaf key.</summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
            .Add(SkipInlineWhitespaceLeafKey, SkipInlineWhitespaceLeafDelegate)
            .Add(SkipToIdentifierEndLeafKey, SkipToIdentifierEndLeafDelegate)
            .Add(SkipToAsciiDecimalDigitEndLeafKey, SkipToAsciiDecimalDigitEndLeafDelegate)
            .Add(SkipToAsciiHexDigitEndLeafKey, SkipToAsciiHexDigitEndLeafDelegate)
            .Add(ScanUnicodeEscapeDigitsLeafKey, ScanUnicodeEscapeDigitsLeafDelegate)
            .Add(FindLiteralRunEndLeafKey, FindLiteralRunEndLeafDelegate)
            .Add(SkipOperatorCharsLeafKey, SkipOperatorCharsLeafDelegate)
            .Add(DropTriviaLeafKey, DropTriviaLeafDelegate)
            .Add(TokenLexemesLeafKey, TokenLexemesLeafDelegate)
            .Add(HexStringToIntLeafKey, HexStringToIntLeafDelegate));
}
