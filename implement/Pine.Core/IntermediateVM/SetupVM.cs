using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using System;
using System.Collections.Generic;
using System.IO;

namespace Pine.Core.IntermediateVM;

/// <summary>
/// Setup helpers for the <see cref="PineVM"/> that depend only on material
/// available inside the <c>Pine.Core</c> project.
/// </summary>
public static class SetupVM
{
    /// <summary>
    /// Prefix used for bundled declaration names that represent precompiled leaf values.
    /// </summary>
    public const string PrecompiledLeafValueNamePrefix = "precompiled-leaf/";

    private const string BasicsCompareName = PrecompiledLeafValueNamePrefix + "Basics/compare";

    private const string BasicsEqName = PrecompiledLeafValueNamePrefix + "Basics/eq";

    private const string BasicsIdivName = PrecompiledLeafValueNamePrefix + "Basics/idiv";

    private const string BasicsGcdName = PrecompiledLeafValueNamePrefix + "Basics/gcd";

    private const string DictGetName = PrecompiledLeafValueNamePrefix + "Dict/get";

    private const string DictToListName = PrecompiledLeafValueNamePrefix + "Dict/toList";

    private const string DictSizeName = PrecompiledLeafValueNamePrefix + "Dict/size";

    private const string DictKeysName = PrecompiledLeafValueNamePrefix + "Dict/keys";

    private const string DictValuesName = PrecompiledLeafValueNamePrefix + "Dict/values";

    private const string DictInsertName = PrecompiledLeafValueNamePrefix + "Dict/insert";

    private const string JsonDecodeParseValueName = PrecompiledLeafValueNamePrefix + "Json.Decode/parseValue";

    private const string StringToListRecursiveName = PrecompiledLeafValueNamePrefix + "String/toListRecursive";

    private const string StringSplitHelperOnBlobName = PrecompiledLeafValueNamePrefix + "String/splitHelperOnBlob";

    private const string StringLinesHelperName = PrecompiledLeafValueNamePrefix + "String/linesHelper";

    private const string StringToFloatName = PrecompiledLeafValueNamePrefix + "String/toFloat";

    private const string StringToIntName = PrecompiledLeafValueNamePrefix + "String/toInt";

    private const string StringFromIntName = PrecompiledLeafValueNamePrefix + "String/fromInt";

    private const string StringTrimLeftCountBytesTrimmedName =
        PrecompiledLeafValueNamePrefix + "String/trimLeftCountBytesTrimmed";

    private const string StringTrimRightCountBytesRemainingName =
        PrecompiledLeafValueNamePrefix + "String/trimRightCountBytesRemaining";

    private const string LanguageServiceRemoveWrappingFromMultilineCommentName =
        PrecompiledLeafValueNamePrefix + "LanguageService/removeWrappingFromMultilineComment";

    private const string LanguageServiceDropWhileEmptyName =
        PrecompiledLeafValueNamePrefix + "LanguageService/dropWhileEmpty";

    private const string LanguageServiceSliceRangeFromTextLinesName =
        PrecompiledLeafValueNamePrefix + "LanguageService/sliceRangeFromTextLines";

    private const string BytesDecodeBlobAsCharsRecName =
        PrecompiledLeafValueNamePrefix + "Bytes.Decode/decodeBlobAsCharsRec";

    private const string BytesEncodeCharsAsBlobHelpName =
        PrecompiledLeafValueNamePrefix + "Bytes.Encode/encodeCharsAsBlobHelp";

    private const string RecordAccessName = PrecompiledLeafValueNamePrefix + "Record/access";

    private const string RecordUpdateName = PrecompiledLeafValueNamePrefix + "Record/update";

    private const string Base64EncodeToBytesName = PrecompiledLeafValueNamePrefix + "Base64.Encode/toBytes";

    private const string Base64DecodeFromBytesName = PrecompiledLeafValueNamePrefix + "Base64.Decode/fromBytes";

    private const string ConcreteParserStringParsingPrefix =
        PrecompiledLeafValueNamePrefix + "ElmSyntax.Concrete.Parser.StringParsing/";

    private const string ConcreteParserFromStringPrefix =
        PrecompiledLeafValueNamePrefix + "ElmSyntax.Concrete.Parser.FromString/";

    private const string ConcreteParserSkipInlineWhitespaceName =
        ConcreteParserStringParsingPrefix + "skipInlineWhitespace";

    private const string ConcreteParserSkipWhitespaceAtName =
        ConcreteParserFromStringPrefix + "skipWhitespaceAt";

    private const string ConcreteParserSkipToIdentifierEndName =
        ConcreteParserStringParsingPrefix + "skipToIdentifierEnd";

    private const string ConcreteParserSkipToAsciiDecimalDigitEndName =
        ConcreteParserStringParsingPrefix + "skipToAsciiDecimalDigitEnd";

    private const string ConcreteParserSkipToAsciiHexDigitEndName =
        ConcreteParserStringParsingPrefix + "skipToAsciiHexDigitEnd";

    private const string ConcreteParserNumberEndDecimalName =
        ConcreteParserStringParsingPrefix + "numberEndDecimal";

    private const string ConcreteParserIsFloatLiteralAtName =
        ConcreteParserStringParsingPrefix + "isFloatLiteralAt";

    private const string ConcreteParserConvert0OrMoreHexadecimalValueName =
        ConcreteParserStringParsingPrefix + "convert0OrMoreHexadecimalValue";

    private const string ConcreteParserScanUnicodeEscapeDigitsName =
        ConcreteParserStringParsingPrefix + "scanUnicodeEscapeDigits";

    private const string ConcreteParserFindLiteralRunEndName =
        ConcreteParserStringParsingPrefix + "findLiteralRunEnd";

    private const string ConcreteParserSkipOperatorCharsName =
        ConcreteParserStringParsingPrefix + "skipOperatorChars";

    private const string AbstractConvertFromConcretePrefix =
        PrecompiledLeafValueNamePrefix + "ElmSyntax.Abstract.ConvertFromConcrete/";

    private const string AbstractConvertFromConcreteMergeRecordSettersName =
        AbstractConvertFromConcretePrefix + "mergeRecordSetters";

    /// <summary>
    /// Stable names mapped to the C# implementations of all default precompiled leaves.
    /// </summary>
    public static IReadOnlyDictionary<string, Func<PineValue, PineValue?>> DefaultPrecompiledLeafFunctionsByName { get; } =
        new Dictionary<string, Func<PineValue, PineValue?>>
        {
            [BasicsCompareName] = CoreBasicsPrecompiledLeaves.CompareLeafDelegate,
            [BasicsEqName] = CoreBasicsPrecompiledLeaves.EqLeafDelegate,
            [BasicsIdivName] = CoreBasicsPrecompiledLeaves.IdivLeafDelegate,
            [BasicsGcdName] = CoreBasicsPrecompiledLeaves.GcdLeafDelegate,
            [DictGetName] = CoreDictPrecompiledLeaves.DictGetLeafDelegate,
            [DictToListName] = CoreDictPrecompiledLeaves.DictToListLeafDelegate,
            [DictSizeName] = CoreDictPrecompiledLeaves.DictSizeLeafDelegate,
            [DictKeysName] = CoreDictPrecompiledLeaves.DictKeysLeafDelegate,
            [DictValuesName] = CoreDictPrecompiledLeaves.DictValuesLeafDelegate,
            [DictInsertName] = CoreDictPrecompiledLeaves.DictInsertLeafDelegate,
            [JsonDecodeParseValueName] = KernelJsonDecodePrecompiledLeaves.ParseValueLeafDelegate,
            [StringToListRecursiveName] = CoreStringPrecompiledLeaves.ToListRecursiveLeafDelegate,
            [StringSplitHelperOnBlobName] = CoreStringPrecompiledLeaves.SplitHelperOnBlobLeafDelegate,
            [StringLinesHelperName] = CoreStringPrecompiledLeaves.LinesHelperLeafDelegate,
            [StringToFloatName] = CoreStringPrecompiledLeaves.ToFloatLeafDelegate,
            [StringToIntName] = CoreStringPrecompiledLeaves.ToIntLeafDelegate,
            [StringFromIntName] = CoreStringPrecompiledLeaves.FromIntLeafDelegate,
            [StringTrimLeftCountBytesTrimmedName] =
            CoreStringPrecompiledLeaves.TrimLeftCountBytesTrimmedLeafDelegate,
            [StringTrimRightCountBytesRemainingName] =
            CoreStringPrecompiledLeaves.TrimRightCountBytesRemainingLeafDelegate,
            [LanguageServiceRemoveWrappingFromMultilineCommentName] =
            Elm.ElmCompilerInDotnet.PrecompiledLeaves.LanguageServicePrecompiledLeaves
            .RemoveWrappingFromMultilineCommentLeafDelegate,
            [LanguageServiceDropWhileEmptyName] =
            Elm.ElmCompilerInDotnet.PrecompiledLeaves.LanguageServicePrecompiledLeaves
            .DropWhileEmptyLeafDelegate,
            [LanguageServiceSliceRangeFromTextLinesName] =
            Elm.ElmCompilerInDotnet.PrecompiledLeaves.LanguageServicePrecompiledLeaves
            .SliceRangeFromTextLinesLeafDelegate,
            [BytesDecodeBlobAsCharsRecName] = KernelBytesPrecompiledLeaves.DecodeBlobAsCharsRecLeafDelegate,
            [BytesEncodeCharsAsBlobHelpName] = KernelBytesPrecompiledLeaves.EncodeCharsAsBlobHelpLeafDelegate,
            [RecordAccessName] = CoreRecordPrecompiledLeaves.RecordAccessLeafDelegate,
            [RecordUpdateName] = CoreRecordPrecompiledLeaves.RecordUpdateLeafDelegate,
            [Base64EncodeToBytesName] = Base64PrecompiledLeaves.EncodeToBytesLeafDelegate,
            [Base64DecodeFromBytesName] = Base64PrecompiledLeaves.DecodeFromBytesLeafDelegate,
            [ConcreteParserSkipInlineWhitespaceName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipInlineWhitespaceLeafDelegate,
            [ConcreteParserSkipWhitespaceAtName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipWhitespaceAtLeafDelegate,
            [ConcreteParserSkipToIdentifierEndName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToIdentifierEndLeafDelegate,
            [ConcreteParserSkipToAsciiDecimalDigitEndName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiDecimalDigitEndLeafDelegate,
            [ConcreteParserSkipToAsciiHexDigitEndName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiHexDigitEndLeafDelegate,
            [ConcreteParserNumberEndDecimalName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.NumberEndDecimalLeafDelegate,
            [ConcreteParserIsFloatLiteralAtName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.IsFloatLiteralAtLeafDelegate,
            [ConcreteParserConvert0OrMoreHexadecimalValueName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.Convert0OrMoreHexadecimalValueLeafDelegate,
            [ConcreteParserScanUnicodeEscapeDigitsName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.ScanUnicodeEscapeDigitsLeafDelegate,
            [ConcreteParserFindLiteralRunEndName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.FindLiteralRunEndLeafDelegate,
            [ConcreteParserSkipOperatorCharsName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipOperatorCharsLeafDelegate,
            [AbstractConvertFromConcreteMergeRecordSettersName] =
            ElmSyntaxAbstractConvertFromConcretePrecompiledLeaves.MergeRecordSettersLeafDelegate,
        };

    /// <summary>
    /// Freshly derives every named Pine value used as a default precompiled-leaf key.
    /// This is intentionally expensive and is used by prebuild and bundle verification.
    /// </summary>
    public static IReadOnlyDictionary<string, PineValue> BuildDefaultPrecompiledLeafValues() =>
        new Dictionary<string, PineValue>
        {
            [BasicsCompareName] = CoreBasicsPrecompiledLeaves.CompareLeafKey,
            [BasicsEqName] = CoreBasicsPrecompiledLeaves.EqLeafKey,
            [BasicsIdivName] = CoreBasicsPrecompiledLeaves.IdivLeafKey,
            [BasicsGcdName] = CoreBasicsPrecompiledLeaves.GcdLeafKey,
            [DictGetName] = CoreDictPrecompiledLeaves.DictGetLeafKey,
            [DictToListName] = CoreDictPrecompiledLeaves.DictToListLeafKey,
            [DictSizeName] = CoreDictPrecompiledLeaves.DictSizeLeafKey,
            [DictKeysName] = CoreDictPrecompiledLeaves.DictKeysLeafKey,
            [DictValuesName] = CoreDictPrecompiledLeaves.DictValuesLeafKey,
            [DictInsertName] = CoreDictPrecompiledLeaves.DictInsertLeafKey,
            [JsonDecodeParseValueName] = KernelJsonDecodePrecompiledLeaves.ParseValueLeafKey,
            [StringToListRecursiveName] = CoreStringPrecompiledLeaves.ToListRecursiveLeafKey,
            [StringSplitHelperOnBlobName] = CoreStringPrecompiledLeaves.SplitHelperOnBlobLeafKey,
            [StringLinesHelperName] = CoreStringPrecompiledLeaves.LinesHelperLeafKey,
            [StringToFloatName] = CoreStringPrecompiledLeaves.ToFloatLeafKey,
            [StringToIntName] = CoreStringPrecompiledLeaves.ToIntLeafKey,
            [StringFromIntName] = CoreStringPrecompiledLeaves.FromIntLeafKey,
            [StringTrimLeftCountBytesTrimmedName] =
            CoreStringPrecompiledLeaves.TrimLeftCountBytesTrimmedLeafKey,
            [StringTrimRightCountBytesRemainingName] =
            CoreStringPrecompiledLeaves.TrimRightCountBytesRemainingLeafKey,
            [LanguageServiceRemoveWrappingFromMultilineCommentName] =
            Elm.ElmCompilerInDotnet.PrecompiledLeaves.LanguageServicePrecompiledLeaves
            .RemoveWrappingFromMultilineCommentLeafKey,
            [LanguageServiceDropWhileEmptyName] =
            Elm.ElmCompilerInDotnet.PrecompiledLeaves.LanguageServicePrecompiledLeaves
            .DropWhileEmptyLeafKey,
            [LanguageServiceSliceRangeFromTextLinesName] =
            Elm.ElmCompilerInDotnet.PrecompiledLeaves.LanguageServicePrecompiledLeaves
            .SliceRangeFromTextLinesLeafKey,
            [BytesDecodeBlobAsCharsRecName] = KernelBytesPrecompiledLeaves.DecodeBlobAsCharsRecLeafKey,
            [BytesEncodeCharsAsBlobHelpName] = KernelBytesPrecompiledLeaves.EncodeCharsAsBlobHelpLeafKey,
            [RecordAccessName] = CoreRecordPrecompiledLeaves.RecordAccessLeafKey,
            [RecordUpdateName] = CoreRecordPrecompiledLeaves.RecordUpdateLeafKey,
            [Base64EncodeToBytesName] = Base64PrecompiledLeaves.EncodeToBytesLeafKey,
            [Base64DecodeFromBytesName] = Base64PrecompiledLeaves.DecodeFromBytesLeafKey,
            [ConcreteParserSkipInlineWhitespaceName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipInlineWhitespaceLeafKey,
            [ConcreteParserSkipWhitespaceAtName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipWhitespaceAtLeafKey,
            [ConcreteParserSkipToIdentifierEndName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToIdentifierEndLeafKey,
            [ConcreteParserSkipToAsciiDecimalDigitEndName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiDecimalDigitEndLeafKey,
            [ConcreteParserSkipToAsciiHexDigitEndName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiHexDigitEndLeafKey,
            [ConcreteParserNumberEndDecimalName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.NumberEndDecimalLeafKey,
            [ConcreteParserIsFloatLiteralAtName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.IsFloatLiteralAtLeafKey,
            [ConcreteParserConvert0OrMoreHexadecimalValueName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.Convert0OrMoreHexadecimalValueLeafKey,
            [ConcreteParserScanUnicodeEscapeDigitsName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.ScanUnicodeEscapeDigitsLeafKey,
            [ConcreteParserFindLiteralRunEndName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.FindLiteralRunEndLeafKey,
            [ConcreteParserSkipOperatorCharsName] =
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipOperatorCharsLeafKey,
            [AbstractConvertFromConcreteMergeRecordSettersName] =
            ElmSyntaxAbstractConvertFromConcretePrecompiledLeaves.MergeRecordSettersLeafKey,
        };

    /// <summary>
    /// Loads every named precompiled-leaf key from bundled declarations.
    /// </summary>
    public static IReadOnlyDictionary<string, PineValue> LoadDefaultPrecompiledLeafValues(
        BundledDeclarations bundledDeclarations)
    {
        var loaded = new Dictionary<string, PineValue>();

        foreach (var name in DefaultPrecompiledLeafFunctionsByName.Keys)
        {
            if (!bundledDeclarations.EmbeddedDeclarations.TryGetValue(name, out var value))
            {
                throw new InvalidDataException("Bundled declarations do not contain " + name);
            }

            loaded.Add(name, value);
        }

        return loaded;
    }

    /// <summary>
    /// Loads every named precompiled-leaf key from the resource bundled in Pine.Core.
    /// </summary>
    public static IReadOnlyDictionary<string, PineValue> LoadDefaultPrecompiledLeafValuesFromBundledResource() =>
        LoadDefaultPrecompiledLeafValues(
            ReusedInstances.Instance.BundledDeclarations
            ?? throw new InvalidDataException("Pine.Core does not contain bundled declarations"));

    /// <summary>
    /// Combines named key values with their named C# implementations.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> BuildDefaultPrecompiledLeaves(
        IReadOnlyDictionary<string, PineValue> valuesByName)
    {
        var leaves = new Dictionary<PineValue, Func<PineValue, PineValue?>>();

        foreach (var (name, function) in DefaultPrecompiledLeafFunctionsByName)
        {
            if (!valuesByName.TryGetValue(name, out var key))
            {
                throw new InvalidDataException("Precompiled-leaf values do not contain " + name);
            }

            leaves.TryAdd(key, function);
        }

        return leaves;
    }

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys from the Basics module.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> BasicsPrecompiledLeaves =>
        s_basicsPrecompiledLeaves.Value;

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys from the Dict module.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DictPrecompiledLeaves =>
        s_dictPrecompiledLeaves.Value;

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys from Json.Decode.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> JsonDecodePrecompiledLeaves =>
        s_jsonDecodePrecompiledLeaves.Value;

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys from the String module.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> StringPrecompiledLeaves =>
        s_stringPrecompiledLeaves.Value;

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys used by the Elm language service helpers.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> LanguageServicePrecompiledLeaves =>
        s_languageServicePrecompiledLeaves.Value;

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys from the Bytes helpers.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> BytesPrecompiledLeaves =>
        s_bytesPrecompiledLeaves.Value;

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys that implement record access and update.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> RecordAccessAndUpdatePrecompiledLeaves =>
        s_recordPrecompiledLeaves.Value;

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys used for Base64 conversion.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> Base64ConversionPrecompiledLeaves =>
        s_base64PrecompiledLeaves.Value;

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys from the concrete Elm parser.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> ConcreteParserPrecompiledLeaves =>
        s_concreteParserPrecompiledLeaves.Value;

    /// <summary>
    /// Gets the native implementations for the precompiled leaf keys that convert concrete Elm syntax into abstract form.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> ConvertFromConcretePrecompiledLeaves =>
        s_convertFromConcretePrecompiledLeaves.Value;

    /// <summary>
    /// Gets the combined map of all default precompiled leaf keys to their native implementations.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultPrecompiledLeaves =>
        s_defaultPrecompiledLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultPrecompiledLeaves =
        new(() => BuildDefaultPrecompiledLeaves(s_defaultPrecompiledLeafValues.Value));

    private static readonly Lazy<IReadOnlyDictionary<string, PineValue>> s_defaultPrecompiledLeafValues =
        new(
            () =>
            {
                try
                {
                    return LoadDefaultPrecompiledLeafValuesFromBundledResource();
                }
                catch (InvalidDataException exception)
                {
                    Console.WriteLine(
                        "Failed loading precompiled-leaf keys from the bundled resource; building them fresh: " +
                        exception.Message);

                    return BuildDefaultPrecompiledLeafValues();
                }
            });

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_basicsPrecompiledLeaves =
        new(() => BuildPrecompiledLeavesForNames(BasicsCompareName, BasicsEqName, BasicsIdivName, BasicsGcdName));

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_dictPrecompiledLeaves =
        new(
            () =>
            BuildPrecompiledLeavesForNames(
                DictGetName,
                DictToListName,
                DictSizeName,
                DictKeysName,
                DictValuesName,
                DictInsertName));

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_jsonDecodePrecompiledLeaves =
        new(() => BuildPrecompiledLeavesForNames(JsonDecodeParseValueName));

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_stringPrecompiledLeaves =
        new(
            () =>
            BuildPrecompiledLeavesForNames(
                StringToListRecursiveName,
                StringSplitHelperOnBlobName,
                StringLinesHelperName,
                StringToFloatName,
                StringToIntName,
                StringFromIntName,
                StringTrimLeftCountBytesTrimmedName,
                StringTrimRightCountBytesRemainingName));

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>>
        s_languageServicePrecompiledLeaves =
        new(
            () =>
            BuildPrecompiledLeavesForNames(
                LanguageServiceRemoveWrappingFromMultilineCommentName,
                LanguageServiceDropWhileEmptyName,
                LanguageServiceSliceRangeFromTextLinesName));

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_bytesPrecompiledLeaves =
        new(
            () =>
            BuildPrecompiledLeavesForNames(
                BytesDecodeBlobAsCharsRecName,
                BytesEncodeCharsAsBlobHelpName));

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_recordPrecompiledLeaves =
        new(() => BuildPrecompiledLeavesForNames(RecordAccessName, RecordUpdateName));

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_base64PrecompiledLeaves =
        new(() => BuildPrecompiledLeavesForNames(Base64EncodeToBytesName, Base64DecodeFromBytesName));

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>>
        s_concreteParserPrecompiledLeaves =
        new(
            () =>
            BuildPrecompiledLeavesForNames(
                ConcreteParserSkipInlineWhitespaceName,
                ConcreteParserSkipWhitespaceAtName,
                ConcreteParserSkipToIdentifierEndName,
                ConcreteParserSkipToAsciiDecimalDigitEndName,
                ConcreteParserSkipToAsciiHexDigitEndName,
                ConcreteParserNumberEndDecimalName,
                ConcreteParserIsFloatLiteralAtName,
                ConcreteParserConvert0OrMoreHexadecimalValueName,
                ConcreteParserScanUnicodeEscapeDigitsName,
                ConcreteParserFindLiteralRunEndName,
                ConcreteParserSkipOperatorCharsName));

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>>
        s_convertFromConcretePrecompiledLeaves =
        new(
            () =>
            BuildPrecompiledLeavesForNames(
                AbstractConvertFromConcreteMergeRecordSettersName));

    private static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> BuildPrecompiledLeavesForNames(
        params string[] names)
    {
        var leaves = new Dictionary<PineValue, Func<PineValue, PineValue?>>();
        var valuesByName = s_defaultPrecompiledLeafValues.Value;

        foreach (var name in names)
        {
            leaves.Add(valuesByName[name], DefaultPrecompiledLeafFunctionsByName[name]);
        }

        return leaves;
    }
}
