using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Precompiled leaves for functions in the <c>LanguageService</c> module.
/// </summary>
public static class LanguageServicePrecompiledLeaves
{
    /// <summary>
    /// Pine value key for the precompiled
    /// <c>LanguageService.removeWrappingFromMultilineComment</c> leaf.
    /// </summary>
    public static PineValue RemoveWrappingFromMultilineCommentLeafKey =>
        s_leafInfos.Value["removeWrappingFromMultilineComment"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>LanguageService.dropWhileEmpty</c> leaf.
    /// </summary>
    public static PineValue DropWhileEmptyLeafKey =>
        s_leafInfos.Value["dropWhileEmpty"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>LanguageService.sliceRangeFromTextLines</c> leaf.
    /// </summary>
    public static PineValue SliceRangeFromTextLinesLeafKey =>
        s_leafInfos.Value["sliceRangeFromTextLines"].leafKey;

    private static readonly Lazy<IReadOnlyDictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>>
        s_leafInfos =
        new(BuildLeafInfos);

    private static IReadOnlyDictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>
        BuildLeafInfos()
    {
        var compilerSources = BundledFiles.CompilerSourceContainerFilesDefault.Value;
        var mergedTree = BundledFiles.ElmKernelModulesDefault.Value;

        foreach (var sourcePath in new[]
        {
            new[] { "pine-elm-syntax", "src" },
            ["src"],
            ["other-library-modules"],
        })
        {
            if (compilerSources.GetNodeAtPath(sourcePath) is not { } sourceTree)
            {
                continue;
            }

            foreach (var (path, file) in sourceTree.EnumerateFilesTransitive())
            {
                mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
            }
        }

        var rootFilePaths =
            mergedTree.EnumerateFilesTransitive()
            .Where(file => file.path[^1].Equals("LanguageService.elm", StringComparison.OrdinalIgnoreCase))
            .Select(file => (IReadOnlyList<string>)file.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                mergedTree,
                rootFilePaths: rootFilePaths)
            .Map(result => result.compiledEnvValue)
            .Extract(
                error => throw new Exception(
                    "Failed compiling LanguageService module to derive leaf info: " + error));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(
                error => throw new Exception(
                    "Failed parsing LanguageService module to derive leaf info: " + error));

        var languageServiceModule =
            parsedEnv.Modules
            .First(module => module.moduleName is "LanguageService")
            .moduleContent;

        var parseCache = new PineVMParseCache();
        var infos = new Dictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>();

        foreach (var functionName in new[]
        {
            "removeWrappingFromMultilineComment",
            "dropWhileEmpty",
            "sliceRangeFromTextLines",
        })
        {
            var record =
                FunctionRecord.ParseFunctionRecordTagged(
                    languageServiceModule.FunctionDeclarations[functionName],
                    parseCache)
                .Extract(
                    error => throw new Exception(
                        $"Failed parsing LanguageService.{functionName} function record to derive leaf info: {error}"));

            infos.Add(
                functionName,
                (ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction),
                PineValue.List([.. record.EnvFunctions.ToArray()])));
        }

        return infos;
    }

    /// <summary>
    /// Executes <c>LanguageService.removeWrappingFromMultilineComment</c> directly,
    /// or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValueInProcess? RemoveWrappingFromMultilineCommentLeafDelegate(PineValueInProcess environment)
    {
        if (!EnvironmentMatches(environment, "removeWrappingFromMultilineComment") ||
            environment.ValueFromPathOrEmptyList([1]) is not PineValue.ListValue stringValue ||
            stringValue.Items.Length is not 3 ||
            stringValue.Items.Span[0] != ElmValue.ElmChoiceTypeTagNameAsValue ||
            stringValue.Items.Span[1] != ElmValue.ElmStringTypeTagNameAsValue ||
            stringValue.Items.Span[2] is not PineValue.BlobValue chars ||
            chars.Bytes.Length % 4 is not 0)
        {
            return null;
        }

        var charsSpan = chars.Bytes.Span;
        var (start, end) = TrimOffsets(charsSpan, 0, charsSpan.Length);

        if (CodePointEquals(charsSpan, start, '{') &&
            CodePointEquals(charsSpan, start + 4, '-'))
        {
            start +=
                CodePointEquals(charsSpan, start + 8, '|')
                ?
                12
                :
                8;
        }

        if (CodePointEquals(charsSpan, end - 8, '-') &&
            CodePointEquals(charsSpan, end - 4, '}'))
        {
            end -= 8;
        }

        end = Math.Max(start, end);
        (start, end) = TrimOffsets(charsSpan, start, end);

        var unwrappedChars = chars.Bytes.Slice(start, end - start);

        return StringValueInProcess(unwrappedChars);
    }

    /// <summary>
    /// Executes <c>LanguageService.dropWhileEmpty</c> directly,
    /// or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValueInProcess? DropWhileEmptyLeafDelegate(PineValueInProcess environment)
    {
        if (!EnvironmentMatches(environment, "dropWhileEmpty") ||
            PineValueInProcess.ValueInProcessFromPathOrNull(environment, [1]) is not { } lines ||
            lines.Evaluate() is not PineValue.ListValue linesValue)
        {
            return null;
        }

        for (var index = 0; index < linesValue.Items.Length; ++index)
        {
            if (!TryGetStringChars(linesValue.Items.Span[index], out var chars))
            {
                return null;
            }

            if (chars.Length is not 0)
            {
                return index is 0 ? lines : PineValueInProcess.Skip(index, lines);
            }
        }

        return PineValueInProcess.EmptyList;
    }

    /// <summary>
    /// Executes <c>LanguageService.sliceRangeFromTextLines</c> directly,
    /// or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValueInProcess? SliceRangeFromTextLinesLeafDelegate(PineValueInProcess environment)
    {
        if (!EnvironmentMatches(environment, "sliceRangeFromTextLines") ||
            PineValueInProcess.ValueInProcessFromPathOrNull(environment, [1]) is not { } textLines ||
            !textLines.IsList() ||
            !TryParseRange(
                environment.ValueFromPathOrEmptyList([2]),
                out var startRow,
                out var startColumn,
                out var endRow,
                out var endColumn) ||
            endRow < startRow)
        {
            return null;
        }

        var startRowIndex = startRow - 1;
        var startColumnIndex = startColumn - 1;
        var endColumnIndex = endColumn - 1;
        var rangeRowCount = endRow - startRow;

        if (rangeRowCount is 0)
        {
            if (startRowIndex >= textLines.GetLength())
            {
                return PineValueInProcess.EmptyList;
            }

            return
                TrySliceString(
                    textLines.GetElementAt(startRowIndex).Evaluate(),
                    startColumnIndex,
                    endColumnIndex,
                    out var sliced)
                ?
                PineValueInProcess.CreateList([PineValueInProcess.Create(sliced)])
                :
                null;
        }

        var firstLine =
            startRowIndex < textLines.GetLength()
            ?
            textLines.GetElementAt(startRowIndex).Evaluate()
            :
            s_emptyString;

        if (!TryDropStringLeft(firstLine, startColumnIndex, out firstLine))
        {
            return null;
        }

        var endRowIndex = endRow - 1;

        var lastLine =
            endRowIndex < textLines.GetLength()
            ?
            textLines.GetElementAt(endRowIndex).Evaluate()
            :
            s_emptyString;

        if (!TryTakeStringLeft(lastLine, endColumnIndex, out lastLine))
        {
            return null;
        }

        var middleLineCount =
            Math.Min(
                rangeRowCount - 1,
                Math.Max(0, textLines.GetLength() - startRowIndex - 1));

        var result = new PineValueInProcess[middleLineCount + 2];
        result[0] = PineValueInProcess.Create(firstLine);

        for (var index = 0; index < middleLineCount; ++index)
        {
            result[index + 1] = textLines.GetElementAt(startRowIndex + 1 + index);
        }

        result[^1] = PineValueInProcess.Create(lastLine);

        return PineValueInProcess.CreateList(result);
    }

    private static (int Start, int End) TrimOffsets(
        ReadOnlySpan<byte> chars,
        int start,
        int end)
    {
        while (start < end && IsTrimmedCharacter(chars[start..]))
        {
            start += 4;
        }

        while (start < end && IsTrimmedCharacter(chars[(end - 4)..]))
        {
            end -= 4;
        }

        return (start, end);
    }

    private static bool IsTrimmedCharacter(ReadOnlySpan<byte> charBytes)
    {
        var codePoint =
            System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(charBytes);

        return codePoint is ' ' or '\t' or '\n' or '\r' or 0x00A0;
    }

    private static bool CodePointEquals(
        ReadOnlySpan<byte> chars,
        int offset,
        uint expected) =>
        offset >= 0 &&
        offset <= chars.Length - 4 &&
        System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(chars[offset..]) == expected;

    private static bool EnvironmentMatches(PineValueInProcess environment, string functionName) =>
        PineValueInProcess.AreEqual(
            environment.GetElementAt(0),
            s_leafInfos.Value[functionName].envFunctionsValue);

    private static readonly PineValue s_rangeTagName =
        StringEncoding.ValueFromString("Range");

    private static readonly PineValue s_emptyString =
        StringValue(ReadOnlyMemory<byte>.Empty);

    private static bool TryParseRange(
        PineValue value,
        out int startRow,
        out int startColumn,
        out int endRow,
        out int endColumn)
    {
        if (value is PineValue.ListValue rangeValue &&
            rangeValue.Items.Length is 4 &&
            rangeValue.Items.Span[0] == ElmValue.ElmChoiceTypeTagNameAsValue &&
            rangeValue.Items.Span[1] == s_rangeTagName &&
            rangeValue.Items.Span[2] is PineValue.ListValue start &&
            start.Items.Length is 2 &&
            rangeValue.Items.Span[3] is PineValue.ListValue end &&
            end.Items.Length is 2 &&
            TryParsePositiveInt(start.Items.Span[0], out startRow) &&
            TryParsePositiveInt(start.Items.Span[1], out startColumn) &&
            TryParsePositiveInt(end.Items.Span[0], out endRow) &&
            TryParsePositiveInt(end.Items.Span[1], out endColumn))
        {
            return true;
        }

        startRow = 0;
        startColumn = 0;
        endRow = 0;
        endColumn = 0;
        return false;
    }

    private static bool TryParsePositiveInt(PineValue value, out int parsedInt)
    {
        var parsed = IntegerEncoding.ParseSignedIntegerRelaxed(value).IsOkOrNullable();

        if (parsed is null || parsed < 1 || parsed > int.MaxValue)
        {
            parsedInt = 0;
            return false;
        }

        parsedInt = (int)parsed;
        return true;
    }

    private static bool TryGetStringChars(
        PineValue value,
        out ReadOnlyMemory<byte> chars)
    {
        if (value is PineValue.ListValue stringValue &&
            stringValue.Items.Length is 3 &&
            stringValue.Items.Span[0] == ElmValue.ElmChoiceTypeTagNameAsValue &&
            stringValue.Items.Span[1] == ElmValue.ElmStringTypeTagNameAsValue &&
            stringValue.Items.Span[2] is PineValue.BlobValue charsBlob &&
            charsBlob.Bytes.Length % 4 is 0)
        {
            chars = charsBlob.Bytes;
            return true;
        }

        chars = default;
        return false;
    }

    private static bool TrySliceString(
        PineValue value,
        int start,
        int end,
        out PineValue sliced)
    {
        if (!TryGetStringChars(value, out var chars))
        {
            sliced = default!;
            return false;
        }

        var characterCount = chars.Length / 4;
        start = Math.Min(start, characterCount);
        end = Math.Min(end, characterCount);

        if (end <= start)
        {
            sliced = s_emptyString;
            return true;
        }

        if (start is 0 && end == characterCount)
        {
            sliced = value;
            return true;
        }

        sliced = StringValue(chars.Slice(start * 4, (end - start) * 4));
        return true;
    }

    private static bool TryDropStringLeft(
        PineValue value,
        int count,
        out PineValue dropped)
    {
        if (!TryGetStringChars(value, out var chars))
        {
            dropped = default!;
            return false;
        }

        var characterCount = chars.Length / 4;

        if (count is 0)
        {
            dropped = value;
            return true;
        }

        if (count >= characterCount)
        {
            dropped = s_emptyString;
            return true;
        }

        dropped = StringValue(chars[(count * 4)..]);
        return true;
    }

    private static bool TryTakeStringLeft(
        PineValue value,
        int count,
        out PineValue taken)
    {
        if (!TryGetStringChars(value, out var chars))
        {
            taken = default!;
            return false;
        }

        var characterCount = chars.Length / 4;

        if (count is 0)
        {
            taken = s_emptyString;
            return true;
        }

        if (count >= characterCount)
        {
            taken = value;
            return true;
        }

        taken = StringValue(chars[..(count * 4)]);
        return true;
    }

    private static PineValue StringValue(ReadOnlyMemory<byte> chars) =>
        ElmValueEncoding.TagAsPineValue(
            ElmValue.ElmStringTypeTagName,
            [PineValue.Blob(chars)]);

    private static PineValueInProcess StringValueInProcess(ReadOnlyMemory<byte> chars) =>
        ElmValueInProcess.CreateChoice(
            PineValueInProcess.Create(ElmValue.ElmStringTypeTagNameAsValue),
            [PineValueInProcess.Create(PineValue.Blob(chars))]);
}
