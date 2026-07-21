using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
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
        s_leafInfo.Value.leafKey;

    private static readonly Lazy<(PineValue leafKey, PineValue envFunctionsValue)> s_leafInfo =
        new(BuildLeafInfo);

    private static (PineValue leafKey, PineValue envFunctionsValue) BuildLeafInfo()
    {
        var compilerSources = BundledFiles.CompilerSourceContainerFilesDefault.Value;
        var mergedTree = BundledFiles.ElmKernelModulesDefault.Value;

        foreach (var sourcePath in new[]
        {
            new[] { "elm-syntax", "src" },
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

        var function =
            parsedEnv.Modules
            .First(module => module.moduleName is "LanguageService")
            .moduleContent.FunctionDeclarations["removeWrappingFromMultilineComment"];

        var record =
            FunctionRecord.ParseFunctionRecordTagged(function, new PineVMParseCache())
            .Extract(
                error => throw new Exception(
                    "Failed parsing LanguageService.removeWrappingFromMultilineComment " +
                    "function record to derive leaf info: " + error));

        return
            (CommonEncodings.ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction),
            PineValue.List([.. record.EnvFunctions.ToArray()]));
    }

    /// <summary>
    /// Executes <c>LanguageService.removeWrappingFromMultilineComment</c> directly,
    /// or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValue? RemoveWrappingFromMultilineCommentLeafDelegate(PineValue environment)
    {
        if (environment.ValueFromPathOrEmptyList([0]) != s_leafInfo.Value.envFunctionsValue ||
            environment.ValueFromPathOrEmptyList([1]) is not PineValue.ListValue stringValue ||
            stringValue.Items.Length is not 2 ||
            stringValue.Items.Span[0] != ElmValue.ElmStringTypeTagNameAsValue ||
            stringValue.Items.Span[1] is not PineValue.ListValue arguments ||
            arguments.Items.Length is not 1 ||
            arguments.Items.Span[0] is not PineValue.BlobValue chars ||
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

        return
            PineValue.List(
                [
                ElmValue.ElmStringTypeTagNameAsValue,
                PineValue.List([PineValue.Blob(unwrappedChars)]),
                ]);
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
}
