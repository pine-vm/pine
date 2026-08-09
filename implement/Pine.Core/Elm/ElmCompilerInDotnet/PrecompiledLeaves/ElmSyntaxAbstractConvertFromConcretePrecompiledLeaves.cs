using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Precompiled leaves for recursive helpers in <c>ElmSyntax.Abstract.ConvertFromConcrete</c>.
/// </summary>
public static class ElmSyntaxAbstractConvertFromConcretePrecompiledLeaves
{
    private const string ModuleName = "ElmSyntax.Abstract.ConvertFromConcrete";

    private const string MergeRecordSettersFunctionName = "mergeRecordSetters";

    /// <summary>Gets the leaf key for merging sorted record setters.</summary>
    public static PineValue MergeRecordSettersLeafKey =>
        s_leafInfo.Value.LeafKey;

    private static readonly Lazy<LeafInfo> s_leafInfo =
        new(BuildLeafInfo);

    private sealed record LeafInfo(PineValue LeafKey, PineValue EnvFunctionsValue);

    private static LeafInfo BuildLeafInfo()
    {
        var mergedTree = BundledFiles.ElmKernelModulesDefault.Value;
        var compilerSourceTree = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var elmSyntaxSourceTree =
            compilerSourceTree.GetNodeAtPath(["pine-elm-syntax", "src"])
            ?? throw new Exception("Did not find pine-elm-syntax/src");

        foreach (var (path, file) in elmSyntaxSourceTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        var rootFilePath =
            mergedTree.EnumerateFilesTransitive()
            .Single(
                file =>
                file.path[^1].Equals("ConvertFromConcrete.elm", StringComparison.OrdinalIgnoreCase) &&
                file.path.Contains("Abstract"));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(mergedTree, rootFilePaths: [rootFilePath.path])
            .Map(result => result.compiledEnvValue)
            .Extract(
                error => throw new Exception(
                    "Failed compiling ElmSyntax.Abstract.ConvertFromConcrete to derive leaf info: " + error));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(
                error => throw new Exception(
                    "Failed parsing ElmSyntax.Abstract.ConvertFromConcrete to derive leaf info: " + error));

        var module =
            parsedEnv.Modules
            .First(parsedModule => parsedModule.moduleName == ModuleName)
            .moduleContent;

        var record =
            FunctionRecord.ParseFunctionRecordTagged(
                module.FunctionDeclarations[MergeRecordSettersFunctionName],
                new PineVMParseCache())
            .Extract(
                error => throw new Exception(
                    $"Failed parsing {ModuleName}.{MergeRecordSettersFunctionName} function record: {error}"));

        return
            new(
                ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction),
                PineValue.List([.. record.EnvFunctions.ToArray()]));
    }

    /// <summary>
    /// Merges two lists of record setters sorted by field name.
    /// </summary>
    public static PineValue? MergeRecordSettersLeafDelegate(PineValue environment)
    {
        if (environment.ValueFromPathOrEmptyList([0]) != s_leafInfo.Value.EnvFunctionsValue ||
            environment.ValueFromPathOrEmptyList([1]) is not PineValue.ListValue left ||
            environment.ValueFromPathOrEmptyList([2]) is not PineValue.ListValue right)
        {
            return null;
        }

        if (left.Items.Length is 0)
        {
            return right;
        }

        if (right.Items.Length is 0)
        {
            return left;
        }

        var merged = new PineValue[left.Items.Length + right.Items.Length];
        var leftIndex = 0;
        var rightIndex = 0;
        var mergedIndex = 0;

        while (leftIndex < left.Items.Length && rightIndex < right.Items.Length)
        {
            var leftSetter = left.Items.Span[leftIndex];
            var rightSetter = right.Items.Span[rightIndex];

            if (!TryGetFieldNameBytes(leftSetter, out var leftFieldName) ||
                !TryGetFieldNameBytes(rightSetter, out var rightFieldName))
            {
                return null;
            }

            if (CompareUtf32Strings(leftFieldName.Span, rightFieldName.Span) <= 0)
            {
                merged[mergedIndex++] = leftSetter;
                leftIndex++;
            }
            else
            {
                merged[mergedIndex++] = rightSetter;
                rightIndex++;
            }
        }

        left.Items.Span[leftIndex..].CopyTo(merged.AsSpan(mergedIndex));
        mergedIndex += left.Items.Length - leftIndex;
        right.Items.Span[rightIndex..].CopyTo(merged.AsSpan(mergedIndex));

        return PineValue.List(merged);
    }

    private static bool TryGetFieldNameBytes(PineValue setter, out ReadOnlyMemory<byte> bytes)
    {
        if (setter is PineValue.ListValue setterRecord &&
            setterRecord.Items.Length >= 3 &&
            setterRecord.Items.Length % 2 is 1 &&
            setterRecord.Items.Span[0] == ElmValue.ElmRecordTypeTagNameAsValue)
        {
            for (var index = 1; index + 1 < setterRecord.Items.Length; index += 2)
            {
                if (setterRecord.Items.Span[index] != s_fieldNameField)
                {
                    continue;
                }

                if (setterRecord.Items.Span[index + 1] is PineValue.ListValue stringValue &&
                    stringValue.Items.Length is 2 &&
                    stringValue.Items.Span[0] == ElmValue.ElmStringTypeTagNameAsValue &&
                    stringValue.Items.Span[1] is PineValue.ListValue arguments &&
                    arguments.Items.Length is 1 &&
                    arguments.Items.Span[0] is PineValue.BlobValue characters &&
                    characters.Bytes.Length % 4 is 0)
                {
                    bytes = characters.Bytes;
                    return true;
                }

                break;
            }
        }

        bytes = ReadOnlyMemory<byte>.Empty;
        return false;
    }

    private static int CompareUtf32Strings(ReadOnlySpan<byte> left, ReadOnlySpan<byte> right)
    {
        var commonCharacterCount = Math.Min(left.Length, right.Length) / 4;

        for (var index = 0; index < commonCharacterCount; index++)
        {
            var offset = index * 4;
            var leftCodePoint = BinaryPrimitives.ReadUInt32BigEndian(left[offset..]);
            var rightCodePoint = BinaryPrimitives.ReadUInt32BigEndian(right[offset..]);

            if (leftCodePoint != rightCodePoint)
            {
                return leftCodePoint < rightCodePoint ? -1 : 1;
            }
        }

        return left.Length.CompareTo(right.Length);
    }

    private static readonly PineValue s_fieldNameField =
        StringEncoding.ValueFromString("fieldName");

    /// <summary>Gets the default precompiled ConvertFromConcrete leaves by leaf key.</summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
            .Add(MergeRecordSettersLeafKey, MergeRecordSettersLeafDelegate));
}
