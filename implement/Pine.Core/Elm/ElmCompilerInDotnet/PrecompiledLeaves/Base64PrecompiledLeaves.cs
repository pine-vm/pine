using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Precompiled leaves (short-circuit .NET implementations) for inner bodies of the
/// <see href="https://github.com/danfishgold/base64-bytes/tree/ee966331d3819f56244145ed485ab13b0dc4f45a">
/// danfishgold/base64-bytes</see> functions <c>Base64.Encode.toBytes</c> and
/// <c>Base64.Decode.fromBytes</c>.
/// <para>
/// Each entry maps the Pine value encoding of a function's innermost
/// <c>ParseAndEval</c>-target expression to a delegate that returns the result directly,
/// bypassing the intermediate VM's interpretation of the recursive base64 chunking loop.
/// </para>
/// <para>
/// Because the inner body and captured <c>envFunctions</c> value of each function are
/// determined by the output of the Elm compiler, they are derived at runtime (lazily,
/// on first access) by compiling the <c>Base64</c> modules — bundled with
/// <c>Pine.Core</c> via <see cref="BundledFiles.CompilerSourceContainerFilesDefault"/>
/// — and parsing their function records.
/// </para>
/// </summary>
public static class Base64PrecompiledLeaves
{
    public static PineValue EncodeToBytesLeafKey =>
        s_leafInfos.Value["encode"].leafKey;

    public static PineValue DecodeFromBytesLeafKey =>
        s_leafInfos.Value["decode"].leafKey;

    /// <summary>
    /// Default precompiled-leaves dictionary contributed by the <c>Base64</c> modules
    /// (<c>Base64.Encode.toBytes</c> and <c>Base64.Decode.fromBytes</c>). Suitable for
    /// merging into the dictionary consumed by the intermediate VM.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultLeaves =
        new(BuildDefaultLeaves);

    private static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> BuildDefaultLeaves()
    {
        return
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
            .Add(EncodeToBytesLeafKey, EncodeToBytesLeafDelegate)
            .Add(DecodeFromBytesLeafKey, DecodeFromBytesLeafDelegate);
    }

    // ---------- compilation + environment ----------

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_parsedBase64Env =
        new(ParseBase64Environment);

    private static readonly Lazy<IReadOnlyDictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>>
        s_leafInfos =
        new(
            () =>
            new Dictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>
            {
                ["encode"] = DeriveLeafInfo(s_parsedBase64Env.Value, "Base64.Encode", "toBytes"),
                ["decode"] = DeriveLeafInfo(s_parsedBase64Env.Value, "Base64.Decode", "fromBytes"),
            });

    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment ParseBase64Environment()
    {
        var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

        var srcTree =
            BundledFiles.CompilerSourceContainerFilesDefault.Value
            .GetNodeAtPath(["src"])
            ?? throw new Exception("Did not find compiler source 'src' directory to derive Base64 leaf info");

        var base64File = srcTree.GetNodeAtPath(["Base64.elm"]);
        var base64DecodeFile = srcTree.GetNodeAtPath(["Base64", "Decode.elm"]);
        var base64EncodeFile = srcTree.GetNodeAtPath(["Base64", "Encode.elm"]);

        var treeWithBase64 = kernelModulesTree;

        if (base64File is not null)
            treeWithBase64 = treeWithBase64.SetNodeAtPathSorted(["Base64.elm"], base64File);

        if (base64DecodeFile is not null)
            treeWithBase64 = treeWithBase64.SetNodeAtPathSorted(["Base64", "Decode.elm"], base64DecodeFile);

        if (base64EncodeFile is not null)
            treeWithBase64 = treeWithBase64.SetNodeAtPathSorted(["Base64", "Encode.elm"], base64EncodeFile);

        var rootFilePaths =
            treeWithBase64.EnumerateFilesTransitive()
            .Where(
                b =>
                (b.path.Count is 1 && b.path[^1].Equals("Base64.elm", StringComparison.OrdinalIgnoreCase)) ||
                (b.path.Count is 2 && b.path[0].Equals("Base64", StringComparison.OrdinalIgnoreCase)))
            .Select(b => (IReadOnlyList<string>)b.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                treeWithBase64,
                rootFilePaths: rootFilePaths)
            .Map(r => r.compiledEnvValue)
            .Extract(
                err => throw new Exception("Failed compiling Base64 modules to derive leaf info: " + err));

        return
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(
                err => throw new Exception("Failed parsing Base64 modules to derive leaf info: " + err));
    }

    /// <summary>
    /// Derives the precompiled-leaf key and the captured <c>envFunctions</c> value
    /// for the named single-parameter function by parsing its compiled function
    /// record. The leaf key is the Pine value encoding of the function's inner
    /// body — the expression the intermediate VM dispatches to via
    /// <c>ParseAndEval</c> — and the env-functions value is used to gate the leaf
    /// so that only invocations of this specific function are short-circuited.
    /// </summary>
    private static (PineValue leafKey, PineValue envFunctionsValue) DeriveLeafInfo(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv,
        string moduleName,
        string functionName)
    {
        var functionValue =
            parsedEnv.Modules
            .First(m => m.moduleName == moduleName)
            .moduleContent.FunctionDeclarations[functionName];

        var record =
            FunctionRecord.ParseFunctionRecordTagged(functionValue, new PineVMParseCache())
            .Extract(
                err => throw new Exception(
                    "Failed parsing " + moduleName + "." + functionName + " function record: " + err));

        var leafKey =
            ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction);

        var envFunctionsValue =
            PineValue.List([.. record.EnvFunctions.ToArray()]);

        return (leafKey, envFunctionsValue);
    }

    // ---------- leaf delegates ----------

    private static readonly PineValue s_tagNothingValue =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("Nothing", []));

    private static readonly PineValue s_tagJustNameValue =
        StringEncoding.ValueFromString("Just");

    private static PineValue JustValue(PineValue inner) =>
        PineValue.List([s_tagJustNameValue, PineValue.List([inner])]);

    /// <summary>
    /// Short-circuit .NET implementation of <c>Base64.Encode.toBytes</c>
    /// (<c>String -&gt; Maybe Bytes</c>): decodes the base64 string argument into
    /// raw bytes, returning the corresponding <c>Maybe Bytes</c> value (or
    /// <c>Nothing</c> for an invalid base64 string), without interpreting the
    /// recursive chunking loop.
    /// </summary>
    public static PineValue? EncodeToBytesLeafDelegate(PineValue environment)
    {
        var envFunctionsValue = s_leafInfos.Value["encode"].envFunctionsValue;

        if (environment.ValueFromPathOrEmptyList([0]) != envFunctionsValue)
        {
            return null;
        }

        var stringArg = environment.ValueFromPathOrEmptyList([1]);

        if (stringArg.ValueFromPathOrEmptyList([0]) != ElmValue.ElmStringTypeTagNameAsValue)
        {
            return null;
        }

        if (stringArg.ValueFromPathOrEmptyList([1, 0]) is not PineValue.BlobValue stringCharsBlob)
        {
            return null;
        }

        if (StringEncoding.StringFromValue(stringCharsBlob).IsOkOrNull() is not { } dotnetString)
        {
            return null;
        }

        var dotnetBytesBuffer = new byte[dotnetString.Length];

        if (!Convert.TryFromBase64String(dotnetString, dotnetBytesBuffer, out var bytesWritten))
        {
            return s_tagNothingValue;
        }

        var bytesValue =
            PineValue.List(
                [
                ElmValue.ElmBytesTypeTagNameAsValue,
                PineValue.List([PineValue.Blob(dotnetBytesBuffer.AsMemory(start: 0, length: bytesWritten))])
                ]);

        return JustValue(bytesValue);
    }

    /// <summary>
    /// Short-circuit .NET implementation of <c>Base64.Decode.fromBytes</c>
    /// (<c>Bytes -&gt; Maybe String</c>): encodes the raw bytes argument into a
    /// base64 string, returning the corresponding <c>Maybe String</c> value,
    /// without interpreting the recursive decoding loop.
    /// </summary>
    public static PineValue? DecodeFromBytesLeafDelegate(PineValue environment)
    {
        var envFunctionsValue = s_leafInfos.Value["decode"].envFunctionsValue;

        if (environment.ValueFromPathOrEmptyList([0]) != envFunctionsValue)
        {
            return null;
        }

        var bytesArg = environment.ValueFromPathOrEmptyList([1]);

        if (bytesArg.ValueFromPathOrEmptyList([0]) != ElmValue.ElmBytesTypeTagNameAsValue)
        {
            return null;
        }

        if (bytesArg.ValueFromPathOrEmptyList([1, 0]) is not PineValue.BlobValue bytesBlob)
        {
            return null;
        }

        var dotnetString = Convert.ToBase64String(bytesBlob.Bytes.Span);

        return JustValue(ElmValueEncoding.StringAsPineValue(dotnetString));
    }
}
