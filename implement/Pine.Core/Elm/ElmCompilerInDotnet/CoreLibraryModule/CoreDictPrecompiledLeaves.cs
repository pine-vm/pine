using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;

/// <summary>
/// Precompiled leaves (short-circuit .NET implementations) for inner bodies of function
/// values defined in the kernel <c>Dict</c> module.
/// <para>
/// Each entry maps the Pine value encoding of a function's innermost
/// <c>ParseAndEval</c>-target expression to a delegate that returns the result directly,
/// bypassing the intermediate VM's interpretation of the recursive expression tree.
/// </para>
/// <para>
/// Because the inner body and captured <c>envFunctions</c> value of the kernel
/// <c>Dict.get</c> function are determined by the output of the Elm compiler, they are
/// derived at runtime (lazily, on first access) by compiling the kernel <c>Dict</c>
/// module — bundled with <c>Pine.Core</c> via <see cref="BundledFiles.ElmKernelModulesDefault"/>
/// — and parsing its <c>get</c> function record.
/// </para>
/// </summary>
public static class CoreDictPrecompiledLeaves
{
    /// <summary>
    /// Pine value encoding of the inner body of <c>Dict.get</c>'s function record (the
    /// expression that the intermediate VM dispatches to via <c>ParseAndEval</c> at the
    /// innermost level of the curried application of <c>Dict.get</c>); used as the key
    /// under which the <c>Dict.get</c> leaf is registered in the precompiled-leaves
    /// dictionary.
    /// </summary>
    public static PineValue DictGetLeafKey => s_dictGetLeafInfo.Value.leafKey;

    /// <summary>
    /// Pine value list capturing the <c>envFunctions</c> slot of the runtime environment
    /// that the kernel <c>Dict.get</c> function expects at the inner-body call site.
    /// <para>
    /// The leaf delegate gates execution on this value matching <c>env[0]</c> so that
    /// only invocations of the kernel <c>Dict.get</c> (and not arbitrary expressions
    /// sharing the same inner body) are short-circuited.
    /// </para>
    /// </summary>
    public static PineValue DictGetEnvFunctionsValue => s_dictGetLeafInfo.Value.envFunctionsValue;

    private static readonly Lazy<(PineValue leafKey, PineValue envFunctionsValue)> s_dictGetLeafInfo =
        new(BuildDictGetLeafInfo);

    private static (PineValue leafKey, PineValue envFunctionsValue) BuildDictGetLeafInfo()
    {
        var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

        var rootFilePaths =
            kernelModulesTree.EnumerateFilesTransitive()
            .Where(b => b.path[^1].Equals("Dict.elm", StringComparison.OrdinalIgnoreCase))
            .Select(b => (IReadOnlyList<string>)b.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                kernelModulesTree,
                rootFilePaths: rootFilePaths)
            .Map(r => r.compiledEnvValue)
            .Extract(
                err => throw new Exception(
                    "Failed compiling kernel Dict module to derive Dict.get leaf info: " + err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(
                err => throw new Exception(
                    "Failed parsing kernel Dict module to derive Dict.get leaf info: " + err));

        var dictGetFunctionValue =
            parsedEnv.Modules
            .First(m => m.moduleName is "Dict")
            .moduleContent.FunctionDeclarations["get"];

        var record =
            FunctionRecord.ParseFunctionRecordTagged(
                dictGetFunctionValue,
                new PineVMParseCache())
            .Extract(
                err => throw new Exception(
                    "Failed parsing Dict.get function record to derive leaf info: " + err));

        var leafKey =
            ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction);

        var envFunctionsValue =
            PineValue.List([.. record.EnvFunctions.ToArray()]);

        return (leafKey, envFunctionsValue);
    }

    /// <summary>
    /// Precompiled-leaf delegate for <c>Dict.get</c>; executes the lookup directly in
    /// .NET and returns the resulting <c>Maybe</c> tag, or <c>null</c> if the environment
    /// does not match the expected shape (i.e. <c>env[0]</c> is not
    /// <see cref="DictGetEnvFunctionsValue"/>).
    /// </summary>
    public static PineValue? DictGetLeafDelegate(PineValue environment)
    {
        if (environment.ValueFromPathOrEmptyList([0]) != DictGetEnvFunctionsValue)
        {
            return null;
        }

        var targetKey = environment.ValueFromPathOrEmptyList([1]);
        var dict = environment.ValueFromPathOrEmptyList([2]);

        return DictGetImpl(targetKey, dict);
    }

    /// <summary>
    /// Default precompiled-leaves dictionary contributed by the kernel <c>Dict</c> module.
    /// Suitable for merging into the dictionary consumed by the intermediate VM.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
            .Add(DictGetLeafKey, DictGetLeafDelegate));

    // ========== .NET implementation of Dict.get ==========

    private static readonly PineValue s_tag_Nothing_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("Nothing", []));

    private static readonly PineValue s_tag_Just_Name_Value =
        StringEncoding.ValueFromString("Just");

    private static readonly PineValue s_tag_LT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("LT", []));

    private static readonly PineValue s_tag_GT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("GT", []));

    /// <summary>
    /// .NET implementation of <c>Dict.get</c>. Iterates over the red-black tree of the
    /// Elm <c>Dict</c> while reusing
    /// <see cref="CoreBasicsPrecompiledLeaves.BasicsCompare"/> for the comparison of the
    /// requested key against the branch keys.
    /// </summary>
    internal static PineValue DictGetImpl(PineValue targetKey, PineValue dict)
    {
        while (true)
        {
            var dictTag = dict.ValueFromPathOrEmptyList([0]);

            if (dictTag == ElmValue.ElmDictEmptyTagNameAsValue)
            {
                return s_tag_Nothing_Value;
            }

            if (dictTag != ElmValue.ElmDictNotEmptyTagNameAsValue)
            {
                throw new ParseExpressionException("Unexpected dict tag");
            }

            var nodeArgs = dict.ValueFromPathOrEmptyList([1]);

            var key = nodeArgs.ValueFromPathOrEmptyList([1]);
            var value = nodeArgs.ValueFromPathOrEmptyList([2]);
            var left = nodeArgs.ValueFromPathOrEmptyList([3]);
            var right = nodeArgs.ValueFromPathOrEmptyList([4]);

            var comparison = CoreBasicsPrecompiledLeaves.BasicsCompare(targetKey, key);

            if (comparison == s_tag_LT_Value)
            {
                dict = left;
                continue;
            }

            if (comparison == s_tag_GT_Value)
            {
                dict = right;
                continue;
            }

            return PineValue.List([s_tag_Just_Name_Value, PineValue.List([value])]);
        }
    }
}
