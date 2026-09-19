using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

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
/// <c>Dict</c> functions are determined by the output of the Elm compiler, they are
/// derived at runtime (lazily, on first access) by compiling the kernel <c>Dict</c>
/// module — bundled with <c>Pine.Core</c> via <see cref="BundledFiles.ElmKernelModulesDefault"/>
/// — and parsing the relevant function records.
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
    /// Pine value key for the precompiled <c>Dict.toList</c> leaf.
    /// </summary>
    public static PineValue DictToListLeafKey => s_dictLeafInfos.Value["toList"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>Dict.size</c> leaf.
    /// </summary>
    public static PineValue DictSizeLeafKey => s_dictLeafInfos.Value["size"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>Dict.keys</c> leaf.
    /// </summary>
    public static PineValue DictKeysLeafKey => s_dictLeafInfos.Value["keys"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>Dict.values</c> leaf.
    /// </summary>
    public static PineValue DictValuesLeafKey => s_dictLeafInfos.Value["values"].leafKey;

    /// <summary>
    /// Pine value key for the precompiled <c>Dict.insert</c> leaf.
    /// </summary>
    public static PineValue DictInsertLeafKey => s_dictLeafInfos.Value["insert"].leafKey;

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
        new(() => s_dictLeafInfos.Value["get"]);

    private static readonly Lazy<IReadOnlyDictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>>
        s_dictLeafInfos =
        new(BuildDictLeafInfos);

    private static IReadOnlyDictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>
        BuildDictLeafInfos()
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
                    "Failed compiling kernel Dict module to derive leaf info: " + err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(
                err => throw new Exception(
                    "Failed parsing kernel Dict module to derive leaf info: " + err));

        var dictModule =
            parsedEnv.Modules
            .First(m => m.moduleName is "Dict")
            .moduleContent;

        var parseCache = new PineVMParseCache();
        var infos = new Dictionary<string, (PineValue leafKey, PineValue envFunctionsValue)>();

        foreach (var functionName in new[] { "get", "toList", "size", "keys", "values", "insert" })
        {
            var record =
                FunctionRecord.ParseFunctionRecordTagged(
                    dictModule.FunctionDeclarations[functionName],
                    parseCache)
                .Extract(
                    err => throw new Exception(
                        $"Failed parsing Dict.{functionName} function record to derive leaf info: {err}"));

            infos.Add(
                functionName,
                (ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction),
                PineValue.List([.. record.EnvFunctions.ToArray()])));
        }

        return infos;
    }

    /// <summary>
    /// Precompiled-leaf delegate for <c>Dict.get</c>; executes the lookup directly in
    /// .NET and returns the resulting <c>Maybe</c> tag, or <c>null</c> if the environment
    /// does not match the expected shape (i.e. <c>env[0]</c> is not
    /// <see cref="DictGetEnvFunctionsValue"/>).
    /// </summary>
    public static PineValueInProcess? DictGetLeafDelegate(PineValueInProcess environment)
    {
        if (!PineValueInProcess.AreEqual(environment.GetElementAt(0), DictGetEnvFunctionsValue))
        {
            return null;
        }

        var targetKey = environment.ValueFromPathOrEmptyList([1]);
        var dict = environment.ValueFromPathOrEmptyList([2]);

        return DictGetImplInProcess(targetKey, dict);
    }

    /// <summary>
    /// Executes <c>Dict.toList</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValueInProcess? DictToListLeafDelegate(PineValueInProcess environment) =>
        GetUnaryArgument(environment, "toList") is { } dict
        ?
        DictToListInProcess(dict, DictListItem.Pair)
        :
        null;

    /// <summary>
    /// Executes <c>Dict.size</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValueInProcess? DictSizeLeafDelegate(PineValueInProcess environment)
    {
        var dict = GetUnaryArgument(environment, "size");

        return
            dict is null
            ?
            null
            :
            PineValueInProcess.CreateInteger(DictSizeImpl(dict));
    }

    /// <summary>
    /// Executes <c>Dict.keys</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValueInProcess? DictKeysLeafDelegate(PineValueInProcess environment) =>
        GetUnaryArgument(environment, "keys") is { } dict
        ?
        DictToListInProcess(dict, DictListItem.Key)
        :
        null;

    /// <summary>
    /// Executes <c>Dict.values</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValueInProcess? DictValuesLeafDelegate(PineValueInProcess environment) =>
        GetUnaryArgument(environment, "values") is { } dict
        ?
        DictToListInProcess(dict, DictListItem.Value)
        :
        null;

    /// <summary>
    /// Executes <c>Dict.insert</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValueInProcess? DictInsertLeafDelegate(PineValueInProcess environment)
    {
        if (!EnvironmentMatches(environment, "insert"))
        {
            return null;
        }

        var key = PineValueInProcess.ValueInProcessFromPathOrNull(environment, [1]);
        var value = PineValueInProcess.ValueInProcessFromPathOrNull(environment, [2]);
        var dict = PineValueInProcess.ValueInProcessFromPathOrNull(environment, [3]);

        return
            key is null || value is null || dict is null
            ?
            null
            :
            DictInsertImpl(key, value, dict);
    }

    private static PineValue? GetUnaryArgument(PineValueInProcess environment, string functionName) =>
        EnvironmentMatches(environment, functionName)
        ?
        environment.ValueFromPathOrEmptyList([1])
        :
        null;

    private static bool EnvironmentMatches(PineValueInProcess environment, string functionName) =>
        PineValueInProcess.AreEqual(
            environment.GetElementAt(0),
            s_dictLeafInfos.Value[functionName].envFunctionsValue);

    /// <summary>
    /// Default precompiled-leaves dictionary contributed by the kernel <c>Dict</c> module.
    /// Suitable for merging into the dictionary consumed by the intermediate VM.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, PrecompiledLeaf> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, PrecompiledLeaf>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, PrecompiledLeaf>.Empty
            .Add(DictGetLeafKey, DictGetLeafDelegate)
            .Add(DictToListLeafKey, DictToListLeafDelegate)
            .Add(DictSizeLeafKey, DictSizeLeafDelegate)
            .Add(DictKeysLeafKey, DictKeysLeafDelegate)
            .Add(DictValuesLeafKey, DictValuesLeafDelegate)
            .Add(DictInsertLeafKey, DictInsertLeafDelegate));

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
        return DictGetImplInProcess(targetKey, dict).Evaluate();
    }

    private static PineValueInProcess DictGetImplInProcess(PineValue targetKey, PineValue dict)
    {
        while (true)
        {
            var dictTag = dict.ValueFromPathOrEmptyList([1]);

            if (dictTag == ElmValue.ElmDictEmptyTagNameAsValue)
            {
                return PineValueInProcess.Create(s_tag_Nothing_Value);
            }

            if (dictTag != ElmValue.ElmDictNotEmptyTagNameAsValue)
            {
                throw new ParseExpressionException("Unexpected dict tag");
            }

            var key = dict.ValueFromPathOrEmptyList([3]);
            var value = dict.ValueFromPathOrEmptyList([4]);
            var left = dict.ValueFromPathOrEmptyList([5]);
            var right = dict.ValueFromPathOrEmptyList([6]);

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

            return
                ElmValueInProcess.CreateChoice(
                    PineValueInProcess.Create(s_tag_Just_Name_Value),
                    [PineValueInProcess.Create(value)]);
        }
    }

    private enum DictListItem
    {
        Pair,
        Key,
        Value,
    }

    private readonly record struct DictNode(
        PineValue Color,
        PineValue Key,
        PineValue Value,
        PineValue Left,
        PineValue Right);

    private readonly record struct DictNodeInProcess(
        PineValueInProcess Color,
        PineValueInProcess Key,
        PineValueInProcess Value,
        PineValueInProcess Left,
        PineValueInProcess Right);

    private static readonly PineValue s_emptyDict =
        ElmValueEncoding.ElmValueAsPineValue(
            ElmValue.TagInstance(ElmValue.ElmDictEmptyTagName, []));

    private static readonly PineValue s_red =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("Red", []));

    private static readonly PineValue s_black =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("Black", []));

    private static readonly PineValueInProcess s_emptyDictInProcess =
        PineValueInProcess.Create(s_emptyDict);

    private static readonly PineValueInProcess s_redInProcess =
        PineValueInProcess.Create(s_red);

    private static readonly PineValueInProcess s_blackInProcess =
        PineValueInProcess.Create(s_black);

    private static readonly PineValueInProcess s_dictNotEmptyTagNameInProcess =
        PineValueInProcess.Create(ElmValue.ElmDictNotEmptyTagNameAsValue);

    /// <summary>
    /// Executes <c>Dict.toList</c> directly.
    /// </summary>
    public static PineValue DictToList(PineValue dict) =>
        DictToListInProcess(dict, DictListItem.Pair).Evaluate();

    private static PineValueInProcess DictToListInProcess(PineValue dict, DictListItem item)
    {
        var result = new List<PineValueInProcess>();
        var pending = new Stack<(PineValue dict, bool emit)>();
        pending.Push((dict, false));

        while (pending.Count > 0)
        {
            var current = pending.Pop();

            if (IsEmpty(current.dict))
            {
                continue;
            }

            var node = ParseNode(current.dict);

            if (current.emit)
            {
                result.Add(
                    item switch
                    {
                        DictListItem.Pair =>
                        PineValueInProcess.CreateList(
                            [PineValueInProcess.Create(node.Key), PineValueInProcess.Create(node.Value)]),

                        DictListItem.Key => PineValueInProcess.Create(node.Key),
                        DictListItem.Value => PineValueInProcess.Create(node.Value),

                        _ =>
                        throw new NotImplementedException(
                            "DictToListImpl does not handle item variant: " + item),
                    });

                continue;
            }

            pending.Push((node.Right, false));
            pending.Push((current.dict, true));
            pending.Push((node.Left, false));
        }

        return PineValueInProcess.CreateList(result);
    }

    private static long DictSizeImpl(PineValue dict)
    {
        long count = 0;
        var pending = new Stack<PineValue>();
        pending.Push(dict);

        while (pending.Count > 0)
        {
            var current = pending.Pop();

            if (IsEmpty(current))
            {
                continue;
            }

            var node = ParseNode(current);
            count++;
            pending.Push(node.Left);
            pending.Push(node.Right);
        }

        return count;
    }

    private static PineValueInProcess DictInsertImpl(
        PineValueInProcess key,
        PineValueInProcess value,
        PineValueInProcess dict)
    {
        var inserted = DictInsertHelp(key, value, dict);
        var root = ParseNodeInProcess(inserted);

        return
            PineValueInProcess.AreEqual(root.Color, s_red)
            ?
            NodeInProcess(s_blackInProcess, root.Key, root.Value, root.Left, root.Right)
            :
            inserted;
    }

    private static PineValueInProcess DictInsertHelp(
        PineValueInProcess key,
        PineValueInProcess value,
        PineValueInProcess dict)
    {
        if (IsEmptyInProcess(dict))
        {
            return NodeInProcess(s_redInProcess, key, value, s_emptyDictInProcess, s_emptyDictInProcess);
        }

        var node = ParseNodeInProcess(dict);

        var comparison =
            CoreBasicsPrecompiledLeaves.BasicsCompare(
                key.Evaluate(),
                node.Key.Evaluate());

        if (comparison == s_tag_LT_Value)
        {
            return
                BalanceInProcess(
                    node.Color,
                    node.Key,
                    node.Value,
                    DictInsertHelp(key, value, node.Left),
                    node.Right);
        }

        if (comparison == s_tag_GT_Value)
        {
            return
                BalanceInProcess(
                    node.Color,
                    node.Key,
                    node.Value,
                    node.Left,
                    DictInsertHelp(key, value, node.Right));
        }

        return NodeInProcess(node.Color, node.Key, value, node.Left, node.Right);
    }

    private static PineValueInProcess BalanceInProcess(
        PineValueInProcess color,
        PineValueInProcess key,
        PineValueInProcess value,
        PineValueInProcess left,
        PineValueInProcess right)
    {
        if (TryParseRedNodeInProcess(right, out var redRight))
        {
            if (TryParseRedNodeInProcess(left, out var redLeft))
            {
                return
                    NodeInProcess(
                        s_redInProcess,
                        key,
                        value,
                        NodeInProcess(
                            s_blackInProcess,
                            redLeft.Key,
                            redLeft.Value,
                            redLeft.Left,
                            redLeft.Right),
                        NodeInProcess(
                            s_blackInProcess,
                            redRight.Key,
                            redRight.Value,
                            redRight.Left,
                            redRight.Right));
            }

            return
                NodeInProcess(
                    color,
                    redRight.Key,
                    redRight.Value,
                    NodeInProcess(s_redInProcess, key, value, left, redRight.Left),
                    redRight.Right);
        }

        if (TryParseRedNodeInProcess(left, out var leftRed) &&
            TryParseRedNodeInProcess(leftRed.Left, out var leftLeftRed))
        {
            return
                NodeInProcess(
                    s_redInProcess,
                    leftRed.Key,
                    leftRed.Value,
                    NodeInProcess(
                        s_blackInProcess,
                        leftLeftRed.Key,
                        leftLeftRed.Value,
                        leftLeftRed.Left,
                        leftLeftRed.Right),
                    NodeInProcess(s_blackInProcess, key, value, leftRed.Right, right));
        }

        return NodeInProcess(color, key, value, left, right);
    }

    private static bool TryParseRedNodeInProcess(
        PineValueInProcess dict,
        out DictNodeInProcess node)
    {
        if (IsEmptyInProcess(dict))
        {
            node = default;
            return false;
        }

        node = ParseNodeInProcess(dict);
        return PineValueInProcess.AreEqual(node.Color, s_red);
    }

    private static bool IsEmptyInProcess(PineValueInProcess dict) =>
        PineValueInProcess.AreEqual(dict.GetElementAt(1), ElmValue.ElmDictEmptyTagNameAsValue);

    private static DictNodeInProcess ParseNodeInProcess(PineValueInProcess dict)
    {
        if (!PineValueInProcess.AreEqual(
                dict.GetElementAt(1),
                ElmValue.ElmDictNotEmptyTagNameAsValue))
        {
            throw new ParseExpressionException("Unexpected dict tag");
        }

        return
            new DictNodeInProcess(
                dict.GetElementAt(2),
                dict.GetElementAt(3),
                dict.GetElementAt(4),
                dict.GetElementAt(5),
                dict.GetElementAt(6));
    }

    private static PineValueInProcess NodeInProcess(
        PineValueInProcess color,
        PineValueInProcess key,
        PineValueInProcess value,
        PineValueInProcess left,
        PineValueInProcess right) =>
        ElmValueInProcess.CreateChoice(
            s_dictNotEmptyTagNameInProcess,
            [color, key, value, left, right]);

    private static bool IsEmpty(PineValue dict) =>
        dict.ValueFromPathOrEmptyList([1]) == ElmValue.ElmDictEmptyTagNameAsValue;

    private static DictNode ParseNode(PineValue dict)
    {
        if (dict.ValueFromPathOrEmptyList([1]) != ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            throw new ParseExpressionException("Unexpected dict tag");
        }

        return
            new DictNode(
                dict.ValueFromPathOrEmptyList([2]),
                dict.ValueFromPathOrEmptyList([3]),
                dict.ValueFromPathOrEmptyList([4]),
                dict.ValueFromPathOrEmptyList([5]),
                dict.ValueFromPathOrEmptyList([6]));
    }

}
