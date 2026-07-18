using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

/// <summary>
/// Precompiled leaves for recursive functions in the kernel <c>Json.Decode</c> module.
/// </summary>
public static class KernelJsonDecodePrecompiledLeaves
{
    /// <summary>
    /// Pine value key for the precompiled <c>Json.Decode.parseValue</c> leaf.
    /// </summary>
    public static PineValue ParseValueLeafKey => s_parseValueLeafInfo.Value.leafKey;

    private static readonly Lazy<(PineValue leafKey, PineValue envFunctionsValue)> s_parseValueLeafInfo =
        new(BuildParseValueLeafInfo);

    private static (PineValue leafKey, PineValue envFunctionsValue) BuildParseValueLeafInfo()
    {
        var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

        var rootFilePaths =
            kernelModulesTree.EnumerateFilesTransitive()
            .Where(
                file =>
                file.path[^1].Equals("Decode.elm", StringComparison.OrdinalIgnoreCase) &&
                file.path.Contains("Json"))
            .Select(file => (IReadOnlyList<string>)file.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                kernelModulesTree,
                rootFilePaths: rootFilePaths)
            .Map(result => result.compiledEnvValue)
            .Extract(
                error => throw new Exception(
                    "Failed compiling kernel Json.Decode module to derive leaf info: " + error));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(
                error => throw new Exception(
                    "Failed parsing kernel Json.Decode module to derive leaf info: " + error));

        var jsonDecodeModule =
            parsedEnv.Modules
            .First(module => module.moduleName is "Json.Decode")
            .moduleContent;

        var record =
            FunctionRecord.ParseFunctionRecordTagged(
                jsonDecodeModule.FunctionDeclarations["parseValue"],
                new PineVMParseCache())
            .Extract(
                error => throw new Exception(
                    "Failed parsing Json.Decode.parseValue function record to derive leaf info: " + error));

        return
            (ExpressionEncoding.EncodeExpressionAsValue(record.InnerFunction),
            PineValue.List([.. record.EnvFunctions.ToArray()]));
    }

    /// <summary>
    /// Executes <c>Json.Decode.parseValue</c> directly, or returns <c>null</c> for an unexpected environment.
    /// </summary>
    public static PineValue? ParseValueLeafDelegate(PineValue environment)
    {
        if (environment.ValueFromPathOrEmptyList([0]) != s_parseValueLeafInfo.Value.envFunctionsValue)
        {
            return null;
        }

        return
            ElmSyntaxInterpreter.JsonDecodeParseValue(
                environment.ValueFromPathOrEmptyList([1]),
                environment.ValueFromPathOrEmptyList([2]));
    }

    /// <summary>
    /// Default precompiled-leaves dictionary contributed by the kernel <c>Json.Decode</c> module.
    /// </summary>
    public static IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> DefaultLeaves =>
        s_defaultLeaves.Value;

    private static readonly Lazy<IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>> s_defaultLeaves =
        new(
            () =>
            ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty
            .Add(ParseValueLeafKey, ParseValueLeafDelegate));
}
