using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Elm;

/// <summary>Caller conveniences for selecting declarations from source and executing compiled exports.</summary>
public static class ElmSourceCompilation
{
    /// <summary>
    /// Enumerates top-level function declarations in exactly the selected files.
    /// An empty selection produces no roots; imports do not expand the selection.
    /// </summary>
    public static IReadOnlyList<DeclQualifiedName> EnumerateRootDeclarations(
        FileTree sourceTree,
        IReadOnlyList<IReadOnlyList<string>> filePaths,
        IDictionary<string, Result<ElmSyntaxParseError, SyntaxTypes.File>>? sourceParseCache = null)
    {
        var roots = new List<DeclQualifiedName>();

        foreach (var path in filePaths)
        {
            if (path.Count is 0 || !path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
                continue;

            if (sourceTree.GetNodeAtPath(path) is not FileTree.FileNode file)
                throw new ArgumentException("Selected Elm source file not found: " + string.Join("/", path));

            var text = Encoding.UTF8.GetString(file.Bytes.Span);

            if (sourceParseCache is null || !sourceParseCache.TryGetValue(text, out var parsed))
            {
                parsed = ElmSyntaxParser.ParseModuleText(text);

                sourceParseCache?[text] = parsed;
            }

            var syntax =
                parsed
                .Extract(
                    error => throw new ArgumentException("Failed parsing " + string.Join("/", path) + ": " + error));

            var moduleName = SyntaxTypes.Module.GetModuleName(syntax.ModuleDefinition.Value).Value;

            roots.AddRange(
                syntax.Declarations.Select(declaration => declaration.Value)
                .OfType<SyntaxTypes.Declaration.FunctionDeclaration>()
                .Select(
                    declaration =>
                    DeclQualifiedName.Create(moduleName, declaration.Function.Declaration.Value.Name.Value)));
        }

        return [.. roots.Distinct()];
    }

    /// <summary>Enumerates selected prepared declarations using their owner-isolated compiler identities.</summary>
    public static IReadOnlyList<DeclQualifiedName> EnumerateRootDeclarations(
        ElmResolvedBuild build,
        IReadOnlyList<IReadOnlyList<string>> filePaths)
    {
        var roots = new List<DeclQualifiedName>();

        foreach (var path in filePaths)
        {
            if (path.Count is 0 || !path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
                continue;

            var sourcePath = string.Join("/", path);

            if (!build.CompilerModuleNames.TryGetValue(sourcePath, out var compilerName) ||
                !build.CompilerModuleSyntax.TryGetValue(compilerName, out var syntax))
                throw new ArgumentException("Selected prepared Elm source file not found: " + sourcePath);

            roots.AddRange(
                syntax.Declarations.Select(declaration => declaration.Value)
                .OfType<SyntaxTypes.Declaration.FunctionDeclaration>()
                .Select(
                    declaration =>
                    DeclQualifiedName.Create(compilerName.Split('.'), declaration.Function.Declaration.Value.Name.Value)));
        }

        return [.. roots.Distinct()];
    }

    /// <summary>Expands a caller's source selection into explicit roots, without evaluating any exports.</summary>
    public static Result<string, (PineValue compiledEnvValue, CompilationPipelineStageResults<DefaultLoweredResults> pipelineStageResults)>
        CompileInteractiveEnvironmentFromFiles(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        ElmSyntaxOptimizationConfig? syntaxOptimization = null,
        bool disableGenericApplicationChainConsolidation = false,
        bool includeBundledKernelModules = true,
        IDictionary<string, Result<ElmSyntaxParseError, SyntaxTypes.File>>? sourceParseCache = null,
        IDictionary<(SyntaxTypes.Node<SyntaxTypes.Declaration> Declaration, string Scope),
            DeclarationCanonicalizationCacheEntry>? canonicalizationCache = null) =>
        ElmCompiler.CompileInteractiveEnvironment(
            appCodeTree,
            EnumerateRootDeclarations(appCodeTree, rootFilePaths, sourceParseCache),
            syntaxOptimization,
            disableGenericApplicationChainConsolidation,
            includeBundledKernelModules: includeBundledKernelModules,
            sourceParseCache: sourceParseCache,
            canonicalizationCache: canonicalizationCache);

    /// <summary>Executes zero-parameter exports on the caller's VM; other exports remain executable functions.</summary>
    public static Result<string, PineValue> EvaluateZeroParameterRoots(PineValue compiledEnvironment, IPineVM vm)
    {
        var parsed =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnvironment);

        if (parsed.IsErrOrNull() is { } parseError)
            return parseError;

        var environment = parsed.Extract(error => throw new InvalidOperationException(error));
        var parseCache = new PineVMParseCache();
        var modules = new List<PineValue>();

        foreach (var module in environment.Modules)
        {
            var declarations = new List<PineValue>();

            foreach (var declaration in module.moduleContent.FunctionDeclarations.Concat(module.moduleContent.TypeDeclarations))
            {
                var value = declaration.Value;

                var function = FunctionRecord.ParseFunctionRecordTagged(value, parseCache).IsOkOrNull();

                if (function is { ParameterCount: 0 } ||
                    (function is null && parseCache.ParseExpression(value).IsOkOrNull() is not null))
                {
                    var evaluated = EvaluateZeroParameterRoot(value, vm, parseCache);

                    if (evaluated.IsErrOrNull() is { } evaluationError)
                        return "Failed evaluating " + module.moduleName + "." + declaration.Key + ": " + evaluationError;

                    value = evaluated.Extract(error => throw new InvalidOperationException(error));
                }

                declarations.Add(PineValue.List([StringEncoding.ValueFromString(declaration.Key), value]));
            }

            modules.Add(
                PineValue.List(
                    [
                    StringEncoding.ValueFromString(module.moduleName),
                    PineValue.List([.. declarations])
                    ]));
        }

        return PineValue.List([.. modules]);
    }

    /// <summary>Executes a compiled value declaration, accepting tagged function records and legacy expression wrappers.</summary>
    public static Result<string, PineValue> EvaluateZeroParameterRoot(
        PineValue wrapper,
        IPineVM vm,
        PineVMParseCache? parseCache = null)
    {
        parseCache ??= new PineVMParseCache();

        if (ElmInteractiveEnvironment.ParseTagged(wrapper).IsOkOrNullable() is { name: "Function" } &&
            FunctionRecord.ParseFunctionRecordTagged(wrapper, parseCache).IsOkOrNull() is { } function)
            return ElmInteractiveEnvironment.ApplyFunction(vm, function, []);

        if (parseCache.ParseExpression(wrapper).IsOkOrNull() is { } expression)
            return vm.EvaluateExpression(expression, PineValue.EmptyList);

        return wrapper;
    }
}
