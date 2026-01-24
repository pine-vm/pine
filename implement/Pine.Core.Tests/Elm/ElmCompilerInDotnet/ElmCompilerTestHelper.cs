using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ElmCompilerTestHelper
{
    private static readonly FrozenSet<string> s_pineKernelModuleNames =
        FrozenSet.Create(["Pine_builtin", "Pine_kernel"]);


    /// <summary>
    /// Creates a delegate for invoking an Elm function value (from PartialApplicationWrapper) directly.
    /// This method treats the function value as an expression that expects arguments passed one at a time.
    /// </summary>
    /// <param name="functionValue">The encoded function value (from PartialApplicationWrapper).</param>
    /// <param name="parseCache">Cache for parsing expressions.</param>
    /// <returns>
    /// A delegate that takes a list of arguments and returns a tuple containing:
    /// - The return value of the invoked function
    /// - The list of profiling invocation reports collected during execution
    /// </returns>
    public static Func<IReadOnlyList<PineValue>, (PineValue returnValue, IReadOnlyList<EvaluationReport> invocationReports)>
        CreateFunctionValueInvocationDelegate(PineValue functionValue, PineVMParseCache parseCache)
    {
        if (FunctionRecord.ParseFunctionRecordTagged(functionValue, parseCache).IsOkOrNull() is { } functionRecord)
        {
            var withOverallReportDelegate = CreateFunctionInvocationDelegate(functionRecord);

            return arguments =>
            {
                var (evalReport, invocationReports) = withOverallReportDelegate(arguments);

                return (evalReport.ReturnValue.Evaluate(), invocationReports);
            };
        }

        return arguments =>
        {
            var invocationReports = new List<EvaluationReport>();

            var vm = PineVMForProfiling(invocationReports.Add);

            // Apply arguments one at a time by evaluating the wrapper expression
            var currentValue = functionValue;

            for (var argIndex = 0; argIndex < arguments.Count; argIndex++)
            {
                var arg = arguments[argIndex];

                // Parse the current value as an expression
                var parseResult = parseCache.ParseExpression(currentValue);

                if (parseResult.IsErrOrNull() is { } parseErr)
                {
                    throw new Exception(
                        "Failed to parse function value as expression: " + parseErr +
                        " (argument index: " + argIndex + ")");
                }

                if (parseResult.IsOkOrNull() is not { } expression)
                {
                    throw new Exception("Failed to extract parsed expression");
                }

                // Evaluate the expression with the argument as environment
                var evalResult = vm.EvaluateExpression(expression, arg);

                if (evalResult.IsErrOrNull() is { } evalErr)
                {
                    throw new Exception("Failed to evaluate function value: " + evalErr);
                }

                if (evalResult.IsOkOrNull() is not { } resultValue)
                {
                    throw new Exception("Failed to extract evaluation result");
                }

                currentValue = resultValue;
            }

            return (currentValue, invocationReports);
        };
    }

    /// <summary>
    /// Compiles Elm modules and returns the parsed environment without static program analysis.
    /// Use this for tests that only need to invoke functions at runtime without static analysis.
    /// </summary>
    public static ElmInteractiveEnvironment.ParsedInteractiveEnvironment
        CompileElmModules(
        IReadOnlyList<string> elmModulesTexts,
        bool disableInlining)
    {
        var testCase =
            TestCase.DefaultAppWithoutPackages(elmModulesTexts);

        var appCodeTree = testCase.AsFileTree();

        var rootFilePaths =
            appCodeTree.EnumerateFilesTransitive()
            .Where(b => b.path[^1].EndsWith(".elm"))
            .Select(b => b.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: rootFilePaths,
                disableInlining: disableInlining)
            .Extract(err => throw new Exception(err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing interactive environment: " + err));

        return parsedEnv;
    }

    /// <summary>
    /// Creates a delegate for invoking an Elm function and collecting profiling reports.
    /// </summary>
    /// <param name="functionRecord">The parsed function record from the declaration value.</param>
    /// <returns>
    /// A delegate that takes a list of arguments and returns a tuple containing:
    /// - The return value of the invoked function
    /// - The list of profiling invocation reports collected during execution
    /// </returns>
    public static Func<IReadOnlyList<PineValue>, (EvaluationReport evalReport, IReadOnlyList<EvaluationReport> invocationReports)>
        CreateFunctionInvocationDelegate(FunctionRecord functionRecord)
    {
        return arguments =>
        {
            var composed =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(functionRecord, appendArguments: arguments)
            .Extract(err => throw new Exception(err));

            return EvaluateWithProfiling(composed.expression, composed.environment);
        };
    }

    public static (ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv, StaticProgram staticProgram)
        StaticProgramFromElmModules(
        IReadOnlyList<string> elmModulesTexts,
        bool disableInlining,
        Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var testCase =
            TestCase.DefaultAppWithoutPackages(elmModulesTexts);

        return
            StaticProgramFromTestCase(
                testCase,
                disableInlining: disableInlining,
                includeDeclaration,
                parseCache);
    }

    public static (ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv, StaticProgram staticProgram)
        StaticProgramFromTestCase(
        TestCase testCase,
        bool disableInlining,
        Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var appCodeTree = testCase.AsFileTree();

        var rootFilePaths =
            appCodeTree.EnumerateFilesTransitive()
            .Where(b => b.path[^1].EndsWith(".elm"))
            .Select(b => b.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: rootFilePaths,
                disableInlining: disableInlining)
            .Extract(err => throw new Exception(err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing interactive environment: " + err));

        var staticProgram =
            TestCase.ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnv,
                includeDeclaration: includeDeclaration,
                parseCache);

        return (parsedEnv, staticProgram);
    }

    /// <summary>
    /// Computes and returns the dependency layouts for all functions in an Elm module.
    /// This is useful for testing the first pass of the two-pass compilation approach.
    /// </summary>
    /// <param name="elmModuleText">The Elm module source text.</param>
    /// <returns>Dictionary mapping qualified function names to their dependency layouts.</returns>
    public static IReadOnlyDictionary<string, IReadOnlyList<string>> ComputeDependencyLayoutsFromModule(
        string elmModuleText)
    {
        var testCase = TestCase.DefaultAppWithoutPackages([elmModuleText]);
        var appCodeTree = testCase.AsFileTree();

        // Parse the modules
        var elmModuleFiles =
            appCodeTree.EnumerateFilesTransitive()
            .Where(file => file.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        var parsedModulesBeforeCanonicalize =
            new List<Core.Elm.ElmSyntax.Stil4mElmSyntax7.File>();

        foreach (var moduleFile in elmModuleFiles)
        {
            var moduleText = Encoding.UTF8.GetString(moduleFile.fileContent.Span);
            var parseResult = Core.Elm.ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText);

            if (parseResult.IsErrOrNull() is { } err)
            {
                throw new Exception(err);
            }

            if (parseResult.IsOkOrNull() is not { } parseModuleOk)
            {
                throw new Exception("Unexpected parse result type");
            }

            var parseModuleAst =
                Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromStil4mConcretized.Convert(parseModuleOk);

            parsedModulesBeforeCanonicalize.Add(parseModuleAst);
        }

        var canonicalizationResult =
            Canonicalization.Canonicalize(parsedModulesBeforeCanonicalize);

        if (canonicalizationResult.IsErrOrNull() is { } canonErr)
        {
            throw new Exception(canonErr);
        }

        if (canonicalizationResult.IsOkOrNull() is not { } canonicalizedModulesDict)
        {
            throw new Exception("Unexpected canonicalization result type");
        }

        var canonicalizedModules =
            canonicalizedModulesDict
            .Select(kvp => kvp.Value.IsOkOrNull())
            .Where(m => m is not null)
            .Select(m => m!)
            .ToList();

        var lambdaLiftedModules =
            canonicalizedModules
            .Select(LambdaLifting.LiftLambdas)
            .ToList();

        // Collect all functions
        var allFunctions =
            new Dictionary<string, (string moduleName, string functionName, Core.Elm.ElmSyntax.Stil4mElmSyntax7.Declaration.FunctionDeclaration declaration)>();

        foreach (var elmModuleSyntax in lambdaLiftedModules)
        {
            var moduleName =
                Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module.GetModuleName(elmModuleSyntax.ModuleDefinition.Value).Value;

            var moduleNameFlattened = string.Join(".", moduleName);

            var declarations =
                elmModuleSyntax.Declarations
                .Select(declNode => declNode.Value)
                .OfType<Core.Elm.ElmSyntax.Stil4mElmSyntax7.Declaration.FunctionDeclaration>();

            foreach (var declaration in declarations)
            {
                var functionName = declaration.Function.Declaration.Value.Name.Value;
                var qualifiedName = moduleNameFlattened + "." + functionName;
                allFunctions[qualifiedName] = (moduleNameFlattened, functionName, declaration);
            }
        }

        // Create initial context
        var initialContext =
            new ModuleCompilationContext(
                allFunctions,
                CompiledFunctionsCache: [],
                PineKernelModuleNames: s_pineKernelModuleNames);

        // Compute and return dependency layouts
        var (layouts, _, _) =
            ElmCompiler.ComputeDependencyLayoutsAndSccs(allFunctions, initialContext);

        return layouts;
    }

    /// <summary>
    /// Create a VM with all optimizations disabled, to support repeatable profiling.
    /// </summary>
    public static Core.Interpreter.IntermediateVM.PineVM PineVMForProfiling(
        Action<EvaluationReport> reportFunctionApplication)
    {
        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: reportFunctionApplication,
                compilationEnvClasses: null,
                disableReductionInCompilation: false,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null);

        return vm;
    }

    public static (EvaluationReport evalReport, IReadOnlyList<EvaluationReport> invocations)
        EvaluateWithProfiling(
        Expression expression,
        PineValue environment)
    {
        var invocationReports = new List<EvaluationReport>();
        var vm = PineVMForProfiling(invocationReports.Add);

        var evalResult =
            vm.EvaluateExpressionOnCustomStack(
                expression,
                environment,
                config: new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(ParseAndEvalCountLimit: null))
            .Extract(err => throw new Exception(err));

        return (evalResult, invocationReports);
    }

    public static string ParseAndRenderStaticProgram(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedInteractiveEnvironment,
        Func<DeclQualifiedName, bool> includeDeclaration,
        PineVMParseCache parseCache)
    {
        var parserConfig =
            BuildStaticProgramParserConfig.Default(
                parsedInteractiveEnvironment,
                parseCache: parseCache);

        var roots = new Dictionary<DeclQualifiedName, PineValue>();

        foreach (var (moduleName, moduleValue, moduleContent) in parsedInteractiveEnvironment.Modules)
        {
            foreach (var declaration in moduleContent.FunctionDeclarations)
            {
                var qualifiedName =
                    DeclQualifiedName.FromString(moduleName + "." + declaration.Key);

                if (includeDeclaration(qualifiedName))
                {
                    roots[qualifiedName] = declaration.Value;
                }
            }
        }

        var staticProgram =
            StaticProgramParser.ParseProgram(
                roots: roots,
                parseConfig: parserConfig,
                parseCache: parseCache)
            .Extract(err => throw new Exception("Failed parsing static program: " + err));

        var functionsTexts =
            staticProgram
            .OrderBy(func => func.Key)
            .Select(decl =>
                RenderStaticFunction(
                    functionName: decl.Key.FullName,
                    parameterReferences: decl.Value.ParametersExprs,
                    functionBody: decl.Value.BodyExpression))
            .ToList();

        return string.Join("\n\n", functionsTexts);
    }

    public static string RenderStaticFunction(
        string functionName,
        IReadOnlyList<StaticExpression<DeclQualifiedName>> parameterReferences,
        StaticExpression<DeclQualifiedName> functionBody)
    {
        // Use param_1_N naming to match the old parser's output format
        // The "1" comes from the environment structure [[closures], [arguments]] where parameters are at index 1
        // Build ordered list of parameter names to preserve correct parameter order
        var paramNames = new List<string>();
        var paramDict = new Dictionary<string, StaticExpression<DeclQualifiedName>>();

        for (var index = 0; index < parameterReferences.Count; index++)
        {
            var paramName = "param_1_" + index;
            paramNames.Add(paramName);
            paramDict[paramName] = parameterReferences[index];
        }

        var headerText =
            (functionName + " " + string.Join(" ", paramNames)).Trim() + " =";

        string? ReplaceExprWithIdentifier(StaticExpression<DeclQualifiedName> expr)
        {
            if (paramDict.FirstOrDefault(kvp => kvp.Value.Equals(expr)).Key is { } key)
            {
                return key;
            }

            if (expr is StaticExpression<DeclQualifiedName>.ParameterReferenceExpr paramRefExpr)
            {
                // Use param_1_N naming to match the old parser's output format
                var paramName = "param_1_" + paramRefExpr.ParameterIndex;
                return paramName;
            }

            // Check if this is a path expression (skip/head chain) starting from environment.
            // This handles tuple element access and choice type deconstruction
            // like param_1_0[0] (tuple element) or param_1_0[1][0] (choice type payload).
            if (StaticExpressionExtension.TryParseAsPathToExpression(
                expr,
                StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathInEnv)
            {
                // Path format is [envListIndex, paramIndex, ...nestedAccess]
                // Environment structure is [[closures], [arguments]]
                // So envListIndex=1 means arguments list, paramIndex is the parameter number,
                // and nestedAccess are indices into nested structures (tuples, choice type payloads).
                // For example: env[1][0][0] = arguments[0][0] = first element of first parameter's tuple
                if (pathInEnv.Count >= 2 && pathInEnv[0] is 1)
                {
                    var paramIndex = pathInEnv[1];
                    var paramName = "param_1_" + paramIndex;

                    // If there are more elements in the path, render them as indexing
                    if (pathInEnv.Count > 2)
                    {
                        var indexingSuffix = string.Concat(
                            pathInEnv.Skip(2).Select(idx => "[" + idx + "]"));
                        return paramName + indexingSuffix;
                    }

                    return paramName;
                }
            }

            return null;
        }

        StaticExpressionDisplay.FunctionApplicationRenderingNew<DeclQualifiedName> FunctionApplicationRenderer(DeclQualifiedName funcName)
        {
            // Extract arguments from the Arguments expression (which should be a List)
            static IReadOnlyList<StaticExpression<DeclQualifiedName>> ExtractArgs(StaticExpression<DeclQualifiedName> argsExpr)
            {
                if (argsExpr is StaticExpression<DeclQualifiedName>.List listExpr)
                {
                    return listExpr.Items;
                }

                // For non-list arguments, return a single-element list
                return [argsExpr];
            }

            return new StaticExpressionDisplay.FunctionApplicationRenderingNew<DeclQualifiedName>(
                FunctionName: funcName.FullName,
                FunctionInterface: ExtractArgs);
        }

        return
            headerText +
            "\n" +
            StaticExpressionDisplay.RenderToString(
                expression: functionBody,
                valueRenderer: val => StaticExpressionDisplay.RenderValueAsExpression(val, StaticExpressionDisplay.DefaultBlobRenderer),
                functionApplicationRenderer: FunctionApplicationRenderer,
                replaceExprWithIdentifier: ReplaceExprWithIdentifier,
                indentString: "    ",
                indentLevel: 1,
                kernelApplicationPrefix: "Pine_builtin");
    }
}
