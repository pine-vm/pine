using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ElmCompilerTestHelper
{
    /// <summary>
    /// Creates a delegate for invoking an Elm function and collecting profiling reports.
    /// </summary>
    /// <param name="functionRecord">The parsed function record from the declaration value.</param>
    /// <returns>
    /// A delegate that takes a list of arguments and returns a tuple containing:
    /// - The return value of the invoked function
    /// - The list of profiling invocation reports collected during execution
    /// </returns>
    public static Func<IReadOnlyList<PineValue>, (PineValue returnValue, IReadOnlyList<EvaluationReport> invocationReports)>
        CreateFunctionInvocationDelegate(FunctionRecord functionRecord)
    {
        return arguments =>
        {
            var invocationReports = new List<EvaluationReport>();

            var vm = PineVMForProfiling(invocationReports.Add);

            var applyRunResult =
                ElmInteractiveEnvironment.ApplyFunction(
                    vm,
                    functionRecord,
                    arguments: arguments)
                .Extract(err => throw new Exception(err));

            return (applyRunResult, invocationReports);
        };
    }

    public static (ElmInteractiveEnvironment.ParsedInteractiveEnvironment parsedEnv, StaticProgram staticProgram)
        StaticProgramFromElmModules(
        IReadOnlyList<string> elmModulesTexts,
        bool disableInlining,
        System.Func<DeclQualifiedName, bool> includeDeclaration,
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
        System.Func<DeclQualifiedName, bool> includeDeclaration,
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
            .Extract(err => throw new System.Exception(err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var staticProgram =
            TestCase.ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnv,
                includeDeclaration: includeDeclaration,
                parseCache);

        return (parsedEnv, staticProgram);
    }

    /// <summary>
    /// Create a VM with all optimizations disabled, to support repeatable profiling.
    /// </summary>
    public static IPineVM PineVMForProfiling(
        System.Action<Core.Interpreter.IntermediateVM.EvaluationReport> reportFunctionApplication)
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
                precompiledLeaves: ImmutableDictionary<PineValue, System.Func<PineValue, PineValue?>>.Empty,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null);

        return vm;
    }
}
