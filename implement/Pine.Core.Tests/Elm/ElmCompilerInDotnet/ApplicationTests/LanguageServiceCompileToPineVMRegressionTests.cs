using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Regression test for the test-side defect that originally surfaced in
/// <see cref="ElmLanguageServiceTests.References_request_finds_usage_across_modules"/>
/// as <c>Stack depth limit exceeded: 100_000</c> on the very first
/// <c>addWorkspaceFile</c> call (with the same <c>a1b3551e</c>-repeating
/// stack-frame hash).
/// <para>
/// <b>Root cause.</b> Under the current "Approach A1" compilation, the
/// raw declaration value cached in
/// <see cref="ElmInteractiveEnvironment.ElmModule.FunctionDeclarations"/>
/// for any top-level binding — including 0-argument ones — is a
/// function-record wrapper, not the binding's evaluated result. The
/// pre-existing scenario test obtained <c>initState</c> via
/// <c>GetTestFunction("initState")</c> (returning the raw wrapper) and
/// passed it as the record-typed <c>stateBefore</c> argument. The body's
/// compile-time-known field-index access for <c>stateBefore.documentCache</c>
/// then read out an inner Pine sub-expression of the wrapper, which
/// <c>Dict.insertHelp</c> walked into recursively forever.
/// </para>
/// <para>
/// <b>Fix.</b> 0-argument top-level bindings must be invoked with zero
/// arguments to obtain their evaluated value before being passed as
/// regular arguments to other functions. This test does so explicitly.
/// </para>
/// <para>
/// See <see cref="CrossModuleZeroArgDeclarationRegressionTests"/> for an
/// even tighter reproducer that strips the record update, the
/// <c>let</c>-binding, the record literal and the tuple — and proves that
/// none of them are required to trigger the same overflow.
/// </para>
/// </summary>
public class LanguageServiceCompileToPineVMRegressionTests
{
    /// <summary>
    /// Minimal reproducer module. The <c>dictInsertThenRecordUpdate</c> function:
    /// <list type="bullet">
    ///   <item>Takes a LanguageServiceState as argument (<c>stateBefore</c>),</item>
    ///   <item>Performs <c>Dict.insert</c> into <c>stateBefore.documentCache</c>,</item>
    ///   <item>Returns a record update <c>{ stateBefore | documentCache = ... }</c>.</item>
    /// </list>
    /// When compiled and evaluated, this triggers infinite self-recursion.
    /// </summary>
    private const string MinimalReproducerModule =
        """"
        module MinimalReproducer exposing (..)

        import LanguageService
        import LanguageServiceInterface
        import Dict


        initState : LanguageService.LanguageServiceState
        initState =
            LanguageService.initLanguageServiceState []


        dictInsertThenRecordUpdate :
            String
            -> String
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        dictInsertThenRecordUpdate fileUri fileText stateBefore =
            let
                documentCache =
                    Dict.insert
                        fileUri
                        { sourceBase64 = ""
                        , textContent =
                            Just
                                { text = fileText
                                , parsedFile = Nothing
                                }
                        , parsedFileLastSuccess = Nothing
                        }
                        stateBefore.documentCache
            in
            ( LanguageServiceInterface.WorkspaceSummaryResponse
            , { stateBefore
                | documentCache = documentCache
              }
            )
        """"
        ;

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    /// <summary>
    /// Builds a parsed interactive environment containing the bundled language
    /// service sources together with the minimal reproducer test module.
    /// Uses the same compilation settings as the main LanguageService tests:
    /// <c>disableInlining: true, maxOptimizationRounds: 1</c>.
    /// </summary>
    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment BuildEnvWithReproducerModule()
    {
        var bundledTree =
            BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var kernelModulesTree =
            bundledTree
            .GetNodeAtPath(["elm-kernel-modules"])
            ?? throw new Exception("Did not find elm-kernel-modules");

        var elmSyntaxSrcTree =
            bundledTree
            .GetNodeAtPath(["elm-syntax", "src"])
            ?? throw new Exception("Did not find elm-syntax/src");

        var elmInElmSrcTree =
            bundledTree
            .GetNodeAtPath(["src"])
            ?? throw new Exception("Did not find src");

        var otherLibraryModulesTree =
            bundledTree
            .GetNodeAtPath(["other-library-modules"]);

        var mergedTree = kernelModulesTree;

        foreach (var (path, file) in elmSyntaxSrcTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        foreach (var (path, file) in elmInElmSrcTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        if (otherLibraryModulesTree is not null)
        {
            foreach (var (path, file) in otherLibraryModulesTree.EnumerateFilesTransitive())
            {
                mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
            }
        }

        var treeWithTest =
            mergedTree.SetNodeAtPathSorted(
                ["MinimalReproducer.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(MinimalReproducerModule)));

        var rootFilePaths =
            treeWithTest.EnumerateFilesTransitive()
            .Where(b => b.path[^1].Equals("MinimalReproducer.elm", StringComparison.OrdinalIgnoreCase))
            .Select(b => (System.Collections.Generic.IReadOnlyList<string>)b.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                treeWithTest,
                rootFilePaths: rootFilePaths,
                disableInlining: true,
                maxOptimizationRounds: 1)
            .Map(r => r.compiledEnvValue)
            .Extract(err => throw new Exception("Failed compiling: " + err));

        return
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing: " + err));
    }

    private static PineValue GetFunction(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment env,
        string name)
    {
        var module = env.Modules.First(m => m.moduleName == "MinimalReproducer");
        return module.moduleContent.FunctionDeclarations[name];
    }

    private static (PineValue result, EvaluationReport report) ApplyWithProfiling(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment env,
        string functionName,
        PineValue[] arguments)
    {
        var funcValue = GetFunction(env, functionName);

        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(
                funcValue,
                parseCache: new PineVMParseCache())
            .Extract(err => throw new Exception("Failed to parse function record for '" + functionName + "': " + err));

        var evalArgs =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(functionRecord, arguments)
            .Extract(err => throw new Exception("Failed to compose eval args for '" + functionName + "': " + err));

        var report =
            s_vm.EvaluateExpressionOnCustomStack(
                evalArgs.expression,
                evalArgs.environment,
                config: ElmCompilerTestHelper.DefaultTestEvaluationConfig)
            .Extract(err => throw new Exception("Failed eval for '" + functionName + "': " + err));

        return (report.ReturnValue.Evaluate(), report);
    }

    private static ElmValue ElmString(string s) =>
        ElmValue.StringInstance(s);

    /// <summary>
    /// Verifies that the
    /// <c>(Dict.insert into stateBefore.documentCache) + record update</c>
    /// pattern compiles and evaluates without recursion when
    /// <c>initState</c> is properly evaluated to a record value first.
    /// <para>
    /// Originally surfaced as
    /// <c>Stack depth limit exceeded: 100_000</c> with the same Pine
    /// expression hash <c>a1b3551e</c> repeating on every reported stack
    /// frame. The root cause is documented on the enclosing class.
    /// </para>
    /// </summary>
    [Fact]
    public void Dict_insert_with_record_update_on_LanguageServiceState_does_not_overflow()
    {
        var env = BuildEnvWithReproducerModule();

        // initState is a 0-argument top-level binding. Under the current
        // "Approach A1" compilation, its raw declaration value is a
        // function-record wrapper; evaluate it to obtain the actual
        // LanguageServiceState record value before passing it as an argument.
        var initStateFr =
            FunctionRecord.ParseFunctionRecordTagged(
                GetFunction(env, "initState"),
                parseCache: new PineVMParseCache())
            .Extract(err => throw new Exception("Failed to parse initState: " + err));

        var initStateEvalArgs =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(initStateFr, [])
            .Extract(err => throw new Exception("Failed to compose initState eval args: " + err));

        var initStatePine =
            s_vm.EvaluateExpressionOnCustomStack(
                initStateEvalArgs.expression,
                initStateEvalArgs.environment,
                config: ElmCompilerTestHelper.DefaultTestEvaluationConfig)
            .Extract(err => throw new Exception("Failed to evaluate initState: " + err))
            .ReturnValue.Evaluate();

        var (_, report) =
            ApplyWithProfiling(
                env,
                "dictInsertThenRecordUpdate",
                [
                    ElmValueEncoding.ElmValueAsPineValue(ElmString("test.elm")),
                    ElmValueEncoding.ElmValueAsPineValue(ElmString("module M exposing (..)\n")),
                    initStatePine,
                ]);

        report.Counters.InstructionCount.Should().BeGreaterThan(0);
    }
}
