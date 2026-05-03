using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Tests exercising the Elm language service (<c>LanguageService.elm</c>) via
/// <c>initLanguageServiceState</c> and <c>handleRequestInCurrentWorkspace</c>.
/// Each test chains requests (adding workspace files, then querying) and asserts on
/// both the rendered Elm expression of the response and the runtime cost snapshot.
/// </summary>
public class ElmLanguageServiceTests
{
    /// <summary>
    /// Wrapper module that imports the real language service and exposes thin
    /// functions for exercising it from C# tests.
    /// </summary>
    private const string TestModuleText =
        """"
        module ElmLanguageServiceTestModule exposing (..)

        import LanguageService
        import LanguageServiceInterface


        initState : LanguageService.LanguageServiceState
        initState =
            LanguageService.initLanguageServiceState []


        handleRequest :
            LanguageServiceInterface.Request
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        handleRequest request state =
            case LanguageService.handleRequestInCurrentWorkspace request state of
                ( Ok response, newState ) ->
                    ( response, newState )

                ( Err err, newState ) ->
                    ( LanguageServiceInterface.ProvideHoverResponse [ "Error: " ++ err ], newState )


        addWorkspaceFile :
            String
            -> String
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        addWorkspaceFile filePath fileText state =
            handleRequest
                (LanguageServiceInterface.AddWorkspaceFileRequest
                    filePath
                    { asBase64 = ""
                    , asText = Just fileText
                    }
                )
                state


        textDocumentReferences :
            String
            -> Int
            -> Int
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        textDocumentReferences filePath lineNumber column state =
            handleRequest
                (LanguageServiceInterface.TextDocumentReferencesRequest
                    { fileLocation = LanguageServiceInterface.WorkspaceFileLocation filePath
                    , positionLineNumber = lineNumber
                    , positionColumn = column
                    }
                )
                state
        """"
        ;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(() => BuildLanguageServiceEnv(disableInlining: true));

    /// <summary>
    /// Builds the parsed interactive environment containing the bundled language
    /// service sources together with <see cref="TestModuleText"/>.
    /// Parameters control how aggressively the optimization pipeline is applied —
    /// see <see cref="ElmCompiler.CompileInteractiveEnvironment(FileTree, IReadOnlyList{IReadOnlyList{string}}, bool, int, bool, IReadOnlyList{DeclQualifiedName})"/>.
    /// When <paramref name="maxOptimizationRounds"/> is <c>null</c>, the call
    /// inherits the API default (<see cref="ElmCompiler.MaxOptimizationRoundsDefault"/>),
    /// matching the configuration used by the productive
    /// <c>LanguageServiceCompilation.CompileLanguageServiceEnv</c> path.
    /// </summary>
    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment BuildLanguageServiceEnv(
        bool disableInlining,
        int? maxOptimizationRounds = null)
    {
        var bundledTree =
            BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var kernelModulesTree =
            BundledFiles.ElmKernelModulesDefault.Value;

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

        // Start from kernel modules and merge elm-syntax, elm-in-elm sources.
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

        // Add our test module
        var treeWithTest =
            mergedTree.SetNodeAtPathSorted(
                ["ElmLanguageServiceTestModule.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

        var rootFilePaths =
            treeWithTest.EnumerateFilesTransitive()
            .Where(
                b =>
                b.path[^1].Equals("ElmLanguageServiceTestModule.elm", StringComparison.OrdinalIgnoreCase))
            .Select(b => (IReadOnlyList<string>)b.path)
            .ToList();

        var compiledEnv =
            (maxOptimizationRounds is { } rounds
                ?
                ElmCompiler.CompileInteractiveEnvironment(
                    treeWithTest,
                    rootFilePaths: rootFilePaths,
                    disableInlining: disableInlining,
                    maxOptimizationRounds: rounds)
                :
                ElmCompiler.CompileInteractiveEnvironment(
                    treeWithTest,
                    rootFilePaths: rootFilePaths,
                    disableInlining: disableInlining))
            .Map(r => r.compiledEnvValue)
            .Extract(err => throw new Exception("Failed compiling: " + err));

        return
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing: " + err));
    }

    private static PineValue GetTestFunction(string name) =>
        GetTestFunction(s_env.Value, name);

    private static PineValue GetTestFunction(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment env,
        string name) =>
        env.Modules
        .First(m => m.moduleName is "ElmLanguageServiceTestModule")
        .moduleContent.FunctionDeclarations[name];

    /// <summary>
    /// Evaluates a 0-argument top-level declaration to its actual result.
    /// <para>
    /// Under the current "Approach A1" compilation, the value cached in
    /// <see cref="ElmInteractiveEnvironment.ElmModule.FunctionDeclarations"/>
    /// for any top-level binding — including 0-argument ones — is a
    /// function-record wrapper, not the binding's evaluated result. Callers
    /// that want the binding's *value* (e.g. tests that pass <c>initState</c>
    /// as an argument to another function) must invoke the wrapper with zero
    /// arguments first; otherwise the function-record wrapper is silently fed
    /// into the callee in place of the expected value, which can lead to
    /// runtime self-recursion when the callee performs structural operations
    /// (field access, pattern matching) on the wrapper.
    /// </para>
    /// </summary>
    private static PineValue EvaluateZeroArgTestDeclaration(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment env,
        string name)
    {
        var declarationValue = GetTestFunction(env, name);

        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(
                declarationValue,
                parseCache: new PineVMParseCache())
            .Extract(
                err => throw new Exception(
                    "Failed to parse function record for '" + name + "': " + err));

        if (functionRecord.ParameterCount != 0)
        {
            throw new InvalidOperationException(
                nameof(EvaluateZeroArgTestDeclaration) + " requires a 0-arg declaration, " +
                "but '" + name + "' has " + functionRecord.ParameterCount + " parameters.");
        }

        var evalArgs =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(functionRecord, [])
            .Extract(
                err => throw new Exception(
                    "Failed to compose eval args for '" + name + "': " + err));

        return
            s_vm.EvaluateExpressionOnCustomStack(
                evalArgs.expression,
                evalArgs.environment,
                config: ElmCompilerTestHelper.DefaultTestEvaluationConfig)
            .Extract(
                err => throw new Exception(
                    "Failed to evaluate 0-arg declaration '" + name + "': " + err))
            .ReturnValue.Evaluate();
    }

    private static PineValue EvaluateZeroArgTestDeclaration(string name) =>
        EvaluateZeroArgTestDeclaration(s_env.Value, name);

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ElmString(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    /// <summary>
    /// Applies an Elm function (by name) to the given PineValue arguments with profiling.
    /// Parses the function value into a <see cref="CodeAnalysis.FunctionRecord"/> and uses
    /// <see cref="ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr"/> to compose
    /// the correct expression/environment, then evaluates with profiling on the custom stack.
    /// </summary>
    private static (PineValue result, EvaluationReport report) ApplyWithProfiling(
        string functionName,
        PineValue[] arguments) =>
        ApplyWithProfiling(s_env.Value, functionName, arguments);

    private static (PineValue result, EvaluationReport report) ApplyWithProfiling(
        ElmInteractiveEnvironment.ParsedInteractiveEnvironment env,
        string functionName,
        PineValue[] arguments)
    {
        var funcValue = GetTestFunction(env, functionName);

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

    /// <summary>
    /// ModuleA workspace file used by the
    /// <see cref="References_request_finds_usage_across_modules"/> scenario and
    /// the parallel intermediate-stage check
    /// <see cref="References_request_finds_usage_across_modules_via_interpreter"/>.
    /// Note the layout: <c>helper</c> appears in the exposing list at row 1
    /// and as a top-level declaration starting at row 3 (1-indexed).
    /// </summary>
    private const string ReferencesScenario_ModuleAText =
        """
        module ModuleA exposing (helper)

        helper : Int -> Int
        helper x =
            x + 1

        """;

    /// <summary>
    /// ModuleB workspace file used by the references scenario.
    /// <c>doWork</c> imports <c>ModuleA</c> and contains two call sites of
    /// <c>ModuleA.helper</c> on row 7 (1-indexed).
    /// </summary>
    private const string ReferencesScenario_ModuleBText =
        """
        module ModuleB exposing (doWork)

        import ModuleA

        doWork : Int -> Int
        doWork n =
            ModuleA.helper n + ModuleA.helper (n + 1)

        """;

    /// <summary>
    /// Position used by the references request: row 3 / column 1 lands on the
    /// <c>helper</c> name in the type signature
    /// <c>helper : Int -> Int</c> of <see cref="ReferencesScenario_ModuleAText"/>.
    /// (The Elm parser uses 1-indexed rows.)
    /// </summary>
    private const int ReferencesScenario_PositionLineNumber = 3;

    private const int ReferencesScenario_PositionColumn = 1;

    private const string ReferencesScenario_QueryFilePath = "src/ModuleA.elm";

    /// <summary>
    /// Expected rendered Elm-expression form of the language service response
    /// for the references scenario. Reports three references to <c>helper</c>:
    /// the original name in the <c>module ModuleA exposing (helper)</c>
    /// declaration, plus the unqualified <c>helper</c> portion of both
    /// <c>ModuleA.helper</c> call sites in <see cref="ReferencesScenario_ModuleBText"/>.
    /// This expectation matches what the language service produces when run
    /// through the <see cref="ElmSyntaxInterpreter"/> intermediate stage
    /// (see <see cref="References_request_finds_usage_across_modules_via_interpreter"/>).
    /// </summary>
    private const string ReferencesScenario_ExpectedResponse =
        """TextDocumentReferencesResponse [ { fileLocation = WorkspaceFileLocation "src/ModuleA.elm", range = { endColumn = 32, endLineNumber = 1, startColumn = 26, startLineNumber = 1 } }, { fileLocation = WorkspaceFileLocation "src/ModuleB.elm", range = { endColumn = 19, endLineNumber = 7, startColumn = 13, startLineNumber = 7 } }, { fileLocation = WorkspaceFileLocation "src/ModuleB.elm", range = { endColumn = 38, endLineNumber = 7, startColumn = 32, startLineNumber = 7 } } ]""";

    /// <summary>
    /// Simulates a language client workflow:
    /// 1. Initialize language service state
    /// 2. Add two workspace files (one module references a function from the other)
    /// 3. Send a TextDocumentReferencesRequest for the referenced function
    /// 4. Assert on the rendered response and aggregate performance counters
    /// <para>
    /// This test is currently skipped because the compile-to-PineVM pipeline produces
    /// malformed bytecode for <c>addWorkspaceFile</c> (the wrapper around
    /// <c>LanguageService.handleRequestInCurrentWorkspace</c> with an
    /// <c>AddWorkspaceFileRequest</c>): the VM fails at the very first call with
    /// <c>Failed eval for 'addWorkspaceFile': Failed to parse expression from value:
    /// Unexpected number of items in list: Not 2 but 0 — expressionValue is string ''
    /// — environmentValue is not a string</c>. This indicates the bytecode emits an
    /// <c>EvalExpr</c> opcode whose (expression, environment) pair is the empty string
    /// rather than a serialized expression. The same scenario succeeds when run via
    /// the parallel intermediate-stage check
    /// <see cref="References_request_finds_usage_across_modules_via_interpreter"/>,
    /// proving that the language service Elm code itself is correct and the defect
    /// lies in the compile-to-PineVM stage. See <c>ElmSyntaxInterpreter-language-service-gaps.md</c>.
    /// </para>
    /// </summary>
    [Fact]
    public void References_request_finds_usage_across_modules()
    {
        var reports = new List<EvaluationReport>();

        // initState is a 0-argument top-level binding. Under the current
        // "Approach A1" compilation, its raw declaration value is a
        // function-record wrapper; we must evaluate it to obtain the actual
        // initial LanguageServiceState record value.
        var initStatePine = EvaluateZeroArgTestDeclaration("initState");

        var (addModuleAResult, addModuleAReport) =
            ApplyWithProfiling(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleA.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ReferencesScenario_ModuleAText)),
                initStatePine,
                ]);

        reports.Add(addModuleAReport);

        var stateAfterModuleA =
            ((PineValue.ListValue)addModuleAResult).Items.Span[1];

        var (addModuleBResult, addModuleBReport) =
            ApplyWithProfiling(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleB.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ReferencesScenario_ModuleBText)),
                stateAfterModuleA,
                ]);

        reports.Add(addModuleBReport);

        var stateAfterModuleB =
            ((PineValue.ListValue)addModuleBResult).Items.Span[1];

        var (refsResult, refsReport) =
            ApplyWithProfiling(
                "textDocumentReferences",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ReferencesScenario_QueryFilePath)),
                ElmValueEncoding.ElmValueAsPineValue(Integer(ReferencesScenario_PositionLineNumber)),
                ElmValueEncoding.ElmValueAsPineValue(Integer(ReferencesScenario_PositionColumn)),
                stateAfterModuleB,
                ]);

        reports.Add(refsReport);

        // Extract response from tuple (first element)
        var responsePine =
            ((PineValue.ListValue)refsResult).Items.Span[0];

        var responseElmValue =
            ElmValueEncoding.PineValueAsElmValue(responsePine, null, null)
            .Extract(err => throw new Exception("Failed to decode response: " + err));

        var responseAsExpression =
            ElmValue.RenderAsElmExpression(responseElmValue);

        responseAsExpression.expressionString.Should().Be(ReferencesScenario_ExpectedResponse);

        var aggregateCounters =
            PerformanceCounters.Aggregate(
                reports.Select(r => r.Counters));

        PerformanceCountersFormatting.FormatCounts(aggregateCounters).Should().Be(
            """
            InvocationCount: 43_582
            BuildListCount: 307_695
            LoopIterationCount: 0
            InstructionCount: 1_107_334
            """);
    }

    /// <summary>
    /// Intermediate-stage check that runs the same input scenario as
    /// <see cref="References_request_finds_usage_across_modules"/> through
    /// <see cref="ElmInterpreter.ParseAndInterpret(string, IReadOnlyList{string})"/>
    /// instead of through the compiled Pine VM.
    ///
    /// <para>
    /// This isolates whether a divergence from the expected language service
    /// behaviour is introduced by:
    /// </para>
    /// <list type="bullet">
    ///   <item>the language service Elm code itself, or one of the
    ///   parser/canonicalization stages — both of which are exercised here
    ///   through <see cref="ElmInterpreter"/> — in which case this test will
    ///   also fail; or</item>
    ///   <item>the compile-to-PineVM step or the runtime VM — in which case
    ///   this test passes while
    ///   <see cref="References_request_finds_usage_across_modules"/> fails.</item>
    /// </list>
    ///
    /// <para>
    /// The wrapper module dispatches the same three-step workflow
    /// (init &#8594; add ModuleA &#8594; add ModuleB &#8594; references query)
    /// embedding the same <see cref="ReferencesScenario_ModuleAText"/>,
    /// <see cref="ReferencesScenario_ModuleBText"/>,
    /// <see cref="ReferencesScenario_QueryFilePath"/>,
    /// <see cref="ReferencesScenario_PositionLineNumber"/>, and
    /// <see cref="ReferencesScenario_PositionColumn"/> as Elm-side literals.
    /// </para>
    /// </summary>
    [Fact]
    public void References_request_finds_usage_across_modules_via_interpreter()
    {
        var wrapperModuleText =
            BuildReferencesScenarioWrapperModule(
                moduleAText: ReferencesScenario_ModuleAText,
                moduleBText: ReferencesScenario_ModuleBText,
                queryFilePath: ReferencesScenario_QueryFilePath,
                positionLineNumber: ReferencesScenario_PositionLineNumber,
                positionColumn: ReferencesScenario_PositionColumn);

        var modulesWithWrapper =
            new List<string>(s_compilerSourceModules.Value)
            {
                wrapperModuleText,
            };

        var result =
            Core.Elm.ElmSyntax.ElmSyntaxInterpreter.ParseAndInterpret(
                "ElmLanguageServiceReferencesScenario.referencesScenarioResponse",
                modulesWithWrapper);

        var value =
            result.Extract(err => throw new Exception(err.ToString()));

        var rendered =
            ElmValue.RenderAsElmExpression(value).expressionString;

        rendered.Should().Be(ReferencesScenario_ExpectedResponse);
    }

    /// <summary>
    /// Bundled compiler source modules (kernel, elm-syntax, elm-in-elm src,
    /// other-library-modules) loaded as a flat list of source texts ready to
    /// be passed to
    /// <see cref="ElmSyntaxInterpreter.ParseAndInterpret(string, IReadOnlyList{string})"/>.
    /// </summary>
    private static readonly Lazy<IReadOnlyList<string>> s_compilerSourceModules =
        new(
            () =>
            {
                var bundledTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value;

                var modules = new List<string>();

                AppendModuleTextsAtPath(BundledFiles.ElmKernelModulesDefault.Value, [], modules);
                AppendModuleTextsAtPath(bundledTree, ["elm-syntax", "src"], modules);
                AppendModuleTextsAtPath(bundledTree, ["src"], modules);
                AppendModuleTextsAtPath(bundledTree, ["other-library-modules"], modules, optional: true);

                return modules;
            });

    private static void AppendModuleTextsAtPath(
        FileTree root,
        IReadOnlyList<string> path,
        List<string> sink,
        bool optional = false)
    {
        var node = root.GetNodeAtPath(path);

        if (node is null)
        {
            if (optional)
                return;

            throw new Exception("Did not find: " + string.Join("/", path));
        }

        foreach (var (_, file) in node.EnumerateFilesTransitive())
        {
            sink.Add(Encoding.UTF8.GetString(file.Span));
        }
    }

    /// <summary>
    /// Builds the wrapper Elm module that drives the references scenario
    /// through <see cref="ElmInterpreter.ParseAndInterpret(string, IReadOnlyList{string})"/>.
    /// The wrapper threads the language service state through three requests
    /// (add ModuleA, add ModuleB, references query) and exposes the final
    /// response as the top-level value <c>referencesScenarioResponse</c>.
    /// </summary>
    private static string BuildReferencesScenarioWrapperModule(
        string moduleAText,
        string moduleBText,
        string queryFilePath,
        int positionLineNumber,
        int positionColumn)
    {
        return
            "module ElmLanguageServiceReferencesScenario exposing (..)\n"
            + "\n"
            + "import LanguageService\n"
            + "import LanguageServiceInterface\n"
            + "\n"
            + "\n"
            + "moduleAText : String\n"
            + "moduleAText =\n"
            + "    " + EncodeAsElmStringLiteral(moduleAText) + "\n"
            + "\n"
            + "\n"
            + "moduleBText : String\n"
            + "moduleBText =\n"
            + "    " + EncodeAsElmStringLiteral(moduleBText) + "\n"
            + "\n"
            + "\n"
            + "addFile :\n"
            + "    String\n"
            + "    -> String\n"
            + "    -> LanguageService.LanguageServiceState\n"
            + "    -> LanguageService.LanguageServiceState\n"
            + "addFile path text state =\n"
            + "    let\n"
            + "        ( _, newState ) =\n"
            + "            LanguageService.handleRequestInCurrentWorkspace\n"
            + "                (LanguageServiceInterface.AddWorkspaceFileRequest\n"
            + "                    path\n"
            + "                    { asBase64 = \"\", asText = Just text }\n"
            + "                )\n"
            + "                state\n"
            + "    in\n"
            + "    newState\n"
            + "\n"
            + "\n"
            + "referencesScenarioResponse : LanguageServiceInterface.Response\n"
            + "referencesScenarioResponse =\n"
            + "    let\n"
            + "        state0 : LanguageService.LanguageServiceState\n"
            + "        state0 =\n"
            + "            LanguageService.initLanguageServiceState []\n"
            + "\n"
            + "        state1 : LanguageService.LanguageServiceState\n"
            + "        state1 =\n"
            + "            addFile \"src/ModuleA.elm\" moduleAText state0\n"
            + "\n"
            + "        state2 : LanguageService.LanguageServiceState\n"
            + "        state2 =\n"
            + "            addFile \"src/ModuleB.elm\" moduleBText state1\n"
            + "\n"
            + "        ( serviceResult, _ ) =\n"
            + "            LanguageService.handleRequestInCurrentWorkspace\n"
            + "                (LanguageServiceInterface.TextDocumentReferencesRequest\n"
            + "                    { fileLocation = LanguageServiceInterface.WorkspaceFileLocation "
            + EncodeAsElmStringLiteral(queryFilePath) + "\n"
            + "                    , positionLineNumber = " + positionLineNumber + "\n"
            + "                    , positionColumn = " + positionColumn + "\n"
            + "                    }\n"
            + "                )\n"
            + "                state2\n"
            + "    in\n"
            + "    case serviceResult of\n"
            + "        Ok response ->\n"
            + "            response\n"
            + "\n"
            + "        Err err ->\n"
            + "            LanguageServiceInterface.ProvideHoverResponse [ \"Error: \" ++ err ]\n";
    }

    /// <summary>
    /// Encodes <paramref name="value"/> as an Elm string literal, escaping
    /// backslashes, double quotes, and embedded newlines/tabs so that the
    /// produced Elm source text is well-formed regardless of the contents of
    /// <paramref name="value"/>.
    /// </summary>
    private static string EncodeAsElmStringLiteral(string value)
    {
        var sb = new StringBuilder(value.Length + 2);
        sb.Append('"');

        foreach (var ch in value)
        {
            switch (ch)
            {
                case '\\':
                    sb.Append("\\\\");
                    break;

                case '"':
                    sb.Append("\\\"");
                    break;

                case '\n':
                    sb.Append("\\n");
                    break;

                case '\r':
                    sb.Append("\\r");
                    break;

                case '\t':
                    sb.Append("\\t");
                    break;

                default:
                    sb.Append(ch);
                    break;
            }
        }

        sb.Append('"');
        return sb.ToString();
    }

    /// <summary>
    /// Analog of <see cref="References_request_finds_usage_across_modules"/> that
    /// targets the optimization pipeline behind <c>ElmCompiler.ApplyOptimizationPipelineWithStageResults</c>.
    /// The same end-to-end language-service scenario (initialize state → add two
    /// workspace files → request references for <c>helper</c>) is compiled and
    /// executed multiple times — each time with a different <c>maxRounds</c>
    /// value (0, 1, 2, 3) for the optimization pipeline. The rendered Elm
    /// expression of the references response (or any error message thrown) is
    /// captured per iteration and all results are compared. If any optimization
    /// iteration produces a response that differs from the others, the test
    /// fails and reports both the differing values and the iteration counts that
    /// produced them, surfacing optimization passes that break program
    /// semantics.
    /// </summary>
    [Fact]
    public void References_request_finds_usage_across_modules_optimization_pipeline_iterations()
    {
        var maxRoundsValues = new[] { 0, 1, 2, 3 };

        // Capture the rendered response (or the error message) per maxRounds value.
        var renderedPerMaxRounds = new Dictionary<int, string>();

        foreach (var maxRounds in maxRoundsValues)
        {
            try
            {
                renderedPerMaxRounds[maxRounds] = RunReferencesScenario(maxRounds);
            }
            catch (Exception ex)
            {
                // Normalize the captured exception message so that a successful
                // bisection across optimization rounds is not foiled by purely
                // statistical noise: when an evaluation error bubbles up via
                // <see cref="EvaluationError.ToString"/> its trailing
                // " - stack frames: N - instructions: N - invocations: N -
                // build lists: N - loop iterations: N" suffix encodes the
                // VM performance counters of the failing run, which legitimately
                // change as the optimization pipeline does more or less work.
                // The semantic part of the error is everything before that
                // suffix, so we strip the suffix here before comparing across
                // iterations.
                renderedPerMaxRounds[maxRounds] =
                    "ERROR: " + StripEvaluationErrorPerformanceCounters(ex.Message);
            }
        }

        // Compare every iteration's result against the baseline (maxRounds = 0)
        // — any divergence indicates that an optimization round broke the
        // program code.
        var baselineMaxRounds = maxRoundsValues[0];
        var baseline = renderedPerMaxRounds[baselineMaxRounds];

        var divergences = new List<string>();

        foreach (var maxRounds in maxRoundsValues)
        {
            if (maxRounds == baselineMaxRounds)
                continue;

            var actual = renderedPerMaxRounds[maxRounds];

            if (actual != baseline)
            {
                divergences.Add(
                    "After " + maxRounds + " optimization round(s) the result " +
                    "differs from the baseline (" + baselineMaxRounds + " round(s)):\n" +
                    "  baseline (" + baselineMaxRounds + " round(s)): " + baseline + "\n" +
                    "  actual   (" + maxRounds + " round(s)): " + actual);
            }
        }

        divergences.Should().BeEmpty(
            "the optimization pipeline must not change the observable behaviour of the program — " +
            "each iteration of ApplyOptimizationPipelineWithStageResults should preserve program semantics");
    }

    /// <summary>
    /// Removes the performance-counter suffix that
    /// <see cref="EvaluationError.ToString"/>
    /// appends to its <c>Message</c> (i.e. everything starting from the first
    /// occurrence of <c>" - stack frames: "</c>). When an evaluation error is
    /// stringified through that <c>ToString</c> the resulting text mixes a
    /// stable semantic prefix (the actual error message) with run-dependent
    /// counter values (instruction / invocation / build-list / loop-iteration
    /// counts and the captured stack-frame depth). For tests that compare
    /// error messages across compiler configurations, those counter values
    /// would otherwise fool an exact-string comparison into reporting a
    /// divergence even when both runs hit the very same defect.
    /// Returns the original string unchanged if the suffix marker is not
    /// present.
    /// </summary>
    private static string StripEvaluationErrorPerformanceCounters(string message)
    {
        const string suffixMarker = " - stack frames: ";

        var suffixStart = message.IndexOf(suffixMarker, StringComparison.Ordinal);

        if (suffixStart < 0)
            return message;

        return message[..suffixStart];
    }

    /// <summary>
    /// Compiles the language-service test environment with the given
    /// <paramref name="maxOptimizationRounds"/> for
    /// <c>ApplyOptimizationPipelineWithStageResults</c> and runs the same
    /// references scenario as <see cref="References_request_finds_usage_across_modules"/>:
    /// initialize state, add ModuleA + ModuleB, then request references for the
    /// <c>helper</c> declaration. Returns the rendered Elm expression of the
    /// response.
    /// </summary>
    private static string RunReferencesScenario(int maxOptimizationRounds)
    {
        var env =
            BuildLanguageServiceEnv(
                disableInlining: false,
                maxOptimizationRounds: maxOptimizationRounds);

        // initState is a 0-argument top-level binding. Under the current
        // "Approach A1" compilation, its raw declaration value is a
        // function-record wrapper; we must evaluate it to obtain the actual
        // initial LanguageServiceState record value.
        var initStatePine = EvaluateZeroArgTestDeclaration(env, "initState");

        var moduleAText =
            """
            module ModuleA exposing (helper)


            helper : Int -> Int
            helper x =
                x + 1
            """;

        var (addModuleAResult, _) =
            ApplyWithProfiling(
                env,
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleA.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(moduleAText)),
                initStatePine,
                ]);

        var stateAfterModuleA =
            ((PineValue.ListValue)addModuleAResult).Items.Span[1];

        var moduleBText =
            """
            module ModuleB exposing (doWork)

            import ModuleA


            doWork : Int -> Int
            doWork n =
                ModuleA.helper n + ModuleA.helper (n + 1)
            """;

        var (addModuleBResult, _) =
            ApplyWithProfiling(
                env,
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleB.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(moduleBText)),
                stateAfterModuleA,
                ]);

        var stateAfterModuleB =
            ((PineValue.ListValue)addModuleBResult).Items.Span[1];

        // Request references for `helper`. Row 4 lands on the `helper` name in
        // the type signature (`helper : Int -> Int`) of `src/ModuleA.elm`,
        // matching the position used by the working analog in
        // `LanguageServiceReferencesTests.References_request_finds_usage_across_modules`.
        var (refsResult, _) =
            ApplyWithProfiling(
                env,
                "textDocumentReferences",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleA.elm")),
                ElmValueEncoding.ElmValueAsPineValue(Integer(4)),
                ElmValueEncoding.ElmValueAsPineValue(Integer(1)),
                stateAfterModuleB,
                ]);

        var responsePine =
            ((PineValue.ListValue)refsResult).Items.Span[0];

        var responseElmValue =
            ElmValueEncoding.PineValueAsElmValue(responsePine, null, null)
            .Extract(err => throw new Exception("Failed to decode response: " + err));

        return ElmValue.RenderAsElmExpression(responseElmValue).expressionString;
    }
}
