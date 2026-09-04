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


        textDocumentRename :
            String
            -> Int
            -> Int
            -> String
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        textDocumentRename filePath lineNumber column newName state =
            handleRequest
                (LanguageServiceInterface.TextDocumentRenameRequest
                    { filePath = filePath
                    , positionLineNumber = lineNumber
                    , positionColumn = column
                    , newName = newName
                    }
                )
                state


        addPackageModule :
            String
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        addPackageModule moduleText state =
            handleRequest
                (LanguageServiceInterface.AddElmPackageVersionRequest
                    (LanguageServiceInterface.ElmPackageVersion019Identifer
                        "author/package"
                        "1.0.0"
                    )
                    [ ( [ "src", "PackageModule.elm" ]
                      , { asBase64 = "", asText = Just moduleText }
                      )
                    ]
                )
                state


        provideHoverWorkspace :
            String
            -> Int
            -> Int
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        provideHoverWorkspace filePath lineNumber column state =
            handleRequest
                (LanguageServiceInterface.ProvideHoverRequest
                    { fileLocation = LanguageServiceInterface.WorkspaceFileLocation filePath
                    , positionLineNumber = lineNumber
                    , positionColumn = column
                    }
                )
                state


        provideHoverPackage :
            Int
            -> Int
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        provideHoverPackage lineNumber column state =
            handleRequest
                (LanguageServiceInterface.ProvideHoverRequest
                    { fileLocation =
                        LanguageServiceInterface.ElmPackageFileLocation
                            (LanguageServiceInterface.ElmPackageVersion019Identifer
                                "author/package"
                                "1.0.0"
                            )
                            [ "src", "PackageModule.elm" ]
                    , positionLineNumber = lineNumber
                    , positionColumn = column
                    }
                )
                state


        provideDefinitionWorkspace :
            String
            -> Int
            -> Int
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        provideDefinitionWorkspace filePath lineNumber column state =
            handleRequest
                (LanguageServiceInterface.ProvideDefinitionRequest
                    { fileLocation = LanguageServiceInterface.WorkspaceFileLocation filePath
                    , positionLineNumber = lineNumber
                    , positionColumn = column
                    }
                )
                state


        provideCompletionItems :
            String
            -> Int
            -> Int
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        provideCompletionItems filePath lineNumber column state =
            handleRequest
                (LanguageServiceInterface.ProvideCompletionItemsRequest
                    { filePathOpenedInEditor = filePath
                    , cursorLineNumber = lineNumber
                    , cursorColumn = column
                    }
                )
                state


        textDocumentReferencesPackage :
            Int
            -> Int
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        textDocumentReferencesPackage lineNumber column state =
            handleRequest
                (LanguageServiceInterface.TextDocumentReferencesRequest
                    { fileLocation =
                        LanguageServiceInterface.ElmPackageFileLocation
                            (LanguageServiceInterface.ElmPackageVersion019Identifer
                                "author/package"
                                "1.0.0"
                            )
                            [ "src", "PackageModule.elm" ]
                    , positionLineNumber = lineNumber
                    , positionColumn = column
                    }
                )
                state


        textDocumentSymbol :
            String
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        textDocumentSymbol filePath state =
            handleRequest
                (LanguageServiceInterface.TextDocumentSymbolRequest filePath)
                state
        """"
        ;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(() => BuildLanguageServiceEnv(disableInlining: false));

    /// <summary>
    /// Builds the parsed interactive environment containing the bundled language
    /// service sources together with <see cref="TestModuleText"/>.
    /// </summary>
    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment BuildLanguageServiceEnv(
        bool disableInlining)
    {
        var bundledTree =
            BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var kernelModulesTree =
            BundledFiles.ElmKernelModulesDefault.Value;

        var elmSyntaxSrcTree =
            bundledTree
            .GetNodeAtPath(["pine-elm-syntax", "src"])
            ?? throw new Exception("Did not find pine-elm-syntax/src");

        var elmInElmSrcTree =
            bundledTree
            .GetNodeAtPath(["src"])
            ?? throw new Exception("Did not find src");

        var otherLibraryModulesTree =
            bundledTree
            .GetNodeAtPath(["other-library-modules"]);

        // Start from kernel modules and merge pine-elm-syntax, elm-in-elm sources.
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

        var syntaxOptimization =
            disableInlining
            ?
            (ElmSyntaxOptimizationConfig)new ElmSyntaxOptimizationConfig.SyntaxOptimizationDisabled()
            :
            new ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                treeWithTest,
                rootFilePaths: rootFilePaths,
                syntaxOptimization: syntaxOptimization)
            .Map(r => r.compiledEnvValue)
            .Extract(err => throw new Exception("Failed compiling: " + err));

        return
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing: " + err));
    }

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

        if (functionRecord.ParameterCount is not 0)
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
        ElmCompilerTestHelper.PineVMForProfiling(
            reportFunctionApplication: _ => { },
            enableTailRecursionOptimization: true);

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

    private static string RenderResponseFromResult(PineValue result)
    {
        var responsePine =
            ((PineValue.ListValue)result).Items.Span[0];

        var responseElmValue =
            ElmValueEncoding.PineValueAsElmValue(responsePine, null, null)
            .Extract(err => throw new Exception("Failed to decode response: " + err));

        return ElmValue.RenderAsElmExpression(responseElmValue).expressionString;
    }

    /// <summary>
    /// Same as <see cref="ApplyWithProfiling(string, PineValue[])"/> but additionally
    /// builds a per-evaluation <see cref="InvocationCountReport"/> by observing the
    /// <see cref="ReportEnteredStackFrame"/> events of this single evaluation task.
    /// This makes per-call histograms easy to aggregate across multiple
    /// applications via
    /// <see cref="InvocationCountReport.Aggregate(IEnumerable{InvocationCountReport})"/>.
    /// </summary>
    private static (PineValue result, EvaluationReport report, InvocationCountReport invocationCounts)
        ApplyWithProfilingAndInvocationCounts(
        string functionName,
        PineValue[] arguments)
    {
        var env = s_env.Value;

        var funcValue = GetTestFunction(env, functionName);

        var functionRecord =
            FunctionRecord.ParseFunctionRecordTagged(
                funcValue,
                parseCache: new PineVMParseCache())
            .Extract(err => throw new Exception("Failed to parse function record for '" + functionName + "': " + err));

        var evalArgs =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(functionRecord, arguments)
            .Extract(err => throw new Exception("Failed to compose eval args for '" + functionName + "': " + err));

        var invocationCountsBuilder = new InvocationCountReportBuilder();

        var report =
            s_vm.EvaluateExpressionOnCustomStack(
                evalArgs.expression,
                evalArgs.environment,
                config: ElmCompilerTestHelper.DefaultTestEvaluationConfig,
                reportEnteredStackFrame: invocationCountsBuilder.Add)
            .Extract(err => throw new Exception("Failed eval for '" + functionName + "': " + err));

        return (report.ReturnValue.Evaluate(), report, invocationCountsBuilder.ToReport());
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

    [Fact]
    public void AddWorkspaceFile_updates_hover_to_use_the_latest_valid_document()
    {
        const string FilePath = "src/Main.elm";

        const string OriginalContent =
            """
            module Main exposing (init, name)


            name = init


            init : Int
            init =
                0

            """;

        PineValue UpdateDocument(PineValue state, string content)
        {
            var (result, _) =
                ApplyWithProfiling(
                    "addWorkspaceFile",
                    [
                    ElmValueEncoding.ElmValueAsPineValue(ElmString(FilePath)),
                    ElmValueEncoding.ElmValueAsPineValue(ElmString(content)),
                    state,
                    ]);

            return ((PineValue.ListValue)result).Items.Span[1];
        }

        string HoverAtLine(PineValue state, long lineNumber)
        {
            var (result, _) =
                ApplyWithProfiling(
                    "provideHoverWorkspace",
                    [
                    ElmValueEncoding.ElmValueAsPineValue(ElmString(FilePath)),
                    ElmValueEncoding.ElmValueAsPineValue(Integer(lineNumber)),
                    ElmValueEncoding.ElmValueAsPineValue(Integer(9)),
                    state,
                    ]);

            return RenderResponseFromResult(result);
        }

        var state = EvaluateZeroArgTestDeclaration("initState");

        state = UpdateDocument(state, OriginalContent);

        HoverAtLine(state, lineNumber: 4).Should().Be(
            """ProvideHoverResponse [ "    init : Int" ]""");

        HoverAtLine(state, lineNumber: 5).Should().Be(
            """ProvideHoverResponse []""");

        var changedContent = "\n" + OriginalContent;

        state = UpdateDocument(state, changedContent);

        HoverAtLine(state, lineNumber: 5).Should().Be(
            """ProvideHoverResponse [ "    init : Int" ]""");

        HoverAtLine(state, lineNumber: 4).Should().Be(
            """ProvideHoverResponse []""");
    }

    [Fact]
    public void Hover_completion_definition_and_document_symbols_use_migrated_syntax()
    {
        const string PackageModuleText =
            """
            module PackageModule exposing (Choice(..), packageValue)

            {-| Package value docs -}
            packageValue : Int
            packageValue =
                41

            type Choice
                = First
                | Second Int

            """;

        const string WorkspaceModuleText =
            """
            module Main exposing (mainValue, LocalChoice(..))

            import PackageModule exposing (packageValue)

            mainValue =
                packageValue + 1

            type LocalChoice
                = LocalA
                | LocalB Int

            """;

        var initialState = EvaluateZeroArgTestDeclaration("initState");

        var (addPackageResult, _) =
            ApplyWithProfiling(
                "addPackageModule",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString(PackageModuleText)),
                initialState,
                ]);

        var stateAfterPackage =
            ((PineValue.ListValue)addPackageResult).Items.Span[1];

        var (addWorkspaceResult, _) =
            ApplyWithProfiling(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/Main.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(WorkspaceModuleText)),
                stateAfterPackage,
                ]);

        var state =
            ((PineValue.ListValue)addWorkspaceResult).Items.Span[1];

        string Request(string functionName, params ElmValue[] arguments)
        {
            var pineArguments =
                arguments
                .Select(ElmValueEncoding.ElmValueAsPineValue)
                .Append(state)
                .ToArray();

            return RenderResponseFromResult(ApplyWithProfiling(functionName, pineArguments).result);
        }

        var hoverWorkspace =
            Request(
                "provideHoverWorkspace",
                ElmString("src/Main.elm"),
                Integer(6),
                Integer(5));

        var hoverPackage =
            Request(
                "provideHoverPackage",
                Integer(4),
                Integer(2));

        var definition =
            Request(
                "provideDefinitionWorkspace",
                ElmString("src/Main.elm"),
                Integer(6),
                Integer(5));

        var completion =
            Request(
                "provideCompletionItems",
                ElmString("src/Main.elm"),
                Integer(7),
                Integer(1));

        var symbols =
            Request(
                "textDocumentSymbol",
                ElmString("src/Main.elm"));

        var references =
            Request(
                "textDocumentReferencesPackage",
                Integer(4),
                Integer(2));

        hoverWorkspace.Should().Be(
            """ProvideHoverResponse [ "    packageValue : Int\n\nPackage value docs" ]""");

        hoverPackage.Should().Be(
            """ProvideHoverResponse [ "    packageValue : Int\n\nPackage value docs" ]""");

        definition.Should().Be(
            """ProvideDefinitionResponse [ { fileLocation = ElmPackageFileLocation (ElmPackageVersion019Identifer "author/package" "1.0.0") [ "src", "PackageModule.elm" ], range = { endColumn = 7, endLineNumber = 6, startColumn = 1, startLineNumber = 4 } } ]""");

        completion.Should().Be(
            """ProvideCompletionItemsResponse [ { documentation = "", insertText = "PackageModule", kind = ModuleCompletionItemKind, label = "PackageModule" }, { documentation = "    LocalA\n\nA variant of the choice type `LocalChoice`\n\n    type LocalChoice\n        = LocalA\n        | LocalB Int", insertText = "LocalA", kind = EnumMemberCompletionItemKind, label = "LocalA" }, { documentation = "    LocalB\n\nA variant of the choice type `LocalChoice`\n\n    type LocalChoice\n        = LocalA\n        | LocalB Int", insertText = "LocalB", kind = EnumMemberCompletionItemKind, label = "LocalB" }, { documentation = "    type LocalChoice\n        = LocalA\n        | LocalB Int", insertText = "LocalChoice", kind = EnumCompletionItemKind, label = "LocalChoice" }, { documentation = "    mainValue", insertText = "mainValue", kind = FunctionCompletionItemKind, label = "mainValue" }, { documentation = "    packageValue : Int\n\nPackage value docs", insertText = "packageValue", kind = FunctionCompletionItemKind, label = "packageValue" } ]""");

        symbols.Should().Be(
            """TextDocumentSymbolResponse [ DocumentSymbol { children = [], kind = SymbolKind_Function, name = "mainValue", range = { endColumn = 21, endLineNumber = 6, startColumn = 1, startLineNumber = 5 }, selectionRange = { endColumn = 10, endLineNumber = 5, startColumn = 1, startLineNumber = 5 } }, DocumentSymbol { children = [ DocumentSymbol { children = [], kind = SymbolKind_EnumMember, name = "LocalA", range = { endColumn = 13, endLineNumber = 9, startColumn = 7, startLineNumber = 9 }, selectionRange = { endColumn = 13, endLineNumber = 9, startColumn = 7, startLineNumber = 9 } }, DocumentSymbol { children = [], kind = SymbolKind_EnumMember, name = "LocalB", range = { endColumn = 17, endLineNumber = 10, startColumn = 7, startLineNumber = 10 }, selectionRange = { endColumn = 13, endLineNumber = 10, startColumn = 7, startLineNumber = 10 } } ], kind = SymbolKind_Enum, name = "LocalChoice", range = { endColumn = 17, endLineNumber = 10, startColumn = 1, startLineNumber = 8 }, selectionRange = { endColumn = 17, endLineNumber = 8, startColumn = 6, startLineNumber = 8 } } ]""");

        references.Should().Be(
            """TextDocumentReferencesResponse [ { fileLocation = WorkspaceFileLocation "src/Main.elm", range = { endColumn = 17, endLineNumber = 6, startColumn = 5, startLineNumber = 6 } }, { fileLocation = ElmPackageFileLocation (ElmPackageVersion019Identifer "author/package" "1.0.0") [ "src", "PackageModule.elm" ], range = { endColumn = 56, endLineNumber = 1, startColumn = 44, startLineNumber = 1 } } ]""");
    }

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
        var invocationCountReports = new List<InvocationCountReport>();

        // initState is a 0-argument top-level binding. Under the current
        // "Approach A1" compilation, its raw declaration value is a
        // function-record wrapper; we must evaluate it to obtain the actual
        // initial LanguageServiceState record value.
        var initStatePine = EvaluateZeroArgTestDeclaration("initState");

        var (addModuleAResult, addModuleAReport, addModuleAInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleA.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ReferencesScenario_ModuleAText)),
                initStatePine,
                ]);

        reports.Add(addModuleAReport);
        invocationCountReports.Add(addModuleAInvocationCounts);

        var stateAfterModuleA =
            ((PineValue.ListValue)addModuleAResult).Items.Span[1];

        var (addModuleBResult, addModuleBReport, addModuleBInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleB.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ReferencesScenario_ModuleBText)),
                stateAfterModuleA,
                ]);

        reports.Add(addModuleBReport);
        invocationCountReports.Add(addModuleBInvocationCounts);

        var stateAfterModuleB =
            ((PineValue.ListValue)addModuleBResult).Items.Span[1];

        var (refsResult, refsReport, refsInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "textDocumentReferences",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ReferencesScenario_QueryFilePath)),
                ElmValueEncoding.ElmValueAsPineValue(Integer(ReferencesScenario_PositionLineNumber)),
                ElmValueEncoding.ElmValueAsPineValue(Integer(ReferencesScenario_PositionColumn)),
                stateAfterModuleB,
                ]);

        reports.Add(refsReport);
        invocationCountReports.Add(refsInvocationCounts);

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

        var aggregateInvocationCounts =
            InvocationCountReport.Aggregate(invocationCountReports);

        PerformanceCountersFormatting.FormatCounts(aggregateCounters).Should().Be(
            """
            InvocationCount: 3_368
            BuildListCount: 5_554
            LoopIterationCount: 2_511
            InstructionCount: 148_209
            """);

        InvocationCountReportFormatting.FormatCounts(aggregateInvocationCounts).Should().Be(
            """
            CompiledExpressionCount: 179
            InvocationCountTotal: 2_919
            InvocationCountAverage: 16
            InvocationCountPercentile10: 1
            InvocationCountMedian: 4
            InvocationCountPercentile90: 16
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
        var preparedApp = s_referencesScenarioPreparedApp.Value;

        var rootExpression =
            BuildReferencesScenarioRootExpression(
                workspaceFiles:
                [
                    ("src/ModuleA.elm", ReferencesScenario_ModuleAText),
                    ("src/ModuleB.elm", ReferencesScenario_ModuleBText),
                ],
                queryFilePath: ReferencesScenario_QueryFilePath,
                positionLineNumber: ReferencesScenario_PositionLineNumber,
                positionColumn: ReferencesScenario_PositionColumn);

        var interpreterStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var result =
            Core.Elm.ElmSyntax.ElmSyntaxInterpreter.InterpretAsElmValue(
                rootExpression,
                preparedApp);

        interpreterStopwatch.Stop();

        TestContext.Current.TestOutputHelper?.WriteLine(
            $"Interpreter execution time: {interpreterStopwatch.Elapsed}");

        var value =
            result.Extract(err => throw new Exception(err.ToString()));

        var rendered =
            ElmValue.RenderAsElmExpression(value).expressionString;

        rendered.Should().Be(ReferencesScenario_ExpectedResponse);
    }

    [Fact]
    public void References_resolution_limits_argument_patterns_to_their_function()
    {
        const string ModuleText =
            """
            module Main exposing (..)

            withSimple argument =
                argument

            outsideSimple =
                argument

            withTuple ( first, second ) =
                first + second

            outsideTuple =
                first + second

            """;

        var preparedApp = s_referencesScenarioPreparedApp.Value;

        string ReferencesAt(int line, int column)
        {
            var rootExpression =
                BuildReferencesScenarioRootExpression(
                    workspaceFiles: [("src/Main.elm", ModuleText)],
                    queryFilePath: "src/Main.elm",
                    positionLineNumber: line,
                    positionColumn: column);

            var result =
                Core.Elm.ElmSyntax.ElmSyntaxInterpreter.InterpretAsElmValue(
                    rootExpression,
                    preparedApp);

            return
                ElmValue.RenderAsElmExpression(
                    result.Extract(err => throw new Exception(err.ToString())))
                .expressionString;
        }

        ReferencesAt(line: 4, column: 5).Should().Be(
            """TextDocumentReferencesResponse [ { fileLocation = WorkspaceFileLocation "src/Main.elm", range = { endColumn = 13, endLineNumber = 4, startColumn = 5, startLineNumber = 4 } } ]""");

        ReferencesAt(line: 10, column: 5).Should().Be(
            """TextDocumentReferencesResponse [ { fileLocation = WorkspaceFileLocation "src/Main.elm", range = { endColumn = 10, endLineNumber = 10, startColumn = 5, startLineNumber = 10 } } ]""");
    }

    /// <summary>
    /// ModuleA workspace file used by the harder
    /// <see cref="References_request_finds_usage_across_modules_via_interpreter_challenging"/>
    /// scenario. Compared with <see cref="ReferencesScenario_ModuleAText"/> it
    /// adds a record type alias (<c>Settings</c>), a record-valued declaration
    /// (<c>defaultSettings</c>), and a record-access application inside
    /// <c>helper</c>, so the parser has to handle a record before reaching the
    /// queried <c>helper</c> declaration. <c>helper</c> appears in the exposing
    /// list at row 1 and as a top-level declaration starting at row 12
    /// (1-indexed).
    /// </summary>
    private const string ChallengingReferencesScenario_ModuleAText =
        """
        module ModuleA exposing (helper, Settings, defaultSettings)

        type alias Settings =
            { base : Int
            , step : Int
            }

        defaultSettings : Settings
        defaultSettings =
            { base = 10, step = 2 }

        helper : Int -> Int
        helper x =
            x + defaultSettings.step

        """;

    /// <summary>
    /// ModuleB workspace file used by the harder references scenario. It imports
    /// <c>ModuleA</c> and combines several application expressions, including two
    /// call sites of <c>ModuleA.helper</c>.
    /// </summary>
    private const string ChallengingReferencesScenario_ModuleBText =
        """
        module ModuleB exposing (doWork)

        import ModuleA

        doWork : Int -> Int
        doWork n =
            ModuleA.helper n + ModuleA.helper (n + 1)

        """;

    /// <summary>
    /// ModuleC workspace file used by the harder references scenario. It imports
    /// <c>ModuleA</c>, declares a record (<c>Bounds</c>), and contains three more
    /// call sites of <c>ModuleA.helper</c> spread across two declarations with
    /// nested application expressions.
    /// </summary>
    private const string ChallengingReferencesScenario_ModuleCText =
        """
        module ModuleC exposing (compute)

        import ModuleA

        type alias Bounds =
            { lower : Int
            , upper : Int
            }

        transform : Int -> Int -> Int
        transform a b =
            ModuleA.helper a + ModuleA.helper (b + 2)

        compute : Bounds -> Int
        compute bounds =
            transform (ModuleA.helper bounds.lower) (bounds.upper + 3)

        """;

    /// <summary>
    /// Position used by the harder references request: row 12 / column 1 lands on
    /// the <c>helper</c> name in the type signature <c>helper : Int -> Int</c> of
    /// <see cref="ChallengingReferencesScenario_ModuleAText"/>. (The Elm parser
    /// uses 1-indexed rows.)
    /// </summary>
    private const int ChallengingReferencesScenario_PositionLineNumber = 12;

    private const int ChallengingReferencesScenario_PositionColumn = 1;

    private const string ChallengingReferencesScenario_QueryFilePath = "src/ModuleA.elm";

    /// <summary>
    /// Expected rendered Elm-expression form of the language service response for
    /// the harder references scenario. The query for <c>helper</c> yields matches
    /// in multiple modules: the original name in the
    /// <c>module ModuleA exposing (helper, ...)</c> declaration, both
    /// <c>ModuleA.helper</c> call sites in
    /// <see cref="ChallengingReferencesScenario_ModuleBText"/>, and all three
    /// <c>ModuleA.helper</c> call sites in
    /// <see cref="ChallengingReferencesScenario_ModuleCText"/>.
    /// </summary>
    private const string ChallengingReferencesScenario_ExpectedResponse =
        """TextDocumentReferencesResponse [ { fileLocation = WorkspaceFileLocation "src/ModuleA.elm", range = { endColumn = 32, endLineNumber = 1, startColumn = 26, startLineNumber = 1 } }, { fileLocation = WorkspaceFileLocation "src/ModuleB.elm", range = { endColumn = 19, endLineNumber = 7, startColumn = 13, startLineNumber = 7 } }, { fileLocation = WorkspaceFileLocation "src/ModuleB.elm", range = { endColumn = 38, endLineNumber = 7, startColumn = 32, startLineNumber = 7 } }, { fileLocation = WorkspaceFileLocation "src/ModuleC.elm", range = { endColumn = 19, endLineNumber = 12, startColumn = 13, startLineNumber = 12 } }, { fileLocation = WorkspaceFileLocation "src/ModuleC.elm", range = { endColumn = 38, endLineNumber = 12, startColumn = 32, startLineNumber = 12 } }, { fileLocation = WorkspaceFileLocation "src/ModuleC.elm", range = { endColumn = 30, endLineNumber = 16, startColumn = 24, startLineNumber = 16 } } ]""";

    /// <summary>
    /// Harder variant of
    /// <see cref="References_request_finds_usage_across_modules_via_interpreter"/>
    /// that also runs through the <see cref="ElmSyntaxInterpreter"/> intermediate
    /// stage. The Elm source modules contain more declarations, more application
    /// expressions, and at least one record (the <c>Settings</c> and <c>Bounds</c>
    /// type aliases plus their record literals), so parsing is a bit more
    /// challenging. The references query for <c>helper</c> is modelled so that it
    /// yields matches in multiple modules (ModuleA, ModuleB, and ModuleC).
    /// </summary>
    [Fact]
    public void References_request_finds_usage_across_modules_via_interpreter_challenging()
    {
        var preparedApp = s_referencesScenarioPreparedApp.Value;

        var rootExpression =
            BuildReferencesScenarioRootExpression(
                workspaceFiles:
                [
                    ("src/ModuleA.elm", ChallengingReferencesScenario_ModuleAText),
                    ("src/ModuleB.elm", ChallengingReferencesScenario_ModuleBText),
                    ("src/ModuleC.elm", ChallengingReferencesScenario_ModuleCText),
                ],
                queryFilePath: ChallengingReferencesScenario_QueryFilePath,
                positionLineNumber: ChallengingReferencesScenario_PositionLineNumber,
                positionColumn: ChallengingReferencesScenario_PositionColumn);

        var interpreterStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var result =
            Core.Elm.ElmSyntax.ElmSyntaxInterpreter.InterpretAsElmValue(
                rootExpression,
                preparedApp);

        interpreterStopwatch.Stop();

        TestContext.Current.TestOutputHelper?.WriteLine(
            $"Interpreter execution time: {interpreterStopwatch.Elapsed}");

        var value =
            result.Extract(err => throw new Exception(err.ToString()));

        var rendered =
            ElmValue.RenderAsElmExpression(value).expressionString;

        rendered.Should().Be(ChallengingReferencesScenario_ExpectedResponse);
    }

    /// <summary>
    /// VM-based analog of
    /// <see cref="References_request_finds_usage_across_modules_via_interpreter_challenging"/>.
    /// It exercises the very same harder references scenario (the three modules
    /// <see cref="ChallengingReferencesScenario_ModuleAText"/>,
    /// <see cref="ChallengingReferencesScenario_ModuleBText"/>, and
    /// <see cref="ChallengingReferencesScenario_ModuleCText"/>, then a
    /// <c>TextDocumentReferencesRequest</c> for <c>helper</c>) but compiles the
    /// language service to Pine bytecode and runs it on the
    /// <see cref="Core.Interpreter.IntermediateVM.PineVM"/> — like
    /// <see cref="References_request_finds_usage_across_modules"/>. It asserts the
    /// same rendered response as the interpreter analog
    /// (<see cref="ChallengingReferencesScenario_ExpectedResponse"/>) and, in
    /// addition, the aggregated performance-counter snapshots.
    /// </summary>
    [Fact]
    public void References_request_finds_usage_across_modules_challenging()
    {
        var reports = new List<EvaluationReport>();
        var invocationCountReports = new List<InvocationCountReport>();

        // initState is a 0-argument top-level binding. Under the current
        // "Approach A1" compilation, its raw declaration value is a
        // function-record wrapper; we must evaluate it to obtain the actual
        // initial LanguageServiceState record value.
        var initStatePine = EvaluateZeroArgTestDeclaration("initState");

        var (addModuleAResult, addModuleAReport, addModuleAInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleA.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ChallengingReferencesScenario_ModuleAText)),
                initStatePine,
                ]);

        reports.Add(addModuleAReport);
        invocationCountReports.Add(addModuleAInvocationCounts);

        var stateAfterModuleA =
            ((PineValue.ListValue)addModuleAResult).Items.Span[1];

        var (addModuleBResult, addModuleBReport, addModuleBInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleB.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ChallengingReferencesScenario_ModuleBText)),
                stateAfterModuleA,
                ]);

        reports.Add(addModuleBReport);
        invocationCountReports.Add(addModuleBInvocationCounts);

        var stateAfterModuleB =
            ((PineValue.ListValue)addModuleBResult).Items.Span[1];

        var (addModuleCResult, addModuleCReport, addModuleCInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleC.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ChallengingReferencesScenario_ModuleCText)),
                stateAfterModuleB,
                ]);

        reports.Add(addModuleCReport);
        invocationCountReports.Add(addModuleCInvocationCounts);

        var stateAfterModuleC =
            ((PineValue.ListValue)addModuleCResult).Items.Span[1];

        var (refsResult, refsReport, refsInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "textDocumentReferences",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ChallengingReferencesScenario_QueryFilePath)),
                ElmValueEncoding.ElmValueAsPineValue(Integer(ChallengingReferencesScenario_PositionLineNumber)),
                ElmValueEncoding.ElmValueAsPineValue(Integer(ChallengingReferencesScenario_PositionColumn)),
                stateAfterModuleC,
                ]);

        reports.Add(refsReport);
        invocationCountReports.Add(refsInvocationCounts);

        // Extract response from tuple (first element)
        var responsePine =
            ((PineValue.ListValue)refsResult).Items.Span[0];

        var responseElmValue =
            ElmValueEncoding.PineValueAsElmValue(responsePine, null, null)
            .Extract(err => throw new Exception("Failed to decode response: " + err));

        var responseAsExpression =
            ElmValue.RenderAsElmExpression(responseElmValue);

        responseAsExpression.expressionString.Should().Be(ChallengingReferencesScenario_ExpectedResponse);

        var aggregateCounters =
            PerformanceCounters.Aggregate(
                reports.Select(r => r.Counters));

        var aggregateInvocationCounts =
            InvocationCountReport.Aggregate(invocationCountReports);

        PerformanceCountersFormatting.FormatCounts(aggregateCounters).Should().Be(
            """
            InvocationCount: 8_270
            BuildListCount: 13_148
            LoopIterationCount: 6_702
            InstructionCount: 362_328
            """);

        InvocationCountReportFormatting.FormatCounts(aggregateInvocationCounts).Should().Be(
            """
            CompiledExpressionCount: 234
            InvocationCountTotal: 7_183
            InvocationCountAverage: 31
            InvocationCountPercentile10: 2
            InvocationCountMedian: 5
            InvocationCountPercentile90: 24
            """);
    }

    /// <summary>
    /// Expected rendered Elm-expression form of the language service response for
    /// a <c>TextDocumentRenameRequest</c> issued against the harder references
    /// scenario (the same three modules
    /// <see cref="ChallengingReferencesScenario_ModuleAText"/>,
    /// <see cref="ChallengingReferencesScenario_ModuleBText"/>, and
    /// <see cref="ChallengingReferencesScenario_ModuleCText"/>). Renaming
    /// <c>helper</c> to <c>renamedHelper</c> must yield a
    /// <c>TextDocumentRenameResponse</c> whose workspace edits cover the
    /// declaration site (in <c>src/ModuleA.elm</c>) as well as every usage site
    /// across <c>src/ModuleB.elm</c> and <c>src/ModuleC.elm</c>.
    /// </summary>
    private const string ChallengingRenameScenario_NewName = "renamedHelper";

    /// <summary>
    /// VM-based rename scenario that starts from the same three Elm modules as
    /// <see cref="References_request_finds_usage_across_modules_challenging"/> and,
    /// instead of a references request, issues a
    /// <c>TextDocumentRenameRequest</c> for <c>helper</c>. It compiles the
    /// language service to Pine bytecode, runs it on the
    /// <see cref="Core.Interpreter.IntermediateVM.PineVM"/>, and asserts that the
    /// resulting <c>WorkspaceEdit</c> covers both the declaration site and all
    /// usage sites of <c>helper</c>. It also asserts the aggregated
    /// performance-counter snapshots.
    /// </summary>
    [Fact]
    public void Rename_request_renames_usage_across_modules_challenging()
    {
        var reports = new List<EvaluationReport>();
        var invocationCountReports = new List<InvocationCountReport>();

        var initStatePine = EvaluateZeroArgTestDeclaration("initState");

        var (addModuleAResult, addModuleAReport, addModuleAInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleA.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ChallengingReferencesScenario_ModuleAText)),
                initStatePine,
                ]);

        reports.Add(addModuleAReport);
        invocationCountReports.Add(addModuleAInvocationCounts);

        var stateAfterModuleA =
            ((PineValue.ListValue)addModuleAResult).Items.Span[1];

        var (addModuleBResult, addModuleBReport, addModuleBInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleB.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ChallengingReferencesScenario_ModuleBText)),
                stateAfterModuleA,
                ]);

        reports.Add(addModuleBReport);
        invocationCountReports.Add(addModuleBInvocationCounts);

        var stateAfterModuleB =
            ((PineValue.ListValue)addModuleBResult).Items.Span[1];

        var (addModuleCResult, addModuleCReport, addModuleCInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString("src/ModuleC.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ChallengingReferencesScenario_ModuleCText)),
                stateAfterModuleB,
                ]);

        reports.Add(addModuleCReport);
        invocationCountReports.Add(addModuleCInvocationCounts);

        var stateAfterModuleC =
            ((PineValue.ListValue)addModuleCResult).Items.Span[1];

        var (renameResult, renameReport, renameInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "textDocumentRename",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ChallengingReferencesScenario_QueryFilePath)),
                ElmValueEncoding.ElmValueAsPineValue(Integer(ChallengingReferencesScenario_PositionLineNumber)),
                ElmValueEncoding.ElmValueAsPineValue(Integer(ChallengingReferencesScenario_PositionColumn)),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ChallengingRenameScenario_NewName)),
                stateAfterModuleC,
                ]);

        reports.Add(renameReport);
        invocationCountReports.Add(renameInvocationCounts);

        var responsePine =
            ((PineValue.ListValue)renameResult).Items.Span[0];

        var responseElmValue =
            ElmValueEncoding.PineValueAsElmValue(responsePine, null, null)
            .Extract(err => throw new Exception("Failed to decode response: " + err));

        var responseAsExpression =
            ElmValue.RenderAsElmExpression(responseElmValue);

        responseAsExpression.expressionString.Should().Be(
            """TextDocumentRenameResponse [ { edits = [ { newText = "renamedHelper", range = { endColumn = 7, endLineNumber = 12, startColumn = 1, startLineNumber = 12 } }, { newText = "renamedHelper", range = { endColumn = 7, endLineNumber = 13, startColumn = 1, startLineNumber = 13 } }, { newText = "renamedHelper", range = { endColumn = 32, endLineNumber = 1, startColumn = 26, startLineNumber = 1 } } ], filePath = "src/ModuleA.elm" }, { edits = [ { newText = "renamedHelper", range = { endColumn = 19, endLineNumber = 7, startColumn = 13, startLineNumber = 7 } }, { newText = "renamedHelper", range = { endColumn = 38, endLineNumber = 7, startColumn = 32, startLineNumber = 7 } } ], filePath = "src/ModuleB.elm" }, { edits = [ { newText = "renamedHelper", range = { endColumn = 19, endLineNumber = 12, startColumn = 13, startLineNumber = 12 } }, { newText = "renamedHelper", range = { endColumn = 38, endLineNumber = 12, startColumn = 32, startLineNumber = 12 } }, { newText = "renamedHelper", range = { endColumn = 30, endLineNumber = 16, startColumn = 24, startLineNumber = 16 } } ], filePath = "src/ModuleC.elm" } ]""");

        var aggregateCounters =
            PerformanceCounters.Aggregate(
                reports.Select(r => r.Counters));

        var aggregateInvocationCounts =
            InvocationCountReport.Aggregate(invocationCountReports);

        PerformanceCountersFormatting.FormatCounts(aggregateCounters).Should().Be(
            """
            InvocationCount: 10_620
            BuildListCount: 17_441
            LoopIterationCount: 8_930
            InstructionCount: 475_420
            """);

        InvocationCountReportFormatting.FormatCounts(aggregateInvocationCounts).Should().Be(
            """
            CompiledExpressionCount: 235
            InvocationCountTotal: 9_143
            InvocationCountAverage: 39
            InvocationCountPercentile10: 2
            InvocationCountMedian: 6
            InvocationCountPercentile90: 38
            """);
    }

    /// <summary>
    /// Complex Elm module used by
    /// <see cref="Document_symbol_request_returns_symbols_for_module_with_complex_syntax"/>.
    /// Contains multiple custom choice types (with primitive, tuple, and record-shaped tags),
    /// a type alias with nested record fields, parameterized type declarations, and
    /// functions with type signatures, recursive branching, nested let-in expressions,
    /// case-of expressions, and local functions.
    /// </summary>
    private const string ComplexDocumentSymbolsScenario_ModuleText =
        """
        module ComplexSyntax exposing
            ( Status(..)
            , Config
            , Tree(..)
            , buildReport
            , transformTree
            )


        type Status
            = Inactive
            | Pending Int { retries : Int, label : String }
            | Completed String


        type alias Config =
            { maxDepth : Int
            , enableLogging : Bool
            , threshold : Int
            }


        type Tree a
            = Leaf a
            | Branch (Tree a) (Tree a)


        transformTree : (a -> b) -> Tree a -> Tree b
        transformTree mapper tree =
            case tree of
                Leaf value ->
                    Leaf (mapper value)

                Branch left right ->
                    let
                        transformedLeft =
                            transformTree mapper left

                        transformedRight =
                            transformTree mapper right
                    in
                    Branch transformedLeft transformedRight


        buildReport : Config -> Status -> Tree Int -> { summary : String, total : Int, isApproved : Bool }
        buildReport config status tree =
            let
                multiplier =
                    case status of
                        Inactive ->
                            0

                        Pending code details ->
                            if details.retries > config.threshold then
                                code * 2

                            else
                                code + details.retries

                        Completed _ ->
                            10

                foldTree : Tree Int -> Int
                foldTree current =
                    case current of
                        Leaf n ->
                            let
                                scaled =
                                    n * multiplier
                            in
                            if scaled > 100 then
                                scaled + config.maxDepth

                            else
                                scaled

                        Branch l r ->
                            foldTree l + foldTree r

                treeTotal =
                    foldTree tree

                approved =
                    treeTotal >= config.threshold && config.enableLogging
            in
            { summary =
                "Status evaluated with total: "
                    ++ (if approved then
                            "PASS"

                        else
                            "FAIL"
                       )
            , total = treeTotal
            , isApproved = approved
            }

        """;

    private const string ComplexDocumentSymbolsScenario_FilePath = "src/ComplexSyntax.elm";

    private const string ComplexDocumentSymbolsScenario_ExpectedResponse =
        """TextDocumentSymbolResponse [ DocumentSymbol { children = [ DocumentSymbol { children = [], kind = SymbolKind_EnumMember, name = "Inactive", range = { endColumn = 15, endLineNumber = 11, startColumn = 7, startLineNumber = 11 }, selectionRange = { endColumn = 15, endLineNumber = 11, startColumn = 7, startLineNumber = 11 } }, DocumentSymbol { children = [], kind = SymbolKind_EnumMember, name = "Pending", range = { endColumn = 52, endLineNumber = 12, startColumn = 7, startLineNumber = 12 }, selectionRange = { endColumn = 14, endLineNumber = 12, startColumn = 7, startLineNumber = 12 } }, DocumentSymbol { children = [], kind = SymbolKind_EnumMember, name = "Completed", range = { endColumn = 23, endLineNumber = 13, startColumn = 7, startLineNumber = 13 }, selectionRange = { endColumn = 16, endLineNumber = 13, startColumn = 7, startLineNumber = 13 } } ], kind = SymbolKind_Enum, name = "Status", range = { endColumn = 23, endLineNumber = 13, startColumn = 1, startLineNumber = 10 }, selectionRange = { endColumn = 12, endLineNumber = 10, startColumn = 6, startLineNumber = 10 } }, DocumentSymbol { children = [], kind = SymbolKind_Struct, name = "Config", range = { endColumn = 6, endLineNumber = 20, startColumn = 1, startLineNumber = 16 }, selectionRange = { endColumn = 18, endLineNumber = 16, startColumn = 12, startLineNumber = 16 } }, DocumentSymbol { children = [ DocumentSymbol { children = [], kind = SymbolKind_EnumMember, name = "Leaf", range = { endColumn = 13, endLineNumber = 24, startColumn = 7, startLineNumber = 24 }, selectionRange = { endColumn = 11, endLineNumber = 24, startColumn = 7, startLineNumber = 24 } }, DocumentSymbol { children = [], kind = SymbolKind_EnumMember, name = "Branch", range = { endColumn = 31, endLineNumber = 25, startColumn = 7, startLineNumber = 25 }, selectionRange = { endColumn = 13, endLineNumber = 25, startColumn = 7, startLineNumber = 25 } } ], kind = SymbolKind_Enum, name = "Tree", range = { endColumn = 31, endLineNumber = 25, startColumn = 1, startLineNumber = 23 }, selectionRange = { endColumn = 10, endLineNumber = 23, startColumn = 6, startLineNumber = 23 } }, DocumentSymbol { children = [], kind = SymbolKind_Function, name = "transformTree", range = { endColumn = 52, endLineNumber = 42, startColumn = 1, startLineNumber = 28 }, selectionRange = { endColumn = 14, endLineNumber = 29, startColumn = 1, startLineNumber = 29 } }, DocumentSymbol { children = [], kind = SymbolKind_Function, name = "buildReport", range = { endColumn = 6, endLineNumber = 96, startColumn = 1, startLineNumber = 45 }, selectionRange = { endColumn = 12, endLineNumber = 46, startColumn = 1, startLineNumber = 46 } } ]""";

    /// <summary>
    /// Exercises a <c>TextDocumentSymbolRequest</c> against an Elm module with complex
    /// contents, deeper declaration syntax (custom choice types with varied constructor
    /// payloads, type alias records, parameterized types), and deeper expression syntax
    /// (nested let-in bindings, case-of branches, recursive branching, and local functions).
    /// Asserts the returned document symbol hierarchy as well as aggregated performance
    /// counters and invocation counts.
    /// </summary>
    [Fact]
    public void Document_symbol_request_returns_symbols_for_module_with_complex_syntax()
    {
        var reports = new List<EvaluationReport>();
        var invocationCountReports = new List<InvocationCountReport>();

        var initStatePine = EvaluateZeroArgTestDeclaration("initState");

        var (addModuleResult, addModuleReport, addModuleInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "addWorkspaceFile",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ComplexDocumentSymbolsScenario_FilePath)),
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ComplexDocumentSymbolsScenario_ModuleText)),
                initStatePine,
                ]);

        reports.Add(addModuleReport);
        invocationCountReports.Add(addModuleInvocationCounts);

        var stateAfterModule =
            ((PineValue.ListValue)addModuleResult).Items.Span[1];

        var (symbolsResult, symbolsReport, symbolsInvocationCounts) =
            ApplyWithProfilingAndInvocationCounts(
                "textDocumentSymbol",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmString(ComplexDocumentSymbolsScenario_FilePath)),
                stateAfterModule,
                ]);

        reports.Add(symbolsReport);
        invocationCountReports.Add(symbolsInvocationCounts);

        var responsePine =
            ((PineValue.ListValue)symbolsResult).Items.Span[0];

        var responseElmValue =
            ElmValueEncoding.PineValueAsElmValue(responsePine, null, null)
            .Extract(err => throw new Exception("Failed to decode response: " + err));

        var responseAsExpression =
            ElmValue.RenderAsElmExpression(responseElmValue);

        responseAsExpression.expressionString.Should().Be(ComplexDocumentSymbolsScenario_ExpectedResponse);

        var aggregateCounters =
            PerformanceCounters.Aggregate(
                reports.Select(r => r.Counters));

        var aggregateInvocationCounts =
            InvocationCountReport.Aggregate(invocationCountReports);

        PerformanceCountersFormatting.FormatCounts(aggregateCounters).Should().Be(
            """
            InvocationCount: 15_133
            BuildListCount: 22_801
            LoopIterationCount: 15_123
            InstructionCount: 741_814
            """);

        InvocationCountReportFormatting.FormatCounts(aggregateInvocationCounts).Should().Be(
            """
            CompiledExpressionCount: 236
            InvocationCountTotal: 13_548
            InvocationCountAverage: 57
            InvocationCountPercentile10: 1
            InvocationCountMedian: 7
            InvocationCountPercentile90: 75
            """);
    }

    /// <summary>
    /// Builds the generic wrapper Elm module that drives the references scenarios
    /// through the <see cref="ElmSyntaxInterpreter"/>. The module is independent of
    /// any concrete scenario: it embeds no workspace file texts or query positions.
    /// Instead it exposes <c>referencesScenarioResponse</c> as a function that takes
    /// the workspace files (a list of <c>( path, text )</c> pairs) and the references
    /// query position (file path, line number, column) as arguments, adds every file
    /// to a fresh language service state, issues a
    /// <c>TextDocumentReferencesRequest</c>, and returns the resulting response. Tests
    /// supply the concrete inputs through the root expression built by
    /// <see cref="BuildReferencesScenarioRootExpression"/>.
    /// </summary>
    private static string BuildReferencesScenarioWrapperModule()
    {
        return
            "module ElmLanguageServiceReferencesScenario exposing (..)\n"
            + "\n"
            + "import LanguageService\n"
            + "import LanguageServiceInterface\n"
            + "\n"
            + "\n"
            + "addFile :\n"
            + "    ( String, String )\n"
            + "    -> LanguageService.LanguageServiceState\n"
            + "    -> LanguageService.LanguageServiceState\n"
            + "addFile ( path, text ) state =\n"
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
            + "addFiles :\n"
            + "    List ( String, String )\n"
            + "    -> LanguageService.LanguageServiceState\n"
            + "    -> LanguageService.LanguageServiceState\n"
            + "addFiles files state =\n"
            + "    case files of\n"
            + "        [] ->\n"
            + "            state\n"
            + "\n"
            + "        file :: rest ->\n"
            + "            addFiles rest (addFile file state)\n"
            + "\n"
            + "\n"
            + "referencesScenarioResponse :\n"
            + "    List ( String, String )\n"
            + "    -> String\n"
            + "    -> Int\n"
            + "    -> Int\n"
            + "    -> LanguageServiceInterface.Response\n"
            + "referencesScenarioResponse workspaceFiles queryFilePath positionLineNumber positionColumn =\n"
            + "    let\n"
            + "        state0 : LanguageService.LanguageServiceState\n"
            + "        state0 =\n"
            + "            LanguageService.initLanguageServiceState []\n"
            + "\n"
            + "        stateWithFiles : LanguageService.LanguageServiceState\n"
            + "        stateWithFiles =\n"
            + "            addFiles workspaceFiles state0\n"
            + "\n"
            + "        ( serviceResult, _ ) =\n"
            + "            LanguageService.handleRequestInCurrentWorkspace\n"
            + "                (LanguageServiceInterface.TextDocumentReferencesRequest\n"
            + "                    { fileLocation = LanguageServiceInterface.WorkspaceFileLocation queryFilePath\n"
            + "                    , positionLineNumber = positionLineNumber\n"
            + "                    , positionColumn = positionColumn\n"
            + "                    }\n"
            + "                )\n"
            + "                stateWithFiles\n"
            + "    in\n"
            + "    case serviceResult of\n"
            + "        Ok response ->\n"
            + "            response\n"
            + "\n"
            + "        Err err ->\n"
            + "            LanguageServiceInterface.ProvideHoverResponse [ \"Error: \" ++ err ]\n";
    }

    /// <summary>
    /// Builds the Elm root expression that drives one references scenario through the
    /// generic wrapper module built by <see cref="BuildReferencesScenarioWrapperModule"/>.
    /// The caller supplies the scenario inputs: the <paramref name="workspaceFiles"/>
    /// (each a <c>( path, text )</c> pair), the <paramref name="queryFilePath"/>, and
    /// the <paramref name="positionLineNumber"/> / <paramref name="positionColumn"/> of
    /// the references query. These are encoded as Elm literals so the prepared app does
    /// not need to embed any of them.
    /// </summary>
    private static string BuildReferencesScenarioRootExpression(
        IReadOnlyList<(string path, string text)> workspaceFiles,
        string queryFilePath,
        int positionLineNumber,
        int positionColumn)
    {
        var workspaceFilesLiteral =
            "[ "
            + string.Join(
                ", ",
                workspaceFiles.Select(
                    file =>
                    "( " + EncodeAsElmStringLiteral(file.path)
                    + ", " + EncodeAsElmStringLiteral(file.text) + " )"))
            + " ]";

        return
            "ElmLanguageServiceReferencesScenario.referencesScenarioResponse\n"
            + "    " + workspaceFilesLiteral + "\n"
            + "    " + EncodeAsElmStringLiteral(queryFilePath) + "\n"
            + "    " + positionLineNumber + "\n"
            + "    " + positionColumn + "\n";
    }

    /// <summary>
    /// Bundled compiler source modules (kernel, pine-elm-syntax, elm-in-elm src,
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
                AppendModuleTextsAtPath(bundledTree, ["pine-elm-syntax", "src"], modules);
                AppendModuleTextsAtPath(bundledTree, ["src"], modules);
                AppendModuleTextsAtPath(bundledTree, ["other-library-modules"], modules, optional: true);

                return modules;
            });

    /// <summary>
    /// Single prepared app shared by
    /// <see cref="References_request_finds_usage_across_modules_via_interpreter"/> and
    /// <see cref="References_request_finds_usage_across_modules_via_interpreter_challenging"/>.
    /// It bundles the compiler source modules together with a single <em>generic</em>
    /// references-scenario wrapper module (<c>ElmLanguageServiceReferencesScenario</c>)
    /// that exposes <c>referencesScenarioResponse</c> as a <em>function</em> taking the
    /// workspace files and the references-query position as arguments. No scenario
    /// inputs (module source texts, query file path, position) are embedded here as
    /// literals; instead each test supplies them itself via the root expression built
    /// by <see cref="BuildReferencesScenarioRootExpression"/>. The comparatively
    /// expensive <see cref="ElmSyntaxInterpreter.PrepareModules(IReadOnlyList{string})"/>
    /// step therefore runs only once and is reusable for arbitrary scenario inputs.
    /// </summary>
    private static readonly Lazy<Core.Elm.ElmSyntax.ElmSyntaxInterpreter.Prepared> s_referencesScenarioPreparedApp =
        new(
            () =>
            {
                var modulesWithWrapper =
                    new List<string>(s_compilerSourceModules.Value)
                    {
                        BuildReferencesScenarioWrapperModule(),
                    };

                return
                    Core.Elm.ElmSyntax.ElmSyntaxInterpreter.PrepareModules(modulesWithWrapper)
                    .Extract(err => throw new Exception("Failed to prepare modules: " + err));
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
}
