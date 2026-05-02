using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Focused regression test for a defect surfaced when migrating the
/// "pine lang-server" command to use a language service compiled by
/// <see cref="ElmCompiler.CompileInteractiveEnvironment(FileTree, IReadOnlyList{IReadOnlyList{string}}, bool, int)"/>.
/// <para>
/// Calling <c>provideHover</c> on the freshly compiled language service
/// returns a <c>ProvideHoverResponse</c> whose item list contains a
/// <c>LocationInFile</c> custom-type value where a <c>String</c> is
/// expected. The integration test
/// <c>Pine.IntegrationTests.ElmLanguageServiceTests.Language_service_provides_hover</c>
/// asserts on the textual hover content and therefore only failed at the
/// response decoding stage with
/// <c>"Unexpected response tag element type: ElmTag"</c>.
/// </para>
/// <para>
/// <b>Likely root cause.</b> The path runs through
/// <c>LanguageService.hoverItemsFromParsedModule</c>, which builds a
/// <c>List ( Range, LocationInFile DeclarationRange, String )</c> and
/// then strips the middle slot via
/// <code>
/// |> List.map (\( range, _, documentation ) -&gt; ( range, documentation ))
/// </code>
/// On the compiled-VM path the resulting 2-tuple binds the wildcard
/// (the <c>LocationInFile</c> in slot 2) into the <c>documentation</c>
/// position of the produced 2-tuple, instead of the third element.
/// </para>
/// <para>
/// This is the <i>same</i> underlying compile-to-PineVM defect already
/// documented in
/// <see cref="LanguageServiceReferencesEmptyResponseRegressionTests"/> —
/// where bisection ruled out plain isolated 3-tuple destructures with a
/// custom-type middle slot — and adds a second user-observable failure
/// mode (hover content) on top of the references-empty failure mode.
/// </para>
/// </summary>
public class LanguageServiceProvideHoverRegressionTests
{
    /// <summary>
    /// Wrapper module exposing thin shims around the language service
    /// init / addWorkspaceFile / provideHover entry points so that the
    /// test can drive them from C# without depending on the host-side
    /// <c>LanguageServiceState</c> wrapper.
    /// </summary>
    private const string TestModuleText =
        """"
        module Probe exposing (..)

        import Dict
        import LanguageService
        import LanguageServiceInterface


        initState : LanguageService.LanguageServiceState
        initState =
            LanguageService.initLanguageServiceState []


        addWorkspaceFile :
            String
            -> String
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        addWorkspaceFile filePath fileText state =
            case LanguageService.handleRequestInCurrentWorkspace
                (LanguageServiceInterface.AddWorkspaceFileRequest
                    filePath
                    { asBase64 = ""
                    , asText = Just fileText
                    }
                )
                state
            of
                ( Ok response, newState ) ->
                    ( response, newState )

                ( Err err, newState ) ->
                    ( LanguageServiceInterface.ProvideHoverResponse [ "Error: " ++ err ], newState )


        provideHover :
            String
            -> Int
            -> Int
            -> LanguageService.LanguageServiceState
            -> ( LanguageServiceInterface.Response, LanguageService.LanguageServiceState )
        provideHover filePath lineNumber column state =
            case LanguageService.handleRequestInCurrentWorkspace
                (LanguageServiceInterface.ProvideHoverRequest
                    { fileLocation = LanguageServiceInterface.WorkspaceFileLocation filePath
                    , positionLineNumber = lineNumber
                    , positionColumn = column
                    }
                )
                state
            of
                ( Ok response, newState ) ->
                    ( response, newState )

                ( Err err, newState ) ->
                    ( LanguageServiceInterface.ProvideHoverResponse [ "Error: " ++ err ], newState )
        """";

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(BuildEnv);

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment BuildEnv()
    {
        var bundledTree = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

        var elmSyntaxSrcTree =
            bundledTree.GetNodeAtPath(["elm-syntax", "src"])
            ?? throw new Exception("Did not find elm-syntax/src");

        var elmInElmSrcTree =
            bundledTree.GetNodeAtPath(["src"])
            ?? throw new Exception("Did not find src");

        var otherLibraryModulesTree =
            bundledTree.GetNodeAtPath(["other-library-modules"]);

        var mergedTree = kernelModulesTree;

        foreach (var (path, file) in elmSyntaxSrcTree.EnumerateFilesTransitive())
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));

        foreach (var (path, file) in elmInElmSrcTree.EnumerateFilesTransitive())
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));

        if (otherLibraryModulesTree is not null)
        {
            foreach (var (path, file) in otherLibraryModulesTree.EnumerateFilesTransitive())
                mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        var treeWithTest =
            mergedTree.SetNodeAtPathSorted(
                ["Probe.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

        var rootFilePaths =
            treeWithTest.EnumerateFilesTransitive()
            .Where(b => b.path[^1].Equals("Probe.elm", StringComparison.OrdinalIgnoreCase))
            .Select(b => (IReadOnlyList<string>)b.path)
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

    private static PineValue GetFunc(string name) =>
        s_env.Value.Modules
        .First(m => m.moduleName is "Probe")
        .moduleContent.FunctionDeclarations[name];

    private static PineValue Apply(string name, PineValue[] args)
    {
        var fr =
            FunctionRecord.ParseFunctionRecordTagged(GetFunc(name), new PineVMParseCache())
            .Extract(err => throw new Exception(err.ToString()));

        var ev =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(fr, args)
            .Extract(err => throw new Exception(err.ToString()));

        return
            s_vm.EvaluateExpressionOnCustomStack(
                ev.expression,
                ev.environment,
                config: ElmCompilerTestHelper.DefaultTestEvaluationConfig)
            .Extract(err => throw new Exception("Failed eval for '" + name + "': " + err))
            .ReturnValue.Evaluate();
    }

    private static PineValue Eval0Arg(string name) =>
        Apply(name, []);

    /// <summary>
    /// FAILS on the compiled-VM path: the hover response is
    /// <c>ProvideHoverResponse [ &lt;LocationInFile ...&gt; ]</c> instead
    /// of the expected <c>ProvideHoverResponse [ "    init : State" ]</c>.
    /// The list element comes back as a custom-type value
    /// (<c>LocationInFile (WorkspaceFileLocation "src/Main.elm") (DeclarationRange ...)</c>)
    /// rather than a String.
    /// <para>
    /// The minimal user-side Elm source is a single module that imports
    /// nothing and contains exactly one top-level type-annotated
    /// declaration plus one site that references it. Hovering on the
    /// reference is the smallest scenario where
    /// <c>hoverItemsFromParsedModule</c> populates a non-empty
    /// <c>fromDeclarations</c> list.
    /// </para>
    /// <para>
    /// This test is intentionally kept as a <c>[Fact]</c> (without
    /// <c>Skip</c>) so it surfaces the regression in CI. It is the same
    /// underlying compile-to-PineVM defect already tracked by
    /// <see cref="LanguageServiceReferencesEmptyResponseRegressionTests"/>;
    /// the prior bisection there ruled out reproducing it from any
    /// isolated 3-tuple destructure with a custom-type middle slot, so
    /// a reduced minimal-Elm reproducer outside the full LanguageService
    /// context is not currently known.
    /// </para>
    /// </summary>
    [Fact]
    public void Hover_response_items_are_strings_not_LocationInFile()
    {
        var initState = Eval0Arg("initState");

        // Minimal user-side Elm source: a single module containing
        // exactly one type-annotated declaration and one reference.
        const string ModuleText =
            """"
            module Main exposing (State)


            name = init


            init : State
            init =
                0

            """";

        var stateAfter =
            ((PineValue.ListValue)Apply("addWorkspaceFile",
                [
                    ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("src/Main.elm")),
                    ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(ModuleText)),
                    initState,
                ])).Items.Span[1];

        // Hover on the reference 'init' on line 4 (column inside the identifier).
        var hoverResult =
            Apply(
                "provideHover",
                [
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("src/Main.elm")),
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(4)),
                ElmValueEncoding.ElmValueAsPineValue(ElmValue.Integer(9)),
                stateAfter,
                ]);

        var responsePine =
            ((PineValue.ListValue)hoverResult).Items.Span[0];

        var responseElmValue =
            ElmValueEncoding.PineValueAsElmValue(responsePine, null, null)
            .Extract(err => throw new Exception(err));

        var responseElmSyntax =
            ElmValue.RenderAsElmExpression(responseElmValue).expressionString;

        responseElmSyntax.Should().Be(
            "ProvideHoverResponse [ \"    init : State\" ]");
    }
}
