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

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;

/// <summary>
/// Focused regression test for a defect where Phase 4 (size-based inlining of small
/// functions and plain values) produces compiled code that enters infinite runtime
/// recursion when executing the Elm.Parser module.
///
/// The stack overflow manifests as:
///   System.Exception : Failed eval: Stack depth limit exceeded: 100_000
///   Last stack frames expressions: 4791cc44 (repeating)
///
/// Root cause: Phase 4 inlines small wrapper functions in the Elm.Parser module
/// (which has only 2 functions: parse and parseToFile). This inlining somehow
/// produces Pine expressions with a runtime cycle.
///
/// This test compiles the elm-syntax library with a minimal test module that calls
/// Elm.Parser.parseToFile, then runs the function. The test passes when compiled
/// with disableInlining:true but fails with disableInlining:false due to Phase 4.
///
/// See docs/2026-04-11-size-based-inlining-stack-overflow-investigation.md
/// </summary>
public class SizeBasedInliningRegressionTests
{
    /// <summary>
    /// Minimal test module that calls Elm.Parser.parseToFile.
    /// We avoid Elm.Parser.parse because it adds Result.map overhead;
    /// parseToFile is the simpler wrapper that still triggers the bug.
    /// </summary>
    private const string TestModuleText =
        """
        module SizeInlineTestModule exposing (..)

        import Elm.Parser


        callParseToFile : String -> Result (List Elm.Parser.DeadEnd) Elm.Syntax.File.File
        callParseToFile input =
            Elm.Parser.parseToFile input
        """;

    /// <summary>
    /// The simplest possible Elm module text to parse.
    /// </summary>
    private const string TrivialElmModule =
        """
        module A exposing (..)

        x = 1
        """;

    /// <summary>
    /// Compile and run Elm.Parser.parseToFile to verify it does not stack overflow.
    /// This is the minimal end-to-end reproduction of the Phase 4 size-based inlining defect.
    ///
    /// When the defect is present, this test fails with:
    ///   System.Exception : Failed eval: Stack depth limit exceeded: 100_000
    /// </summary>
    [Fact]
    public void ParseToFile_does_not_stack_overflow_with_inlining_enabled()
    {
        var (value, _) = CompileAndRunParseToFile(disableInlining: false);

        // The result should be Ok(...) — any Ok variant means the parser ran successfully.
        var rendered = ElmValue.RenderAsElmExpression(value);
        rendered.expressionString.Should().StartWith("Ok ");
    }

    /// <summary>
    /// Baseline: same test with inlining disabled (should always pass).
    /// If this test fails, the issue is not in inlining but elsewhere.
    /// </summary>
    [Fact]
    public void ParseToFile_works_with_inlining_disabled()
    {
        var (value, _) = CompileAndRunParseToFile(disableInlining: true);

        var rendered = ElmValue.RenderAsElmExpression(value);
        rendered.expressionString.Should().StartWith("Ok ");
    }

    /// <summary>
    /// Verify that compilation with inlining produces a valid function record for parseToFile.
    /// </summary>
    [Fact]
    public void ParseToFile_compiles_to_valid_function_with_inlining()
    {
        var envWith = CompileEnvironment(disableInlining: false);

        var parsedWith =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(envWith)
            .Extract(err => throw new Exception("Failed parsing env: " + err));

        var elmParserWith = parsedWith.Modules.First(m => m.moduleName == "Elm.Parser");

        var parseToFileWith = elmParserWith.moduleContent.FunctionDeclarations["parseToFile"];

        var parseCache = new PineVMParseCache();

        var parsedPTFWith =
            FunctionRecord.ParseFunctionValue(parseToFileWith, parseCache)
            .Extract(err => throw new Exception("Failed parsing parseToFile WITH: " + err));

        parsedPTFWith.Should().BeOfType<ParsedFunctionValue.WithEnvFunctions>();
    }

    private static PineValue CompileEnvironment(bool disableInlining)
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

        var mergedTree = kernelModulesTree;

        foreach (var (path, file) in elmSyntaxSrcTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        var treeWithTest =
            mergedTree.SetNodeAtPathSorted(
                ["SizeInlineTestModule.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

        var rootFilePaths =
            treeWithTest.EnumerateFilesTransitive()
            .Where(
                file =>
                file.path[^1].Equals("SizeInlineTestModule.elm", StringComparison.OrdinalIgnoreCase))
            .Select(file => (IReadOnlyList<string>)file.path)
            .ToList();

        return
            ElmCompiler.CompileInteractiveEnvironment(
                treeWithTest,
                rootFilePaths: rootFilePaths,
                disableInlining: disableInlining)
            .Extract(err => throw new Exception("Failed compiling: " + err)).compiledEnvValue;
    }

    private static (ElmValue value, object report) CompileAndRunParseToFile(bool disableInlining)
    {
        var compiledEnv = CompileEnvironment(disableInlining);

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing env: " + err));

        var testFunction =
            parsedEnv.Modules
            .First(module => module.moduleName is "SizeInlineTestModule")
            .moduleContent.FunctionDeclarations["callParseToFile"];

        var vm = ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                testFunction,
                ElmValue.StringInstance(TrivialElmModule),
                vm);

        return (value, report);
    }
}
