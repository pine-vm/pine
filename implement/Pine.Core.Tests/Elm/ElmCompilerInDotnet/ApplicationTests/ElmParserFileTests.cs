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
/// Tests for the <c>file</c> parser from <c>Elm.Parser.File</c> via
/// <c>Elm.Parser.parseToFile</c> on complete module texts.
/// </summary>
public class ElmParserFileTests
{
    private const string MinimalModuleText =
        """
        module Minimal exposing (..)

        alfa = 79

        beta = "hello"
        """;

    private const string ImportsModuleText =
        """
        module WithImports exposing (..)

        import Dict
        import Maybe exposing (Maybe(..))

        alfa = Dict.empty

        beta = Nothing
        """;

    private const string ExpectedMinimalModuleExpression =
        """Ok { comments = [], declarations = [ Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } { arguments = [], expression = Node { end = { column = 10, row = 3 }, start = { column = 8, row = 3 } } (Integer 79), name = Node { end = { column = 5, row = 3 }, start = { column = 1, row = 3 } } "alfa" }, documentation = Nothing, signature = Nothing }), Node { end = { column = 15, row = 5 }, start = { column = 1, row = 5 } } (FunctionDeclaration { declaration = Node { end = { column = 15, row = 5 }, start = { column = 1, row = 5 } } { arguments = [], expression = Node { end = { column = 15, row = 5 }, start = { column = 8, row = 5 } } (Literal "hello"), name = Node { end = { column = 5, row = 5 }, start = { column = 1, row = 5 } } "beta" }, documentation = Nothing, signature = Nothing }) ], imports = [], moduleDefinition = Node { end = { column = 29, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 29, row = 1 }, start = { column = 16, row = 1 } } (All { end = { column = 28, row = 1 }, start = { column = 26, row = 1 } }), moduleName = Node { end = { column = 15, row = 1 }, start = { column = 8, row = 1 } } [ "Minimal" ] }) }""";

    private const string ExpectedImportsModuleExpression =
        """Ok { comments = [], declarations = [ Node { end = { column = 18, row = 6 }, start = { column = 1, row = 6 } } (FunctionDeclaration { declaration = Node { end = { column = 18, row = 6 }, start = { column = 1, row = 6 } } { arguments = [], expression = Node { end = { column = 18, row = 6 }, start = { column = 8, row = 6 } } (FunctionOrValue [ "Dict" ] "empty"), name = Node { end = { column = 5, row = 6 }, start = { column = 1, row = 6 } } "alfa" }, documentation = Nothing, signature = Nothing }), Node { end = { column = 15, row = 8 }, start = { column = 1, row = 8 } } (FunctionDeclaration { declaration = Node { end = { column = 15, row = 8 }, start = { column = 1, row = 8 } } { arguments = [], expression = Node { end = { column = 15, row = 8 }, start = { column = 8, row = 8 } } (FunctionOrValue [] "Nothing"), name = Node { end = { column = 5, row = 8 }, start = { column = 1, row = 8 } } "beta" }, documentation = Nothing, signature = Nothing }) ], imports = [ Node { end = { column = 12, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Nothing, moduleAlias = Nothing, moduleName = Node { end = { column = 12, row = 3 }, start = { column = 8, row = 3 } } [ "Dict" ] }, Node { end = { column = 34, row = 4 }, start = { column = 1, row = 4 } } { exposingList = Just (Node { end = { column = 34, row = 4 }, start = { column = 14, row = 4 } } (Explicit [ Node { end = { column = 33, row = 4 }, start = { column = 24, row = 4 } } (TypeExpose { name = "Maybe", open = Just { end = { column = 33, row = 4 }, start = { column = 29, row = 4 } } }) ])), moduleAlias = Nothing, moduleName = Node { end = { column = 13, row = 4 }, start = { column = 8, row = 4 } } [ "Maybe" ] } ], moduleDefinition = Node { end = { column = 33, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 33, row = 1 }, start = { column = 20, row = 1 } } (All { end = { column = 32, row = 1 }, start = { column = 30, row = 1 } }), moduleName = Node { end = { column = 19, row = 1 }, start = { column = 8, row = 1 } } [ "WithImports" ] }) }""";

    private const string TestModuleText =
        """"
        module ElmParserFileTestModule exposing (..)

        import Elm.Parser
        import Elm.Syntax.File exposing (File)


        parseFile : String -> Result String File
        parseFile fileText =
            case Elm.Parser.parseToFile fileText of
                Err deadEnds ->
                    Err ("parse-failed:" ++ String.fromInt (List.length deadEnds))

                Ok file ->
                    Ok file
        """"
        ;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
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
                        ["ElmParserFileTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        file =>
                        file.path[^1].Equals("ElmParserFileTestModule.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(file => (IReadOnlyList<string>)file.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        treeWithTest,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Map(r => r.compiledEnvValue)
                    .Extract(err => throw new Exception("Failed compiling: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing: " + err));
            });

    private static PineValue GetTestFunction(string name) =>
        s_env.Value.Modules
        .First(module => module.moduleName is "ElmParserFileTestModule")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue ElmString(string text) =>
        ElmValue.StringInstance(text);

    private static ((string expressionString, bool needsParens) valueAsExpression, string counts) ParseFileAndRender(
        string moduleText)
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseFile"),
                ElmString(moduleText),
                s_vm);

        return
            (ElmValue.RenderAsElmExpression(value), ElmCompilerTestHelper.FormatCounts(report));
    }

    [Fact]
    public void File_with_zero_imports_and_two_simple_declarations()
    {
        var result =
            ParseFileAndRender(MinimalModuleText);

        result.valueAsExpression.expressionString.Should().Be(
            ExpectedMinimalModuleExpression);

        result.counts.Should().Be(
            """
            InstructionCount: 369_115
            InvocationCount: 11_156
            BuildListCount: 120_502
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void File_with_imports()
    {
        var result =
            ParseFileAndRender(ImportsModuleText);

        result.valueAsExpression.expressionString.Should().Be(
            ExpectedImportsModuleExpression);

        result.counts.Should().Be(
            """
            InstructionCount: 682_968
            InvocationCount: 22_585
            BuildListCount: 124_816
            LoopIterationCount: 0
            """);
    }
}
