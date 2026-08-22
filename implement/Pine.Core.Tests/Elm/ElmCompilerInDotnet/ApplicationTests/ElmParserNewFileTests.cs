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
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Tests for the <c>file</c> parser from
/// <c>ElmSyntax.Concrete.Parser.FromString</c> on complete module texts.
/// </summary>
public class ElmParserNewFileTests
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
        """Ok { comments = [], declarations = [ Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } { arguments = [], equalsTokenLocation = { column = 6, row = 3 }, expression = Node { end = { column = 10, row = 3 }, start = { column = 8, row = 3 } } (IntegerLiteral "79"), name = Node { end = { column = 5, row = 3 }, start = { column = 1, row = 3 } } "alfa" }, documentation = Nothing, signature = Nothing }), Node { end = { column = 15, row = 5 }, start = { column = 1, row = 5 } } (FunctionDeclaration { declaration = Node { end = { column = 15, row = 5 }, start = { column = 1, row = 5 } } { arguments = [], equalsTokenLocation = { column = 6, row = 5 }, expression = Node { end = { column = 15, row = 5 }, start = { column = 8, row = 5 } } (StringLiteral "hello" (Just "hello")), name = Node { end = { column = 5, row = 5 }, start = { column = 1, row = 5 } } "beta" }, documentation = Nothing, signature = Nothing }) ], imports = [], incompleteDeclarations = [], moduleDefinition = Node { end = { column = 29, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 29, row = 1 }, start = { column = 16, row = 1 } } (All { end = { column = 28, row = 1 }, start = { column = 26, row = 1 } }), moduleName = Node { end = { column = 15, row = 1 }, start = { column = 8, row = 1 } } [ "Minimal" ] }) }""";

    private const string ExpectedImportsModuleExpression =
        """Ok { comments = [], declarations = [ Node { end = { column = 18, row = 6 }, start = { column = 1, row = 6 } } (FunctionDeclaration { declaration = Node { end = { column = 18, row = 6 }, start = { column = 1, row = 6 } } { arguments = [], equalsTokenLocation = { column = 6, row = 6 }, expression = Node { end = { column = 18, row = 6 }, start = { column = 8, row = 6 } } (Identifier [ "Dict" ] "empty"), name = Node { end = { column = 5, row = 6 }, start = { column = 1, row = 6 } } "alfa" }, documentation = Nothing, signature = Nothing }), Node { end = { column = 15, row = 8 }, start = { column = 1, row = 8 } } (FunctionDeclaration { declaration = Node { end = { column = 15, row = 8 }, start = { column = 1, row = 8 } } { arguments = [], equalsTokenLocation = { column = 6, row = 8 }, expression = Node { end = { column = 15, row = 8 }, start = { column = 8, row = 8 } } (Identifier [] "Nothing"), name = Node { end = { column = 5, row = 8 }, start = { column = 1, row = 8 } } "beta" }, documentation = Nothing, signature = Nothing }) ], imports = [ Node { end = { column = 12, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Nothing, importTokenLocation = { column = 1, row = 3 }, moduleAlias = Nothing, moduleName = Node { end = { column = 12, row = 3 }, start = { column = 8, row = 3 } } [ "Dict" ] }, Node { end = { column = 34, row = 4 }, start = { column = 1, row = 4 } } { exposingList = Just ({ column = 14, row = 4 }, Node { end = { column = 34, row = 4 }, start = { column = 14, row = 4 } } (Explicit { column = 23, row = 4 } (NonEmpty (Node { end = { column = 33, row = 4 }, start = { column = 24, row = 4 } } (TypeExpose { name = "Maybe", open = Just { end = { column = 33, row = 4 }, start = { column = 29, row = 4 } } })) []) { column = 33, row = 4 })), importTokenLocation = { column = 1, row = 4 }, moduleAlias = Nothing, moduleName = Node { end = { column = 13, row = 4 }, start = { column = 8, row = 4 } } [ "Maybe" ] } ], incompleteDeclarations = [], moduleDefinition = Node { end = { column = 33, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 33, row = 1 }, start = { column = 20, row = 1 } } (All { end = { column = 32, row = 1 }, start = { column = 30, row = 1 } }), moduleName = Node { end = { column = 19, row = 1 }, start = { column = 8, row = 1 } } [ "WithImports" ] }) }""";

    /// <summary>
    /// ModuleA text from the language-service references scenario in
    /// <see cref="ElmLanguageServiceTests"/>. Covers: single-name explicit
    /// exposing list, a type annotation of the form <c>name : Int -> Int</c>,
    /// a top-level function with one parameter, and a <c>+</c> operator
    /// application on <c>Int</c>.
    /// </summary>
    private const string LanguageServiceScenarioModuleAText =
        """
        module ModuleA exposing (helper)

        helper : Int -> Int
        helper x =
            x + 1

        """;

    /// <summary>
    /// ModuleB text from the language-service references scenario. Covers:
    /// single-name explicit exposing list, a bare <c>import ModuleA</c>
    /// without exposing or alias, a type annotation of the form
    /// <c>name : Int -> Int</c>, a top-level function with one parameter,
    /// qualified function calls (<c>ModuleA.helper n</c>) as arguments,
    /// a parenthesized sub-expression <c>(n + 1)</c>, and chained
    /// <c>+</c> operator applications with qualified function calls.
    /// </summary>
    private const string LanguageServiceScenarioModuleBText =
        """
        module ModuleB exposing (doWork)

        import ModuleA

        doWork : Int -> Int
        doWork n =
            ModuleA.helper n + ModuleA.helper (n + 1)

        """;

    /// <summary>
    /// A module with two declarations, each preceded by a type
    /// annotation. Bisection partner.
    /// </summary>
    private const string TypeAnnotationModuleText =
        """
        module Signed exposing (..)

        alfa : Int
        alfa = 79

        beta : String
        beta = "hello"
        """;

    /// <summary>
    /// A module with a top-level function declaration that takes one
    /// parameter and returns it unchanged. Bisection partner.
    /// </summary>
    private const string FunctionWithParamModuleText =
        """
        module WithParam exposing (..)

        alfa x = x

        beta = 79
        """;

    /// <summary>
    /// A module with an explicit exposing list that names each
    /// declaration. Bisection partner.
    /// </summary>
    private const string ExplicitExposingModuleText =
        """
        module Explicit exposing (alfa, beta)

        alfa = 79

        beta = "hello"
        """;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var bundledTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value;

                var kernelModulesTree =
                    BundledFiles.ElmKernelModulesDefault.Value;

                var pineElmSyntaxSrcTree =
                    bundledTree
                    .GetNodeAtPath(["pine-elm-syntax", "src"])
                    ?? throw new Exception("Did not find pine-elm-syntax/src");

                var mergedTree = kernelModulesTree;

                foreach (var (path, file) in pineElmSyntaxSrcTree.EnumerateFilesTransitive())
                {
                    mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
                }

                var rootFilePaths =
                    mergedTree.EnumerateFilesTransitive()
                    .Where(
                        file =>
                        file.path[^1].Equals("FromString.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(file => (IReadOnlyList<string>)file.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        mergedTree,
                        rootFilePaths: rootFilePaths)
                    .Map(r => r.compiledEnvValue)
                    .Extract(err => throw new Exception("Failed compiling: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing: " + err));
            });

    private static PineValue GetParserFunction(string name) =>
        s_env.Value.Modules
        .First(module => module.moduleName is "ElmSyntax.Concrete.Parser.FromString")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(
            reportFunctionApplication: _ => { },
            enableTailRecursionOptimization: true);

    private static ElmValue ElmString(string text) =>
        ElmValue.StringInstance(text);

    private static ((string expressionString, bool needsParens) valueAsExpression, string counts) ParseFileAndRender(
        string moduleText)
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetParserFunction("parseFile"),
                ElmString(moduleText),
                s_vm);

        return
            (ElmValue.RenderAsElmExpression(value), PerformanceCountersFormatting.FormatCounts(report));
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
            InvocationCount: 332
            BuildListCount: 639
            LoopIterationCount: 436
            InstructionCount: 19_399
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
            InvocationCount: 763
            BuildListCount: 1_367
            LoopIterationCount: 1_135
            InstructionCount: 47_264
            """);
    }

    [Fact]
    public void File_matches_language_service_scenario_ModuleA()
    {
        var result =
            ParseFileAndRender(LanguageServiceScenarioModuleAText);

        result.valueAsExpression.expressionString
            .Should().Be(
            """Ok { comments = [], declarations = [ Node { end = { column = 10, row = 5 }, start = { column = 1, row = 3 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 5 }, start = { column = 1, row = 4 } } { arguments = [ Node { end = { column = 9, row = 4 }, start = { column = 8, row = 4 } } (VarPattern "x") ], equalsTokenLocation = { column = 10, row = 4 }, expression = Node { end = { column = 10, row = 5 }, start = { column = 5, row = 5 } } (OperatorApplication (Node { end = { column = 8, row = 5 }, start = { column = 7, row = 5 } } "+") Left (Node { end = { column = 6, row = 5 }, start = { column = 5, row = 5 } } (Identifier [] "x")) (Node { end = { column = 10, row = 5 }, start = { column = 9, row = 5 } } (IntegerLiteral "1"))), name = Node { end = { column = 7, row = 4 }, start = { column = 1, row = 4 } } "helper" }, documentation = Nothing, signature = Just (Node { end = { column = 20, row = 3 }, start = { column = 1, row = 3 } } { colonLocation = { column = 8, row = 3 }, name = Node { end = { column = 7, row = 3 }, start = { column = 1, row = 3 } } "helper", typeAnnotation = Node { end = { column = 20, row = 3 }, start = { column = 10, row = 3 } } (FunctionTypeAnnotation (Node { end = { column = 13, row = 3 }, start = { column = 10, row = 3 } } (Typed (Node { end = { column = 13, row = 3 }, start = { column = 10, row = 3 } } ([], "Int")) [])) { column = 14, row = 3 } (Node { end = { column = 20, row = 3 }, start = { column = 17, row = 3 } } (Typed (Node { end = { column = 20, row = 3 }, start = { column = 17, row = 3 } } ([], "Int")) []))) }) }) ], imports = [], incompleteDeclarations = [], moduleDefinition = Node { end = { column = 33, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 33, row = 1 }, start = { column = 16, row = 1 } } (Explicit { column = 25, row = 1 } (NonEmpty (Node { end = { column = 32, row = 1 }, start = { column = 26, row = 1 } } (FunctionExpose "helper")) []) { column = 32, row = 1 }), moduleName = Node { end = { column = 15, row = 1 }, start = { column = 8, row = 1 } } [ "ModuleA" ] }) }""");
    }

    [Fact]
    public void File_matches_language_service_scenario_ModuleB()
    {
        var result =
            ParseFileAndRender(LanguageServiceScenarioModuleBText);

        result.valueAsExpression.expressionString
            .Should().Be(
            """Ok { comments = [], declarations = [ Node { end = { column = 46, row = 7 }, start = { column = 1, row = 5 } } (FunctionDeclaration { declaration = Node { end = { column = 46, row = 7 }, start = { column = 1, row = 6 } } { arguments = [ Node { end = { column = 9, row = 6 }, start = { column = 8, row = 6 } } (VarPattern "n") ], equalsTokenLocation = { column = 10, row = 6 }, expression = Node { end = { column = 46, row = 7 }, start = { column = 5, row = 7 } } (OperatorApplication (Node { end = { column = 23, row = 7 }, start = { column = 22, row = 7 } } "+") Left (Node { end = { column = 21, row = 7 }, start = { column = 5, row = 7 } } (Application (Node { end = { column = 19, row = 7 }, start = { column = 5, row = 7 } } (Identifier [ "ModuleA" ] "helper")) [ Node { end = { column = 21, row = 7 }, start = { column = 20, row = 7 } } (Identifier [] "n") ])) (Node { end = { column = 46, row = 7 }, start = { column = 24, row = 7 } } (Application (Node { end = { column = 38, row = 7 }, start = { column = 24, row = 7 } } (Identifier [ "ModuleA" ] "helper")) [ Node { end = { column = 46, row = 7 }, start = { column = 39, row = 7 } } (Parenthesized (Node { end = { column = 45, row = 7 }, start = { column = 40, row = 7 } } (OperatorApplication (Node { end = { column = 43, row = 7 }, start = { column = 42, row = 7 } } "+") Left (Node { end = { column = 41, row = 7 }, start = { column = 40, row = 7 } } (Identifier [] "n")) (Node { end = { column = 45, row = 7 }, start = { column = 44, row = 7 } } (IntegerLiteral "1"))))) ]))), name = Node { end = { column = 7, row = 6 }, start = { column = 1, row = 6 } } "doWork" }, documentation = Nothing, signature = Just (Node { end = { column = 20, row = 5 }, start = { column = 1, row = 5 } } { colonLocation = { column = 8, row = 5 }, name = Node { end = { column = 7, row = 5 }, start = { column = 1, row = 5 } } "doWork", typeAnnotation = Node { end = { column = 20, row = 5 }, start = { column = 10, row = 5 } } (FunctionTypeAnnotation (Node { end = { column = 13, row = 5 }, start = { column = 10, row = 5 } } (Typed (Node { end = { column = 13, row = 5 }, start = { column = 10, row = 5 } } ([], "Int")) [])) { column = 14, row = 5 } (Node { end = { column = 20, row = 5 }, start = { column = 17, row = 5 } } (Typed (Node { end = { column = 20, row = 5 }, start = { column = 17, row = 5 } } ([], "Int")) []))) }) }) ], imports = [ Node { end = { column = 15, row = 3 }, start = { column = 1, row = 3 } } { exposingList = Nothing, importTokenLocation = { column = 1, row = 3 }, moduleAlias = Nothing, moduleName = Node { end = { column = 15, row = 3 }, start = { column = 8, row = 3 } } [ "ModuleA" ] } ], incompleteDeclarations = [], moduleDefinition = Node { end = { column = 33, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 33, row = 1 }, start = { column = 16, row = 1 } } (Explicit { column = 25, row = 1 } (NonEmpty (Node { end = { column = 32, row = 1 }, start = { column = 26, row = 1 } } (FunctionExpose "doWork")) []) { column = 32, row = 1 }), moduleName = Node { end = { column = 15, row = 1 }, start = { column = 8, row = 1 } } [ "ModuleB" ] }) }""");
    }

    [Fact]
    public void File_with_type_annotations()
    {
        var result =
            ParseFileAndRender(TypeAnnotationModuleText);

        result.valueAsExpression.expressionString
            .Should().Be(
            """Ok { comments = [], declarations = [ Node { end = { column = 10, row = 4 }, start = { column = 1, row = 3 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 4 }, start = { column = 1, row = 4 } } { arguments = [], equalsTokenLocation = { column = 6, row = 4 }, expression = Node { end = { column = 10, row = 4 }, start = { column = 8, row = 4 } } (IntegerLiteral "79"), name = Node { end = { column = 5, row = 4 }, start = { column = 1, row = 4 } } "alfa" }, documentation = Nothing, signature = Just (Node { end = { column = 11, row = 3 }, start = { column = 1, row = 3 } } { colonLocation = { column = 6, row = 3 }, name = Node { end = { column = 5, row = 3 }, start = { column = 1, row = 3 } } "alfa", typeAnnotation = Node { end = { column = 11, row = 3 }, start = { column = 8, row = 3 } } (Typed (Node { end = { column = 11, row = 3 }, start = { column = 8, row = 3 } } ([], "Int")) []) }) }), Node { end = { column = 15, row = 7 }, start = { column = 1, row = 6 } } (FunctionDeclaration { declaration = Node { end = { column = 15, row = 7 }, start = { column = 1, row = 7 } } { arguments = [], equalsTokenLocation = { column = 6, row = 7 }, expression = Node { end = { column = 15, row = 7 }, start = { column = 8, row = 7 } } (StringLiteral "hello" (Just "hello")), name = Node { end = { column = 5, row = 7 }, start = { column = 1, row = 7 } } "beta" }, documentation = Nothing, signature = Just (Node { end = { column = 14, row = 6 }, start = { column = 1, row = 6 } } { colonLocation = { column = 6, row = 6 }, name = Node { end = { column = 5, row = 6 }, start = { column = 1, row = 6 } } "beta", typeAnnotation = Node { end = { column = 14, row = 6 }, start = { column = 8, row = 6 } } (Typed (Node { end = { column = 14, row = 6 }, start = { column = 8, row = 6 } } ([], "String")) []) }) }) ], imports = [], incompleteDeclarations = [], moduleDefinition = Node { end = { column = 28, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 28, row = 1 }, start = { column = 15, row = 1 } } (All { end = { column = 27, row = 1 }, start = { column = 25, row = 1 } }), moduleName = Node { end = { column = 14, row = 1 }, start = { column = 8, row = 1 } } [ "Signed" ] }) }""");
    }

    [Fact]
    public void File_with_function_with_parameter()
    {
        var result =
            ParseFileAndRender(FunctionWithParamModuleText);

        result.valueAsExpression.expressionString
            .Should().Be(
            """Ok { comments = [], declarations = [ Node { end = { column = 11, row = 3 }, start = { column = 1, row = 3 } } (FunctionDeclaration { declaration = Node { end = { column = 11, row = 3 }, start = { column = 1, row = 3 } } { arguments = [ Node { end = { column = 7, row = 3 }, start = { column = 6, row = 3 } } (VarPattern "x") ], equalsTokenLocation = { column = 8, row = 3 }, expression = Node { end = { column = 11, row = 3 }, start = { column = 10, row = 3 } } (Identifier [] "x"), name = Node { end = { column = 5, row = 3 }, start = { column = 1, row = 3 } } "alfa" }, documentation = Nothing, signature = Nothing }), Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 5 }, start = { column = 1, row = 5 } } { arguments = [], equalsTokenLocation = { column = 6, row = 5 }, expression = Node { end = { column = 10, row = 5 }, start = { column = 8, row = 5 } } (IntegerLiteral "79"), name = Node { end = { column = 5, row = 5 }, start = { column = 1, row = 5 } } "beta" }, documentation = Nothing, signature = Nothing }) ], imports = [], incompleteDeclarations = [], moduleDefinition = Node { end = { column = 31, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 31, row = 1 }, start = { column = 18, row = 1 } } (All { end = { column = 30, row = 1 }, start = { column = 28, row = 1 } }), moduleName = Node { end = { column = 17, row = 1 }, start = { column = 8, row = 1 } } [ "WithParam" ] }) }""");
    }

    [Fact]
    public void File_with_explicit_exposing_list()
    {
        var result =
            ParseFileAndRender(ExplicitExposingModuleText);

        result.valueAsExpression.expressionString
            .Should().Be(
            """Ok { comments = [], declarations = [ Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } (FunctionDeclaration { declaration = Node { end = { column = 10, row = 3 }, start = { column = 1, row = 3 } } { arguments = [], equalsTokenLocation = { column = 6, row = 3 }, expression = Node { end = { column = 10, row = 3 }, start = { column = 8, row = 3 } } (IntegerLiteral "79"), name = Node { end = { column = 5, row = 3 }, start = { column = 1, row = 3 } } "alfa" }, documentation = Nothing, signature = Nothing }), Node { end = { column = 15, row = 5 }, start = { column = 1, row = 5 } } (FunctionDeclaration { declaration = Node { end = { column = 15, row = 5 }, start = { column = 1, row = 5 } } { arguments = [], equalsTokenLocation = { column = 6, row = 5 }, expression = Node { end = { column = 15, row = 5 }, start = { column = 8, row = 5 } } (StringLiteral "hello" (Just "hello")), name = Node { end = { column = 5, row = 5 }, start = { column = 1, row = 5 } } "beta" }, documentation = Nothing, signature = Nothing }) ], imports = [], incompleteDeclarations = [], moduleDefinition = Node { end = { column = 38, row = 1 }, start = { column = 1, row = 1 } } (NormalModule { exposingList = Node { end = { column = 38, row = 1 }, start = { column = 17, row = 1 } } (Explicit { column = 26, row = 1 } (NonEmpty (Node { end = { column = 31, row = 1 }, start = { column = 27, row = 1 } } (FunctionExpose "alfa")) [ ({ column = 31, row = 1 }, Node { end = { column = 37, row = 1 }, start = { column = 33, row = 1 } } (FunctionExpose "beta")) ]) { column = 37, row = 1 }), moduleName = Node { end = { column = 16, row = 1 }, start = { column = 8, row = 1 } } [ "Explicit" ] }) }""");
    }
}
