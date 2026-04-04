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
/// Tests for the <c>expression</c> parser from
/// <c>Elm.Parser.Expression</c> (elm-syntax/src/Elm/Parser/Expression.elm).
/// Unlike <see cref="ParserFastTests"/>, which duplicates parser helpers in a
/// standalone module, these tests compile the real elm-syntax expression parser
/// and call it directly via <c>ParserFast.run Elm.Parser.Expression.expression</c>.
/// Each test asserts on the parsed result and on the runtime cost snapshot from
/// <see cref="CoreLibraryModule.CoreLibraryTestHelper.FormatCounts"/>.
/// </summary>
public class ElmParserExpressionTests
{
    /// <summary>
    /// Test wrapper module that imports the real elm-syntax expression parser
    /// and exposes thin functions for exercising it from C# tests.
    /// Each wrapper calls <c>ParserFast.run Elm.Parser.Expression.expression</c>
    /// directly on the input string, bypassing <c>parseToFile</c>.
    /// </summary>
    private const string TestModuleText =
        """"
        module ElmParserExpressionTestModule exposing (..)

        import Elm.Parser.Expression
        import Elm.Syntax.Expression exposing (Expression(..))
        import Elm.Syntax.Node exposing (Node(..))
        import ParserFast
        import ParserWithComments exposing (WithComments(..))


        parseExpression : String -> Result String Expression
        parseExpression exprText =
            case ParserFast.run Elm.Parser.Expression.expression exprText of
                Err deadEnds ->
                    Err ("parse-failed:" ++ String.fromInt (List.length deadEnds))

                Ok (WithComments _ nodeExpr) ->
                    Ok (Elm.Syntax.Node.value nodeExpr)


        parseIntLiteral : String -> Int
        parseIntLiteral input =
            case parseExpression input of
                Ok (Integer n) ->
                    n

                _ ->
                    -1


        parseStringLiteral : String -> String
        parseStringLiteral input =
            case parseExpression input of
                Ok (Literal s) ->
                    s

                _ ->
                    ""


        expressionTag : String -> String
        expressionTag input =
            case parseExpression input of
                Ok expr ->
                    expressionVariantTag expr

                Err e ->
                    "Err:" ++ e


        expressionVariantTag : Expression -> String
        expressionVariantTag expr =
            case expr of
                Integer _ ->
                    "Integer"

                Literal _ ->
                    "Literal"

                ListExpr _ ->
                    "ListExpr"

                UnitExpr ->
                    "UnitExpr"

                FunctionOrValue _ _ ->
                    "FunctionOrValue"

                _ ->
                    "Other"
        """"
        ;

    // ---------- compilation + environment ----------

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

                // Merge the elm-syntax source files into the kernel modules tree.
                var mergedTree = kernelModulesTree;

                foreach (var (path, file) in elmSyntaxSrcTree.EnumerateFilesTransitive())
                {
                    mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
                }

                // Add our test module
                var treeWithTest =
                    mergedTree.SetNodeAtPathSorted(
                        ["ElmParserExpressionTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("ElmParserExpressionTestModule.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        treeWithTest,
                        rootFilePaths: rootFilePaths,
                        disableInlining: false)
                    .Extract(err => throw new Exception("Failed compiling: " + err));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(err => throw new Exception("Failed parsing: " + err));
            });

    private static PineValue GetTestFunction(string name) =>
        s_env.Value.Modules
        .First(m => m.moduleName is "ElmParserExpressionTestModule")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue ElmString(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue Ok(ElmValue inner) =>
        ElmValue.TagInstance("Ok", [inner]);

    private static ElmValue ListExpr(params ElmValue[] items) =>
        ElmValue.TagInstance("ListExpr", [ElmValue.ListInstance(items)]);

    private static ElmValue IntegerExpr(long n) =>
        ElmValue.TagInstance("Integer", [ElmValue.Integer(n)]);

    private static ElmValue Node(int startRow, int startCol, int endRow, int endCol, ElmValue expr) =>
        ElmValue.TagInstance(
            "Node",
            [
            new ElmValue.ElmRecord(
                [
                ("end", new ElmValue.ElmRecord([("column", ElmValue.Integer(endCol)), ("row", ElmValue.Integer(endRow))])),
                ("start", new ElmValue.ElmRecord([("column", ElmValue.Integer(startCol)), ("row", ElmValue.Integer(startRow))]))
                ]),
            expr
            ]);

    // ===== Int literal =====

    [Fact]
    public void Expression_int_literal()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseIntLiteral"),
                ElmString("123"),
                s_vm);

        value.Should().Be(Integer(123));

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 70831
            InvocationCount: 2165
            BuildListCount: 26749
            LoopIterationCount: 0
            """);
    }

    // ===== String literal =====

    [Fact]
    public void Expression_string_literal()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseStringLiteral"),
                ElmString("\"hello world\""),
                s_vm);

        value.Should().Be(ElmString("hello world"));

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 176191
            InvocationCount: 8075
            BuildListCount: 52639
            LoopIterationCount: 0
            """);
    }

    // ===== Empty list =====

    [Fact]
    public void Expression_empty_list()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("[]"),
                s_vm);

        value.Should().Be(Ok(ListExpr()));

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 86411
            InvocationCount: 1591
            BuildListCount: 38752
            LoopIterationCount: 0
            """);
    }

    // ===== List with one item =====

    [Fact]
    public void Expression_list_one_item()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("[1]"),
                s_vm);

        value.Should().Be(
            Ok(
                ListExpr(
                    Node(1, 2, 1, 3, IntegerExpr(1)))));

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 537571
            InvocationCount: 15671
            BuildListCount: 207912
            LoopIterationCount: 0
            """);
    }

    // ===== List with ten items =====

    [Fact]
    public void Expression_list_ten_items()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("[1,2,3,4,5,6,7,8,9,10]"),
                s_vm);

        value.Should().Be(
            Ok(
                ListExpr(
                    Node(1, 2, 1, 3, IntegerExpr(1)),
                    Node(1, 4, 1, 5, IntegerExpr(2)),
                    Node(1, 6, 1, 7, IntegerExpr(3)),
                    Node(1, 8, 1, 9, IntegerExpr(4)),
                    Node(1, 10, 1, 11, IntegerExpr(5)),
                    Node(1, 12, 1, 13, IntegerExpr(6)),
                    Node(1, 14, 1, 15, IntegerExpr(7)),
                    Node(1, 16, 1, 17, IntegerExpr(8)),
                    Node(1, 18, 1, 19, IntegerExpr(9)),
                    Node(1, 20, 1, 22, IntegerExpr(10)))));

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 5359811
            InvocationCount: 159291
            BuildListCount: 1991532
            LoopIterationCount: 0
            """);
    }

    // ===== Flat list with 40 items =====

    [Fact]
    public void Expression_flat_list_forty_items()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100,101,102,103,104,105,106,107,108,109]"),
                s_vm);

        value.Should().Be(
            Ok(
                ListExpr(
                    Node(1, 2, 1, 3, IntegerExpr(1)),
                    Node(1, 4, 1, 5, IntegerExpr(2)),
                    Node(1, 6, 1, 7, IntegerExpr(3)),
                    Node(1, 8, 1, 9, IntegerExpr(4)),
                    Node(1, 10, 1, 11, IntegerExpr(5)),
                    Node(1, 12, 1, 13, IntegerExpr(6)),
                    Node(1, 14, 1, 15, IntegerExpr(7)),
                    Node(1, 16, 1, 17, IntegerExpr(8)),
                    Node(1, 18, 1, 19, IntegerExpr(9)),
                    Node(1, 20, 1, 22, IntegerExpr(10)),
                    Node(1, 23, 1, 25, IntegerExpr(11)),
                    Node(1, 26, 1, 28, IntegerExpr(12)),
                    Node(1, 29, 1, 31, IntegerExpr(13)),
                    Node(1, 32, 1, 34, IntegerExpr(14)),
                    Node(1, 35, 1, 37, IntegerExpr(15)),
                    Node(1, 38, 1, 40, IntegerExpr(16)),
                    Node(1, 41, 1, 43, IntegerExpr(17)),
                    Node(1, 44, 1, 46, IntegerExpr(18)),
                    Node(1, 47, 1, 49, IntegerExpr(19)),
                    Node(1, 50, 1, 52, IntegerExpr(20)),
                    Node(1, 53, 1, 55, IntegerExpr(21)),
                    Node(1, 56, 1, 58, IntegerExpr(22)),
                    Node(1, 59, 1, 61, IntegerExpr(23)),
                    Node(1, 62, 1, 64, IntegerExpr(24)),
                    Node(1, 65, 1, 67, IntegerExpr(25)),
                    Node(1, 68, 1, 70, IntegerExpr(26)),
                    Node(1, 71, 1, 73, IntegerExpr(27)),
                    Node(1, 74, 1, 76, IntegerExpr(28)),
                    Node(1, 77, 1, 79, IntegerExpr(29)),
                    Node(1, 80, 1, 82, IntegerExpr(30)),
                    Node(1, 83, 1, 86, IntegerExpr(100)),
                    Node(1, 87, 1, 90, IntegerExpr(101)),
                    Node(1, 91, 1, 94, IntegerExpr(102)),
                    Node(1, 95, 1, 98, IntegerExpr(103)),
                    Node(1, 99, 1, 102, IntegerExpr(104)),
                    Node(1, 103, 1, 106, IntegerExpr(105)),
                    Node(1, 107, 1, 110, IntegerExpr(106)),
                    Node(1, 111, 1, 114, IntegerExpr(107)),
                    Node(1, 115, 1, 118, IntegerExpr(108)),
                    Node(1, 119, 1, 122, IntegerExpr(109)))));

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 21884051
            InvocationCount: 653091
            BuildListCount: 8061732
            LoopIterationCount: 0
            """);
    }

    // ===== Nested list: 4 sublists of 10 items each (same 40 leaf items) =====

    [Fact]
    public void Expression_nested_list_four_by_ten()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("[[1,2,3,4,5,6,7,8,9,10],[11,12,13,14,15,16,17,18,19,20],[21,22,23,24,25,26,27,28,29,30],[100,101,102,103,104,105,106,107,108,109]]"),
                s_vm);

        value.Should().Be(
            Ok(
                ListExpr(
                    Node(
                        1,
                        2,
                        1,
                        24,
                        ListExpr(
                            Node(1, 3, 1, 4, IntegerExpr(1)),
                            Node(1, 5, 1, 6, IntegerExpr(2)),
                            Node(1, 7, 1, 8, IntegerExpr(3)),
                            Node(1, 9, 1, 10, IntegerExpr(4)),
                            Node(1, 11, 1, 12, IntegerExpr(5)),
                            Node(1, 13, 1, 14, IntegerExpr(6)),
                            Node(1, 15, 1, 16, IntegerExpr(7)),
                            Node(1, 17, 1, 18, IntegerExpr(8)),
                            Node(1, 19, 1, 20, IntegerExpr(9)),
                            Node(1, 21, 1, 23, IntegerExpr(10)))),
                    Node(
                        1,
                        25,
                        1,
                        56,
                        ListExpr(
                            Node(1, 26, 1, 28, IntegerExpr(11)),
                            Node(1, 29, 1, 31, IntegerExpr(12)),
                            Node(1, 32, 1, 34, IntegerExpr(13)),
                            Node(1, 35, 1, 37, IntegerExpr(14)),
                            Node(1, 38, 1, 40, IntegerExpr(15)),
                            Node(1, 41, 1, 43, IntegerExpr(16)),
                            Node(1, 44, 1, 46, IntegerExpr(17)),
                            Node(1, 47, 1, 49, IntegerExpr(18)),
                            Node(1, 50, 1, 52, IntegerExpr(19)),
                            Node(1, 53, 1, 55, IntegerExpr(20)))),
                    Node(
                        1,
                        57,
                        1,
                        88,
                        ListExpr(
                            Node(1, 58, 1, 60, IntegerExpr(21)),
                            Node(1, 61, 1, 63, IntegerExpr(22)),
                            Node(1, 64, 1, 66, IntegerExpr(23)),
                            Node(1, 67, 1, 69, IntegerExpr(24)),
                            Node(1, 70, 1, 72, IntegerExpr(25)),
                            Node(1, 73, 1, 75, IntegerExpr(26)),
                            Node(1, 76, 1, 78, IntegerExpr(27)),
                            Node(1, 79, 1, 81, IntegerExpr(28)),
                            Node(1, 82, 1, 84, IntegerExpr(29)),
                            Node(1, 85, 1, 87, IntegerExpr(30)))),
                    Node(
                        1,
                        89,
                        1,
                        130,
                        ListExpr(
                            Node(1, 90, 1, 93, IntegerExpr(100)),
                            Node(1, 94, 1, 97, IntegerExpr(101)),
                            Node(1, 98, 1, 101, IntegerExpr(102)),
                            Node(1, 102, 1, 105, IntegerExpr(103)),
                            Node(1, 106, 1, 109, IntegerExpr(104)),
                            Node(1, 110, 1, 113, IntegerExpr(105)),
                            Node(1, 114, 1, 117, IntegerExpr(106)),
                            Node(1, 118, 1, 121, IntegerExpr(107)),
                            Node(1, 122, 1, 125, IntegerExpr(108)),
                            Node(1, 126, 1, 129, IntegerExpr(109)))))));

        CoreLibraryModule.CoreLibraryTestHelper.FormatCounts(report).Should().Be(
            """
            InstructionCount: 237562051
            InvocationCount: 7106611
            BuildListCount: 87790852
            LoopIterationCount: 0
            """);
    }
}
