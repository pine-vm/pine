using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Tests for the <c>expression</c> parser from
/// <c>ElmSyntax.Concrete.Parser.FromString</c> (elm-in-elm\pine-elm-syntax\src\ElmSyntax\Concrete\Parser\FromString.elm).
/// Unlike <see cref="ParserFastTests"/>, which duplicates parser helpers in a
/// standalone module, these tests compile the real elm-syntax expression parser
/// and call it directly via <c>ParserFast.run Elm.Parser.Expression.expression</c>.
/// Each test asserts on the parsed result and on the runtime cost snapshot from
/// <see cref="PerformanceCountersFormatting.FormatCounts"/>.
/// </summary>
public class ElmParserNewExpressionTests
{
    /// <summary>
    /// Test wrapper module that imports the real elm-syntax expression parser
    /// and exposes thin functions for exercising it from C# tests.
    /// Each wrapper calls <c>ParserFast.run ElmSyntax.Concrete.Parser.FromString.expression</c>
    /// directly on the input string, bypassing <c>parseToFile</c>.
    /// </summary>
    private const string TestModuleText =
        """"
        module ElmParserExpressionTestModule exposing (..)

        import ElmSyntax.Concrete.Parser.FromString
        import ElmSyntax.Concrete.Expression exposing (Expression(..))
        import ElmSyntax.Concrete.Node exposing (Node(..))


        parseExpression : String -> Result String Expression
        parseExpression exprText =
            ElmSyntax.Concrete.Parser.FromString.parseExpression exprText


        parseIntLiteral : String -> Int
        parseIntLiteral input =
            case parseExpression input of
                Ok (IntegerLiteral n) ->
                    n

                _ ->
                    -1


        parseStringLiteral : String -> String
        parseStringLiteral input =
            case parseExpression input of
                Ok (StringLiteral s _) ->
                    s

                _ ->
                    ""


        parseCharLiteral : String -> String
        parseCharLiteral input =
            case parseExpression input of
                Ok (CharLiteral c) ->
                    String.fromChar (Char.fromCode c)
        
                _ ->
                    ""

        """"
        ;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var bundledTree =
                    BundledFiles.CompilerSourceContainerFilesDefault.Value;

                var kernelModulesTree =
                    BundledFiles.ElmKernelModulesDefault.Value;

                var elmSyntaxSrcTree =
                    bundledTree
                    .GetNodeAtPath(["pine-elm-syntax", "src"])
                    ?? throw new Exception("Did not find pine-elm-syntax/src");

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
                        rootFilePaths: rootFilePaths)
                    .Map(r => r.compiledEnvValue)
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

    /// <summary>
    /// Shared <see cref="CompareInterpreterWithIntermediateVM"/> instance built from the same
    /// source corpus used by the rest of this class. Wraps the existing VM-only test pattern
    /// in the framework that additionally evaluates each root expression through the
    /// <see cref="ElmSyntaxInterpreter"/>, so tests can assert both the VM-side
    /// <see cref="PerformanceCounters"/> and the interpreter-side
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters"/> snapshot from a single
    /// <c>Eval(...)</c> call.
    /// </summary>
    private static readonly Lazy<CompareInterpreterWithIntermediateVM> s_compareFramework =
        new(BuildCompareFramework);

    private static CompareInterpreterWithIntermediateVM BuildCompareFramework()
    {
        var bundledTree =
            BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var kernelModulesTree =
            BundledFiles.ElmKernelModulesDefault.Value;

        var elmSyntaxSrcTree =
            bundledTree
            .GetNodeAtPath(["pine-elm-syntax", "src"])
            ?? throw new Exception("Did not find pine-elm-syntax/src");

        var mergedTree = kernelModulesTree;

        foreach (var (path, file) in elmSyntaxSrcTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

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

        return
            CompareInterpreterWithIntermediateVM.Prepare(
                appCodeTree: treeWithTest,
                rootFilePaths: rootFilePaths,
                entryPoints:
                [
                DeclQualifiedName.Create(
                    ["ElmParserExpressionTestModule"],
                    "parseCharLiteral"),
                DeclQualifiedName.Create(
                    ["ElmParserExpressionTestModule"],
                    "parseExpression"),
                ]);
    }

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue ElmString(string s) =>
        ElmValue.StringInstance(s);

    private static ElmValue Ok(ElmValue inner) =>
        ElmValue.TagInstance("Ok", [inner]);

    private static ElmValue ListExpr(params ElmValue[] items) =>
        ElmValue.TagInstance("ListExpr", [ElmValue.ListInstance(items)]);

    private static ElmValue IntegerLiteralExpr(long n) =>
        ElmValue.TagInstance("IntegerLiteral", [ElmValue.Integer(n)]);

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

    [Fact]
    public void Expression_int_literal()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseIntLiteral"),
                ElmString("123"),
                s_vm);

        var rendered = ElmValue.RenderAsElmExpression(value);

        rendered.expressionString.Should().Be(
            """  "123"  """.Trim());

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InvocationCount: 208
            BuildListCount: 308
            LoopIterationCount: 0
            InstructionCount: 4_906
            """);
    }

    [Fact]
    public void Expression_string_literal()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseStringLiteral"),
                ElmString("\"hello world\""),
                s_vm);

        var rendered = ElmValue.RenderAsElmExpression(value);

        rendered.expressionString.Should().Be(
            """  "hello world"  """.Trim());

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InvocationCount: 205
            BuildListCount: 348
            LoopIterationCount: 0
            InstructionCount: 4_739
            """);
    }

    [Fact]
    public void Expression_char_literal()
    {
        var report = s_compareFramework.Value.Eval("""parseCharLiteral "'&'" """);

        report.Value.Should().Be(ElmString("&"));

        PerformanceCountersFormatting.FormatCounts(report.VmCounters).Should().Be(
            """
            InvocationCount: 191
            BuildListCount: 315
            LoopIterationCount: 0
            InstructionCount: 4_120
            """);

        // Snapshot of the Elm syntax interpreter's metrics for the same root expression.
        // The interpreter's counts (especially DirectFunctionApplicationCount) are far higher
        // than the VM's InvocationCount because the interpreter dispatches every named
        // reference, whereas the VM has already inlined and lowered most of those into
        // direct bytecode. The gap is itself a useful signal: it identifies how much work
        // the Elm-source level still entails before optimization, which can guide further
        // optimization opportunities.
        ElmSyntaxInterpreterPerformanceCountersFormatting.FormatCounts(report.InterpreterCounters).Should().Be(
            """
            InstructionLoopCount: 1_671
            DirectFunctionApplicationCount: 237
            FunctionValueApplicationCount: 0
            PineBuiltinInvocationCount: 132
            """);
    }

    [Fact]
    public void Expression_empty_list()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("[]"),
                s_vm);

        var rendered = ElmValue.RenderAsElmExpression(value);

        rendered.expressionString.Should().Be(
            """Ok (ListExpr Empty)""");

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InvocationCount: 276
            BuildListCount: 480
            LoopIterationCount: 0
            InstructionCount: 6_159
            """);
    }

    [Fact]
    public void Expression_list_one_item()
    {
        var report = s_compareFramework.Value.Eval("""parseExpression "[1]" """);

        var rendered = ElmValue.RenderAsElmExpression(report.Value);

        rendered.expressionString.Should().Be(
            """Ok (ListExpr (NonEmpty (Node { end = { column = 3, row = 1 }, start = { column = 2, row = 1 } } (IntegerLiteral "1")) []))""");

        PerformanceCountersFormatting.FormatCounts(report.VmCounters).Should().Be(
            """
            InvocationCount: 997
            BuildListCount: 1_492
            LoopIterationCount: 0
            InstructionCount: 21_016
            """);

        // Snapshot of the Elm syntax interpreter's metrics for the same root expression.
        // See the comment on Expression_char_literal: the interpreter's counts capture work
        // done at the Elm-source level and are useful as a baseline against which to read the
        // VM's optimized cost.
        ElmSyntaxInterpreterPerformanceCountersFormatting.FormatCounts(report.InterpreterCounters).Should().Be(
            """
            InstructionLoopCount: 3_474
            DirectFunctionApplicationCount: 498
            FunctionValueApplicationCount: 4
            PineBuiltinInvocationCount: 260
            """);
    }

    [Fact]
    public void Expression_list_ten_items()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("[1,2,3,4,5,6,7,8,9,10]"),
                s_vm);

        var rendered = ElmValue.RenderAsElmExpression(value);

        rendered.expressionString.Should().Be(
            """
            Ok (ListExpr (NonEmpty (Node { end = { column = 3, row = 1 }, start = { column = 2, row = 1 } } (IntegerLiteral "1")) [ ({ column = 3, row = 1 }, Node { end = { column = 5, row = 1 }, start = { column = 4, row = 1 } } (IntegerLiteral "2")), ({ column = 5, row = 1 }, Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } } (IntegerLiteral "3")), ({ column = 7, row = 1 }, Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } (IntegerLiteral "4")), ({ column = 9, row = 1 }, Node { end = { column = 11, row = 1 }, start = { column = 10, row = 1 } } (IntegerLiteral "5")), ({ column = 11, row = 1 }, Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } } (IntegerLiteral "6")), ({ column = 13, row = 1 }, Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } (IntegerLiteral "7")), ({ column = 15, row = 1 }, Node { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } } (IntegerLiteral "8")), ({ column = 17, row = 1 }, Node { end = { column = 19, row = 1 }, start = { column = 18, row = 1 } } (IntegerLiteral "9")), ({ column = 19, row = 1 }, Node { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } } (IntegerLiteral "10")) ]))
            """.Trim());

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InvocationCount: 9_132
            BuildListCount: 14_057
            LoopIterationCount: 0
            InstructionCount: 186_211
            """);
    }

    [Fact]
    public void Expression_application_with_various_argument_kinds()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("alfa 79 \"hello world\" beta [41] (\\gamma -> delta gamma)"),
                s_vm);

        var valueAsExpression =
            ElmValue.RenderAsElmExpression(value);

        valueAsExpression.expressionString.Should().Be(
            """Ok (Application (Node { end = { column = 5, row = 1 }, start = { column = 1, row = 1 } } (Identifier [] "alfa")) [ Node { end = { column = 8, row = 1 }, start = { column = 6, row = 1 } } (IntegerLiteral "79"), Node { end = { column = 22, row = 1 }, start = { column = 9, row = 1 } } (StringLiteral "hello world" (Just "hello world")), Node { end = { column = 27, row = 1 }, start = { column = 23, row = 1 } } (Identifier [] "beta"), Node { end = { column = 32, row = 1 }, start = { column = 28, row = 1 } } (ListExpr (NonEmpty (Node { end = { column = 31, row = 1 }, start = { column = 29, row = 1 } } (IntegerLiteral "41")) [])), Node { end = { column = 56, row = 1 }, start = { column = 33, row = 1 } } (Parenthesized (Node { end = { column = 55, row = 1 }, start = { column = 34, row = 1 } } (LambdaExpression { arguments = [ Node { end = { column = 40, row = 1 }, start = { column = 35, row = 1 } } (VarPattern "gamma") ], arrowLocation = { column = 41, row = 1 }, backslashLocation = { column = 34, row = 1 }, expression = Node { end = { column = 55, row = 1 }, start = { column = 44, row = 1 } } (Application (Node { end = { column = 49, row = 1 }, start = { column = 44, row = 1 } } (Identifier [] "delta")) [ Node { end = { column = 55, row = 1 }, start = { column = 50, row = 1 } } (Identifier [] "gamma") ]) }))) ])""");

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InvocationCount: 6_512
            BuildListCount: 9_707
            LoopIterationCount: 0
            InstructionCount: 135_036
            """);
    }

    [Fact]
    public void Expression_flat_list_forty_items()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100,101,102,103,104,105,106,107,108,109]"),
                s_vm);

        var rendered = ElmValue.RenderAsElmExpression(value);

        rendered.expressionString.Should().Be(
            """"
            Ok (ListExpr (NonEmpty (Node { end = { column = 3, row = 1 }, start = { column = 2, row = 1 } } (IntegerLiteral "1")) [ ({ column = 3, row = 1 }, Node { end = { column = 5, row = 1 }, start = { column = 4, row = 1 } } (IntegerLiteral "2")), ({ column = 5, row = 1 }, Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } } (IntegerLiteral "3")), ({ column = 7, row = 1 }, Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } (IntegerLiteral "4")), ({ column = 9, row = 1 }, Node { end = { column = 11, row = 1 }, start = { column = 10, row = 1 } } (IntegerLiteral "5")), ({ column = 11, row = 1 }, Node { end = { column = 13, row = 1 }, start = { column = 12, row = 1 } } (IntegerLiteral "6")), ({ column = 13, row = 1 }, Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } (IntegerLiteral "7")), ({ column = 15, row = 1 }, Node { end = { column = 17, row = 1 }, start = { column = 16, row = 1 } } (IntegerLiteral "8")), ({ column = 17, row = 1 }, Node { end = { column = 19, row = 1 }, start = { column = 18, row = 1 } } (IntegerLiteral "9")), ({ column = 19, row = 1 }, Node { end = { column = 22, row = 1 }, start = { column = 20, row = 1 } } (IntegerLiteral "10")), ({ column = 22, row = 1 }, Node { end = { column = 25, row = 1 }, start = { column = 23, row = 1 } } (IntegerLiteral "11")), ({ column = 25, row = 1 }, Node { end = { column = 28, row = 1 }, start = { column = 26, row = 1 } } (IntegerLiteral "12")), ({ column = 28, row = 1 }, Node { end = { column = 31, row = 1 }, start = { column = 29, row = 1 } } (IntegerLiteral "13")), ({ column = 31, row = 1 }, Node { end = { column = 34, row = 1 }, start = { column = 32, row = 1 } } (IntegerLiteral "14")), ({ column = 34, row = 1 }, Node { end = { column = 37, row = 1 }, start = { column = 35, row = 1 } } (IntegerLiteral "15")), ({ column = 37, row = 1 }, Node { end = { column = 40, row = 1 }, start = { column = 38, row = 1 } } (IntegerLiteral "16")), ({ column = 40, row = 1 }, Node { end = { column = 43, row = 1 }, start = { column = 41, row = 1 } } (IntegerLiteral "17")), ({ column = 43, row = 1 }, Node { end = { column = 46, row = 1 }, start = { column = 44, row = 1 } } (IntegerLiteral "18")), ({ column = 46, row = 1 }, Node { end = { column = 49, row = 1 }, start = { column = 47, row = 1 } } (IntegerLiteral "19")), ({ column = 49, row = 1 }, Node { end = { column = 52, row = 1 }, start = { column = 50, row = 1 } } (IntegerLiteral "20")), ({ column = 52, row = 1 }, Node { end = { column = 55, row = 1 }, start = { column = 53, row = 1 } } (IntegerLiteral "21")), ({ column = 55, row = 1 }, Node { end = { column = 58, row = 1 }, start = { column = 56, row = 1 } } (IntegerLiteral "22")), ({ column = 58, row = 1 }, Node { end = { column = 61, row = 1 }, start = { column = 59, row = 1 } } (IntegerLiteral "23")), ({ column = 61, row = 1 }, Node { end = { column = 64, row = 1 }, start = { column = 62, row = 1 } } (IntegerLiteral "24")), ({ column = 64, row = 1 }, Node { end = { column = 67, row = 1 }, start = { column = 65, row = 1 } } (IntegerLiteral "25")), ({ column = 67, row = 1 }, Node { end = { column = 70, row = 1 }, start = { column = 68, row = 1 } } (IntegerLiteral "26")), ({ column = 70, row = 1 }, Node { end = { column = 73, row = 1 }, start = { column = 71, row = 1 } } (IntegerLiteral "27")), ({ column = 73, row = 1 }, Node { end = { column = 76, row = 1 }, start = { column = 74, row = 1 } } (IntegerLiteral "28")), ({ column = 76, row = 1 }, Node { end = { column = 79, row = 1 }, start = { column = 77, row = 1 } } (IntegerLiteral "29")), ({ column = 79, row = 1 }, Node { end = { column = 82, row = 1 }, start = { column = 80, row = 1 } } (IntegerLiteral "30")), ({ column = 82, row = 1 }, Node { end = { column = 86, row = 1 }, start = { column = 83, row = 1 } } (IntegerLiteral "100")), ({ column = 86, row = 1 }, Node { end = { column = 90, row = 1 }, start = { column = 87, row = 1 } } (IntegerLiteral "101")), ({ column = 90, row = 1 }, Node { end = { column = 94, row = 1 }, start = { column = 91, row = 1 } } (IntegerLiteral "102")), ({ column = 94, row = 1 }, Node { end = { column = 98, row = 1 }, start = { column = 95, row = 1 } } (IntegerLiteral "103")), ({ column = 98, row = 1 }, Node { end = { column = 102, row = 1 }, start = { column = 99, row = 1 } } (IntegerLiteral "104")), ({ column = 102, row = 1 }, Node { end = { column = 106, row = 1 }, start = { column = 103, row = 1 } } (IntegerLiteral "105")), ({ column = 106, row = 1 }, Node { end = { column = 110, row = 1 }, start = { column = 107, row = 1 } } (IntegerLiteral "106")), ({ column = 110, row = 1 }, Node { end = { column = 114, row = 1 }, start = { column = 111, row = 1 } } (IntegerLiteral "107")), ({ column = 114, row = 1 }, Node { end = { column = 118, row = 1 }, start = { column = 115, row = 1 } } (IntegerLiteral "108")), ({ column = 118, row = 1 }, Node { end = { column = 122, row = 1 }, start = { column = 119, row = 1 } } (IntegerLiteral "109")) ]))
            """".Trim());

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InvocationCount: 37_202
            BuildListCount: 56_967
            LoopIterationCount: 0
            InstructionCount: 756_661
            """);
    }

    [Fact]
    public void Expression_nested_list_four_by_ten()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString(
                    """
                    [ [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    , [ 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ]
                    , [ 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 ]
                    , [ 100, 101, 102, 103, 104, 105, 106, 107, 108, 109 ]
                    ]
                    """.Trim()),
                s_vm);

        var rendered = ElmValue.RenderAsElmExpression(value);

        rendered.expressionString.Should().Be(
            """"
            Ok (ListExpr (NonEmpty (Node { end = { column = 36, row = 1 }, start = { column = 3, row = 1 } } (ListExpr (NonEmpty (Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } (IntegerLiteral "1")) [ ({ column = 6, row = 1 }, Node { end = { column = 9, row = 1 }, start = { column = 8, row = 1 } } (IntegerLiteral "2")), ({ column = 9, row = 1 }, Node { end = { column = 12, row = 1 }, start = { column = 11, row = 1 } } (IntegerLiteral "3")), ({ column = 12, row = 1 }, Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } (IntegerLiteral "4")), ({ column = 15, row = 1 }, Node { end = { column = 18, row = 1 }, start = { column = 17, row = 1 } } (IntegerLiteral "5")), ({ column = 18, row = 1 }, Node { end = { column = 21, row = 1 }, start = { column = 20, row = 1 } } (IntegerLiteral "6")), ({ column = 21, row = 1 }, Node { end = { column = 24, row = 1 }, start = { column = 23, row = 1 } } (IntegerLiteral "7")), ({ column = 24, row = 1 }, Node { end = { column = 27, row = 1 }, start = { column = 26, row = 1 } } (IntegerLiteral "8")), ({ column = 27, row = 1 }, Node { end = { column = 30, row = 1 }, start = { column = 29, row = 1 } } (IntegerLiteral "9")), ({ column = 30, row = 1 }, Node { end = { column = 34, row = 1 }, start = { column = 32, row = 1 } } (IntegerLiteral "10")) ]))) [ ({ column = 1, row = 2 }, Node { end = { column = 45, row = 2 }, start = { column = 3, row = 2 } } (ListExpr (NonEmpty (Node { end = { column = 7, row = 2 }, start = { column = 5, row = 2 } } (IntegerLiteral "11")) [ ({ column = 7, row = 2 }, Node { end = { column = 11, row = 2 }, start = { column = 9, row = 2 } } (IntegerLiteral "12")), ({ column = 11, row = 2 }, Node { end = { column = 15, row = 2 }, start = { column = 13, row = 2 } } (IntegerLiteral "13")), ({ column = 15, row = 2 }, Node { end = { column = 19, row = 2 }, start = { column = 17, row = 2 } } (IntegerLiteral "14")), ({ column = 19, row = 2 }, Node { end = { column = 23, row = 2 }, start = { column = 21, row = 2 } } (IntegerLiteral "15")), ({ column = 23, row = 2 }, Node { end = { column = 27, row = 2 }, start = { column = 25, row = 2 } } (IntegerLiteral "16")), ({ column = 27, row = 2 }, Node { end = { column = 31, row = 2 }, start = { column = 29, row = 2 } } (IntegerLiteral "17")), ({ column = 31, row = 2 }, Node { end = { column = 35, row = 2 }, start = { column = 33, row = 2 } } (IntegerLiteral "18")), ({ column = 35, row = 2 }, Node { end = { column = 39, row = 2 }, start = { column = 37, row = 2 } } (IntegerLiteral "19")), ({ column = 39, row = 2 }, Node { end = { column = 43, row = 2 }, start = { column = 41, row = 2 } } (IntegerLiteral "20")) ]))), ({ column = 1, row = 3 }, Node { end = { column = 45, row = 3 }, start = { column = 3, row = 3 } } (ListExpr (NonEmpty (Node { end = { column = 7, row = 3 }, start = { column = 5, row = 3 } } (IntegerLiteral "21")) [ ({ column = 7, row = 3 }, Node { end = { column = 11, row = 3 }, start = { column = 9, row = 3 } } (IntegerLiteral "22")), ({ column = 11, row = 3 }, Node { end = { column = 15, row = 3 }, start = { column = 13, row = 3 } } (IntegerLiteral "23")), ({ column = 15, row = 3 }, Node { end = { column = 19, row = 3 }, start = { column = 17, row = 3 } } (IntegerLiteral "24")), ({ column = 19, row = 3 }, Node { end = { column = 23, row = 3 }, start = { column = 21, row = 3 } } (IntegerLiteral "25")), ({ column = 23, row = 3 }, Node { end = { column = 27, row = 3 }, start = { column = 25, row = 3 } } (IntegerLiteral "26")), ({ column = 27, row = 3 }, Node { end = { column = 31, row = 3 }, start = { column = 29, row = 3 } } (IntegerLiteral "27")), ({ column = 31, row = 3 }, Node { end = { column = 35, row = 3 }, start = { column = 33, row = 3 } } (IntegerLiteral "28")), ({ column = 35, row = 3 }, Node { end = { column = 39, row = 3 }, start = { column = 37, row = 3 } } (IntegerLiteral "29")), ({ column = 39, row = 3 }, Node { end = { column = 43, row = 3 }, start = { column = 41, row = 3 } } (IntegerLiteral "30")) ]))), ({ column = 1, row = 4 }, Node { end = { column = 55, row = 4 }, start = { column = 3, row = 4 } } (ListExpr (NonEmpty (Node { end = { column = 8, row = 4 }, start = { column = 5, row = 4 } } (IntegerLiteral "100")) [ ({ column = 8, row = 4 }, Node { end = { column = 13, row = 4 }, start = { column = 10, row = 4 } } (IntegerLiteral "101")), ({ column = 13, row = 4 }, Node { end = { column = 18, row = 4 }, start = { column = 15, row = 4 } } (IntegerLiteral "102")), ({ column = 18, row = 4 }, Node { end = { column = 23, row = 4 }, start = { column = 20, row = 4 } } (IntegerLiteral "103")), ({ column = 23, row = 4 }, Node { end = { column = 28, row = 4 }, start = { column = 25, row = 4 } } (IntegerLiteral "104")), ({ column = 28, row = 4 }, Node { end = { column = 33, row = 4 }, start = { column = 30, row = 4 } } (IntegerLiteral "105")), ({ column = 33, row = 4 }, Node { end = { column = 38, row = 4 }, start = { column = 35, row = 4 } } (IntegerLiteral "106")), ({ column = 38, row = 4 }, Node { end = { column = 43, row = 4 }, start = { column = 40, row = 4 } } (IntegerLiteral "107")), ({ column = 43, row = 4 }, Node { end = { column = 48, row = 4 }, start = { column = 45, row = 4 } } (IntegerLiteral "108")), ({ column = 48, row = 4 }, Node { end = { column = 53, row = 4 }, start = { column = 50, row = 4 } } (IntegerLiteral "109")) ]))) ]))
            """".Trim());

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InvocationCount: 47_230
            BuildListCount: 69_899
            LoopIterationCount: 0
            InstructionCount: 961_341
            """);
    }

    /// <summary>
    /// Parses the bare expression <c>"1 + 2"</c> via the real
    /// <c>Elm.Parser.Expression.expression</c>. This is the narrowest
    /// reproduction of the open compile-to-PineVM defect tracked in
    /// <c>ElmSyntaxInterpreter-language-service-gaps.md</c>: it fails
    /// with the same
    /// <c>Failed to parse expression from value: Unexpected number of
    /// items in list: Not 2 but 0 — expressionValue is string ''</c>
    /// symptom observed through <c>addWorkspaceFile</c> and through the
    /// file-level reproductions in
    /// <see cref="ElmParserFileTests.File_matches_language_service_scenario_ModuleA"/>
    /// and
    /// <see cref="ElmParserFileTests.File_matches_language_service_scenario_ModuleB"/>,
    /// but without needing an entire module header around it.
    /// <para>
    /// The passing sibling tests
    /// <see cref="Expression_int_literal"/>,
    /// <see cref="Expression_application_with_various_argument_kinds"/>,
    /// and the list-expression cases prove that the expression
    /// parser's literal, application, lambda, and list code paths all
    /// compile to IR correctly. What this test adds is the
    /// precedence-climbing path — specifically <c>precedence6Add</c> in
    /// <c>elm-syntax/src/Elm/Parser/Expression.elm</c> and the
    /// surrounding combinators — which the previous passing tests
    /// never reach because their inputs contain no infix operator.
    /// </para>
    /// </summary>
    [Fact]
    public void Expression_int_plus_int()
    {
        var (value, _) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("1 + 2"),
                s_vm);

        var rendered = ElmValue.RenderAsElmExpression(value);

        rendered.expressionString
            .Should().Be(
            """Ok (OperatorApplication (Node { end = { column = 4, row = 1 }, start = { column = 3, row = 1 } } "+") Left (Node { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } } (IntegerLiteral "1")) (Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } (IntegerLiteral "2")))""");
    }

    /// <summary>
    /// Companion to <see cref="Expression_int_plus_int"/>: probes whether
    /// the compile-to-PineVM defect also reproduces on the <c>|&gt;</c>
    /// operator (another <c>infixLeft</c>). If this fails identically to
    /// <see cref="Expression_int_plus_int"/> the defect is not specific
    /// to <c>precedence6Add</c> or to <c>Basics.add</c>, and the shared
    /// <c>infixLeft</c> / precedence-climbing machinery is the suspect;
    /// if it passes, something distinguishes <c>+</c> from <c>|&gt;</c>.
    /// </summary>
    [Fact]
    public void Expression_value_pipeRight_value()
    {
        var (value, _) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("a |> b"),
                s_vm);

        var rendered = ElmValue.RenderAsElmExpression(value);

        rendered.expressionString
            .Should().Be(
            """Ok (OperatorApplication (Node { end = { column = 5, row = 1 }, start = { column = 3, row = 1 } } "|>") Left (Node { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } } (Identifier [] "a")) (Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } } (Identifier [] "b")))""");
    }

    /// <summary>
    /// Companion probe: <c>==</c> is <c>infixNonAssociative 4 "=="</c>,
    /// a sibling of <c>infixLeft</c> sharing the same
    /// <c>extendedSubExpressionOptimisticLayout</c> machinery. Used to
    /// discriminate between <c>infixLeft</c>-only defects and
    /// defects in the shared precedence-climbing code path.
    /// </summary>
    [Fact]
    public void Expression_value_eq_value()
    {
        var (value, _) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseExpression"),
                ElmString("a == b"),
                s_vm);

        var rendered = ElmValue.RenderAsElmExpression(value);

        rendered.expressionString
            .Should().Be(
            """Ok (OperatorApplication (Node { end = { column = 5, row = 1 }, start = { column = 3, row = 1 } } "==") Non (Node { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } } (Identifier [] "a")) (Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } } (Identifier [] "b")))""");
    }
}
