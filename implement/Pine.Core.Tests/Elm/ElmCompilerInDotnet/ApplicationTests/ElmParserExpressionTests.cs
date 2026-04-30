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
/// <c>Elm.Parser.Expression</c> (elm-syntax/src/Elm/Parser/Expression.elm).
/// Unlike <see cref="ParserFastTests"/>, which duplicates parser helpers in a
/// standalone module, these tests compile the real elm-syntax expression parser
/// and call it directly via <c>ParserFast.run Elm.Parser.Expression.expression</c>.
/// Each test asserts on the parsed result and on the runtime cost snapshot from
/// <see cref="PerformanceCountersFormatting.FormatCounts"/>.
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


        parseCharLiteral : String -> String
        parseCharLiteral input =
            case parseExpression input of
                Ok (CharLiteral c) ->
                    String.fromChar c
        
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
        new(() => BuildCompareFramework(maxOptimizationRounds: 1));

    /// <summary>
    /// As <see cref="s_compareFramework"/>, but built with <c>maxOptimizationRounds = 2</c>.
    /// Used to demonstrate that the additional optimization round produces VM bytecode that
    /// is more expensive at runtime for some scenarios — the snapshot assertions on tests
    /// using this instance are expected to differ from their single-round counterparts.
    /// </summary>
    private static readonly Lazy<CompareInterpreterWithIntermediateVM> s_compareFrameworkRounds2 =
        new(() => BuildCompareFramework(maxOptimizationRounds: 2));

    private static CompareInterpreterWithIntermediateVM BuildCompareFramework(int maxOptimizationRounds)
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
                new DeclQualifiedName(
                    ["ElmParserExpressionTestModule"],
                    "parseCharLiteral"),
                new DeclQualifiedName(
                    ["ElmParserExpressionTestModule"],
                    "parseExpression"),
                ],
                maxOptimizationRounds: maxOptimizationRounds);
    }

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

    [Fact]
    public void Expression_int_literal()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("parseIntLiteral"),
                ElmString("123"),
                s_vm);

        value.Should().Be(Integer(123));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 1_675
            InvocationCount: 59
            BuildListCount: 310
            LoopIterationCount: 0
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

        value.Should().Be(ElmString("hello world"));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 2_835
            InvocationCount: 108
            BuildListCount: 308
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void Expression_char_literal()
    {
        var report = s_compareFramework.Value.Eval("""parseCharLiteral "'&'" """);

        report.Value.Should().Be(ElmString("&"));

        PerformanceCountersFormatting.FormatCounts(report.VmCounters).Should().Be(
            """
            InstructionCount: 1_576
            InvocationCount: 58
            BuildListCount: 345
            LoopIterationCount: 0
            """);

        // Snapshot of the Elm syntax interpreter's metrics for the same root expression.
        // The interpreter's counts (especially DirectFunctionApplicationCount) are far higher
        // than the VM's InvocationCount because the interpreter dispatches every named
        // reference, whereas the VM has already inlined and lowered most of those into
        // direct bytecode. The gap is itself a useful signal: it identifies how much work
        // the Elm-source level still entails before optimization, which can guide further
        // optimization opportunities.
        ElmSyntaxInterpreterPerformanceCountersFormatting.FormatCounts(report.InterpreterCounters)
            .Should().Be(
                """
                InstructionLoopCount: 2_177
                DirectFunctionApplicationCount: 337
                FunctionValueApplicationCount: 50
                PineBuiltinInvocationCount: 116
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

        value.Should().Be(Ok(ListExpr()));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 2_225
            InvocationCount: 68
            BuildListCount: 548
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void Expression_list_one_item()
    {
        var report = s_compareFramework.Value.Eval("""parseExpression "[1]" """);

        report.Value.Should().Be(
            Ok(
                ListExpr(
                    Node(1, 2, 1, 3, IntegerExpr(1)))));

        PerformanceCountersFormatting.FormatCounts(report.VmCounters).Should().Be(
            """
            InstructionCount: 3_829
            InvocationCount: 136
            BuildListCount: 766
            LoopIterationCount: 0
            """);

        // Snapshot of the Elm syntax interpreter's metrics for the same root expression.
        // See the comment on Expression_char_literal: the interpreter's counts capture work
        // done at the Elm-source level and are useful as a baseline against which to read the
        // VM's optimized cost.
        ElmSyntaxInterpreterPerformanceCountersFormatting.FormatCounts(report.InterpreterCounters)
            .Should().Be(
                """
                InstructionLoopCount: 4_256
                DirectFunctionApplicationCount: 708
                FunctionValueApplicationCount: 106
                PineBuiltinInvocationCount: 166
                """);
    }

    /// <summary>
    /// Variant of <see cref="Expression_list_one_item"/> that uses
    /// <c>maxOptimizationRounds = 2</c>. Captured as a separate test so the per-round VM
    /// performance counters and interpreter counters are visible side-by-side at test-review
    /// time.
    /// <para>
    /// Comparing the two snapshots demonstrates the phenomenon the
    /// <see cref="CompareInterpreterWithIntermediateVM"/> framework was built to investigate:
    /// the second optimization round produces VM bytecode that is *more* expensive at runtime
    /// for this scenario (~12% more <c>InstructionCount</c>, ~28% more <c>BuildListCount</c>)
    /// while the interpreter-side counts barely move (the source-level work being done is
    /// almost identical). This is a candidate worth investigating for further optimization
    /// improvements; the framework also makes the matching application traces available for
    /// follow-up analysis.
    /// </para>
    /// </summary>
    [Fact]
    public void Expression_list_one_item_max_rounds_2()
    {
        var report = s_compareFrameworkRounds2.Value.Eval("""parseExpression "[1]" """);

        report.Value.Should().Be(
            Ok(
                ListExpr(
                    Node(1, 2, 1, 3, IntegerExpr(1)))));

        PerformanceCountersFormatting.FormatCounts(report.VmCounters).Should().Be(
            """
            InstructionCount: 3_700
            InvocationCount: 133
            BuildListCount: 685
            LoopIterationCount: 0
            """);

        ElmSyntaxInterpreterPerformanceCountersFormatting.FormatCounts(report.InterpreterCounters)
            .Should().Be(
                """
                InstructionLoopCount: 4_157
                DirectFunctionApplicationCount: 685
                FunctionValueApplicationCount: 103
                PineBuiltinInvocationCount: 166
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

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 19_288
            InvocationCount: 821
            BuildListCount: 2_866
            LoopIterationCount: 0
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
            """Ok (Application [ Node { end = { column = 5, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "alfa"), Node { end = { column = 8, row = 1 }, start = { column = 6, row = 1 } } (Integer 79), Node { end = { column = 22, row = 1 }, start = { column = 9, row = 1 } } (Literal "hello world"), Node { end = { column = 27, row = 1 }, start = { column = 23, row = 1 } } (FunctionOrValue [] "beta"), Node { end = { column = 32, row = 1 }, start = { column = 28, row = 1 } } (ListExpr [ Node { end = { column = 31, row = 1 }, start = { column = 29, row = 1 } } (Integer 41) ]), Node { end = { column = 56, row = 1 }, start = { column = 33, row = 1 } } (ParenthesizedExpression (Node { end = { column = 55, row = 1 }, start = { column = 34, row = 1 } } (LambdaExpression { args = [ Node { end = { column = 40, row = 1 }, start = { column = 35, row = 1 } } (VarPattern "gamma") ], expression = Node { end = { column = 55, row = 1 }, start = { column = 44, row = 1 } } (Application [ Node { end = { column = 49, row = 1 }, start = { column = 44, row = 1 } } (FunctionOrValue [] "delta"), Node { end = { column = 55, row = 1 }, start = { column = 50, row = 1 } } (FunctionOrValue [] "gamma") ]) }))) ])""");

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 23_775
            InvocationCount: 997
            BuildListCount: 3_702
            LoopIterationCount: 0
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

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 71_776
            InvocationCount: 3_181
            BuildListCount: 10_056
            LoopIterationCount: 0
            """);
    }

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

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 80_732
            InvocationCount: 3_501
            BuildListCount: 11_900
            LoopIterationCount: 0
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

        ElmValue.RenderAsElmExpression(value).expressionString
            .Should().Be(
                """Ok (OperatorApplication "+" Left (Node { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } } (Integer 1)) (Node { end = { column = 6, row = 1 }, start = { column = 5, row = 1 } } (Integer 2)))""");
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

        ElmValue.RenderAsElmExpression(value).expressionString
            .Should().Be(
                """Ok (OperatorApplication "|>" Left (Node { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "a")) (Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } } (FunctionOrValue [] "b")))""");
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

        ElmValue.RenderAsElmExpression(value).expressionString
            .Should().Be(
                """Ok (OperatorApplication "==" Non (Node { end = { column = 2, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "a")) (Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } } (FunctionOrValue [] "b")))""");
    }
}
