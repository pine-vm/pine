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
/// Tests for functions from the ParserFast module
/// (elm-syntax/src/ParserFast.elm).
/// Exercises <c>skipWhileWhitespaceHelp</c> and
/// <c>skipWhileWithoutLinebreakHelp</c> by copying them
/// into a standalone test module, then calling them through
/// wrapper functions that exercise the parsers and return
/// simple values. Each test asserts on the return value
/// and on the runtime cost snapshot from
/// <see cref="PerformanceCountersFormatting.FormatCounts"/>.
/// </summary>
public class ParserFastTests
{
    /// <summary>
    /// Self-contained test module that duplicates the relevant
    /// ParserFast types and helpers so we can call them from
    /// test wrapper functions without cross-module String
    /// type issues.
    /// </summary>
    private const string TestModuleText =
        """"
        module ParserFastTestModule exposing (..)


        type Parser a
            = Parser (State -> PStep a)


        type PStep value
            = Good value State
            | Bad Bool Problem


        type State
            = PState
                Int
                Int
                Int
                Int
                Int


        type String
            = String Int


        type Problem
            = ExpectingNumber Int Int
            | ExpectingSymbol Int Int String
            | ExpectingAnyChar Int Int
            | ExpectingKeyword Int Int String
            | ExpectingCharSatisfyingPredicate Int Int
            | ExpectingStringSatisfyingPredicate Int Int
            | ExpectingCustom Int Int String
            | ExpectingOneOf Problem Problem (List Problem)


        pStepCommit : PStep a -> PStep a
        pStepCommit pStep =
            case pStep of
                Good res s ->
                    Good res s

                Bad _ x ->
                    Bad True x


        -- ====== skipWhileWhitespaceHelp ======

        skipWhileWhitespaceHelp : Int -> Int -> Int -> Int -> Int -> State
        skipWhileWhitespaceHelp offsetBytes row col srcBytes indent =
            case Pine_kernel.take [ 4, Pine_kernel.skip [ offsetBytes, srcBytes ] ] of
                ' ' ->
                    skipWhileWhitespaceHelp
                        (offsetBytes + 4)
                        row
                        (col + 1)
                        srcBytes
                        indent

                '\n' ->
                    skipWhileWhitespaceHelp
                        (offsetBytes + 4)
                        (row + 1)
                        1
                        srcBytes
                        indent

                '\u{000D}' ->
                    skipWhileWhitespaceHelp
                        (offsetBytes + 4)
                        row
                        (col + 1)
                        srcBytes
                        indent

                _ ->
                    PState srcBytes offsetBytes indent row col


        skipWhileWhitespaceFollowedBy : Parser next -> Parser next
        skipWhileWhitespaceFollowedBy (Parser parseNext) =
            Parser
                (\(PState s0SrcBytes s0OffsetBytes s0Indent s0Row s0Col) ->
                    let
                        s1 : State
                        s1 =
                            skipWhileWhitespaceHelp s0OffsetBytes s0Row s0Col s0SrcBytes s0Indent
                    in
                    parseNext s1 |> pStepCommit
                )


        -- ====== skipWhileWithoutLinebreakHelp ======

        skipWhileWithoutLinebreakHelp : (Char -> Bool) -> Int -> Int -> Int -> Int -> Int -> State
        skipWhileWithoutLinebreakHelp isGood offset row col srcBytes indent =
            let
                nextChar : Int
                nextChar =
                    Pine_kernel.take [ 4, Pine_kernel.skip [ offset, srcBytes ] ]
            in
            if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                PState srcBytes offset indent row col

            else if isGood nextChar then
                skipWhileWithoutLinebreakHelp
                    isGood
                    (offset + 4)
                    row
                    (col + 1)
                    srcBytes
                    indent

            else
                PState srcBytes offset indent row col


        whileWithoutLinebreak : (Char -> Bool) -> Parser String
        whileWithoutLinebreak isGood =
            Parser
                (\(PState s0SrcBytes s0Offset s0Indent s0Row s0Col) ->
                    let
                        s0OffsetInt : Int
                        s0OffsetInt =
                            s0Offset

                        s1 : State
                        s1 =
                            skipWhileWithoutLinebreakHelp isGood s0Offset s0Row s0Col s0SrcBytes s0Indent

                        (PState _ s1Offset _ _ _) =
                            s1

                        s1OffsetInt : Int
                        s1OffsetInt =
                            s1Offset

                        sliceBytesLength : Int
                        sliceBytesLength =
                            Pine_kernel.int_add
                                [ s1OffsetInt
                                , Pine_kernel.int_mul [ -1, s0OffsetInt ]
                                ]

                        sliceBytes : Int
                        sliceBytes =
                            Pine_kernel.take
                                [ sliceBytesLength
                                , Pine_kernel.skip [ s0OffsetInt, s0SrcBytes ]
                                ]
                    in
                    Good
                        (String sliceBytes)
                        s1
                )


        -- ====== symbol (needed as a building block) ======

        symbol : String -> res -> Parser res
        symbol ((String strBytes) as str) res =
            let
                strBytesLength : Int
                strBytesLength =
                    Pine_kernel.length strBytes

                strLength : Int
                strLength =
                    Pine_kernel.concat
                        [ Pine_kernel.take [ 1, 0 ]
                        , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, strBytesLength ] ]
                        ]
            in
            Parser
                (\(PState sSrcBytes sOffset sIndent sRow sCol) ->
                    let
                        sOffsetInt : Int
                        sOffsetInt =
                            sOffset

                        sColInt : Int
                        sColInt =
                            sCol
                    in
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.take [ strBytesLength, Pine_kernel.skip [ sOffsetInt, sSrcBytes ] ]
                            , strBytes
                            ]
                    then
                        Good res
                            (PState sSrcBytes
                                (sOffsetInt + strBytesLength)
                                sIndent
                                sRow
                                (sColInt + strLength)
                            )

                    else
                        Bad False (ExpectingSymbol sRow sCol str)
                )


        map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
        map2 func (Parser parseA) (Parser parseB) =
            Parser
                (\s0 ->
                    case parseA s0 of
                        Good a s1 ->
                            case parseB s1 of
                                Good b s2 ->
                                    Good (func a b) s2

                                Bad committed x ->
                                    Bad True x

                        Bad committed x ->
                            Bad committed x
                )


        map : (a -> b) -> Parser a -> Parser b
        map func (Parser parse) =
            Parser
                (\s0 ->
                    case parse s0 of
                        Good a s1 ->
                            Good (func a) s1

                        Bad committed x ->
                            Bad committed x
                )


        -- ====== run (entry point) ======

        run : Parser a -> String -> Result (List { row : Int, col : Int }) a
        run (Parser parse) (String srcBytes) =
            case parse (PState srcBytes 0 1 1 1) of
                Good value (PState finalSrc finalOffset _ finalRow finalCol) ->
                    if Pine_kernel.equal [ finalOffset, Pine_kernel.length srcBytes ] then
                        Ok value

                    else
                        Err [ { row = finalRow, col = finalCol } ]

                Bad _ deadEnds ->
                    Err []


        -- ====== Test wrapper functions ======

        {-| Test skipWhileWhitespaceHelp directly: skip whitespace in the
        input string and return the byte offset where non-whitespace begins.
        Consolidates testSkipWhitespace_spaces, testSkipWhitespace_newlines,
        and testSkipWhitespace_none which all looked the same after
        parameterizing the input.
        -}
        testSkipWhitespace : String -> Int
        testSkipWhitespace (String srcBytes) =
            let
                (PState _ offset _ _ _) =
                    skipWhileWhitespaceHelp 0 1 1 srcBytes 1
            in
            offset

        {-| Parse input matching alpha chars via whileWithoutLinebreak.
        Returns the byte-length of the matched slice divided by 4
        (i.e. the character count). Exercises skipWhileWithoutLinebreakHelp.
        -}
        testWithoutLinebreak_alpha : String -> Int
        testWithoutLinebreak_alpha input =
            case
                run
                    (map
                        (\(String bytes) ->
                            Pine_kernel.concat
                                [ Pine_kernel.take [ 1, 0 ]
                                , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, Pine_kernel.length bytes ] ]
                                ]
                        )
                        (whileWithoutLinebreak Char.isAlpha)
                    )
                    input
            of
                Ok v ->
                    v

                Err _ ->
                    -1

        {-| Parse input matching digits. Returns char count.
        Exercises skipWhileWithoutLinebreakHelp with Char.isDigit.
        -}
        testWithoutLinebreak_digits : String -> Int
        testWithoutLinebreak_digits input =
            case
                run
                    (map
                        (\(String bytes) ->
                            Pine_kernel.concat
                                [ Pine_kernel.take [ 1, 0 ]
                                , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, Pine_kernel.length bytes ] ]
                                ]
                        )
                        (whileWithoutLinebreak Char.isDigit)
                    )
                    input
            of
                Ok v ->
                    v

                Err _ ->
                    -1

        {-| Parse input matching alpha chars then "!" symbol.
        Returns matched alpha char count.
        Exercises skipWhileWithoutLinebreakHelp in combination with symbol.
        -}
        testWithoutLinebreak_thenSymbol : String -> Int
        testWithoutLinebreak_thenSymbol input =
            case
                run
                    (map2
                        (\(String bytes) _ ->
                            Pine_kernel.concat
                                [ Pine_kernel.take [ 1, 0 ]
                                , Pine_kernel.bit_shift_right [ 2, Pine_kernel.skip [ 1, Pine_kernel.length bytes ] ]
                                ]
                        )
                        (whileWithoutLinebreak Char.isAlpha)
                        (symbol "!" ())
                    )
                    input
            of
                Ok v ->
                    v

                Err _ ->
                    -1
        """"
        ;

    // ---------- compilation + environment ----------

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.ElmKernelModulesDefault.Value;

                // Add our test module to the kernel modules tree
                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["ParserFastTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("ParserFastTestModule.elm", StringComparison.OrdinalIgnoreCase))
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
        .First(m => m.moduleName is "ParserFastTestModule")
        .moduleContent.FunctionDeclarations[name];

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    /// <summary>
    /// Shared <see cref="CompareInterpreterWithIntermediateVM"/> framework instance for
    /// this test class. Compiled once (via <see cref="Lazy{T}"/>) from the same
    /// <see cref="BundledFiles.ElmKernelModulesDefault"/> + <see cref="TestModuleText"/>
    /// tree as <see cref="s_env"/>, so the lowered Elm syntax declarations the
    /// <see cref="ElmSyntaxInterpreter"/> sees match the bytecode the
    /// <see cref="Core.Interpreter.IntermediateVM.PineVM"/> path executes.
    /// <para>
    /// Used by <see cref="SkipWhileWithoutLinebreakHelp_alpha_longer"/> to dispatch
    /// <c>testWithoutLinebreak_alpha</c> through the syntax interpreter and capture every
    /// function application as an <see cref="ApplicationLogEntry"/>. Snapshotting the
    /// rendered application log gives a tight feedback loop on which functions are
    /// actually invoked at runtime — a missing specialization or failed inlining shows up
    /// directly as extra entries in the log.
    /// </para>
    /// </summary>
    private static readonly Lazy<CompareInterpreterWithIntermediateVM> s_compareFramework =
        new(
            () =>
            {
                var kernelModulesTree =
                    BundledFiles.ElmKernelModulesDefault.Value;

                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["ParserFastTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        b =>
                        b.path[^1].Equals("ParserFastTestModule.elm", StringComparison.OrdinalIgnoreCase))
                    .Select(b => (IReadOnlyList<string>)b.path)
                    .ToList();

                return
                    CompareInterpreterWithIntermediateVM.Prepare(
                        appCodeTree: treeWithTest,
                        rootFilePaths: rootFilePaths,
                        entryPoints:
                        [
                        new DeclQualifiedName(
                            ["ParserFastTestModule"],
                            "testWithoutLinebreak_alpha"),
                        ],
                        maxOptimizationRounds: ElmCompiler.MaxOptimizationRoundsDefault);
            });

    /// <summary>
    /// Applies <paramref name="functionValue"/> to <paramref name="argument"/> with a
    /// fresh VM that captures every function invocation via the
    /// <c>reportFunctionApplication</c> callback. Returns the result, the aggregated
    /// performance counters, and the list of per-invocation reports so callers can
    /// inspect which functions were called and how often.
    /// </summary>
    private static (ElmValue value, PerformanceCounters report, IReadOnlyList<EvaluationReport> invocations)
        ApplyAndRecordInvocations(
            PineValue functionValue,
            ElmValue argument)
    {
        var invocations = new List<EvaluationReport>();
        var vm = ElmCompilerTestHelper.PineVMForProfiling(invocations.Add);

        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                functionValue,
                argument,
                vm);

        return (value, report, invocations);
    }

    private static ElmValue Integer(long i) =>
        ElmValue.Integer(i);

    private static ElmValue ElmString(string s) =>
        ElmValue.StringInstance(s);

    // ===== skipWhileWhitespaceHelp tests =====

    [Fact]
    public void SkipWhileWhitespaceHelp_spaces_then_symbol()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testSkipWhitespace"),
                ElmString("   ok"),
                s_vm);

        value.Should().Be(Integer(12));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 2
            InvocationCount: 0
            BuildListCount: 0
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void SkipWhileWhitespaceHelp_with_newlines()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testSkipWhitespace"),
                ElmString(" \n  x"),
                s_vm);

        value.Should().Be(Integer(16));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 2
            InvocationCount: 0
            BuildListCount: 0
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void SkipWhileWhitespaceHelp_no_whitespace()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testSkipWhitespace"),
                ElmString("ab"),
                s_vm);

        value.Should().Be(Integer(0));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 2
            InvocationCount: 0
            BuildListCount: 0
            LoopIterationCount: 0
            """);
    }

    // ===== skipWhileWithoutLinebreakHelp tests =====

    [Fact]
    public void SkipWhileWithoutLinebreakHelp_alpha()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testWithoutLinebreak_alpha"),
                ElmString("hello"),
                s_vm);

        value.Should().Be(Integer(5));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 208
            InvocationCount: 9
            BuildListCount: 6
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void SkipWhileWithoutLinebreakHelp_digits()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testWithoutLinebreak_digits"),
                ElmString("12345"),
                s_vm);

        value.Should().Be(Integer(5));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 184
            InvocationCount: 9
            BuildListCount: 6
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void SkipWhileWithoutLinebreakHelp_then_symbol()
    {
        var (value, report) =
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                GetTestFunction("testWithoutLinebreak_thenSymbol"),
                ElmString("abc!"),
                s_vm);

        value.Should().Be(Integer(3));

        PerformanceCountersFormatting.FormatCounts(report).Should().Be(
            """
            InstructionCount: 258
            InvocationCount: 9
            BuildListCount: 14
            LoopIterationCount: 0
            """);
    }

    [Fact]
    public void SkipWhileWithoutLinebreakHelp_alpha_longer()
    {
        const string Input1 = "abcdefghijklmnopqrstuvwxyz";
        const string Input2 = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";

        // -------- Smaller input scenario: snapshot the syntax-interpreter invocation log.
        //
        // Before checking aggregate counters on the larger inputs, run the same workload
        // on a tiny 3-character input through the ElmSyntaxInterpreter and snapshot the
        // exact sequence of function applications it dispatches — filtered to drop
        // Pine_builtin / Pine_kernel leaf operations (kernel primitives, integer
        // arithmetic, etc.) that dominate any per-line trace but reveal nothing about
        // which user-level functions are dispatched. The lowered Elm syntax declarations
        // the interpreter sees come from CompilationPipelineStageResults.ModulesForCompilation,
        // so the call graph observed here is the same one the bytecode-emission backend
        // sees. One line per dispatched application — `direct <QualifiedName> [ args ]`
        // for name-based dispatch and `fnvalue <identity> [ args ]` for closure
        // application — makes it immediately visible *why* the predicate function is (or
        // is not) inlined into the recursive helper. A future fix that successfully
        // inlines `Char.isAlpha` into a specialized
        // `skipWhileWithoutLinebreakHelp__specialized__1` will surface here as the
        // disappearance of the per-character `Char.isAlpha` and
        // `whileWithoutLinebreak__lifted__lambda*` lines.
        var smallReport =
            s_compareFramework.Value.Eval("""testWithoutLinebreak_alpha "abc" """);

        smallReport.Value.Should().Be(Integer(3));

        var renderedInvocationLog =
            CompareInterpreterWithIntermediateVM.RenderApplicationLog(
                CompareInterpreterWithIntermediateVM.WithoutPineBuiltinInvocations(
                    smallReport.ApplicationLog));

        renderedInvocationLog.Should().Be(
            ExpectedInterpreterInvocationLog_testWithoutLinebreak_alpha_abc);

        var (value1, report1, invocations1) =
            ApplyAndRecordInvocations(
                GetTestFunction("testWithoutLinebreak_alpha"),
                ElmString(Input1));

        value1.Should().Be(Integer(26));

        PerformanceCountersFormatting.FormatCounts(report1).Should().Be(
            """
            InstructionCount: 943
            InvocationCount: 51
            BuildListCount: 27
            LoopIterationCount: 0
            """);

        var (value2, report2, invocations2) =
            ApplyAndRecordInvocations(
                GetTestFunction("testWithoutLinebreak_alpha"),
                ElmString(Input2));

        value2.Should().Be(Integer(52));

        PerformanceCountersFormatting.FormatCounts(report2).Should().Be(
            """
            InstructionCount: 1_853
            InvocationCount: 103
            BuildListCount: 53
            LoopIterationCount: 0
            """);
    }

    /// <summary>
    /// Expected sequence of <see cref="ApplicationLogEntry"/> records the syntax interpreter
    /// dispatches when evaluating <c>testWithoutLinebreak_alpha "abc"</c> against the
    /// post-pipeline lowered declarations, after filtering out
    /// <c>Pine_builtin</c> / <c>Pine_kernel</c> leaf invocations via
    /// <see cref="CompareInterpreterWithIntermediateVM.WithoutPineBuiltinInvocations"/>.
    /// One line per surviving application, in source-order.
    /// <para>
    /// This snapshot is intentionally exhaustive: any change in which user-level functions
    /// are invoked (e.g. successful specialization eliminating <c>Char.isAlpha</c> from the
    /// per-character loop, or the addition of a new wrapper) will surface here as a diff.
    /// The diff is the feedback loop guiding the inlining/specialization fix described in
    /// <see cref="SkipWhileWithoutLinebreakHelp_alpha_longer"/>.
    /// </para>
    /// </summary>
    private const string ExpectedInterpreterInvocationLog_testWithoutLinebreak_alpha_abc =
        """
        direct testWithoutLinebreak_alpha [ "abc" ]
        direct ParserFastTestModule.PState [ <pine_blob 12 bytes>, 0, 1, 1, 1 ]
        direct ParserFastTestModule.testWithoutLinebreak_alpha__lifted__lambda1 [  ]
        direct Char.isAlpha [  ]
        direct ParserFastTestModule.whileWithoutLinebreak__lifted__lambda1 [ <function> ]
        direct ParserFastTestModule.map__lifted__lambda1 [ (<function>, <function>) ]
        fnvalue ParserFastTestModule.map__lifted__lambda1 [ PState (<pine_blob 12 bytes>) 0 1 1 1 ]
        fnvalue ParserFastTestModule.whileWithoutLinebreak__lifted__lambda1 [ PState (<pine_blob 12 bytes>) 0 1 1 1 ]
        direct ParserFastTestModule.skipWhileWithoutLinebreakHelp [ <function>, 0, 1, 1, <pine_blob 12 bytes>, 1 ]
        fnvalue Char.isAlpha [ 'a' ]
        direct ParserFastTestModule.skipWhileWithoutLinebreakHelp [ <function>, 4, 1, 2, <pine_blob 12 bytes>, 1 ]
        fnvalue Char.isAlpha [ 'b' ]
        direct ParserFastTestModule.skipWhileWithoutLinebreakHelp [ <function>, 8, 1, 3, <pine_blob 12 bytes>, 1 ]
        fnvalue Char.isAlpha [ 'c' ]
        direct ParserFastTestModule.skipWhileWithoutLinebreakHelp [ <function>, 12, 1, 4, <pine_blob 12 bytes>, 1 ]
        direct ParserFastTestModule.PState [ <pine_blob 12 bytes>, 12, 1, 1, 4 ]
        direct ParserFastTestModule.String [ <pine_blob 12 bytes> ]
        direct ParserFastTestModule.Good [ "abc", PState (<pine_blob 12 bytes>) 12 1 1 4 ]
        fnvalue ParserFastTestModule.testWithoutLinebreak_alpha__lifted__lambda1 [ "abc" ]
        direct ParserFastTestModule.Good [ 3, PState (<pine_blob 12 bytes>) 12 1 1 4 ]
        direct Result.Ok [ 3 ]
        """;
}
