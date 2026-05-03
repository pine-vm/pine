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
            InvocationCount: 0
            BuildListCount: 0
            LoopIterationCount: 0
            InstructionCount: 2
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
            InvocationCount: 0
            BuildListCount: 0
            LoopIterationCount: 0
            InstructionCount: 2
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
            InvocationCount: 0
            BuildListCount: 0
            LoopIterationCount: 0
            InstructionCount: 2
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
            InvocationCount: 15
            BuildListCount: 18
            LoopIterationCount: 0
            InstructionCount: 544
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
            InvocationCount: 15
            BuildListCount: 18
            LoopIterationCount: 0
            InstructionCount: 439
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
            InvocationCount: 7
            BuildListCount: 14
            LoopIterationCount: 0
            InstructionCount: 250
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

        // -------- Snapshot the post-optimization declarations relevant to the
        //         skipWhileWithoutLinebreakHelp call chain. --------
        //
        // The investigation goal: verify *why* the recursive helper is not specialized
        // enough to embed `Char.isAlpha` as a local literal (which would be a prerequisite
        // for the IR inliner to fold the predicate into the helper frame, per the
        // positive baseline in InlineSmallNonRecursiveCalleeRegressionTests).
        //
        // We render three declarations from the post-optimization module list (the same
        // input the bytecode emitter consumes) — the one site that has `Char.isAlpha` as
        // a literal, plus the two declarations along the call chain that *should* have
        // been specialized for `isGood = Char.isAlpha` but were not:
        //
        //   1. testWithoutLinebreak_alpha — has `Char.isAlpha` as a literal at the
        //      `whileWithoutLinebreak__lifted__lambda1 Char.isAlpha` call site.
        //   2. whileWithoutLinebreak__lifted__lambda1 — receives `isGood` as a parameter
        //      and forwards it to `skipWhileWithoutLinebreakHelp`. A successful
        //      wrapper-with-captured-lambda specialization would materialize a
        //      `whileWithoutLinebreak__lifted__lambda1__specialized__1` with `isGood`
        //      substituted by `Char.isAlpha`, and `testWithoutLinebreak_alpha` would
        //      reference that specialized wrapper instead.
        //   3. skipWhileWithoutLinebreakHelp — the recursive helper; the per-iteration
        //      `isGood nextChar` call would become `Char.isAlpha nextChar` after
        //      specialization, and the recursive self-call would no longer thread the
        //      `isGood` parameter.
        //
        // None of those specializations have happened: the snapshot below shows
        // `Char.isAlpha` appears at exactly one site (testWithoutLinebreak_alpha), and
        // the recursive helper continues to receive `isGood` as a parameter and pass it
        // along on every recursive call. This is the "specs collected via virtual
        // substitution through wrappers are NOT realized unless the rewriter also follows
        // the wrapper chain" pattern — see Inlining.cs's
        // TrySpecializeWrapperWithCapturedFunctionPartialApplication path.
        {
            var module =
                s_compareFramework.Value.PostOptimizationModules
                    .Single(
                        f =>
                        Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Module
                            .GetModuleName(f.ModuleDefinition.Value).Value
                            is ["ParserFastTestModule"]);

            var fullModuleFile =
                Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel.Convert(module);

            string RenderDeclByName(string name)
            {
                var declNode =
                    fullModuleFile.Declarations
                        .Single(
                            d =>
                            d.Value is Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration.FunctionDeclaration funcDecl &&
                            funcDecl.Function.Declaration.Value.Name.Value == name);

                return Pine.Core.Elm.ElmSyntax.SnapshotTestFormat.RenderQualifiedDeclaration(
                    new DeclQualifiedName(["ParserFastTestModule"], name),
                    declNode.Value);
            }

            // After the wrapper-with-captured-function specialization fix:
            // testWithoutLinebreak_alpha applies `whileWithoutLinebreak__lifted__lambda1 Char.isAlpha`
            // (a partial application supplying a known function for `isGood`). The optimization
            // pipeline now inlines the wrapper at that call site, exposing a recursive call
            // `skipWhileWithoutLinebreakHelp Char.isAlpha ...` that the existing recursive-spec
            // machinery turns into a first-order `skipWhileWithoutLinebreakHelp__specialized__1`
            // with `Char.isAlpha` embedded as a local literal call. The remaining lambda from
            // the inlined wrapper body is lifted to a new `testWithoutLinebreak_alpha__lifted__lambda2`.
            //
            // This is the "Char.isAlpha appears locally in the helper" outcome we wanted.
            var allFunctionDeclNames =
                fullModuleFile.Declarations
                    .OfType<Pine.Core.Elm.ElmSyntax.SyntaxModel.Node<
                        Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration>>()
                    .Select(d => d.Value)
                    .OfType<Pine.Core.Elm.ElmSyntax.SyntaxModel.Declaration.FunctionDeclaration>()
                    .Select(d => d.Function.Declaration.Value.Name.Value)
                    .ToList();

            allFunctionDeclNames
                .Should().Contain(
                    "skipWhileWithoutLinebreakHelp__specialized__1",
                    "the wrapper-with-captured-function specialization must materialize a "
                    + "first-order specialized helper with Char.isAlpha embedded locally.");

            var renderedTestEntry = RenderDeclByName("testWithoutLinebreak_alpha");
            var renderedHelperSpecialized = RenderDeclByName("skipWhileWithoutLinebreakHelp__specialized__1");
            var renderedNewLifted = RenderDeclByName("testWithoutLinebreak_alpha__lifted__lambda2");

            renderedTestEntry.Should().Be(
                ExpectedPostOpt_testWithoutLinebreak_alpha);

            renderedHelperSpecialized.Should().Be(
                ExpectedPostOpt_skipWhileWithoutLinebreakHelp__specialized__1);

            renderedNewLifted.Should().Be(
                ExpectedPostOpt_testWithoutLinebreak_alpha__lifted__lambda2);
        }

        var (value1, report1, invocations1) =
            ApplyAndRecordInvocations(
                GetTestFunction("testWithoutLinebreak_alpha"),
                ElmString(Input1));

        value1.Should().Be(Integer(26));

        var (value2, report2, invocations2) =
            ApplyAndRecordInvocations(
                GetTestFunction("testWithoutLinebreak_alpha"),
                ElmString(Input2));

        value2.Should().Be(Integer(52));

        PerformanceCountersFormatting.FormatCounts(report1).Should().Be(
            """
            InvocationCount: 78
            BuildListCount: 81
            LoopIterationCount: 0
            InstructionCount: 2_497
            """);

        PerformanceCountersFormatting.FormatCounts(report2).Should().Be(
            """
            InvocationCount: 156
            BuildListCount: 159
            LoopIterationCount: 0
            InstructionCount: 4_915
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
    /// After the wrapper-with-captured-function specialization fix, every per-character
    /// dispatch goes through <c>skipWhileWithoutLinebreakHelp__specialized__1</c> (no
    /// <c>isGood</c> parameter) and <c>Char.isAlpha</c> is invoked as a known direct
    /// reference rather than via a function-value slot.
    /// </para>
    /// </summary>
    private const string ExpectedInterpreterInvocationLog_testWithoutLinebreak_alpha_abc =
        """
        direct testWithoutLinebreak_alpha [ "abc" ]
        direct ParserFastTestModule.PState [ <pine_blob 12 bytes>, 0, 1, 1, 1 ]
        direct ParserFastTestModule.testWithoutLinebreak_alpha__lifted__lambda1 [  ]
        direct ParserFastTestModule.testWithoutLinebreak_alpha__lifted__lambda2 [  ]
        direct ParserFastTestModule.map__lifted__lambda1 [ (<function>, <function>) ]
        fnvalue ParserFastTestModule.map__lifted__lambda1 [ PState (<pine_blob 12 bytes>) 0 1 1 1 ]
        fnvalue ParserFastTestModule.testWithoutLinebreak_alpha__lifted__lambda2 [ PState (<pine_blob 12 bytes>) 0 1 1 1 ]
        direct ParserFastTestModule.skipWhileWithoutLinebreakHelp__specialized__1 [ 0, 1, 1, <pine_blob 12 bytes>, 1 ]
        direct ParserFastTestModule.skipWhileWithoutLinebreakHelp__specialized__1 [ 4, 1, 2, <pine_blob 12 bytes>, 1 ]
        direct ParserFastTestModule.skipWhileWithoutLinebreakHelp__specialized__1 [ 8, 1, 3, <pine_blob 12 bytes>, 1 ]
        direct ParserFastTestModule.skipWhileWithoutLinebreakHelp__specialized__1 [ 12, 1, 4, <pine_blob 12 bytes>, 1 ]
        direct ParserFastTestModule.PState [ <pine_blob 12 bytes>, 12, 1, 1, 4 ]
        direct ParserFastTestModule.String [ <pine_blob 12 bytes> ]
        direct ParserFastTestModule.Good [ "abc", PState (<pine_blob 12 bytes>) 12 1 1 4 ]
        fnvalue ParserFastTestModule.testWithoutLinebreak_alpha__lifted__lambda1 [ "abc" ]
        direct ParserFastTestModule.Good [ 3, PState (<pine_blob 12 bytes>) 12 1 1 4 ]
        direct Result.Ok [ 3 ]
        """;

    /// <summary>
    /// Post-optimization rendering of <c>testWithoutLinebreak_alpha</c>. The original call
    /// site <c>(whileWithoutLinebreak__lifted__lambda1 Char.isAlpha)</c> has been replaced
    /// by a reference to the new lifted lambda
    /// <c>testWithoutLinebreak_alpha__lifted__lambda2</c>, which the wrapper-with-captured-function
    /// specialization materialized when it inlined the wrapper at this site (and the post-pass
    /// lambda lifting then lifted the resulting closure).
    /// </summary>
    private const string ExpectedPostOpt_testWithoutLinebreak_alpha =
        """
        {-| Parse input matching alpha chars via whileWithoutLinebreak.
        Returns the byte-length of the matched slice divided by 4
        (i.e. the character count). Exercises skipWhileWithoutLinebreakHelp.
        -}
        ParserFastTestModule.testWithoutLinebreak_alpha : ParserFastTestModule.String -> Basics.Int
        ParserFastTestModule.testWithoutLinebreak_alpha input =
            case
                let
                    (ParserFastTestModule.String srcBytes) =
                        input
                in
                case (ParserFastTestModule.map__lifted__lambda1 ( ParserFastTestModule.testWithoutLinebreak_alpha__lifted__lambda1, ParserFastTestModule.testWithoutLinebreak_alpha__lifted__lambda2 )) (ParserFastTestModule.PState srcBytes 0 1 1 1) of
                    ParserFastTestModule.Good value (ParserFastTestModule.PState finalSrc finalOffset _ finalRow finalCol) ->
                        if Pine_kernel.equal [ finalOffset, Pine_kernel.length srcBytes ] then
                            Result.Ok value

                        else
                            Result.Err [ { row = finalRow, col = finalCol } ]

                    ParserFastTestModule.Bad _ deadEnds ->
                        Result.Err []
            of
                Result.Ok v ->
                    v

                Result.Err _ ->
                    -1
        """;

    /// <summary>
    /// Post-optimization rendering of the new lifted lambda introduced by the
    /// wrapper-with-captured-function specialization. The body is the original
    /// <c>whileWithoutLinebreak__lifted__lambda1</c> body with <c>isGood</c>
    /// substituted away — there is no <c>isGood</c> parameter, and the recursive call
    /// goes directly to the first-order
    /// <c>skipWhileWithoutLinebreakHelp__specialized__1</c>.
    /// </summary>
    private const string ExpectedPostOpt_testWithoutLinebreak_alpha__lifted__lambda2 =
        """
        ParserFastTestModule.testWithoutLinebreak_alpha__lifted__lambda2 (ParserFastTestModule.PState s0SrcBytes s0Offset s0Indent s0Row s0Col) =
            let
                s0OffsetInt : Basics.Int
                s0OffsetInt =
                    s0Offset

                s1 : ParserFastTestModule.State
                s1 =
                    ParserFastTestModule.skipWhileWithoutLinebreakHelp__specialized__1
                        s0Offset
                        s0Row
                        s0Col
                        s0SrcBytes
                        s0Indent

                (ParserFastTestModule.PState _ s1Offset _ _ _) =
                    s1

                s1OffsetInt : Basics.Int
                s1OffsetInt =
                    s1Offset

                sliceBytesLength : Basics.Int
                sliceBytesLength =
                    Pine_kernel.int_add
                        [ s1OffsetInt
                        , Pine_kernel.int_mul
                            [ -1, s0OffsetInt ]
                        ]

                sliceBytes : Basics.Int
                sliceBytes =
                    Pine_kernel.take
                        [ sliceBytesLength
                        , Pine_kernel.skip
                            [ s0OffsetInt, s0SrcBytes ]
                        ]
            in
            ParserFastTestModule.Good
                (ParserFastTestModule.String
                    sliceBytes
                )
                s1
        """;

    /// <summary>
    /// Post-optimization rendering of the recursive helper specialized for
    /// <c>isGood = Char.isAlpha</c>. With the size-based inliner threshold raised to 24
    /// (the size of <c>Char.isAlpha</c>'s body), the predicate is now inlined directly
    /// into the helper as a <c>let code = ... in if int_is_sorted_asc ...</c> form,
    /// eliminating the per-iteration <c>Char.isAlpha</c> dispatch.
    /// </summary>
    private const string ExpectedPostOpt_skipWhileWithoutLinebreakHelp__specialized__1 =
        """
        ParserFastTestModule.skipWhileWithoutLinebreakHelp__specialized__1 offset row col srcBytes indent =
            let
                nextChar : Basics.Int
                nextChar =
                    Pine_kernel.take
                        [ 4
                        , Pine_kernel.skip
                            [ offset, srcBytes ]
                        ]
            in
            if
                Pine_kernel.equal
                    [ Pine_kernel.length
                        nextChar
                    , 0
                    ]
            then
                ParserFastTestModule.PState
                    srcBytes
                    offset
                    indent
                    row
                    col

            else if let
                        code : Basics.Int
                        code =
                            Pine_kernel.concat
                                [ Pine_kernel.take
                                    [ 1, 0 ]
                                , nextChar
                                ]
                    in
                    if
                        Pine_kernel.int_is_sorted_asc
                            [ 0x41, code, 0x5A ]
                    then
                        Basics.True

                    else
                        Pine_kernel.int_is_sorted_asc
                            [ 0x61, code, 0x7A ] then
                ParserFastTestModule.skipWhileWithoutLinebreakHelp__specialized__1
                    (Pine_builtin.int_add
                        [ offset, 4 ]
                    )
                    row
                    (Pine_builtin.int_add
                        [ col, 1 ]
                    )
                    srcBytes
                    indent

            else
                ParserFastTestModule.PState
                    srcBytes
                    offset
                    indent
                    row
                    col
        """;
}
