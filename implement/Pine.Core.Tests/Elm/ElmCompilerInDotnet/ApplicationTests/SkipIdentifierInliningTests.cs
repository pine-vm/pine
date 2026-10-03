using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

public class SkipIdentifierInliningTests
{
    private const string TestModuleText =
        """
        module SkipIdentifierInliningTestModuleNoSlice exposing (..)


        parseThreeIdentifiers : String -> ( String, String, String )
        parseThreeIdentifiers source =
            let
                firstEnd =
                    skipIdentifier source 0

                secondStart =
                    firstEnd + 1

                secondEnd =
                    skipIdentifier source secondStart

                thirdStart =
                    secondEnd + 1

                thirdEnd =
                    skipIdentifier source thirdStart
            in
            ( String.left firstEnd source
            , String.left (secondEnd - secondStart) (String.dropLeft secondStart source)
            , String.left (thirdEnd - thirdStart) (String.dropLeft thirdStart source)
            )


        type alias ParserState =
            { source : String
            , offset : Int
            , row : Int
            , column : Int
            }


        parseApplicationExpression : String -> List String
        parseApplicationExpression source =
            let
                stateAtFirst =
                    skipApplicationTrivia
                        { source = source, offset = 0, row = 1, column = 1 }
            in
            case parseIdentifier stateAtFirst of
                Just ( first, afterFirst ) ->
                    parseApplicationArguments stateAtFirst.column [ first ] afterFirst

                Nothing ->
                    []


        parseApplicationArguments : Int -> List String -> ParserState -> List String
        parseApplicationArguments firstColumn identifiersRev state =
            let
                stateAtArgument =
                    skipApplicationTrivia state
            in
            if stateAtArgument.column > firstColumn then
                case parseIdentifier stateAtArgument of
                    Just ( identifier, afterIdentifier ) ->
                        parseApplicationArguments firstColumn (identifier :: identifiersRev) afterIdentifier

                    Nothing ->
                        List.reverse identifiersRev

            else
                List.reverse identifiersRev


        parseIdentifier : ParserState -> Maybe ( String, ParserState )
        parseIdentifier state =
            let
                endOffset =
                    skipIdentifier state.source state.offset
            in
            if endOffset == state.offset then
                Nothing

            else
                Just
                    ( String.left (endOffset - state.offset) (String.dropLeft state.offset state.source)
                    , { state
                        | offset = endOffset
                        , column = state.column + endOffset - state.offset
                      }
                    )


        skipApplicationTrivia : ParserState -> ParserState
        skipApplicationTrivia state =
            let
                remaining =
                    String.dropLeft state.offset state.source
            in
            case String.left 2 remaining of
                "\r\n" ->
                    skipApplicationTrivia (advanceApplicationPosition state)

                "--" ->
                    skipApplicationTrivia
                        (skipLineComment (advanceApplicationPosition (advanceApplicationPosition state)))

                "{-" ->
                    skipApplicationTrivia
                        (skipBlockComment 1 (advanceApplicationPosition (advanceApplicationPosition state)))

                _ ->
                    case String.left 1 remaining of
                        " " ->
                            skipApplicationTrivia (advanceApplicationPosition state)

                        "\t" ->
                            skipApplicationTrivia (advanceApplicationPosition state)

                        "\n" ->
                            skipApplicationTrivia (advanceApplicationPosition state)

                        "\r" ->
                            skipApplicationTrivia (advanceApplicationPosition state)

                        _ ->
                            state


        skipLineComment : ParserState -> ParserState
        skipLineComment state =
            case String.left 1 (String.dropLeft state.offset state.source) of
                "" ->
                    state

                "\n" ->
                    state

                "\r" ->
                    state

                _ ->
                    skipLineComment (advanceApplicationPosition state)


        skipBlockComment : Int -> ParserState -> ParserState
        skipBlockComment depth state =
            let
                remaining =
                    String.dropLeft state.offset state.source
            in
            case String.left 2 remaining of
                "{-" ->
                    skipBlockComment (depth + 1) (advanceApplicationPosition (advanceApplicationPosition state))

                "-}" ->
                    let
                        afterClose =
                            advanceApplicationPosition (advanceApplicationPosition state)
                    in
                    if depth == 1 then
                        afterClose

                    else
                        skipBlockComment (depth - 1) afterClose

                _ ->
                    if String.isEmpty remaining then
                        state

                    else
                        skipBlockComment depth (advanceApplicationPosition state)


        advanceApplicationPosition : ParserState -> ParserState
        advanceApplicationPosition state =
            let
                remaining =
                    String.dropLeft state.offset state.source
            in
            if String.left 2 remaining == "\r\n" then
                { state | offset = state.offset + 2, row = state.row + 1, column = 1 }

            else
                case String.left 1 remaining of
                    "\n" ->
                        { state | offset = state.offset + 1, row = state.row + 1, column = 1 }

                    "\r" ->
                        { state | offset = state.offset + 1, row = state.row + 1, column = 1 }

                    _ ->
                        { state | offset = state.offset + 1, column = state.column + 1 }


        skipIdentifier : String -> Int -> Int
        skipIdentifier source offset =
            if isIdentifierStart (String.left 1 (String.dropLeft offset source)) then
                skipToIdentifierEnd source (offset + 1)

            else
                offset


        skipToIdentifierEnd : String -> Int -> Int
        skipToIdentifierEnd source offset =
            if isIdentifierChar (String.left 1 (String.dropLeft offset source)) then
                skipToIdentifierEnd source (offset + 1)

            else
                offset


        isIdentifierStart : String -> Bool
        isIdentifierStart character =
            case character of
                "_" ->
                    True

                "a" ->
                    True

                "b" ->
                    True

                _ ->
                    False


        isIdentifierChar : String -> Bool
        isIdentifierChar character =
            case character of
                "_" ->
                    True

                "0" ->
                    True

                "a" ->
                    True

                "b" ->
                    True

                _ ->
                    False
        """;

    [Fact]
    public void ParseThreeIdentifiers_without_string_slice_values_and_all_stack_frame_instructions()
    {
        var function = CompileFunction("parseThreeIdentifiers");
        var vm = ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        var cases =
            new (string source, string first, string second, string third)[]
            {
                ("ab a0 _b", "ab", "a0", "_b"),
                ("a0 b_ __", "a0", "b_", "__"),
                ("b a9_0!", "b", "a", "_0"),
                ("0a_b", "", "a_b", ""),
                ("a 9 b", "a", "", ""),
                ("a b 0", "a", "b", ""),
                ("", "", "", "")
            };

        foreach (var (source, first, second, third) in cases)
        {
            var frames = new List<EnteredStackFrame>();

            var (value, _) =
                CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                    function,
                    ElmValue.StringInstance(source),
                    vm,
                    reportEnteredStackFrame: (in EnteredStackFrame frame) => frames.Add(frame));

            value.Should().Be(
                ElmValue.ListInstance(
                    [ElmValue.StringInstance(first), ElmValue.StringInstance(second), ElmValue.StringInstance(third)]),
                "source: {0}",
                source);

            if (source is "ab a0 _b")
                AssertFrameSnapshot(frames, nameof(ParseThreeIdentifiers_without_string_slice_values_and_all_stack_frame_instructions));
        }
    }

    [Fact]
    public void ParseApplicationExpression_without_string_slice_values_and_all_stack_frame_instructions()
    {
        var function = CompileFunction("parseApplicationExpression");
        var vm = ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        var cases =
            new (string source, string[] identifiers)[]
            {
                ("a", ["a"]),
                ("a b0 _b", ["a", "b0", "_b"]),
                ("a\n b\n  _0", ["a", "b", "_0"]),
                (" a\n   b\n  _0", ["a", "b", "_0"]),
                ("a\n  b\n\nb\n  a", ["a", "b"]),
                ("  a\n    b\n  _0\n    a", ["a", "b"]),
                ("  a\n     b\n _0", ["a", "b"]),
                ("a -- note\n {- outer {- inner -} -} b\nc", ["a", "b"]),
                ("a\r\n\tb\r\nb", ["a", "b"]),
                (" a\r\n\tb", ["a"]),
                ("a b!", ["a", "b"]),
                ("  0a b", []),
                ("", [])
            };

        foreach (var (source, identifiers) in cases)
        {
            var frames = new List<EnteredStackFrame>();

            var (value, _) =
                CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                    function,
                    ElmValue.StringInstance(source),
                    vm,
                    reportEnteredStackFrame: (in EnteredStackFrame frame) => frames.Add(frame));

            value.Should().Be(
                ElmValue.ListInstance([.. identifiers.Select(ElmValue.StringInstance)]),
                "source: {0}",
                source);

            if (source is "a\n b\n  _0")
                AssertFrameSnapshot(frames, nameof(ParseApplicationExpression_without_string_slice_values_and_all_stack_frame_instructions));
        }
    }

    private static PineValue CompileFunction(string name)
    {
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules([TestModuleText], disableInlining: false).parsedEnv;

        return
            parsedEnv.Modules
            .First(module => module.moduleName is "SkipIdentifierInliningTestModuleNoSlice")
            .moduleContent.FunctionDeclarations[name];
    }

    private static void AssertFrameSnapshot(IReadOnlyList<EnteredStackFrame> frames, string name)
    {
        frames.Should().NotBeEmpty();

        var bodies = new List<string>();
        var frameLines = new List<string>();

        for (var index = 0; index < frames.Count; index++)
        {
            var frame = frames[index];
            var rendered = StackInstructionTraceRenderer.RenderStackFrameInstructions(frame.Instructions);
            var bodyIndex = bodies.IndexOf(rendered);

            if (bodyIndex < 0)
            {
                bodyIndex = bodies.Count;
                bodies.Add(rendered);
            }

            frameLines.Add($"{index}: depth={frame.StackFrameDepth} body={bodyIndex}");
        }

        var snapshot =
            "Frames:\n" + string.Join('\n', frameLines) +
            "\n\nInstruction bodies:\n" +
            string.Join("\n\n", bodies.Select((body, index) => $"Body {index}:\n{body}"));

        snapshot.Should().Be(SnapshotRecorder.ReadEmbeddedTrace(name));
    }
}
