module SkipIdentifierStringSliceInliningTests exposing (suite)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "skipIdentifier inlining using String.slice"
        [ Test.describe "three identifier positions" threeIdentifierSuite
        , Test.describe "application expression" applicationExpressionSuite
        ]


threeIdentifierSuite : List Test
threeIdentifierSuite =
    [ { title = "identifiers with digits and underscores"
      , source = "ab a0 _b"
      , expected = ( "ab", "a0", "_b" )
      }
    , { title = "different identifier lengths"
      , source = "a0 b_ __"
      , expected = ( "a0", "b_", "__" )
      }
    , { title = "stops at a non-identifier character"
      , source = "b a9_0!"
      , expected = ( "b", "a", "_0" )
      }
    , { title = "rejects a digit at the first start"
      , source = "0a_b"
      , expected = ( "", "a_b", "" )
      }
    , { title = "rejects a digit at the second start"
      , source = "a 9 b"
      , expected = ( "a", "", "" )
      }
    , { title = "rejects a digit at the third start"
      , source = "a b 0"
      , expected = ( "a", "b", "" )
      }
    , { title = "empty source"
      , source = ""
      , expected = ( "", "", "" )
      }
    ]
        |> List.map
            (\testCase ->
                Test.test testCase.title <|
                    \_ ->
                        Expect.equal testCase.expected (parseThreeIdentifiers testCase.source)
            )


applicationExpressionSuite : List Test
applicationExpressionSuite =
    [ { title = "one identifier"
      , source = "a"
      , expected = [ "a" ]
      }
    , { title = "same-line arguments"
      , source = "a b0 _b"
      , expected = [ "a", "b0", "_b" ]
      }
    , { title = "multiline arguments with increasing indentation"
      , source = "a\n b\n  _0"
      , expected = [ "a", "b", "_0" ]
      }
    , { title = "later argument may be less indented than preceding argument"
      , source = " a\n   b\n  _0"
      , expected = [ "a", "b", "_0" ]
      }
    , { title = "equal indentation ends application"
      , source = "a\n  b\n\nb\n  a"
      , expected = [ "a", "b" ]
      }
    , { title = "dedent to first identifier after leading whitespace"
      , source = "  a\n    b\n  _0\n    a"
      , expected = [ "a", "b" ]
      }
    , { title = "dedent between arguments"
      , source = "  a\n     b\n _0"
      , expected = [ "a", "b" ]
      }
    , { title = "line and nested block comments are trivia"
      , source = "a -- note\n {- outer {- inner -} -} b\nc"
      , expected = [ "a", "b" ]
      }
    , { title = "CRLF and tabs advance the parser position"
      , source = "a\r\n\tb\r\nb"
      , expected = [ "a", "b" ]
      }
    , { title = "a tab at the first identifier column stops application"
      , source = " a\r\n\tb"
      , expected = [ "a" ]
      }
    , { title = "non-identifier ends application"
      , source = "a b!"
      , expected = [ "a", "b" ]
      }
    , { title = "invalid first identifier"
      , source = "  0a b"
      , expected = []
      }
    , { title = "empty source"
      , source = ""
      , expected = []
      }
    ]
        |> List.map
            (\testCase ->
                Test.test testCase.title <|
                    \_ ->
                        Expect.equal testCase.expected (parseApplicationExpression testCase.source)
            )


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
    ( String.slice 0 firstEnd source
    , String.slice secondStart secondEnd source
    , String.slice thirdStart thirdEnd source
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
            ( String.slice state.offset endOffset state.source
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
