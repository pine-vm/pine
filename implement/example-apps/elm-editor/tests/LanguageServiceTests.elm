module LanguageServiceTests exposing (..)

import Bytes
import Bytes.Encode
import Expect
import FileTreeInWorkspace
import Frontend.MonacoEditor
import LanguageService
import Test


provide_hover : Test.Test
provide_hover =
    let
        otherFiles =
            [ ( [ "src", "Alpha.elm" ]
              , """
module Alpha exposing (..)

{-| Documentation comment on module Alpha
-}

from_alpha = 123


"""
              )
            , ( [ "src", "Delta.elm" ]
              , """
module Delta exposing (..)

{-| Module comment
-}


{-| Comment on function
-}
from_delta : ( Int, String )
from_delta =
    ( 1, "" )

"""
              )
            ]

        expectationFromScenarioInMain mainModuleText expectedItems =
            hoverExpectationFromScenario
                otherFiles
                ( [ "src", "Main.elm" ], mainModuleText )
                expectedItems
    in
    Test.describe "Provide hover"
        [ Test.test "On local top-level function" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)


name = i👈🚁nit


init : State
init =
    0
"""
                    [ "    init : State"
                    ]
        , Test.test "On import syntax module name" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)

import A👈🚁lpha


init : State
init =
    0
"""
                    [ "Documentation comment on module Alpha"
                    ]
        , Test.test "On type annotation referencing local declaration" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)


name : S👈🚁tate
name = 4

{-| Comment on type alias declaration
-}
type alias State =
    Int

"""
                    [ stringTrimUpToLineBreaks """
    type alias State =
        Int


Comment on type alias declaration"""
                    ]
        , Test.test "On type alias declaration referencing local declaration" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)


type alias OtherRecord =
    { field_a : S👈🚁tate
    }


{-| Comment on type alias declaration
-}
type alias State =
    Int

"""
                    [ stringTrimUpToLineBreaks """
    type alias State =
        Int


Comment on type alias declaration"""
                    ]
        , Test.test "On imported module alias" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)

import Alpha exposing (from_alpha)
import Delta as ModuleAlias


name = ModuleAlia👈🚁s.from_delta


init : State
init =
    0
"""
                    [ "Module comment"
                    ]
        , Test.test "On declaration from imported module alias" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)

import Alpha exposing (from_alpha)
import Delta as ModuleAlias


name = ModuleAlias.from_d👈🚁elta


init : State
init =
    0
"""
                    [ """    from_delta : ( Int, String )

Comment on function"""
                    ]
        , Test.test "On function argument" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)


function : Int -> Int
function argumentName =
    argu👈🚁mentName + 1

"""
                    [ """    argumentName : Int"""
                    ]
        ]


hoverExpectationFromScenario :
    List ( List String, String )
    -> ( List String, String )
    -> List String
    -> Expect.Expectation
hoverExpectationFromScenario otherFiles ( fileOpenedInEditorPath, fileOpenedInEditorText ) expectedItems =
    case String.split "👈🚁" fileOpenedInEditorText of
        [ textUntilCursor, textAfterCursor ] ->
            hoverExpectationFromScenarioDescribingOpenFile
                otherFiles
                { filePath = fileOpenedInEditorPath
                , textUntilCursor = textUntilCursor
                , textAfterCursor = textAfterCursor
                }
                expectedItems

        splitElements ->
            Expect.fail ("Unexpected shape of fileOpenedInEditorText: Unexpected number of split symbols: " ++ String.fromInt (List.length splitElements - 1))


hoverExpectationFromScenarioDescribingOpenFile :
    List ( List String, String )
    -> { filePath : List String, textUntilCursor : String, textAfterCursor : String }
    -> List String
    -> Expect.Expectation
hoverExpectationFromScenarioDescribingOpenFile otherFiles fileOpenedInEditor expectedItems =
    let
        languageServiceState =
            buildLanguageServiceStateFindingParsableModuleText
                { maxLinesToRemoveBeforeCursor = 3 }
                otherFiles
                fileOpenedInEditor

        wholeText =
            fileOpenedInEditor.textUntilCursor ++ fileOpenedInEditor.textAfterCursor

        positionLineNumber =
            fileOpenedInEditor.textUntilCursor |> String.lines |> List.length

        lineText =
            wholeText
                |> String.lines
                |> List.drop (positionLineNumber - 1)
                |> List.head

        positionColumn =
            fileOpenedInEditor.textUntilCursor
                |> String.lines
                |> List.reverse
                |> List.head
                |> Maybe.map (String.length >> (+) 1)
                |> Maybe.withDefault 0
    in
    Expect.equal expectedItems
        (LanguageService.provideHover
            { filePathOpenedInEditor = fileOpenedInEditor.filePath
            , positionLineNumber = fileOpenedInEditor.textUntilCursor |> String.lines |> List.length
            , positionColumn = positionColumn
            , lineText = Maybe.withDefault "" lineText
            }
            languageServiceState
        )


provide_completion_items : Test.Test
provide_completion_items =
    let
        otherFiles =
            [ ( [ "src", "Alpha.elm" ]
              , """
module Alpha exposing (..)

{-| Documentation comment on module Alpha
-}

from_alpha = 123


"""
              )
            , ( [ "src", "Beta.elm" ]
              , """
module Beta exposing (from_beta, from_beta_function)


from_beta = "a string literal"


from_beta_function : Int -> String
from_beta_function arg =
  String.fromInt arg


from_beta_not_exposed : String
from_beta_not_exposed = "literal"

"""
              )
            , ( [ "src", "Beta", "Gamma.elm" ]
              , """
module Beta.Gamma exposing (..)


from_beta_gamma = 567


"""
              )
            , ( [ "src", "Delta.elm" ]
              , """
module Delta exposing (..)


from_delta = (1, 3)


"""
              )
            , ( [ "src", "Beta", "Epsilon.elm" ]
              , """
module Epsilon exposing (..)


from_epsilon = ""


"""
              )
            ]

        expectationFromScenarioInMain mainModuleText expectedItems =
            expectationFromScenario
                otherFiles
                ( [ "src", "Main.elm" ], mainModuleText )
                expectedItems
    in
    Test.describe "Provide completion items"
        [ Test.test "In top-level declaration after equals sign" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)

import Alpha exposing (from_alpha)
import Beta
import Beta.Gamma
import Delta as ModuleAlias


{-| Comment on declaration
-}
type alias State =
    Int

name = ✂➕

type Event
    = Increment
    | Decrement


init : State
init =
    0
"""
                    [ { label = "Alpha"
                      , documentation = "Documentation comment on module Alpha"
                      , insertText = "Alpha"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Beta"
                      , documentation = ""
                      , insertText = "Beta"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Beta.Gamma"
                      , documentation = ""
                      , insertText = "Beta.Gamma"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Delta as ModuleAlias"
                      , documentation = ""
                      , insertText = "ModuleAlias"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Decrement"
                      , documentation = stringTrimUpToLineBreaks """
    Decrement

A variant of the choice type `Event`

    type Event
        = Increment
        | Decrement
"""
                      , insertText = "Decrement"
                      , kind = Frontend.MonacoEditor.EnumMemberCompletionItemKind
                      }
                    , { label = "Event"
                      , documentation = stringTrimUpToLineBreaks """
    type Event
        = Increment
        | Decrement
"""
                      , insertText = "Event"
                      , kind = Frontend.MonacoEditor.EnumCompletionItemKind
                      }
                    , { label = "Increment"
                      , documentation = stringTrimUpToLineBreaks """
    Increment

A variant of the choice type `Event`

    type Event
        = Increment
        | Decrement
"""
                      , insertText = "Increment"
                      , kind = Frontend.MonacoEditor.EnumMemberCompletionItemKind
                      }
                    , { label = "State"
                      , documentation = stringTrimUpToLineBreaks """
    type alias State =
        Int

Comment on declaration
"""
                      , insertText = "State"
                      , kind = Frontend.MonacoEditor.StructCompletionItemKind
                      }
                    , { label = "from_alpha"
                      , documentation = "    from_alpha"
                      , insertText = "from_alpha"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    , { label = "init"
                      , documentation = stringTrimUpToLineBreaks """
    init : State
"""
                      , insertText = "init"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    ]
        , Test.test "In top-level declaration after equals sign and Module referece" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)

import Alpha exposing (from_alpha)
import Beta
import Beta.Gamma
import Delta as ModuleAlias


{-| Comment on declaration
-}
type alias State =
    Int


type Event
    = Increment
    | Decrement

name =Beta.✂➕

init : State
init =
    0
"""
                    [ { label = "Gamma"
                      , documentation = ""
                      , insertText = "Gamma"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "from_beta"
                      , documentation = stringTrimUpToLineBreaks """
    from_beta
"""
                      , insertText = "from_beta"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    , { label = "from_beta_function"
                      , documentation = stringTrimUpToLineBreaks """
    from_beta_function : Int -> String
"""
                      , insertText = "from_beta_function"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    ]
        , Test.test "In application expression after function" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (State)

import Alpha
import Beta


type alias State =
    Int

name = Alpha.from_alpha ✂➕

type Event
    = Increment
    | Decrement


init : State
init =
    0
"""
                    [ { label = "Alpha"
                      , documentation = "Documentation comment on module Alpha"
                      , insertText = "Alpha"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Beta"
                      , documentation = ""
                      , insertText = "Beta"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Decrement"
                      , documentation = stringTrimUpToLineBreaks """
    Decrement

A variant of the choice type `Event`

    type Event
        = Increment
        | Decrement
"""
                      , insertText = "Decrement"
                      , kind = Frontend.MonacoEditor.EnumMemberCompletionItemKind
                      }
                    , { label = "Event"
                      , documentation = stringTrimUpToLineBreaks """
    type Event
        = Increment
        | Decrement
"""
                      , insertText = "Event"
                      , kind = Frontend.MonacoEditor.EnumCompletionItemKind
                      }
                    , { label = "Increment"
                      , documentation = stringTrimUpToLineBreaks """
    Increment

A variant of the choice type `Event`

    type Event
        = Increment
        | Decrement
"""
                      , insertText = "Increment"
                      , kind = Frontend.MonacoEditor.EnumMemberCompletionItemKind
                      }
                    , { label = "State"
                      , documentation = stringTrimUpToLineBreaks """
    type alias State =
        Int
"""
                      , insertText = "State"
                      , kind = Frontend.MonacoEditor.StructCompletionItemKind
                      }
                    , { label = "init"
                      , documentation = stringTrimUpToLineBreaks """
    init : State
"""
                      , insertText = "init"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    , { label = "name"
                      , documentation = "    name"
                      , insertText = "name"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    ]
        , Test.test "Right of 'import '" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (..)

import ✂➕

local_decl = 123

"""
                    [ { label = "Alpha"
                      , documentation = "Documentation comment on module Alpha"
                      , insertText = "Alpha"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Beta"
                      , documentation = ""
                      , insertText = "Beta"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Beta.Gamma"
                      , documentation = ""
                      , insertText = "Beta.Gamma"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Delta"
                      , documentation = ""
                      , insertText = "Delta"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Epsilon"
                      , documentation = ""
                      , insertText = "Epsilon"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    ]
        , Test.test "Right of 'import E'" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (..)

import E✂➕

local_decl = 123

"""
                    [ { label = "Alpha"
                      , documentation = "Documentation comment on module Alpha"
                      , insertText = "Alpha"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Beta"
                      , documentation = ""
                      , insertText = "Beta"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Beta.Gamma"
                      , documentation = ""
                      , insertText = "Beta.Gamma"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Delta"
                      , documentation = ""
                      , insertText = "Delta"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Epsilon"
                      , documentation = ""
                      , insertText = "Epsilon"
                      , kind = Frontend.MonacoEditor.ModuleCompletionItemKind
                      }
                    ]
        , Test.test "In declaration in let-block after equals sign" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (..)


alpha =
  let
    epsilon =✂➕

    delta = 678
  in
  delta


beta =
  let
    gamma = ""
  in
  gamma

"""
                    [ { label = "alpha"
                      , documentation = "    alpha"
                      , insertText = "alpha"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    , { label = "beta"
                      , documentation = "    beta"
                      , insertText = "beta"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    , { label = "delta"
                      , documentation = "    delta"
                      , insertText = "delta"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    ]
        , Test.test "None in multi-line comment before module declaration" <|
            \_ ->
                expectationFromScenarioInMain
                    """
{- First line in comment
Second line ✂➕
-}

module Main exposing (..)

import Alpha exposing (..)
import Beta


beta : Int
beta = 123

"""
                    []
        , Test.test "None in middle of multi-line documentation comment on a function declaration" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (..)

import Alpha exposing (..)
import Beta


{-| First line in comment
Second line ✂➕
-}
beta : Int
beta = 123

"""
                    []
        , Test.test "None at the beginning of multi-line comment" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (..)

import Alpha exposing (..)
import Beta


{-✂➕ First line in comment
Second line
-}


beta : Int
beta = 123

"""
                    []
        , Test.test "None at the end of multi-line comment" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (..)

import Alpha exposing (..)
import Beta


{- First line in comment
Second line
 ✂➕-}


beta : Int
beta = 123

"""
                    []
        , Test.test "None in single-line comment in a function declaration" <|
            \_ ->
                expectationFromScenarioInMain
                    """
module Main exposing (..)

import Alpha exposing (..)
import Beta


beta : Int
beta =
  -- Beginning of the comment ✂➕
  123

"""
                    []

        {- TODO: Add test for completion items out of core modules like List, Maybe, Result, etc. -}
        ]


expectationFromScenario :
    List ( List String, String )
    -> ( List String, String )
    -> List Frontend.MonacoEditor.MonacoCompletionItem
    -> Expect.Expectation
expectationFromScenario otherFiles ( fileOpenedInEditorPath, fileOpenedInEditorText ) expectedItems =
    case String.split "✂➕" fileOpenedInEditorText of
        [ textUntilCursor, textAfterCursor ] ->
            expectationFromScenarioDescribingOpenFile
                otherFiles
                { filePath = fileOpenedInEditorPath, textUntilCursor = textUntilCursor, textAfterCursor = textAfterCursor }
                expectedItems

        splitElements ->
            Expect.fail ("Unexpected shape of fileOpenedInEditorText: Unexpected number of split symbols: " ++ String.fromInt (List.length splitElements - 1))


expectationFromScenarioDescribingOpenFile :
    List ( List String, String )
    -> { filePath : List String, textUntilCursor : String, textAfterCursor : String }
    -> List Frontend.MonacoEditor.MonacoCompletionItem
    -> Expect.Expectation
expectationFromScenarioDescribingOpenFile otherFiles fileOpenedInEditor expectedItems =
    let
        languageServiceState =
            buildLanguageServiceStateFindingParsableModuleText
                { maxLinesToRemoveBeforeCursor = 3 }
                otherFiles
                fileOpenedInEditor
    in
    Expect.equal expectedItems
        (LanguageService.provideCompletionItems
            { filePathOpenedInEditor = fileOpenedInEditor.filePath
            , textUntilPosition = fileOpenedInEditor.textUntilCursor
            , cursorLineNumber = fileOpenedInEditor.textUntilCursor |> String.lines |> List.length
            }
            languageServiceState
        )


buildLanguageServiceStateFindingParsableModuleText :
    { maxLinesToRemoveBeforeCursor : Int }
    -> List ( List String, String )
    -> { filePath : List String, textUntilCursor : String, textAfterCursor : String }
    -> LanguageService.LanguageServiceState
buildLanguageServiceStateFindingParsableModuleText { maxLinesToRemoveBeforeCursor } otherFiles fileOpenedInEditor =
    let
        textUntilCursorLines =
            String.lines fileOpenedInEditor.textUntilCursor

        fileTreeWithPreviousLinesRemoved linesToRemove =
            let
                textUntilCursor =
                    textUntilCursorLines |> List.reverse |> List.drop linesToRemove |> List.reverse |> String.join "\n"

                fileOpenedInEditorText =
                    textUntilCursor ++ fileOpenedInEditor.textAfterCursor
            in
            ( fileOpenedInEditor.filePath, fileOpenedInEditorText )
                :: otherFiles
                |> List.map (Tuple.mapSecond fileContentFromString)
                |> FileTreeInWorkspace.sortedFileTreeFromListOfBlobsAsBytes
    in
    List.range 0 maxLinesToRemoveBeforeCursor
        |> List.foldr
            (fileTreeWithPreviousLinesRemoved >> LanguageService.updateLanguageServiceState)
            LanguageService.initLanguageServiceState


fileContentFromString : String -> Bytes.Bytes
fileContentFromString =
    Bytes.Encode.string >> Bytes.Encode.encode


stringTrimUpToLineBreaks : String -> String
stringTrimUpToLineBreaks string =
    case String.lines string of
        [] ->
            string

        firstLine :: afterFirstLine ->
            let
                right =
                    case List.reverse afterFirstLine of
                        lastLine :: middleLinesReversed ->
                            String.join "\n" (List.reverse middleLinesReversed) ++ String.trimRight lastLine

                        [] ->
                            ""
            in
            String.trimLeft firstLine ++ right
