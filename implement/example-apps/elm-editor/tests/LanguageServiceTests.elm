module LanguageServiceTests exposing (..)

import Bytes
import Bytes.Encode
import Expect
import FileTreeInWorkspace
import Frontend.MonacoEditor
import LanguageService
import Test


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

A variant of the union type `Event`

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

A variant of the union type `Event`

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
                      , documentation = ""
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

A variant of the union type `Event`

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

A variant of the union type `Event`

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
                      , documentation = ""
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
                      , documentation = ""
                      , insertText = "alpha"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    , { label = "beta"
                      , documentation = ""
                      , insertText = "beta"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    , { label = "delta"
                      , documentation = ""
                      , insertText = "delta"
                      , kind = Frontend.MonacoEditor.FunctionCompletionItemKind
                      }
                    ]

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
