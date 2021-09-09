module LanguageServiceTests exposing (..)

import Bytes
import Bytes.Encode
import Expect
import FileTreeInWorkspace
import FrontendWeb.MonacoEditor
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

        fileOpenedInEditor =
            ( [ "src", "Main.elm" ]
            , """
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


init : State
init =
    0
"""
            )
    in
    Test.describe "Provide completion items"
        [ Test.test "Start of line" <|
            \_ ->
                expectationFromScenario
                    otherFiles
                    fileOpenedInEditor
                    { textUntilPosition = "previousline\n" }
                    [ { label = "Alpha"
                      , documentation = "Documentation comment on module Alpha"
                      , insertText = "Alpha"
                      , kind = FrontendWeb.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Beta"
                      , documentation = ""
                      , insertText = "Beta"
                      , kind = FrontendWeb.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Beta.Gamma"
                      , documentation = ""
                      , insertText = "Beta.Gamma"
                      , kind = FrontendWeb.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Delta as ModuleAlias"
                      , documentation = ""
                      , insertText = "ModuleAlias"
                      , kind = FrontendWeb.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "Decrement"
                      , documentation = String.trim """
`Decrement` is a variant of `Event`

```Elm
type Event
    = Increment
    | Decrement
```
"""
                      , insertText = "Decrement"
                      , kind = FrontendWeb.MonacoEditor.EnumMemberCompletionItemKind
                      }
                    , { label = "Event"
                      , documentation = String.trim """
```Elm
type Event
    = Increment
    | Decrement
```
"""
                      , insertText = "Event"
                      , kind = FrontendWeb.MonacoEditor.EnumCompletionItemKind
                      }
                    , { label = "Increment"
                      , documentation = String.trim """
`Increment` is a variant of `Event`

```Elm
type Event
    = Increment
    | Decrement
```
"""
                      , insertText = "Increment"
                      , kind = FrontendWeb.MonacoEditor.EnumMemberCompletionItemKind
                      }
                    , { label = "State"
                      , documentation = String.trim """
```Elm
type alias State =
    Int
```

Comment on declaration
"""
                      , insertText = "State"
                      , kind = FrontendWeb.MonacoEditor.StructCompletionItemKind
                      }
                    , { label = "from_alpha"
                      , documentation = """```Elm
```"""
                      , insertText = "from_alpha"
                      , kind = FrontendWeb.MonacoEditor.FunctionCompletionItemKind
                      }
                    , { label = "init"
                      , documentation = String.trim """
```Elm
init : State
```
"""
                      , insertText = "init"
                      , kind = FrontendWeb.MonacoEditor.FunctionCompletionItemKind
                      }
                    ]
        , Test.test "Right of 'Beta.'" <|
            \_ ->
                expectationFromScenario
                    otherFiles
                    fileOpenedInEditor
                    { textUntilPosition = "previousline\nBeta." }
                    [ { label = "Gamma"
                      , documentation = ""
                      , insertText = "Gamma"
                      , kind = FrontendWeb.MonacoEditor.ModuleCompletionItemKind
                      }
                    , { label = "from_beta"
                      , documentation = String.trim """
```Elm
```
"""
                      , insertText = "from_beta"
                      , kind = FrontendWeb.MonacoEditor.FunctionCompletionItemKind
                      }
                    , { label = "from_beta_function"
                      , documentation = String.trim """
```Elm
from_beta_function : Int -> String
```
"""
                      , insertText = "from_beta_function"
                      , kind = FrontendWeb.MonacoEditor.FunctionCompletionItemKind
                      }
                    ]
        , Test.test "Right of 'import '" <|
            \_ ->
                expectationFromScenario
                    otherFiles
                    fileOpenedInEditor
                    { textUntilPosition = "previousline\nimport " }
                    [ { label = "Epsilon"
                      , documentation = ""
                      , insertText = "Epsilon"
                      , kind = FrontendWeb.MonacoEditor.ModuleCompletionItemKind
                      }
                    ]
        , Test.test "Right of 'import E'" <|
            \_ ->
                expectationFromScenario
                    otherFiles
                    fileOpenedInEditor
                    { textUntilPosition = "previousline\nimport E" }
                    [ { label = "Epsilon"
                      , documentation = ""
                      , insertText = "Epsilon"
                      , kind = FrontendWeb.MonacoEditor.ModuleCompletionItemKind
                      }
                    ]

        {- TODO: Add test for completion items out of core modules like List, Maybe, Result, etc. -}
        ]


expectationFromScenario :
    List ( List String, String )
    -> ( List String, String )
    -> FrontendWeb.MonacoEditor.RequestCompletionItemsStruct
    -> List FrontendWeb.MonacoEditor.MonacoCompletionItem
    -> Expect.Expectation
expectationFromScenario otherFiles fileOpenedInEditor request expectedItems =
    let
        fileTree =
            fileOpenedInEditor
                :: otherFiles
                |> List.map (Tuple.mapSecond fileContentFromString)
                |> FileTreeInWorkspace.sortedFileTreeFromListOfBlobsAsBytes

        languageServiceState =
            LanguageService.initLanguageServiceState
                |> LanguageService.updateLanguageServiceState fileTree
    in
    Expect.equal expectedItems
        (LanguageService.provideCompletionItems
            { filePathOpenedInEditor = Tuple.first fileOpenedInEditor
            , textUntilPosition = request.textUntilPosition
            }
            languageServiceState
        )


fileContentFromString : String -> Bytes.Bytes
fileContentFromString =
    Bytes.Encode.string >> Bytes.Encode.encode
