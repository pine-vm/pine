module ProjectStateModelTests exposing (..)

import Base64
import Bytes
import Bytes.Encode
import Expect
import FileTree
import FileTreeInWorkspace
import ProjectState_2021_01
import SHA256
import Test


demoProject : String -> FileTreeInWorkspace.FileTreeNode
demoProject elmModuleText =
    FileTreeInWorkspace.sortedFileTreeFromListOfBlobsAsBytes
        [ ( [ "src", "TestModule.elm" ]
          , elmModuleText |> Bytes.Encode.string |> Bytes.Encode.encode
          )
        ]


project_state_find_diff_encoding_2021_01 : Test.Test
project_state_find_diff_encoding_2021_01 =
    let
        testName =
            "Project state find diff encoding"

        baseProjectModuleText =
            """
module Main exposing (main)

import Html
import Html.Attributes
import Keyboard.Key
import Simplegamedev20190510
    exposing
        ( KeyboardEvent
        , SimpleGame
        , composeSimpleGame
        , listDictGet
        , listRemoveSet
        , svgRectFrom_Fill_Left_Top_Width_Height
        )
import Svg
import Svg.Attributes


worldSizeX : Int
worldSizeX =
    16


worldSizeY : Int
worldSizeY =
    12


type SnakeDirection
    = Up
    | Right
    | Down
    | Left


type alias GameState =
    { snake : Snake
    , appleLocation : Location
    }


type alias Location =
    { x : Int
    , y : Int
    }


type alias Snake =
    { headDirection : SnakeDirection
    , headLocation : Location
    , tailSegments : List Location
    }


main : SimpleGame GameState ()
main =
    composeSimpleGame
        { updateIntervalInMilliseconds = 125
        , updatePerInterval = moveSnakeForwardOneStep
        , updateOnKeyDown = onKeyDown
        , updateOnKeyUp = always identity
        , renderToHtml = renderToHtml
        , initialState = initialState
        , updateForEventFromHtml = always identity
        }


initialState : GameState
initialState =
    { snake = { headDirection = Right, headLocation = { x = 4, y = 5 }, tailSegments = [ { x = 3, y = 5 } ] }
    , appleLocation = { x = 3, y = 2 }
    }


snakeDirectionFromKeyboardKey : List ( Keyboard.Key.Key, SnakeDirection )
snakeDirectionFromKeyboardKey =
    [ ( Keyboard.Key.W, Up )
    , ( Keyboard.Key.A, Left )
    , ( Keyboard.Key.S, Down )
    , ( Keyboard.Key.D, Right )
    , ( Keyboard.Key.Up, Up )
    , ( Keyboard.Key.Down, Down )
    , ( Keyboard.Key.Left, Left )
    , ( Keyboard.Key.Right, Right )
    ]


onKeyDown : KeyboardEvent -> GameState -> GameState
onKeyDown keyboardEvent gameStateBefore =
    case snakeDirectionFromKeyboardKey |> listDictGet keyboardEvent.keyCode of
        Nothing ->
            gameStateBefore

        Just snakeDirection ->
            let
                snakeBefore =
                    gameStateBefore.snake
            in
            { gameStateBefore | snake = { snakeBefore | headDirection = snakeDirection } }


xyOffsetFromDirection : SnakeDirection -> { x : Int, y : Int }
xyOffsetFromDirection direction =
    case direction of
        Up ->
            { x = 0, y = -1 }

        Down ->
            { x = 0, y = 1 }

        Left ->
            { x = -1, y = 0 }

        Right ->
            { x = 1, y = 0 }


moveSnakeForwardOneStep : GameState -> GameState
moveSnakeForwardOneStep gameStateBefore =
    let
        snakeBefore =
            gameStateBefore.snake

        headLocationBefore =
            snakeBefore.headLocation

        headMovement =
            xyOffsetFromDirection snakeBefore.headDirection

        headLocationBeforeWrapping =
            { x = headLocationBefore.x + headMovement.x
            , y = headLocationBefore.y + headMovement.y
            }

        headLocation =
            { x = (headLocationBeforeWrapping.x + worldSizeX) |> modBy worldSizeX
            , y = (headLocationBeforeWrapping.y + worldSizeY) |> modBy worldSizeY
            }

        snakeEats =
            headLocation == gameStateBefore.appleLocation

        tailSegmentsIfSnakeWereGrowing =
            headLocationBefore :: snakeBefore.tailSegments

        tailSegments =
            tailSegmentsIfSnakeWereGrowing
                |> List.reverse
                |> List.drop
                    (if snakeEats then
                        0

                     else
                        1
                    )
                |> List.reverse

        appleLocation =
            if not snakeEats then
                gameStateBefore.appleLocation

            else
                let
                    cellsLocationsWithoutSnake =
                        List.range 0 (worldSizeX - 1)
                            |> List.concatMap
                                (\\x ->
                                    List.range 0 (worldSizeY - 1)
                                        |> List.map (\\y -> { x = x, y = y })
                                )
                            |> listRemoveSet (headLocation :: tailSegments)
                in
                cellsLocationsWithoutSnake
                    |> List.drop (15485863 |> modBy ((cellsLocationsWithoutSnake |> List.length) - 1))
                    |> List.head
                    |> Maybe.withDefault { x = -1, y = -1 }
    in
    { gameStateBefore
        | snake = { snakeBefore | headLocation = headLocation, tailSegments = tailSegments }
        , appleLocation = appleLocation
    }


renderToHtml : GameState -> Html.Html ()
renderToHtml gameState =
    let
        cellSideLength =
            30

        svgRectAtCellLocation fill cellLocation =
            svgRectFrom_Fill_Left_Top_Width_Height
                fill
                ( cellLocation.x * cellSideLength + 1
                , cellLocation.y * cellSideLength + 1
                )
                ( cellSideLength - 2
                , cellSideLength - 2
                )

        snakeView =
            gameState.snake.headLocation
                :: gameState.snake.tailSegments
                |> List.map (svgRectAtCellLocation "whitesmoke")
                |> Svg.g []

        appleView =
            svgRectAtCellLocation "red" gameState.appleLocation
    in
    Svg.svg
        [ Svg.Attributes.width (worldSizeX * cellSideLength |> String.fromInt)
        , Svg.Attributes.height (worldSizeY * cellSideLength |> String.fromInt)
        , Html.Attributes.style "background" "black"
        ]
        [ snakeView, appleView ]
"""

        baseProject =
            demoProject baseProjectModuleText

        editedProject =
            demoProject
                (String.replace
                    """svgRectAtCellLocation "red" gameState.appleLocation"""
                    """svgRectAtCellLocation "blue" gameState.appleLocation"""
                    baseProjectModuleText
                )
    in
    case FileTreeInWorkspace.searchProjectStateDifference_2021_01 editedProject { baseComposition = baseProject } of
        Err error ->
            Test.test testName <|
                \_ ->
                    Expect.fail ("Failed to find diff model: " ++ error)

        Ok projectStateDiffModel ->
            case
                baseProject
                    |> FileTreeInWorkspace.mapBlobsToBytes
                    |> FileTreeInWorkspace.applyProjectStateDifference_2021_01 projectStateDiffModel
            of
                Err applyDiffError ->
                    Test.test "Try apply diff to restore project state" <|
                        \_ ->
                            Expect.fail ("Failed to apply project state diff: " ++ applyDiffError)

                Ok restoredProjectState ->
                    let
                        restoredProjectStateAsBase64 =
                            restoredProjectState
                                |> FileTree.mapBlobs (Base64.fromBytes >> Maybe.withDefault "Failed to encode in base64")
                    in
                    Test.describe testName
                        [ Test.test "Projects modeled in the test code are not equal" <|
                            \_ ->
                                {- Avoid bug in Elm core library as reported at https://github.com/elm/bytes/issues/15 :
                                   Convert to other representation before comparing.
                                -}
                                Expect.notEqual (compositionHashFromProject editedProject) (compositionHashFromProject baseProject)
                        , Test.test "Project state restored from diff equals input project state" <|
                            \_ ->
                                Expect.equal (FileTree.mapBlobs .asBase64 editedProject) restoredProjectStateAsBase64
                        , Test.test "projectStateDiffModel.setNodes contains 1 element" <|
                            \_ ->
                                Expect.equal 1 (projectStateDiffModel.changeBlobs |> List.length)
                        , Test.test "Difference model is small enough" <|
                            \_ ->
                                Expect.atMost (String.length baseProjectModuleText // 2)
                                    (approximateSizeOfProjectStateDifferenceModel_2021_01 projectStateDiffModel)
                        ]


compositionHashFromProject : FileTreeInWorkspace.FileTreeNode -> String
compositionHashFromProject =
    FileTreeInWorkspace.compositionHashFromFileTreeNode >> SHA256.toHex


approximateSizeOfProjectStateDifferenceModel_2021_01 : ProjectState_2021_01.ProjectStateDifference -> Int
approximateSizeOfProjectStateDifferenceModel_2021_01 diff =
    [ diff.removeNodes
        |> List.concatMap (\removeNode -> [ removeNode |> List.map String.length |> List.sum, 20 ])
    , diff.changeBlobs
        |> List.concatMap
            (\( blobPath, blobChanges ) ->
                [ [ 30 ]
                , blobPath |> List.map String.length
                , blobChanges
                    |> List.concatMap
                        (\changeElement ->
                            case changeElement of
                                ProjectState_2021_01.RemoveBytes _ ->
                                    [ 30 ]

                                ProjectState_2021_01.ReuseBytes _ ->
                                    [ 30 ]

                                ProjectState_2021_01.AddBytes addBytes ->
                                    [ 30, Bytes.width addBytes ]
                        )
                ]
            )
        |> List.concat
    ]
        |> List.concat
        |> List.sum
