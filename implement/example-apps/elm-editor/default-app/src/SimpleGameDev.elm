module SimpleGameDev exposing (..)

{-| This module provides a framework to build video games as well as a library of standard helper functions.
The framework wraps the more general Elm program type with an interface optimized for video games.


# Composing the Game

@docs game, GameProgram, GameComposition, KeyboardEvent


# Common Helpers

Following are generic helper functions which are not specific to one particular game.

@docs svgRectangle, listRemoveSet, listDictGet

-}

import Browser
import Browser.Dom
import Browser.Events
import Html
import Json.Decode
import Keyboard.Event
import Playground
import Svg
import Svg.Attributes
import Task
import Time



{-
   Provide a framework so beginners can start developing a game with less effort.
   Take care of the subscriptions and the overall update function, and provide an interface with nice names for the update functions for specific kinds of events.
-}


{-| Describes how your game program is composed of functions that describe how to handle the different kinds of events like for example, pressing or releasing key.
-}
type alias GameComposition state eventFromView =
    { initialState : state
    , view : GameView state eventFromView
    , updateOnKeyDown : Maybe (Keyboard.Event.KeyboardEvent -> state -> state)
    , updateOnKeyUp : Maybe (Keyboard.Event.KeyboardEvent -> state -> state)
    , updateBasedOnTime : Maybe (UpdateBasedOnTime state)
    }


{-| This type helps you write a type annotation for the function describing the composition of your game:

    main : GameProgram GameState ()
    main =
    ....

-}
type alias GameProgram state eventFromView =
    Program () (GameStateIncludingFramework state) (GameEventStructure eventFromView)


type alias GameStateIncludingFramework state =
    { specific : state
    , framework : FrameworkState
    }


type alias FrameworkState =
    { browserSize : { width : Int, height : Int }
    }


type GameView state event
    = HtmlViewWithoutInputs (state -> Html.Html event)
    | HtmlViewWithInputs (state -> Html.Html event) (event -> state -> state)
    | PictureView (state -> PictureViewResult)


type alias PictureViewResult =
    { shapes : List Playground.Shape
    , viewport : { width : Float, height : Float }
    , backgroundColor : Playground.Color
    }


type UpdateBasedOnTime state
    = FixedInterval { intervalInMilliseconds : Int, update : state -> state }


type GameEventStructure eventFromView
    = TimeArrivedEvent Time.Posix
    | KeyDownEvent KeyboardEventStructure
    | KeyUpEvent KeyboardEventStructure
    | FromHtmlEvent eventFromView
    | BrowserResizedEvent Int Int
    | BrowserGotViewportEvent Browser.Dom.Viewport


{-| This type describes the keyboard events as used in the functions `updateOnKeyDown` and `updateOnKeyUp`.
Use as follows:

    updateOnKeyDown : KeyboardEvent -> GameState -> GameState
    updateOnKeyDown =
    ...

-}
type alias KeyboardEventStructure =
    Keyboard.Event.KeyboardEvent


htmlViewWithoutInputs : { renderToHtml : state -> Html.Html event } -> GameView state event
htmlViewWithoutInputs { renderToHtml } =
    HtmlViewWithoutInputs renderToHtml


htmlViewWithInputs :
    { renderToHtml : state -> Html.Html event, updateForInput : event -> state -> state }
    -> GameView state event
htmlViewWithInputs { renderToHtml, updateForInput } =
    HtmlViewWithInputs renderToHtml updateForInput


pictureView : (state -> PictureViewResult) -> GameView state event
pictureView =
    PictureView


updateWithFixedInterval : { intervalInMilliseconds : Int, update : state -> state } -> UpdateBasedOnTime state
updateWithFixedInterval =
    FixedInterval


{-| Use this function to compose a complete game, connecting the specific functions in your project.
Following is an example:

    main : SimpleGameDev.GameProgram GameState ()
    main =
        SimpleGameDev.game
            { initialState = initialState
            , view =
                SimpleGameDev.htmlViewWithoutInputs
                    { renderToHtml = renderToHtml }
            , updateBasedOnTime =
                Just
                    (SimpleGameDev.updateWithFixedInterval
                        { intervalInMilliseconds = 125
                        , update = moveSnakeForwardOneStep
                        }
                    )
            , updateOnKeyDown = Just onKeyDown
            , updateOnKeyUp = Nothing
            }

-}
game :
    GameComposition state eventFromHtml
    -> GameProgram state eventFromHtml
game gameConfig =
    let
        ( view, updateForEventFromHtml ) =
            case gameConfig.view of
                HtmlViewWithoutInputs renderToHtml ->
                    ( .specific >> renderToHtml >> Html.map FromHtmlEvent, Nothing )

                HtmlViewWithInputs renderToHtml updateForInput ->
                    ( .specific >> renderToHtml >> Html.map FromHtmlEvent, Just updateForInput )

                PictureView specifcView ->
                    ( \state ->
                        let
                            { shapes, viewport, backgroundColor } =
                                specifcView state.specific

                            shapesIncludingBackground =
                                Playground.rectangle backgroundColor 99999 99999
                                    :: shapes
                        in
                        Playground.render
                            (Playground.toScreen viewport.width viewport.height)
                            shapesIncludingBackground
                    , Nothing
                    )

        wrapUpdateGameSpecificState :
            (state -> state)
            -> GameStateIncludingFramework state
            -> GameStateIncludingFramework state
        wrapUpdateGameSpecificState updateGameSpecificState { specific, framework } =
            GameStateIncludingFramework (updateGameSpecificState specific) framework

        update :
            GameEventStructure eventFromHtml
            -> GameStateIncludingFramework state
            -> GameStateIncludingFramework state
        update event stateBefore =
            case event of
                KeyDownEvent keyDown ->
                    wrapUpdateGameSpecificState
                        ((gameConfig.updateOnKeyDown |> Maybe.withDefault (always identity)) keyDown)
                        stateBefore

                KeyUpEvent keyUp ->
                    wrapUpdateGameSpecificState
                        ((gameConfig.updateOnKeyUp |> Maybe.withDefault (always identity)) keyUp)
                        stateBefore

                TimeArrivedEvent _ ->
                    wrapUpdateGameSpecificState
                        (gameConfig.updateBasedOnTime
                            |> Maybe.map
                                (\updateBasedOnTime ->
                                    case updateBasedOnTime of
                                        FixedInterval fixedInterval ->
                                            fixedInterval.update
                                )
                            |> Maybe.withDefault identity
                        )
                        stateBefore

                FromHtmlEvent fromHtmlEvent ->
                    wrapUpdateGameSpecificState
                        ((updateForEventFromHtml |> Maybe.withDefault (always identity)) fromHtmlEvent)
                        stateBefore

                BrowserResizedEvent width height ->
                    let
                        frameworkBefore =
                            stateBefore.framework
                    in
                    { stateBefore
                        | framework =
                            { frameworkBefore
                                | browserSize = { width = width, height = height }
                            }
                    }

                BrowserGotViewportEvent { viewport } ->
                    let
                        frameworkBefore =
                            stateBefore.framework
                    in
                    { stateBefore
                        | framework =
                            { frameworkBefore
                                | browserSize = { width = floor viewport.width, height = floor viewport.height }
                            }
                    }

        subscriptions _ =
            let
                updateBasedOnTimeSub =
                    case gameConfig.updateBasedOnTime of
                        Nothing ->
                            Nothing

                        Just (FixedInterval fixedInterval) ->
                            Just (Time.every (toFloat fixedInterval.intervalInMilliseconds) TimeArrivedEvent)
            in
            [ Just (Browser.Events.onKeyDown (Keyboard.Event.decodeKeyboardEvent |> Json.Decode.map KeyDownEvent))
            , updateBasedOnTimeSub
            , Just (Browser.Events.onResize BrowserResizedEvent)
            ]
                |> List.filterMap identity
                |> Sub.batch
    in
    Browser.element
        { init =
            always
                ( { specific = gameConfig.initialState
                  , framework = { browserSize = { width = 100, height = 100 } }
                  }
                , Task.perform BrowserGotViewportEvent Browser.Dom.getViewport
                )
        , view = view
        , update = \event state -> ( update event state, Cmd.none )
        , subscriptions = subscriptions
        }


{-| Generate the HTML code for an SVG rectangle. Note the rectangle will only be visible when placed in an SVG element.
Following is an example of how to use it:

    svgRectangle { fill = "red" } { left = 10, top = 10, width = 7, height = 4 }

-}
svgRectangle : { fill : String } -> { left : Int, top : Int, width : Int, height : Int } -> Svg.Svg a
svgRectangle { fill } { left, top, width, height } =
    Svg.rect
        [ Svg.Attributes.fill fill
        , Svg.Attributes.x (left |> String.fromInt)
        , Svg.Attributes.y (top |> String.fromInt)
        , Svg.Attributes.width (width |> String.fromInt)
        , Svg.Attributes.height (height |> String.fromInt)
        ]
        []


{-| Remove a set of values from a list
-}
listRemoveSet : List a -> List a -> List a
listRemoveSet elementsToRemove =
    List.filter (\element -> elementsToRemove |> List.member element |> not)


{-| Get the value matching the given key out of a dictionary.
-}
listDictGet : key -> List ( key, value ) -> Maybe value
listDictGet key =
    List.filterMap
        (\( candidateKey, candidateValue ) ->
            if key == candidateKey then
                Just candidateValue

            else
                Nothing
        )
        >> List.head
