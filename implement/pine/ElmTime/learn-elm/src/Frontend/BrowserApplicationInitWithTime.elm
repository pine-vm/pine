module Frontend.BrowserApplicationInitWithTime exposing (..)

{-| This module extends `Browser.application` to add the current time as a parameter for the `init` function.
-}

import Browser
import Browser.Navigation
import Html
import Task
import Time
import Url


type alias Program flags state event =
    Platform.Program flags (State flags state event) (Event event)


type State flags state event
    = WaitingForTimeState (WaitingForTimeStateStructure flags event)
    | InitializedState state


type alias WaitingForTimeStateStructure flags event =
    { flags : flags
    , navigationKey : Browser.Navigation.Key
    , url : Url.Url
    , queuedEvents : List event
    }


type Event event
    = TimeHasArrivedEvent Time.Posix
    | InitializedEvent event


type alias Configuration flags state event =
    { update : event -> state -> ( state, Cmd event )
    , init : flags -> Url.Url -> Browser.Navigation.Key -> Time.Posix -> ( state, Cmd event )
    , viewWhileWaitingForTime : Browser.Document event
    , view : state -> Browser.Document event
    , subscriptions : state -> Sub event
    , onUrlRequest : Browser.UrlRequest -> event
    , onUrlChange : Url.Url -> event
    }


application : Configuration flags state event -> Program flags state event
application configuration =
    Browser.application
        { init = init
        , update = update configuration
        , subscriptions = subscriptions configuration
        , view = view configuration
        , onUrlRequest = configuration.onUrlRequest >> InitializedEvent
        , onUrlChange = configuration.onUrlChange >> InitializedEvent
        }


init : flags -> Url.Url -> Browser.Navigation.Key -> ( State flags state event, Cmd (Event event) )
init flags url navigationKey =
    ( WaitingForTimeState
        { flags = flags
        , navigationKey = navigationKey
        , url = url
        , queuedEvents = []
        }
    , Task.perform TimeHasArrivedEvent Time.now
    )


view : Configuration flags state event -> State flags state event -> Browser.Document (Event event)
view configuration state =
    case state of
        WaitingForTimeState _ ->
            { title = configuration.viewWhileWaitingForTime.title
            , body = configuration.viewWhileWaitingForTime.body |> List.map (Html.map InitializedEvent)
            }

        InitializedState initializedState ->
            let
                { title, body } =
                    configuration.view initializedState
            in
            { title = title, body = body |> List.map (Html.map InitializedEvent) }


subscriptions : Configuration flags state event -> State flags state event -> Sub (Event event)
subscriptions configuration state =
    case state of
        WaitingForTimeState _ ->
            Time.every 500 TimeHasArrivedEvent

        InitializedState initializedState ->
            Sub.map InitializedEvent (configuration.subscriptions initializedState)


update : Configuration flags state event -> Event event -> State flags state event -> ( State flags state event, Cmd (Event event) )
update configuration event state =
    case state of
        WaitingForTimeState initState ->
            case event of
                TimeHasArrivedEvent time ->
                    initState
                        |> completeInit configuration time
                        |> Tuple.mapFirst InitializedState
                        |> Tuple.mapSecond (Cmd.map InitializedEvent)

                InitializedEvent initializedEvent ->
                    ( WaitingForTimeState { initState | queuedEvents = initializedEvent :: initState.queuedEvents }, Cmd.none )

        InitializedState initialized ->
            case event of
                InitializedEvent initializedEvent ->
                    initialized
                        |> configuration.update initializedEvent
                        |> Tuple.mapFirst InitializedState
                        |> Tuple.mapSecond (Cmd.map InitializedEvent)

                TimeHasArrivedEvent _ ->
                    ( state, Cmd.none )


completeInit : Configuration flags state event -> Time.Posix -> WaitingForTimeStateStructure flags event -> ( state, Cmd event )
completeInit configuration time stateBefore =
    stateBefore.queuedEvents
        |> List.foldr
            (\event ( state, cmdBefore ) ->
                let
                    ( nextState, nextCmd ) =
                        state |> configuration.update event
                in
                ( nextState, Cmd.batch [ cmdBefore, nextCmd ] )
            )
            (configuration.init
                stateBefore.flags
                stateBefore.url
                stateBefore.navigationKey
                time
            )
