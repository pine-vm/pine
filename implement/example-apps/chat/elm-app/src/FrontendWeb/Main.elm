module FrontendWeb.Main exposing (Event(..), State, init, main, update, view)

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Conversation
import Dict
import ElmFullstackCompilerInterface.GenerateJsonCoders
import FrontendBackendInterface
import FrontendWeb.Visuals as Visuals exposing (HtmlStyle, htmlAttributesStyles)
import Html
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode
import Task
import Time
import Url
import Url.Builder


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = always (Time.every 3000 ArrivedAtTime)
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


type alias State =
    { navigationKey : Navigation.Key
    , time : Time.Posix
    , lastRequestToBackendTime : Maybe Time.Posix
    , messageToAddText : String
    , editingChosenName : Maybe String
    , lastRequestToBackendResult : Maybe { time : Time.Posix, result : RequestToBackendResultStructure }
    , lastMessageFromBackend : Maybe { time : Time.Posix, message : FrontendBackendInterface.MessageToClient }
    , conversationHistory : List Conversation.Event
    }


type Event
    = ArrivedAtTime Time.Posix
    | RequestToBackendResult RequestToBackendResultStructure
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | UserEnterMessageText String
    | UserInputAddMessage
    | DomTaskResult (Result Browser.Dom.Error ())


type alias RequestToBackendResultStructure =
    Result Http.Error FrontendBackendInterface.MessageToClient


type alias ViewConfiguration =
    { usersProfiles : Dict.Dict Int { chosenName : String } }


init : () -> Url.Url -> Navigation.Key -> ( State, Cmd Event )
init _ url navigationKey =
    { navigationKey = navigationKey
    , time = Time.millisToPosix 0
    , messageToAddText = ""
    , editingChosenName = Nothing
    , lastRequestToBackendTime = Nothing
    , lastRequestToBackendResult = Nothing
    , lastMessageFromBackend = Nothing
    , conversationHistory = []
    }
        |> update (UrlChange url)


requestToBackendCmd : FrontendBackendInterface.RequestFromUser -> Cmd Event
requestToBackendCmd request =
    Http.post
        { url = Url.Builder.relative [ "api" ] []
        , body = Http.jsonBody (request |> ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeRequestFromUser)
        , expect = Http.expectJson RequestToBackendResult ElmFullstackCompilerInterface.GenerateJsonCoders.jsonDecodeMessageToClient
        }


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    let
        ( state, cmdsLessScrolling ) =
            updateLessScrolling event stateBefore

        scrollTask =
            if shouldScrollHistoryToBottomAfterUpdate stateBefore state then
                Task.attempt DomTaskResult (setViewportToBottom historyViewContainerId)

            else
                Cmd.none
    in
    ( state, [ cmdsLessScrolling, scrollTask ] |> Cmd.batch )


setViewportToBottom : String -> Task.Task Browser.Dom.Error ()
setViewportToBottom id =
    Browser.Dom.getViewportOf id
        |> Task.andThen (\info -> Browser.Dom.setViewportOf id 0 info.scene.height)


shouldScrollHistoryToBottomAfterUpdate : State -> State -> Bool
shouldScrollHistoryToBottomAfterUpdate stateBeforeUpdate stateAfterUpdate =
    stateBeforeUpdate.conversationHistory /= stateAfterUpdate.conversationHistory


updateLessScrolling : Event -> State -> ( State, Cmd Event )
updateLessScrolling event stateBefore =
    case event of
        ArrivedAtTime time ->
            ( { stateBefore | time = time }, requestToBackendCmd FrontendBackendInterface.BeOnline )

        RequestToBackendResult requestToBackendResult ->
            let
                stateAfterRememberResult =
                    { stateBefore
                        | lastRequestToBackendResult = Just { time = stateBefore.time, result = requestToBackendResult }
                    }

                state =
                    case requestToBackendResult of
                        Err _ ->
                            stateAfterRememberResult

                        Ok response ->
                            stateAfterRememberResult |> updateForMessageFromServer response
            in
            ( state, Cmd.none )

        UrlChange url ->
            ( stateBefore, Cmd.none )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( stateBefore, Navigation.pushUrl stateBefore.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( stateBefore, Navigation.load url )

        UserEnterMessageText messageText ->
            ( { stateBefore | messageToAddText = messageText }, Cmd.none )

        UserInputAddMessage ->
            if not (stateBefore |> offerUserToSendMessage) then
                ( stateBefore, Cmd.none )

            else
                let
                    requestToServerCmd =
                        requestToBackendCmd (FrontendBackendInterface.AddTextMessage stateBefore.messageToAddText)

                    focusCmd =
                        Browser.Dom.focus messageToAddTextboxId |> Task.attempt DomTaskResult
                in
                ( { stateBefore | lastRequestToBackendTime = Just stateBefore.time, editingChosenName = Nothing }
                , [ requestToServerCmd, focusCmd ] |> Cmd.batch
                )

        DomTaskResult _ ->
            ( stateBefore, Cmd.none )


updateForMessageFromServer : FrontendBackendInterface.MessageToClient -> State -> State
updateForMessageFromServer messageFromServer stateBefore =
    let
        ( eventsToAdd, eventRepresentsMessageToSend ) =
            let
                innerEventRepresentsMessageToSend historyEvent =
                    case messageFromServer.currentUserId of
                        Nothing ->
                            False

                        Just currentUserId ->
                            ((messageFromServer.currentPosixTimeMilli - historyEvent.posixTimeMilli) < addMessageWaitTimeMaxMilli)
                                && (historyEvent.message == Conversation.LeafPlainText stateBefore.messageToAddText)
                                && (historyEvent.origin == Conversation.FromUser { userId = currentUserId })

                eventsToAddFromErrorMessage =
                    case messageFromServer.messageToUser of
                        Nothing ->
                            []

                        Just messageToUser ->
                            [ { origin = Conversation.FromSystem
                              , message = messageToUser
                              , posixTimeMilli = messageFromServer.currentPosixTimeMilli
                              }
                            ]

                -- Also, add a 'connection lost' notification to `eventsToAdd` when we have not heard from the server in a while.
            in
            ( messageFromServer.conversationHistory ++ eventsToAddFromErrorMessage, innerEventRepresentsMessageToSend )

        history =
            eventsToAdd
                |> List.foldr addEventIntoHistory stateBefore.conversationHistory

        historyContainsMessageToAdd =
            history |> List.any eventRepresentsMessageToSend

        truncatedHistory =
            history |> List.take historyRetainedLength

        messageToAddText =
            if historyContainsMessageToAdd then
                ""

            else
                stateBefore.messageToAddText
    in
    { stateBefore
        | conversationHistory = truncatedHistory
        , messageToAddText = messageToAddText
        , lastMessageFromBackend = Just { time = stateBefore.time, message = messageFromServer }
    }


addEventIntoHistory : Conversation.Event -> List Conversation.Event -> List Conversation.Event
addEventIntoHistory event historyBefore =
    if historyBefore |> List.member event then
        historyBefore

    else
        event :: historyBefore


view : State -> Browser.Document Event
view state =
    let
        messageTextboxAttributes =
            [ HA.id messageToAddTextboxId
            , HA.value state.messageToAddText
            , HA.placeholder "Enter your message here"
            , HE.onInput UserEnterMessageText
            , onEnterKeyDown UserInputAddMessage
            ]
                ++ htmlAttributesStyles messageTextboxStyle

        sendMessageButtonAttributes =
            [ HE.onClick UserInputAddMessage
            , HA.style "visibility"
                (if state |> offerUserToSendMessage then
                    "visible"

                 else
                    "hidden"
                )
            , HA.style "margin" "2px"
            , HA.style "font-size" "inherit"
            ]

        sendMessageButton =
            [ "Add" |> Html.text ]
                |> Visuals.button sendMessageButtonAttributes

        participateControlView =
            [ [] |> Html.input messageTextboxAttributes
            , sendMessageButton
            ]
                |> Html.div [ HA.style "display" "flex", HA.style "flex-direction" "row", HA.style "margin" "2px" ]

        viewConfiguration =
            { usersProfiles = Dict.empty }

        historyView =
            state.conversationHistory
                |> List.reverse
                |> List.map (viewConversationEvent viewConfiguration state)
                |> Html.div (HA.id historyViewContainerId :: htmlAttributesStyles historyViewStyle)

        conversationColumn =
            [ historyView, participateControlView ]
                |> Html.div (htmlAttributesStyles [ ( "width", "100%" ), ( "height", "100%" ), ( "display", "flex" ), ( "flex-direction", "column" ) ])

        peopleOnlineCaption =
            [ "People online" |> Html.text ]
                |> Html.div (htmlAttributesStyles [ ( "font-size", "120%" ), ( "margin", "4px" ) ])

        peopleOnlineViewBeforeStyle =
            case state.lastMessageFromBackend of
                Just lastMessageFromBackend ->
                    lastMessageFromBackend.message.usersOnline
                        |> List.map (Conversation.FromUser >> viewDialogEventOrigin viewConfiguration state)
                        |> List.map (List.singleton >> Html.div [])

                Nothing ->
                    [ "❌ Connection error" |> Html.text ]

        peopleOnlineView =
            peopleOnlineViewBeforeStyle |> Html.div (htmlAttributesStyles peopleOnlineViewStyle)

        peopleOnlineColumn =
            [ peopleOnlineCaption, peopleOnlineView ]
                |> Html.div (htmlAttributesStyles [ ( "display", "flex" ), ( "flex-direction", "column" ), ( "width", "25%" ), ( "padding", "4px" ) ])

        connectionStateHtml =
            case state.lastRequestToBackendResult of
                Nothing ->
                    "Connecting to server..." |> Html.text

                Just lastRequestResult ->
                    case lastRequestResult.result of
                        Err error ->
                            ("Error communicating with server: " ++ (error |> Visuals.describeHttpError)) |> Html.text

                        Ok result ->
                            "Connection OK" |> Html.text

        chatHtml =
            [ [ [ conversationColumn ] |> Html.div (htmlAttributesStyles [ ( "display", "flex" ), ( "width", "75%" ) ])
              , peopleOnlineColumn
              ]
                |> Html.div (htmlAttributesStyles [ ( "display", "flex" ), ( "flex-direction", "row" ), ( "width", "100%" ), ( "height", "100%" ) ])
            ]
                |> Html.div
                    [ HA.style "flex" "1"
                    , HA.style "display" "flex"
                    , HA.style "height" "100%"
                    ]

        body =
            [ Visuals.globalStylesHtmlElement
            , [ chatHtml
              ]
                |> Html.div
                    [ HA.style "margin" "0"
                    , HA.style "height" "99vh"
                    ]
            ]
    in
    { title = "Chat demo Elm-fullstack app", body = body }


historyViewStyle : HtmlStyle
historyViewStyle =
    [ ( "overflow-y", "auto" )
    , ( "flex", "1" )
    , ( "background", "rgba(111, 111, 111, 0.3)" )
    , ( "opacity", "0.9" )
    , ( "padding", "4px" )
    , ( "margin", "2px" )
    ]


peopleOnlineViewStyle : HtmlStyle
peopleOnlineViewStyle =
    [ ( "overflow-y", "auto" )
    , ( "overflow-x", "hidden" )
    , ( "background", "rgba(111, 111, 111, 0.3)" )
    , ( "opacity", "0.9" )
    , ( "padding", "4px" )
    , ( "margin", "2px" )
    , ( "flex", "1" )
    , ( "white-space", "nowrap" )
    ]


messageTextboxStyle : HtmlStyle
messageTextboxStyle =
    ( "flex", "1" ) :: ( "color", "inherit" ) :: textboxStyle


textboxStyle : HtmlStyle
textboxStyle =
    [ ( "margin", "2px" )
    , ( "border", "2px solid #333" )
    , ( "background", "rgba(111,111,111,0.2)" )
    , ( "padding", "1px" )
    ]
        ++ ([ "font-size", "font-family" ] |> List.map (\property -> ( property, "inherit" )))


{-| Based on <https://github.com/elm-community/html-extra/blob/bfceab2694342bd3b421c27079b622eb7410c13d/src/Html/Events/Extra.elm#L264-L277>
-}
onEnterKeyDown : event -> Html.Attribute event
onEnterKeyDown event =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed event

            else
                Json.Decode.fail ("not ENTER (" ++ (code |> String.fromInt) ++ ")")
    in
    HE.on "keydown" (Json.Decode.andThen isEnter HE.keyCode)


offerUserToSendMessage : State -> Bool
offerUserToSendMessage state =
    (state.messageToAddText |> String.length) > 0


viewConversationEvent : ViewConfiguration -> State -> Conversation.Event -> Html.Html event
viewConversationEvent viewConfiguration state event =
    [ [ event.origin |> viewDialogEventOrigin viewConfiguration state
      , [] |> Html.span [ HA.style "padding-left" "1em" ]
      , viewConversationEventTime state { eventPosixTime = event.posixTimeMilli // 1000 }
      ]
        |> Html.div []
    , [ [ event.message |> viewConversationEventMessage viewConfiguration state
        ]
            |> Html.div [ viewDialogEventTextStyle event.origin ]
      ]
        |> Html.div [ HA.style "margin-left" "0.3em" ]
    ]
        |> Html.div [ HA.style "margin" "0.5em" ]


viewConversationEventTime : State -> { eventPosixTime : Int } -> Html.Html event
viewConversationEventTime state { eventPosixTime } =
    let
        eventTime =
            Time.millisToPosix (eventPosixTime * 1000)

        relativeDayName =
            state
                |> predictServerPosixTime
                |> Maybe.andThen (\currentTime -> Visuals.getRelativeDayName { currentTime = currentTime, namedTime = eventTime })

        dayText =
            case relativeDayName of
                Just Visuals.Today ->
                    "Today"

                Just Visuals.Yesterday ->
                    "Yesterday"

                Nothing ->
                    Visuals.calendarDayTextFromPosixTime eventTime
    in
    [ (dayText ++ " at " ++ (eventTime |> intraDayTimeDisplayText)) |> Html.text ]
        |> Html.span [ HA.style "opacity" "0.4", HA.style "font-size" "80%" ]


intraDayTimeDisplayText : Time.Posix -> String
intraDayTimeDisplayText time =
    (time |> Time.toHour Time.utc |> String.fromInt)
        ++ ":"
        ++ (time |> Time.toMinute Time.utc |> String.fromInt |> String.padLeft 2 '0')


predictServerPosixTime : State -> Maybe Time.Posix
predictServerPosixTime state =
    state.lastMessageFromBackend
        |> Maybe.map
            (\lastMessageFromBackend ->
                Time.millisToPosix
                    (lastMessageFromBackend.message.currentPosixTimeMilli
                        + (state.time |> Time.posixToMillis)
                        - (lastMessageFromBackend.time |> Time.posixToMillis)
                    )
            )


viewDialogEventOrigin : ViewConfiguration -> State -> Conversation.EventOrigin -> Html.Html event
viewDialogEventOrigin viewConfiguration state origin =
    case origin of
        Conversation.FromUser fromOriginUser ->
            [ viewPlayerLink viewConfiguration { userId = fromOriginUser.userId } ] |> Html.span []

        Conversation.FromSystem ->
            [ "\u{1F916} System" |> Html.text ] |> Html.span []


viewPlayerLink : ViewConfiguration -> { userId : Int } -> Html.Html event
viewPlayerLink viewConfiguration { userId } =
    let
        playerIdView =
            "[" ++ (userId |> String.fromInt) ++ "] "

        playerName =
            viewConfiguration.usersProfiles |> Dict.get userId |> Maybe.map .chosenName |> Maybe.withDefault ""
    in
    [ ("👤 " ++ playerIdView ++ playerName) |> Html.text ]
        |> Html.span [ HA.style "color" playerNameColor ]


playerNameColor : String
playerNameColor =
    "gold"


viewDialogEventTextStyle : Conversation.EventOrigin -> Html.Attribute a
viewDialogEventTextStyle origin =
    case origin of
        Conversation.FromUser _ ->
            HA.style "color" "whitesmoke"

        Conversation.FromSystem ->
            HA.style "color" "orange"


viewConversationEventMessage : ViewConfiguration -> State -> Conversation.EventContentNode -> Html.Html a
viewConversationEventMessage viewConfiguration state message =
    case message of
        Conversation.LeafPlainText text ->
            text |> Html.text

        Conversation.LeafUser user ->
            ({ userId = user.userId } |> Conversation.FromUser) |> viewDialogEventOrigin viewConfiguration state

        Conversation.LeafLinkToUrl link ->
            [ link.url |> Html.text ]
                |> Html.a [ HA.href link.url ]

        Conversation.SequenceOfNodes components ->
            components
                |> List.map (viewConversationEventMessage viewConfiguration state)
                |> Html.span []


readEventsTimeDistanceMilli : Int
readEventsTimeDistanceMilli =
    3000


addMessageWaitTimeMaxMilli : Int
addMessageWaitTimeMaxMilli =
    readEventsTimeDistanceMilli - 500


historyRetainedLength : Int
historyRetainedLength =
    100


historyViewContainerId : String
historyViewContainerId =
    "conversation-history"


messageToAddTextboxId : String
messageToAddTextboxId =
    "message-to-add-input"
