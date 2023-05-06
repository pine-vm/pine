module Frontend.Main exposing (Event(..), State, init, main, update, view)

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import CompilationInterface.GenerateJsonConverters
import CompilationInterface.SourceFiles
import Conversation exposing (UserId)
import Dict
import Frontend.PlayAudio as PlayAudio
import Frontend.Visuals as Visuals exposing (HtmlStyle, htmlAttributesStyles)
import FrontendBackendInterface
import Html
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode
import Markdown.Parser
import Markdown.Renderer
import Set
import Task
import Time
import Url
import Url.Builder


main : Program () State Event
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = always (Time.every 5000 ArrivedAtTime)
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


type alias State =
    { navigationKey : Navigation.Key
    , time : Time.Posix
    , lastRequestsToBackendTimes : List Time.Posix
    , messageToAddText : String
    , editingChosenName : Maybe String
    , lastRequestToBackendResult : Maybe { time : Time.Posix, result : RequestToBackendResultStructure }
    , lastMessageFromBackend : Maybe { time : Time.Posix, message : FrontendBackendInterface.MessageToClient }
    , lastSeeingLobby : Maybe { time : Time.Posix, message : FrontendBackendInterface.SeeingLobbyStructure }
    , usersProfilesReads : Dict.Dict UserId { time : Time.Posix, userProfile : { chosenName : String } }
    , conversationHistory : List Conversation.Event
    , showChangeNameGuide : Bool
    , playAudio : PlayAudio.State
    }


type Event
    = ArrivedAtTime Time.Posix
    | RequestToBackendResult RequestToBackendResultStructure
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | UserEnterMessageText String
    | UserInputAddMessage
    | DomTaskResult (Result Browser.Dom.Error ())
    | UserInputContinueEditChosenName String
    | UserInputStopEditChosenName
    | EnterKeyDownOnChooseNameTextbox
    | UserInputCompleteEnterChosenName
    | UserInputShowChangeNameGuide Bool


type alias RequestToBackendResultStructure =
    Result Http.Error RequestToBackendResultOkStructure


type alias RequestToBackendResultOkStructure =
    { originatingRequest : FrontendBackendInterface.RequestFromUser
    , messageFromBackend : FrontendBackendInterface.MessageToClient
    }


type alias ViewConfiguration =
    { usersProfiles : Dict.Dict Int { chosenName : String } }


init : () -> Url.Url -> Navigation.Key -> ( State, Cmd Event )
init _ url navigationKey =
    { navigationKey = navigationKey
    , time = Time.millisToPosix 0
    , messageToAddText = ""
    , editingChosenName = Nothing
    , lastRequestsToBackendTimes = []
    , lastRequestToBackendResult = Nothing
    , lastMessageFromBackend = Nothing
    , lastSeeingLobby = Nothing
    , conversationHistory = []
    , usersProfilesReads = Dict.empty
    , showChangeNameGuide = False
    , playAudio = PlayAudio.init
    }
        |> update (UrlChange url)


requestToBackendCmd : FrontendBackendInterface.RequestFromUser -> Cmd Event
requestToBackendCmd request =
    let
        jsonDecoder =
            CompilationInterface.GenerateJsonConverters.jsonDecodeMessageToClient
                |> Json.Decode.map (\messageFromBackend -> { originatingRequest = request, messageFromBackend = messageFromBackend })
    in
    Http.post
        { url = Url.Builder.relative (FrontendBackendInterface.ApiRoute |> FrontendBackendInterface.urlPathFromRoute) []
        , body = Http.jsonBody (request |> CompilationInterface.GenerateJsonConverters.jsonEncodeRequestFromUser)
        , expect = Http.expectJson RequestToBackendResult jsonDecoder
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
    ( state |> updateForSoundEffects { stateBeforeEvent = stateBefore }
    , [ cmdsLessScrolling, scrollTask ] |> Cmd.batch
    )


setViewportToBottom : String -> Task.Task Browser.Dom.Error ()
setViewportToBottom id =
    Browser.Dom.getViewportOf id
        |> Task.andThen (\info -> Browser.Dom.setViewportOf id 0 info.scene.height)


shouldScrollHistoryToBottomAfterUpdate : State -> State -> Bool
shouldScrollHistoryToBottomAfterUpdate stateBeforeUpdate stateAfterUpdate =
    stateBeforeUpdate.conversationHistory /= stateAfterUpdate.conversationHistory


updateLessScrolling : Event -> State -> ( State, Cmd Event )
updateLessScrolling event stateBefore =
    let
        ( stateWithIntegratedEvent, eventSpecificCmd ) =
            updateSpecificToEvent event stateBefore

        ( state, generalCmd ) =
            updateToRequestSeeLobby stateWithIntegratedEvent
    in
    ( state, Cmd.batch [ eventSpecificCmd, generalCmd, httpRequestsForUserProfiles state ] )


updateSpecificToEvent : Event -> State -> ( State, Cmd Event )
updateSpecificToEvent event stateBefore =
    case event of
        ArrivedAtTime time ->
            ( { stateBefore | time = time }, Cmd.none )

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

        UrlChange _ ->
            ( stateBefore, Cmd.none )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( stateBefore, Navigation.pushUrl stateBefore.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( stateBefore, Navigation.load url )

        UserEnterMessageText messageText ->
            ( { stateBefore | messageToAddText = messageText, showChangeNameGuide = False }, Cmd.none )

        UserInputAddMessage ->
            if not (stateBefore |> offerUserToSendMessage) then
                ( stateBefore, Cmd.none )

            else
                let
                    requestToServerCmd =
                        requestToBackendCmd (FrontendBackendInterface.AddTextMessageRequest stateBefore.messageToAddText)

                    focusCmd =
                        Browser.Dom.focus messageToAddTextboxId |> Task.attempt DomTaskResult
                in
                ( { stateBefore
                    | lastRequestsToBackendTimes = stateBefore.time :: stateBefore.lastRequestsToBackendTimes
                    , editingChosenName = Nothing
                  }
                , [ requestToServerCmd, focusCmd ] |> Cmd.batch
                )

        UserInputShowChangeNameGuide showGuide ->
            ( { stateBefore | showChangeNameGuide = showGuide }, Cmd.none )

        UserInputContinueEditChosenName nameBefore ->
            ( { stateBefore | editingChosenName = Just nameBefore, showChangeNameGuide = False }
            , Browser.Dom.focus chosenNameTextboxId |> Task.attempt DomTaskResult
            )

        UserInputStopEditChosenName ->
            ( { stateBefore | editingChosenName = Nothing }, Cmd.none )

        EnterKeyDownOnChooseNameTextbox ->
            stateBefore
                |> updateSpecificToEvent UserInputCompleteEnterChosenName
                |> Tuple.mapSecond (List.singleton >> (::) (Browser.Dom.focus messageToAddTextboxId |> Task.attempt DomTaskResult) >> Cmd.batch)

        UserInputCompleteEnterChosenName ->
            ( { stateBefore | editingChosenName = Nothing }
            , stateBefore.editingChosenName
                |> Maybe.map (FrontendBackendInterface.ChooseNameRequest >> requestToBackendCmd)
                |> Maybe.withDefault Cmd.none
            )

        DomTaskResult _ ->
            ( stateBefore, Cmd.none )


updateToRequestSeeLobby : State -> ( State, Cmd Event )
updateToRequestSeeLobby stateBefore =
    let
        timeForRequestToSeeLobby =
            case stateBefore.lastRequestsToBackendTimes of
                [] ->
                    True

                lastRequestToBackendTime :: _ ->
                    let
                        requestAgeMilli =
                            Time.posixToMillis stateBefore.time - Time.posixToMillis lastRequestToBackendTime
                    in
                    5000 < requestAgeMilli
    in
    if not timeForRequestToSeeLobby then
        ( stateBefore, Cmd.none )

    else
        ( { stateBefore
            | lastRequestsToBackendTimes = stateBefore.time :: stateBefore.lastRequestsToBackendTimes |> List.take 4
          }
        , requestToBackendCmd (FrontendBackendInterface.ShowUpRequest (showUpRequestToBackendFromState stateBefore))
        )


updateForSoundEffects : { stateBeforeEvent : State } -> State -> State
updateForSoundEffects { stateBeforeEvent } stateBefore =
    let
        playAudio =
            if shouldPlaySoundMessageAddedAfterUpdate { stateBeforeEvent = stateBeforeEvent, stateAfterEvent = stateBefore } then
                stateBefore.playAudio
                    |> PlayAudio.startPlayback
                        { sourceUrl =
                            Url.Builder.relative
                                (CompilationInterface.SourceFiles.file____static_chat_message_added_0_mp3.bytes
                                    |> FrontendBackendInterface.staticContentFileName
                                    |> FrontendBackendInterface.StaticContentRoute
                                    |> FrontendBackendInterface.urlPathFromRoute
                                )
                                []
                        , volume = 1
                        }
                    |> Tuple.first

            else
                stateBefore.playAudio
    in
    { stateBefore | playAudio = playAudio }


shouldPlaySoundMessageAddedAfterUpdate : { stateBeforeEvent : State, stateAfterEvent : State } -> Bool
shouldPlaySoundMessageAddedAfterUpdate { stateBeforeEvent, stateAfterEvent } =
    case ( stateBeforeEvent.lastSeeingLobby, stateAfterEvent.lastSeeingLobby ) of
        ( Just lobbyBeforeEvent, Just lobbyAfterEvent ) ->
            lobbyAfterEvent.message.conversationHistory
                |> List.any (\eventAfterUpdate -> lobbyBeforeEvent.message.conversationHistory |> List.member eventAfterUpdate |> not)

        _ ->
            False


showUpRequestToBackendFromState : State -> { lastSeenEventPosixTimeMilli : Maybe Int }
showUpRequestToBackendFromState state =
    { lastSeenEventPosixTimeMilli =
        state.conversationHistory |> List.map .posixTimeMilli |> List.maximum
    }


httpRequestsForUserProfiles : State -> Cmd Event
httpRequestsForUserProfiles state =
    let
        visibleUsersIdsFromHistory =
            state.conversationHistory
                |> List.filterMap
                    (\event ->
                        case event.origin of
                            Conversation.FromUser { userId } ->
                                Just userId

                            Conversation.FromSystem ->
                                Nothing
                    )

        visibleUsersIdsFromSeeingLobby =
            case state.lastSeeingLobby of
                Nothing ->
                    []

                Just lastSeeingLobby ->
                    lastSeeingLobby.message.usersOnline

        visibleUsersIds =
            (visibleUsersIdsFromSeeingLobby ++ visibleUsersIdsFromHistory)
                |> Set.fromList

        userIdsWithProfilesUpdated =
            state.usersProfilesReads
                |> Dict.keys
                |> Set.fromList

        profilesToRequestUserIds =
            Set.diff visibleUsersIds userIdsWithProfilesUpdated
    in
    profilesToRequestUserIds
        |> Set.toList
        |> List.map (FrontendBackendInterface.ReadUserProfileRequest >> requestToBackendCmd)
        |> Cmd.batch


updateForMessageFromServer : RequestToBackendResultOkStructure -> State -> State
updateForMessageFromServer { originatingRequest, messageFromBackend } stateBeforeGeneralUpdate =
    let
        stateBefore =
            { stateBeforeGeneralUpdate
                | lastMessageFromBackend = Just { time = stateBeforeGeneralUpdate.time, message = messageFromBackend }
            }
    in
    case messageFromBackend.responseToUser of
        FrontendBackendInterface.SeeingLobby seeingLobby ->
            { stateBefore | lastSeeingLobby = Just { time = stateBefore.time, message = seeingLobby } }
                |> updateAddingToConversationHistory
                    { currentUserId = messageFromBackend.currentUserId, backendPosixTimeMilli = messageFromBackend.currentPosixTimeMilli }
                    seeingLobby.conversationHistory

        FrontendBackendInterface.ReadUserProfile userProfile ->
            let
                maybeUserId =
                    case originatingRequest of
                        FrontendBackendInterface.ReadUserProfileRequest userId ->
                            Just userId

                        FrontendBackendInterface.ChooseNameRequest _ ->
                            messageFromBackend.currentUserId

                        _ ->
                            Nothing

                usersProfilesReads =
                    case maybeUserId of
                        Nothing ->
                            stateBefore.usersProfilesReads

                        Just requestUserId ->
                            stateBefore.usersProfilesReads
                                |> Dict.insert requestUserId { time = stateBefore.time, userProfile = userProfile }
            in
            { stateBefore | usersProfilesReads = usersProfilesReads }

        FrontendBackendInterface.MessageToUser messageToUser ->
            stateBefore
                |> updateAddingToConversationHistory
                    { currentUserId = messageFromBackend.currentUserId, backendPosixTimeMilli = messageFromBackend.currentPosixTimeMilli }
                    [ { origin = Conversation.FromSystem
                      , message = messageToUser
                      , posixTimeMilli = messageFromBackend.currentPosixTimeMilli
                      }
                    ]


updateAddingToConversationHistory : { currentUserId : Maybe UserId, backendPosixTimeMilli : Int } -> List Conversation.Event -> State -> State
updateAddingToConversationHistory { currentUserId, backendPosixTimeMilli } eventsToAddToHistory stateBefore =
    let
        eventRepresentsMessageToAdd historyEvent =
            case currentUserId of
                Nothing ->
                    False

                Just selfUserId ->
                    ((backendPosixTimeMilli - historyEvent.posixTimeMilli) < addMessageWaitTimeMaxMilli)
                        && (historyEvent.message == Conversation.LeafPlainText stateBefore.messageToAddText)
                        && (historyEvent.origin == Conversation.FromUser { userId = selfUserId })

        history =
            eventsToAddToHistory |> List.foldr addEventIntoHistory stateBefore.conversationHistory

        historyContainsMessageToAdd =
            history |> List.any eventRepresentsMessageToAdd

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
        viewConfiguration =
            { usersProfiles = state.usersProfilesReads |> Dict.map (\_ read -> read.userProfile) }

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
            [ [ nameSection |> Html.span [], historyEventSeparatorBetweenOriginAndText ] |> placedVerticallyCentered
            , [] |> Html.input messageTextboxAttributes
            , sendMessageButton
            ]
                |> Html.div [ HA.style "display" "flex", HA.style "flex-direction" "row", HA.style "margin" "2px" ]

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
            case state.lastSeeingLobby of
                Just lastSeeingLobby ->
                    lastSeeingLobby.message.usersOnline
                        |> List.map
                            (\userId -> Conversation.FromUser { userId = userId } |> viewDialogEventOrigin viewConfiguration state)
                        |> List.map (List.singleton >> Html.div [])

                Nothing ->
                    case state.lastRequestToBackendResult of
                        Nothing ->
                            [ "Connecting to backend..." |> Html.text ]

                        Just _ ->
                            [ "❌ Connection error" |> Html.text ]

        peopleOnlineView =
            peopleOnlineViewBeforeStyle |> Html.div (htmlAttributesStyles peopleOnlineViewStyle)

        peopleOnlineColumn =
            [ peopleOnlineCaption, peopleOnlineView ]
                |> Html.div (htmlAttributesStyles [ ( "display", "flex" ), ( "flex-direction", "column" ), ( "width", "25%" ), ( "padding", "4px" ) ])

        nameSection =
            case state.editingChosenName of
                Just editingChosenName ->
                    let
                        keyDownEventHandler =
                            HE.on "keydown"
                                (HE.keyCode
                                    |> Json.Decode.andThen
                                        (\keyCode ->
                                            if keyCode == 13 then
                                                Json.Decode.succeed EnterKeyDownOnChooseNameTextbox

                                            else if keyCode == 27 then
                                                Json.Decode.succeed UserInputStopEditChosenName

                                            else
                                                Json.Decode.fail "Another key code"
                                        )
                                )

                        chosenNameTextboxAttributes =
                            [ HA.id chosenNameTextboxId
                            , HA.value editingChosenName
                            , HA.placeholder "Enter your name here"
                            , HE.onInput UserInputContinueEditChosenName
                            , keyDownEventHandler
                            , HE.onBlur UserInputCompleteEnterChosenName
                            ]
                                ++ htmlAttributesStyles chosenNameTextboxStyle
                    in
                    [ { userId = state |> getCurrentUserId |> Maybe.withDefault -1 }
                        |> Conversation.FromUser
                        |> viewDialogEventOrigin { viewConfiguration | usersProfiles = Dict.empty } state
                    , [] |> Html.input chosenNameTextboxAttributes
                    ]

                Nothing ->
                    [ { userId = state |> getCurrentUserId |> Maybe.withDefault -1 }
                        |> Conversation.FromUser
                        |> viewDialogEventOrigin viewConfiguration state
                    , startEditChosenNameButton
                        { currentName =
                            state.editingChosenName
                                |> Maybe.withDefault
                                    (state.editingChosenName
                                        |> Maybe.withDefault (getCurrentUserName viewConfiguration state |> Maybe.withDefault "")
                                    )
                        , showGuide = state.showChangeNameGuide
                        }
                    ]

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

        playAudioHtml =
            state.playAudio |> PlayAudio.renderHtml

        body =
            [ Visuals.globalStylesHtmlElement
            , [ [ appDescriptionHtml ] |> Html.div [ HA.style "margin" "1em", HA.style "overflow" "scroll", HA.style "height" "20%" ]
              , [ chatHtml ] |> Html.div [ HA.style "flex" "1", HA.style "margin" "1em" ]
              , playAudioHtml
              ]
                |> Html.div
                    [ HA.style "margin" "0"
                    , HA.style "height" "99vh"
                    , HA.style "display" "flex"
                    , HA.style "flex-direction" "column"
                    ]
            ]
    in
    { title = "Rich chat room example app", body = body }


appDescriptionHtml : Html.Html a
appDescriptionHtml =
    CompilationInterface.SourceFiles.file____README_md.utf8
        |> Markdown.Parser.parse
        |> Result.map
            (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer
                >> Result.withDefault [ "Failed to render markdown" |> Html.text ]
            )
        |> Result.withDefault [ "Failed to parse markdown" |> Html.text ]
        |> Html.div []


getCurrentUserName : ViewConfiguration -> State -> Maybe String
getCurrentUserName viewConfiguration =
    getCurrentUserId
        >> Maybe.andThen
            (\currentUserId -> viewConfiguration.usersProfiles |> Dict.get currentUserId |> Maybe.map .chosenName)


getCurrentUserId : State -> Maybe UserId
getCurrentUserId =
    .lastMessageFromBackend >> Maybe.andThen (.message >> .currentUserId)


startEditChosenNameButton : { currentName : String, showGuide : Bool } -> Html.Html Event
startEditChosenNameButton { currentName, showGuide } =
    let
        buttonStyle =
            [ ( "cursor", "pointer" )
            , ( "transform", "scale(-1,1)" )
            , ( "display", "inline-block" )
            , ( "margin-left", "10px" )
            ]

        button =
            [ "✎" |> Html.text ]
                |> Html.span
                    ([ HA.tabindex 0
                     , HE.onClick (UserInputContinueEditChosenName currentName)
                     , HE.onMouseEnter (UserInputShowChangeNameGuide True)
                     , HE.onMouseLeave (UserInputShowChangeNameGuide False)
                     ]
                        ++ htmlAttributesStyles buttonStyle
                    )

        guideWidthInEm =
            12

        guidePopupStyle =
            [ ( "z-index", "1" )
            , ( "position", "absolute" )
            , ( "text-align", "center" )
            , ( "width", (guideWidthInEm |> String.fromFloat) ++ "em" )
            , ( "margin-left", "-" ++ ((guideWidthInEm / 2) |> String.fromFloat) ++ "em" )
            , ( "bottom", "2em" )
            , ( "left", "50%" )
            , ( "padding", "0.4em" )
            , ( "color", "whitesmoke" )
            , ( "background-color", "#111" )
            ]

        guide =
            if showGuide then
                [ "Click here to change your display name" |> Html.text ]
                    |> Html.span (htmlAttributesStyles guidePopupStyle)

            else
                "" |> Html.text
    in
    [ button
    , guide
    ]
        |> Html.span [ HA.style "position" "relative" ]


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


placedVerticallyCentered : List (Html.Html a) -> Html.Html a
placedVerticallyCentered =
    Html.span (htmlAttributesStyles [ ( "display", "flex" ), ( "justify-content", "center" ), ( "align-items", "center" ) ])


historyEventSeparatorBetweenOriginAndText : Html.Html a
historyEventSeparatorBetweenOriginAndText =
    [ " : " |> Html.text ] |> Html.span []


chosenNameTextboxStyle : HtmlStyle
chosenNameTextboxStyle =
    textboxStyle
        ++ [ ( "color", playerNameColor ) ]


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
            [ "🤖 System" |> Html.text ] |> Html.span []


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


chosenNameTextboxId : String
chosenNameTextboxId =
    "chosen-name-input"
