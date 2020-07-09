module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Conversation exposing (UserId)
import Dict
import ElmFullstackCompilerInterface.ElmMake
import ElmFullstackCompilerInterface.GenerateJsonCoders as GenerateJsonCoders
import ElmFullstackCompilerInterface.SourceFiles
import FrontendBackendInterface
import Json.Decode
import Json.Encode
import SHA1
import Url


type alias State =
    { posixTimeMilli : Int
    , conversationHistory : List Conversation.Event
    , usersSessions : Dict.Dict String UserSessionState
    , usersProfiles : Dict.Dict UserId UserProfile
    , usersLastSeen : Dict.Dict UserId { posixTime : Int }
    }


type alias UserSessionState =
    { beginPosixTime : Int
    , clientAddress : Maybe String
    , userId : Maybe UserId
    , lastUsePosixTime : Int
    }


type alias UserProfile =
    { chosenName : String
    }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            processEventHttpRequest httpRequestEvent { stateBefore | posixTimeMilli = httpRequestEvent.posixTimeMilli }

        InterfaceToHost.TaskCompleteEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )

        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )


processEventHttpRequest : InterfaceToHost.HttpRequestEventStructure -> State -> ( State, InterfaceToHost.AppEventResponse )
processEventHttpRequest httpRequestEvent stateBefore =
    let
        respondWithFrontendHtmlDocument { enableInspector } =
            ( stateBefore
            , InterfaceToHost.passiveAppEventResponse
                |> InterfaceToHost.withCompleteHttpResponsesAdded
                    [ { httpRequestId = httpRequestEvent.httpRequestId
                      , response =
                            { statusCode = 200
                            , bodyAsBase64 =
                                Just
                                    (if enableInspector then
                                        ElmFullstackCompilerInterface.ElmMake.elm_make__debug__base64____src_FrontendWeb_Main_elm

                                     else
                                        ElmFullstackCompilerInterface.ElmMake.elm_make__base64____src_FrontendWeb_Main_elm
                                    )
                            , headersToAdd = []
                            }
                      }
                    ]
            )
    in
    case
        httpRequestEvent.request.uri
            |> Url.fromString
            |> Maybe.andThen FrontendBackendInterface.routeFromUrl
    of
        Nothing ->
            respondWithFrontendHtmlDocument { enableInspector = False }

        Just FrontendBackendInterface.FrontendWithInspectorRoute ->
            respondWithFrontendHtmlDocument { enableInspector = True }

        Just FrontendBackendInterface.ApiRoute ->
            case
                httpRequestEvent.request.bodyAsBase64
                    |> Maybe.map (Base64.toBytes >> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string") >> Maybe.withDefault "Failed to decode from base64")
                    |> Maybe.withDefault "Missing HTTP body"
                    |> Json.Decode.decodeString GenerateJsonCoders.jsonDecodeRequestFromUser
            of
                Err decodeError ->
                    let
                        httpResponse =
                            { httpRequestId = httpRequestEvent.httpRequestId
                            , response =
                                { statusCode = 400
                                , bodyAsBase64 =
                                    ("Failed to decode request: " ++ (decodeError |> Json.Decode.errorToString))
                                        |> encodeStringToBytes
                                        |> Base64.fromBytes
                                , headersToAdd = []
                                }
                            }
                    in
                    ( stateBefore
                    , InterfaceToHost.passiveAppEventResponse
                        |> InterfaceToHost.withCompleteHttpResponsesAdded [ httpResponse ]
                    )

                Ok requestFromUser ->
                    let
                        ( userSessionId, userSessionStateBefore ) =
                            userSessionIdAndStateFromRequestOrCreateNew httpRequestEvent.request stateBefore

                        userSessionState =
                            userSessionStateBefore

                        usersLastSeen =
                            case userSessionStateBefore.userId of
                                Nothing ->
                                    stateBefore.usersLastSeen

                                Just userId ->
                                    stateBefore.usersLastSeen
                                        |> Dict.insert userId { posixTime = stateBefore.posixTimeMilli // 1000 }

                        usersSessions =
                            stateBefore.usersSessions
                                |> Dict.insert userSessionId userSessionState

                        ( state, responseToUser ) =
                            processMessageFromClient
                                { posixTimeMilli = stateBefore.posixTimeMilli, userId = userSessionStateBefore.userId, loginUrl = "" }
                                requestFromUser
                                { stateBefore
                                    | usersSessions = usersSessions
                                    , usersLastSeen = usersLastSeen
                                }

                        responseToClient =
                            { currentPosixTimeMilli = state.posixTimeMilli
                            , currentUserId = userSessionState.userId
                            , responseToUser = responseToUser
                            }

                        httpResponse =
                            { httpRequestId = httpRequestEvent.httpRequestId
                            , response =
                                { statusCode = 200
                                , bodyAsBase64 =
                                    responseToClient
                                        |> GenerateJsonCoders.jsonEncodeMessageToClient
                                        |> Json.Encode.encode 0
                                        |> encodeStringToBytes
                                        |> Base64.fromBytes
                                , headersToAdd = []
                                }
                                    |> addCookieUserSessionId userSessionId
                            }
                    in
                    ( state
                    , InterfaceToHost.passiveAppEventResponse
                        |> InterfaceToHost.withCompleteHttpResponsesAdded [ httpResponse ]
                    )

        Just (FrontendBackendInterface.StaticContentRoute contentName) ->
            let
                httpResponse =
                    case availableStaticContent |> Dict.get contentName of
                        Nothing ->
                            { statusCode = 404
                            , bodyAsBase64 =
                                ("Found no content with the name " ++ contentName)
                                    |> encodeStringToBytes
                                    |> Base64.fromBytes
                            , headersToAdd = []
                            }

                        Just content ->
                            { statusCode = 200
                            , bodyAsBase64 = content |> Base64.fromBytes
                            , headersToAdd = [ { name = "Cache-Control", values = [ "public, max-age=31536000" ] } ]
                            }
            in
            ( stateBefore
            , InterfaceToHost.passiveAppEventResponse
                |> InterfaceToHost.withCompleteHttpResponsesAdded
                    [ { httpRequestId = httpRequestEvent.httpRequestId
                      , response = httpResponse
                      }
                    ]
            )


availableStaticContent : Dict.Dict String Bytes.Bytes
availableStaticContent =
    [ ElmFullstackCompilerInterface.SourceFiles.file____static_chat_message_added_0_mp3 ]
        |> List.map (\content -> ( content |> FrontendBackendInterface.staticContentFileName, content ))
        |> Dict.fromList


seeingLobbyFromState : State -> FrontendBackendInterface.SeeingLobbyStructure
seeingLobbyFromState state =
    let
        usersOnline =
            state.usersLastSeen
                |> Dict.filter (\_ lastSeen -> state.posixTimeMilli // 1000 - 10 < lastSeen.posixTime)
                |> Dict.keys
    in
    { conversationHistory = state.conversationHistory
    , usersOnline = usersOnline
    }


processMessageFromClient :
    { userId : Maybe Int, posixTimeMilli : Int, loginUrl : String }
    -> FrontendBackendInterface.RequestFromUser
    -> State
    -> ( State, FrontendBackendInterface.ResponseToUser )
processMessageFromClient context requestFromUser stateBefore =
    let
        ( state, responseToUser ) =
            case requestFromUser of
                FrontendBackendInterface.ShowUpRequest ->
                    ( stateBefore, stateBefore |> seeingLobbyFromState |> FrontendBackendInterface.SeeingLobby )

                FrontendBackendInterface.AddTextMessageRequest message ->
                    case context.userId of
                        Nothing ->
                            ( stateBefore
                            , FrontendBackendInterface.MessageToUser
                                ([ Conversation.LeafPlainText "⚠️ To add a message, please sign in first at "
                                 , Conversation.LeafLinkToUrl { url = context.loginUrl }
                                 ]
                                    |> Conversation.SequenceOfNodes
                                )
                            )

                        Just userId ->
                            if hasUserExhaustedRateLimitToAddMessage stateBefore { userId = userId } then
                                ( stateBefore
                                , FrontendBackendInterface.MessageToUser (Conversation.LeafPlainText "❌ Too many messages – Maximum sending rate exceeded.")
                                )

                            else
                                let
                                    conversationHistory =
                                        { posixTimeMilli = stateBefore.posixTimeMilli
                                        , origin = Conversation.FromUser { userId = userId }
                                        , message = Conversation.LeafPlainText message
                                        }
                                            :: stateBefore.conversationHistory

                                    stateAfterAddingMessage =
                                        { stateBefore | conversationHistory = conversationHistory }
                                in
                                ( stateAfterAddingMessage
                                , stateAfterAddingMessage |> seeingLobbyFromState |> FrontendBackendInterface.SeeingLobby
                                )

                FrontendBackendInterface.ChooseNameRequest chosenName ->
                    case context.userId of
                        Nothing ->
                            ( stateBefore
                            , FrontendBackendInterface.MessageToUser
                                ([ Conversation.LeafPlainText "⚠️ To choose a name, please sign in first at "
                                 , Conversation.LeafLinkToUrl { url = context.loginUrl }
                                 ]
                                    |> Conversation.SequenceOfNodes
                                )
                            )

                        Just userId ->
                            let
                                userProfileBefore =
                                    stateBefore.usersProfiles |> Dict.get userId |> Maybe.withDefault initUserProfile

                                userProfile =
                                    { userProfileBefore | chosenName = chosenName }

                                usersProfiles =
                                    stateBefore.usersProfiles |> Dict.insert userId userProfile
                            in
                            ( { stateBefore | usersProfiles = usersProfiles }
                            , FrontendBackendInterface.ReadUserProfile userProfile
                            )

                FrontendBackendInterface.ReadUserProfileRequest userId ->
                    ( stateBefore
                    , stateBefore.usersProfiles
                        |> Dict.get userId
                        |> Maybe.withDefault initUserProfile
                        |> FrontendBackendInterface.ReadUserProfile
                    )
    in
    ( state, responseToUser )


initUserProfile : UserProfile
initUserProfile =
    { chosenName = "" }


userSessionIdAndStateFromRequestOrCreateNew : InterfaceToHost.HttpRequestProperties -> State -> ( String, UserSessionState )
userSessionIdAndStateFromRequestOrCreateNew httpRequest state =
    state |> getFirstMatchingUserSessionOrCreateNew (getSessionIdsFromHttpRequest httpRequest)


getFirstMatchingUserSessionOrCreateNew : List String -> State -> ( String, UserSessionState )
getFirstMatchingUserSessionOrCreateNew sessionIds state =
    sessionIds
        |> List.filterMap
            (\requestSessionId ->
                state.usersSessions
                    |> Dict.get requestSessionId
                    |> Maybe.map
                        (\sessionState ->
                            ( requestSessionId, sessionState )
                        )
            )
        |> List.sortBy
            (\( _, sessionState ) ->
                if sessionState.userId == Nothing then
                    1

                else
                    0
            )
        |> List.head
        |> Maybe.withDefault (state |> getNextUserSessionIdAndState)


getSessionIdsFromHttpRequest : InterfaceToHost.HttpRequestProperties -> List String
getSessionIdsFromHttpRequest httpRequest =
    let
        cookies =
            httpRequest.headers
                |> List.filter (\{ name } -> (name |> String.toLower) == "cookie")
                |> List.head
                |> Maybe.map .values
                |> Maybe.withDefault []
                |> List.concatMap (String.split ";")
                |> List.map String.trim

        prefix =
            httpSessionIdCookieName ++ "="
    in
    cookies
        |> List.filter (String.startsWith prefix)
        |> List.map (\sessionIdCookie -> sessionIdCookie |> String.dropLeft (prefix |> String.length))


getNextUserSessionIdAndState : State -> ( String, UserSessionState )
getNextUserSessionIdAndState state =
    let
        posixTime =
            state.posixTimeMilli // 1000
    in
    ( state |> getNextUserSessionId
    , { userId = Just ((state |> getLastUserId |> Maybe.withDefault 0) + 1)
      , beginPosixTime = posixTime
      , lastUsePosixTime = posixTime
      , clientAddress = Nothing
      }
    )


getLastUserId : State -> Maybe Int
getLastUserId state =
    [ state.usersProfiles |> Dict.keys
    , state.usersSessions |> Dict.values |> List.filterMap .userId
    ]
        |> List.concat
        |> List.maximum


getNextUserSessionId : State -> String
getNextUserSessionId state =
    let
        otherSessionsIds =
            state.usersSessions |> Dict.keys

        source =
            (otherSessionsIds |> String.concat) ++ (state.posixTimeMilli |> String.fromInt)
    in
    source |> SHA1.fromString |> SHA1.toHex |> String.left 30


hasUserExhaustedRateLimitToAddMessage : State -> { userId : Int } -> Bool
hasUserExhaustedRateLimitToAddMessage state { userId } =
    let
        addedMessagesAges =
            state.conversationHistory
                |> List.filterMap
                    (\event ->
                        case event.origin of
                            Conversation.FromSystem ->
                                Nothing

                            Conversation.FromUser fromUser ->
                                if fromUser.userId /= userId then
                                    Nothing

                                else
                                    Just event.posixTimeMilli
                    )
                |> List.map (\addedMessagePosixTimeMilli -> (state.posixTimeMilli - addedMessagePosixTimeMilli) // 1000)

        numberOfMessagesWithinAge ageInSeconds =
            addedMessagesAges
                |> List.filter (\messageAge -> messageAge <= ageInSeconds)
                |> List.length
    in
    userAddMessageRateLimits
        |> List.any (\limit -> limit.numberOfMessages <= numberOfMessagesWithinAge limit.timespanInSeconds)


userAddMessageRateLimits : List { timespanInSeconds : Int, numberOfMessages : Int }
userAddMessageRateLimits =
    [ { timespanInSeconds = 3, numberOfMessages = 2 }
    , { timespanInSeconds = 10, numberOfMessages = 3 }
    , { timespanInSeconds = 60, numberOfMessages = 8 }
    , { timespanInSeconds = 60 * 10, numberOfMessages = 50 }
    ]


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


encodeStringToBytes : String -> Bytes.Bytes
encodeStringToBytes =
    Bytes.Encode.string >> Bytes.Encode.encode


addCookieUserSessionId : String -> InterfaceToHost.HttpResponse -> InterfaceToHost.HttpResponse
addCookieUserSessionId userSessionId httpResponse =
    let
        cookieHeader =
            { name = "Set-Cookie", values = [ httpSessionIdCookieName ++ "=" ++ userSessionId ++ "; Path=/; Max-Age=3600" ] }
    in
    { httpResponse | headersToAdd = cookieHeader :: httpResponse.headersToAdd }


httpSessionIdCookieName : String
httpSessionIdCookieName =
    "sessionid"


interfaceToHost_initState : State
interfaceToHost_initState =
    { posixTimeMilli = 0
    , conversationHistory = []
    , usersProfiles = Dict.empty
    , usersSessions = Dict.empty
    , usersLastSeen = Dict.empty
    }
