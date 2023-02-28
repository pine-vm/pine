module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes
import Bytes.Encode
import Platform.WebServer


type alias State =
    { posixTimeMilli : Int
    , httpRequestsToRespondTo : List Platform.WebServer.HttpRequestEventStruct
    }


backendMain : Platform.WebServer.WebServerConfig State
backendMain =
    { init = ( initState, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebServer.Subscriptions State
subscriptions state =
    let
        nextCompletionPosixTimeMilli =
            state
                |> getHttpRequestsWithCompletionTimes
                |> List.map (Tuple.second >> .completionPosixTimeMilli)
                |> List.minimum
    in
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast =
        Just
            { minimumPosixTimeMilli = nextCompletionPosixTimeMilli |> Maybe.withDefault (state.posixTimeMilli + 1000)
            , update =
                \{ currentPosixTimeMilli } stateBefore ->
                    { stateBefore | posixTimeMilli = currentPosixTimeMilli } |> updateForHttpResponses
            }
    }


updateForHttpRequestEvent : Platform.WebServer.HttpRequestEventStruct -> State -> ( State, Platform.WebServer.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        state =
            { stateBefore
                | posixTimeMilli = httpRequestEvent.posixTimeMilli
                , httpRequestsToRespondTo = httpRequestEvent :: stateBefore.httpRequestsToRespondTo
            }
    in
    state |> updateForHttpResponses


updateForHttpResponses : State -> ( State, Platform.WebServer.Commands State )
updateForHttpResponses state =
    ( state
    , state
        |> getHttpRequestsWithCompletionTimes
        |> List.filter (\( _, { completionPosixTimeMilli } ) -> completionPosixTimeMilli <= state.posixTimeMilli)
        |> List.map Tuple.first
        |> List.map
            (\requestEvent ->
                let
                    ageInMilliseconds =
                        state.posixTimeMilli - requestEvent.posixTimeMilli
                in
                { httpRequestId = requestEvent.httpRequestId
                , response =
                    { statusCode = 200
                    , bodyAsBase64 =
                        ("Completed in " ++ (ageInMilliseconds |> String.fromInt) ++ " milliseconds.")
                            |> encodeStringToBytes
                            |> Base64.fromBytes
                    , headersToAdd = []
                    }
                }
            )
        |> List.map Platform.WebServer.RespondToHttpRequest
    )


getHttpRequestsWithCompletionTimes : State -> List ( Platform.WebServer.HttpRequestEventStruct, { completionPosixTimeMilli : Int } )
getHttpRequestsWithCompletionTimes state =
    state.httpRequestsToRespondTo
        |> List.map (\requestEvent -> ( requestEvent, completionTimeForHttpRequest requestEvent ))


completionTimeForHttpRequest : Platform.WebServer.HttpRequestEventStruct -> { completionPosixTimeMilli : Int }
completionTimeForHttpRequest httpRequest =
    let
        delayMilliseconds =
            httpRequest.request.headers
                |> List.filter (.name >> (==) "delay-milliseconds")
                |> List.filterMap (.values >> List.head)
                |> List.head
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0
    in
    { completionPosixTimeMilli = delayMilliseconds + httpRequest.posixTimeMilli }


encodeStringToBytes : String -> Bytes.Bytes
encodeStringToBytes =
    Bytes.Encode.string >> Bytes.Encode.encode


initState : State
initState =
    { posixTimeMilli = 0, httpRequestsToRespondTo = [] }
