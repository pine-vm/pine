module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Base64
import Bytes
import Bytes.Encode


type alias State =
    { posixTimeMilli : Int
    , httpRequestsToRespondTo : List InterfaceToHost.HttpRequestEventStructure
    }


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            let
                state =
                    { stateBefore
                        | posixTimeMilli = httpRequestEvent.posixTimeMilli
                        , httpRequestsToRespondTo = httpRequestEvent :: stateBefore.httpRequestsToRespondTo
                    }
            in
            state |> updateForHttpResponses

        InterfaceToHost.TaskCompleteEvent _ ->
            stateBefore |> updateForHttpResponses

        InterfaceToHost.ArrivedAtTimeEvent { posixTimeMilli } ->
            { stateBefore | posixTimeMilli = posixTimeMilli } |> updateForHttpResponses


updateForHttpResponses : State -> ( State, InterfaceToHost.AppEventResponse )
updateForHttpResponses state =
    let
        httpRequestsWithCompletionTimes =
            state.httpRequestsToRespondTo
                |> List.map (\requestEvent -> ( requestEvent, completionTimeForHttpRequest requestEvent ))

        nextCompletionPosixTimeMilli =
            httpRequestsWithCompletionTimes
                |> List.map (Tuple.second >> .completionPosixTimeMilli)
                |> List.minimum

        completeHttpRequestsTasks =
            httpRequestsWithCompletionTimes
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
    in
    ( state
    , { completeHttpResponses = completeHttpRequestsTasks
      , notifyWhenArrivedAtTime = Just { posixTimeMilli = nextCompletionPosixTimeMilli |> Maybe.withDefault (state.posixTimeMilli + 1000) }
      , startTasks = []
      }
    )


completionTimeForHttpRequest : InterfaceToHost.HttpRequestEventStructure -> { completionPosixTimeMilli : Int }
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


interfaceToHost_initState : State
interfaceToHost_initState =
    { posixTimeMilli = 0, httpRequestsToRespondTo = [] }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
