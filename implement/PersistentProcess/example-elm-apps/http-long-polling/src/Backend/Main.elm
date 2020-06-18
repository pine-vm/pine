module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Bytes
import Bytes.Encode


type alias State =
    { posixTimeMilli : Int
    , httpRequestsToRespondTo : List InterfaceToHost.HttpRequestEvent
    }


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                state =
                    { stateBefore
                        | posixTimeMilli = httpRequestEvent.posixTimeMilli
                        , httpRequestsToRespondTo = httpRequestEvent :: stateBefore.httpRequestsToRespondTo
                    }
            in
            state |> updateForHttpResponses

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )

        InterfaceToHost.ArrivedAtTimeEvent { posixTimeMilli } ->
            { stateBefore | posixTimeMilli = posixTimeMilli } |> updateForHttpResponses


{-| TODO: 2020-06-18 Clean up interface to host:
Why have types supporting to specify more than one time to get notified? This seems redundant.
Also, HTTP responses are expected to move into Tasks, as mentioned earlier.
-}
updateForHttpResponses : State -> ( State, List InterfaceToHost.ProcessRequest )
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
                            , body =
                                ("Completed in " ++ (ageInMilliseconds |> String.fromInt) ++ " milliseconds.")
                                    |> encodeStringToBytes
                                    |> Just
                            , headersToAdd = []
                            }
                        }
                    )
                |> List.map InterfaceToHost.CompleteHttpResponse
    in
    ( state
    , completeHttpRequestsTasks
        ++ [ InterfaceToHost.NotifyWhenArrivedAtTimeRequest { posixTimeMilli = nextCompletionPosixTimeMilli |> Maybe.withDefault (state.posixTimeMilli + 1000) } ]
    )


completionTimeForHttpRequest : InterfaceToHost.HttpRequestEvent -> { completionPosixTimeMilli : Int }
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
