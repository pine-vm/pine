module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Bytes
import Bytes.Encode
import Platform.WebService


type alias State =
    { posixTimeMilli : Int
    , httpRequestsToRespondTo : List HttpRequestToRespondTo
    }


type HttpRequestToRespondTo
    = HttpRequestToRespondTo Platform.WebService.HttpRequestEventStruct { completionPosixTimeMilli : Int }


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( initState, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions state =
    let
        nextCompletionPosixTimeMilli : Maybe Int
        nextCompletionPosixTimeMilli =
            state.httpRequestsToRespondTo
                |> List.map
                    (\(HttpRequestToRespondTo _ { completionPosixTimeMilli }) ->
                        completionPosixTimeMilli
                    )
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


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        completionPosixTimeMilli : Int
        completionPosixTimeMilli =
            (completionTimeForHttpRequest httpRequestEvent).completionPosixTimeMilli

        state =
            { stateBefore
                | posixTimeMilli = httpRequestEvent.posixTimeMilli
                , httpRequestsToRespondTo =
                    HttpRequestToRespondTo
                        httpRequestEvent
                        { completionPosixTimeMilli = completionPosixTimeMilli }
                        :: stateBefore.httpRequestsToRespondTo
            }
    in
    updateForHttpResponses state


updateForHttpResponses : State -> ( State, Platform.WebService.Commands State )
updateForHttpResponses stateBefore =
    let
        httpResponses : List Platform.WebService.RespondToHttpRequestStruct
        httpResponses =
            stateBefore.httpRequestsToRespondTo
                |> List.filterMap
                    (\(HttpRequestToRespondTo requestEvent { completionPosixTimeMilli }) ->
                        if completionPosixTimeMilli > stateBefore.posixTimeMilli then
                            Nothing

                        else
                            Just
                                (let
                                    ageInMilliseconds : Int
                                    ageInMilliseconds =
                                        stateBefore.posixTimeMilli - requestEvent.posixTimeMilli
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
                    )

        httpResponsesRequestIds : List String
        httpResponsesRequestIds =
            List.map .httpRequestId httpResponses

        state : State
        state =
            { stateBefore
                | httpRequestsToRespondTo =
                    stateBefore.httpRequestsToRespondTo
                        |> List.filter
                            (\(HttpRequestToRespondTo requestEvent _) ->
                                not (List.member requestEvent.httpRequestId httpResponsesRequestIds)
                            )
            }
    in
    ( state
    , List.map
        Platform.WebService.RespondToHttpRequest
        httpResponses
    )


completionTimeForHttpRequest : Platform.WebService.HttpRequestEventStruct -> { completionPosixTimeMilli : Int }
completionTimeForHttpRequest httpRequest =
    let
        delayMilliseconds : Int
        delayMilliseconds =
            httpRequest.request.headers
                |> List.filterMap
                    (\header ->
                        if header.name == "delay-milliseconds" then
                            List.head header.values

                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0
    in
    { completionPosixTimeMilli = delayMilliseconds + httpRequest.posixTimeMilli }


encodeStringToBytes : String -> Bytes.Bytes
encodeStringToBytes string =
    Bytes.Encode.encode
        (Bytes.Encode.string string)


initState : State
initState =
    { posixTimeMilli = 0, httpRequestsToRespondTo = [] }
