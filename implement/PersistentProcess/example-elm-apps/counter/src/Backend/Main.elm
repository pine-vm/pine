module Backend.Main exposing
    ( Event
    , State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import Json.Decode
import Platform


type alias State =
    Int


type alias Event =
    { addition : Int }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent serializedEvent stateBefore =
    case serializedEvent |> deserializeEvent of
        Err error ->
            ( stateBefore, "Failed to deserialize event: " ++ error )

        Ok event ->
            let
                state =
                    stateBefore + event.addition
            in
            ( state, state |> String.fromInt )


deserializeEvent : String -> Result String Event
deserializeEvent serializedEvent =
    serializedEvent
        |> Json.Decode.decodeString (Json.Decode.field "addition" Json.Decode.int |> Json.Decode.map Event)
        |> Result.mapError Json.Decode.errorToString


interfaceToHost_serializeState : State -> String
interfaceToHost_serializeState =
    String.fromInt


interfaceToHost_deserializeState : String -> State
interfaceToHost_deserializeState =
    String.toInt >> Maybe.withDefault interfaceToHost_initState


interfaceToHost_initState : State
interfaceToHost_initState =
    0



-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        { init = \_ -> ( interfaceToHost_initState, Cmd.none )
        , update =
            \event stateBefore ->
                interfaceToHost_processEvent event (stateBefore |> interfaceToHost_serializeState |> interfaceToHost_deserializeState) |> Tuple.mapSecond (always Cmd.none)
        , subscriptions = \_ -> Sub.none
        }
