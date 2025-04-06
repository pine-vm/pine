module Backend.Generated.StateShim exposing (..)

import Backend.Generated.StateShimTypes exposing (..)
import Dict
import Json.Decode
import Json.Encode


type alias StateShimConfig appState appStateLessShim =
    { exposedFunctions : Dict.Dict String (ExposedFunctionRecord appState)
    , jsonEncodeAppState : appStateLessShim -> Json.Encode.Value
    , jsonDecodeAppState : Json.Decode.Decoder appStateLessShim
    , initAppShimState : appStateLessShim -> appState
    , appStateLessShim : appState -> appStateLessShim
    }


type alias StateShimState appState =
    { branches : Dict.Dict String appState }


type alias ExposedFunctionHandler appState =
    ApplyFunctionArguments (Maybe appState) -> Result String ( Maybe appState, Maybe Json.Encode.Value )


type alias ExposedFunctionRecord appState =
    { description : ExposedFunctionDescription
    , handler : ExposedFunctionHandler appState
    }


init : StateShimState appState
init =
    { branches = Dict.empty }


processEvent :
    StateShimConfig appState appStateLessShim
    -> StateShimRequest
    -> StateShimState appState
    -> ( StateShimState appState, StateShimResponse )
processEvent config hostEvent stateBefore =
    case hostEvent of
        ListExposedFunctionsShimRequest ->
            ( stateBefore
            , config.exposedFunctions
                |> Dict.toList
                |> List.map (Tuple.mapSecond .description)
                |> List.map
                    (\( functionName, functionDescription ) ->
                        { functionName = functionName, functionDescription = functionDescription }
                    )
                |> ListExposedFunctionsShimResponse
            )


exposedFunctionExpectingSingleArgument :
    Json.Decode.Decoder arg
    -> (arg -> Result String ( Maybe appState, Maybe Json.Encode.Value ))
    -> ExposedFunctionHandler appState
exposedFunctionExpectingSingleArgument argumentDecoder funcAfterDecode genericArguments =
    case genericArguments.serializedArgumentsJson of
        [ singleArgumentJson ] ->
            case Json.Decode.decodeValue argumentDecoder singleArgumentJson of
                Err err ->
                    Err ("Failed to JSON decode argument: " ++ Json.Decode.errorToString err)

                Ok argument ->
                    funcAfterDecode argument

        serializedArgumentsJson ->
            Err
                ("Unexpected number of arguments: "
                    ++ String.fromInt (List.length serializedArgumentsJson)
                    ++ " instead of 1"
                )


exposedFunctionExpectingSingleArgumentAndAppState :
    Json.Decode.Decoder arg
    -> (arg -> appState -> Result String ( Maybe appState, Maybe Json.Encode.Value ))
    -> ExposedFunctionHandler appState
exposedFunctionExpectingSingleArgumentAndAppState argumentDecoder funcAfterDecode genericArguments =
    case genericArguments.stateArgument of
        Nothing ->
            Err "Type mismatch: Missing state argument"

        Just appState ->
            case genericArguments.serializedArgumentsJson of
                [ singleArgumentJson ] ->
                    case Json.Decode.decodeValue argumentDecoder singleArgumentJson of
                        Err err ->
                            Err ("Failed to JSON decode argument: " ++ Json.Decode.errorToString err)

                        Ok argument ->
                            funcAfterDecode argument appState

                serializedArgumentsJson ->
                    Err
                        ("Unexpected number of arguments: "
                            ++ String.fromInt (List.length serializedArgumentsJson)
                            ++ " instead of 1"
                        )

