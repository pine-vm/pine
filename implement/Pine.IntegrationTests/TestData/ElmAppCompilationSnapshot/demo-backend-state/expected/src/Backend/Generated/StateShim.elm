module Backend.Generated.StateShim exposing (..)

import Backend.Generated.StateShimTypes exposing (..)
import Dict
import Json.Decode
import Json.Encode


type alias ExposedFunctionHandler appState =
    ApplyFunctionArguments (Maybe appState) -> Result String ( Maybe appState, Maybe Json.Encode.Value )


type alias ExposedFunctionRecord appState =
    { description : ExposedFunctionDescription
    , handler : ExposedFunctionHandler appState
    }


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

