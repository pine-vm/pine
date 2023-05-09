module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Array
import Backend.State exposing (ChoiceType(..), ChoiceTypeWithTypeParameter(..), RecursiveType(..), valueForOpaqueChoiceType)
import Base64
import Bytes.Encode
import Common
import CompilationInterface.ElmMake
import CompilationInterface.GenerateJsonConverters
import Dict
import Json.Encode
import ListDict
import Platform.WebService
import Set
import Url


type alias State =
    Backend.State.State


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( initState, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent :
    Platform.WebService.HttpRequestEventStruct
    -> State
    -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        state =
            { stateBefore
                | httpRequestsCount = stateBefore.httpRequestsCount + 1
                , lastHttpRequests = httpRequestEvent :: stateBefore.lastHttpRequests |> List.take 4
            }

        httpResponse =
            if
                httpRequestEvent.request.uri
                    |> Url.fromString
                    |> Maybe.map urlLeadsToFrontendHtmlDocument
                    |> Maybe.withDefault False
            then
                { statusCode = 200
                , bodyAsBase64 = Just CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm.debug.base64
                , headersToAdd = []
                }

            else
                { statusCode = 200
                , bodyAsBase64 =
                    [ Common.describeApp
                    , "I received "
                        ++ (state.httpRequestsCount |> String.fromInt)
                        ++ " HTTP requests."
                    , "Here is a serialized representation of the backend state:"
                    , state |> CompilationInterface.GenerateJsonConverters.jsonEncodeBackendState |> Json.Encode.encode 4
                    ]
                        |> String.join "\n"
                        |> Bytes.Encode.string
                        |> Bytes.Encode.encode
                        |> Base64.fromBytes
                , headersToAdd = []
                }
    in
    ( state
    , [ Platform.WebService.RespondToHttpRequest
            { httpRequestId = httpRequestEvent.httpRequestId, response = httpResponse }
      ]
    )


urlLeadsToFrontendHtmlDocument : Url.Url -> Bool
urlLeadsToFrontendHtmlDocument url =
    not (url.path == "/api" || (url.path |> String.startsWith "/api/"))


initState : State
initState =
    { httpRequestsCount = 0
    , lastHttpRequests = []
    , tuple2 = ( 123, "second element in tuple A" )
    , tuple3 = ( 456, "second element in tuple B", 789 )
    , list_custom_type =
        [ CustomTagWithoutParameter
        , CustomTagWithOneParameter 4
        , CustomTagWithTwoParameters "test" 11
        , CustomTagWithMaybeInstance Nothing
        , CustomTagWithMaybeInstance (Just 123)
        , CustomTagWithResultInstance (Err "error string")
        , CustomTagWithResultInstance (Ok 678)
        ]
    , opaque_custom_type = valueForOpaqueChoiceType "content"
    , recursive_type = TagRecurse (TagRecurse (TagRecurse (TagTerminate 4)))
    , bool = True
    , maybe = Just "Hello"
    , result = Ok 42
    , set = [ 1, 3, 3, 3, 4, 5 ] |> Set.fromList
    , dict = [ ( 100, "A" ), ( 101, "B" ) ] |> Dict.fromList
    , empty_record = {}
    , empty_tuple = ()
    , choiceTypeInstance = ChoiceTypeWithTypeParameter 4
    , record_instance_string =
        { field_a = 123
        , field_parameterized = "test arg"
        , field_parameterized_maybe = Nothing
        , field_parameterized_tuple = ( "alfa", "beta" )
        , field_parameterized_record = { field_int = 0, field_parameterized = "test" }
        }
    , record_instance_int =
        { field_a = 234
        , field_parameterized = 456
        , field_parameterized_maybe = Just 567
        , field_parameterized_tuple = ( 3, 4 )
        , field_parameterized_record = { field_int = 0, field_parameterized = 78 }
        }
    , listDict =
        [ ( { orig = 1, dest = 3 }, "Edge A" )
        , ( { orig = 3, dest = 4 }, "Edge B" )
        ]
            |> ListDict.fromList
    , bytes = "Hello World!" |> Bytes.Encode.string |> Bytes.Encode.encode
    , array_primitive = Array.fromList [ 0, 1, 3, 4 ]
    }
