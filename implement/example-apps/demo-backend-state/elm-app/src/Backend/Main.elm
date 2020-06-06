module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Backend.State exposing (CustomType(..), CustomTypeWithTypeParameter(..), RecursiveType(..), valueForOpaqueCustomType)
import Bytes.Encode
import Common
import Dict
import ElmFullstackCompilerInterface.ElmMakeFrontendWeb
import ElmFullstackCompilerInterface.GenerateJsonCoders
import Json.Encode
import ListDict
import Set
import Url


type alias State =
    Backend.State.State


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
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
                        , body = Just ElmFullstackCompilerInterface.ElmMakeFrontendWeb.elm_make_frontendWeb_html_debug
                        , headersToAdd = []
                        }

                    else
                        { statusCode = 200
                        , body =
                            [ Common.describeApp
                            , "I received "
                                ++ (state.httpRequestsCount |> String.fromInt)
                                ++ " HTTP requests."
                            , "Here is a serialized representation of the backend state:"
                            , state |> ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeBackendState |> Json.Encode.encode 4
                            ]
                                |> String.join "\n"
                                |> Bytes.Encode.string
                                |> Bytes.Encode.encode
                                |> Just
                        , headersToAdd = []
                        }
            in
            ( state
            , [ { httpRequestId = httpRequestEvent.httpRequestId, response = httpResponse }
                    |> InterfaceToHost.CompleteHttpResponse
              ]
            )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


urlLeadsToFrontendHtmlDocument : Url.Url -> Bool
urlLeadsToFrontendHtmlDocument url =
    not (url.path == "/api" || (url.path |> String.startsWith "/api/"))


interfaceToHost_initState : State
interfaceToHost_initState =
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
    , opaque_custom_type = valueForOpaqueCustomType "content"
    , recursive_type = TagRecurse (TagRecurse (TagRecurse (TagTerminate 4)))
    , bool = True
    , maybe = Just "Hello"
    , result = Ok 42
    , set = [ 1, 3, 3, 3, 4, 5 ] |> Set.fromList
    , dict = [ ( 100, "A" ), ( 101, "B" ) ] |> Dict.fromList
    , empty_record = {}
    , empty_tuple = ()
    , customTypeInstance = CustomTypeWithTypeParameter 4
    , listDict =
        [ ( { orig = 1, dest = 3 }, "Edge A" )
        , ( { orig = 3, dest = 4 }, "Edge B" )
        ]
            |> ListDict.fromList
    , bytes = "Hello World!" |> Bytes.Encode.string |> Bytes.Encode.encode
    }
