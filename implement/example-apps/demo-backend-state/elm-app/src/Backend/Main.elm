module Backend.Main exposing
    ( State
    , Tuple2
    , Tuple3
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Bytes
import Bytes.Encode
import Common
import Dict
import ListDict
import Set


type alias State =
    { httpRequestsCount : Int
    , lastHttpRequests : List InterfaceToHost.HttpRequestEvent
    , tuple2 : Tuple2
    , tuple3 : Tuple3
    , list_custom_type : List CustomType
    , opaque_custom_type : OpaqueCustomType
    , recursive_type : RecursiveType
    , bool : Bool
    , maybe : Maybe String
    , result : Result String Int
    , set : Set.Set Int
    , dict : Dict.Dict Int String
    , empty_record : {}
    , empty_tuple : ()
    , customTypeInstance : CustomTypeWithTypeParameter Int
    , listDict : ListDict.Dict { orig : Int, dest : Int } String
    , bytes : Bytes.Bytes
    }


type RecursiveType
    = TagTerminate Int
    | TagRecurse RecursiveType


type OpaqueCustomType
    = OpaqueCustomType String


type alias Tuple2 =
    ( Int, String )


type alias Tuple3 =
    ( Int, String, Int )


type CustomType
    = CustomTagWithoutParameter
    | CustomTagWithOneParameter Int
    | CustomTagWithTwoParameters String Int
    | CustomTagWithMaybeInstance (Maybe Int)
    | CustomTagWithResultInstance (Result String Int)


type CustomTypeWithTypeParameter a
    = CustomTypeWithTypeParameter a


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
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , body =
                            [ Common.describeApp
                            , "I received "
                                ++ (state.httpRequestsCount |> String.fromInt)
                                ++ " HTTP requests."
                            ]
                                |> String.join "\n"
                                |> Bytes.Encode.string
                                |> Bytes.Encode.encode
                                |> Just
                        , headersToAdd = []
                        }
                    }
                        |> InterfaceToHost.CompleteHttpResponse
            in
            ( state, [ httpResponse ] )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


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
    , opaque_custom_type = OpaqueCustomType "content"
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
