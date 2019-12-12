module Backend.Main exposing
    ( CustomType(..)
    , RecursiveType(..)
    , State
    , Tuple2
    , Tuple3
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Common
import Dict


type alias State =
    { httpRequestsCount : Int
    , lastHttpRequests : List InterfaceToHost.HttpRequestEvent
    , tuple2 : Tuple2
    , tuple3 : Tuple3
    , custom_type : CustomType
    , recursive_type : RecursiveType
    , bool : Bool
    , maybe : Maybe String
    , result : Result String Int
    , dict : Dict.Dict Int String
    , empty_record : {}
    , empty_tuple : ()
    }


type RecursiveType
    = TagTerminate Int
    | TagRecurse RecursiveType


type alias Tuple2 =
    ( Int, String )


type alias Tuple3 =
    ( Int, String, Int )


type CustomType
    = CustomTagA String
    | CustomTagB Int


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
                        , bodyAsString =
                            Just
                                (Common.describeApp
                                    ++ "\nI received "
                                    ++ (state.httpRequestsCount |> String.fromInt)
                                    ++ " HTTP requests."
                                )
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
    , custom_type = CustomTagA "tag A"
    , recursive_type = TagRecurse (TagRecurse (TagRecurse (TagTerminate 4)))
    , bool = True
    , maybe = Just "Hello"
    , result = Ok 42
    , dict = [ ( 100, "A" ), ( 101, "B" ) ] |> Dict.fromList
    , empty_record = {}
    , empty_tuple = ()
    }
