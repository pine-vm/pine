module Backend.InterfaceToHost_Root exposing (..)

import Backend.Main
import Array
import Backend.Generated.StateShim
import Backend.Generated.StateShimTypes
import Backend.InterfaceToHost_Root.Generated_JsonConverters
import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict
import Json.Decode
import Json.Encode
import Set
import Platform
import Backend.Generated.StateShim exposing (StateShimConfig, StateShimState)


type alias AppState =
    ({ posixTimeMilli : Int, volatileProcessesIds : ( Set.Set String ), pendingHttpRequests : ( List { httpRequestId : String, posixTimeMilli : Int, requestContext : { clientAddress : ( Maybe String ) }, request : { method : String, uri : String, body : ( Maybe Bytes.Bytes ), headers : ( List { name : String, values : ( List String ) } ) } } ), pendingTasksForRequestVolatileProcess : ( Dict.Dict String { volatileProcessId : String, startPosixTimeMilli : Int } ) })


type alias AppStateWithPlatformShim =
    AppState


type alias State =
    StateShimState AppStateWithPlatformShim


jsonDecodeAppState =
    Backend.InterfaceToHost_Root.Generated_JsonConverters.jsonDecode_1082357013


jsonEncodeAppState =
    Backend.InterfaceToHost_Root.Generated_JsonConverters.jsonEncode_1082357013


config_exposedFunctions =
    [
    ]
    |> Dict.fromList


config_init = Backend.Main.webServiceMain.init


config_subscriptions = Backend.Main.webServiceMain.subscriptions