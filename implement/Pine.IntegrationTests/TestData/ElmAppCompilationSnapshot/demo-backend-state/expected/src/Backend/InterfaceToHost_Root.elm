module Backend.InterfaceToHost_Root exposing (..)

import Backend.Main
import Backend.InterfaceToHost_Root.Generated_JsonConverters
import Dict
import Set
import Array
import Json.Decode
import Json.Encode
import Bytes
import Bytes.Decode
import Bytes.Encode
import Backend.State
import ListDict
import Backend.MigrateState
import Backend.Generated.StateShimTypes
import Backend.Generated.StateShim
import Platform
import Backend.Generated.StateShim exposing (StateShimConfig, StateShimState)


type alias AppState =
    ({ httpRequestsCount : Int, lastHttpRequests : ( List { httpRequestId : String, posixTimeMilli : Int, requestContext : { clientAddress : ( Maybe String ) }, request : { method : String, uri : String, body : ( Maybe Bytes.Bytes ), headers : ( List { name : String, values : ( List String ) } ) } } ), tuple2 : (Int, String), tuple3 : (Int, String, Int), list_custom_type : ( List Backend.State.ChoiceType ), opaque_custom_type : Backend.State.OpaqueChoiceType, recursive_type : Backend.State.RecursiveType, bool : Bool, maybe : ( Maybe String ), result : ( Result String Int ), set : ( Set.Set Int ), dict : ( Dict.Dict Int String ), empty_record : {  }, empty_tuple : (), choiceTypeInstance : ( Backend.State.ChoiceTypeWithTypeParameter Int ), record_instance_string : { field_a : Int, field_parameterized : String, field_parameterized_maybe : ( Maybe String ), field_parameterized_tuple : (String, String), field_parameterized_record : { field_int : Int, field_parameterized : String } }, record_instance_int : { field_a : Int, field_parameterized : Int, field_parameterized_maybe : ( Maybe Int ), field_parameterized_tuple : (Int, Int), field_parameterized_record : { field_int : Int, field_parameterized : Int } }, listDict : ( ListDict.Dict { orig : Int, dest : Int } String ), bytes : Bytes.Bytes, array_primitive : ( Array.Array Int ) })

jsonDecodeAppState =
    Backend.InterfaceToHost_Root.Generated_JsonConverters.jsonDecode_51061477


jsonDecodeMigratePreviousState =
    Backend.InterfaceToHost_Root.Generated_JsonConverters.jsonDecode_51061477


jsonEncodeAppState =
    Backend.InterfaceToHost_Root.Generated_JsonConverters.jsonEncode_51061477


config_exposedFunctions =
    [
    ]
    |> Dict.fromList


config_init = Backend.Main.webServiceMain.init


config_subscriptions = Backend.Main.webServiceMain.subscriptions