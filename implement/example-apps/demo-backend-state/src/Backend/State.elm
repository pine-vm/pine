module Backend.State exposing
    ( ChoiceType(..)
    , ChoiceTypeWithTypeParameter(..)
    , OpaqueChoiceType
    , RecursiveType(..)
    , State
    , valueForOpaqueChoiceType
    )

import Array
import Bytes
import Dict
import ElmFullstack
import ListDict
import Set


type alias State =
    { httpRequestsCount : Int
    , lastHttpRequests : List ElmFullstack.HttpRequestEventStruct
    , tuple2 : Tuple2
    , tuple3 : Tuple3
    , list_custom_type : List ChoiceType
    , opaque_custom_type : OpaqueChoiceType
    , recursive_type : RecursiveType
    , bool : Bool
    , maybe : Maybe String
    , result : Result String Int
    , set : Set.Set Int
    , dict : Dict.Dict Int String
    , empty_record : {}
    , empty_tuple : ()
    , choiceTypeInstance : ChoiceTypeWithTypeParameter Int
    , record_instance : RecordAliasWithTypeParameter String
    , listDict :
        ListDict.Dict
            { orig : Int
            , dest : Int
            }
            String
    , bytes : Bytes.Bytes
    , array_primitive : Array.Array Int
    }


type RecursiveType
    = TagTerminate Int
    | TagRecurse RecursiveType


type OpaqueChoiceType
    = OpaqueChoiceType String


valueForOpaqueChoiceType : String -> OpaqueChoiceType
valueForOpaqueChoiceType =
    OpaqueChoiceType


type alias Tuple2 =
    ( Int, String )


type alias Tuple3 =
    ( Int, String, Int )


type ChoiceType
    = CustomTagWithoutParameter
    | CustomTagWithOneParameter Int
    | CustomTagWithTwoParameters String Int
    | CustomTagWithMaybeInstance (Maybe Int)
    | CustomTagWithResultInstance (Result String Int)


type ChoiceTypeWithTypeParameter a
    = ChoiceTypeWithTypeParameter a


type alias RecordAliasWithTypeParameter typeParamInRecord =
    { field_a : Int
    , field_parameterized : typeParamInRecord
    }
