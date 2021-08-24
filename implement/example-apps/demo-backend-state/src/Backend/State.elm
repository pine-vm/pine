module Backend.State exposing
    ( CustomType(..)
    , CustomTypeWithTypeParameter(..)
    , OpaqueCustomType
    , RecursiveType(..)
    , State
    , valueForOpaqueCustomType
    )

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
    , record_instance : RecordAliasWithTypeParameter String
    , listDict :
        ListDict.Dict
            { orig : Int
            , dest : Int
            }
            String
    , bytes : Bytes.Bytes
    }


type RecursiveType
    = TagTerminate Int
    | TagRecurse RecursiveType


type OpaqueCustomType
    = OpaqueCustomType String


valueForOpaqueCustomType : String -> OpaqueCustomType
valueForOpaqueCustomType =
    OpaqueCustomType


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


type alias RecordAliasWithTypeParameter typeParamInRecord =
    { field_a : Int
    , field_parameterized : typeParamInRecord
    }
