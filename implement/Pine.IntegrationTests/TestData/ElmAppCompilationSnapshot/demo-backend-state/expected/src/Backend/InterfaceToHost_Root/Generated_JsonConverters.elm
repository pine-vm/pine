module Backend.InterfaceToHost_Root.Generated_JsonConverters exposing (..)

import Array
import Backend.State
import Bytes
import Bytes.Decode
import Bytes.Encode
import CompilerGenerated.Base64 as Base64
import Dict
import Json.Decode
import Json.Encode
import ListDict
import Set


jsonEncode_51061477 valueToEncode =
    Json.Encode.object
        [ ( "httpRequestsCount"
          , Json.Encode.int valueToEncode.httpRequestsCount
          )
        , ( "lastHttpRequests"
          , jsonEncode__generic_List (\type_arg -> Json.Encode.object
                [ ( "httpRequestId"
                  , Json.Encode.string type_arg.httpRequestId
                  )
                , ( "posixTimeMilli"
                  , Json.Encode.int type_arg.posixTimeMilli
                  )
                , ( "requestContext"
                  , Json.Encode.object
                        [ ( "clientAddress"
                          , jsonEncode__generic_Maybe (\type_arg_ -> Json.Encode.string type_arg_) type_arg.requestContext.clientAddress
                          )
                        ]
                  )
                , ( "request"
                  , Json.Encode.object
                        [ ( "method"
                          , Json.Encode.string type_arg.request.method
                          )
                        , ( "uri"
                          , Json.Encode.string type_arg.request.uri
                          )
                        , ( "body"
                          , jsonEncode__generic_Maybe (\type_arg_ -> json_encode_Bytes type_arg_) type_arg.request.body
                          )
                        , ( "headers"
                          , jsonEncode__generic_List (\type_arg_ -> Json.Encode.object
                                [ ( "name"
                                  , Json.Encode.string type_arg_.name
                                  )
                                , ( "values"
                                  , jsonEncode__generic_List (\type_arg__ -> Json.Encode.string type_arg__) type_arg_.values
                                  )
                                ]) type_arg.request.headers
                          )
                        ]
                  )
                ]) valueToEncode.lastHttpRequests
          )
        , ( "tuple2"
          , Json.Encode.list identity
                [ Json.Encode.int ((\( item_0, item_1 ) -> item_0) valueToEncode.tuple2)
                , Json.Encode.string ((\( item_0, item_1 ) -> item_1) valueToEncode.tuple2)
                ]
          )
        , ( "tuple3"
          , Json.Encode.list identity
                [ Json.Encode.int ((\( item_0, item_1, item_2 ) -> item_0) valueToEncode.tuple3)
                , Json.Encode.string ((\( item_0, item_1, item_2 ) -> item_1) valueToEncode.tuple3)
                , Json.Encode.int ((\( item_0, item_1, item_2 ) -> item_2) valueToEncode.tuple3)
                ]
          )
        , ( "list_custom_type"
          , jsonEncode__generic_List (\type_arg -> jsonEncode_Backend_State_ChoiceType type_arg) valueToEncode.list_custom_type
          )
        , ( "opaque_custom_type"
          , jsonEncode_Backend_State_OpaqueChoiceType valueToEncode.opaque_custom_type
          )
        , ( "recursive_type"
          , jsonEncode_Backend_State_RecursiveType valueToEncode.recursive_type
          )
        , ( "bool"
          , Json.Encode.bool valueToEncode.bool
          )
        , ( "maybe"
          , jsonEncode__generic_Maybe (\type_arg -> Json.Encode.string type_arg) valueToEncode.maybe
          )
        , ( "result"
          , jsonEncode__generic_Result (\type_arg -> Json.Encode.string type_arg) (\type_arg -> Json.Encode.int type_arg) valueToEncode.result
          )
        , ( "set"
          , jsonEncode__generic_Set (\type_arg -> Json.Encode.int type_arg) valueToEncode.set
          )
        , ( "dict"
          , jsonEncode__generic_Dict (\type_arg -> Json.Encode.int type_arg) (\type_arg -> Json.Encode.string type_arg) valueToEncode.dict
          )
        , ( "empty_record"
          , Json.Encode.object []
          )
        , ( "empty_tuple"
          , Json.Encode.list (always (Json.Encode.object [])) []
          )
        , ( "choiceTypeInstance"
          , jsonEncode_Backend_State_ChoiceTypeWithTypeParameter (\type_arg -> Json.Encode.int type_arg) valueToEncode.choiceTypeInstance
          )
        , ( "record_instance_string"
          , Json.Encode.object
                [ ( "field_a"
                  , Json.Encode.int valueToEncode.record_instance_string.field_a
                  )
                , ( "field_parameterized"
                  , Json.Encode.string valueToEncode.record_instance_string.field_parameterized
                  )
                , ( "field_parameterized_maybe"
                  , jsonEncode__generic_Maybe (\type_arg -> Json.Encode.string type_arg) valueToEncode.record_instance_string.field_parameterized_maybe
                  )
                , ( "field_parameterized_tuple"
                  , Json.Encode.list identity
                        [ Json.Encode.string ((\( item_0, item_1 ) -> item_0) valueToEncode.record_instance_string.field_parameterized_tuple)
                        , Json.Encode.string ((\( item_0, item_1 ) -> item_1) valueToEncode.record_instance_string.field_parameterized_tuple)
                        ]
                  )
                , ( "field_parameterized_record"
                  , Json.Encode.object
                        [ ( "field_int"
                          , Json.Encode.int valueToEncode.record_instance_string.field_parameterized_record.field_int
                          )
                        , ( "field_parameterized"
                          , Json.Encode.string valueToEncode.record_instance_string.field_parameterized_record.field_parameterized
                          )
                        ]
                  )
                ]
          )
        , ( "record_instance_int"
          , Json.Encode.object
                [ ( "field_a"
                  , Json.Encode.int valueToEncode.record_instance_int.field_a
                  )
                , ( "field_parameterized"
                  , Json.Encode.int valueToEncode.record_instance_int.field_parameterized
                  )
                , ( "field_parameterized_maybe"
                  , jsonEncode__generic_Maybe (\type_arg -> Json.Encode.int type_arg) valueToEncode.record_instance_int.field_parameterized_maybe
                  )
                , ( "field_parameterized_tuple"
                  , Json.Encode.list identity
                        [ Json.Encode.int ((\( item_0, item_1 ) -> item_0) valueToEncode.record_instance_int.field_parameterized_tuple)
                        , Json.Encode.int ((\( item_0, item_1 ) -> item_1) valueToEncode.record_instance_int.field_parameterized_tuple)
                        ]
                  )
                , ( "field_parameterized_record"
                  , Json.Encode.object
                        [ ( "field_int"
                          , Json.Encode.int valueToEncode.record_instance_int.field_parameterized_record.field_int
                          )
                        , ( "field_parameterized"
                          , Json.Encode.int valueToEncode.record_instance_int.field_parameterized_record.field_parameterized
                          )
                        ]
                  )
                ]
          )
        , ( "listDict"
          , jsonEncode_ListDict_Dict (\type_arg -> Json.Encode.object
                [ ( "orig"
                  , Json.Encode.int type_arg.orig
                  )
                , ( "dest"
                  , Json.Encode.int type_arg.dest
                  )
                ]) (\type_arg -> Json.Encode.string type_arg) valueToEncode.listDict
          )
        , ( "bytes"
          , json_encode_Bytes valueToEncode.bytes
          )
        , ( "array_primitive"
          , jsonEncode__generic_Array (\type_arg -> Json.Encode.int type_arg) valueToEncode.array_primitive
          )
        ]


jsonDecode_51061477 =
    Json.Decode.succeed (\httpRequestsCount lastHttpRequests tuple2 tuple3 list_custom_type opaque_custom_type recursive_type bool maybe result set dict empty_record empty_tuple choiceTypeInstance record_instance_string record_instance_int listDict bytes array_primitive -> { httpRequestsCount = httpRequestsCount, lastHttpRequests = lastHttpRequests, tuple2 = tuple2, tuple3 = tuple3, list_custom_type = list_custom_type, opaque_custom_type = opaque_custom_type, recursive_type = recursive_type, bool = bool, maybe = maybe, result = result, set = set, dict = dict, empty_record = empty_record, empty_tuple = empty_tuple, choiceTypeInstance = choiceTypeInstance, record_instance_string = record_instance_string, record_instance_int = record_instance_int, listDict = listDict, bytes = bytes, array_primitive = array_primitive })
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "httpRequestsCount"
                [ "HttpRequestsCount" ]
                Json.Decode.int
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "lastHttpRequests"
                [ "LastHttpRequests" ]
                ( jsonDecode__generic_List (Json.Decode.succeed (\httpRequestId posixTimeMilli requestContext request -> { httpRequestId = httpRequestId, posixTimeMilli = posixTimeMilli, requestContext = requestContext, request = request })
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "httpRequestId"
                            [ "HttpRequestId" ]
                            Json.Decode.string
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "posixTimeMilli"
                            [ "PosixTimeMilli" ]
                            Json.Decode.int
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "requestContext"
                            [ "RequestContext" ]
                            ( Json.Decode.succeed (\clientAddress -> { clientAddress = clientAddress })
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "clientAddress"
                                        [ "ClientAddress" ]
                                        ( jsonDecode__generic_Maybe Json.Decode.string
                                        )
                                    )
                            )
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "request"
                            [ "Request" ]
                            ( Json.Decode.succeed (\method uri body headers -> { method = method, uri = uri, body = body, headers = headers })
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "method"
                                        [ "Method" ]
                                        Json.Decode.string
                                    )
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "uri"
                                        [ "Uri" ]
                                        Json.Decode.string
                                    )
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "body"
                                        [ "Body" ]
                                        ( jsonDecode__generic_Maybe json_decode_Bytes
                                        )
                                    )
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "headers"
                                        [ "Headers" ]
                                        ( jsonDecode__generic_List (Json.Decode.succeed (\name values -> { name = name, values = values })
                                            |> jsonDecode_andMap
                                                ( jsonDecode_field_withAlternateNames "name"
                                                    [ "Name" ]
                                                    Json.Decode.string
                                                )
                                            |> jsonDecode_andMap
                                                ( jsonDecode_field_withAlternateNames "values"
                                                    [ "Values" ]
                                                    ( jsonDecode__generic_List Json.Decode.string
                                                    )
                                                ))
                                        )
                                    )
                            )
                        ))
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "tuple2"
                [ "Tuple2" ]
                ( Json.Decode.map2 (\item_0 item_1 -> ( item_0, item_1 ))
                    (Json.Decode.index 0 Json.Decode.int)
                    (Json.Decode.index 1 Json.Decode.string)
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "tuple3"
                [ "Tuple3" ]
                ( Json.Decode.map3 (\item_0 item_1 item_2 -> ( item_0, item_1, item_2 ))
                    (Json.Decode.index 0 Json.Decode.int)
                    (Json.Decode.index 1 Json.Decode.string)
                    (Json.Decode.index 2 Json.Decode.int)
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "list_custom_type"
                [ "List_custom_type" ]
                ( jsonDecode__generic_List jsonDecode_Backend_State_ChoiceType
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "opaque_custom_type"
                [ "Opaque_custom_type" ]
                jsonDecode_Backend_State_OpaqueChoiceType
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "recursive_type"
                [ "Recursive_type" ]
                jsonDecode_Backend_State_RecursiveType
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "bool"
                [ "Bool" ]
                Json.Decode.bool
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "maybe"
                [ "Maybe" ]
                ( jsonDecode__generic_Maybe Json.Decode.string
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "result"
                [ "Result" ]
                ( jsonDecode__generic_Result Json.Decode.string Json.Decode.int
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "set"
                [ "Set" ]
                ( jsonDecode__generic_Set Json.Decode.int
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "dict"
                [ "Dict" ]
                ( jsonDecode__generic_Dict Json.Decode.int Json.Decode.string
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "empty_record"
                [ "Empty_record" ]
                ( Json.Decode.succeed {}
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "empty_tuple"
                [ "Empty_tuple" ]
                ( Json.Decode.succeed ()
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "choiceTypeInstance"
                [ "ChoiceTypeInstance" ]
                ( jsonDecode_Backend_State_ChoiceTypeWithTypeParameter Json.Decode.int
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "record_instance_string"
                [ "Record_instance_string" ]
                ( Json.Decode.succeed (\field_a field_parameterized field_parameterized_maybe field_parameterized_tuple field_parameterized_record -> { field_a = field_a, field_parameterized = field_parameterized, field_parameterized_maybe = field_parameterized_maybe, field_parameterized_tuple = field_parameterized_tuple, field_parameterized_record = field_parameterized_record })
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_a"
                            [ "Field_a" ]
                            Json.Decode.int
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_parameterized"
                            [ "Field_parameterized" ]
                            Json.Decode.string
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_parameterized_maybe"
                            [ "Field_parameterized_maybe" ]
                            ( jsonDecode__generic_Maybe Json.Decode.string
                            )
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_parameterized_tuple"
                            [ "Field_parameterized_tuple" ]
                            ( Json.Decode.map2 (\item_0 item_1 -> ( item_0, item_1 ))
                                (Json.Decode.index 0 Json.Decode.string)
                                (Json.Decode.index 1 Json.Decode.string)
                            )
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_parameterized_record"
                            [ "Field_parameterized_record" ]
                            ( Json.Decode.succeed (\field_int field_parameterized -> { field_int = field_int, field_parameterized = field_parameterized })
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "field_int"
                                        [ "Field_int" ]
                                        Json.Decode.int
                                    )
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "field_parameterized"
                                        [ "Field_parameterized" ]
                                        Json.Decode.string
                                    )
                            )
                        )
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "record_instance_int"
                [ "Record_instance_int" ]
                ( Json.Decode.succeed (\field_a field_parameterized field_parameterized_maybe field_parameterized_tuple field_parameterized_record -> { field_a = field_a, field_parameterized = field_parameterized, field_parameterized_maybe = field_parameterized_maybe, field_parameterized_tuple = field_parameterized_tuple, field_parameterized_record = field_parameterized_record })
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_a"
                            [ "Field_a" ]
                            Json.Decode.int
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_parameterized"
                            [ "Field_parameterized" ]
                            Json.Decode.int
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_parameterized_maybe"
                            [ "Field_parameterized_maybe" ]
                            ( jsonDecode__generic_Maybe Json.Decode.int
                            )
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_parameterized_tuple"
                            [ "Field_parameterized_tuple" ]
                            ( Json.Decode.map2 (\item_0 item_1 -> ( item_0, item_1 ))
                                (Json.Decode.index 0 Json.Decode.int)
                                (Json.Decode.index 1 Json.Decode.int)
                            )
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "field_parameterized_record"
                            [ "Field_parameterized_record" ]
                            ( Json.Decode.succeed (\field_int field_parameterized -> { field_int = field_int, field_parameterized = field_parameterized })
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "field_int"
                                        [ "Field_int" ]
                                        Json.Decode.int
                                    )
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "field_parameterized"
                                        [ "Field_parameterized" ]
                                        Json.Decode.int
                                    )
                            )
                        )
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "listDict"
                [ "ListDict" ]
                ( jsonDecode_ListDict_Dict (Json.Decode.succeed (\orig dest -> { orig = orig, dest = dest })
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "orig"
                            [ "Orig" ]
                            Json.Decode.int
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "dest"
                            [ "Dest" ]
                            Json.Decode.int
                        )) Json.Decode.string
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "bytes"
                [ "Bytes" ]
                json_decode_Bytes
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "array_primitive"
                [ "Array_primitive" ]
                ( jsonDecode__generic_Array Json.Decode.int
                )
            )


jsonEncode_Backend_State_ChoiceType valueToEncode =
    case valueToEncode of
        Backend.State.CustomTagWithMaybeInstance tagArgument0 ->
            Json.Encode.object [ ( "CustomTagWithMaybeInstance", Json.Encode.list identity [ jsonEncode__generic_Maybe (\type_arg -> Json.Encode.int type_arg) tagArgument0 ] ) ]
        Backend.State.CustomTagWithOneParameter tagArgument0 ->
            Json.Encode.object [ ( "CustomTagWithOneParameter", Json.Encode.list identity [ Json.Encode.int tagArgument0 ] ) ]
        Backend.State.CustomTagWithResultInstance tagArgument0 ->
            Json.Encode.object [ ( "CustomTagWithResultInstance", Json.Encode.list identity [ jsonEncode__generic_Result (\type_arg -> Json.Encode.string type_arg) (\type_arg -> Json.Encode.int type_arg) tagArgument0 ] ) ]
        Backend.State.CustomTagWithTwoParameters tagArgument0 tagArgument1 ->
            Json.Encode.object [ ( "CustomTagWithTwoParameters", Json.Encode.list identity [ Json.Encode.string tagArgument0, Json.Encode.int tagArgument1 ] ) ]
        Backend.State.CustomTagWithoutParameter ->
            Json.Encode.object [ ( "CustomTagWithoutParameter", Json.Encode.list identity [] ) ]


jsonDecode_Backend_State_ChoiceType =
    Json.Decode.oneOf
        [ Json.Decode.field "CustomTagWithMaybeInstance" (Json.Decode.lazy (\_ -> Json.Decode.map Backend.State.CustomTagWithMaybeInstance (Json.Decode.index 0 (jsonDecode__generic_Maybe Json.Decode.int))))
        , Json.Decode.field "CustomTagWithOneParameter" (Json.Decode.lazy (\_ -> Json.Decode.map Backend.State.CustomTagWithOneParameter (Json.Decode.index 0 Json.Decode.int)))
        , Json.Decode.field "CustomTagWithResultInstance" (Json.Decode.lazy (\_ -> Json.Decode.map Backend.State.CustomTagWithResultInstance (Json.Decode.index 0 (jsonDecode__generic_Result Json.Decode.string Json.Decode.int))))
        , Json.Decode.field "CustomTagWithTwoParameters" (Json.Decode.lazy (\_ -> Json.Decode.map2 Backend.State.CustomTagWithTwoParameters (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 Json.Decode.int)))
        , Json.Decode.field "CustomTagWithoutParameter" (jsonDecodeSucceedWhenNotNull Backend.State.CustomTagWithoutParameter)
        ]


jsonEncode_Backend_State_ChoiceTypeWithTypeParameter jsonEncode_type_parameter_a valueToEncode =
    case valueToEncode of
        Backend.State.ChoiceTypeWithTypeParameter tagArgument0 ->
            Json.Encode.object [ ( "ChoiceTypeWithTypeParameter", Json.Encode.list identity [ jsonEncode_type_parameter_a tagArgument0 ] ) ]


jsonDecode_Backend_State_ChoiceTypeWithTypeParameter jsonDecode_type_parameter_a =
    Json.Decode.oneOf
        [ Json.Decode.field "ChoiceTypeWithTypeParameter" (Json.Decode.lazy (\_ -> Json.Decode.map Backend.State.ChoiceTypeWithTypeParameter (Json.Decode.index 0 jsonDecode_type_parameter_a)))
        ]


jsonEncode_Backend_State_OpaqueChoiceType valueToEncode =
    case valueToEncode of
        Backend.State.OpaqueChoiceType tagArgument0 ->
            Json.Encode.object [ ( "OpaqueChoiceType", Json.Encode.list identity [ Json.Encode.string tagArgument0 ] ) ]


jsonDecode_Backend_State_OpaqueChoiceType =
    Json.Decode.oneOf
        [ Json.Decode.field "OpaqueChoiceType" (Json.Decode.lazy (\_ -> Json.Decode.map Backend.State.OpaqueChoiceType (Json.Decode.index 0 Json.Decode.string)))
        ]


jsonEncode_Backend_State_RecursiveType valueToEncode =
    case valueToEncode of
        Backend.State.TagRecurse tagArgument0 ->
            Json.Encode.object [ ( "TagRecurse", Json.Encode.list identity [ jsonEncode_Backend_State_RecursiveType tagArgument0 ] ) ]
        Backend.State.TagTerminate tagArgument0 ->
            Json.Encode.object [ ( "TagTerminate", Json.Encode.list identity [ Json.Encode.int tagArgument0 ] ) ]


jsonDecode_Backend_State_RecursiveType =
    Json.Decode.oneOf
        [ Json.Decode.field "TagRecurse" (Json.Decode.lazy (\_ -> Json.Decode.map Backend.State.TagRecurse (Json.Decode.index 0 jsonDecode_Backend_State_RecursiveType)))
        , Json.Decode.field "TagTerminate" (Json.Decode.lazy (\_ -> Json.Decode.map Backend.State.TagTerminate (Json.Decode.index 0 Json.Decode.int)))
        ]


jsonEncode_ListDict_Dict jsonEncode_type_parameter_key jsonEncode_type_parameter_value valueToEncode =
    case valueToEncode of
        ListDict.Dict tagArgument0 ->
            Json.Encode.object [ ( "Dict", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.list identity
                [ jsonEncode_type_parameter_key ((\( item_0, item_1 ) -> item_0) type_arg)
                , jsonEncode_type_parameter_value ((\( item_0, item_1 ) -> item_1) type_arg)
                ]) tagArgument0 ] ) ]


jsonDecode_ListDict_Dict jsonDecode_type_parameter_key jsonDecode_type_parameter_value =
    Json.Decode.oneOf
        [ Json.Decode.field "Dict" (Json.Decode.lazy (\_ -> Json.Decode.map ListDict.Dict (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.map2 (\item_0 item_1 -> ( item_0, item_1 ))
            (Json.Decode.index 0 jsonDecode_type_parameter_key)
            (Json.Decode.index 1 jsonDecode_type_parameter_value))))))
        ]


jsonEncode__generic_Maybe encodeJust valueToEncode =
    case valueToEncode of
        Nothing ->
            [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object

        Just just ->
            [ ( "Just", [ just ] |> Json.Encode.list encodeJust ) ] |> Json.Encode.object


jsonDecode__generic_Maybe decoder =
    Json.Decode.oneOf
        [ Json.Decode.field "Nothing" (Json.Decode.succeed Nothing)
        , Json.Decode.field "Just" ((Json.Decode.index 0 decoder) |> Json.Decode.map Just)
        , Json.Decode.field "Just" (decoder |> Json.Decode.map Just) -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.null Nothing -- Temporary backwardscompatibility: Map 'null' to Nothing
        ]


jsonEncode__generic_List  = Json.Encode.list


jsonDecode__generic_List  = Json.Decode.list


jsonEncode__generic_Array  = Json.Encode.array


jsonDecode__generic_Array  = Json.Decode.array


jsonEncode__generic_Set encoder =
    Set.toList >> Json.Encode.list encoder


jsonDecode__generic_Set decoder =
    Json.Decode.list decoder |> Json.Decode.map Set.fromList


jsonEncode__generic_Dict encodeKey encodeValue =
    Dict.toList >> Json.Encode.list (jsonEncode__tuple_2 encodeKey encodeValue)


jsonDecode__generic_Dict decodeKey decodeValue =
        (Json.Decode.list (jsonDecode__tuple_2 decodeKey decodeValue))
            |> Json.Decode.map Dict.fromList


jsonEncode__generic_Result encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( "Err", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( "Ok", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object


jsonDecode__generic_Result decodeErr decodeOk =
    Json.Decode.oneOf
        [ Json.Decode.field "Err" (Json.Decode.index 0 decodeErr) |> Json.Decode.map Err
        , Json.Decode.field "Ok" (Json.Decode.index 0 decodeOk) |> Json.Decode.map Ok
        , Json.Decode.field "Err" decodeErr |> Json.Decode.map Err -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.field "Ok" decodeOk |> Json.Decode.map Ok -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        ]


jsonEncode__tuple_2 encodeA encodeB ( a, b ) =
    [ a |> encodeA, b |> encodeB ]
        |> Json.Encode.list identity


jsonDecode__tuple_2 decodeA decodeB =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)


jsonEncode__tuple_3 encodeA encodeB encodeC ( a, b, c ) =
    [ a |> encodeA, b |> encodeB, c |> encodeC ]
        |> Json.Encode.list identity


jsonDecode__tuple_3 decodeA decodeB decodeC =
    Json.Decode.map3 (\a b c -> ( a, b, c ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)
        (Json.Decode.index 2 decodeC)


jsonDecode_andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
jsonDecode_andMap =
    Json.Decode.map2 (|>)


json_encode_Bytes : Bytes.Bytes -> Json.Encode.Value
json_encode_Bytes bytes =
    [ ( "AsBase64", bytes |> Base64.fromBytes |> Maybe.withDefault "Error encoding to base64" |> Json.Encode.string ) ]
        |> Json.Encode.object


json_decode_Bytes : Json.Decode.Decoder Bytes.Bytes
json_decode_Bytes =
    Json.Decode.field "AsBase64"
        (Json.Decode.string
            |> Json.Decode.andThen
                (Base64.toBytes >> Maybe.map Json.Decode.succeed >> Maybe.withDefault (Json.Decode.fail "Failed to decode base64."))
        )


jsonDecodeSucceedWhenNotNull : a -> Json.Decode.Decoder a
jsonDecodeSucceedWhenNotNull valueIfNotNull =
    Json.Decode.value
        |> Json.Decode.andThen
            (\asValue ->
                if asValue == Json.Encode.null then
                    Json.Decode.fail "Is null."

                else
                    Json.Decode.succeed valueIfNotNull
            )


jsonDecode_field_withAlternateNames : String -> List String -> Json.Decode.Decoder a -> Json.Decode.Decoder a
jsonDecode_field_withAlternateNames fieldName alternateNames decoder =
    Json.Decode.oneOf
        ((fieldName :: alternateNames)
            |> List.map (\name -> Json.Decode.field name decoder)
        )