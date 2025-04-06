module Backend.InterfaceToHost_Root.Generated_JsonConverters exposing (..)

import Array
import Backend.Generated.StateShimTypes
import Bytes
import Bytes.Decode
import Bytes.Encode
import CompilerGenerated.Base64 as Base64
import Dict
import Json.Decode
import Json.Encode
import Set


jsonEncode_1082357013 valueToEncode =
    Json.Encode.object
        [ ( "posixTimeMilli"
          , Json.Encode.int valueToEncode.posixTimeMilli
          )
        , ( "volatileProcessesIds"
          , jsonEncode__generic_Set (\type_arg -> Json.Encode.string type_arg) valueToEncode.volatileProcessesIds
          )
        , ( "pendingHttpRequests"
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
                ]) valueToEncode.pendingHttpRequests
          )
        , ( "pendingTasksForRequestVolatileProcess"
          , jsonEncode__generic_Dict (\type_arg -> Json.Encode.string type_arg) (\type_arg -> Json.Encode.object
                [ ( "volatileProcessId"
                  , Json.Encode.string type_arg.volatileProcessId
                  )
                , ( "startPosixTimeMilli"
                  , Json.Encode.int type_arg.startPosixTimeMilli
                  )
                ]) valueToEncode.pendingTasksForRequestVolatileProcess
          )
        ]


jsonDecode_1082357013 =
    Json.Decode.succeed (\posixTimeMilli volatileProcessesIds pendingHttpRequests pendingTasksForRequestVolatileProcess -> { posixTimeMilli = posixTimeMilli, volatileProcessesIds = volatileProcessesIds, pendingHttpRequests = pendingHttpRequests, pendingTasksForRequestVolatileProcess = pendingTasksForRequestVolatileProcess })
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "posixTimeMilli"
                [ "PosixTimeMilli" ]
                Json.Decode.int
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "volatileProcessesIds"
                [ "VolatileProcessesIds" ]
                ( jsonDecode__generic_Set Json.Decode.string
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "pendingHttpRequests"
                [ "PendingHttpRequests" ]
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
            ( jsonDecode_field_withAlternateNames "pendingTasksForRequestVolatileProcess"
                [ "PendingTasksForRequestVolatileProcess" ]
                ( jsonDecode__generic_Dict Json.Decode.string (Json.Decode.succeed (\volatileProcessId startPosixTimeMilli -> { volatileProcessId = volatileProcessId, startPosixTimeMilli = startPosixTimeMilli })
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "volatileProcessId"
                            [ "VolatileProcessId" ]
                            Json.Decode.string
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "startPosixTimeMilli"
                            [ "StartPosixTimeMilli" ]
                            Json.Decode.int
                        ))
                )
            )


jsonEncode_3061625842 valueToEncode =
    jsonEncode_Backend_Generated_StateShimTypes_StateShimRequest valueToEncode


jsonDecode_3061625842 =
    jsonDecode_Backend_Generated_StateShimTypes_StateShimRequest


jsonEncode_881329171 valueToEncode =
    jsonEncode__generic_Result (\type_arg -> Json.Encode.string type_arg) (\type_arg -> jsonEncode_Backend_Generated_StateShimTypes_StateShimResponse type_arg) valueToEncode


jsonDecode_881329171 =
    jsonDecode__generic_Result Json.Decode.string jsonDecode_Backend_Generated_StateShimTypes_StateShimResponse


jsonEncode_Backend_Generated_StateShimTypes_StateShimRequest valueToEncode =
    case valueToEncode of
        Backend.Generated.StateShimTypes.ListExposedFunctionsShimRequest ->
            Json.Encode.object [ ( "ListExposedFunctionsShimRequest", Json.Encode.list identity [] ) ]


jsonDecode_Backend_Generated_StateShimTypes_StateShimRequest =
    Json.Decode.oneOf
        [ Json.Decode.field "ListExposedFunctionsShimRequest" (jsonDecodeSucceedWhenNotNull Backend.Generated.StateShimTypes.ListExposedFunctionsShimRequest)
        ]


jsonEncode_Backend_Generated_StateShimTypes_StateShimResponse valueToEncode =
    case valueToEncode of
        Backend.Generated.StateShimTypes.ListExposedFunctionsShimResponse tagArgument0 ->
            Json.Encode.object [ ( "ListExposedFunctionsShimResponse", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.object
                [ ( "functionName"
                  , Json.Encode.string type_arg.functionName
                  )
                , ( "functionDescription"
                  , Json.Encode.object
                        [ ( "returnType"
                          , Json.Encode.object
                                [ ( "sourceCodeText"
                                  , Json.Encode.string type_arg.functionDescription.returnType.sourceCodeText
                                  )
                                , ( "containsAppStateType"
                                  , Json.Encode.bool type_arg.functionDescription.returnType.containsAppStateType
                                  )
                                ]
                          )
                        , ( "parameters"
                          , jsonEncode__generic_List (\type_arg_ -> Json.Encode.object
                                [ ( "patternSourceCodeText"
                                  , Json.Encode.string type_arg_.patternSourceCodeText
                                  )
                                , ( "typeSourceCodeText"
                                  , Json.Encode.string type_arg_.typeSourceCodeText
                                  )
                                , ( "typeIsAppStateType"
                                  , Json.Encode.bool type_arg_.typeIsAppStateType
                                  )
                                ]) type_arg.functionDescription.parameters
                          )
                        ]
                  )
                ]) tagArgument0 ] ) ]


jsonDecode_Backend_Generated_StateShimTypes_StateShimResponse =
    Json.Decode.oneOf
        [ Json.Decode.field "ListExposedFunctionsShimResponse" (Json.Decode.lazy (\_ -> Json.Decode.map Backend.Generated.StateShimTypes.ListExposedFunctionsShimResponse (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.succeed (\functionName functionDescription -> { functionName = functionName, functionDescription = functionDescription })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "functionName"
                    [ "FunctionName" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "functionDescription"
                    [ "FunctionDescription" ]
                    ( Json.Decode.succeed (\returnType parameters -> { returnType = returnType, parameters = parameters })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "returnType"
                                [ "ReturnType" ]
                                ( Json.Decode.succeed (\sourceCodeText containsAppStateType -> { sourceCodeText = sourceCodeText, containsAppStateType = containsAppStateType })
                                    |> jsonDecode_andMap
                                        ( jsonDecode_field_withAlternateNames "sourceCodeText"
                                            [ "SourceCodeText" ]
                                            Json.Decode.string
                                        )
                                    |> jsonDecode_andMap
                                        ( jsonDecode_field_withAlternateNames "containsAppStateType"
                                            [ "ContainsAppStateType" ]
                                            Json.Decode.bool
                                        )
                                )
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "parameters"
                                [ "Parameters" ]
                                ( jsonDecode__generic_List (Json.Decode.succeed (\patternSourceCodeText typeSourceCodeText typeIsAppStateType -> { patternSourceCodeText = patternSourceCodeText, typeSourceCodeText = typeSourceCodeText, typeIsAppStateType = typeIsAppStateType })
                                    |> jsonDecode_andMap
                                        ( jsonDecode_field_withAlternateNames "patternSourceCodeText"
                                            [ "PatternSourceCodeText" ]
                                            Json.Decode.string
                                        )
                                    |> jsonDecode_andMap
                                        ( jsonDecode_field_withAlternateNames "typeSourceCodeText"
                                            [ "TypeSourceCodeText" ]
                                            Json.Decode.string
                                        )
                                    |> jsonDecode_andMap
                                        ( jsonDecode_field_withAlternateNames "typeIsAppStateType"
                                            [ "TypeIsAppStateType" ]
                                            Json.Decode.bool
                                        ))
                                )
                            )
                    )
                ))))))
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