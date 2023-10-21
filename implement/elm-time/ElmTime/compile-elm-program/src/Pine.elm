module Pine exposing (..)

import BigInt
import Dict
import Result.Extra


type Expression
    = LiteralExpression Value
    | ListExpression (List Expression)
    | DecodeAndEvaluateExpression DecodeAndEvaluateExpressionStructure
    | KernelApplicationExpression KernelApplicationExpressionStructure
    | ConditionalExpression ConditionalExpressionStructure
    | EnvironmentExpression
    | StringTagExpression String Expression


type alias DecodeAndEvaluateExpressionStructure =
    { expression : Expression
    , environment : Expression
    }


type alias KernelApplicationExpressionStructure =
    { functionName : String
    , argument : Expression
    }


type alias ConditionalExpressionStructure =
    { condition : Expression
    , ifTrue : Expression
    , ifFalse : Expression
    }


type Value
    = BlobValue (List Int)
    | ListValue (List Value)


type alias KernelFunction =
    Value -> Result (PathDescription String) Value


type alias EvalContext =
    { environment : Value }


type PathDescription a
    = DescribePathNode a (PathDescription a)
    | DescribePathEnd a


environmentFromDeclarations : List ( String, Value ) -> Value
environmentFromDeclarations declarations =
    declarations |> List.map valueFromContextExpansionWithName |> ListValue


addToEnvironment : List Value -> EvalContext -> EvalContext
addToEnvironment names context =
    let
        environment =
            case context.environment of
                ListValue applicationArgumentList ->
                    ListValue (names ++ applicationArgumentList)

                _ ->
                    ListValue names
    in
    { context | environment = environment }


emptyEvalContext : EvalContext
emptyEvalContext =
    { environment = ListValue [] }


evaluateExpression : EvalContext -> Expression -> Result (PathDescription String) Value
evaluateExpression context expression =
    case expression of
        LiteralExpression value ->
            Ok value

        ListExpression listElements ->
            listElements
                |> List.indexedMap
                    (\listElementIndex listElement ->
                        evaluateExpression context listElement
                            |> Result.mapError
                                (DescribePathNode
                                    ("Failed to evaluate list item " ++ String.fromInt listElementIndex ++ ": ")
                                )
                    )
                |> Result.Extra.combine
                |> Result.map ListValue

        DecodeAndEvaluateExpression decodeAndEvaluate ->
            evaluateDecodeAndEvaluate context decodeAndEvaluate
                |> Result.mapError
                    (\e ->
                        e |> DescribePathNode ("Failed decode and evaluate of '" ++ describeExpression 1 decodeAndEvaluate.expression ++ "'")
                    )

        KernelApplicationExpression application ->
            evaluateExpression context application.argument
                |> Result.mapError (DescribePathNode ("Failed to evaluate argument for kernel function " ++ application.functionName ++ ": "))
                |> Result.andThen
                    (\argument ->
                        application.functionName
                            |> decodeKernelFunctionFromName
                            |> Result.mapError DescribePathEnd
                            |> Result.map (\kernelFunction -> kernelFunction argument |> Result.withDefault (ListValue []))
                    )

        ConditionalExpression conditional ->
            case evaluateExpression context conditional.condition of
                Err error ->
                    Err (DescribePathNode "Failed to evaluate condition" error)

                Ok conditionValue ->
                    evaluateExpression context
                        (if conditionValue == trueValue then
                            conditional.ifTrue

                         else
                            conditional.ifFalse
                        )

        EnvironmentExpression ->
            Ok context.environment

        StringTagExpression tag tagged ->
            let
                log =
                    Debug.log "eval expression with tag"
                        tag
            in
            evaluateExpression context tagged
                |> Result.mapError (DescribePathNode ("Failed to evaluate tagged expression '" ++ tag ++ "': "))


valueFromContextExpansionWithName : ( String, Value ) -> Value
valueFromContextExpansionWithName ( declName, declValue ) =
    ListValue [ valueFromString declName, declValue ]


namedValueFromValue : Value -> Maybe ( String, Value )
namedValueFromValue value =
    case value of
        ListValue [ elementLabelCandidate, elementValue ] ->
            case stringFromValue elementLabelCandidate of
                Ok elementLabel ->
                    Just ( elementLabel, elementValue )

                Err _ ->
                    Nothing

        _ ->
            Nothing


kernelFunctions : Dict.Dict String KernelFunction
kernelFunctions =
    [ ( "equal"
      , mapFromListValueOrBlobValue { fromList = list_all_same, fromBlob = list_all_same }
            >> valueFromBool
            >> Ok
      )
    , ( "negate"
      , kernelFunction_Negate
      )
    , ( "logical_and", kernelFunctionExpectingListOfTypeBool (List.foldl (&&) True) )
    , ( "logical_or", kernelFunctionExpectingListOfTypeBool (List.foldl (||) False) )
    , ( "length"
      , mapFromListValueOrBlobValue { fromList = List.length, fromBlob = List.length }
            >> (BigInt.fromInt >> valueFromBigInt)
            >> Ok
      )
    , ( "skip"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = intFromValue >> Result.mapError DescribePathEnd
            , mapArg1 = Ok
            , apply =
                \count ->
                    mapFromListValueOrBlobValue
                        { fromList = List.drop count >> ListValue
                        , fromBlob = List.drop count >> BlobValue
                        }
                        >> Ok
            }
      )
    , ( "take"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = intFromValue >> Result.mapError DescribePathEnd
            , mapArg1 = Ok
            , apply =
                \count ->
                    mapFromListValueOrBlobValue
                        { fromList = List.take count >> ListValue
                        , fromBlob = List.take count >> BlobValue
                        }
                        >> Ok
            }
      )
    , ( "reverse"
      , mapFromListValueOrBlobValue
            { fromList = List.reverse >> ListValue
            , fromBlob = List.reverse >> BlobValue
            }
            >> Ok
      )
    , ( "concat"
      , decodePineListValue
            >> Result.mapError DescribePathEnd
            >> Result.map
                (List.foldl
                    (\next aggregate ->
                        case ( aggregate, next ) of
                            ( ListValue aggregateList, ListValue nextList ) ->
                                ListValue (aggregateList ++ nextList)

                            ( BlobValue aggregateBlob, BlobValue nextBlob ) ->
                                BlobValue (aggregateBlob ++ nextBlob)

                            _ ->
                                next
                    )
                    (ListValue [])
                )
      )
    , ( "list_head"
      , decodePineListValue
            >> Result.map (List.head >> Maybe.withDefault (ListValue []))
            >> Result.mapError DescribePathEnd
      )
    , ( "add_int"
      , kernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt (List.foldl BigInt.add)
      )
    , ( "sub_int"
      , kernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt (List.foldl (\a b -> BigInt.sub b a))
      )
    , ( "mul_int"
      , kernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt (List.foldl BigInt.mul)
      )
    , ( "div_int"
      , kernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt (List.foldl (\a b -> BigInt.div b a))
      )
    , ( "is_sorted_ascending_int"
      , is_sorted_ascending_int >> valueFromBool >> Ok
      )
    ]
        |> Dict.fromList


kernelFunction_Negate : KernelFunction
kernelFunction_Negate value =
    Ok
        (case value of
            BlobValue blob ->
                case blob of
                    4 :: rest ->
                        BlobValue (2 :: rest)

                    2 :: rest ->
                        BlobValue (4 :: rest)

                    _ ->
                        ListValue []

            ListValue _ ->
                ListValue []
        )


list_all_same : List a -> Bool
list_all_same list =
    case list of
        [] ->
            True

        first :: rest ->
            List.all ((==) first) rest


is_sorted_ascending_int : Value -> Bool
is_sorted_ascending_int value =
    value == sort_int value


sort_int : Value -> Value
sort_int value =
    case value of
        BlobValue _ ->
            value

        ListValue list ->
            list
                |> List.map sort_int
                |> List.sortWith sort_int_order
                |> ListValue


sort_int_order : Value -> Value -> Order
sort_int_order x y =
    case ( x, y ) of
        ( BlobValue blobX, BlobValue blobY ) ->
            case ( bigIntFromBlobValue blobX, bigIntFromBlobValue blobY ) of
                ( Ok intX, Ok intY ) ->
                    BigInt.compare intX intY

                ( Ok _, Err _ ) ->
                    LT

                ( Err _, Ok _ ) ->
                    GT

                ( Err _, Err _ ) ->
                    EQ

        ( ListValue listX, ListValue listY ) ->
            compare (List.length listX) (List.length listY)

        ( ListValue _, BlobValue _ ) ->
            LT

        ( BlobValue _, ListValue _ ) ->
            GT


mapFromListValueOrBlobValue : { fromList : List Value -> a, fromBlob : List Int -> a } -> Value -> a
mapFromListValueOrBlobValue { fromList, fromBlob } value =
    case value of
        ListValue list ->
            fromList list

        BlobValue blob ->
            fromBlob blob


evaluateDecodeAndEvaluate : EvalContext -> DecodeAndEvaluateExpressionStructure -> Result (PathDescription String) Value
evaluateDecodeAndEvaluate context decodeAndEvaluate =
    evaluateExpression context decodeAndEvaluate.environment
        |> Result.mapError
            (\e ->
                e |> DescribePathNode ("Failed to evaluate environment '" ++ describeExpression 1 decodeAndEvaluate.environment ++ "'")
            )
        |> Result.andThen
            (\environmentValue ->
                evaluateExpression context decodeAndEvaluate.expression
                    |> Result.mapError
                        (\e ->
                            e |> DescribePathNode ("Failed to evaluate encoded expression '" ++ describeExpression 1 decodeAndEvaluate.expression ++ "'")
                        )
                    |> Result.andThen
                        (\functionValue ->
                            functionValue
                                |> decodeExpressionFromValue
                                |> Result.mapError
                                    (\e ->
                                        e
                                            |> DescribePathEnd
                                            |> DescribePathNode ("Failed to decode expression from value '" ++ describeValue 3 functionValue ++ "'")
                                    )
                                |> Result.andThen
                                    (\functionExpression ->
                                        evaluateExpression { environment = environmentValue } functionExpression
                                    )
                        )
            )


kernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt :
    (BigInt.BigInt -> List BigInt.BigInt -> BigInt.BigInt)
    -> KernelFunction
kernelFunctionExpectingListOfBigIntWithAtLeastOneAndProducingBigInt apply =
    kernelFunctionExpectingListOfBigInt
        (\list ->
            case list of
                [] ->
                    Err "List is empty. Expected at least one element"

                firstElement :: otherElements ->
                    Ok (valueFromBigInt (apply firstElement otherElements))
        )


kernelFunctionExpectingListOfBigInt : (List BigInt.BigInt -> Result String Value) -> KernelFunction
kernelFunctionExpectingListOfBigInt apply =
    decodePineListValue
        >> Result.andThen (List.map bigIntFromValue >> Result.Extra.combine)
        >> Result.andThen apply
        >> Result.mapError DescribePathEnd


kernelFunctionExpectingListOfTypeBool : (List Bool -> Bool) -> KernelFunction
kernelFunctionExpectingListOfTypeBool apply =
    decodePineListValue
        >> Result.andThen (List.map (boolFromValue >> Result.fromMaybe "Value is neither True nor False") >> Result.Extra.combine)
        >> Result.map (apply >> valueFromBool)
        >> Result.mapError DescribePathEnd


kernelFunctionExpectingExactlyTwoArguments :
    { mapArg0 : Value -> Result (PathDescription String) arg0
    , mapArg1 : Value -> Result (PathDescription String) arg1
    , apply : arg0 -> arg1 -> Result (PathDescription String) Value
    }
    -> KernelFunction
kernelFunctionExpectingExactlyTwoArguments configuration =
    decodePineListValue
        >> Result.andThen decodeListWithExactlyTwoElements
        >> Result.mapError DescribePathEnd
        >> Result.andThen
            (\( arg0Value, arg1Value ) ->
                arg0Value
                    |> configuration.mapArg0
                    |> Result.mapError (DescribePathNode "Failed to map argument 0")
                    |> Result.andThen
                        (\arg0 ->
                            arg1Value
                                |> configuration.mapArg1
                                |> Result.mapError (DescribePathNode "Failed to map argument 1")
                                |> Result.map (Tuple.pair arg0)
                        )
            )
        >> Result.andThen
            (\( arg0, arg1 ) -> configuration.apply arg0 arg1)


boolFromValue : Value -> Maybe Bool
boolFromValue value =
    if value == trueValue then
        Just True

    else if value == falseValue then
        Just False

    else
        Nothing


valueFromBool : Bool -> Value
valueFromBool bool =
    if bool then
        trueValue

    else
        falseValue


trueValue : Value
trueValue =
    BlobValue [ 4 ]


falseValue : Value
falseValue =
    BlobValue [ 2 ]


describeExpression : Int -> Expression -> String
describeExpression depthLimit expression =
    case expression of
        ListExpression list ->
            "list["
                ++ (if depthLimit < 1 then
                        "..."

                    else
                        String.join "," (list |> List.map (describeExpression (depthLimit - 1)))
                   )
                ++ "]"

        LiteralExpression literal ->
            "literal(" ++ describeValue (depthLimit - 1) literal ++ ")"

        DecodeAndEvaluateExpression decodeAndEvaluate ->
            "decode-and-evaluate("
                ++ (if depthLimit < 1 then
                        "..."

                    else
                        describeExpression (depthLimit - 1) decodeAndEvaluate.expression
                   )
                ++ ")"

        KernelApplicationExpression application ->
            "kernel-application(" ++ application.functionName ++ ")"

        ConditionalExpression _ ->
            "conditional"

        EnvironmentExpression ->
            "environment"

        StringTagExpression tag tagged ->
            "string-tag-" ++ tag ++ "(" ++ describeExpression (depthLimit - 1) tagged ++ ")"


describeValue : Int -> Value -> String
describeValue maxDepth value =
    case value of
        BlobValue blob ->
            "BlobValue 0x" ++ hexadecimalRepresentationFromBlobValue blob

        ListValue list ->
            let
                standard =
                    "["
                        ++ (if maxDepth < 0 then
                                "..."

                            else
                                String.join ", " (List.map (describeValue (maxDepth - 1)) list)
                           )
                        ++ "]"
            in
            String.join " "
                [ "ListValue"
                , case stringFromValue value of
                    Err _ ->
                        ""

                    Ok string ->
                        "\"" ++ string ++ "\""
                , standard
                ]


displayStringFromPineError : PathDescription String -> String
displayStringFromPineError error =
    case error of
        DescribePathEnd end ->
            end

        DescribePathNode nodeDescription node ->
            nodeDescription ++ "\n" ++ prependAllLines "  " (displayStringFromPineError node)


prependAllLines : String -> String -> String
prependAllLines prefix text =
    text
        |> String.lines
        |> List.map ((++) prefix)
        |> String.join "\n"


valueFromString : String -> Value
valueFromString =
    String.toList
        >> List.map valueFromChar
        >> ListValue


valueFromChar : Char -> Value
valueFromChar =
    Char.toCode >> BigInt.fromInt >> unsignedBlobValueFromBigInt >> Maybe.withDefault [] >> BlobValue


stringFromValue : Value -> Result String String
stringFromValue value =
    case value of
        ListValue charsValues ->
            stringFromListValue charsValues

        _ ->
            Err "Only a ListValue can represent a string."


stringFromListValue : List Value -> Result String String
stringFromListValue =
    List.map
        (bigIntFromUnsignedValue
            >> Result.fromMaybe "Failed to map to big int"
            >> Result.andThen intFromBigInt
            >> Result.andThen
                (\int ->
                    if int <= 0xFFFF then
                        Ok int

                    else
                        Err "Avoiding codes above 0xFFFF since transfer encoding failed for 0x10000."
                )
        )
        >> Result.Extra.combine
        >> Result.mapError ((++) "Failed to map list items to chars: ")
        >> Result.map (List.map Char.fromCode >> String.fromList)


valueFromBigInt : BigInt.BigInt -> Value
valueFromBigInt =
    blobValueFromBigInt >> BlobValue


blobValueFromBigInt : BigInt.BigInt -> List Int
blobValueFromBigInt bigint =
    let
        value =
            BigInt.abs bigint

        signByte =
            if value == bigint then
                4

            else
                2

        unsignedBytesFromIntValue intValue =
            if BigInt.lt intValue (BigInt.fromInt 0x0100) then
                String.toInt (BigInt.toString intValue) |> Maybe.map List.singleton

            else
                case BigInt.divmod intValue (BigInt.fromInt 0x0100) of
                    Nothing ->
                        Nothing

                    Just ( upper, lower ) ->
                        case unsignedBytesFromIntValue upper of
                            Nothing ->
                                Nothing

                            Just upperBytes ->
                                case String.toInt (BigInt.toString lower) of
                                    Nothing ->
                                        Nothing

                                    Just lowerByte ->
                                        Just (upperBytes ++ [ lowerByte ])
    in
    signByte :: Maybe.withDefault [] (unsignedBytesFromIntValue value)


unsignedBlobValueFromBigInt : BigInt.BigInt -> Maybe (List Int)
unsignedBlobValueFromBigInt bigint =
    case blobValueFromBigInt bigint of
        [] ->
            Nothing

        signByte :: unsignedBytes ->
            if signByte == 4 then
                Just unsignedBytes

            else
                Nothing


intFromValue : Value -> Result String Int
intFromValue =
    bigIntFromValue >> Result.andThen intFromBigInt


intFromBigInt : BigInt.BigInt -> Result String Int
intFromBigInt bigInt =
    case bigInt |> BigInt.toString |> String.toInt of
        Nothing ->
            Err "Failed to String.toInt"

        Just int ->
            if String.fromInt int /= BigInt.toString bigInt then
                Err "Integer out of supported range for String.toInt"

            else
                Ok int


bigIntFromValue : Value -> Result String BigInt.BigInt
bigIntFromValue value =
    case value of
        BlobValue blobValue ->
            bigIntFromBlobValue blobValue

        _ ->
            Err "Only a BlobValue can represent an integer."


bigIntFromBlobValue : List Int -> Result String BigInt.BigInt
bigIntFromBlobValue blobValue =
    case blobValue of
        [] ->
            Err "Empty blob is not a valid integer because the sign byte is missing. Did you mean to use an unsigned integer?"

        sign :: intValueBytes ->
            case sign of
                4 ->
                    intValueBytes |> bigIntFromUnsignedBlobValue |> Ok

                2 ->
                    intValueBytes |> bigIntFromUnsignedBlobValue |> BigInt.negate |> Ok

                _ ->
                    Err ("Unexpected value for sign byte of integer: " ++ String.fromInt sign)


bigIntFromUnsignedValue : Value -> Maybe BigInt.BigInt
bigIntFromUnsignedValue value =
    case value of
        BlobValue intValueBytes ->
            Just (bigIntFromUnsignedBlobValue intValueBytes)

        _ ->
            Nothing


bigIntFromUnsignedBlobValue : List Int -> BigInt.BigInt
bigIntFromUnsignedBlobValue intValueBytes =
    intValueBytes
        |> List.foldl
            (\nextByte aggregate ->
                BigInt.add (BigInt.fromInt nextByte) (BigInt.mul (BigInt.fromInt 0x0100) aggregate)
            )
            (BigInt.fromInt 0)


hexadecimalRepresentationFromBlobValue : List Int -> String
hexadecimalRepresentationFromBlobValue =
    List.map BigInt.fromInt
        >> List.map (BigInt.toHexString >> String.padLeft 2 '0')
        >> String.join ""


encodeExpressionAsValue : Expression -> Value
encodeExpressionAsValue expression =
    (case expression of
        LiteralExpression literal ->
            ( "Literal"
            , literal
            )

        ListExpression listExpr ->
            ( "List"
            , listExpr |> List.map encodeExpressionAsValue |> ListValue
            )

        DecodeAndEvaluateExpression decodeAndEvaluate ->
            ( "DecodeAndEvaluate"
            , [ ( "environment", encodeExpressionAsValue decodeAndEvaluate.environment )
              , ( "expression", encodeExpressionAsValue decodeAndEvaluate.expression )
              ]
                |> Dict.fromList
                |> encodeRecordToPineValue
            )

        KernelApplicationExpression app ->
            ( "KernelApplication"
            , [ ( "argument", encodeExpressionAsValue app.argument )
              , ( "functionName", valueFromString app.functionName )
              ]
                |> Dict.fromList
                |> encodeRecordToPineValue
            )

        ConditionalExpression conditional ->
            ( "Conditional"
            , [ ( "condition", conditional.condition )
              , ( "ifFalse", conditional.ifFalse )
              , ( "ifTrue", conditional.ifTrue )
              ]
                |> List.map (Tuple.mapSecond encodeExpressionAsValue)
                |> Dict.fromList
                |> encodeRecordToPineValue
            )

        EnvironmentExpression ->
            ( "Environment"
            , ListValue []
            )

        StringTagExpression tag tagged ->
            ( "StringTag"
            , [ valueFromString tag
              , encodeExpressionAsValue tagged
              ]
                |> ListValue
            )
    )
        |> (\( tagName, unionTagValue ) -> encodeUnionToPineValue tagName unionTagValue)


decodeExpressionFromValue : Value -> Result String Expression
decodeExpressionFromValue value =
    value
        |> decodeUnionFromPineValue
            (Dict.fromList
                [ ( "Literal"
                  , LiteralExpression >> Ok
                  )
                , ( "List"
                  , decodePineListValue
                        >> Result.andThen
                            (List.indexedMap
                                (\itemIndex item ->
                                    item
                                        |> decodeExpressionFromValue
                                        |> Result.mapError ((++) ("Failed to decode item at index " ++ String.fromInt itemIndex ++ ": "))
                                )
                                >> Result.Extra.combine
                            )
                        >> Result.map ListExpression
                  )
                , ( "DecodeAndEvaluate"
                  , decodeDecodeAndEvaluateExpression >> Result.map DecodeAndEvaluateExpression
                  )
                , ( "KernelApplication"
                  , decodeKernelApplicationExpression >> Result.map KernelApplicationExpression
                  )
                , ( "Conditional"
                  , decodeConditionalExpression >> Result.map ConditionalExpression
                  )
                , ( "Environment"
                  , always (Ok EnvironmentExpression)
                  )
                , ( "StringTag"
                  , decodePineListValue
                        >> Result.andThen decodeListWithExactlyTwoElements
                        >> Result.andThen
                            (\( tagValue, taggedValue ) ->
                                tagValue
                                    |> stringFromValue
                                    |> Result.mapError ((++) "Failed to decode tag: ")
                                    |> Result.andThen
                                        (\tag ->
                                            taggedValue
                                                |> decodeExpressionFromValue
                                                |> Result.mapError ((++) "Failed to decoded tagged expression: ")
                                                |> Result.map (\tagged -> StringTagExpression tag tagged)
                                        )
                            )
                  )
                ]
            )


decodeDecodeAndEvaluateExpression : Value -> Result String DecodeAndEvaluateExpressionStructure
decodeDecodeAndEvaluateExpression =
    decodeRecordFromPineValue
        >> Result.andThen
            (always (Ok DecodeAndEvaluateExpressionStructure)
                |> decodeRecordField "expression" decodeExpressionFromValue
                |> decodeRecordField "environment" decodeExpressionFromValue
            )


decodeKernelApplicationExpression : Value -> Result String KernelApplicationExpressionStructure
decodeKernelApplicationExpression =
    decodeRecordFromPineValue
        >> Result.andThen
            (always (Ok KernelApplicationExpressionStructure)
                |> decodeRecordField "functionName"
                    (stringFromValue
                        >> Result.andThen
                            (\functionName ->
                                functionName
                                    |> decodeKernelFunctionFromName
                                    |> Result.map (always functionName)
                            )
                    )
                |> decodeRecordField "argument" decodeExpressionFromValue
            )


decodeKernelFunctionFromName : String -> Result String KernelFunction
decodeKernelFunctionFromName functionName =
    case Dict.get functionName kernelFunctions of
        Nothing ->
            Err
                ("Did not find kernel function '"
                    ++ functionName
                    ++ "'. There are "
                    ++ String.fromInt (Dict.size kernelFunctions)
                    ++ " kernel functions available: "
                    ++ String.join ", " (Dict.keys kernelFunctions)
                )

        Just kernelFunction ->
            Ok kernelFunction


decodeConditionalExpression : Value -> Result String ConditionalExpressionStructure
decodeConditionalExpression =
    decodeRecordFromPineValue
        >> Result.andThen
            (always (Ok ConditionalExpressionStructure)
                |> decodeRecordField "condition" decodeExpressionFromValue
                |> decodeRecordField "ifTrue" decodeExpressionFromValue
                |> decodeRecordField "ifFalse" decodeExpressionFromValue
            )


decodeRecordField :
    String
    -> (recordfield -> Result String field)
    -> (Dict.Dict String recordfield -> Result String (field -> record))
    -> (Dict.Dict String recordfield -> Result String record)
decodeRecordField fieldName fieldDecoder finalDecoder =
    \recordDict ->
        case Dict.get fieldName recordDict of
            Nothing ->
                Err ("Did not find field with name " ++ fieldName)

            Just fieldValue ->
                fieldValue
                    |> fieldDecoder
                    |> Result.mapError ((++) ("Failed to decode field '" ++ fieldName ++ "': "))
                    |> Result.andThen
                        (\fieldValueDecoded ->
                            recordDict
                                |> finalDecoder
                                |> Result.map (\dec -> dec fieldValueDecoded)
                        )


decodeRecordFromPineValue : Value -> Result String (Dict.Dict String Value)
decodeRecordFromPineValue =
    decodePineListValue
        >> Result.andThen
            (List.foldl
                (\fieldAsValue ->
                    Result.andThen
                        (\fields ->
                            fieldAsValue
                                |> decodePineListValue
                                |> Result.andThen
                                    (\fieldList ->
                                        case fieldList of
                                            [ fieldNameValue, fieldValue ] ->
                                                stringFromValue fieldNameValue
                                                    |> Result.mapError ((++) "Failed to decode field name string: ")
                                                    |> Result.map (\fieldName -> ( fieldName, fieldValue ) :: fields)

                                            _ ->
                                                Err ("Unexpected number of list items for field: " ++ String.fromInt (List.length fieldList))
                                    )
                        )
                )
                (Ok [])
            )
        >> Result.map Dict.fromList


encodeRecordToPineValue : Dict.Dict String Value -> Value
encodeRecordToPineValue =
    Dict.toList
        >> List.map
            (\( fieldName, fieldValue ) ->
                ListValue [ valueFromString fieldName, fieldValue ]
            )
        >> ListValue


encodeUnionToPineValue : String -> Value -> Value
encodeUnionToPineValue tagName unionTagValue =
    ListValue [ valueFromString tagName, unionTagValue ]


decodeUnionFromPineValue : Dict.Dict String (Value -> Result String a) -> Value -> Result String a
decodeUnionFromPineValue tags value =
    value
        |> decodePineListValue
        |> Result.andThen decodeListWithExactlyTwoElements
        |> Result.andThen
            (\( tagNameValue, unionTagValue ) ->
                stringFromValue tagNameValue
                    |> Result.mapError ((++) "Failed to decode union tag name: ")
                    |> Result.andThen
                        (\tagName ->
                            case tags |> Dict.get tagName of
                                Nothing ->
                                    if tagName == "" then
                                        Err "Tag name is empty"

                                    else
                                        Err ("Unexpected tag name: " ++ tagName)

                                Just tagDecode ->
                                    unionTagValue
                                        |> tagDecode
                                        |> Result.mapError ((++) ("Failed to decode value for tag " ++ tagName ++ ": "))
                        )
            )
        >> Result.mapError ((++) "Failed to decode union: ")


decodeListWithExactlyTwoElements : List a -> Result String ( a, a )
decodeListWithExactlyTwoElements list =
    case list of
        [ a, b ] ->
            Ok ( a, b )

        _ ->
            Err ("Unexpected number of elements in list: Not 2 but " ++ String.fromInt (List.length list))


decodePineListValue : Value -> Result String (List Value)
decodePineListValue value =
    case value of
        ListValue list ->
            Ok list

        BlobValue _ ->
            Err "Is not list but blob"
