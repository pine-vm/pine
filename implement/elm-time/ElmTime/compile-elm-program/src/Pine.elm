module Pine exposing (..)

import BigInt
import Dict
import Hex
import Maybe.Extra
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
    Value -> Value


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
                            |> Result.map (\kernelFunction -> kernelFunction argument)
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


kernelFunctions : Dict.Dict String KernelFunction
kernelFunctions =
    [ ( "equal"
      , mapFromListValueOrBlobValue { fromList = list_all_same, fromBlob = list_all_same }
            >> valueFromBool
      )
    , ( "negate"
      , kernelFunction_Negate
      )
    , ( "logical_and", kernelFunctionExpectingListOfTypeBool (List.foldl (&&) True) )
    , ( "logical_or", kernelFunctionExpectingListOfTypeBool (List.foldl (||) False) )
    , ( "length"
      , mapFromListValueOrBlobValue { fromList = List.length, fromBlob = List.length }
            >> valueFromInt
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
      )
    , ( "concat"
      , kernel_function_concat
      )
    , ( "list_head"
      , kernelFunctionExpectingList
            (List.head >> Maybe.withDefault (ListValue []))
      )
    , ( "add_int"
      , kernelFunctionExpectingListOfBigIntAndProducingBigInt (List.foldl BigInt.add (BigInt.fromInt 0))
      )
    , ( "mul_int"
      , kernelFunctionExpectingListOfBigIntAndProducingBigInt (List.foldl BigInt.mul (BigInt.fromInt 1))
      )
    , ( "is_sorted_ascending_int"
      , is_sorted_ascending_int >> valueFromBool
      )
    ]
        |> Dict.fromList


kernelFunction_Negate : KernelFunction
kernelFunction_Negate value =
    case value of
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


kernel_function_concat : Value -> Value
kernel_function_concat value =
    case value of
        ListValue list ->
            case list of
                [] ->
                    ListValue []

                (BlobValue _) :: _ ->
                    BlobValue
                        (List.concatMap
                            (\item ->
                                case item of
                                    BlobValue blob ->
                                        blob

                                    _ ->
                                        []
                            )
                            list
                        )

                (ListValue _) :: _ ->
                    ListValue
                        (List.concatMap
                            (\item ->
                                case item of
                                    ListValue innerList ->
                                        innerList

                                    _ ->
                                        []
                            )
                            list
                        )

        _ ->
            ListValue []


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


kernelFunctionExpectingListOfBigIntAndProducingBigInt :
    (List BigInt.BigInt -> BigInt.BigInt)
    -> KernelFunction
kernelFunctionExpectingListOfBigIntAndProducingBigInt aggregate =
    kernelFunctionExpectingListOfBigInt
        (aggregate >> valueFromBigInt)


kernelFunctionExpectingListOfBigInt : (List BigInt.BigInt -> Value) -> KernelFunction
kernelFunctionExpectingListOfBigInt apply =
    kernelFunctionExpectingList
        (List.map bigIntFromValue
            >> Result.Extra.combine
            >> Result.map apply
            >> Result.withDefault (ListValue [])
        )


kernelFunctionExpectingListOfTypeBool : (List Bool -> Bool) -> KernelFunction
kernelFunctionExpectingListOfTypeBool apply =
    kernelFunctionExpectingList
        (\list ->
            case list of
                [] ->
                    ListValue []

                _ ->
                    List.map boolFromValue list
                        |> Maybe.Extra.combine
                        |> Maybe.map (apply >> valueFromBool)
                        |> Maybe.withDefault (ListValue [])
        )


kernelFunctionExpectingExactlyTwoArguments :
    { mapArg0 : Value -> Result (PathDescription String) arg0
    , mapArg1 : Value -> Result (PathDescription String) arg1
    , apply : arg0 -> arg1 -> Result (PathDescription String) Value
    }
    -> KernelFunction
kernelFunctionExpectingExactlyTwoArguments configuration =
    kernelFunctionExpectingList
        (\list ->
            case list of
                [ arg0Value, arg1Value ] ->
                    case configuration.mapArg0 arg0Value of
                        Err _ ->
                            ListValue []

                        Ok arg0 ->
                            case configuration.mapArg1 arg1Value of
                                Err _ ->
                                    ListValue []

                                Ok arg1 ->
                                    configuration.apply arg0 arg1
                                        |> Result.withDefault (ListValue [])

                _ ->
                    ListValue []
        )


kernelFunctionExpectingList : (List Value -> Value) -> KernelFunction
kernelFunctionExpectingList continueWithList value =
    case value of
        ListValue list ->
            continueWithList list

        _ ->
            ListValue []


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
valueFromString string =
    ListValue
        (String.foldr (\char aggregate -> valueFromChar char :: aggregate) [] string)


valueFromChar : Char -> Value
valueFromChar char =
    BlobValue (unsafeUnsignedBlobValueFromInt (Char.toCode char))


stringFromValue : Value -> Result String String
stringFromValue value =
    case value of
        ListValue charsValues ->
            stringFromListValue charsValues

        _ ->
            Err "Only a ListValue can represent a string."


stringFromListValue : List Value -> Result String String
stringFromListValue values =
    let
        continueRecursive : List Value -> List Char -> Result String (List Char)
        continueRecursive remaining processed =
            case remaining of
                [] ->
                    Ok processed

                charValue :: rest ->
                    case charValue of
                        BlobValue intValueBytes ->
                            case intValueBytes of
                                [ b1 ] ->
                                    continueRecursive rest (Char.fromCode b1 :: processed)

                                [ b1, b2 ] ->
                                    continueRecursive rest (Char.fromCode ((b1 * 256) + b2) :: processed)

                                [ b1, b2, b3 ] ->
                                    continueRecursive rest (Char.fromCode ((b1 * 65536) + (b2 * 256) + b3) :: processed)

                                _ ->
                                    Err
                                        ("Failed to map to char - unsupported number of bytes: "
                                            ++ String.fromInt (List.length intValueBytes)
                                        )

                        _ ->
                            Err "Failed to map to char - not a BlobValue"
    in
    continueRecursive values []
        |> Result.mapError ((++) "Failed to map list items to chars: ")
        |> Result.map (String.fromList >> String.reverse)


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


valueFromInt : Int -> Value
valueFromInt =
    blobValueFromInt >> BlobValue


blobValueFromInt : Int -> List Int
blobValueFromInt int =
    if int < 0 then
        2 :: unsafeUnsignedBlobValueFromInt (abs int)

    else
        4 :: unsafeUnsignedBlobValueFromInt int


unsafeUnsignedBlobValueFromInt : Int -> List Int
unsafeUnsignedBlobValueFromInt int =
    let
        recursiveFromInt : Int -> List Int -> List Int
        recursiveFromInt remaining aggregate =
            if remaining < 0x0100 then
                remaining :: aggregate

            else
                let
                    lower =
                        modBy 0x0100 remaining
                in
                recursiveFromInt (remaining // 0x0100) (lower :: aggregate)
    in
    recursiveFromInt int []


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
            Err "Only a BlobValue can encode an integer."


bigIntFromBlobValue : List Int -> Result String BigInt.BigInt
bigIntFromBlobValue blobValue =
    case blobValue of
        [] ->
            Err "Empty blob does not encode an integer."

        sign :: intValueBytes ->
            case sign of
                4 ->
                    Ok (bigIntFromUnsignedBlobValue intValueBytes)

                2 ->
                    Ok (BigInt.negate (intValueBytes |> bigIntFromUnsignedBlobValue))

                _ ->
                    Err ("Unexpected value for sign byte: " ++ String.fromInt sign)


bigIntFromUnsignedBlobValue : List Int -> BigInt.BigInt
bigIntFromUnsignedBlobValue intValueBytes =
    List.foldl
        (\nextByte aggregate ->
            BigInt.add (BigInt.fromInt nextByte) (BigInt.mul (BigInt.fromInt 0x0100) aggregate)
        )
        (BigInt.fromInt 0)
        intValueBytes


hexadecimalRepresentationFromBlobValue : List Int -> String
hexadecimalRepresentationFromBlobValue =
    List.map (Hex.toString >> String.padLeft 2 '0')
        >> String.join ""


encodeExpressionAsValue : Expression -> Value
encodeExpressionAsValue expression =
    case expression of
        LiteralExpression literal ->
            encodeUnionToPineValue
                "Literal"
                literal

        ListExpression listExpr ->
            encodeUnionToPineValue
                "List"
                (ListValue (List.map encodeExpressionAsValue listExpr))

        DecodeAndEvaluateExpression decodeAndEvaluate ->
            encodeUnionToPineValue
                "DecodeAndEvaluate"
                ([ ( "environment", encodeExpressionAsValue decodeAndEvaluate.environment )
                 , ( "expression", encodeExpressionAsValue decodeAndEvaluate.expression )
                 ]
                    |> Dict.fromList
                    |> encodeRecordToPineValue
                )

        KernelApplicationExpression app ->
            encodeUnionToPineValue
                "KernelApplication"
                ([ ( "argument", encodeExpressionAsValue app.argument )
                 , ( "functionName", valueFromString app.functionName )
                 ]
                    |> Dict.fromList
                    |> encodeRecordToPineValue
                )

        ConditionalExpression conditional ->
            encodeUnionToPineValue
                "Conditional"
                ([ ( "condition", conditional.condition )
                 , ( "ifFalse", conditional.ifFalse )
                 , ( "ifTrue", conditional.ifTrue )
                 ]
                    |> List.foldl
                        (\( fieldName, fieldExpr ) aggregate ->
                            Dict.insert fieldName (encodeExpressionAsValue fieldExpr) aggregate
                        )
                        Dict.empty
                    |> encodeRecordToPineValue
                )

        EnvironmentExpression ->
            encodeUnionToPineValue
                "Environment"
                (ListValue [])

        StringTagExpression tag tagged ->
            encodeUnionToPineValue
                "StringTag"
                (ListValue
                    [ valueFromString tag
                    , encodeExpressionAsValue tagged
                    ]
                )


decodeExpressionFromValue : Value -> Result String Expression
decodeExpressionFromValue =
    decodeUnionFromPineValue decodeExpressionFromValueDict


decodeExpressionFromValueDict : Dict.Dict String (Value -> Result String Expression)
decodeExpressionFromValueDict =
    Dict.fromList
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


decodeDecodeAndEvaluateExpression : Value -> Result String DecodeAndEvaluateExpressionStructure
decodeDecodeAndEvaluateExpression =
    decodeRecordFromPineValue
        >> Result.andThen
            (\recordDict ->
                case Dict.get "expression" recordDict of
                    Nothing ->
                        Err "Did not find field 'expression'"

                    Just expressionValue ->
                        case decodeExpressionFromValue expressionValue of
                            Err error ->
                                Err ("Failed to decode field 'expression': " ++ error)

                            Ok expression ->
                                case Dict.get "environment" recordDict of
                                    Nothing ->
                                        Err "Did not find field 'environment'"

                                    Just environmentValue ->
                                        case decodeExpressionFromValue environmentValue of
                                            Err error ->
                                                Err ("Failed to decode field 'environment': " ++ error)

                                            Ok environment ->
                                                Ok
                                                    { expression = expression
                                                    , environment = environment
                                                    }
            )


decodeKernelApplicationExpression : Value -> Result String KernelApplicationExpressionStructure
decodeKernelApplicationExpression expressionValue =
    Result.andThen
        decodeKernelApplicationExpressionRecord
        (decodeRecordFromPineValue expressionValue)


decodeKernelApplicationExpressionRecord : Dict.Dict String Value -> Result String KernelApplicationExpressionStructure
decodeKernelApplicationExpressionRecord recordDict =
    case Dict.get "functionName" recordDict of
        Nothing ->
            Err "Did not find field 'functionName'"

        Just functionNameValue ->
            case stringFromValue functionNameValue of
                Err error ->
                    Err ("Failed to decode field 'functionName': " ++ error)

                Ok functionName ->
                    case decodeKernelFunctionFromName functionName of
                        Err error ->
                            Err ("Failed to decode field 'functionName': " ++ error)

                        Ok _ ->
                            case Dict.get "argument" recordDict of
                                Nothing ->
                                    Err "Did not find field 'argument'"

                                Just argumentValue ->
                                    case decodeExpressionFromValue argumentValue of
                                        Err error ->
                                            Err ("Failed to decode field 'argument': " ++ error)

                                        Ok argument ->
                                            Ok
                                                { functionName = functionName
                                                , argument = argument
                                                }


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
        >> Result.andThen decodeConditionalExpressionRecord


decodeConditionalExpressionRecord : Dict.Dict String Value -> Result String ConditionalExpressionStructure
decodeConditionalExpressionRecord recordDict =
    case Dict.get "condition" recordDict of
        Nothing ->
            Err "Did not find field 'condition'"

        Just conditionValue ->
            case decodeExpressionFromValue conditionValue of
                Err error ->
                    Err ("Failed to decode field 'condition': " ++ error)

                Ok condition ->
                    case Dict.get "ifTrue" recordDict of
                        Nothing ->
                            Err "Did not find field 'ifTrue'"

                        Just ifTrueValue ->
                            case decodeExpressionFromValue ifTrueValue of
                                Err error ->
                                    Err ("Failed to decode field 'ifTrue': " ++ error)

                                Ok ifTrue ->
                                    case Dict.get "ifFalse" recordDict of
                                        Nothing ->
                                            Err "Did not find field 'ifFalse'"

                                        Just ifFalseValue ->
                                            case decodeExpressionFromValue ifFalseValue of
                                                Err error ->
                                                    Err ("Failed to decode field 'ifFalse': " ++ error)

                                                Ok ifFalse ->
                                                    Ok
                                                        { condition = condition
                                                        , ifTrue = ifTrue
                                                        , ifFalse = ifFalse
                                                        }


decodeRecordFromPineValue : Value -> Result String (Dict.Dict String Value)
decodeRecordFromPineValue value =
    let
        parseListRecursively : Dict.Dict String Value -> List Value -> Result String (Dict.Dict String Value)
        parseListRecursively aggregate remaining =
            case remaining of
                [] ->
                    Ok aggregate

                fieldAsValue :: rest ->
                    fieldAsValue
                        |> decodePineListValue
                        |> Result.andThen
                            (\fieldList ->
                                case fieldList of
                                    [ fieldNameValue, fieldValue ] ->
                                        stringFromValue fieldNameValue
                                            |> Result.mapError ((++) "Failed to decode field name string: ")
                                            |> Result.andThen
                                                (\fieldName ->
                                                    parseListRecursively
                                                        (Dict.insert fieldName fieldValue aggregate)
                                                        rest
                                                )

                                    _ ->
                                        Err
                                            ("Unexpected number of list items for field: "
                                                ++ String.fromInt (List.length fieldList)
                                            )
                            )
    in
    Result.andThen
        (parseListRecursively Dict.empty)
        (decodePineListValue value)


encodeRecordToPineValue : Dict.Dict String Value -> Value
encodeRecordToPineValue recordDict =
    ListValue
        (Dict.foldr
            (\fieldName fieldValue aggregate ->
                ListValue [ valueFromString fieldName, fieldValue ] :: aggregate
            )
            []
            recordDict
        )


encodeUnionToPineValue : String -> Value -> Value
encodeUnionToPineValue tagName unionTagValue =
    ListValue [ valueFromString tagName, unionTagValue ]


decodeUnionFromPineValue : Dict.Dict String (Value -> Result String a) -> Value -> Result String a
decodeUnionFromPineValue tags value =
    case Result.andThen decodeListWithExactlyTwoElements (decodePineListValue value) of
        Err err ->
            Err ("Failed to decode union: " ++ err)

        Ok ( tagNameValue, unionTagValue ) ->
            case stringFromValue tagNameValue of
                Err err ->
                    Err ("Failed to decode union tag name: " ++ err)

                Ok tagName ->
                    case Dict.get tagName tags of
                        Nothing ->
                            if tagName == "" then
                                Err "Tag name is empty"

                            else
                                Err ("Unexpected tag name: " ++ tagName)

                        Just tagDecode ->
                            case tagDecode unionTagValue of
                                Err err ->
                                    Err (("Failed to decode value for tag " ++ tagName ++ ": ") ++ err)

                                Ok ok ->
                                    Ok ok


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
