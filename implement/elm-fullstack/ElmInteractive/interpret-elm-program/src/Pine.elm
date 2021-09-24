module Pine exposing (..)

import BigInt
import Dict
import Json.Decode
import Json.Encode
import Maybe.Extra
import Result.Extra


type Expression
    = LiteralExpression Value
    | ListExpression ListExpressionStructure
    | ApplicationExpression ApplicationExpressionStructure
    | LookupNameExpression LookupNameExpressionStruct
    | LookupNameInKernelExpression String
    | ContextExpansionWithNameExpression ContextExpansionWithNameExpressionStructure
    | IfBlockExpression IfBlockExpressionStructure
    | FunctionExpression FunctionExpressionStructure


type alias ListExpressionStructure =
    List Expression


type alias LookupNameExpressionStruct =
    { scopeExpression : Maybe Expression
    , name : String
    }


type alias ApplicationExpressionStructure =
    { function : Expression
    , argument : Expression
    }


type alias IfBlockExpressionStructure =
    { condition : Expression
    , ifTrue : Expression
    , ifFalse : Expression
    }


type alias FunctionExpressionStructure =
    { argumentName : String
    , body : Expression
    }


type alias ContextExpansionWithNameExpressionStructure =
    { name : String
    , namedValue : Value
    , expression : Expression
    }


type Value
    = BlobValue (List Int)
    | ListValue (List Value)
    | ClosureValue ExpressionContext Expression
    | KernelFunction (Value -> Result (PathDescription String) Value)


type alias ExpressionContext =
    -- TODO: Test consolidate into simple Value
    { commonModel : List Value
    }


type PathDescription a
    = DescribePathNode a (PathDescription a)
    | DescribePathEnd a


addToContext : List Value -> ExpressionContext -> ExpressionContext
addToContext names context =
    { context | commonModel = names ++ context.commonModel }


evaluateExpression : ExpressionContext -> Expression -> Result (PathDescription String) Value
evaluateExpression context expression =
    evaluateExpressionExceptClosure context expression
        |> Result.andThen
            (\value ->
                case value of
                    ClosureValue closureContext closureExpression ->
                        case closureExpression of
                            FunctionExpression _ ->
                                Ok value

                            _ ->
                                evaluateExpression closureContext closureExpression

                    _ ->
                        Ok value
            )


evaluateExpressionExceptClosure : ExpressionContext -> Expression -> Result (PathDescription String) Value
evaluateExpressionExceptClosure context expression =
    case expression of
        LiteralExpression value ->
            Ok value

        ListExpression listElements ->
            listElements
                |> List.map (evaluateExpression context)
                |> Result.Extra.combine
                |> Result.map ListValue
                |> Result.mapError (DescribePathNode "Failed to evaluate list element")

        ApplicationExpression application ->
            evaluateFunctionApplication context application
                |> Result.mapError (DescribePathNode ("Failed application of '" ++ describeExpression application.function ++ "'"))

        LookupNameExpression lookupNameExpression ->
            lookupNameExpression.scopeExpression
                |> Maybe.map
                    (evaluateExpression context
                        >> Result.map
                            (\contextValue ->
                                case contextValue of
                                    ListValue list ->
                                        { commonModel = list }

                                    _ ->
                                        { commonModel = [] }
                            )
                    )
                |> Maybe.withDefault (Ok context)
                |> Result.map
                    (\contextValue ->
                        let
                            beforeCheckForExpression =
                                lookUpNameAsStringInContext lookupNameExpression.name contextValue
                                    |> Result.withDefault (ListValue [])
                        in
                        case decodeExpressionFromValue beforeCheckForExpression of
                            Ok expressionFromLookup ->
                                ClosureValue contextValue expressionFromLookup

                            _ ->
                                beforeCheckForExpression
                    )
                |> Result.map (List.singleton >> ListValue)

        LookupNameInKernelExpression name ->
            Dict.get name pineKernelFunctions
                |> Maybe.withDefault (ListValue [])
                |> Ok

        IfBlockExpression ifBlock ->
            case evaluateExpression context ifBlock.condition of
                Err error ->
                    Err (DescribePathNode "Failed to evaluate condition" error)

                Ok conditionValue ->
                    evaluateExpression context
                        (if conditionValue == trueValue then
                            ifBlock.ifTrue

                         else
                            ifBlock.ifFalse
                        )

        ContextExpansionWithNameExpression expansion ->
            evaluateExpression
                { context
                    | commonModel = valueFromContextExpansionWithName ( expansion.name, expansion.namedValue ) :: context.commonModel
                }
                expansion.expression

        FunctionExpression _ ->
            Ok (ClosureValue context expression)


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


lookUpNameAsStringInContext : String -> ExpressionContext -> Result (PathDescription String) Value
lookUpNameAsStringInContext name context =
    let
        nameAsValue =
            valueFromString name

        getMatchFromList contextList =
            case contextList of
                [] ->
                    Nothing

                nextElement :: remainingElements ->
                    case nextElement of
                        ListValue [ labelValue, namedValue ] ->
                            if labelValue == nameAsValue then
                                Just namedValue

                            else
                                getMatchFromList remainingElements

                        _ ->
                            getMatchFromList remainingElements
    in
    case getMatchFromList context.commonModel of
        Nothing ->
            let
                availableNames =
                    context.commonModel |> List.filterMap namedValueFromValue
            in
            Err
                (DescribePathEnd
                    ("Did not find '"
                        ++ name
                        ++ "'. There are "
                        ++ (availableNames |> List.length |> String.fromInt)
                        ++ " names available in that scope: "
                        ++ (availableNames |> List.map Tuple.first |> String.join ", ")
                    )
                )

        Just firstNameValue ->
            Ok firstNameValue


pineKernelFunctions : Dict.Dict String Value
pineKernelFunctions =
    [ ( "equals"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = Ok
            , mapArg1 = Ok
            , apply = \leftValue rightValue -> Ok (valueFromBool (leftValue == rightValue))
            }
      )
    , ( "equalsNot"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = Ok
            , mapArg1 = Ok
            , apply = \leftValue rightValue -> Ok (valueFromBool (leftValue /= rightValue))
            }
      )
    , ( "notBool"
      , kernelFunctionExpectingExactlyOneArgument
            { mapArg0 = Ok
            , apply = \argument -> Ok (valueFromBool (argument /= trueValue))
            }
      )
    , ( "and", kernelFunctionExpectingExactlyTwoArgumentsOfTypeBool (&&) )
    , ( "or", kernelFunctionExpectingExactlyTwoArgumentsOfTypeBool (||) )
    , ( "listHead", kernelFunctionExpectingExactlyOneArgumentOfTypeList (List.head >> Maybe.withDefault (ListValue []) >> Ok) )
    , ( "listTail", kernelFunctionExpectingExactlyOneArgumentOfTypeList ((List.tail >> Maybe.withDefault [] >> ListValue) >> Ok) )
    , ( "negate"
      , kernelFunctionExpectingExactlyOneArgument
            { mapArg0 = bigIntFromValue >> Result.mapError DescribePathEnd
            , apply = BigInt.negate >> valueFromBigInt >> Ok
            }
      )
    , ( "addInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt BigInt.add )
    , ( "subInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt BigInt.sub )
    , ( "mulInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt BigInt.mul )
    , ( "divInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt BigInt.div )
    , ( "lessThanInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBool BigInt.lt )
    , ( "greaterThanInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBool BigInt.gt )
    , ( "append"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = Ok
            , mapArg1 = Ok
            , apply =
                \leftValue rightValue ->
                    case leftValue of
                        ListValue leftList ->
                            case rightValue of
                                ListValue rightList ->
                                    Ok (ListValue (leftList ++ rightList))

                                _ ->
                                    Err (DescribePathEnd "Mismatched operand types for 'append': Right operand not a list.")

                        BlobValue leftBlob ->
                            case rightValue of
                                BlobValue rightBlob ->
                                    Ok (BlobValue (leftBlob ++ rightBlob))

                                _ ->
                                    Err (DescribePathEnd "Mismatched operand types for 'append': Right operand not a blob.")

                        _ ->
                            Err (DescribePathEnd "Left operand for append is not a list and not a blob.")
            }
      )
    , ( "listCons"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = Ok
            , mapArg1 = Ok
            , apply =
                \leftValue rightValue ->
                    case rightValue of
                        ListValue rightList ->
                            Ok (ListValue (leftValue :: rightList))

                        _ ->
                            Err (DescribePathEnd "Right operand for listCons is not a list.")
            }
      )
    ]
        |> Dict.fromList


kernelModuleName : String
kernelModuleName =
    "PineKernel"


evaluateFunctionApplication : ExpressionContext -> { function : Expression, argument : Expression } -> Result (PathDescription String) Value
evaluateFunctionApplication context application =
    evaluateExpression context application.function
        |> Result.mapError (DescribePathNode ("Failed to evaluate function expression '" ++ describeExpression application.function ++ "'"))
        |> Result.andThen
            (\functionValue ->
                evaluateExpression context application.argument
                    |> Result.mapError (DescribePathNode ("Failed to evaluate argument '" ++ describeExpression application.argument ++ "'"))
                    |> Result.andThen
                        (\argumentValue ->
                            evaluateFunctionApplicationWithValues context
                                { function = functionValue, argument = argumentValue }
                        )
            )


evaluateFunctionApplicationWithValues : ExpressionContext -> { function : Value, argument : Value } -> Result (PathDescription String) Value
evaluateFunctionApplicationWithValues context application =
    case application.function of
        ClosureValue nextClosureContext closureExpression ->
            case closureExpression of
                FunctionExpression functionExpression ->
                    Ok
                        (ClosureValue
                            (addToContext
                                [ valueFromContextExpansionWithName ( functionExpression.argumentName, application.argument ) ]
                                nextClosureContext
                            )
                            functionExpression.body
                        )

                _ ->
                    Err
                        (DescribePathEnd
                            ("Failed to apply: Expression "
                                ++ describeExpression closureExpression
                                ++ " is not a function (Too many arguments)."
                            )
                        )

        KernelFunction kernelFunction ->
            kernelFunction application.argument
                |> Result.mapError (DescribePathNode "Failed to apply kernel function")

        _ ->
            decodeExpressionFromValue application.function
                |> Result.mapError
                    (DescribePathEnd >> DescribePathNode "Too many arguments: Failed to decode expression from value")
                |> Result.andThen
                    (\expressionFromValue ->
                        evaluateFunctionApplicationWithValues
                            { commonModel = [] }
                            { function = ClosureValue context expressionFromValue
                            , argument = application.argument
                            }
                    )


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


kernelFunctionOnTwoBigIntWithBooleanResult : (BigInt.BigInt -> BigInt.BigInt -> Bool) -> Value
kernelFunctionOnTwoBigIntWithBooleanResult apply =
    kernelFunctionExpectingExactlyTwoBigInt
        (\leftInt rightInt -> Ok (valueFromBool (apply leftInt rightInt)))


kernelFunctionExpectingExactlyTwoBigIntAndProducingBool : (BigInt.BigInt -> BigInt.BigInt -> Bool) -> Value
kernelFunctionExpectingExactlyTwoBigIntAndProducingBool apply =
    kernelFunctionExpectingExactlyTwoBigInt
        (\a0 a1 -> Ok (valueFromBool (apply a0 a1)))


kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt : (BigInt.BigInt -> BigInt.BigInt -> BigInt.BigInt) -> Value
kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt apply =
    kernelFunctionExpectingExactlyTwoBigInt
        (\a0 a1 -> Ok (valueFromBigInt (apply a0 a1)))


kernelFunctionExpectingExactlyTwoBigInt : (BigInt.BigInt -> BigInt.BigInt -> Result (PathDescription String) Value) -> Value
kernelFunctionExpectingExactlyTwoBigInt apply =
    kernelFunctionExpectingExactlyTwoArguments
        { mapArg0 = bigIntFromValue >> Result.mapError DescribePathEnd
        , mapArg1 = bigIntFromValue >> Result.mapError DescribePathEnd
        , apply = apply
        }


kernelFunctionExpectingExactlyTwoArgumentsOfTypeBool : (Bool -> Bool -> Bool) -> Value
kernelFunctionExpectingExactlyTwoArgumentsOfTypeBool apply =
    kernelFunctionExpectingExactlyTwoArguments
        { mapArg0 = boolFromValue >> Result.fromMaybe (DescribePathEnd "Is not trueValue or falseValue")
        , mapArg1 = boolFromValue >> Result.fromMaybe (DescribePathEnd "Is not trueValue or falseValue")
        , apply = \a b -> Ok (valueFromBool (apply a b))
        }


kernelFunctionExpectingExactlyTwoArguments :
    { mapArg0 : Value -> Result (PathDescription String) arg0
    , mapArg1 : Value -> Result (PathDescription String) arg1
    , apply : arg0 -> arg1 -> Result (PathDescription String) Value
    }
    -> Value
kernelFunctionExpectingExactlyTwoArguments configuration =
    KernelFunction
        (configuration.mapArg0
            >> Result.mapError (DescribePathNode "Failed to map argument 0")
            >> Result.map
                (\mappedArg0 ->
                    KernelFunction
                        (configuration.mapArg1
                            >> Result.mapError (DescribePathNode "Failed to map argument 1")
                            >> Result.andThen (configuration.apply mappedArg0)
                        )
                )
        )


kernelFunctionExpectingExactlyOneArgumentOfTypeList : (List Value -> Result (PathDescription String) Value) -> Value
kernelFunctionExpectingExactlyOneArgumentOfTypeList apply =
    kernelFunctionExpectingExactlyOneArgument
        { mapArg0 =
            \argument ->
                case argument of
                    ListValue list ->
                        Ok list

                    _ ->
                        Err (DescribePathEnd ("Argument is not a list ('" ++ describeValue argument ++ "')"))
        , apply = apply
        }


kernelFunctionExpectingExactlyOneArgument :
    { mapArg0 : Value -> Result (PathDescription String) arg0
    , apply : arg0 -> Result (PathDescription String) Value
    }
    -> Value
kernelFunctionExpectingExactlyOneArgument configuration =
    KernelFunction
        (configuration.mapArg0
            >> Result.mapError (DescribePathNode "Failed to map argument 0")
            >> Result.andThen configuration.apply
        )


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
    tagValue "True" []


falseValue : Value
falseValue =
    tagValue "False" []


tagValue : String -> List Value -> Value
tagValue tagName tagArguments =
    ListValue [ valueFromString tagName, ListValue tagArguments ]


tagValueExpression : String -> List Expression -> Expression
tagValueExpression tagName tagArgumentsExpressions =
    ListExpression [ LiteralExpression (valueFromString tagName), ListExpression tagArgumentsExpressions ]


describeExpression : Expression -> String
describeExpression expression =
    case expression of
        LookupNameExpression lookupNameExpression ->
            "lookup-name(" ++ (lookupNameExpression.scopeExpression |> Maybe.map describeExpression |> Maybe.withDefault "Nothing") ++ ", " ++ lookupNameExpression.name ++ ")"

        LookupNameInKernelExpression name ->
            "lookup-name-in-kernel(" ++ name ++ ")"

        ListExpression list ->
            "[" ++ String.join "," (list |> List.map describeExpression) ++ ")"

        LiteralExpression literal ->
            "literal(" ++ describeValue literal ++ ")"

        ApplicationExpression application ->
            "application(" ++ describeExpression application.function ++ ")"

        FunctionExpression functionExpression ->
            "function(" ++ functionExpression.argumentName ++ ", " ++ describeExpression functionExpression.body ++ ")"

        IfBlockExpression _ ->
            "if-block"

        ContextExpansionWithNameExpression expansion ->
            "context-expansion(" ++ expansion.name ++ ")"


describeValueSuperficial : Value -> String
describeValueSuperficial value =
    case value of
        BlobValue _ ->
            "BlobValue"

        ListValue _ ->
            "ListValue"

        ClosureValue _ _ ->
            "ClosureValue"

        KernelFunction _ ->
            "KernelFunction"


describeValue : Value -> String
describeValue value =
    case value of
        BlobValue blob ->
            "BlobValue 0x" ++ hexadecimalRepresentationFromBlobValue blob

        ListValue list ->
            "[" ++ String.join ", " (List.map describeValue list) ++ "]"

        ClosureValue _ expression ->
            "closure(" ++ describeExpression expression ++ ")"

        KernelFunction _ ->
            "KernelFunction"


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
            case charsValues |> List.map bigIntFromUnsignedValue |> Maybe.Extra.combine of
                Nothing ->
                    Err "Failed to map list elements to unsigned integers."

                Just chars ->
                    chars
                        |> List.map (BigInt.toString >> String.toInt >> Maybe.map Char.fromCode)
                        |> Maybe.Extra.combine
                        |> Maybe.map String.fromList
                        |> Result.fromMaybe "Programming error: Failed to map from integers to chars."

        _ ->
            Err "Only a ListValue can represent a string."


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
                0

            else
                0x80

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
            if signByte == 0 then
                Just unsignedBytes

            else
                Nothing


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
            if sign /= 0 && sign /= 0x80 then
                Err ("Unexpected value for sign byte of integer: " ++ String.fromInt sign)

            else
                intValueBytes
                    |> bigIntFromUnsignedBlobValue
                    |> (if sign == 0 then
                            identity

                        else
                            BigInt.negate
                       )
                    |> Ok


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
    [ ( "pine_expression", jsonEncodeExpression expression ) ]
        |> Json.Encode.object
        |> Json.Encode.encode 0
        |> valueFromAsciiString


decodeExpressionFromValue : Value -> Result String Expression
decodeExpressionFromValue value =
    case asciiStringFromValue value of
        Err error ->
            Err ("Failed to decode as string: " ++ error)

        Ok string ->
            Json.Decode.decodeString (Json.Decode.field "pine_expression" jsonDecodeExpression) string
                |> Result.mapError Json.Decode.errorToString


jsonEncodeExpression : Expression -> Json.Encode.Value
jsonEncodeExpression expression =
    case expression of
        LiteralExpression literal ->
            Json.Encode.object [ ( "LiteralExpression", jsonEncodeValue literal ) ]

        ListExpression list ->
            Json.Encode.object [ ( "ListExpression", Json.Encode.list jsonEncodeExpression list ) ]

        ApplicationExpression applicationExpression ->
            Json.Encode.object
                [ ( "ApplicationExpression"
                  , Json.Encode.object
                        [ ( "function", jsonEncodeExpression applicationExpression.function )
                        , ( "argument", jsonEncodeExpression applicationExpression.argument )
                        ]
                  )
                ]

        LookupNameExpression lookupNameExpression ->
            Json.Encode.object
                [ ( "LookupNameExpression"
                  , Json.Encode.object
                        [ ( "name", Json.Encode.string lookupNameExpression.name )
                        , ( "scopeExpression", jsonEncode__generic_Maybe jsonEncodeExpression lookupNameExpression.scopeExpression )
                        ]
                  )
                ]

        LookupNameInKernelExpression name ->
            Json.Encode.object
                [ ( "LookupNameInKernelExpression"
                  , Json.Encode.object [ ( "name", Json.Encode.string name ) ]
                  )
                ]

        ContextExpansionWithNameExpression contextExpansionWithNameExpression ->
            Json.Encode.object
                [ ( "ContextExpansionWithNameExpression"
                  , jsonEncodeContextExpansionWithNameExpression contextExpansionWithNameExpression
                  )
                ]

        IfBlockExpression ifBlock ->
            Json.Encode.object
                [ ( "IfBlockExpression"
                  , Json.Encode.object
                        [ ( "condition", jsonEncodeExpression ifBlock.condition )
                        , ( "ifTrue", jsonEncodeExpression ifBlock.ifTrue )
                        , ( "ifFalse", jsonEncodeExpression ifBlock.ifFalse )
                        ]
                  )
                ]

        FunctionExpression functionExpression ->
            Json.Encode.object
                [ ( "FunctionExpression"
                  , Json.Encode.object
                        [ ( "argumentName", Json.Encode.string functionExpression.argumentName )
                        , ( "body", jsonEncodeExpression functionExpression.body )
                        ]
                  )
                ]


jsonDecodeExpression : Json.Decode.Decoder Expression
jsonDecodeExpression =
    Json.Decode.oneOf
        [ Json.Decode.field "LiteralExpression"
            (Json.Decode.map LiteralExpression jsonDecodeValue)
        , Json.Decode.field "ListExpression"
            (Json.Decode.map ListExpression (Json.Decode.list (Json.Decode.lazy (\() -> jsonDecodeExpression))))
        , Json.Decode.field "ApplicationExpression"
            (Json.Decode.map ApplicationExpression
                (Json.Decode.map2 ApplicationExpressionStructure
                    (Json.Decode.field "function" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                    (Json.Decode.field "argument" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                )
            )
        , Json.Decode.field "LookupNameExpression"
            (Json.Decode.map LookupNameExpression
                (Json.Decode.map2 LookupNameExpressionStruct
                    (Json.Decode.field "scopeExpression" (Json.Decode.lazy (\() -> jsonDecode__generic_Maybe jsonDecodeExpression)))
                    (Json.Decode.field "name" Json.Decode.string)
                )
            )
        , Json.Decode.field "LookupNameInKernelExpression"
            (Json.Decode.map LookupNameInKernelExpression
                (Json.Decode.field "name" Json.Decode.string)
            )
        , Json.Decode.field "ContextExpansionWithNameExpression"
            (Json.Decode.map ContextExpansionWithNameExpression jsonDecodeContextExpansionWithNameExpression)
        , Json.Decode.field "IfBlockExpression"
            (Json.Decode.lazy
                (\() ->
                    Json.Decode.map IfBlockExpression
                        (Json.Decode.map3 IfBlockExpressionStructure
                            (Json.Decode.field "condition" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                            (Json.Decode.field "ifTrue" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                            (Json.Decode.field "ifFalse" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                        )
                )
            )
        , Json.Decode.field "FunctionExpression"
            (Json.Decode.lazy
                (\() ->
                    Json.Decode.map FunctionExpression
                        (Json.Decode.map2 FunctionExpressionStructure
                            (Json.Decode.field "argumentName" Json.Decode.string)
                            (Json.Decode.field "body" (Json.Decode.lazy (\() -> jsonDecodeExpression)))
                        )
                )
            )
        ]


jsonEncodeContextExpansionWithNameExpression : ContextExpansionWithNameExpressionStructure -> Json.Encode.Value
jsonEncodeContextExpansionWithNameExpression contextExpansionWithNameExpression =
    Json.Encode.object
        [ ( "name", Json.Encode.string contextExpansionWithNameExpression.name )
        , ( "namedValue", jsonEncodeValue contextExpansionWithNameExpression.namedValue )
        , ( "expression", jsonEncodeExpression contextExpansionWithNameExpression.expression )
        ]


jsonDecodeContextExpansionWithNameExpression : Json.Decode.Decoder ContextExpansionWithNameExpressionStructure
jsonDecodeContextExpansionWithNameExpression =
    Json.Decode.map3 ContextExpansionWithNameExpressionStructure
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "namedValue" jsonDecodeValue)
        (Json.Decode.field "expression" (Json.Decode.lazy (\() -> jsonDecodeExpression)))


jsonEncodeValue : Value -> Json.Encode.Value
jsonEncodeValue value =
    case value of
        BlobValue blob ->
            Json.Encode.object [ ( "BlobValue", Json.Encode.list Json.Encode.int blob ) ]

        ListValue list ->
            Json.Encode.object [ ( "ListValue", Json.Encode.list jsonEncodeValue list ) ]

        ClosureValue _ _ ->
            Json.Encode.object [ ( "ClosureValue", Json.Encode.string "not_implemented" ) ]

        KernelFunction _ ->
            Json.Encode.object [ ( "KernelFunction", Json.Encode.string "not_implemented" ) ]


jsonDecodeValue : Json.Decode.Decoder Value
jsonDecodeValue =
    Json.Decode.oneOf
        [ Json.Decode.field "BlobValue"
            (Json.Decode.map BlobValue (Json.Decode.list Json.Decode.int))
        , Json.Decode.field "ListValue"
            (Json.Decode.map ListValue (Json.Decode.list (Json.Decode.lazy (\() -> jsonDecodeValue))))
        , Json.Decode.field "ClosureValue"
            (Json.Decode.fail "Decoding of ClosureValue not implemented")
        ]


jsonEncode__generic_Maybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
jsonEncode__generic_Maybe encodeJust valueToEncode =
    case valueToEncode of
        Nothing ->
            [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object

        Just just ->
            [ ( "Just", [ just ] |> Json.Encode.list encodeJust ) ] |> Json.Encode.object


jsonDecode__generic_Maybe : Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
jsonDecode__generic_Maybe decoder =
    Json.Decode.oneOf
        [ Json.Decode.field "Nothing" (Json.Decode.succeed Nothing)
        , Json.Decode.field "Just" (Json.Decode.index 0 decoder |> Json.Decode.map Just)
        ]


asciiStringFromValue : Value -> Result String String
asciiStringFromValue value =
    case value of
        BlobValue charsValues ->
            charsValues
                |> List.map Char.fromCode
                |> String.fromList
                |> Ok

        _ ->
            Err "Only a BlobValue can represent an ASCII string."


valueFromAsciiString : String -> Value
valueFromAsciiString =
    String.toList >> List.map Char.toCode >> BlobValue
