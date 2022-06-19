module Pine exposing (..)

import BigInt
import Dict
import Maybe.Extra
import Result.Extra


type Expression
    = LiteralExpression Value
    | ListExpression ListExpressionStructure
    | ApplicationExpression ApplicationExpressionStructure
    | LookupNameExpression LookupNameExpressionStruct
    | LookupNameInKernelExpression String
      {- Review name 'ContextExpansionWithName': Use 'bind'?
         Maybe we can even consolidate this into an `ApplicationExpression` (Apply kernel 'cons' to add a value to the context)?
      -}
    | ContextExpansionWithNameExpression ContextExpansionWithNameExpressionStructure
    | ConditionalExpression ConditionalExpressionStructure
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


type alias ConditionalExpressionStructure =
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
    | ClosureValue EvalContext Expression
    | KernelFunctionValue (Value -> Result (PathDescription String) Value)


type alias EvalContext =
    -- TODO: Test consolidate into simple Value
    { commonModel : List Value
    }


type PathDescription a
    = DescribePathNode a (PathDescription a)
    | DescribePathEnd a


addToContext : List Value -> EvalContext -> EvalContext
addToContext names context =
    { context | commonModel = names ++ context.commonModel }


evaluateExpression : EvalContext -> Expression -> Result (PathDescription String) Value
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


evaluateExpressionExceptClosure : EvalContext -> Expression -> Result (PathDescription String) Value
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


lookUpNameAsStringInContext : String -> EvalContext -> Result (PathDescription String) Value
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


evaluateFunctionApplication : EvalContext -> { function : Expression, argument : Expression } -> Result (PathDescription String) Value
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


evaluateFunctionApplicationWithValues : EvalContext -> { function : Value, argument : Value } -> Result (PathDescription String) Value
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

        KernelFunctionValue kernelFunction ->
            kernelFunction application.argument
                |> Result.mapError (DescribePathNode "Failed to apply kernel function")

        _ ->
            decodeExpressionFromValue application.function
                |> Result.mapError
                    (DescribePathEnd >> DescribePathNode ("Too many arguments: Failed to decode expression from value (" ++ describeValue application.function ++ ")"))
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
    KernelFunctionValue
        (configuration.mapArg0
            >> Result.mapError (DescribePathNode "Failed to map argument 0")
            >> Result.map
                (\mappedArg0 ->
                    KernelFunctionValue
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
    KernelFunctionValue
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

        ConditionalExpression _ ->
            "conditional"

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

        KernelFunctionValue _ ->
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

        KernelFunctionValue _ ->
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
encodeExpressionAsValue =
    pineEncodeExpression


decodeExpressionFromValue : Value -> Result String Expression
decodeExpressionFromValue =
    pineDecodeExpression


pineEncodeExpression : Expression -> Value
pineEncodeExpression expression =
    (case expression of
        LiteralExpression literal ->
            ( "Literal"
            , literal
            )

        ListExpression listExpr ->
            ( "List"
            , listExpr |> List.map pineEncodeExpression |> ListValue
            )

        ApplicationExpression app ->
            ( "Application"
            , [ ( "function", app.function )
              , ( "argument", app.argument )
              ]
                |> List.map (Tuple.mapSecond pineEncodeExpression)
                |> Dict.fromList
                |> pineEncodeRecord
            )

        LookupNameExpression lookup ->
            ( "LookupName"
            , [ ( "scopeExpression", pineEncodeMaybe pineEncodeExpression lookup.scopeExpression )
              , ( "name", valueFromString lookup.name )
              ]
                |> Dict.fromList
                |> pineEncodeRecord
            )

        LookupNameInKernelExpression lookup ->
            ( "LookupNameInKernel"
            , valueFromString lookup
            )

        ContextExpansionWithNameExpression contextExpansionWithName ->
            ( "ContextExpansionWithName"
            , [ ( "name", valueFromString contextExpansionWithName.name )
              , ( "namedValue", contextExpansionWithName.namedValue )
              , ( "expression", pineEncodeExpression contextExpansionWithName.expression )
              ]
                |> Dict.fromList
                |> pineEncodeRecord
            )

        ConditionalExpression conditional ->
            ( "Conditional"
            , [ ( "condition", conditional.condition )
              , ( "ifTrue", conditional.ifTrue )
              , ( "ifFalse", conditional.ifFalse )
              ]
                |> List.map (Tuple.mapSecond pineEncodeExpression)
                |> Dict.fromList
                |> pineEncodeRecord
            )

        FunctionExpression function ->
            ( "Function"
            , [ ( "argumentName", valueFromString function.argumentName )
              , ( "body", pineEncodeExpression function.body )
              ]
                |> Dict.fromList
                |> pineEncodeRecord
            )
    )
        |> (\( tagName, unionTagValue ) -> pineEncodeUnion tagName unionTagValue)


pineDecodeExpression : Value -> Result String Expression
pineDecodeExpression value =
    value
        |> pineDecodeUnion
            ([ ( "Literal"
               , LiteralExpression >> Ok
               )
             , ( "List"
               , pineDecodeList
                    >> Result.andThen (List.map pineDecodeExpression >> Result.Extra.combine)
                    >> Result.map ListExpression
               )
             , ( "Application"
               , pineDecodeApplicationExpression >> Result.map ApplicationExpression
               )
             , ( "LookupName"
               , pineDecodeLookupNameExpression >> Result.map LookupNameExpression
               )
             , ( "LookupNameInKernel"
               , stringFromValue >> Result.map LookupNameInKernelExpression
               )
             , ( "ContextExpansionWithName"
               , pineDecodeContextExpansionWithNameExpression >> Result.map ContextExpansionWithNameExpression
               )
             , ( "Conditional"
               , pineDecodeConditionalExpression >> Result.map ConditionalExpression
               )
             , ( "Function"
               , pineDecodeFunctionExpression >> Result.map FunctionExpression
               )
             ]
                |> Dict.fromList
            )


pineDecodeApplicationExpression : Value -> Result String ApplicationExpressionStructure
pineDecodeApplicationExpression =
    pineDecodeRecord
        >> Result.andThen
            (always (Ok ApplicationExpressionStructure)
                |> pineDecodeRecordField "function" pineDecodeExpression
                |> pineDecodeRecordField "argument" pineDecodeExpression
            )


pineDecodeLookupNameExpression : Value -> Result String LookupNameExpressionStruct
pineDecodeLookupNameExpression =
    pineDecodeRecord
        >> Result.andThen
            (always (Ok LookupNameExpressionStruct)
                |> pineDecodeRecordField "scopeExpression" (pineDecodeMaybe pineDecodeExpression)
                |> pineDecodeRecordField "name" stringFromValue
            )


pineDecodeContextExpansionWithNameExpression : Value -> Result String ContextExpansionWithNameExpressionStructure
pineDecodeContextExpansionWithNameExpression =
    pineDecodeRecord
        >> Result.andThen
            (always (Ok ContextExpansionWithNameExpressionStructure)
                |> pineDecodeRecordField "name" stringFromValue
                |> pineDecodeRecordField "namedValue" Ok
                |> pineDecodeRecordField "expression" pineDecodeExpression
            )


pineDecodeConditionalExpression : Value -> Result String ConditionalExpressionStructure
pineDecodeConditionalExpression =
    pineDecodeRecord
        >> Result.andThen
            (always (Ok ConditionalExpressionStructure)
                |> pineDecodeRecordField "condition" pineDecodeExpression
                |> pineDecodeRecordField "ifTrue" pineDecodeExpression
                |> pineDecodeRecordField "ifFalse" pineDecodeExpression
            )


pineDecodeFunctionExpression : Value -> Result String FunctionExpressionStructure
pineDecodeFunctionExpression =
    pineDecodeRecord
        >> Result.andThen
            (always (Ok FunctionExpressionStructure)
                |> pineDecodeRecordField "argumentName" stringFromValue
                |> pineDecodeRecordField "body" pineDecodeExpression
            )


pineEncodeMaybe : (a -> Value) -> Maybe a -> Value
pineEncodeMaybe justEncoder maybe =
    case maybe of
        Nothing ->
            pineEncodeUnion "Nothing" (ListValue [])

        Just just ->
            pineEncodeUnion "Just" (justEncoder just)


pineDecodeMaybe : (Value -> Result String a) -> Value -> Result String (Maybe a)
pineDecodeMaybe justDecoder =
    pineDecodeUnion
        ([ ( "Nothing"
           , always (Ok Nothing)
           )
         , ( "Just"
           , justDecoder >> Result.map Just
           )
         ]
            |> Dict.fromList
        )


pineDecodeRecordField :
    String
    -> (Value -> Result String field)
    -> (Dict.Dict String Value -> Result String (field -> record))
    -> (Dict.Dict String Value -> Result String record)
pineDecodeRecordField fieldName fieldDecoder finalDecoder =
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


pineDecodeRecord : Value -> Result String (Dict.Dict String Value)
pineDecodeRecord =
    pineDecodeList
        >> Result.andThen
            (List.foldl
                (\fieldAsValue ->
                    Result.andThen
                        (\fields ->
                            fieldAsValue
                                |> pineDecodeList
                                |> Result.andThen
                                    (\fieldList ->
                                        case fieldList of
                                            [ fieldNameValue, fieldValue ] ->
                                                stringFromValue fieldNameValue
                                                    |> Result.mapError ((++) "Failed to decode field name string: ")
                                                    |> Result.map (\fieldName -> ( fieldName, fieldValue ) :: fields)

                                            _ ->
                                                Err ("Unexpected number of list elements for field: " ++ String.fromInt (List.length fieldList))
                                    )
                        )
                )
                (Ok [])
            )
        >> Result.map Dict.fromList


pineEncodeRecord : Dict.Dict String Value -> Value
pineEncodeRecord fields =
    fields
        |> Dict.toList
        |> List.map
            (\( fieldName, fieldValue ) ->
                ListValue [ valueFromString fieldName, fieldValue ]
            )
        |> ListValue


pineEncodeUnion : String -> Value -> Value
pineEncodeUnion tagName unionTagValue =
    ListValue [ valueFromString tagName, unionTagValue ]


pineDecodeUnion : Dict.Dict String (Value -> Result String a) -> Value -> Result String a
pineDecodeUnion tags =
    pineDecodeList
        >> Result.andThen
            (\list ->
                case list of
                    [ tagNameValue, unionTagValue ] ->
                        stringFromValue tagNameValue
                            |> Result.mapError ((++) "Failed to decode union tag name: ")
                            |> Result.andThen
                                (\tagName ->
                                    case tags |> Dict.get tagName of
                                        Nothing ->
                                            Err ("Unexpected tag name: " ++ tagName)

                                        Just tagDecode ->
                                            unionTagValue
                                                |> tagDecode
                                                |> Result.mapError ((++) "Failed to decode tag value: ")
                                )

                    _ ->
                        Err ("Unexpected number of elements in list: " ++ String.fromInt (List.length list))
            )
        >> Result.mapError ((++) "Failed to decode union: ")


pineDecodeList : Value -> Result String (List Value)
pineDecodeList value =
    case value of
        ListValue list ->
            Ok list

        BlobValue _ ->
            Err "Is not list but blob"

        ClosureValue _ _ ->
            Err "Is not list but closure"

        KernelFunctionValue _ ->
            Err "Is not list but kernel function"
