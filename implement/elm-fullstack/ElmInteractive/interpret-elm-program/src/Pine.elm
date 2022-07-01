module Pine exposing (..)

import BigInt
import Dict
import Maybe.Extra
import Result.Extra


type Expression
    = LiteralExpression Value
    | ListExpression ListExpressionStructure
    | ApplicationExpression ApplicationExpressionStructure
    | KernelApplicationExpression KernelApplicationExpressionStructure
    | ConditionalExpression ConditionalExpressionStructure
    | ApplicationArgumentExpression
    | StringTagExpression StringTagExpressionStructure


type alias ListExpressionStructure =
    List Expression


type alias KernelApplicationExpressionStructure =
    { function : String
    , argument : Expression
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


type alias StringTagExpressionStructure =
    { tag : String
    , tagged : Expression
    }


type Value
    = BlobValue (List Int)
    | ListValue (List Value)


type alias KernelFunction =
    Value -> Result (PathDescription String) Value


type alias EvalContext =
    { applicationArgument : Value
    }


type PathDescription a
    = DescribePathNode a (PathDescription a)
    | DescribePathEnd a


addToContextAppArgument : List Value -> EvalContext -> EvalContext
addToContextAppArgument names context =
    let
        applicationArgument =
            case context.applicationArgument of
                ListValue applicationArgumentList ->
                    ListValue (names ++ applicationArgumentList)

                _ ->
                    context.applicationArgument
    in
    { context | applicationArgument = applicationArgument }


emptyEvalContext : EvalContext
emptyEvalContext =
    { applicationArgument = ListValue [] }


evaluateExpression : EvalContext -> Expression -> Result (PathDescription String) Value
evaluateExpression context expression =
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
                |> Result.mapError
                    (\e ->
                        e |> DescribePathNode ("Failed application of '" ++ describeExpression 1 application.function ++ "'")
                    )

        KernelApplicationExpression application ->
            case Dict.get application.function pineKernelFunctions of
                Nothing ->
                    Err (DescribePathEnd ("Did not find kernel function '" ++ application.function ++ "'"))

                Just kernelFunction ->
                    evaluateExpression context application.argument
                        |> Result.andThen
                            (\arg ->
                                kernelFunction arg
                                    |> Result.mapError (DescribePathNode ("Failed to apply kernel function '" ++ application.function ++ "': "))
                                    |> Result.mapError (\e -> DescribePathNode ("Argument: " ++ describeValue 2 arg) e)
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

        ApplicationArgumentExpression ->
            Ok context.applicationArgument

        StringTagExpression { tag, tagged } ->
            let
                log =
                    Debug.log "eval expression with tag"
                        tag
            in
            evaluateExpression context tagged


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


lookUpNameInListValue : Value -> Value -> Result (PathDescription String) Value
lookUpNameInListValue nameAsValue context =
    let
        nameAsString =
            nameAsValue |> stringFromValue |> Result.withDefault "name_is_not_a_string"

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
    case context of
        ListValue applicationArgumentList ->
            case getMatchFromList applicationArgumentList of
                Nothing ->
                    let
                        availableNames =
                            applicationArgumentList |> List.filterMap namedValueFromValue
                    in
                    Err
                        (DescribePathEnd
                            ("Did not find '"
                                ++ nameAsString
                                ++ "'. There are "
                                ++ (applicationArgumentList |> List.length |> String.fromInt)
                                ++ " entries and "
                                ++ (availableNames |> List.length |> String.fromInt)
                                ++ " names available in that scope: "
                                ++ (availableNames |> List.map Tuple.first |> String.join ", ")
                            )
                        )

                Just firstNameValue ->
                    Ok firstNameValue

        _ ->
            Err (DescribePathEnd "applicationArgument is not a list")


pineKernelFunctions : Dict.Dict String KernelFunction
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
      , (/=) trueValue >> valueFromBool >> Ok
      )
    , ( "andBool", kernelFunctionExpectingExactlyTwoArgumentsOfTypeBool (&&) )
    , ( "orBool", kernelFunctionExpectingExactlyTwoArgumentsOfTypeBool (||) )
    , ( "listHead"
      , pineDecodeList
            >> Result.map (List.head >> Maybe.withDefault (ListValue []))
            >> Result.mapError DescribePathEnd
      )
    , ( "listSkip"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = bigIntFromValue >> Result.mapError DescribePathEnd
            , mapArg1 = Ok
            , apply =
                \countBigInt listValue ->
                    (case countBigInt |> BigInt.toString |> String.toInt of
                        Nothing ->
                            Err "Failed to map from BigInt"

                        Just count ->
                            case listValue of
                                ListValue list ->
                                    Ok (ListValue (List.drop count list))

                                _ ->
                                    Err "Not a list value"
                    )
                        |> Result.mapError DescribePathEnd
            }
      )
    , ( "listTake"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = bigIntFromValue >> Result.mapError DescribePathEnd
            , mapArg1 = Ok
            , apply =
                \countBigInt listValue ->
                    (case countBigInt |> BigInt.toString |> String.toInt of
                        Nothing ->
                            Err "Failed to map from BigInt"

                        Just count ->
                            case listValue of
                                ListValue list ->
                                    Ok (ListValue (List.take count list))

                                _ ->
                                    Err "Not a list value"
                    )
                        |> Result.mapError DescribePathEnd
            }
      )
    , ( "negateInt"
      , bigIntFromValue
            >> Result.mapError DescribePathEnd
            >> Result.map (BigInt.negate >> valueFromBigInt)
      )
    , ( "addInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt BigInt.add )
    , ( "subInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt BigInt.sub )
    , ( "mulInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt BigInt.mul )
    , ( "divInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt BigInt.div )
    , ( "lessThanInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBool BigInt.lt )
    , ( "greaterThanInt", kernelFunctionExpectingExactlyTwoBigIntAndProducingBool BigInt.gt )
    , ( "listConcat"
      , pineDecodeList
            >> Result.andThen (List.map pineDecodeList >> Result.Extra.combine)
            >> Result.mapError DescribePathEnd
            >> Result.map (List.concat >> ListValue)
      )
    , ( "listReverse"
      , pineDecodeList
            >> Result.mapError DescribePathEnd
            >> Result.map (List.reverse >> ListValue)
      )
    , ( "listLength"
      , pineDecodeList
            >> Result.mapError DescribePathEnd
            >> Result.map (List.length >> BigInt.fromInt >> valueFromBigInt)
      )
    , ( "blobConcat"
      , pineDecodeList
            >> Result.andThen
                (List.map
                    (\blobValue ->
                        case blobValue of
                            BlobValue blob ->
                                Ok blob

                            _ ->
                                Err "Not a blob"
                    )
                    >> Result.Extra.combine
                )
            >> Result.mapError DescribePathEnd
            >> Result.map (List.concat >> BlobValue)
      )
    , ( "look_up_name_in_ListValue"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = Ok
            , mapArg1 = Ok
            , apply =
                \name contextValue ->
                    lookUpNameInListValue name contextValue
                        |> Result.map (List.singleton >> ListValue)
            }
      )
    , ( "make_elm_func"
      , kernelFunctionExpectingExactlyTwoArguments
            { mapArg0 = Ok
            , mapArg1 = Ok
            , apply =
                \contextValue argumentNameAndFunctionExpr ->
                    case contextValue of
                        ListValue contextElements ->
                            case argumentNameAndFunctionExpr of
                                ListValue [ argumentNameValue, functionExpressionValue ] ->
                                    stringFromValue argumentNameValue
                                        |> Result.mapError DescribePathEnd
                                        |> Result.andThen
                                            (\argumentName ->
                                                decodeExpressionFromValue functionExpressionValue
                                                    |> Result.mapError DescribePathEnd
                                                    |> Result.map
                                                        (\funcExpr ->
                                                            ApplicationExpression
                                                                { function =
                                                                    ApplicationExpression
                                                                        { function =
                                                                            funcExpr
                                                                                |> encodeExpressionAsValue
                                                                                |> LiteralExpression
                                                                        , argument = ApplicationArgumentExpression
                                                                        }
                                                                        |> encodeExpressionAsValue
                                                                        |> LiteralExpression
                                                                , argument =
                                                                    KernelApplicationExpression
                                                                        { function = "listConcat"
                                                                        , argument =
                                                                            ListExpression
                                                                                [ ListExpression
                                                                                    [ ListExpression
                                                                                        [ argumentName
                                                                                            |> valueFromString
                                                                                            |> LiteralExpression
                                                                                        , ApplicationArgumentExpression
                                                                                        ]
                                                                                    ]
                                                                                , LiteralExpression (ListValue contextElements)
                                                                                ]
                                                                        }
                                                                }
                                                                |> encodeExpressionAsValue
                                                        )
                                            )

                                _ ->
                                    Err (DescribePathEnd "Unexpected shape in argumentNameAndFunctionExpr")

                        _ ->
                            Err (DescribePathEnd "Unexpected shape in contextValue")
            }
      )
    ]
        |> Dict.fromList


evaluateFunctionApplication : EvalContext -> ApplicationExpressionStructure -> Result (PathDescription String) Value
evaluateFunctionApplication context application =
    evaluateExpression context application.argument
        |> Result.mapError
            (\e ->
                e |> DescribePathNode ("Failed to evaluate argument '" ++ describeExpression 1 application.argument ++ "'")
            )
        |> Result.andThen
            (\argumentValue ->
                evaluateExpression context application.function
                    |> Result.mapError
                        (\e ->
                            e |> DescribePathNode ("Failed to evaluate function '" ++ describeExpression 1 application.function ++ "'")
                        )
                    |> Result.andThen
                        (\functionValue ->
                            functionValue
                                |> decodeExpressionFromValue
                                |> Result.mapError
                                    (\e ->
                                        e
                                            |> DescribePathEnd
                                            |> DescribePathNode ("Failed to decode expression from function value '" ++ describeValue 3 functionValue ++ "'")
                                    )
                                |> Result.andThen
                                    (\functionExpression ->
                                        evaluateExpression { applicationArgument = argumentValue } functionExpression
                                    )
                        )
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


kernelFunctionOnTwoBigIntWithBooleanResult : (BigInt.BigInt -> BigInt.BigInt -> Bool) -> KernelFunction
kernelFunctionOnTwoBigIntWithBooleanResult apply =
    kernelFunctionExpectingExactlyTwoBigInt
        (\leftInt rightInt -> Ok (valueFromBool (apply leftInt rightInt)))


kernelFunctionExpectingExactlyTwoBigIntAndProducingBool : (BigInt.BigInt -> BigInt.BigInt -> Bool) -> KernelFunction
kernelFunctionExpectingExactlyTwoBigIntAndProducingBool apply =
    kernelFunctionExpectingExactlyTwoBigInt
        (\a0 a1 -> Ok (valueFromBool (apply a0 a1)))


kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt : (BigInt.BigInt -> BigInt.BigInt -> BigInt.BigInt) -> KernelFunction
kernelFunctionExpectingExactlyTwoBigIntAndProducingBigInt apply =
    kernelFunctionExpectingExactlyTwoBigInt
        (\a0 a1 -> Ok (valueFromBigInt (apply a0 a1)))


kernelFunctionExpectingExactlyTwoBigInt : (BigInt.BigInt -> BigInt.BigInt -> Result (PathDescription String) Value) -> KernelFunction
kernelFunctionExpectingExactlyTwoBigInt apply =
    kernelFunctionExpectingExactlyTwoArguments
        { mapArg0 = bigIntFromValue >> Result.mapError DescribePathEnd
        , mapArg1 = bigIntFromValue >> Result.mapError DescribePathEnd
        , apply = apply
        }


kernelFunctionExpectingExactlyTwoArgumentsOfTypeBool : (Bool -> Bool -> Bool) -> KernelFunction
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
    -> KernelFunction
kernelFunctionExpectingExactlyTwoArguments configuration =
    pineDecodeListWithExactlyTwoElements
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

        ApplicationExpression application ->
            "application("
                ++ (if depthLimit < 1 then
                        "..."

                    else
                        describeExpression (depthLimit - 1) application.function
                   )
                ++ ")"

        KernelApplicationExpression application ->
            "kernel-application(" ++ application.function ++ ")"

        ConditionalExpression _ ->
            "conditional"

        ApplicationArgumentExpression ->
            "application-argument"

        StringTagExpression { tag, tagged } ->
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
            , [ ( "function", pineEncodeExpression app.function )
              , ( "argument", pineEncodeExpression app.argument )
              ]
                |> Dict.fromList
                |> pineEncodeRecord
            )

        KernelApplicationExpression app ->
            ( "KernelApplication"
            , [ ( "function", valueFromString app.function )
              , ( "argument", pineEncodeExpression app.argument )
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

        ApplicationArgumentExpression ->
            ( "ApplicationArgument"
            , ListValue []
            )

        StringTagExpression { tag, tagged } ->
            ( "StringTag"
            , [ ( "tag", valueFromString tag )
              , ( "tagged", encodeExpressionAsValue tagged )
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
             , ( "KernelApplication"
               , pineDecodeKernelApplicationExpression >> Result.map KernelApplicationExpression
               )
             , ( "Conditional"
               , pineDecodeConditionalExpression >> Result.map ConditionalExpression
               )
             , ( "ApplicationArgument"
               , always (Ok ApplicationArgumentExpression)
               )
             , ( "StringTag"
               , pineDecodeStringTagExpression >> Result.map StringTagExpression
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


pineDecodeKernelApplicationExpression : Value -> Result String KernelApplicationExpressionStructure
pineDecodeKernelApplicationExpression =
    pineDecodeRecord
        >> Result.andThen
            (always (Ok KernelApplicationExpressionStructure)
                |> pineDecodeRecordField "function" stringFromValue
                |> pineDecodeRecordField "argument" pineDecodeExpression
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


pineDecodeStringTagExpression : Value -> Result String StringTagExpressionStructure
pineDecodeStringTagExpression =
    pineDecodeRecord
        >> Result.andThen
            (always (Ok StringTagExpressionStructure)
                |> pineDecodeRecordField "tag" stringFromValue
                |> pineDecodeRecordField "tagged" pineDecodeExpression
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
    pineDecodeListWithExactlyTwoElements
        >> Result.andThen
            (\( tagNameValue, unionTagValue ) ->
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
            )
        >> Result.mapError ((++) "Failed to decode union: ")


pineDecodeListWithExactlyTwoElements : Value -> Result String ( Value, Value )
pineDecodeListWithExactlyTwoElements =
    pineDecodeList
        >> Result.andThen
            (\list ->
                case list of
                    [ a, b ] ->
                        Ok ( a, b )

                    _ ->
                        Err ("Unexpected number of elements in list: Not 2 but " ++ String.fromInt (List.length list))
            )


pineDecodeList : Value -> Result String (List Value)
pineDecodeList value =
    case value of
        ListValue list ->
            Ok list

        BlobValue _ ->
            Err "Is not list but blob"
